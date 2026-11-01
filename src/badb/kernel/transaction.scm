;;; (badb kernel transaction) — Transactional Self-Edit Protocol
(define-module (badb kernel transaction)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (badb kernel registry)
  #:use-module (badb kernel journal)
  #:use-module (badb kernel geis-gate)
  #:export (<transaction-manager> make-transaction-manager transaction-manager?
            transaction-manager-registry transaction-manager-journal
            stage-patch eval-patch apply-transaction!))

(define-record-type <transaction-manager>
  (%make-transaction-manager registry journal)
  transaction-manager?
  (registry transaction-manager-registry)
  (journal  transaction-manager-journal))

(define* (make-transaction-manager registry #:optional (journal-pos #f) #:key (journal #f))
  (%make-transaction-manager registry (or journal journal-pos)))

(define ensure-string kernel-ensure-string)

(define (alist-remove key alist)
  (if (pair? alist)
      (let ((key-str (ensure-string key)))
        (filter (lambda (p) (not (and (pair? p) (string=? (ensure-string (car p)) key-str)))) alist))
      '()))

(define (sanitize-for-json val)
  (cond
   ((or (string? val) (number? val) (boolean? val) (null? val)) val)
   ((symbol? val) (symbol->string val))
   ((pair? val)
    (if (and (or (symbol? (car val)) (string? (car val)))
             (not (pair? (cdr val))) (not (null? (cdr val))))
        (cons (if (symbol? (car val)) (symbol->string (car val)) (car val))
              (sanitize-for-json (cdr val)))
        (map sanitize-for-json val)))
   ((module? val) "#<module>") ((procedure? val) "#<procedure>")
   (else (format #f "~a" val))))

(define (format-exception ex-key ex-args)
  (cond
   ((and (eq? ex-key 'misc-error) (pair? ex-args) (pair? (cdr ex-args)) (string? (cadr ex-args)))
    (catch #t
      (lambda ()
        (let* ((msg-fmt (cadr ex-args))
               (msg-args (if (and (pair? (cddr ex-args)) (pair? (caddr ex-args)))
                             (caddr ex-args) (cddr ex-args))))
          (if (list? msg-args) (apply format #f msg-fmt msg-args) (format #f "~a" msg-fmt))))
      (lambda _ (format #f "~a: ~s" ex-key ex-args))))
   ((and (pair? ex-args) (string? (car ex-args))) (car ex-args))
   (else (format #f "~a: ~s" ex-key ex-args))))

(define (canonical-id-string id) (ensure-string id))

(define (merge-metadata old-meta new-meta)
  (let ((old (if (list? old-meta) old-meta '()))
        (new (if (list? new-meta) new-meta '())))
    (append new (filter (lambda (e) (not (assoc (if (pair? e) (car e) e) new))) old))))

(define (log-journal! journal entry)
  (when journal
    (cond
     ((procedure? journal) (journal entry))
     ((journal? journal) (journal-append! journal entry))
     (else
      (let ((append-fn (catch #t (lambda () (module-ref (resolve-module '(badb kernel journal)) 'journal-append!))
                         (lambda _ #f))))
        (when append-fn (append-fn journal entry)))))))

(define (ensure-forms forms)
  (cond
   ((null? forms) '())
   ((string? forms)
    (let ((port (open-input-string forms)))
      (let loop ((acc '()))
        (let ((item (read port)))
          (if (eof-object? item) (reverse acc) (loop (cons item acc)))))))
   ((not (pair? forms)) (list forms))
   ((and (pair? forms) (symbol? (car forms))) (list forms))
   (else forms)))

(define (stage-patch target-module forms)
  (let ((sandbox (make-fresh-feature-module #f)))
    (when (module? target-module)
      (for-each (lambda (u) (unless (member u (module-uses sandbox)) (module-use! sandbox u)))
                (module-uses target-module))
      (module-for-each (lambda (sym var) (module-define! sandbox sym (variable-ref var)))
                       target-module))
    (for-each (lambda (form) (eval-in-feature-module sandbox form))
              (ensure-forms forms))
    sandbox))

(define* (eval-patch forms #:optional (base-module-pos #f) #:key (base-module #f))
  (stage-patch (or base-module base-module-pos) forms))

(define (invoke-test-thunk thunk staged-module)
  (let ((arity (catch #t (lambda () (procedure-minimum-arity thunk)) (lambda _ #f))))
    (if (and arity (= (car arity) 0) (not (caddr arity)) (= (cadr arity) 0))
        (thunk) (thunk staged-module))))

(define (resolve-implementation staged-module feature-id test-result metadata)
  (let* ((feature-sym (cond ((symbol? feature-id) feature-id)
                            ((string? feature-id) (string->symbol feature-id))
                            (else #f)))
         (entry-sym (or (assoc-ref metadata 'entry-point) (assoc-ref metadata 'entry))))
    (or (and entry-sym (symbol? entry-sym) (feature-module-ref staged-module entry-sym #f))
        (and feature-sym (feature-module-ref staged-module feature-sym #f))
        (feature-module-ref staged-module 'main #f)
        (feature-module-ref staged-module 'run #f)
        (and (procedure? test-result) test-result)
        staged-module)))

(define* (apply-transaction! mgr feature-id forms test-thunk
                            #:optional (meta-pos #f) #:key (metadata #f))
  (unless (transaction-manager? mgr)
    (error "apply-transaction!: expected <transaction-manager>" mgr))
  (let* ((reg (transaction-manager-registry mgr))
         (journal (transaction-manager-journal mgr))
         (effective-meta (or metadata meta-pos '()))
         (prior-meta (or (registry-metadata reg feature-id) '()))
         (target-module (cond ((and prior-meta (assoc-ref prior-meta 'module)) => identity)
                              (else #f))))
    (define (revert-and-log! err-msg details)
      (when journal
        (log-journal! journal
                      `((checkpoint . "revert") (status . "fail")
                        (feature-id . ,(canonical-id-string feature-id))
                        (test-verdict . "fail") (error . ,err-msg) (details . ,details))))
      `((status . "reverted") (error . ,err-msg)))

    (catch #t
      (lambda ()
        (let* ((staged-module (stage-patch target-module forms))
               (test-result (invoke-test-thunk test-thunk staged-module)))
          (if test-result
              (let* ((impl (resolve-implementation staged-module feature-id test-result effective-meta))
                     (merged-meta (merge-metadata prior-meta effective-meta))
                     (updated-meta (cons (cons 'module staged-module)
                                         (filter (lambda (p) (not (eq? (car p) 'module))) merged-meta)))
                     (gate-result (geis-gate-transaction-pins (canonical-id-string feature-id)
                                                             (ensure-forms forms) effective-meta))
                     (pins (alist-remove 'gate gate-result)))
                (if (member (assoc-ref gate-result 'gate) '("skipped" "passed"))
                    (begin
                      (registry-attach! reg feature-id impl #:metadata updated-meta)
                      (when journal
                        (log-journal! journal
                                      (append `((checkpoint . "commit") (status . "pass")
                                                (feature-id . ,(canonical-id-string feature-id))
                                                (test-verdict . "pass")
                                                (metadata . ,(sanitize-for-json effective-meta)))
                                              pins)))
                      `((status . "committed") (feature-id . ,feature-id)))
                    (revert-and-log!
                     (string-append "spec gate: " (or (assoc-ref gate-result 'details) "verdict failed"))
                     `((gate . ,(assoc-ref gate-result 'gate))
                       (details . ,(or (assoc-ref gate-result 'details) ""))))))
              (let ((err-msg "test verification failed: test thunk returned #f"))
                (revert-and-log! err-msg `((reason . ,err-msg)))))))
      (lambda (key . args)
        (revert-and-log! (format-exception key args)
                         `((key . ,(symbol->string key)) (args . ,(format #f "~s" args))))))))
