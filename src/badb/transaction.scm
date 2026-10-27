;;; badb — Transactional Self-Edit Protocol
;;;
;;; Provides transactional isolation for evaluating AST patches and code forms:
;;; 1. Evaluates patch forms inside a sandboxed child module.
;;; 2. Executes verification tests.
;;; 3. Commits to the registry with updated metadata on success,
;;;    or reverts on failure (discarding sandbox, preserving prior state)
;;;    and returning failure diagnostics.
;;; 4. Optionally records audit checkpoints into the checkpoint journal.

(define-module (badb transaction) #:use-module
               (srfi srfi-9)
               #:use-module
               (srfi srfi-11)
               #:use-module
               (ice-9 exceptions)
               #:use-module
               (ice-9 format)
               #:use-module
               (badb util)
               #:use-module
               (badb registry)
               #:use-module
               (badb journal)
               #:export
               (make-transaction-manager transaction-manager?
                                         transaction-manager-registry
                                         transaction-manager-journal
                                         stage-patch
                                         eval-patch
                                         apply-transaction!))

;;;
;;; Transaction Manager Record
;;;
(define-record-type <transaction-manager>
  (%make-transaction-manager registry journal)
  transaction-manager?
  (registry transaction-manager-registry)
  (journal  transaction-manager-journal))

(define* (make-transaction-manager registry #:optional (journal-pos #f) #:key (journal #f))
  "Create a transaction manager record holding a reference to REGISTRY and optional JOURNAL."
  (let ((eff-journal (if (not (eq? journal #f)) journal journal-pos)))
    (%make-transaction-manager registry eff-journal)))

;;;
;;; Form normalization & AST evaluation
;;;
(define (ensure-forms forms)
  "Normalize FORMS to a list of Scheme expressions/AST nodes.
Handles string input, single forms, or lists of forms."
  (cond
   ((null? forms) '())
   ((string? forms)
    (let ((port (open-input-string forms)))
      (let loop ((acc '()))
        (let ((item (read port)))
          (if (eof-object? item)
              (reverse acc)
              (loop (cons item acc)))))))
   ((not (pair? forms)) (list forms))
   ((and (pair? forms) (symbol? (car forms)))
    ;; Single compound form like (define ...) or (lambda ...)
    (list forms))
   (else forms)))

(define (stage-patch target-module forms)
        "Create a child/sandbox module inheriting from TARGET-MODULE (or fresh module
if TARGET-MODULE is #f), and evaluate FORMS within it. Returns the sandboxed module."
        (let ((sandbox (make-fresh-feature-module #f)))
          (when (module? target-module)
            (for-each
              (lambda (u)
                (unless (member u (module-uses sandbox)) (module-use! sandbox u)))
              (module-uses target-module))
            (module-for-each
              (lambda (sym var)
                (module-define! sandbox sym (variable-ref var)))
              target-module))
          (for-each
            (lambda (form)
              (eval-in-feature-module sandbox form))
            (ensure-forms forms))
          sandbox))

(define* (eval-patch forms #:optional (base-module-pos #f) #:key (base-module #f))
  "Helper to evaluate FORMS in a sandboxed module, optionally inheriting from BASE-MODULE."
  (let ((eff-base (if (not (eq? base-module #f)) base-module base-module-pos)))
    (stage-patch eff-base forms)))

;;;
;;; Helpers for metadata, error formatting, and journaling
;;;
(define (canonical-id-string id)
  "Convert feature-id to string."
  (cond
   ((string? id) id)
   ((symbol? id) (symbol->string id))
   (else (format #f "~a" id))))

(define (merge-metadata old-meta new-meta)
        "Merge OLD-META with NEW-META, with NEW-META entries taking precedence."
        (let
            ((old
               (if (list? old-meta)
                 old-meta
                 '()))
             (new
               (if (list? new-meta)
                 new-meta
                 '())))
          (let loop
            ((rest new) (acc '()))
            (if (null? rest)
              (let add-old
                ((o-rest old) (res acc))
                (if (null? o-rest)
                  (reverse res)
                  (let*
                      ((entry (car o-rest))
                       (key
                         (if (pair? entry)
                           (car entry)
                           entry)))
                    (if (assoc key new)
                      (add-old (cdr o-rest) res)
                      (add-old (cdr o-rest) (cons entry res))))))
              (loop (cdr rest) (cons (car rest) acc))))))

(define (log-journal! journal entry)
        "Append an entry to the journal if configured."
        (when journal
          (cond
            ((procedure? journal) (journal entry))
            ((journal? journal) (journal-append! journal entry))
            (else
              (let
                  ((append-fn
                     (catch #t
                       (lambda ()
                         (module-ref (resolve-module '(badb journal)) 'journal-append!))
                       (lambda _
                         #f))))
                (when append-fn (append-fn journal entry)))))))

(define (invoke-test-thunk thunk staged-module)
  "Execute THUNK passing STAGED-MODULE if accepted, otherwise with 0 arguments."
  (let ((arity (catch #t
                 (lambda () (procedure-minimum-arity thunk))
                 (lambda _ #f))))
    (if (and arity (= (car arity) 0) (not (caddr arity)) (= (cadr arity) 0))
        (thunk)
        (thunk staged-module))))

(define (resolve-implementation staged-module feature-id test-result metadata)
  "Determine the implementation object to register:
1. Specified entry-point in metadata.
2. Symbol matching feature-id defined in staged-module.
3. 'main' or 'run' procedure if defined in staged-module.
4. test-result if it is a procedure.
5. staged-module itself."
  (let*
      ((feature-sym
         (cond
           ((symbol? feature-id) feature-id)
           ((string? feature-id) (string->symbol feature-id))
           (else #f)))
       (entry-sym (or (assoc-ref metadata 'entry-point) (assoc-ref metadata 'entry))))
    (or
      (and entry-sym
           (symbol? entry-sym)
           (feature-module-ref staged-module entry-sym #f))
      (and feature-sym (feature-module-ref staged-module feature-sym #f))
      (feature-module-ref staged-module 'main #f)
      (feature-module-ref staged-module 'run #f)
      (and (procedure? test-result) test-result)
      staged-module)))

;;;
;;; Transaction Application Protocol
;;;
(define* (apply-transaction! mgr feature-id forms test-thunk
                             #:optional (meta-pos '())
                             #:key (metadata '()))
  "Apply an atomic transaction for FEATURE-ID:
1. Looks up prior implementation and metadata in (transaction-manager-registry mgr).
2. Creates a staged child module inheriting from prior module (or fresh) and evaluates FORMS.
3. Executes (test-thunk staged-module).
4. If test-thunk succeeds: commits to registry, logs commit checkpoint if journal configured,
   returns '((status . \"committed\") (feature-id . feature-id)).
5. If test-thunk returns #f or raises an error: reverts (registry untouched), logs revert checkpoint,
   returns '((status . \"reverted\") (error . error-message))."
  (unless (transaction-manager? mgr)
    (error "apply-transaction!: expected <transaction-manager>" mgr))
  (let* ((reg (transaction-manager-registry mgr))
         (journal (transaction-manager-journal mgr))
         (effective-meta (if (pair? metadata) metadata meta-pos))
         (prior-impl (registry-lookup reg feature-id))
         (prior-meta (registry-metadata reg feature-id))
         (target-module (cond
                         ((module? prior-impl) prior-impl)
                         ((and (list? prior-meta)
                               (assoc-ref prior-meta 'module)
                               (module? (assoc-ref prior-meta 'module)))
                          (assoc-ref prior-meta 'module))
                         (else #f))))
    (catch #t
      (lambda ()
        (let ((staged-module (stage-patch target-module forms)))
          (let ((test-result (invoke-test-thunk test-thunk staged-module)))
            (if test-result
                ;; Commit
                (let* ((impl (resolve-implementation staged-module feature-id test-result effective-meta))
                       (merged-meta (merge-metadata prior-meta effective-meta))
                       (updated-meta (cons (cons 'module staged-module)
                                           (filter (lambda (p) (not (eq? (car p) 'module)))
                                                   merged-meta))))
                  (registry-attach! reg feature-id impl #:metadata updated-meta)
                  (when journal
                    (log-journal! journal
                                  `((checkpoint . "commit")
                                    (status . "pass")
                                    (feature-id . ,(canonical-id-string feature-id))
                                    (test-verdict . "pass")
                                    (metadata . ,(sanitize-for-json effective-meta)))))
                  `((status . "committed") (feature-id . ,feature-id)))
                ;; Test returned #f: Revert
                (let ((err-msg "test verification failed: test thunk returned #f"))
                  (when journal
                    (log-journal! journal
                                  `((checkpoint . "revert")
                                    (status . "fail")
                                    (feature-id . ,(canonical-id-string feature-id))
                                    (test-verdict . "fail")
                                    (error . ,err-msg)
                                    (details . ((reason . ,err-msg))))))
                  `((status . "reverted") (error . ,err-msg)))))))
      (lambda (key . args)
        ;; Exception during staging or test execution: Revert
        (let ((err-msg (format-exception key args)))
          (when journal
            (log-journal! journal
                          `((checkpoint . "revert")
                            (status . "fail")
                            (feature-id . ,(canonical-id-string feature-id))
                            (test-verdict . "fail")
                            (error . ,err-msg)
                            (details . ((key . ,(symbol->string key))
                                        (args . ,(format #f "~s" args)))))))
          `((status . "reverted") (error . ,err-msg)))))))
