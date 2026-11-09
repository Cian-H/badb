;;; (badb kernel journal) — Append-only provenance ledger
(define-module (badb kernel journal)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 format)
               #:use-module (json)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:use-module (srfi srfi-19)
               #:export (make-journal journal? journal-path journal-append! journal-entries
                                      journal-latest journal-count journal-entry->json-string json-string->journal-entry
                                      journal-verify-integrity journal-entry-ref safe-journal-append!
                                      kernel-ensure-string kernel-mkdir-p kernel-prop-ref kernel-ensure-parent-directory!
                                      <journal-entry> make-journal-entry journal-entry?
                                      journal-entry-timestamp journal-entry-iteration journal-entry-feature-id
                                      journal-entry-commit journal-entry-test-verdict journal-entry-author
                                      journal-entry-details journal-entry-ast-diff-hash journal-entry-spec-hash
                                      journal-entry-quickcheck-seed journal-entry-quickcheck-verdict
                                      journal-entry-kernel-replay-verdict))

(define (kernel-mkdir-p dir)
        (unless (or (not (string? dir)) (string-null? dir) (string=? dir ".") (string=? dir "/") (file-exists? dir))
          (let ((parent (dirname dir)))
            (unless (or (string=? parent dir) (string-null? parent) (string=? parent "."))
              (kernel-mkdir-p parent))
            (catch 'system-error (lambda () (mkdir dir))
                                 (lambda args (unless (file-exists? dir) (apply throw args)))))))
(define mkdir-p kernel-mkdir-p)

(define (kernel-ensure-parent-directory! file-path)
        (when (string? file-path)
          (let ((dir (dirname file-path)))
            (when (and dir (not (string-null? dir)) (not (string=? dir ".")))
              (kernel-mkdir-p dir)))))
(define ensure-parent-directory! kernel-ensure-parent-directory!)

(define (kernel-ensure-string val)
        (cond ((string? val) val) ((symbol? val) (symbol->string val))
              ((keyword? val) (symbol->string (keyword->symbol val)))
              (else (object->string val))))
(define ensure-string kernel-ensure-string)

(define* (kernel-prop-ref alist key #:optional default)
         (if (pair? alist)
           (let* ((sym (cond ((symbol? key) key) ((string? key) (string->symbol key)) (else #f)))
                  (str (cond ((string? key) key) ((symbol? key) (symbol->string key)) (else #f))))
             (let loop ((rest alist))
                       (if (null? rest) default
                                        (let ((pair (car rest)))
                                          (if (and (pair? pair)
                                                   (let ((k (car pair)))
                                                     (or (and sym (eq? k sym))
                                                         (and str (string? k) (string=? k str))
                                                         (equal? k key))))
                                            (cdr pair)
                                            (loop (cdr rest)))))))
           default))
(define prop-ref kernel-prop-ref)

(define (current-iso8601-timestamp)
        (date->string (current-date 0) "~Y-~m-~dT~H:~M:~SZ"))

(define-record-type <journal> (%make-journal path) journal? (path journal-path))

(define (make-journal filepath)
        (ensure-parent-directory! filepath)
        (unless (file-exists? filepath) (close-port (open-file filepath "a")))
        (%make-journal filepath))

(define %unspecified (list 'unspecified))

(define-record-type <journal-entry>
                    (%make-journal-entry timestamp iteration feature-id commit test-verdict
                                         author details ast-diff-hash spec-hash
                                         quickcheck-seed quickcheck-verdict kernel-replay-verdict)
                    journal-entry?
                    (timestamp journal-entry-timestamp) (iteration journal-entry-iteration)
                    (feature-id journal-entry-feature-id) (commit journal-entry-commit)
                    (test-verdict journal-entry-test-verdict) (author journal-entry-author)
                    (details journal-entry-details) (ast-diff-hash journal-entry-ast-diff-hash)
                    (spec-hash journal-entry-spec-hash) (quickcheck-seed journal-entry-quickcheck-seed)
                    (quickcheck-verdict journal-entry-quickcheck-verdict)
                    (kernel-replay-verdict journal-entry-kernel-replay-verdict))

(define* (make-journal-entry #:key (timestamp %unspecified) (iteration %unspecified)
                             (feature-id %unspecified) (commit %unspecified)
                             (test-verdict %unspecified) (author %unspecified)
                             (details %unspecified) (ast-diff-hash %unspecified)
                             (spec-hash %unspecified) (quickcheck-seed %unspecified)
                             (quickcheck-verdict %unspecified) (kernel-replay-verdict %unspecified))
         (%make-journal-entry timestamp iteration feature-id commit test-verdict
                              author details ast-diff-hash spec-hash
                              quickcheck-seed quickcheck-verdict kernel-replay-verdict))

(define (journal-entry->alist entry)
        (filter (lambda (p) (not (eq? (cdr p) %unspecified)))
                (list (cons "timestamp" (journal-entry-timestamp entry))
                      (cons "iteration" (journal-entry-iteration entry))
                      (cons "feature-id" (journal-entry-feature-id entry))
                      (cons "commit" (journal-entry-commit entry))
                      (cons "test-verdict" (journal-entry-test-verdict entry))
                      (cons "author" (journal-entry-author entry))
                      (cons "details" (journal-entry-details entry))
                      (cons "ast-diff-hash" (journal-entry-ast-diff-hash entry))
                      (cons "spec-hash" (journal-entry-spec-hash entry))
                      (cons "quickcheck-seed" (journal-entry-quickcheck-seed entry))
                      (cons "quickcheck-verdict" (journal-entry-quickcheck-verdict entry))
                      (cons "kernel-replay-verdict" (journal-entry-kernel-replay-verdict entry)))))

(define (entry->alist entry)
        (cond
          ((journal-entry? entry) (journal-entry->alist entry))
          ((record? entry)
            (let* ((rtd (record-type-descriptor entry)) (fields (record-type-fields rtd)))
              (map (lambda (f i) (cons (symbol->string f) (struct-ref entry i))) fields (iota (length fields)))))
          ((list? entry) entry)
          (else (error "Invalid journal entry: expected alist or record" entry))))

(define* (journal-entry-ref entry key #:optional default)
         (prop-ref entry key default))

(define (journal-entry->json-string entry)
        (scm->json-string (entry->alist entry)))

(define (json-string->journal-entry str)
        (json-string->scm str))

(define (read-last-journal-entry journal)
        (let ((path (journal-path journal)))
          (if (file-exists? path)
            (catch 'system-error
              (lambda ()
                (call-with-input-file path
                                      (lambda (port)
                                        (let* ((size (seek port 0 2)) (chunk-size (min size 65536)))
                                          (if (<= size 0) #f
                                                          (begin
                                                            (seek port (- size chunk-size) 0)
                                                            (let loop ((last-entry #f))
                                                                      (let ((line (read-line port)))
                                                                        (if (eof-object? line)
                                                                          (or last-entry (let ((entries (journal-entries journal)))
                                                                                           (if (null? entries) #f (last entries))))
                                                                          (let ((trimmed (string-trim-both line)))
                                                                            (if (string-null? trimmed) (loop last-entry)
                                                                                                       (catch #t
                                                                                                         (lambda () (loop (json-string->journal-entry trimmed)))
                                                                                                         (lambda _ (loop last-entry))))))))))))))
              (lambda _ #f))
            #f)))

(define (next-iteration journal)
        (let ((latest (read-last-journal-entry journal)))
          (if latest
            (let ((it (journal-entry-ref latest 'iteration)))
              (if (and it (number? it)) (+ it 1)
                                        (let loop ((rest (reverse (journal-entries journal))))
                                                  (if (null? rest) 1
                                                                   (let ((prior (journal-entry-ref (car rest) 'iteration)))
                                                                     (if (and prior (number? prior)) (+ prior 1) (loop (cdr rest))))))))
            1)))

(define (prepare-entry journal entry)
        (let* ((alist (entry->alist entry))
               (ts (journal-entry-ref alist 'timestamp))
               (it (journal-entry-ref alist 'iteration))
               (with-time (if ts alist
                                 (cons (cons "timestamp" (current-iso8601-timestamp))
                                       (filter (lambda (p) (not (member (car p) '("timestamp" timestamp)))) alist))))
               (with-iter (if (and it (number? it)) with-time
                                                    (cons (cons "iteration" (next-iteration journal))
                                                          (filter (lambda (p) (not (member (car p) '("iteration" iteration)))) with-time)))))
          with-iter))

(define (journal-append! journal entry)
        (unless (journal? journal) (error "journal-append!: expected a <journal> object" journal))
        (let* ((prepared (prepare-entry journal entry))
               (json-str (journal-entry->json-string prepared))
               (path (journal-path journal)))
          (ensure-parent-directory! path)
          (let ((port (open-file path "a")))
            (catch #t
              (lambda () (display json-str port) (newline port) (force-output port))
              (lambda (key . args) (close-port port) (apply throw key args)))
            (close-port port))
          prepared))

(define (safe-journal-append! jrn entry)
        (and (journal? jrn)
             (catch #t (lambda () (journal-append! jrn entry)) (lambda _ #f))))

(define (entry-feature-id entry)
        (or (journal-entry-ref entry "feature-id") (journal-entry-ref entry 'feature-id)
            (journal-entry-ref entry "feature_id") (journal-entry-ref entry 'feature_id)))

(define (matches-feature? entry target-id)
        (let ((fid (entry-feature-id entry)))
          (and fid (string=? (ensure-string fid) (ensure-string target-id)))))

(define* (journal-entries journal #:optional (feature-id-opt #f) #:key (feature-id feature-id-opt))
         (unless (journal? journal) (error "journal-entries: expected a <journal> object" journal))
         (let ((path (journal-path journal)))
           (if (file-exists? path)
             (let ((entries (call-with-input-file path
                                                  (lambda (port)
                                                    (let loop ((acc '()))
                                                              (let ((line (read-line port)))
                                                                (if (eof-object? line) (reverse acc)
                                                                                       (let ((parsed (catch #t (lambda () (json-string->scm line)) (lambda _ #f))))
                                                                                         (if (and (list? parsed) (pair? parsed))
                                                                                           (loop (cons parsed acc)) (loop acc))))))))))
               (if feature-id (filter (lambda (e) (matches-feature? e feature-id)) entries) entries))
             '())))

(define* (journal-latest journal #:optional (feature-id-opt #f) #:key (feature-id feature-id-opt))
         (if feature-id
           (let ((entries (journal-entries journal #:feature-id feature-id)))
             (if (null? entries) #f (last entries)))
           (read-last-journal-entry journal)))

(define* (journal-count journal #:optional (feature-id-opt #f) #:key (feature-id feature-id-opt))
         (if feature-id (length (journal-entries journal #:feature-id feature-id))
                        (let ((path (journal-path journal)))
                          (if (file-exists? path)
                            (call-with-input-file path
                                                  (lambda (port)
                                                    (let loop ((count 0))
                                                              (let ((line (read-line port)))
                                                                (cond ((eof-object? line) count)
                                                                      ((string-null? (string-trim-both line)) (loop count))
                                                                      (else (loop (+ count 1))))))))
                            0))))

(define (journal-verify-integrity journal)
        (unless (journal? journal) (error "journal-verify-integrity: expected a <journal> object" journal))
        (let ((path (journal-path journal)))
          (if (file-exists? path)
            (call-with-input-file path
                                  (lambda (port)
                                    (let loop ((line-num 1) (prev-iter #f) (count 0))
                                              (let ((line (read-line port)))
                                                (if (eof-object? line) (list (cons 'valid #t) (cons 'count count))
                                                                       (let ((parsed (catch #t (lambda () (json-string->scm line))
                                                                                               (lambda (k . a) (cons 'corrupt (format #f "Invalid JSON: ~a" k))))))
                                                                         (cond
                                                                           ((and (pair? parsed) (eq? (car parsed) 'corrupt))
                                                                             (list (cons 'valid #f) (cons 'corrupt-line line-num) (cons 'error (cdr parsed))))
                                                                           ((not (and (list? parsed) (or (null? parsed) (pair? (car parsed)))))
                                                                             (list (cons 'valid #f) (cons 'corrupt-line line-num) (cons 'error "Entry is not a JSON object")))
                                                                           (else
                                                                             (let ((iter (journal-entry-ref parsed 'iteration)))
                                                                               (cond
                                                                                 ((or (not iter) (not (number? iter)))
                                                                                   (list (cons 'valid #f) (cons 'corrupt-line line-num) (cons 'error "Missing or non-numeric iteration")))
                                                                                 ((and prev-iter (<= iter prev-iter))
                                                                                   (list (cons 'valid #f) (cons 'corrupt-line line-num)
                                                                                         (cons 'error (format #f "Non-monotonic iteration: expected > ~a, got ~a" prev-iter iter))))
                                                                                 (else (loop (+ line-num 1) iter (+ count 1)))))))))))))
            '((valid . #f) (corrupt-line . 0) (error . "Journal file does not exist")))))
