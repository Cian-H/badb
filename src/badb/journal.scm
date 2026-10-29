;;; badb — Checkpoint journal module.
;;;
;;; Provides persistent, append-only checkpoint journaling recording
;;; feature iterations and provenance.

(define-module (badb journal) #:use-module
               (badb util)
               #:use-module
               (badb log)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (json)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:export
               (make-journal journal?
                             journal-path
                             journal-append!
                             journal-entries
                             journal-latest
                             journal-count
                             journal-entry->json-string
                             json-string->journal-entry
                             journal-verify-integrity
                             journal-entry-ref
                             safe-journal-append!
                             <journal-entry>
                             make-journal-entry
                             journal-entry?
                             journal-entry-timestamp
                             journal-entry-iteration
                             journal-entry-feature-id
                             journal-entry-commit
                             journal-entry-test-verdict
                             journal-entry-author
                             journal-entry-details
                             journal-entry-ast-diff-hash
                             journal-entry-contract-id
                             journal-entry-kernel-replay-verdict))

;;;
;;; Journal record type
;;;
(define-record-type <journal>
  (%make-journal path)
  journal?
  (path journal-path))

(define (make-journal filepath)
  "Open or create a checkpoint journal at FILEPATH.
Ensures that any parent directory exists."
  (ensure-parent-directory! filepath)
  (unless (file-exists? filepath)
    (close-port (open-file filepath "a")))
  (%make-journal filepath))

;;;
;;; Journal entry record type and conversion
;;;
(define %unspecified (list 'unspecified))

(define-record-type <journal-entry>
                    (%make-journal-entry timestamp
                                         iteration
                                         feature-id
                                         commit
                                         test-verdict
                                         author
                                         details
                                         ast-diff-hash
                                         contract-id
                                         kernel-replay-verdict)
                    journal-entry?
                    (timestamp journal-entry-timestamp)
                    (iteration journal-entry-iteration)
                    (feature-id journal-entry-feature-id)
                    (commit journal-entry-commit)
                    (test-verdict journal-entry-test-verdict)
                    (author journal-entry-author)
                    (details journal-entry-details)
                    (ast-diff-hash journal-entry-ast-diff-hash)
                    (contract-id journal-entry-contract-id)
                    (kernel-replay-verdict journal-entry-kernel-replay-verdict))

(define*
  (make-journal-entry #:key
                      (timestamp %unspecified)
                      (iteration %unspecified)
                      (feature-id %unspecified)
                      (commit %unspecified)
                      (test-verdict %unspecified)
                      (author %unspecified)
                      (details %unspecified)
                      (ast-diff-hash %unspecified)
                      (contract-id %unspecified)
                      (kernel-replay-verdict %unspecified))
  "Create a <journal-entry> record with the given fields."
  (%make-journal-entry timestamp
                       iteration
                       feature-id
                       commit
                       test-verdict
                       author
                       details
                       ast-diff-hash
                       contract-id
                       kernel-replay-verdict))

(define (journal-entry->alist entry)
        "Convert a <journal-entry> record into an alist, omitting unspecified fields."
        (let
            ((fields
               (list
                 (cons "timestamp" (journal-entry-timestamp entry))
                 (cons "iteration" (journal-entry-iteration entry))
                 (cons "feature-id" (journal-entry-feature-id entry))
                 (cons "commit" (journal-entry-commit entry))
                 (cons "test-verdict" (journal-entry-test-verdict entry))
                 (cons "author" (journal-entry-author entry))
                 (cons "details" (journal-entry-details entry))
                 (cons "ast-diff-hash" (journal-entry-ast-diff-hash entry))
                 (cons "contract-id" (journal-entry-contract-id entry))
                 (cons "kernel-replay-verdict" (journal-entry-kernel-replay-verdict entry)))))
          (filter
            (lambda (p)
              (not (eq? (cdr p) %unspecified)))
            fields)))

(define (generic-record->alist rec)
  "Convert an arbitrary SRFI-9 record REC to an alist of string keys."
  (let* ((rtd (record-type-descriptor rec))
         (fields (record-type-fields rtd)))
    (map (lambda (fld idx)
           (cons (symbol->string fld) (struct-ref rec idx)))
         fields
         (iota (length fields)))))

(define (entry->alist entry)
  "Normalize ENTRY (which may be a <journal-entry>, another record, or an alist)
into an alist."
  (cond
   ((journal-entry? entry)
    (journal-entry->alist entry))
   ((record? entry)
    (generic-record->alist entry))
   ((list? entry)
    entry)
   (else
    (error "Invalid journal entry: expected alist or record" entry))))

;;;
;;; Field lookup helper
;;;
(define* (journal-entry-ref entry key #:optional default)
  "Retrieve the value associated with KEY (string or symbol) in ENTRY alist,
or DEFAULT if KEY is not present."
  (prop-ref entry key default))

;;;
;;; JSON serialization & deserialization
;;;
(define (journal-entry->json-string entry)
  "Serialize ENTRY (an alist or record) to a single-line JSON string."
  (scm->json-string (entry->alist entry)))

(define (json-string->journal-entry str)
  "Deserialize a JSON string STR into an entry alist."
  (json-string->scm str))

;;;
;;; Automatic timestamp and iteration calculation
;;;
;; current-iso8601-timestamp is provided by (badb log).

(define (read-last-journal-entry journal)
        "Read the most recent journal entry from disk efficiently via seek,
returning the parsed entry alist or #f if JOURNAL has no entries."
        (let ((path (journal-path journal)))
          (if (file-exists? path)
            (catch 'system-error
              (lambda ()
                (call-with-input-file path
                                      (lambda (port)
                                        (let*
                                            ((size (seek port 0 2)) (chunk-size (min size 65536)))
                                          (if (<= size 0)
                                            #f
                                            (begin (seek port (- size chunk-size) 0)
                                                   (let loop
                                                     ((last-entry #f))
                                                     (let ((line (read-line port)))
                                                       (if (eof-object? line)
                                                         (or last-entry
                                                             (let ((entries (journal-entries journal)))
                                                               (if (null? entries)
                                                                 #f
                                                                 (last entries))))
                                                         (let ((trimmed (string-trim-both line)))
                                                           (if (string-null? trimmed)
                                                             (loop last-entry)
                                                             (catch #t
                                                               (lambda ()
                                                                 (loop (json-string->journal-entry trimmed)))
                                                               (lambda _
                                                                 (loop last-entry))))))))))))))
              (lambda _
                #f))
            #f)))

(define (next-iteration journal)
  "Compute the next auto-incrementing iteration ID for JOURNAL by inspecting
the latest entry on disk."
  (let ((latest (read-last-journal-entry journal)))
    (if latest
        (let ((it (journal-entry-ref latest 'iteration)))
          (if (and it (number? it))
              (+ it 1)
              (let loop ((rest (reverse (journal-entries journal))))
                (if (null? rest)
                    1
                    (let ((prior-it (journal-entry-ref (car rest) 'iteration)))
                      (if (and prior-it (number? prior-it))
                          (+ prior-it 1)
                          (loop (cdr rest))))))))
        1)))

(define (prepare-entry journal entry)
  "Ensure ENTRY has timestamp and auto-incrementing iteration fields injected
if not already provided."
  (let* ((alist (entry->alist entry))
         (timestamp-val (journal-entry-ref alist 'timestamp))
         (iteration-val (journal-entry-ref alist 'iteration))
         (with-time
          (if timestamp-val
              alist
              (cons (cons "timestamp" (current-iso8601-timestamp))
                    (filter (lambda (p)
                              (not (member (car p) '("timestamp" timestamp))))
                            alist))))
         (with-iter
          (if (and iteration-val (number? iteration-val))
              with-time
              (cons (cons "iteration" (next-iteration journal))
                    (filter (lambda (p)
                              (not (member (car p) '("iteration" iteration))))
                            with-time)))))
    with-iter))

;;;
;;; Appending entries
;;;
(define (journal-append! journal entry)
  "Append ENTRY (alist or record) to JOURNAL as a single-line JSON string followed
by newline, and flush the port. Automatically injects timestamp and monotonic
iteration if not provided. Returns the prepared entry alist."
  (unless (journal? journal)
    (error "journal-append!: expected a <journal> object" journal))
  (let* ((prepared (prepare-entry journal entry))
         (json-str (journal-entry->json-string prepared))
         (path (journal-path journal)))
    (ensure-parent-directory! path)
    (let ((port (open-file path "a")))
      (catch #t
        (lambda ()
          (display json-str port)
          (newline port)
          (force-output port))
        (lambda (key . args)
          (close-port port)
          (apply throw key args)))
      (close-port port))
    prepared))

(define (safe-journal-append! jrn entry)
  "Append ENTRY to JRN when it is a valid <journal>, returning the prepared
entry; return #f when JRN is absent, invalid, or the append raises."
  (and (journal? jrn)
       (catch #t
         (lambda () (journal-append! jrn entry))
         (lambda _ #f))))

;;;
;;; Querying entries
;;;
(define (entry-feature-id entry)
  (or (journal-entry-ref entry "feature-id")
      (journal-entry-ref entry 'feature-id)
      (journal-entry-ref entry "feature_id")
      (journal-entry-ref entry 'feature_id)))

(define (matches-feature? entry target-id)
  (let ((fid (entry-feature-id entry)))
    (and fid (string=? (ensure-string fid) (ensure-string target-id)))))

(define* (journal-entries journal #:optional (feature-id-opt #f) #:key (feature-id feature-id-opt))
  "Read all valid journal entries from disk.
If FEATURE-ID is provided, return only entries matching that feature id."
  (unless (journal? journal)
    (error "journal-entries: expected a <journal> object" journal))
  (let ((path (journal-path journal)))
    (if (file-exists? path)
        (let ((entries
               (call-with-input-file path
                 (lambda (port)
                   (let loop ((acc '()))
                     (let ((line (read-line port)))
                       (cond
                        ((eof-object? line)
                         (reverse acc))
                        (else
                         (let ((parsed (catch #t
                                         (lambda () (json-string->scm line))
                                         (lambda _ #f))))
                           (if (and (list? parsed) (pair? parsed))
                               (loop (cons parsed acc))
                               (loop acc)))))))))))
          (if feature-id
              (filter (lambda (e) (matches-feature? e feature-id)) entries)
              entries))
        '())))

(define* (journal-latest journal
                         #:optional (feature-id-opt #f)
                         #:key (feature-id feature-id-opt))
  "Return the most recent journal entry (optionally for given FEATURE-ID),
or #f if the journal has no matching entries."
  (if feature-id
      (let ((entries (journal-entries journal #:feature-id feature-id)))
        (if (null? entries) #f (last entries)))
      (read-last-journal-entry journal)))

(define* (journal-count journal
                        #:optional (feature-id-opt #f)
                        #:key (feature-id feature-id-opt))
  "Return the number of entries in JOURNAL (optionally for given FEATURE-ID)."
  (if feature-id
      (length (journal-entries journal #:feature-id feature-id))
      (let ((path (journal-path journal)))
        (if (file-exists? path)
            (call-with-input-file path
              (lambda (port)
                (let loop ((count 0))
                  (let ((line (read-line port)))
                    (cond
                     ((eof-object? line) count)
                     ((string-null? (string-trim-both line)) (loop count))
                     (else (loop (+ count 1))))))))
            0))))

;;;
;;; Integrity verification
;;;
(define (journal-verify-integrity journal)
  "Scan JOURNAL file line-by-line; check that all lines are valid JSON and iterations
are strictly monotonic.
Returns ((valid . #t) (count . N)) on success,
or ((valid . #f) (corrupt-line . L) (error . reason)) on failure."
  (unless (journal? journal)
    (error "journal-verify-integrity: expected a <journal> object" journal))
  (let ((path (journal-path journal)))
    (if (file-exists? path)
        (call-with-input-file path
          (lambda (port)
            (let loop ((line-num 1) (prev-iter #f) (count 0))
              (let ((line (read-line port)))
                (cond
                 ((eof-object? line)
                  (list (cons 'valid #t) (cons 'count count)))
                 (else
                  (let ((parsed (catch #t
                                  (lambda () (json-string->scm line))
                                  (lambda (key . args)
                                    (cons 'corrupt (format #f "Invalid JSON: ~a" key))))))
                    (cond
                     ((and (pair? parsed) (eq? (car parsed) 'corrupt))
                      (list (cons 'valid #f)
                            (cons 'corrupt-line line-num)
                            (cons 'error (cdr parsed))))
                     ((not (and (list? parsed) (or (null? parsed) (pair? (car parsed)))))
                      (list (cons 'valid #f)
                            (cons 'corrupt-line line-num)
                            (cons 'error "Entry is not a JSON object")))
                     (else
                      (let ((iter (journal-entry-ref parsed 'iteration)))
                        (cond
                         ((or (not iter) (not (number? iter)))
                          (list (cons 'valid #f)
                                (cons 'corrupt-line line-num)
                                (cons 'error "Missing or non-numeric iteration")))
                         ((and prev-iter (<= iter prev-iter))
                          (list (cons 'valid #f)
                                (cons 'corrupt-line line-num)
                                (cons 'error (format #f "Non-monotonic iteration: expected > ~a, got ~a"
                                                     prev-iter iter))))
                         (else
                          (loop (+ line-num 1) iter (+ count 1))))))))))))))
        '((valid . #f) (corrupt-line . 0) (error . "Journal file does not exist")))))
