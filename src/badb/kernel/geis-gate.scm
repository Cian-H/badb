;;; (badb kernel geis-gate) — Supervisor gate enforcing geasa contracts
(define-module (badb kernel geis-gate)
  #:use-module (badb kernel digest)
  #:use-module (badb kernel journal)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (geis-gate-root geis-gate-dir sanitize-feature-id locked-geis-path
            lock-geis verify-locked-geis geis-gate-transaction-pins geis-gate-ast-diff-hash
            lock-geis-bundle verify-locked-geis-bundle
            spec-gate-root spec-gate-dir locked-spec-path lock-spec
            verify-locked-spec spec-gate-transaction-pins spec-gate-ast-diff-hash))

(define ensure-string kernel-ensure-string)
(define mkdir-p kernel-mkdir-p)
(define prop-ref kernel-prop-ref)
(define ensure-parent-directory! kernel-ensure-parent-directory!)

(define (geis-gate-root)
  (let ((env (or (getenv "BADB_GEIS_GATE_ROOT") (getenv "BADB_SPEC_GATE_ROOT"))))
    (if (and env (not (string-null? env))) env ".badb")))

(define (geis-gate-dir)
  (string-append (geis-gate-root) "/specs"))

(define (sanitize-feature-id feature-id)
  (let* ((raw (string-trim-both (ensure-string feature-id)))
         (clean (string-map (lambda (c)
                              (if (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\- #\_ #\.)))
                                  c #\_))
                            raw)))
    (if (string-null? clean) "spec" clean)))

(define* (locked-geis-path feature-id #:optional (root (geis-gate-root)))
  (string-append root "/specs/" (sanitize-feature-id feature-id) ".scm"))

(define (%read-utf8-file path)
  (call-with-input-file path
    (lambda (p) (set-port-encoding! p "UTF-8") (get-string-all p))))

(define (%write-utf8-file path text)
  (call-with-output-file path
    (lambda (p) (set-port-encoding! p "UTF-8") (display text p) (force-output p))))

(define (lock-geis spec-path feature-id)
  (let* ((target (locked-geis-path feature-id))
         (dir (dirname target)))
    (mkdir-p dir)
    (let* ((content (%read-utf8-file spec-path))
           (hash (sha256-hex content)))
      (when (file-exists? target)
        (catch 'system-error (lambda () (chmod target #o644)) (lambda _ #f))
        (delete-file target))
      (%write-utf8-file target content)
      (chmod target #o444)
      hash)))

(define*
  (locked-geis-bundle-path feature-id #:optional (root (geis-gate-root)))
  "Path of the locked copy of bundle FEATURE-ID under the gate store."
  (string-append root "/geasa/" (sanitize-feature-id feature-id)))

(define (%file-is-dir? path)
        (catch #t
          (lambda ()
            (eq? (stat:type (stat path)) 'directory))
          (lambda _
            #f)))

(define (%delete-tree! path)
        "Best-effort recursive delete of the tree at PATH; unlocks permissions
first so previously locked (read-only) trees can be removed."
        (if (%file-is-dir? path)
          (begin
            (catch 'system-error
              (lambda ()
                (chmod path #o755))
              (lambda _
                #f))
            (for-each
              (lambda (e)
                (%delete-tree! (string-append path "/" e)))
              (%list-dir path))
            (catch 'system-error
              (lambda ()
                (rmdir path))
              (lambda _
                #f)))
          (begin
            (catch 'system-error
              (lambda ()
                (chmod path #o644))
              (lambda _
                #f))
            (catch 'system-error
              (lambda ()
                (delete-file path))
              (lambda _
                #f)))))

(define (%list-dir d)
        " scandir without the dot entries, empty list on error."
        (or
          (catch #t
            (lambda ()
              (scandir d
                       (lambda (n)
                         (not (member n '("." ".."))))))
            (lambda _
              #f))
          '()))

(define (%collect-bundle-files bundle-dir)
        "All files under BUNDLE-DIR recursively as (relative . absolute) pairs,
excluding manifest.scm."
        (let
            ((root
               (if (string-suffix? "/" bundle-dir)
                 (substring bundle-dir 0 (- (string-length bundle-dir) 1))
                 bundle-dir)))
          (let scan
            ((dir root) (prefix ""))
            (append-map!
              (lambda (entry)
                (let ((full (string-append dir "/" entry)))
                  (if (%file-is-dir? full)
                    (scan full (string-append prefix entry "/"))
                    (if (string=? entry "manifest.scm")
                      '()
                      (list (cons (string-append prefix entry) full))))))
              (%list-dir dir)))))

(define (lock-geis-bundle bundle-dir feature-id)
        "Lock the bundle at BUNDLE-DIR for FEATURE-ID: copy the whole tree into
the gate store read-only and return the bundle content hash."
        (let*
            ((target (locked-geis-bundle-path feature-id))
             (files (%collect-bundle-files bundle-dir)))
          (mkdir-p (geis-gate-root))
          (when (file-exists? target) (%delete-tree! target))
          (mkdir-p target)
          (for-each
            (lambda (pair)
              (let*
                  ((rel (car pair)) (src (cdr pair)) (dst (string-append target "/" rel)))
                (ensure-parent-directory! dst)
                (%write-utf8-file dst (%read-utf8-file src))
                (chmod dst #o444)))
            files)
          (for-each
            (lambda (pair)
              (let
                  ((d (dirname (string-append target "/" (car pair)))))
                (catch 'system-error
                  (lambda ()
                    (chmod d #o555))
                  (lambda _
                    #f))))
            files)
          (sha256-hex
            (apply string-append
                   (sort
                     (map
                       (lambda (pair)
                         (format #f
                                 "~a ~a~a"
                                 (car pair)
                                 (sha256-hex (%read-utf8-file (cdr pair)))
                                 #\newline))
                       files)
                     string<?)))))

(define*
  (verify-locked-geis-bundle feature-id
                             #:key
                             (root (geis-gate-root))
                             (timeout 60)
                             (load-paths '("src" "." "geis")))
  "Replay the LOCKED copy of bundle FEATURE-ID's spec in a disposable
subprocess, with the locked bundle directory first on the load path so
the locked plugin resolves to the locked copy.  Return shape matches
verify-locked-geis."
  (let*
      ((bundle-root (locked-geis-bundle-path feature-id root))
       (locked-spec (string-append bundle-root "/spec.geis.scm")))
    (catch #t
      (lambda ()
        (if (file-exists? locked-spec)
          (let*
              ((spec-hash (sha256-hex (%read-utf8-file locked-spec)))
               (spawned (%spawn-spec-process locked-spec (cons root load-paths)))
               (pid (car spawned))
               (out-port (cadr spawned))
               (err-port (caddr spawned))
               (result
                 (dynamic-wind
                   (lambda ()
                     #f)
                   (lambda ()
                     (let
                         ((io (%drain-ports-with-timeout! out-port err-port timeout)))
                       (if (eq? io 'timeout)
                         (begin
                           (catch #t
                             (lambda ()
                               (kill pid SIGKILL))
                             (lambda _
                               #f))
                           (%reap-process pid)
                           (list 'timeout "" "" '((reaped . #f))))
                         (list #f (car io) (cdr io) (%reap-process pid)))))
                   (lambda ()
                     (catch #t
                       (lambda ()
                         (close-port out-port))
                       (lambda _
                         #f))
                     (catch #t
                       (lambda ()
                         (close-port err-port))
                       (lambda _
                         #f)))))
               (timeout? (car result))
               (stdout-text (cadr result))
               (stderr-text (caddr result))
               (exit-info (cadddr result))
               (json
                 (if timeout?
                   #f
                   (%parse-diagnostics-json stdout-text)))
               (checked (%cross-check-verdict json exit-info timeout?))
               (verdict (assoc-ref checked 'verdict))
               (reason (assoc-ref checked 'reason))
               (seed (ensure-string (or (assoc-ref json "seed") "")))
               (failures
                 (let ((raw (assoc-ref json "failures")))
                   (cond
                     ((vector? raw) (vector->list raw))
                     ((list? raw) raw)
                     (else '())))))
            (list (cons 'verdict verdict)
                  (cons 'spec-hash spec-hash)
                  (cons 'reason
                        (if (string-null? reason)
                          (format #f
                                  "~a/~a passed, ~a failed"
                                  (or (%json-number json "passed") "?")
                                  (or (%json-number json "total") "?")
                                  (or (%json-number json "failed") "?"))
                          (string-append reason
                                         (if (string-null? stderr-text)
                                           ""
                                           (string-append "; stderr: "
                                                          (string-trim-both
                                                            (if (> (string-length stderr-text) 200)
                                                              (substring stderr-text (- (string-length stderr-text) 200))
                                                              stderr-text)))))))
                  (cons 'exit-status (assoc-ref exit-info 'exit))
                  (cons 'signal (assoc-ref exit-info 'signal))
                  (cons 'timed-out?
                        (if timeout?
                          #t
                          #f))
                  (cons 'seed seed)
                  (cons 'summary-total
                        (if json
                          (%json-number json "total")
                          #f))
                  (cons 'summary-passed
                        (if json
                          (%json-number json "passed")
                          #f))
                  (cons 'summary-failed
                        (if json
                          (%json-number json "failed")
                          #f))
                  (cons 'failures failures)))
          (%make-fail-verdict
            (format #f "no locked bundle for feature ~a at ~s" feature-id bundle-root))))
      (lambda (key . args)
        (%make-fail-verdict (format #f "spec subprocess spawn failed: ~a: ~s" key args))))))

(define (geis-gate-ast-diff-hash forms)
  (let ((diff-text (if (string? forms) forms
                       (apply string-append
                              (map (lambda (f) (string-append (format #f "~s" f) "\n"))
                                   (if (list? forms) forms (list forms)))))))
    (sha256-hex diff-text)))

(define (%now-seconds)
  (let ((t (gettimeofday))) (+ (car t) (/ (cdr t) 1000000.0))))

(define (%spawn-spec-process locked-path load-paths)
  (let* ((out-pipe (pipe)) (err-pipe (pipe)) (pid (primitive-fork)))
    (if (= pid 0)
        (begin
          (close-port (car out-pipe)) (close-port (car err-pipe))
          (dup2 (fileno (cdr out-pipe)) 1) (dup2 (fileno (cdr err-pipe)) 2)
          (close-port (cdr out-pipe)) (close-port (cdr err-pipe))
          (setenv "GUILE_AUTO_COMPILE" "0")
          (apply execlp "guile" "guile" "--no-auto-compile"
                 (append (apply append (map (lambda (d) (list "-L" d)) load-paths))
                         (list locked-path)))
          (primitive-exit 127))
        (begin
          (close-port (cdr out-pipe)) (close-port (cdr err-pipe))
          (list pid (car out-pipe) (car err-pipe))))))

(define (%drain-ports-with-timeout! out-port err-port timeout)
  (let ((deadline (+ (%now-seconds) timeout))
        (out-acc (open-output-string)) (err-acc (open-output-string)))
    (define (drain-available! p target-acc)
      (let drain-loop ((first? #t))
        (if (or first? (char-ready? p))
            (let ((c (catch #t (lambda () (read-char p)) (lambda _ (the-eof-object)))))
              (if (eof-object? c) #t (begin (write-char c target-acc) (drain-loop #f))))
            #f)))
    (let loop ((out-done? #f) (err-done? #f))
      (if (and out-done? err-done?)
          (cons (get-output-string out-acc) (get-output-string err-acc))
          (let ((remaining (- deadline (%now-seconds))))
            (if (<= remaining 0) 'timeout
                (let* ((candidates (append (if out-done? '() (list out-port))
                                           (if err-done? '() (list err-port))))
                       (sel (catch #t (lambda () (select candidates '() '() remaining)) (lambda _ #f)))
                       (ready (if sel (car sel) #f)))
                  (if (or (not ready) (null? ready)) 'timeout
                      (let gather ((ports ready) (od out-done?) (ed err-done?))
                        (if (null? ports) (loop od ed)
                            (let ((p (car ports)))
                              (if (eq? p out-port)
                                  (gather (cdr ports) (drain-available! p out-acc) ed)
                                  (gather (cdr ports) od (drain-available! p err-acc))))))))))))))

(define (%reap-process pid)
  (let* ((status* (catch #t (lambda () (waitpid pid)) (lambda _ #f)))
         (status (if (pair? status*) (cdr status*) #f)))
    (cond
     ((not status) '((reaped . #f)))
     ((status:exit-val status) => (lambda (v) `((exit . ,v))))
     ((status:term-sig status) => (lambda (s) `((signal . ,s))))
     (else '((reaped . #f))))))

(define (%parse-diagnostics-json stdout-text)
  (let ((lines (filter (lambda (l) (not (string-null? (string-trim-both l))))
                       (string-split stdout-text #\newline))))
    (and (pair? lines)
         (let ((obj (catch #t (lambda () (json-string->scm (string-trim-both (last lines)))) (lambda _ #f))))
           (and (list? obj) (pair? obj) obj)))))

(define (%json-number obj key)
  (let ((summary (assoc-ref obj "summary")))
    (and (list? summary) (let ((v (assoc-ref summary key))) (and (number? v) v)))))

(define* (%cross-check-verdict json exit-info timeout?)
  (let ((exit-code (assoc-ref exit-info 'exit))
        (signalled (assoc-ref exit-info 'signal)))
    (cond
     (timeout? '((verdict . "fail") (reason . "spec subprocess timed out")))
     ((not json)
      `((verdict . "fail")
        (reason . ,(format #f "spec subprocess produced no diagnostics JSON on stdout; exit=~s signal=~s"
                           exit-code signalled))))
     (else
      (let* ((json-verdict (assoc-ref json "verdict"))
             (total (%json-number json "total")) (passed (%json-number json "passed"))
             (failed (%json-number json "failed")))
        (cond
         ((not (member json-verdict '("pass" "fail")))
          `((verdict . "fail") (reason . ,(format #f "malformed diagnostics verdict: ~s" json-verdict))))
         ((and signalled (not exit-code))
          `((verdict . "fail") (reason . ,(format #f "spec subprocess died on signal ~a" signalled))))
         ((and (eqv? exit-code 0) (string=? json-verdict "pass")
               failed (= 0 failed) total passed (= passed total))
          '((verdict . "pass") (reason . "")))
         (else
          `((verdict . "fail")
            (reason . ,(format #f "diagnostics/exit mismatch: verdict=~s exit=~s summary total=~s passed=~s failed=~s"
                               json-verdict exit-code total passed failed))))))))))

(define (%make-fail-verdict reason)
  `((verdict . "fail") (spec-hash . #f) (reason . ,reason)
    (exit-status . #f) (signal . #f) (timed-out? . #f)
    (seed . "") (summary-total . #f) (summary-passed . #f)
    (summary-failed . #f) (failures . ())))

(define* (verify-locked-geis feature-id
                             #:key (root (geis-gate-root)) (timeout 60) (load-paths '("src" "." "geis")))
  (let ((locked (locked-geis-path feature-id root)))
    (catch #t
      (lambda ()
        (if (file-exists? locked)
            (let* ((spec-hash (sha256-hex (%read-utf8-file locked)))
                   (spawned (%spawn-spec-process locked load-paths))
                   (pid (car spawned)) (out-port (cadr spawned)) (err-port (caddr spawned))
                   (result
                    (dynamic-wind
                      (lambda () #f)
                      (lambda ()
                        (let ((io (%drain-ports-with-timeout! out-port err-port timeout)))
                          (if (eq? io 'timeout)
                              (begin (catch #t (lambda () (kill pid SIGKILL)) (lambda _ #f))
                                     (%reap-process pid)
                                     (list 'timeout "" "" '((reaped . #f))))
                              (list #f (car io) (cdr io) (%reap-process pid)))))
                      (lambda ()
                        (catch #t (lambda () (close-port out-port)) (lambda _ #f))
                        (catch #t (lambda () (close-port err-port)) (lambda _ #f)))))
                   (timeout? (car result)) (stdout-text (cadr result))
                   (stderr-text (caddr result)) (exit-info (cadddr result))
                   (json (if timeout? #f (%parse-diagnostics-json stdout-text)))
                   (checked (%cross-check-verdict json exit-info timeout?))
                   (verdict (assoc-ref checked 'verdict)) (reason (assoc-ref checked 'reason))
                   (seed (ensure-string (or (assoc-ref json "seed") "")))
                   (failures (let ((raw (assoc-ref json "failures")))
                               (cond ((vector? raw) (vector->list raw))
                                     ((list? raw) raw) (else '())))))
              `((verdict . ,verdict) (spec-hash . ,spec-hash)
                (reason . ,(if (string-null? reason)
                               (format #f "~a/~a passed, ~a failed"
                                       (or (%json-number json "passed") "?")
                                       (or (%json-number json "total") "?")
                                       (or (%json-number json "failed") "?"))
                               (string-append reason
                                              (if (string-null? stderr-text) ""
                                                  (string-append "; stderr: "
                                                   (let ((tail (if (> (string-length stderr-text) 200)
                                                                   (substring stderr-text (- (string-length stderr-text) 200))
                                                                   stderr-text)))
                                                     (string-trim-both tail)))))))
                (exit-status . ,(assoc-ref exit-info 'exit))
                (signal . ,(assoc-ref exit-info 'signal))
                (timed-out? . ,(if timeout? #t #f))
                (seed . ,seed)
                (summary-total . ,(if json (%json-number json "total") #f))
                (summary-passed . ,(if json (%json-number json "passed") #f))
                (summary-failed . ,(if json (%json-number json "failed") #f))
                (failures . ,failures)))
            (%make-fail-verdict (format #f "no locked spec for feature ~a at ~s" feature-id locked))))
      (lambda (key . args)
        (%make-fail-verdict (format #f "spec subprocess spawn failed: ~a: ~s" key args))))))

(define* (geis-gate-transaction-pins feature-id forms metadata
                                     #:key (timeout 60) (load-paths '("src" "." "geis")))
  (let* ((root (geis-gate-root))
         (locked (locked-geis-path feature-id root))
         (override-timeout (prop-ref metadata 'spec-timeout #f))
         (effective-timeout (if (number? override-timeout) override-timeout timeout)))
    (if (file-exists? locked)
        (let* ((hash (geis-gate-ast-diff-hash forms))
               (result (verify-locked-geis feature-id #:root root #:timeout effective-timeout #:load-paths load-paths))
               (passed? (string=? (assoc-ref result 'verdict) "pass"))
               (spec-hash (ensure-string (or (assoc-ref result 'spec-hash) "")))
               (seed (ensure-string (or (assoc-ref result 'seed) "")))
               (details (ensure-string (or (assoc-ref result 'reason) ""))))
          (if passed?
              `((gate . "passed") (ast-diff-hash . ,hash) (spec-hash . ,spec-hash)
                (quickcheck-seed . ,seed) (quickcheck-verdict . "pass")
                (kernel-replay-verdict . "passed") (details . ,details))
              `((gate . "failed") (ast-diff-hash . ,hash) (spec-hash . ,spec-hash)
                (quickcheck-seed . ,seed) (quickcheck-verdict . "fail")
                (kernel-replay-verdict . "failed") (details . ,details))))
        '((gate . "skipped")))))

(define spec-gate-root geis-gate-root)
(define spec-gate-dir geis-gate-dir)
(define locked-spec-path locked-geis-path)
(define lock-spec lock-geis)
(define verify-locked-spec verify-locked-geis)
(define spec-gate-transaction-pins geis-gate-transaction-pins)
(define spec-gate-ast-diff-hash geis-gate-ast-diff-hash)
