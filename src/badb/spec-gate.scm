;;; badb — Kernel-owned spec verification gate (roadmap Phase 2).
;;;
;;; THE GATE
;;; ========
;;; The kernel — never the agent — owns spec verification. An agent
;;; authors a declarative spec file; the kernel locks an immutable
;;; copy of it (lock-spec) and, at commit time, replays THE LOCKED
;;; COPY in a disposable subprocess (verify-locked-spec), parsing the
;;; verdict only from the subprocess's own stdout JSON plus its exit
;;; status. Agent-reported verdicts are never trusted. Any spawn,
;;; timeout, or parse failure is a FAILED verdict: the gate fails
;;; closed.
;;;
;;; LOCKED SPEC STORE
;;; =================
;;; Locked copies live at <root>/specs/<sanitized-feature-id>.scm
;;; where <root> is $BADB_SPEC_GATE_ROOT when set, else .badb. Locked
;;; copies are chmod'd read-only; the gate re-reads THIS copy at
;;; verification time, never the agent's working copy.
;;;
;;; JSON DIAGNOSTICS CONTRACT
;;; ========================
;;; The spec file's LAST stdout line must be a single JSON object:
;;;   {"summary":{"total":N,"passed":N,"failed":N},
;;;    "verdict":"pass"|"fail",
;;;    "seed":"...",
;;;    "failures":[{"name":"...","kind":"property|example|export",
;;;                 "seed":"...","inputs":[...],
;;;                 "exception":"...","backtrace":"..."}]}
;;; A pass is recorded only when the JSON verdict is "pass", the exit
;;; status is 0, and the summary counts are self-consistent (zero
;;; failures, passed = total). Any disagreement is a FAILED verdict.

(define-module (badb spec-gate)
  #:use-module (badb digest)
  #:use-module (badb util)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (spec-gate-root
            spec-gate-dir
            sanitize-feature-id
            locked-spec-path
            lock-spec
            verify-locked-spec
            spec-gate-transaction-pins
            spec-gate-ast-diff-hash))

;;;
;;; Locked spec store layout
;;;
(define (spec-gate-root)
  "Return the base directory of the locked spec store:
$BADB_SPEC_GATE_ROOT when set, else .badb (relative to the current
working directory)."
  (let ((env (getenv "BADB_SPEC_GATE_ROOT")))
    (if (and env (not (string-null? env)))
        env
        ".badb")))

(define (spec-gate-dir)
  "Return the directory holding locked spec copies."
  (string-append (spec-gate-root) "/specs"))

(define (sanitize-feature-id feature-id)
  "Sanitize FEATURE-ID into a filename component: alphanumerics,
hyphens, underscores, and dots are kept; every other character
becomes an underscore."
  (let* ((raw (string-trim-both (ensure-string feature-id)))
         (clean (string-map (lambda (c)
                              (if (or (char-alphabetic? c)
                                      (char-numeric? c)
                                      (memv c '(#\- #\_ #\.)))
                                  c
                                  #\_))
                            raw)))
    (if (string-null? clean)
        "spec"
        clean)))

(define* (locked-spec-path feature-id #:optional (root (spec-gate-root)))
  "Return the path of the locked spec copy for FEATURE-ID under ROOT."
  (string-append root "/specs/" (sanitize-feature-id feature-id) ".scm"))

(define (%read-utf8-file path)
  "Read PATH as a UTF-8 text file, returning its contents."
  (call-with-input-file path
    (lambda (port)
      (set-port-encoding! port "UTF-8")
      (get-string-all port))))

(define (%write-utf8-file path text)
  "Write TEXT to PATH as UTF-8."
  (call-with-output-file path
    (lambda (port)
      (set-port-encoding! port "UTF-8")
      (display text port)
      (force-output port))))

(define (lock-spec spec-path feature-id)
  "Lock the spec at SPEC-PATH for FEATURE-ID: copy it to the locked
spec store, make the copy read-only, and return the SHA-256 hex of
the locked copy. The kernel re-reads THIS copy at gate time; the
agent's working copy is never trusted afterwards."
  (let* ((target (locked-spec-path feature-id))
         (dir (dirname target)))
    (mkdir-p dir)
    (let* ((content (%read-utf8-file spec-path))
           (hash (sha256-hex content)))
      (when (file-exists? target)
        ;; Unlock any prior copy so it can be replaced.
        (catch 'system-error
          (lambda () (chmod target #o644))
          (lambda _ #f))
        (delete-file target))
      (%write-utf8-file target content)
      (chmod target #o444)
      hash)))

;;;
;;; AST diff hashing (identity pinning)
;;;
(define (spec-gate-ast-diff-hash forms)
  "Return the identity-pinning hash for the staged AST patch FORMS:
the SHA-256 hex digest of the normalized diff text. Normalization
choice (documented): each top-level form is rendered with ~s and
terminated with a newline, so the digest is reproducible externally
(one form per line piped to sha256sum). A raw string is used verbatim
as the diff text."
  (let ((diff-text
         (if (string? forms)
             forms
             (apply string-append
                    (map (lambda (form)
                           (string-append (format #f "~s" form) "\n"))
                         (if (list? forms) forms (list forms)))))))
    (sha256-hex diff-text)))

;;;
;;; Disposable subprocess verification (kernel-owned, fail closed)
;;;
(define (%now-seconds)
  "Return the current wall-clock time as an inexact number of seconds."
  (let ((t (gettimeofday)))
    (+ (car t) (/ (cdr t) 1000000.0))))

(define (%spawn-spec-process locked-path load-paths)
  "Fork and exec guile --no-auto-compile on LOCKED-PATH with the
load path flags -L <dir> for each entry of LOAD-PATHS. The child's
stdout and stderr are captured on pipes. Returns
(pid stdout-port stderr-port); the child never returns."
  (let ((out-pipe (pipe))
        (err-pipe (pipe)))
    (let ((pid (primitive-fork)))
      (if (= pid 0)
          (begin
            (close-port (car out-pipe))
            (close-port (car err-pipe))
            (dup2 (fileno (cdr out-pipe)) 1)
            (dup2 (fileno (cdr err-pipe)) 2)
            (close-port (cdr out-pipe))
            (close-port (cdr err-pipe))
            (setenv "GUILE_AUTO_COMPILE" "0")
            (apply execlp "guile" "guile" "--no-auto-compile"
                   (append
                    (apply append
                           (map (lambda (dir) (list "-L" dir)) load-paths))
                    (list locked-path)))
            (primitive-exit 127))
          (begin
            (close-port (cdr out-pipe))
            (close-port (cdr err-pipe))
            (list pid (car out-pipe) (car err-pipe)))))))

(define (%drain-ports-with-timeout! out-port err-port timeout)
  "Read OUT-PORT and ERR-PORT concurrently until both hit EOF or
TIMEOUT (seconds, inexact) elapses. Returns (OUT-TEXT . ERR-TEXT),
or the symbol 'timeout when the deadline expired first."
  (let ((deadline (+ (%now-seconds) timeout)))
    (let loop ((out "") (err "") (out-done? #f) (err-done? #f))
      (if (and out-done? err-done?)
          (cons out err)
          (let ((remaining (- deadline (%now-seconds))))
            (if (<= remaining 0)
                'timeout
                (let* ((candidates
                        (append (if out-done? '() (list out-port))
                                (if err-done? '() (list err-port))))
                       (sel (catch #t
                              (lambda () (select candidates '() '() remaining))
                              (lambda _ #f)))
                       (ready (if sel (car sel) #f)))
                  (if (or (not ready) (null? ready))
                      'timeout
                      (let gather ((ports ready)
                                         (out out) (err err)
                                         (od out-done?) (ed err-done?))
                              (if (null? ports)
                                  (loop out err od ed)
                                  (let* ((p (car ports))
                                         (c (catch #t
                                              (lambda () (read-char p))
                                              (lambda _ 'eof))))
                                    (if (eof-object? c)
                                        (gather (cdr ports) out err
                                                (if (eq? p out-port) #t od)
                                                (if (eq? p err-port) #t ed))
                                        (gather (cdr ports)
                                                (if (eq? p out-port)
                                                    (string-append out (string c))
                                                    out)
                                                (if (eq? p err-port)
                                                    (string-append err (string c))
                                                    err)
                                                od ed)))))))))))))

(define (%reap-process pid)
  "Wait for the child PID, returning
((exit . N) | (signal . N)) as a one-element alist."
  (let* ((status* (catch #t
                    (lambda () (waitpid pid))
                    (lambda _ #f)))
         (status (if (pair? status*) (cdr status*) #f)))
    (cond
     ((not status) '((reaped . #f)))
     ((status:exit-val status) => (lambda (v) `((exit . ,v))))
     ((status:term-sig status) => (lambda (s) `((signal . ,s))))
     (else '((reaped . #f))))))

(define (%parse-diagnostics-json stdout-text)
  "Extract the diagnostics JSON object from the LAST non-empty
stdout line, returning the parsed alist (with STRING keys) or #f."
  (let ((lines (filter (lambda (line)
                         (not (string-null? (string-trim-both line))))
                       (string-split stdout-text #\newline))))
    (and (pair? lines)
         (let ((obj (catch #t
                      (lambda ()
                        (json-string->scm (string-trim-both (last lines))))
                      (lambda _ #f))))
           (and (list? obj) (pair? obj) obj)))))

(define (%json-number obj key)
  "Return the numeric value of the summary sub-field KEY of OBJ, or #f."
  (let ((summary (assoc-ref obj "summary")))
    (and (list? summary)
         (let ((v (assoc-ref summary key)))
           (and (number? v) v)))))

(define* (%cross-check-verdict json exit-info timeout?)
  "Compute the kernel verdict from the parsed JSON diagnostics and
the subprocess EXIT-INFO ((exit . N) or ((signal . N))). Returns a
verdict alist:
  ((verdict . \"pass\"|\"fail\") (reason . STRING))
Pass requires: no timeout, exit status 0, JSON verdict \"pass\",
summary.failed = 0, and summary.passed = summary.total. Everything
else — including any disagreement between JSON and exit code — is
fail."
  (let ((exit-code (assoc-ref exit-info 'exit))
        (signalled (assoc-ref exit-info 'signal)))
    (cond
     (timeout?
      `((verdict . "fail")
        (reason . "spec subprocess timed out")))
     ((not json)
      `((verdict . "fail")
        (reason . ,(format #f "spec subprocess produced no diagnostics JSON on stdout; exit=~s signal=~s"
                           exit-code signalled))))
     (else
      (let* ((json-verdict (assoc-ref json "verdict"))
             (total (%json-number json "total"))
             (passed (%json-number json "passed"))
             (failed (%json-number json "failed")))
        (cond
         ((not (member json-verdict '("pass" "fail")))
          `((verdict . "fail")
            (reason . ,(format #f "malformed diagnostics verdict: ~s" json-verdict))))
         ((and signalled (not exit-code))
          `((verdict . "fail")
            (reason . ,(format #f "spec subprocess died on signal ~a" signalled))))
         ((and (eqv? exit-code 0)
               (string=? json-verdict "pass")
               failed (= 0 failed)
               total passed (= passed total))
          '((verdict . "pass") (reason . "")))
         (else
          `((verdict . "fail")
            (reason . ,(format #f "diagnostics/exit mismatch: verdict=~s exit=~s summary total=~s passed=~s failed=~s"
                               json-verdict exit-code total passed failed))))))))))


(define* (verify-locked-spec feature-id
                             #:key
                             (root (spec-gate-root))
                             (timeout 60)
                             (load-paths '("src" ".")))
  "Run the LOCKED spec copy for FEATURE-ID in a disposable
subprocess (guile --no-auto-compile -L <dir> ... <locked-path>),
capturing stdout, stderr, and exit status with a TIMEOUT. Returns an
alist:
  ((verdict . \"pass\"|\"fail\") (spec-hash . H) (reason . STRING)
   (exit-status . N|#f) (signal . N|#f) (timed-out? . BOOL)
   (seed . STRING) (summary-total . N) (summary-passed . N)
   (summary-failed . N) (failures . LIST))
The verdict is parsed ONLY from the subprocess's own last-stdout-line
JSON cross-checked against its exit code. ANY spawn, timeout, or
parse failure yields verdict \"fail\" — the gate fails CLOSED and
never raises."
  (let* ((locked (locked-spec-path feature-id root)))
    (catch #t
      (lambda ()
        (if (not (file-exists? locked))
            `((verdict . "fail")
              (spec-hash . #f)
              (reason . ,(format #f "no locked spec for feature ~a at ~s"
                                 feature-id locked))
              (exit-status . #f) (signal . #f) (timed-out? . #f)
              (seed . "") (summary-total . #f) (summary-passed . #f)
              (summary-failed . #f) (failures . ()))
            (let* ((spec-hash (sha256-hex (%read-utf8-file locked)))
                   (spawned (%spawn-spec-process locked load-paths))
                   (pid (list-ref spawned 0))
                   (out-port (list-ref spawned 1))
                   (err-port (list-ref spawned 2))
                   (result
                    (dynamic-wind
                      (lambda () #f)
                      (lambda ()
                        (let ((io (%drain-ports-with-timeout! out-port err-port timeout)))
                          (if (eq? io 'timeout)
                              (begin
                                (catch #t
                                  (lambda () (kill pid SIGKILL))
                                  (lambda _ #f))
                                (%reap-process pid)
                                (list 'timeout "" "" '((reaped . #f))))
                            (let ((status-info (%reap-process pid)))
                              (list #f (car io) (cdr io) status-info)))))
                      (lambda ()
                        (catch #t (lambda () (close-port out-port)) (lambda _ #f))
                        (catch #t (lambda () (close-port err-port)) (lambda _ #f)))))
                   (timeout? (list-ref result 0))
                   (stdout-text (list-ref result 1))
                   (stderr-text (list-ref result 2))
                   (exit-info (list-ref result 3))
                   (json (if timeout? #f (%parse-diagnostics-json stdout-text)))
                   (checked (%cross-check-verdict json exit-info timeout?))
                   (verdict (assoc-ref checked 'verdict))
                   (reason (assoc-ref checked 'reason))
                   (seed (ensure-string (or (assoc-ref json "seed") "")))
                   (failures
                    (let ((raw (assoc-ref json "failures")))
                      (if (vector? raw) (vector->list raw)
                          (if (list? raw) raw '())))))
              `((verdict . ,verdict)
                (spec-hash . ,spec-hash)
                (reason . ,(if (string-null? reason)
                               (format #f "~a/~a passed, ~a failed"
                                       (or (%json-number json "passed") "?")
                                       (or (%json-number json "total") "?")
                                       (or (%json-number json "failed") "?"))
                               (string-append reason
                                              (if (string-null? stderr-text)
                                                  ""
                                                  (string-append
                                                   "; stderr: "
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
                (failures . ,failures)))))
      (lambda (key . args)
        ;; Spawn or any unexpected failure: fail closed, never raise.
        `((verdict . "fail")
          (spec-hash . #f)
          (reason . ,(format #f "spec subprocess spawn failed: ~a: ~s" key args))
          (exit-status . #f) (signal . #f) (timed-out? . #f)
          (seed . "") (summary-total . #f) (summary-passed . #f)
          (summary-failed . #f) (failures . ()))))))

;;;
;;; Transaction pinning (what transaction.scm calls)
;;;
(define* (spec-gate-transaction-pins feature-id forms metadata
                                     #:key
                                     (timeout 60)
                                     (load-paths '("src" ".")))
  "Run the kernel-owned spec gate for a transaction commit of
FEATURE-ID with staged FORMS and METADATA, returning the pin outcome
for the journal commit record:
  ((gate . \"skipped\"))  — no locked spec exists for FEATURE-ID
                            (behavior identical to no gate at all)
  ((gate . \"passed\") (ast-diff-hash . H) (spec-hash . H)
   (quickcheck-seed . S) (quickcheck-verdict . \"pass\")
   (kernel-replay-verdict . \"passed\") (details . STRING))
  ((gate . \"failed\") (ast-diff-hash . H) (spec-hash . H)
   (quickcheck-seed . S) (quickcheck-verdict . \"fail\")
   (kernel-replay-verdict . \"failed\") (details . STRING))
The verdict comes only from the kernel's own subprocess run of the
LOCKED spec copy; agent-reported verdicts are never trusted. Any
spawn/parse/timeout failure is a \"failed\" verdict (fail closed);
this procedure never raises and never reports a pass it did not
observe itself. The locked-spec store root is $BADB_SPEC_GATE_ROOT
when set, else .badb; METADATA may override the timeout via a
spec-timeout entry."
  (let* ((root (spec-gate-root))
         (locked (locked-spec-path feature-id root))
         (override-timeout (prop-ref metadata 'spec-timeout #f))
         (effective-timeout (if (number? override-timeout)
                                override-timeout
                                timeout)))
    (if (not (file-exists? locked))
        '((gate . "skipped"))
        (let* ((hash (spec-gate-ast-diff-hash forms))
               (result (verify-locked-spec feature-id
                                          #:root root
                                          #:timeout effective-timeout
                                          #:load-paths load-paths))
               (passed? (string=? (assoc-ref result 'verdict) "pass"))
               (spec-hash (ensure-string (or (assoc-ref result 'spec-hash) "")))
               (seed (ensure-string (or (assoc-ref result 'seed) "")))
               (details (ensure-string (or (assoc-ref result 'reason) ""))))
          (if passed?
              `((gate . "passed")
                (ast-diff-hash . ,hash)
                (spec-hash . ,spec-hash)
                (quickcheck-seed . ,seed)
                (quickcheck-verdict . "pass")
                (kernel-replay-verdict . "passed")
                (details . ,details))
              `((gate . "failed")
                (ast-diff-hash . ,hash)
                (spec-hash . ,spec-hash)
                (quickcheck-seed . ,seed)
                (quickcheck-verdict . "fail")
                (kernel-replay-verdict . "failed")
                (details . ,details)))))))
