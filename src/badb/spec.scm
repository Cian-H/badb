;;; badb — Declarative contract & property spec engine (roadmap Phase 2).
;;;
;;; This module is the SPEC-AUTHORING side of the Phase 2 trust anchor:
;;; the kernel gate (see (badb spec-gate)) locks a spec file, replays
;;; it in a disposable subprocess, and parses the machine-readable
;;; diagnostics this engine emits.  The engine never decides its own
;;; verdict; it only reports what it observed.
;;;
;;; SPEC FILE SHAPE
;;; ===============
;;; A spec file is a standalone Guile script.  The target shape
;;; (what synthesis agents author and the kernel locks):
;;;
;;;     (use-modules (srfi srfi-64)
;;;                  (srfi srfi-253)
;;;                  (quickcheck)
;;;                  (quickcheck arbitrary)
;;;                  (quickcheck property)
;;;                  (badb spec)
;;;                  (my-module-under-test))
;;;
;;;     (spec-begin "my-feature-suite")
;;;     (define-module-spec (my module) proc1 (proc2 2) (proc3 1 3))
;;;     (define-contract-spec proc1
;;;       ((xs ($list $integer)))
;;;       -> (result (and (list? result) (every integer? result))))
;;;     (define-contract-spec proc2
;;;       ((n $integer) (m $natural))
;;;       #:name "proc2 ordering"
;;;       -> (result (<= result (+ n m))))
;;;     (spec-end)
;;;     (spec-exit)
;;;
;;; Ordering matters: define-module-spec forms run the export-boundary
;;; reflection check at their evaluation position, so authoring them
;;; BEFORE the define-contract-spec forms makes boundary violations
;;; (missing export, extra export, non-procedure, wrong arity) fail
;;; fast, ahead of the property suites.
;;;
;;; DIAGNOSTICS CONTRACT
;;; ====================
;;; (spec-end) emits, as the FINAL line of stdout, the JSON object the
;;; kernel gate parses (string keys after json-string->scm):
;;;
;;;   {"summary":{"total":N,"passed":N,"failed":N},
;;;    "verdict":"pass"|"fail",
;;;    "seed":"...",
;;;    "failures":[{"name":"...","kind":"property"|"example"|"export",
;;;                "seed":"...","inputs":[...],
;;;                "exception":"...","backtrace":"..."}]}
;;;
;;;   - total counts every test that ran (pass + fail + xpass + xfail
;;;     + skip); failed counts fail + xpass.  Verdict is "pass" iff
;;;     failed = 0.  Do not author test-skip/test-expect-fail specs:
;;;     the gate additionally requires passed = total.
;;;   - kind "property": a define-contract-spec quickcheck property.
;;;     On falsification the record carries the quickcheck seed and
;;;     the counterexample binding values (parsed from quickcheck's
;;;     "Seed:" / "name = value" falsification output) in inputs.
;;;     If the property body raised, exception holds the Guile
;;;     exception as (key . irritants), rendered with ~s — the first
;;;     element is the Guile exception key.
;;;   - kind "export": an export-boundary reflection failure; the
;;;     human reason (e.g. "missing export: foo") is in exception.
;;;   - kind "example": any other srfi-64 test in the spec.
;;;
;;; SEEDING
;;; =======
;;; spec-begin deterministically seeds quickcheck via
;;; (configure-quickcheck (seed N)): N is $BADB_SPEC_SEED when set to
;;; a non-negative exact integer, else the fixed default 42.  The seed
;;; used is included in the emitted JSON, so a failed suite replays
;;; identically.
;;;
;;; SRFI-253 BOUNDARY CONTRACTS
;;; ===========================
;;; (define-checked-wrapper (wrapper underlying) ((arg pred) ...))
;;; derives WRAPPER from an exported procedure UNDERLYING, checking
;;; each argument with SRFI-253 check-arg (raising &assertion on
;;; violation) before delegating.  Use it inside a spec to test the
;;; runtime boundary of declared exports:
;;;
;;;     (define-checked-wrapper (checked-proc1 proc1) ((xs list?)))
;;;     (test-assert "proc1 boundary rejects malformed input"
;;;       (catch #t
;;;         (lambda () (checked-proc1 42) #f)
;;;         (lambda args #t)))
;;;
;;; Failure records carry no backtrace (srfi-64 catches exceptions
;;; internally, so none is observable at the hook); the field is
;;; emitted empty for contract fidelity.

(define-module (badb spec)
  #:use-module (ice-9 format)
  #:use-module (json)
  #:use-module (quickcheck)
  #:use-module (quickcheck property)
  #:use-module (srfi srfi-253)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:export (define-contract-spec
            define-module-spec
            define-checked-wrapper
            spec-begin
            spec-end
            spec-exit
            spec-suite-active?
            spec-failure-count
            spec-seed
            spec-seed-string
            spec-bundle-path
            spec-bundle-ref
            spec-check-module-exports
            spec-run-property
            spec-parse-falsified-output
            spec-json-safe))

;;;
;;; Suite state (sequential: one active suite per process / test)
;;;
(define %spec-runner #f)
(define %spec-saved-runner #f)
(define %spec-suite-name #f)
(define %spec-seed-value #f)
(define %spec-failures #f)
(define %spec-file-path #f)

(define (spec-seed)
  "Return the numeric quickcheck seed installed by spec-begin."
  %spec-seed-value)

(define (spec-seed-string)
  "Return the quickcheck seed as the string form required by the
kernel gate's JSON contract."
  (if (number? %spec-seed-value)
      (number->string %spec-seed-value)
      ""))

(define (spec-failure-count)
  "Return the failure count of the last completed suite (#f before
the first spec-end)."
  %spec-failures)

(define (spec-suite-active?)
  "Return #t between a spec-begin and its spec-end."
  (and %spec-runner #t))

(define (spec-bundle-path)
  "Return the directory containing the spec file currently being
executed (i.e. the file that called spec-begin), or (getcwd) if
that cannot be determined."
  (if (and %spec-file-path (string? %spec-file-path) (not (string-null? %spec-file-path)))
      (dirname %spec-file-path)
      (getcwd)))

(define (spec-bundle-ref rel-path)
  "Return REL-PATH resolved against (spec-bundle-path).  Returns the
joined path string; it does NOT require the target to exist."
  (string-append (spec-bundle-path) "/" rel-path))

;;;
;;; Quickcheck falsification output parsing
;;;
;;; quickcheck prints, on falsification:
;;;   Falsifiable after N tests.
;;;   Seed: S
;;;   name = value        (one line per property binding, ~s = ~s)
;;; The property body may also print into the captured port, so only
;;; well-formed "name = value" lines (datum, " = ", datum, EOF) and
;;; "Seed: " lines are extracted.

(define (%read-datum text)
  (let* ((port (open-input-string text))
         (value (read port)))
    value))

(define (spec-parse-falsified-output text)
  "Parse quickcheck falsification output TEXT into
((seed . SEED-STRING) (inputs . #(VALUE ...))).  Lines that are not a
well-formed binding or seed declaration are ignored."
  (let loop ((lines (string-split text #\newline))
             (seed #f)
             (inputs '()))
    (cond
     ((null? lines)
      `((seed . ,(if seed seed ""))
        (inputs . ,(list->vector (reverse inputs)))))
     ((string-prefix? "Seed: " (car lines))
      (loop (cdr lines)
            (string-trim-both (substring (car lines) 6))
            inputs))
     (else
      (let ((idx (string-contains (car lines) " = ")))
        (if idx
            (let* ((name (%read-datum (substring (car lines) 0 idx)))
                   (value-text (substring (car lines) (+ idx 3))))
              (if (symbol? name)
                  (let* ((check (open-input-string value-text))
                         (value (read check)))
                    ;; Well-formed binding: one datum consuming the
                    ;; whole remainder of the line.
                    (if (and (not (eof-object? value))
                             (eof-object? (read check)))
                        (loop (cdr lines) seed (cons value inputs))
                        (loop (cdr lines) seed inputs)))
                  (loop (cdr lines) seed inputs)))
            (loop (cdr lines) seed inputs)))))))

;;;
;;; JSON-safe value conversion
;;;
;;; guile-json serializes only strings/numbers/booleans/nested
;;; alists-and-vectors; counterexample values are arbitrary Scheme
;;; data, so symbols become strings, proper lists and vectors become
;;; vectors, and everything else falls back to its ~s rendering.

(define (spec-json-safe value)
  "Convert VALUE into a form guile-json can serialize: strings,
numbers, and booleans pass through, symbols become strings, vectors
and proper lists become vectors (recursively), and any other value is
rendered with ~s."
  (cond
   ((or (string? value) (number? value) (boolean? value)) value)
   ((symbol? value) (symbol->string value))
   ((char? value) (format #f "~s" value))
   ((vector? value) (list->vector (map spec-json-safe (vector->list value))))
   ((list? value) (list->vector (map spec-json-safe value)))
   (else (format #f "~s" value))))

;;;
;;; Property execution with structured capture
;;;
;;; The tested property's quickcheck output (including the "OK,
;;; passed N tests" line on success) is captured on a string port so
;;; suite stdout stays clean; on falsification the seed and the
;;; counterexample binding values are stashed on the current srfi-64
;;; result for the runner's failure record.

(define (spec-run-property thunk)
  "Run THUNK (which must call quickcheck on a property) under output
capture, stashing the quickcheck seed and counterexample inputs on
the current srfi-64 test result when the property falsifies.  Returns
the quickcheck boolean."
  (test-result-set! (test-runner-get) 'spec-kind "property")
  (let* ((port (open-output-string))
         (result (parameterize ((current-output-port port)) (thunk))))
    (if result
        result
        (let* ((parsed (spec-parse-falsified-output
                        (get-output-string port)))
               (seed (or (assoc-ref parsed 'seed) ""))
               (inputs (assoc-ref parsed 'inputs)))
          (unless (string-null? seed)
            (test-result-set! (test-runner-get) 'spec-seed seed))
          (test-result-set! (test-runner-get) 'spec-inputs inputs)
          #f))))

;;;
;;; Export-boundary reflection check
;;;
;;; Declared exports are compared against the module's public
;;; bindings via resolve-interface/module-map/procedure-minimum-arity.
;;; Every violation is recorded as a structured failure (kind
;;; "export") in the suite, never as an exception escape: an
;;; unresolvable module is a single structured failure.

(define (%normalize-export-decl decl)
  "Normalize DECL — NAME, (NAME MIN), or (NAME MIN MAX) — into
(NAME MIN MAX).  Returns #f for malformed declarations."
  (cond
   ((symbol? decl) (list decl 0 #f))
   ((and (pair? decl) (symbol? (car decl))
         (pair? (cdr decl)) (exact-integer? (cadr decl)))
    (cond
     ((null? (cddr decl)) (list (car decl) (cadr decl) #f))
     ((and (null? (cdddr decl)) (exact-integer? (caddr decl)))
      (list (car decl) (cadr decl) (caddr decl)))
     (else #f)))
   (else #f)))

(define (%min-arity-parts value)
  "Return the arity of procedure VALUE as (REQUIRED OPTIONAL
VARIADIC?).  procedure-minimum-arity's return convention has varied
across Guile versions (single list value on Guile 3.0.9, multiple
values elsewhere); both are normalized here."
  (let ((arity (procedure-minimum-arity value)))
    (cond
     ((and (list? arity) (= 3 (length arity))) arity)
     ((number? arity)
      (call-with-values
          (lambda () (procedure-minimum-arity value))
        (lambda (required . rest) (list required (car rest) (cadr rest)))))
     (else (list arity #f #f)))))

(define (%export-decl-violation exports lookup name min-arity max-arity)
  "Return a human-readable violation reason for declared export
NAME, or #f when the binding is exported, a procedure, and its arity
satisfies the declared MIN-ARITY and optional MAX-ARITY."
  (cond
   ((not (memq name exports))
    (format #f "missing export: ~s" name))
   (else
    (let ((value (lookup name)))
      (cond
       ((not (procedure? value))
        (format #f "export ~s is not a procedure: ~s" name value))
       (else
        (let* ((parts (%min-arity-parts value))
               (required (car parts))
               (optional (cadr parts))
               (variadic? (caddr parts)))
          (cond
           ((> min-arity required)
            (format #f "wrong arity for ~s: requires at least ~a, declared minimum ~a"
                    name required min-arity))
           ((and max-arity (not variadic?) (< max-arity (+ required optional)))
            (format #f "wrong arity for ~s: accepts at most ~a, declared maximum ~a"
                    name (+ required optional) max-arity))
           (else #f)))))))))

(define (spec-check-module-exports module-name raw-decls)
  "Run the export-boundary reflection check for MODULE-NAME against
RAW-DECLS (NAME | (NAME MIN) | (NAME MIN MAX) forms), emitting one
srfi-64 test per declared export plus one for undeclared extra
exports — or a single structured failure when the module cannot be
resolved or a declaration is malformed.  All failures carry kind
\"export\" and their reason in the exception field."
  (let* ((decls (map %normalize-export-decl raw-decls))
         (malformed (filter (lambda (d) (not d)) decls))
         (iface (catch #t
                  (lambda () (resolve-interface module-name))
                  (lambda args #f))))
    (for-each
     (lambda (bad)
       (test-assert
           (format #f "~a export declaration is well-formed" module-name)
         (begin
           (test-result-set! (test-runner-get) 'spec-kind "export")
           (test-result-set! (test-runner-get)
             'spec-reason (format #f "malformed export declaration: ~s" bad))
           #f)))
     malformed)
    (if iface
        (let* ((exports (module-map (lambda (name var) name) iface))
               (lookup (lambda (name) (module-ref iface name))))
          (for-each
           (lambda (decl)
             (let ((name (car decl))
                   (min-arity (cadr decl))
                   (max-arity (caddr decl)))
               (test-assert
                   (format #f "~a exports ~a" module-name name)
                 (begin
                   (test-result-set! (test-runner-get) 'spec-kind "export")
                   (let ((reason (%export-decl-violation exports lookup
                                                         name min-arity
                                                         max-arity)))
                     (if reason
                         (begin
                           (test-result-set! (test-runner-get)
                             'spec-reason reason)
                           #f)
                         #t))))))
           (filter (lambda (d) d) decls))
          (test-assert
              (format #f "~a exports nothing undeclared" module-name)
            (begin
              (test-result-set! (test-runner-get) 'spec-kind "export")
              (let* ((declared (map car (filter (lambda (d) d) decls)))
                     (extras (filter (lambda (e) (not (memq e declared)))
                                     exports)))
                (if (pair? extras)
                    (begin
                      (test-result-set!
                       (test-runner-get)
                       'spec-reason
                       (format #f "undeclared exports: ~s" extras))
                      #f)
                    #t)))))
        (test-assert
            (format #f "module ~a resolves" module-name)
          (begin
            (test-result-set! (test-runner-get) 'spec-kind "export")
            (test-result-set!
             (test-runner-get)
             'spec-reason
             (format #f "module ~s could not be resolved" module-name))
            #f)))))

;;;
;;; Structured srfi-64 runner
;;;
;;; A silent (test-runner-null based) runner whose on-test-end hook
;;; captures, per failed test, the structured record required by the
;;; gate's JSON contract.  Per-suite state rides in aux-value as
;;; (REVERSED-FAILURE-RECORDS).

(define (%spec-failure-record runner)
  (let* ((err (test-result-ref runner 'actual-error #f))
         (reason (test-result-ref runner 'spec-reason #f))
         (exception
          (cond
           (err (format #f "~s" err))
           (reason reason)
           (else ""))))
    `((name . ,(test-runner-test-name runner))
      (kind . ,(or (test-result-ref runner 'spec-kind #f) "example"))
      (seed . ,(or (test-result-ref runner 'spec-seed #f) (spec-seed-string)))
      (inputs . ,(spec-json-safe
                  (or (test-result-ref runner 'spec-inputs #f) #())))
      (exception . ,exception)
      (backtrace . ,(or (test-result-ref runner 'spec-backtrace #f) "")))))

(define (%spec-record-suite-failure! runner name-string)
  "Record a suite-level structured failure NAME-STRING (used for name
or count mismatches, which are not counted by srfi-64 itself) and
bump the runner's fail count so the verdict stays honest."
  (test-runner-fail-count! runner (+ 1 (test-runner-fail-count runner)))
  (let ((state (test-runner-aux-value runner)))
    (set-car!
     state
     (cons `((name . ,name-string)
             (kind . "example")
             (seed . ,(spec-seed-string))
             (inputs . #())
             (exception . "")
             (backtrace . ""))
           (car state)))))

(define (%make-spec-runner)
  (let ((runner (test-runner-null)))
    (test-runner-aux-value! runner (list '()))
    (test-runner-on-test-end!
     runner
     (lambda (r)
       (when (memq (test-result-kind r) '(fail xpass))
         (let ((state (test-runner-aux-value r)))
           (set-car! state (cons (%spec-failure-record r) (car state)))))))
    (test-runner-on-bad-end-name!
     runner
     (lambda (r begin-name end-name)
       (%spec-record-suite-failure!
        r
        (format #f "suite name mismatch: ~s vs ~s" begin-name end-name))))
    (test-runner-on-bad-count!
     runner
     (lambda (r actual expected)
       (%spec-record-suite-failure!
        r
        (format #f "test count mismatch: ~a ran, ~a declared" actual expected))))
    runner))

;;;
;;; Suite entry points
;;;
(define (%spec-resolve-seed)
  "Return the quickcheck seed to install: $BADB_SPEC_SEED when it is
a non-negative exact integer, else the fixed default 42."
  (let* ((env (getenv "BADB_SPEC_SEED"))
         (n (and env (false-if-exception (string->number env)))))
    (if (and n (exact-integer? n) (>= n 0))
        n
        42)))

(define (%spec-source-filename)
        "Fallback filename for a spec executed as a script: (current-filename) returns #f when the spec file lives under a directory on the module load path (e.g. bundle specs under src/ run with -L src), and the macro-side (current-filename) already captured the call-site value; this covers that #f case via the script entry from (command-line)."
        (let
            ((script (and (pair? (command-line)) (car (command-line)))))
          (and (string? script)
               (>= (string-length script) 4)
               (string= ".scm" (substring script (- (string-length script) 4)))
               (file-exists? script)
               script)))

(define (%spec-begin suite-name filename)
        "Internal procedure: open spec suite SUITE-NAME with FILENAME."
        (when (spec-suite-active?)
          (error "spec-begin called with a suite already active" suite-name))
        (set! %spec-file-path (or filename (%spec-source-filename)))
        (set! %spec-seed-value (%spec-resolve-seed))
        (configure-quickcheck (seed %spec-seed-value))
        (set! %spec-suite-name suite-name)
        (set! %spec-saved-runner (test-runner-current))
        (let ((runner (%make-spec-runner)))
          (set! %spec-runner runner)
          (test-runner-current runner)
          (test-begin suite-name)))

;;
;; Open spec suite SUITE-NAME: seed quickcheck deterministically
;; ($BADB_SPEC_SEED or the default 42), install the structured runner,
;; and begin the srfi-64 group.  Raises when a suite is already active.
;; Captures (current-filename) at the call site to determine the spec
;; file's directory for spec-bundle-path.
(define-syntax spec-begin
               (syntax-rules () ((_ suite-name) (%spec-begin suite-name (current-filename)))))

(define (spec-end)
  "Close the active spec suite: end the srfi-64 group, emit the JSON
diagnostics line (the kernel gate's contract) as the FINAL line of
stdout, restore the previous runner, and return the failure count."
  (unless (spec-suite-active?)
    (error "spec-end called without a matching spec-begin"))
  (let* ((runner %spec-runner)
         (suite-name %spec-suite-name))
    (test-end suite-name)
    (let* ((failed (+ (test-runner-fail-count runner)
                      (test-runner-xpass-count runner)))
           (passed (test-runner-pass-count runner))
           (total (+ passed failed
                     (test-runner-xfail-count runner)
                     (test-runner-skip-count runner)))
           (records (reverse (car (test-runner-aux-value runner))))
           (json (scm->json-string
                  `((summary . ((total . ,total)
                                (passed . ,passed)
                                (failed . ,failed)))
                    (verdict . ,(if (zero? failed) "pass" "fail"))
                    (seed . ,(spec-seed-string))
                    (failures . ,(list->vector records))))))
      (set! %spec-failures failed)
      (set! %spec-file-path #f)
      (set! %spec-runner #f)
      (set! %spec-suite-name #f)
      (test-runner-current %spec-saved-runner)
      (set! %spec-saved-runner #f)
      (display json (current-output-port))
      (newline (current-output-port))
      failed)))

(define (spec-exit)
  "Exit the spec process with status 0 when the last completed suite
had zero failures, 1 otherwise (fail-closed before any spec-end)."
  (exit (if (and (number? %spec-failures) (zero? %spec-failures)) 0 1)))

;;;
;;; Declarative contract DSL
;;;
;; (define-contract-spec proc ((arg arb) ...) -> (result-var post))
;; expands to an srfi-64 test-assert, named after PROC by default,
;; that runs (quickcheck (property ((arg arb) ...)
;;                        (let ((result-var (proc arg ...))) post)))
;; under structured output capture.  An optional #:name LABEL (before
;; or after the -> clause) overrides the test name for diagnostics.
(define-syntax define-contract-spec
  (syntax-rules (->)
    ((_ proc ((arg arb) ...) #:name label -> (result-var post-condition))
     (test-assert label
       (spec-run-property
        (lambda ()
          (quickcheck
           (property ((arg arb) ...)
             (let ((result-var (proc arg ...)))
               post-condition)))))))
    ((_ proc ((arg arb) ...) -> (result-var post-condition) #:name label)
     (define-contract-spec proc ((arg arb) ...) #:name label
       -> (result-var post-condition)))
    ((_ proc ((arg arb) ...) -> (result-var post-condition))
     (define-contract-spec proc ((arg arb) ...)
       #:name (symbol->string 'proc)
       -> (result-var post-condition)))))

;; (define-module-spec module-name (export-clause ...)) immediately
;; performs the export-boundary reflection check for MODULE-NAME
;; against EXPORT-CLAUSEs (NAME | (NAME MIN) | (NAME MIN MAX)),
;; recording violations as structured failures of kind "export".
(define-syntax define-module-spec
  (syntax-rules ()
    ((_ module-name (export-clause ...))
     (spec-check-module-exports 'module-name '(export-clause ...)))))

;; (define-checked-wrapper (wrapper underlying) ((arg pred) ...))
;; defines WRAPPER as a SRFI-253 runtime-boundary wrapper around the
;; exported procedure UNDERLYING: each argument is checked with
;; check-arg (raising &assertion on violation) before delegating.
(define-syntax define-checked-wrapper
  (syntax-rules ()
    ((_ (wrapper underlying) ((arg pred) ...))
     (define (wrapper arg ...)
       (begin (check-arg pred arg 'wrapper) ...)
       (underlying arg ...)))))
