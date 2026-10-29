;;; test/test-spec.scm — (badb spec) declarative contract engine tests.
;;;
;;; Covers the DSL macros (expansion structure), the export-boundary
;;; reflection check (missing/extra/non-procedure exports, wrong
;;; arity, unresolvable module, malformed declarations), the
;;; structured runner (property/example/export failure records,
;;; exception keys, clean stdout), quickcheck seed determinism and
;;; BADB_SPEC_SEED handling, the emitted JSON diagnostics shape, and
;;; the end-to-end fixture runs through the kernel spec gate
;;; ((badb spec-gate)) — the full Phase 2 loop.

(define-module (test test-spec)
  #:use-module (badb spec)
  #:use-module (badb spec-gate)
  #:use-module (ice-9 format)
  #:use-module (json)
  #:use-module (language tree-il)
  #:use-module (quickcheck)
  #:use-module (quickcheck arbitrary)
  #:use-module (quickcheck property)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (system base compile))

;;;
;;; Helpers
;;;

;; Run (spec-begin NAME), THUNK, (spec-end) with stdout captured so
;; the harness output stays clean; return (FAILURES . CAPTURED-TEXT).
;; spec-end always runs, so the installed runner is always restored.
(define (capture-suite name thunk)
  (let ((port (open-output-string))
        (failures #f))
    (parameterize ((current-output-port port))
      (dynamic-wind
        (lambda () (spec-begin name))
        (lambda () (set! failures (thunk)))
        (lambda ()
          (when (spec-suite-active?) (set! failures (spec-end))))))
    (cons failures (get-output-string port))))

;; Parse the diagnostics JSON (the last non-empty captured line) into
;; the string-keyed alist the kernel gate sees.
(define (suite-json text)
  (json-string->scm (last (nonempty-lines text))))

(define (nonempty-lines text)
  (filter (lambda (line) (not (string-null? (string-trim-both line))))
          (string-split text #\newline)))

(define (json-failures json)
  (let ((raw (assoc-ref json "failures")))
    (if (vector? raw) (vector->list raw) '())))

;; Macro expansion to a printable Scheme rendering (full tree-il).
(define (expand->string expr)
  (format #f "~s" (tree-il->scheme (macroexpand expr (current-module)))))

;; Run an export-boundary reflection suite; return
;; (FAILURE-COUNT . FIRST-FAILURE-RECORD-OR-#f).
(define (reflection-result module-name decls)
  (let* ((out (capture-suite "reflection"
                (lambda ()
                  (spec-check-module-exports module-name decls))))
         (failures (json-failures (suite-json (cdr out)))))
    (cons (car out) (if (null? failures) #f (car failures)))))

;; Set env VAR to VALUE (or unset it when VALUE is #f) around THUNK,
;; restoring the ambient value.
(define (with-env var value thunk)
  (let ((saved (getenv var)))
    (dynamic-wind
      (lambda () (if value (setenv var value) (unsetenv var)))
      thunk
      (lambda () (if saved (setenv var saved) (unsetenv var))))))

;; Procedures under contract inside the in-process suites.
(define (spec-double x) (+ x x))
(define (spec-boom x) (car '()))

;; The harness runner identity, captured before any suite runs, to
;; verify that spec-end restores it.
(define %harness-runner (test-runner-current))

;; A falsifying contract suite, run twice for the determinism test.
(define (run-falsifying-suite)
  (capture-suite "falsify"
    (lambda ()
      (define-contract-spec spec-double
        ((x $integer))
        -> (result (= result (+ x x 1)))))))

;;;
;;; Workspace for the end-to-end gate runs
;;;

(define %temp-dir #f)
(define %root #f)

(test-begin "spec")

;; Save the ambient gate/seed environment once, up front: several
;; groups below set and restore them.
(define %saved-gate-root (getenv "BADB_SPEC_GATE_ROOT"))
(define %saved-seed (getenv "BADB_SPEC_SEED"))

;;;
;;; Macro expansion structure
;;;

(define %contract-expansion
  (expand->string
   '(define-contract-spec spec-double
      ((x $integer))
      -> (result (= result (+ x x))))))

(define %named-expansion
  (expand->string
   '(define-contract-spec spec-double
      ((x $integer))
      #:name "custom label"
      -> (result (= result (+ x x))))))

(define %trailing-expansion
  (expand->string
   '(define-contract-spec spec-double
      ((x $integer))
      -> (result (= result (+ x x)))
      #:name "trailing label")))

(define %module-expansion
  (expand->string
   '(define-module-spec (test fixtures lib math-utils)
      ((double 1) (clamp 3 3)))))

(define %wrapper-expansion
  (expand->string
   '(define-checked-wrapper (checked-car car) ((x pair?)))))

(test-group "macro-expansion"
  (test-assert "define-contract-spec expands through the structured runner"
    (string-contains %contract-expansion "spec-run-property"))
  (test-assert "define-contract-spec runs quickcheck on the property"
    (string-contains %contract-expansion "quickcheck"))
  (test-assert "the postcondition sees the result binding"
    (string-contains %contract-expansion "(let ((result"))
  (test-assert "the default test name is the procedure name"
    (string-contains %contract-expansion "spec-double"))
  (test-assert "the optional #:name label reaches the test name"
    (string-contains %named-expansion "custom label"))
  (test-assert "the trailing #:name form is also supported"
    (string-contains %trailing-expansion "trailing label"))
  (test-assert "define-module-spec expands to the reflection check"
    (string-contains %module-expansion "spec-check-module-exports"))
  (test-assert "the module name is quoted into the check"
    (string-contains %module-expansion "test fixtures lib math-utils"))
  (test-assert "define-checked-wrapper expands to SRFI-253 checks"
    (string-contains %wrapper-expansion "assertion-violation")))

;;;
;;; Quickcheck falsification output parsing (unit)
;;;

(define %parsed-multi
  (spec-parse-falsified-output
   "Falsifiable after 3 tests.\nSeed: 7\nxs = (1 2 3)\nn = 5\n"))

(define %parsed-string-value
  (spec-parse-falsified-output
   "noise line\nSeed: 9\ns = \"a = b\"\nx = -1\n"))

(define %parsed-give-up
  (spec-parse-falsified-output "Gave up! Passed only 0 tests.\n"))

(test-group "falsification-parse"
  (test-equal "the seed line is extracted"
    "7" (assoc-ref %parsed-multi 'seed))
  (test-equal "the counterexample bindings parse in order"
    #((1 2 3) 5) (assoc-ref %parsed-multi 'inputs))
  (test-equal "string values containing \" = \" stay intact"
    #("a = b" -1) (assoc-ref %parsed-string-value 'inputs))
  (test-equal "a give-up has no seed"
    "" (assoc-ref %parsed-give-up 'seed))
  (test-equal "a give-up has no inputs"
    #() (assoc-ref %parsed-give-up 'inputs)))

;;;
;;; JSON-safe conversion
;;;

(test-group "json-safe"
  (test-equal "symbols become strings" "abc" (spec-json-safe 'abc))
  (test-equal "strings pass through" "s" (spec-json-safe "s"))
  (test-equal "booleans pass through" #t (spec-json-safe #t))
  (test-equal "proper lists become vectors"
    #(1 "two" #t) (spec-json-safe '(1 two #t)))
  (test-equal "vectors convert recursively"
    #(#(1 2) "x") (spec-json-safe #((1 2) x)))
  (test-assert "other values render with ~s"
    (string=? "#\\a" (spec-json-safe #\a))))

;;;
;;; Export-boundary reflection check
;;;

(define %r-correct
  (reflection-result '(test fixtures lib math-utils) '((double 1) (clamp 3 3))))

(define %r-missing
  (reflection-result '(test fixtures lib math-utils)
                     '((double 1) (nonexistent-thing 1))))

(define %r-arity
  (reflection-result '(test fixtures lib math-utils)
                    '((double 1) (clamp 4))))

(define %r-nonproc
  (reflection-result '(test fixtures lib nonproc-fixture) '((a-number 0))))

(define %r-extra
  (reflection-result '(test fixtures lib math-utils) '((double 1))))

(define %r-unresolvable
  (reflection-result '(no such module anywhere) '((thing 1))))

(define %r-malformed
  (reflection-result '(test fixtures lib math-utils)
                     '((clamp "not-a-number"))))

(test-group "export-boundary-reflection"
  (test-equal "a correctly declared module boundary passes"
    0 (car %r-correct))
  (test-equal "a missing export records a failure (plus the now-undeclared extra)"
    2 (car %r-missing))
  (test-assert "the failure kind is export"
    (string=? "export" (assoc-ref (cdr %r-missing) "kind")))
  (test-assert "the reason names the missing export"
    (string-contains (assoc-ref (cdr %r-missing) "exception")
                     "missing export"))
  (test-equal "a wrong minimum arity records a failure"
    1 (car %r-arity))
  (test-assert "the reason reports the arity mismatch"
    (string-contains (assoc-ref (cdr %r-arity) "exception")
                     "wrong arity"))
  (test-equal "a non-procedure export records a failure (plus the undeclared extra)"
    2 (car %r-nonproc))
  (test-assert "the reason reports the non-procedure"
    (string-contains (assoc-ref (cdr %r-nonproc) "exception")
                     "not a procedure"))
  (test-equal "an undeclared extra export records a failure"
    1 (car %r-extra))
  (test-assert "the reason names the undeclared exports"
    (string-contains (assoc-ref (cdr %r-extra) "exception")
                     "undeclared exports"))
  (test-equal "an unresolvable module records a single failure"
    1 (car %r-unresolvable))
  (test-assert "the failure is structured, not an escape"
    (string-contains (assoc-ref (cdr %r-unresolvable) "exception")
                     "could not be resolved"))
  (test-equal "a malformed declaration records failures, not a crash"
    2 (car %r-malformed))
  (test-assert "the malformed declaration reason is recorded"
    (string-contains (assoc-ref (cdr %r-malformed) "exception")
                     "malformed export declaration")))

;;;
;;; Structured runner
;;;

(define %pass-out
  (capture-suite "passing-suite"
    (lambda ()
      (define-contract-spec spec-double
        ((x $integer))
        -> (result (= result (+ x x)))))))

(define %fail-out (run-falsifying-suite))

(define %throw-out
  (capture-suite "throwing-property"
    (lambda ()
      (define-contract-spec spec-boom
        ((x $integer))
        -> (result #t)))))

(define %example-out
  (capture-suite "example-kind"
    (lambda ()
      (test-assert "plain example" #f))))

(define %pass-json (suite-json (cdr %pass-out)))
(define %fail-json (suite-json (cdr %fail-out)))
(define %fail-record (car (json-failures %fail-json)))
(define %throw-record (car (json-failures (suite-json (cdr %throw-out)))))
(define %example-record
  (car (json-failures (suite-json (cdr %example-out)))))

(test-group "structured-runner"
  (test-equal "a passing property suite reports zero failures"
    0 (car %pass-out))
  (test-equal "the verdict is pass"
    "pass" (assoc-ref %pass-json "verdict"))
  (test-equal "the summary counts the contract"
    1 (assoc-ref (assoc-ref %pass-json "summary") "total"))
  (test-equal "suite stdout is exactly the diagnostics line"
    1 (length (nonempty-lines (cdr %pass-out))))
  (test-equal "a falsified property records one failure"
    1 (car %fail-out))
  (test-equal "the failure kind is property"
    "property" (assoc-ref %fail-record "kind"))
  (test-equal "the record carries the quickcheck seed"
    "42" (assoc-ref %fail-record "seed"))
  (test-assert "the record carries the counterexample inputs"
    (= 1 (vector-length (assoc-ref %fail-record "inputs"))))
  (test-equal "an exception in the property body records a failure"
    1 (car %throw-out))
  (test-assert "the record captures the Guile exception key"
    (string-contains (assoc-ref %throw-record "exception")
                     "wrong-type-arg"))
  (test-equal "a plain srfi-64 failure records kind example"
    "example" (assoc-ref %example-record "kind")))

(test-group "structured-runner-restore"
  (capture-suite "restore-suite" (lambda () #f))
  (test-assert "the harness runner is restored after spec-end"
    (eq? (test-runner-current) %harness-runner)))

;;;
;;; Seed determinism
;;;

(define %determinism-first (run-falsifying-suite))
(define %determinism-second (run-falsifying-suite))
(define %determinism-record-1
  (car (json-failures (suite-json (cdr %determinism-first)))))
(define %determinism-record-2
  (car (json-failures (suite-json (cdr %determinism-second)))))

(test-group "seed-determinism"
  (test-equal "the same seed reproduces the same counterexample"
    (assoc-ref %determinism-record-1 "inputs")
    (assoc-ref %determinism-record-2 "inputs"))
  (test-equal "the seed is reported in both runs"
    (assoc-ref %determinism-record-1 "seed")
    (assoc-ref %determinism-record-2 "seed"))
  (with-env "BADB_SPEC_SEED" "1234"
    (lambda ()
      (define json (suite-json (cdr (capture-suite "env-seed"
                                                   (lambda () #f)))))
      (test-equal "BADB_SPEC_SEED selects the quickcheck seed"
        1234 (spec-seed))
      (test-equal "the env seed reaches the JSON"
        "1234" (assoc-ref json "seed"))))
  (with-env "BADB_SPEC_SEED" "not-a-number"
    (lambda ()
      (capture-suite "bad-env-seed" (lambda () #f))
      (test-equal "an unparseable BADB_SPEC_SEED falls back to 42"
        42 (spec-seed)))))

;;;
;;; Emitted JSON diagnostics shape (the kernel gate contract)
;;;

(define %shape-out
  (capture-suite "shape"
    (lambda ()
      (spec-check-module-exports
       '(test fixtures lib math-utils)
       '((double 1) (clamp 4)))
      (define-contract-spec spec-double
        ((x $integer))
        -> (result (= result (+ x x 1))))
      (test-assert "plain example" #f))))

(define %shape-json (suite-json (cdr %shape-out)))
(define %shape-failures (json-failures %shape-json))

(test-group "json-shape"
  (for-each
   (lambda (key)
     (test-assert (format #f "the JSON carries the ~a key" key)
       (assoc-ref %shape-json key)))
   '("summary" "verdict" "seed" "failures"))
  (for-each
   (lambda (key)
     (test-assert (format #f "the summary carries ~a" key)
       (number? (assoc-ref (assoc-ref %shape-json "summary") key))))
   '("total" "passed" "failed"))
  (test-assert "failures is a JSON array"
    (vector? (assoc-ref %shape-json "failures")))
  (test-equal "all three failure kinds appear in one suite"
    '("export" "property" "example")
    (map (lambda (record) (assoc-ref record "kind")) %shape-failures))
  (for-each
   (lambda (record)
     (for-each
      (lambda (key)
        (test-assert (format #f "failure record carries ~a" key)
          (assoc-ref record key)))
      '("name" "kind" "seed" "inputs" "exception" "backtrace")))
   %shape-failures))

;;;
;;; SRFI-253 checked wrappers
;;;

(define-checked-wrapper (checked-car car) ((x pair?)))

(test-group "checked-wrappers"
  (test-assert "the wrapper delegates to the underlying export"
    (equal? 'hello (checked-car '(hello . world))))
  (test-assert "the wrapper rejects arguments violating the predicate"
    (catch #t
      (lambda () (checked-car 42) #f)
      (lambda args #t))))

;;;
;;; End-to-end through the kernel spec gate
;;;

(set! %temp-dir (mkdtemp "/tmp/badb-spec-test-XXXXXX"))
(set! %root (string-append %temp-dir "/gate-root"))
(setenv "BADB_SPEC_GATE_ROOT" %root)
(setenv "BADB_SPEC_SEED" "42")

(lock-spec "test/fixtures/spec-pass.scm" "spec-e2e-pass")
(define %gate-pass (verify-locked-spec "spec-e2e-pass" #:timeout 60))

(lock-spec "test/fixtures/spec-fail.scm" "spec-e2e-fail")
(define %gate-fail (verify-locked-spec "spec-e2e-fail" #:timeout 60))

(lock-spec "test/fixtures/spec-export-only.scm" "spec-e2e-export-only")
(define %gate-export-only
  (verify-locked-spec "spec-e2e-export-only" #:timeout 60))

(test-group "gate-end-to-end"
  (test-equal "the pass fixture verdicts pass through the kernel gate"
    "pass" (assoc-ref %gate-pass 'verdict))
  (test-equal "the gate observed exit status 0"
    0 (assoc-ref %gate-pass 'exit-status))
  (test-equal "the gate parsed the quickcheck seed"
    "42" (assoc-ref %gate-pass 'seed))
  (test-equal "the gate parsed the suite summary"
    0 (assoc-ref %gate-pass 'summary-failed))
  (test-equal "the fail fixture verdicts fail through the kernel gate"
    "fail" (assoc-ref %gate-fail 'verdict))
  (test-equal "the gate observed the nonzero exit status"
    1 (assoc-ref %gate-fail 'exit-status))
  (test-equal "the gate parsed the failed count"
    2 (assoc-ref %gate-fail 'summary-failed))
  (test-assert "the gate reports the failure reason"
    (not (string-null? (assoc-ref %gate-fail 'reason))))
  (test-equal "the export-only fixture verdicts pass"
    "pass" (assoc-ref %gate-export-only 'verdict)))

(test-end "spec")

;;;
;;; Teardown: drop the locked copies and the temp workspace, restore
;;; the environment.
;;;

(for-each
 (lambda (fid)
   (let ((path (locked-spec-path fid %root)))
     (when (file-exists? path)
       (chmod path #o644)
       (delete-file path))))
 '("spec-e2e-pass" "spec-e2e-fail" "spec-e2e-export-only"))
(catch #t
  (lambda ()
    (rmdir (string-append %root "/specs"))
    (rmdir %root)
    (rmdir %temp-dir))
  (lambda args #f))
(if %saved-gate-root
    (setenv "BADB_SPEC_GATE_ROOT" %saved-gate-root)
    (unsetenv "BADB_SPEC_GATE_ROOT"))
(if %saved-seed
    (setenv "BADB_SPEC_SEED" %saved-seed)
    (unsetenv "BADB_SPEC_SEED"))
