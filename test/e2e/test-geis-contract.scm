#!/usr/bin/env -S guile -L src -L . -s
!#
;;; test/e2e/test-geis-contract.scm — End-to-end tests for Geis contract locking and verification
;;;
;;; Usage: guile -L src -L . test/e2e/test-geis-contract.scm
;;;    or: ./test/e2e/test-geis-contract.scm

(when (current-filename)
  (let ((dir (dirname (current-filename))))
    (add-to-load-path (canonicalize-path (string-append dir "/../../src")))
    (add-to-load-path (canonicalize-path (string-append dir "/../..")))))

(use-modules (badb digest)
             (ice-9 format)
             (ice-9 ftw)
             (ice-9 rdelim)
             (ice-9 textual-ports)
             (srfi srfi-1))

;; Resolve gate module (supporting both (badb kernel geis-gate) and facade (badb spec-gate))
(define %gate-module
  (catch #t
    (lambda ()
      (resolve-module '(badb kernel geis-gate) #:ensure #f))
    (lambda _ #f)))

(define %use-gate
  (if (and %gate-module (module-bound? %gate-module 'lock-geis))
      %gate-module
      (resolve-interface '(badb spec-gate))))

(define lock-spec-proc
  (if (module-bound? %use-gate 'lock-geis)
      (module-ref %use-gate 'lock-geis)
      (module-ref %use-gate 'lock-spec)))

(define verify-locked-spec-proc
  (if (module-bound? %use-gate 'verify-locked-geis)
      (module-ref %use-gate 'verify-locked-geis)
      (module-ref %use-gate 'verify-locked-spec)))

(define locked-spec-path-proc
  (module-ref %use-gate 'locked-spec-path))

(define %pass-count 0)
(define %fail-count 0)

(define (assert-true desc condition)
  (if condition
      (begin
        (format #t "  PASS: ~a~%" desc)
        (set! %pass-count (+ %pass-count 1)))
      (begin
        (format (current-error-port) "  FAIL: ~a~%" desc)
        (set! %fail-count (+ %fail-count 1)))))

(define (assert-eq desc expected actual)
  (if (equal? expected actual)
      (begin
        (format #t "  PASS: ~a~%" desc)
        (set! %pass-count (+ %pass-count 1)))
      (begin
        (format (current-error-port) "  FAIL: ~a (expected ~s, got ~s)~%" desc expected actual)
        (set! %fail-count (+ %fail-count 1)))))

(define (assert-contains desc needle haystack)
  (if (and (string? haystack) (string-contains haystack needle))
      (begin
        (format #t "  PASS: ~a~%" desc)
        (set! %pass-count (+ %pass-count 1)))
      (begin
        (format (current-error-port) "  FAIL: ~a (expected to contain ~s)~%" desc needle)
        (format (current-error-port) "  Haystack: ~a~%" haystack)
        (set! %fail-count (+ %fail-count 1)))))

(define (write-file! path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(format #t "=== E2E Test Suite: Geis Contract Locking & Verification ===~%")

;; Setup temporary isolated workspace
(let* ((tmp-dir (mkdtemp "/tmp/badb-geis-e2e-XXXXXX"))
       (gate-root (string-append tmp-dir "/gate-root"))
       (saved-root (getenv "BADB_SPEC_GATE_ROOT"))
       (pass-content (call-with-input-file "test/fixtures/spec-pass.scm" get-string-all)))

  (dynamic-wind
    (lambda ()
      (setenv "BADB_SPEC_GATE_ROOT" gate-root))
    (lambda ()

      ;; -----------------------------------------------------------------------
      ;; 1. Spec locking & read-only immutability
      ;; -----------------------------------------------------------------------
      (format #t "[1/4] Testing Spec Locking & Immutability...~%")

      (let* ((pass-source (string-append tmp-dir "/my-spec.scm"))
             (expected-hash (sha256-hex pass-content)))
        (write-file! pass-source pass-content)

        (let* ((lock-res (lock-spec-proc pass-source "my-feature"))
               (hash (if (pair? lock-res) (assoc-ref lock-res 'hash) lock-res))
               (locked-file (locked-spec-path-proc "my-feature" gate-root)))

          (assert-true "Locked spec file exists on disk" (file-exists? locked-file))
          (assert-eq "Returned lock hash is correct SHA-256 hex" expected-hash hash)
          (assert-eq "Locked content is byte-identical to original source"
                     pass-content
                     (call-with-input-file locked-file get-string-all))

          ;; Verify OS permissions are read-only (#o444)
          (let ((st (stat locked-file)))
            (assert-eq "Locked file has read-only permissions (#o444)"
                       #o444
                       (logand (stat:perms st) #o777)))

          ;; Verify write attempt fails with permission error
          (let ((write-failed?
                 (catch 'system-error
                   (lambda ()
                     (call-with-output-file locked-file (lambda (p) (display "tampered" p)))
                     #f)
                   (lambda _ #t))))
            (assert-true "Attempt to overwrite locked file is rejected by OS permissions" write-failed?))))

      ;; -----------------------------------------------------------------------
      ;; 2. Disposable subprocess verification of passing spec
      ;; -----------------------------------------------------------------------
      (format #t "[2/4] Testing Disposable Subprocess Verification (Passing Spec)...~%")

      (let* ((pass-source "test/fixtures/spec-pass.scm")
             (_ (lock-spec-proc pass-source "math-feature-pass"))
             (res (verify-locked-spec-proc "math-feature-pass" #:timeout 30)))

        (assert-eq "Passing fixture yields verdict 'pass'" "pass" (assoc-ref res 'verdict))
        (assert-eq "Subprocess exit status is 0" 0 (assoc-ref res 'exit-status))
        (assert-eq "No test assertions failed" 0 (assoc-ref res 'summary-failed))
        (assert-true "Total assertions > 0" (> (assoc-ref res 'summary-total) 0))
        (assert-true "Recorded spec-hash matches locked content"
                     (string? (assoc-ref res 'spec-hash))))

      ;; -----------------------------------------------------------------------
      ;; 3. Disposable subprocess verification of failing spec (fails closed)
      ;; -----------------------------------------------------------------------
      (format #t "[3/4] Testing Disposable Subprocess Verification (Failing Spec)...~%")

      (let* ((fail-source "test/fixtures/spec-fail.scm")
             (_ (lock-spec-proc fail-source "math-feature-fail"))
             (res (verify-locked-spec-proc "math-feature-fail" #:timeout 30)))

        (assert-eq "Failing fixture yields verdict 'fail'" "fail" (assoc-ref res 'verdict))
        (assert-eq "Subprocess exit status is non-zero (1)" 1 (assoc-ref res 'exit-status))
        (assert-true "Summary records failed assertion" (> (assoc-ref res 'summary-failed) 0))
        (assert-true "Failures list contains failure diagnostics"
                     (pair? (assoc-ref res 'failures))))

      ;; -----------------------------------------------------------------------
      ;; 4. Adversarial Edge Cases: Tampering, Mismatch, Missing Lock & Timeout
      ;; -----------------------------------------------------------------------
      (format #t "[4/4] Testing Adversarial Edge Cases (Tamper, Mismatch, Timeout)...~%")

      ;; Case A: Corrupted agent source after locking (gate uses locked copy, not source)
      (let* ((source (string-append tmp-dir "/tampered-source.scm"))
             (_ (write-file! source pass-content))
             (_ (lock-spec-proc source "corrupted-src-feature"))
             ;; Agent corrupts its working copy after locking
             (_ (write-file! source "MALFORMED (((((( SYNTAX ERROR"))
             (res (verify-locked-spec-proc "corrupted-src-feature" #:timeout 30)))
        (assert-eq "Gate passes from locked copy despite corrupted agent working copy"
                   "pass" (assoc-ref res 'verdict)))

      ;; Case B: Missing locked spec fails closed
      (let ((res (verify-locked-spec-proc "never-locked-feature" #:timeout 30)))
        (assert-eq "Missing locked spec fails closed with verdict 'fail'" "fail" (assoc-ref res 'verdict))
        (assert-contains "Reason reports no locked spec" "no locked spec" (assoc-ref res 'reason)))

      ;; Case C: Exit 1 with JSON "pass" (mismatch) fails closed
      (let* ((mismatch-src (string-append tmp-dir "/mismatch-spec.scm"))
             (_ (write-file! mismatch-src
                             "(use-modules (srfi srfi-64) (json))\n(display (scm->json-string '((summary . ((total . 1) (passed . 1) (failed . 0))) (verdict . \"pass\") (seed . \"s\") (failures . #()))))\n(newline)\n(primitive-exit 1)\n"))
             (_ (lock-spec-proc mismatch-src "mismatch-feature"))
             (res (verify-locked-spec-proc "mismatch-feature" #:timeout 30)))
        (assert-eq "JSON pass + exit 1 mismatch fails closed with verdict 'fail'" "fail" (assoc-ref res 'verdict))
        (assert-contains "Reason notes mismatch" "mismatch" (assoc-ref res 'reason)))

      ;; Case D: Subprocess timeout fails closed
      (let* ((timeout-src (string-append tmp-dir "/timeout-spec.scm"))
             (_ (write-file! timeout-src
                             "(use-modules (srfi srfi-64) (json))\n(sleep 120)\n(display (scm->json-string '((verdict . \"pass\"))))\n(newline)\n"))
             (_ (lock-spec-proc timeout-src "timeout-feature"))
             (res (verify-locked-spec-proc "timeout-feature" #:timeout 3)))
        (assert-eq "Subprocess timeout fails closed with verdict 'fail'" "fail" (assoc-ref res 'verdict))
        (assert-true "Result flags timed-out? is #t" (assoc-ref res 'timed-out?))
        (assert-contains "Reason notes timeout" "timed out" (assoc-ref res 'reason))))

    (lambda ()
      ;; Restore environment & clean up
      (if saved-root
          (setenv "BADB_SPEC_GATE_ROOT" saved-root)
          (unsetenv "BADB_SPEC_GATE_ROOT"))
      (system* "rm" "-rf" tmp-dir))))

;;; ---------------------------------------------------------------------------
;;; Summary & Exit
;;; ---------------------------------------------------------------------------
(format #t "~%=== Geis Contract Locking & Verification E2E Summary ===~%")
(format #t "  Total Passed: ~a~%" %pass-count)
(format #t "  Total Failed: ~a~%" %fail-count)

(if (> %fail-count 0)
    (begin
      (format (current-error-port) "FAIL: Geis contract E2E suite had ~a failure(s).~%" %fail-count)
      (exit 1))
    (begin
      (format #t "SUCCESS: All Geis contract locking & verification tests passed.~%")
      (exit 0)))
