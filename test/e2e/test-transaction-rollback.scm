#!/usr/bin/env -S guile -L src -L . -s
!#
;;; test/e2e/test-transaction-rollback.scm — E2E tests for Transaction atomic rollback upon spec failure
;;;
;;; Usage: guile -L src -L . test/e2e/test-transaction-rollback.scm
;;;    or: ./test/e2e/test-transaction-rollback.scm

(when (current-filename)
  (let ((dir (dirname (current-filename))))
    (add-to-load-path (canonicalize-path (string-append dir "/../../src")))
    (add-to-load-path (canonicalize-path (string-append dir "/../..")))))

(use-modules (badb digest)
             (badb journal)
             (badb registry)
             (badb transaction)
             (ice-9 format)
             (srfi srfi-1))

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

(format #t "=== E2E Test Suite: Transaction Atomic Rollback on Spec Failure ===~%")

(let* ((j-path "/tmp/badb-e2e-tx-journal.jsonl")
       (_ (when (file-exists? j-path) (delete-file j-path)))
       (journal (make-journal j-path))
       (reg (make-registry))
       (tx-mgr (make-transaction-manager reg #:journal journal)))

  (dynamic-wind
    (lambda () #t)
    (lambda ()
      ;; -----------------------------------------------------------------------
      ;; 1. Stage and commit baseline feature
      ;; -----------------------------------------------------------------------
      (format #t "[1/5] Committing initial baseline feature...~%")

      (let ((res1 (apply-transaction! tx-mgr 'crypto-service
                                      '((define (encrypt msg) (string-append "enc:" msg))
                                        (define (decrypt msg)
                                          (if (string-prefix? "enc:" msg)
                                              (substring msg 4)
                                              #f))
                                        (define (crypto-service op . args)
                                          (case op
                                            ((encrypt) (apply encrypt args))
                                            ((decrypt) (apply decrypt args)))))
                                      (lambda (mod)
                                        (let ((svc (feature-module-ref mod 'crypto-service)))
                                          (and (procedure? svc)
                                               (string=? "secret" (svc 'decrypt (svc 'encrypt "secret"))))))
                                      #:metadata '((version . "1.0")
                                                   (status . "stable")))))

        (assert-eq "Baseline transaction status is 'committed'" "committed" (assoc-ref res1 'status))
        (assert-eq "Feature-id is 'crypto-service'" 'crypto-service (assoc-ref res1 'feature-id))

        ;; Registry lookup verification
        (let ((svc (registry-lookup reg 'crypto-service)))
          (assert-true "Service registered in registry as callable procedure" (procedure? svc))
          (assert-eq "Service encrypts correctly via direct call" "enc:hello" (svc 'encrypt "hello"))
          (assert-eq "Registry dispatch executes baseline service" "enc:badb" (registry-dispatch reg 'crypto-service 'encrypt "badb"))))

      ;; -----------------------------------------------------------------------
      ;; 2. Atomic rollback on spec failure (verification thunk returns #f)
      ;; -----------------------------------------------------------------------
      (format #t "[2/5] Testing atomic rollback when verification returns #f...~%")

      (let ((res2 (apply-transaction! tx-mgr 'crypto-service
                                      '((define (crypto-service op . args) "CORRUPTED_CIPHERTEXT"))
                                      (lambda (mod)
                                        ;; Verification fails
                                        #f))))

        (assert-eq "Failed verification yields 'reverted' status" "reverted" (assoc-ref res2 'status))
        (assert-true "Reverted result contains error message" (string? (assoc-ref res2 'error)))

        ;; Verify registry state was NOT modified
        (let ((svc (registry-lookup reg 'crypto-service)))
          (assert-eq "Registry feature remains original implementation" "enc:safe" (svc 'encrypt "safe"))
          (assert-eq "Registry dispatch remains original implementation" "enc:intact" (registry-dispatch reg 'crypto-service 'encrypt "intact")))

        ;; Verify metadata was NOT overwritten
        (let ((meta (registry-metadata reg 'crypto-service)))
          (assert-eq "Original version metadata preserved" "1.0" (assoc-ref meta 'version))
          (assert-eq "Original status metadata preserved" "stable" (assoc-ref meta 'status))))

      ;; -----------------------------------------------------------------------
      ;; 3. Atomic rollback on exception thrown during verification
      ;; -----------------------------------------------------------------------
      (format #t "[3/5] Testing atomic rollback when verification throws exception...~%")

      (let ((res3 (apply-transaction! tx-mgr 'crypto-service
                                      '((define (crypto-service op . args) (error "intentional syntax boom")))
                                      (lambda (mod)
                                        ;; Verification throws an exception
                                        (error "Verification exploded with fatal error" 500)))))

        (assert-eq "Exception in verification yields 'reverted' status" "reverted" (assoc-ref res3 'status))
        (assert-contains "Error message captures exception details"
                         "Verification exploded with fatal error"
                         (assoc-ref res3 'error))

        ;; Verify registry state is still the original baseline
        (let ((svc (registry-lookup reg 'crypto-service)))
          (assert-eq "Registry feature still original after exception" "enc:rock-solid" (svc 'encrypt "rock-solid"))))

      ;; -----------------------------------------------------------------------
      ;; 4. Atomic rollback on staging error (evaluation of patch forms fails)
      ;; -----------------------------------------------------------------------
      (format #t "[4/5] Testing atomic rollback when staging patch fails evaluation...~%")

      (let ((res4 (apply-transaction! tx-mgr 'crypto-service
                                      '((define invalid-divide-by-zero (/ 42 0)))
                                      (lambda (mod) #t))))

        (assert-eq "Staging failure yields 'reverted' status" "reverted" (assoc-ref res4 'status))
        (assert-contains "Error diagnostic mentions divide" "divide" (assoc-ref res4 'error))

        ;; Baseline continues functioning perfectly
        (let ((svc (registry-lookup reg 'crypto-service)))
          (assert-eq "Original service persists after staging error" "enc:unmoved" (svc 'encrypt "unmoved"))))

      ;; -----------------------------------------------------------------------
      ;; 5. Successful upgrade following rollbacks & journal audit trail verification
      ;; -----------------------------------------------------------------------
      (format #t "[5/5] Testing successful upgrade and journal provenance ledger...~%")

      (let ((res5 (apply-transaction! tx-mgr 'crypto-service
                                      '((define (crypto-service op . args)
                                          (case op
                                            ((encrypt) (string-append "v2:" (car args)))
                                            ((decrypt) (substring (car args) 3)))))
                                      (lambda (mod)
                                        (let ((svc (feature-module-ref mod 'crypto-service)))
                                          (string=? "v2:test" (svc 'encrypt "test"))))
                                      #:metadata '((version . "2.0")))))

        (assert-eq "Upgrade transaction committed successfully" "committed" (assoc-ref res5 'status))
        (assert-eq "Registry dispatch now invokes version 2.0" "v2:live" (registry-dispatch reg 'crypto-service 'encrypt "live"))
        (assert-eq "Metadata updated to version 2.0" "2.0" (assoc-ref (registry-metadata reg 'crypto-service) 'version)))

      ;; Journal audit trail check
      (assert-eq "Journal recorded all 5 transaction checkpoints" 5 (journal-count journal))

      (let* ((entries (journal-entries journal))
             (e1 (list-ref entries 0))
             (e2 (list-ref entries 1))
             (e3 (list-ref entries 2))
             (e4 (list-ref entries 3))
             (e5 (list-ref entries 4)))

        (assert-eq "Entry 1: commit / pass" "commit" (assoc-ref e1 "checkpoint"))
        (assert-eq "Entry 1 status: pass" "pass" (assoc-ref e1 "status"))

        (assert-eq "Entry 2: revert / fail (thunk #f)" "revert" (assoc-ref e2 "checkpoint"))
        (assert-eq "Entry 2 status: fail" "fail" (assoc-ref e2 "status"))

        (assert-eq "Entry 3: revert / fail (exception)" "revert" (assoc-ref e3 "checkpoint"))
        (assert-eq "Entry 3 status: fail" "fail" (assoc-ref e3 "status"))

        (assert-eq "Entry 4: revert / fail (staging error)" "revert" (assoc-ref e4 "checkpoint"))
        (assert-eq "Entry 4 status: fail" "fail" (assoc-ref e4 "status"))

        (assert-eq "Entry 5: commit / pass (upgrade)" "commit" (assoc-ref e5 "checkpoint"))
        (assert-eq "Entry 5 status: pass" "pass" (assoc-ref e5 "status"))

        ;; Cryptographic hash chain verification
        (let ((integrity (journal-verify-integrity journal)))
          (assert-true "Journal cryptographic integrity check passes"
                       (assoc-ref integrity 'valid)))))

    (lambda ()
      (when (file-exists? j-path)
        (delete-file j-path)))))

;;; ---------------------------------------------------------------------------
;;; Summary & Exit
;;; ---------------------------------------------------------------------------
(format #t "~%=== Transaction Rollback E2E Summary ===~%")
(format #t "  Total Passed: ~a~%" %pass-count)
(format #t "  Total Failed: ~a~%" %fail-count)

(if (> %fail-count 0)
    (begin
      (format (current-error-port) "FAIL: Transaction rollback E2E suite had ~a failure(s).~%" %fail-count)
      (exit 1))
    (begin
      (format #t "SUCCESS: All Transaction atomic rollback tests passed.~%")
      (exit 0)))
