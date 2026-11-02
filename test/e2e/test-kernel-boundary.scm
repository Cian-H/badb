#!/usr/bin/env -S guile -s
!#
;;; test/e2e/test-kernel-boundary.scm — E2E tests for kernel boundary enforcement & rejection
;;;
;;; Usage: guile -L src -L . test/e2e/test-kernel-boundary.scm
;;;    or: ./test/e2e/test-kernel-boundary.scm

(when (current-filename)
  (let ((dir (dirname (current-filename))))
    (add-to-load-path (canonicalize-path (string-append dir "/../../src")))
    (add-to-load-path (canonicalize-path (string-append dir "/../..")))))

(use-modules (ice-9 format)
             (ice-9 ftw)
             (ice-9 popen)
             (ice-9 rdelim))

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
  (if (string-contains haystack needle)
      (begin
        (format #t "  PASS: ~a~%" desc)
        (set! %pass-count (+ %pass-count 1)))
      (begin
        (format (current-error-port) "  FAIL: ~a (expected to contain ~s)~%" desc needle)
        (format (current-error-port) "  Haystack: ~a~%" haystack)
        (set! %fail-count (+ %fail-count 1)))))

(define (run-cmd program . args)
  (let* ((cmd-parts (cons program (map (lambda (a) (format #f "~a" a)) args)))
         (full-cmd (string-append (string-join cmd-parts " ") " 2>&1"))
         (pipe (open-pipe* OPEN_READ "sh" "-c" full-cmd))
         (output (read-string pipe))
         (status (close-pipe pipe))
         (exit-code (status:exit-val status)))
    (cons exit-code (or output ""))))

(define (make-temp-dir prefix)
  (let ((template (string-append "/tmp/" prefix "-XXXXXX")))
    (mkdtemp template)))

(define (delete-dir-recursive dir)
  (when (file-exists? dir)
    (system* "rm" "-rf" dir)))

(define (write-file path content)
  (call-with-output-file path
    (lambda (port) (display content port))))

;;;
;;; Setup Valid Kernel Mock Directory
;;;
(define (populate-valid-kernel dir)
  (write-file (string-append dir "/registry.scm")
              "(define-module (badb kernel registry)\n  #:use-module (srfi srfi-9)\n  #:export (make-registry))\n(define (make-registry) 'reg)\n")
  (write-file (string-append dir "/geis-gate.scm")
              "(define-module (badb kernel geis-gate)\n  #:use-module (ice-9 format)\n  #:export (verify-geis))\n(define (verify-geis) 'pass)\n")
  (write-file (string-append dir "/transaction.scm")
              "(define-module (badb kernel transaction)\n  #:use-module (srfi srfi-1)\n  #:export (stage-patch))\n(define (stage-patch) 'staged)\n")
  (write-file (string-append dir "/journal.scm")
              "(define-module (badb kernel journal)\n  #:use-module (json)\n  #:export (journal-append!))\n(define (journal-append!) 'ok)\n")
  (write-file (string-append dir "/digest.scm")
              "(define-module (badb kernel digest)\n  #:use-module (rnrs bytevectors)\n  #:export (sha256-hex))\n(define (sha256-hex x) \"mockhash\")\n")
  (write-file (string-append dir "/ipc.scm")
              "(define-module (badb kernel ipc)\n  #:use-module (ice-9 rdelim)\n  #:export (make-ipc-channel))\n(define (make-ipc-channel) 'chan)\n"))

(format #t "=== E2E Test Suite: Kernel Boundary Enforcement & Rejection ===~%")

;;; ---------------------------------------------------------------------------
;;; Part 1: Dependency Boundary Scanner (scripts/verify-kernel-deps.scm)
;;; ---------------------------------------------------------------------------
(format #t "[1/2] Testing Dependency Boundary Enforcement (scripts/verify-kernel-deps.scm)...~%")

(let ((tmp-dir (make-temp-dir "badb-deps-test")))
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      ;; 1. Clean valid directory passes
      (populate-valid-kernel tmp-dir)
      (let ((res (run-cmd "./scripts/verify-kernel-deps.scm" tmp-dir)))
        (assert-eq "Clean kernel directory passes dependency verification (exit 0)" 0 (car res))
        (assert-contains "Clean kernel reports SUCCESS" "SUCCESS: All kernel modules have zero user-space dependencies" (cdr res)))

      ;; 2. Adversarial: Forbidden import (geasa providers-model openai)
      (populate-valid-kernel tmp-dir)
      (write-file (string-append tmp-dir "/registry.scm")
                  "(define-module (badb kernel registry)\n  #:use-module (geasa providers-model openai))\n")
      (let ((res (run-cmd "./scripts/verify-kernel-deps.scm" tmp-dir)))
        (assert-eq "Rejects forbidden import (geasa providers-model openai) with exit 1" 1 (car res))
        (assert-contains "Reports AST violation for providers" "forbidden import (geasa providers-model openai)" (cdr res)))

      ;; 3. Adversarial: Forbidden import (geasa runtimes-react plugin)
      (populate-valid-kernel tmp-dir)
      (write-file (string-append tmp-dir "/registry.scm")
                  "(define-module (badb kernel registry)\n  #:use-module (geasa runtimes-react plugin))\n")
      (let ((res (run-cmd "./scripts/verify-kernel-deps.scm" tmp-dir)))
        (assert-eq "Rejects forbidden import (geasa runtimes-react plugin) with exit 1" 1 (car res))
        (assert-contains "Reports AST violation for runtimes" "forbidden import (geasa runtimes-react plugin)" (cdr res)))

      ;; 4. Adversarial: Forbidden import (badb capabilities filetree-mcp)
      (populate-valid-kernel tmp-dir)
      (write-file (string-append tmp-dir "/registry.scm")
                  "(define-module (badb kernel registry)\n  #:use-module (badb capabilities filetree-mcp))\n")
      (let ((res (run-cmd "./scripts/verify-kernel-deps.scm" tmp-dir)))
        (assert-eq "Rejects forbidden import (badb capabilities filetree-mcp) with exit 1" 1 (car res))
        (assert-contains "Reports AST violation for capabilities" "forbidden import (badb capabilities filetree-mcp)" (cdr res)))

      ;; 5. Adversarial: Forbidden import (badb surfaces cli)
      (populate-valid-kernel tmp-dir)
      (write-file (string-append tmp-dir "/registry.scm")
                  "(define-module (badb kernel registry)\n  #:use-module (badb surfaces cli))\n")
      (let ((res (run-cmd "./scripts/verify-kernel-deps.scm" tmp-dir)))
        (assert-eq "Rejects forbidden import (badb surfaces cli) with exit 1" 1 (car res))
        (assert-contains "Reports AST violation for surfaces" "forbidden import (badb surfaces cli)" (cdr res)))

      ;; 6. Adversarial: Forbidden import (geasa policies-guardrail plugin)
      (populate-valid-kernel tmp-dir)
      (write-file (string-append tmp-dir "/registry.scm")
                  "(define-module (badb kernel registry)\n  #:use-module (geasa policies-guardrail plugin))\n")
      (let ((res (run-cmd "./scripts/verify-kernel-deps.scm" tmp-dir)))
        (assert-eq "Rejects forbidden import (geasa policies-guardrail plugin) with exit 1" 1 (car res))
        (assert-contains "Reports AST violation for policies" "forbidden import (geasa policies-guardrail plugin)" (cdr res)))

      ;; 7. Adversarial: Forbidden import (geasa foundation-util plugin)
      (populate-valid-kernel tmp-dir)
      (write-file (string-append tmp-dir "/registry.scm")
                  "(define-module (badb kernel registry)\n  #:use-module (geasa foundation-util plugin))\n")
      (let ((res (run-cmd "./scripts/verify-kernel-deps.scm" tmp-dir)))
        (assert-eq "Rejects forbidden import (geasa foundation-util plugin) with exit 1" 1 (car res))
        (assert-contains "Reports AST violation for util" "forbidden import (geasa foundation-util plugin)" (cdr res)))

      ;; 8. Adversarial: Forbidden import (geasa foundation-log plugin)
      (populate-valid-kernel tmp-dir)
      (write-file (string-append tmp-dir "/journal.scm")
                  "(define-module (badb kernel journal)\n  #:use-module (geasa foundation-log plugin))\n")
      (let ((res (run-cmd "./scripts/verify-kernel-deps.scm" tmp-dir)))
        (assert-eq "Rejects forbidden import (geasa foundation-log plugin) with exit 1" 1 (car res))
        (assert-contains "Reports AST violation for log" "forbidden import (geasa foundation-log plugin)" (cdr res)))

      ;; 9. Adversarial: Forbidden text regex match
      (populate-valid-kernel tmp-dir)
      (write-file (string-append tmp-dir "/registry.scm")
                  ";; Comment with use-modules (geasa foundation-config plugin)\n(define x 1)\n")
      (let ((res (run-cmd "./scripts/verify-kernel-deps.scm" tmp-dir)))
        (assert-eq "Rejects text pattern matching forbidden module with exit 1" 1 (car res))
        (assert-contains "Reports Text Violation for config" "Text Violation:" (cdr res)))

      ;; 10. Non-existent directory fails closed
      (let ((res (run-cmd "./scripts/verify-kernel-deps.scm" "/tmp/non-existent-badb-kernel-xyz")))
        (assert-eq "Non-existent directory fails closed with exit 1" 1 (car res))
        (assert-contains "Reports directory does not exist" "does not exist" (cdr res))))
    (lambda ()
      (delete-dir-recursive tmp-dir))))

;;; ---------------------------------------------------------------------------
;;; Part 2: Size Budget and Whitelist Scanner (scripts/verify-kernel-size.sh)
;;; ---------------------------------------------------------------------------
(format #t "[2/2] Testing Size Budget and Whitelist Enforcement (scripts/verify-kernel-size.sh)...~%")

(let ((tmp-dir (make-temp-dir "badb-size-test")))
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      ;; 1. Clean valid directory passes
      (populate-valid-kernel tmp-dir)
      (let ((res (run-cmd "./scripts/verify-kernel-size.sh" tmp-dir)))
        (assert-eq "Clean kernel directory passes size verification (exit 0)" 0 (car res))
        (assert-contains "Reports SUCCESS for size verification" "SUCCESS: Kernel whitelist and LOC limit verified" (cdr res)))

      ;; 2. Adversarial: Unauthorized file present
      (populate-valid-kernel tmp-dir)
      (write-file (string-append tmp-dir "/unauthorized.scm") "(define rogue 42)\n")
      (let ((res (run-cmd "./scripts/verify-kernel-size.sh" tmp-dir)))
        (assert-eq "Rejects unauthorized file with exit 1" 1 (car res))
        (assert-contains "Reports unauthorized file name" "Unauthorized file in kernel directory: unauthorized.scm" (cdr res)))
      (delete-file (string-append tmp-dir "/unauthorized.scm"))

      ;; 3. Adversarial: Missing compulsory module (e.g. journal.scm deleted)
      (populate-valid-kernel tmp-dir)
      (delete-file (string-append tmp-dir "/journal.scm"))
      (let ((res (run-cmd "./scripts/verify-kernel-size.sh" tmp-dir)))
        (assert-eq "Rejects missing compulsory journal.scm with exit 1" 1 (car res))
        (assert-contains "Reports missing compulsory module" "Missing compulsory kernel module: journal.scm" (cdr res)))

      ;; 4. Adversarial: Missing message passing module (ipc.scm deleted)
      (populate-valid-kernel tmp-dir)
      (delete-file (string-append tmp-dir "/ipc.scm"))
      (let ((res (run-cmd "./scripts/verify-kernel-size.sh" tmp-dir)))
        (assert-eq "Rejects missing transport/ipc module with exit 1" 1 (car res))
        (assert-contains "Reports missing message passing module" "Missing compulsory message passing module" (cdr res)))

      ;; 5. Transport.scm accepted as valid alternative to ipc.scm
      (populate-valid-kernel tmp-dir)
      (delete-file (string-append tmp-dir "/ipc.scm"))
      (write-file (string-append tmp-dir "/transport.scm") "(define (make-transport) 'ok)\n")
      (let ((res (run-cmd "./scripts/verify-kernel-size.sh" tmp-dir)))
        (assert-eq "Accepts transport.scm as valid alternative to ipc.scm" 0 (car res)))

      ;; 6. Adversarial: LOC budget exceeded (> 1,200 lines)
      (populate-valid-kernel tmp-dir)
      (call-with-output-file (string-append tmp-dir "/registry.scm")
        (lambda (port)
          (display "(define-module (badb kernel registry))\n" port)
          (let loop ((i 0))
            (when (< i 1205)
              (format port "(define padding-~a ~a)\n" i i)
              (loop (+ i 1))))))
      (let ((res (run-cmd "./scripts/verify-kernel-size.sh" tmp-dir)))
        (assert-eq "Rejects LOC exceeding 1200 lines with exit 1" 1 (car res))
        (assert-contains "Reports LOC budget exceeded" "exceeds budget" (cdr res)))

      ;; 7. Non-existent directory fails closed
      (let ((res (run-cmd "./scripts/verify-kernel-size.sh" "/tmp/non-existent-kernel-dir-12345")))
        (assert-eq "Non-existent directory fails closed with exit 1" 1 (car res))
        (assert-contains "Reports directory does not exist" "does not exist" (cdr res))))
    (lambda ()
      (delete-dir-recursive tmp-dir))))

;;; ---------------------------------------------------------------------------
;;; Summary & Exit
;;; ---------------------------------------------------------------------------
(format #t "~%=== Kernel Boundary Enforcement E2E Summary ===~%")
(format #t "  Total Passed: ~a~%" %pass-count)
(format #t "  Total Failed: ~a~%" %fail-count)

(if (> %fail-count 0)
    (begin
      (format (current-error-port) "FAIL: Kernel boundary E2E suite had ~a failure(s).~%" %fail-count)
      (exit 1))
    (begin
      (format #t "SUCCESS: All kernel boundary enforcement tests passed.~%")
      (exit 0)))
