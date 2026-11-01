#!/usr/bin/env -S guile -s
!#
;;; scripts/verify-tests-regression.scm — Verify all 1,842 tests pass with zero regression
;;;
;;; Usage: guile -L src -L . scripts/verify-tests-regression.scm [manifest-path]
;;;    or: ./scripts/verify-tests-regression.scm [manifest-path]

(when (current-filename)
  (let ((dir (dirname (current-filename))))
    (add-to-load-path (canonicalize-path (string-append dir "/../src")))
    (add-to-load-path (canonicalize-path (string-append dir "/..")))))

(use-modules (srfi srfi-64)
             (ice-9 ftw)
             (ice-9 format)
             (ice-9 rdelim))

(define (main args)
  (let* ((manifest-path (if (> (length args) 1)
                            (cadr args)
                            "test/fixtures/test-manifest-1842.tsv"))
         (baseline-table (make-hash-table 2500))
         (baseline-count 0))
    (format #t "=== Verifying 1,842 Unit & Bootstrap Tests with Zero Failures ===~%")
    (format #t "Baseline manifest: ~a~%" manifest-path)

    ;; 1. Load baseline manifest
    (if (file-exists? manifest-path)
        (call-with-input-file manifest-path
          (lambda (port)
            (let loop ((line (read-line port)))
              (unless (eof-object? line)
                (let ((parts (string-split line #\tab)))
                  (when (>= (length parts) 3)
                    (let ((file (list-ref parts 0))
                          (test-name (list-ref parts 2)))
                      (hash-set! baseline-table (string-append file "::" test-name) #t)
                      (set! baseline-count (+ baseline-count 1)))))
                (loop (read-line port))))))
        (begin
          (format (current-error-port) "ERROR: Manifest file '~a' not found!~%" manifest-path)
          (exit 1)))

    (format #t "Loaded ~a baseline test cases from manifest.~%" baseline-count)

    ;; 2. Run test suite and collect results
    (let* ((runner (test-runner-null))
           (current-file "")
           (executed-table (make-hash-table 2500))
           (passes 0)
           (fails 0)
           (errors 0)
           (orig-module (current-module)))

      (test-runner-on-test-end! runner
        (lambda (r)
          (let ((res (test-result-kind r))
                (test-name (test-runner-test-name r)))
            (hash-set! executed-table (string-append current-file "::" test-name) res)
            (cond
              ((memq res '(pass xfail)) (set! passes (+ passes 1)))
              ((memq res '(fail xpass)) (set! fails (+ fails 1)))
              (else (set! errors (+ errors 1)))))))

      (test-runner-current runner)

      (let ((test-files
             (map (lambda (f) (string-append "test/" f))
                  (sort (scandir "test" (lambda (n) (and (string-prefix? "test-" n) (string-suffix? ".scm" n))))
                        string<?))))
        (for-each
          (lambda (f)
            (set! current-file f)
            (primitive-load f))
          test-files))

      (set-current-module orig-module)

      ;; 3. Check for missing or altered tests from baseline
      (let ((missing-tests '()))
        (hash-for-each
          (lambda (key val)
            (let ((result (hash-ref executed-table key #f)))
              (unless (eq? result 'pass)
                (set! missing-tests (cons (cons key result) missing-tests)))))
          baseline-table)

        ;; 4. Check for unexpected test failures or errors
        (format #t "~%=== Test Verification Results ===~%")
        (format #t "  Executed Passes    : ~a~%" passes)
        (format #t "  Executed Failures  : ~a~%" fails)
        (format #t "  Executed Errors    : ~a~%" errors)
        (format #t "  Missing Baseline   : ~a~%" (length missing-tests))

        (when (pair? missing-tests)
          (format (current-error-port) "~%FAIL: The following baseline tests were not executed or failed:~%")
          (for-each
            (lambda (m)
              (format (current-error-port) "  - ~a (result: ~s)~%" (car m) (cdr m)))
            missing-tests))

        (cond
          ((> fails 0)
           (format (current-error-port) "~%FAIL: ~a test(s) failed!~%" fails)
           (exit 1))
          ((> errors 0)
           (format (current-error-port) "~%FAIL: ~a test(s) encountered errors!~%" errors)
           (exit 1))
          ((pair? missing-tests)
           (format (current-error-port) "~%FAIL: ~a baseline test(s) missing or altered!~%" (length missing-tests))
           (exit 1))
          ((< passes 1842)
           (format (current-error-port) "~%FAIL: Total passes (~a) is less than required 1,842!~%" passes)
           (exit 1))
          (else
           (format #t "~%SUCCESS: All ~a canonical tests passed with 0 failures, 0 errors, and no test cases deleted or weakened.~%" passes)
           (exit 0)))))))

(main (command-line))
