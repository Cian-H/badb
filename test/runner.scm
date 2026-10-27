(define-module (test runner)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:export (run-all-tests run-test-file))

(define (run-test-file filename)
  (format #t "Running test file: ~a~%" filename)
  (primitive-load filename))

(define (find-test-files dir)
  (let ((files (scandir dir
                        (lambda (name)
                          (and (string-prefix? "test-" name)
                               (string-suffix? ".scm" name))))))
    (if files
        (map (lambda (f) (string-append dir "/" f)) (sort files string<?))
        '())))

(define (run-all-tests)
  (let* ((runner (test-runner-simple))
         (test-files (find-test-files "test")))
    (test-runner-current runner)
    (format #t "=== Starting badb Bootstrap Test Suite ===~%")
    (for-each run-test-file test-files)
    (let ((fails (test-runner-fail-count runner)))
      (format #t "~%=== Test Run Complete: ~a failure(s) ===~%" fails)
      (if (> fails 0)
          (exit 1)
          (exit 0)))))

(when (equal? (car (command-line)) (current-filename))
  (run-all-tests))
