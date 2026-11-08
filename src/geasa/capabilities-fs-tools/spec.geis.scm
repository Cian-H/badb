;;; spec.geis.scm — Contract specification for capabilities-fs-tools

(use-modules (badb spec)
             (badb kernel registry)
             (geasa capabilities-fs-tools plugin)
             (geasa runtimes-rollback plugin)
             (geasa runtimes-verify plugin)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-64))

(spec-begin "capabilities-fs-tools")

(define %test-dir "/tmp/badb-test-fs-tools")

(define (setup-test-dir!)
  (when (file-exists? %test-dir)
    (system (format #f "rm -rf ~a" %test-dir)))
  (mkdir %test-dir))

(define (teardown-test-dir!)
  (when (file-exists? %test-dir)
    (system (format #f "rm -rf ~a" %test-dir))))

(setup-test-dir!)

(test-group "fs-read-file"
  (let ((sample (string-append %test-dir "/sample.txt")))
    (call-with-output-file sample
      (lambda (p) (display "line 1\nline 2\nline 3\n" p)))
    (let ((out (fs-read-file sample 0 2)))
      (test-assert "contains file header" (string-contains out "showing lines 0..1"))
      (test-assert "contains line 1" (string-contains out "line 1"))
      (test-assert "contains line 2" (string-contains out "line 2"))
      (test-assert "elided footer" (string-contains out "1 more lines elided"))))

  (let ((missing (string-append %test-dir "/nonexistent.txt")))
    (test-assert "missing file returns error"
      (string-contains (fs-read-file missing) "does not exist"))))

(test-group "fs-write-file"
  (let* ((out-file (string-append %test-dir "/written.txt"))
         (res (fs-write-file out-file "alpha\nbeta\ngamma\n")))
    (test-assert "reports wrote count" (string-contains res "Wrote"))
    (test-assert "file exists on disk" (file-exists? out-file))
    (test-equal "file content matches"
      "alpha\nbeta\ngamma\n"
      (call-with-input-file out-file read-string))))

(test-group "fs-edit-file"
  (let ((code-file (string-append %test-dir "/module.scm")))
    (call-with-output-file code-file
      (lambda (p) (display "(define (hello) 'old-world)\n(define (foo) 'bar)\n" p)))

    ;; 1. Single replacement
    (let ((res (fs-edit-file code-file "'old-world" "'new-world")))
      (test-assert "edit reports replaced 1 occurrence" (string-contains res "replaced 1 occurrence"))
      (test-assert "disk content updated"
        (string-contains (call-with-input-file code-file read-string) "'new-world")))

    ;; 2. Target not found
    (let ((res (fs-edit-file code-file "non-existent-token" "replacement")))
      (test-assert "reports target text not found"
        (string-contains res "target text not found")))

    ;; 3. Multiple matches without replace-all? fails with line numbers
    (call-with-output-file code-file
      (lambda (p) (display "dup-token line 1\nother line\ndup-token line 3\n" p)))
    (let ((res (fs-edit-file code-file "dup-token" "unique-token")))
      (test-assert "reports multiple matches" (string-contains res "matched 2 locations"))
      (test-assert "includes line numbers" (string-contains res "lines 1, 3")))

    ;; 4. Multiple matches with replace-all? succeeds
    (let ((res (fs-edit-file code-file "dup-token" "unique-token" #:replace-all? #t)))
      (test-assert "reports 2 occurrences replaced" (string-contains res "replaced 2 occurrence(s)"))
      (test-assert "all occurrences replaced on disk"
        (not (string-contains (call-with-input-file code-file read-string) "dup-token"))))

    ;; 5. Verification failure on broken Scheme leaves file untouched
    (call-with-output-file code-file
      (lambda (p) (display "(define (valid-form) 42)\n" p)))
    (let* ((v (make-verifier #:check-syntax? #t #:run-tests? #f))
           (res (fs-edit-file code-file "42" "(broken-paren" #:verifier v)))
      (test-assert "edit aborted on verification failure" (string-contains res "Edit aborted"))
      (test-assert "file on disk untouched"
        (string-contains (call-with-input-file code-file read-string) "42")))))

(test-group "register-fs-tools!"
  (let ((reg (make-registry)))
    (register-fs-tools! reg #:shell-executor (lambda (cmd) (list 'ok cmd)))
    (test-assert "read_file registered" (procedure? (registry-lookup reg "read_file")))
    (test-assert "write_file registered" (procedure? (registry-lookup reg "write_file")))
    (test-assert "edit_file registered" (procedure? (registry-lookup reg "edit_file")))
    (test-assert "bash registered" (procedure? (registry-lookup reg "bash")))))

(teardown-test-dir!)

(spec-end)
(spec-exit)
