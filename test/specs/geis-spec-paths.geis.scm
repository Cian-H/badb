;;; test/test-geis-spec-paths.scm — Tests for spec-bundle-path and spec-bundle-ref

(define-module (test test-geis-spec-paths)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim))

;; Compute expected bundle path ONCE at top level BEFORE spec-begin
(define %expected-bundle-path (dirname (current-filename)))

(spec-begin "geis-spec-paths")

;;;
;;; Test group 1: spec-bundle-path returns dirname of this spec file
;;;
(test-group "spec-bundle-path-basic"
  (test-assert "spec-bundle-path returns string equal to dirname of spec file"
    (string= (spec-bundle-path) %expected-bundle-path)))

;;;
;;; Test group 2: spec-bundle-ref resolves relative paths correctly
;;;
(test-group "spec-bundle-ref-basic"
  (test-assert "spec-bundle-ref resolves fixtures/x.scm correctly"
    (string= (spec-bundle-ref "fixtures/x.scm")
             (string-append (spec-bundle-path) "/fixtures/x.scm")))
  
  (test-assert "spec-bundle-ref resolves nested path correctly"
    (string= (spec-bundle-ref "fixtures/sub/y.scm")
             (string-append (spec-bundle-path) "/fixtures/sub/y.scm")))
  
  (test-assert "spec-bundle-ref resolves simple filename correctly"
    (string= (spec-bundle-ref "data.txt")
             (string-append (spec-bundle-path) "/data.txt"))))

;;;
;;; Test group 3: spec-bundle-ref works for nonexistent files
;;;
(test-group "spec-bundle-ref-nonexistent"
  (test-assert "spec-bundle-ref returns string for nonexistent file"
    (string? (spec-bundle-ref "nonexistent/file.scm")))
  
  (test-assert "spec-bundle-ref returns correct joined path for nonexistent file"
    (string= (spec-bundle-ref "nonexistent/file.scm")
             (string-append (spec-bundle-path) "/nonexistent/file.scm")))
  
  (test-assert "spec-bundle-ref returns correct path for nonexistent nested"
    (string= (spec-bundle-ref "fixtures/nonexistent.scm")
             (string-append (spec-bundle-path) "/fixtures/nonexistent.scm"))))

;;;
;;; Test group 4: spec-bundle-path is file-relative, NOT CWD-relative
;;;
(test-group "spec-bundle-path-file-relative"
  (let* ((temp-dir (mkdtemp "/tmp/badb-spec-paths-XXXXXX"))
         (spec-file (string-append temp-dir "/test-spec.scm"))
         (result-file (string-append temp-dir "/result.txt")))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        ;; Write a minimal spec that writes its spec-bundle-path to result-file
        (call-with-output-file spec-file
          (lambda (port)
            (format port "(use-modules (badb spec))~%")
            (format port "(spec-begin ~s)~%" "temp-test")
            (format port "(call-with-output-file ~s~%" result-file)
            (format port "  (lambda (p) (display (spec-bundle-path) p)))~%")
            (format port "(spec-end)~%")
            (format port "(spec-exit)~%")))
        
        ;; Run guile on the spec file with CWD=repo-root
        (let* ((repo-root "/home/cianh/Projects/badb")
               (cmd (string-join
                     (list "guile" "--no-auto-compile" "-L" (string-append repo-root "/src")
                           "-L" repo-root spec-file)
                     " ")))
          (system cmd))
        
        ;; Read back the result
        (test-assert "spec-bundle-path in temp dir equals temp dir"
          (let* ((port (open-input-file result-file))
                 (actual (read-line port)))
            (close-port port)
            (string= (string-trim-right actual #\newline) temp-dir)))
        
        ;; Cleanup
        (delete-file result-file))
      (lambda ()
        (catch #t
          (lambda () (delete-file spec-file))
          (lambda _ #f))
        (catch #t
          (lambda () (rmdir temp-dir))
          (lambda _ #f))))))

;;;
;;; Test group 5: Engine compatibility
;;;
(test-group "engine-compatibility"
  (test-assert "spec-suite-active? is true inside the suite"
    (spec-suite-active?)))

(spec-end)
(spec-exit)
