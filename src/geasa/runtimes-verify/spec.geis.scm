;;; test-verify.scm — Tests for Neuro-Symbolic Verification Engine (geasa runtimes-verify plugin)

(define-module (test test-verify)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (srfi srfi-1)
               #:use-module (ice-9 rdelim)
               #:use-module (geasa runtimes-rollback plugin)
               #:use-module (geasa runtimes-verify plugin))

(spec-begin "runtimes-verify")

(define %test-ws "/tmp/badb-test-verify-ws")

(define (setup-test-env!)
        (system (format #f "rm -rf ~a && mkdir -p ~a" %test-ws %test-ws))
        (call-with-output-file (string-append %test-ws "/code.scm")
                               (lambda (p) (display "(define (hello) 'world)\n" p))))

(define (teardown-test-env!)
        (system (format #f "rm -rf ~a" %test-ws)))

;;;
;;; 1. Scheme Syntax Verification
;;;

(test-group "verify-syntax"
            (test-assert "valid code passes syntax check"
                         (assoc-ref (verify-scheme-syntax "(define (add x y) (+ x y))") 'passed?))

            (test-assert "unbalanced parens fail syntax check"
                         (not (assoc-ref (verify-scheme-syntax "(define (unclosed x y)") 'passed?)))

            (test-assert "syntax error diagnostic returned"
                         (string? (assoc-ref (verify-scheme-syntax "(define (unclosed x y)") 'error))))

;;;
;;; 2. Verifier Record & Configuration
;;;

(test-group "verifier-configuration"
            (let ((v (make-verifier)))
              (test-assert "verifier? recognizes record" (verifier? v))
              (test-assert "syntax check enabled by default" (verifier-check-syntax? v))
              (test-assert "test runner disabled by default" (not (verifier-run-tests? v)))
              (test-equal "default test command" "./scripts/test" (verifier-test-command v)))

            (let ((custom (make-verifier #:check-syntax? #f #:run-tests? #t #:test-command "make check")))
              (test-assert "custom syntax setting" (not (verifier-check-syntax? custom)))
              (test-assert "custom test runner setting" (verifier-run-tests? custom))
              (test-equal "custom test command" "make check" (verifier-test-command custom))))

;;;
;;; 3. Action Verification with Custom Validators
;;;

(test-group "verify-action"
            (let* ((v (make-verifier))
                   (pass-res (verify-action v #:code "(define x 42)")))
              (test-assert "valid code passes verify-action" (assoc-ref pass-res 'passed?)))

            (let* ((v (make-verifier))
                   (fail-res (verify-action v #:code "(define x (unclosed")))
              (test-assert "invalid code fails verify-action" (not (assoc-ref fail-res 'passed?)))
              (test-assert "failures list populated" (pair? (assoc-ref fail-res 'failures))))

            ;; Custom validator
            (let ((v (make-verifier)))
              (add-verifier-validator! v
                                       (lambda (ctx)
                                         (let ((code (assoc-ref ctx 'code)))
                                           (if (and code (string-contains code "forbidden-token"))
                                             '((passed? . #f) (error . "Contains forbidden-token"))
                                             '((passed? . #t))))))

              (test-assert "custom validator passes clean code"
                           (assoc-ref (verify-action v #:code "(define safe 1)") 'passed?))
              (test-assert "custom validator catches forbidden token"
                           (not (assoc-ref (verify-action v #:code "(define forbidden-token 1)") 'passed?)))))

;;;
;;; 4. Neuro-Symbolic Gated Execution with Rollback
;;;

(test-group "verify-gated-execution"
            (setup-test-env!)
            (let* ((mgr (make-rollback-manager #:workspace-dir %test-ws))
                   (v (make-verifier #:check-syntax? #t))
                   (target-file (string-append %test-ws "/code.scm")))

              ;; Successful verified edit
              (let ((res (with-neuro-symbolic-gate v mgr "valid edit"
                                                   (lambda ()
                                                     (call-with-output-file target-file
                                                                            (lambda (p) (display "(define (hello) 'updated-world)\n" p)))
                                                     'ok)
                                                   #:verify-target target-file)))
                (test-equal "verified status returned" "verified" (assoc-ref res 'status))
                (test-assert "file updated"
                             (string-contains (call-with-input-file target-file read-line) "updated-world")))

              ;; Malformed edit with syntax error: triggers automatic rollback!
              (let ((res (with-neuro-symbolic-gate v mgr "corrupted edit"
                                                   (lambda ()
                                                     (call-with-output-file target-file
                                                                            (lambda (p) (display "(define (broken-syntax" p)))
                                                     'corrupted)
                                                   #:verify-target target-file)))
                (test-equal "verification_failed status returned" "verification_failed" (assoc-ref res 'status))
                (test-assert "checkpoint rolled back" (assoc-ref res 'checkpoint-rolled-back))
                (test-assert "file content restored to valid state"
                             (string-contains (call-with-input-file target-file read-line) "updated-world")))

              ;; Omitting #:verify-target still checks workspace .scm syntax and rolls back on broken syntax
              (let ((res (with-neuro-symbolic-gate v mgr "corrupted workspace edit without target"
                                                   (lambda ()
                                                     (call-with-output-file target-file
                                                                            (lambda (p) (display "(define (unclosed-workspace-form" p)))
                                                     'corrupted))))
                (test-equal "workspace syntax check catches error without verify-target"
                            "verification_failed" (assoc-ref res 'status))
                (test-assert "file content restored after workspace syntax failure"
                             (string-contains (call-with-input-file target-file read-line) "updated-world")))

              ;; Non-Scheme file with unbalanced parens passes verify-action
              (let* ((v-syntax (make-verifier #:check-syntax? #t #:run-tests? #f))
                     (md-content "# Title\nHere is an unbalanced parenthesis (like this.\n")
                     (md-res (verify-action v-syntax #:code md-content #:file "README.md")))
                (test-assert "markdown with unbalanced paren passes verify-action"
                             (assoc-ref md-res 'passed?)))

              ;; Scheme file with unbalanced parens fails verify-action
              (let* ((v-syntax (make-verifier #:check-syntax? #t #:run-tests? #f))
                     (scm-broken "(define (broken-fn x\n")
                     (scm-res (verify-action v-syntax #:code scm-broken #:file "broken.scm")))
                (test-assert "broken scheme file fails verify-action"
                             (not (assoc-ref scm-res 'passed?)))))
            (teardown-test-env!))

(spec-end)
(spec-exit)
