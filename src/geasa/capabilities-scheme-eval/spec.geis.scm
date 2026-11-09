;;; (geasa capabilities-scheme-eval spec.geis) — Contract verification for Scheme Eval

(define-module (test test-capabilities-scheme-eval)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (badb kernel registry)
               #:use-module (geasa policies-guardrail plugin)
               #:use-module (geasa capabilities-scheme-eval plugin)
               #:use-module (srfi srfi-1))

(spec-begin "capabilities-scheme-eval")

(test-group "scheme-eval-basic"
            (let ((res1 (eval-scheme-sandboxed "(+ 40 2)"))
                  (res2 (eval-scheme-sandboxed "(string-append \"hello \" \"world\")")))
              (test-equal "eval arithmetic" "42" res1)
              (test-equal "eval string manipulation" "hello world" res2)))

(test-group "scheme-eval-tool-composition"
            (let ((reg (make-registry)))
              (registry-attach! reg "math.double" (lambda (x) (* (if (number? x) x 0) 2)))
              (registry-attach! reg "math.square" (lambda (x) (* (if (number? x) x 0) (if (number? x) x 0))))

              (let* ((code "(let* ((a (call-tool \"math.double\" 10))
                         (b (call-tool \"math.square\" a)))
                    b)")
                     (res (eval-scheme-sandboxed code #:registry reg)))
                (test-equal "composed tools: square(double(10)) = 400" "400" res))))

(test-group "scheme-eval-guardrail-enforcement"
            (let* ((reg (make-registry))
                   (_ (registry-attach! reg "danger.rm" (lambda (p) "deleted")))
                   (policy (make-guardrail-policy #:level 'strict
                                                  #:rules (list (make-pattern-rule "danger.*" ".*" 'deny "Forbidden dangerous tool")))))
              (let ((res (eval-scheme-sandboxed "(call-tool \"danger.rm\" \"/important\")"
                                                #:registry reg
                                                #:guardrail policy)))
                (test-assert "guardrail denied error reported"
                             (string-contains res "guardrail-denied")))))

(test-group "scheme-eval-security"
            (let ((res (eval-scheme-sandboxed "(system* \"echo\" \"injected\")")))
              (test-assert "restricted system call blocked"
                           (string-contains res "Security restriction"))))

(test-group "scheme-eval-timeout"
            (let ((res (eval-scheme-sandboxed "(let loop () (loop))" #:timeout-seconds 1)))
              (test-assert "infinite loop timed out"
                           (string-contains res "timed out after 1 seconds"))))

(test-group "scheme-eval-truncation"
            (let ((res (eval-scheme-sandboxed "(make-string 5000 #\\z)")))
              (test-assert "long output is bounded" (<= (string-length res) 4100))
              (test-assert "truncation banner is present"
                           (string-contains res "truncated"))))

(test-group "scheme-eval-tool-registration"
            (let ((reg (make-registry)))
              (register-scheme-eval-tools! reg)
              (let ((fn (registry-lookup reg "scheme_eval")))
                (test-assert "scheme_eval registered" (procedure? fn))
                (let ((res (fn '((code . "(* 6 7)")))))
                  (test-equal "tool invocation evaluates correctly" "42" res)))))

(spec-end)
(spec-exit)
