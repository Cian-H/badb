;;; (geasa capabilities-rtk spec.geis) — Contract verification for RTK CLI proxy

(define-module (test capabilities-rtk-spec)
               #:use-module (badb spec)
               #:use-module (badb kernel registry)
               #:use-module (srfi srfi-64)
               #:use-module (geasa capabilities-rtk plugin))

(spec-begin "capabilities-rtk")

(test-group "rtk-availability-and-toggle"
            (test-assert "rtk-available? returns boolean"
                         (boolean? (rtk-available?)))
            (test-equal "rtk-enabled? defaults to #t" #t (rtk-enabled?))
            (set-rtk-enabled! #f)
            (test-equal "set-rtk-enabled! #f disables hook" #f (rtk-enabled?))
            (set-rtk-enabled! #t)
            (test-equal "set-rtk-enabled! #t re-enables hook" #t (rtk-enabled?)))

(test-group "rtk-rewrite-and-bash-pre-hook"
            (let ((hook (make-rtk-bash-pre-hook)))
              ;; Disabled toggle passes command through unchanged
              (set-rtk-enabled! #f)
              (test-equal "disabled hook passes command unchanged"
                          "git status"
                          (hook "git status"))
              (set-rtk-enabled! #t)

              ;; Unavailable binary passes command through gracefully
              (parameterize ((rtk-binary "/nonexistent/rtk-missing-bin"))
                            (test-equal "missing binary passthrough"
                                        'passthrough
                                        (car (rtk-rewrite "git status")))
                            (test-equal "hook with missing binary passes command unchanged"
                                        "git status"
                                        (hook "git status")))

              ;; When rtk is on PATH, supported commands rewrite cleanly
              (when (rtk-available?)
                (let ((res (rtk-rewrite "git status")))
                  (test-equal "git status rewrites via rtk" 'rewrite (car res))
                  (test-equal "git status becomes rtk git status" "rtk git status" (cdr res))
                  (test-equal "bash pre-hook rewrites git status" "rtk git status" (hook "git status")))
                (let ((no-rw (rtk-rewrite "echo hello-world")))
                  (test-equal "unsupported command passes through" 'passthrough (car no-rw))
                  (test-equal "unsupported command unchanged" "echo hello-world" (cdr no-rw))))))

(test-group "rtk-registry-tools"
            (let ((reg (make-registry)))
              (register-rtk-tools! reg)
              (test-assert "rtk_gain registered" (procedure? (registry-lookup reg "rtk_gain")))))

(spec-end)
