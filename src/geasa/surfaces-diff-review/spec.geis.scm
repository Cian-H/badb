(define-module (test surfaces-diff-review-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa surfaces-diff-review plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin))

(spec-begin "surfaces-diff-review")

(test-group "guardrail-approval-overlay"
            (let* ((decision #f)
                   (ov (make-guardrail-approval-overlay
                         #:risk 'high
                         #:title "Confirm Shell"
                         #:command "rm -rf /tmp/test"
                         #:preview "Deletes /tmp/test"
                         #:detail "Full explanation"
                         #:on-decision (lambda (d) (set! decision d))))
                   (st (modal-overlay-state ov))
                   (lines '()))
              (test-assert "is modal overlay" (modal-overlay? ov))
              (test-assert "state predicate" (guardrail-approval-state? st))
              ((overlay-render ov) ov
               `((draw-ops! . ,(lambda (ops _r) (set! lines (render-ops->lines ops 80 24)))))
               24 80 0)
              (test-assert "rendered non-empty" (> (length lines) 0))
              (test-equal "Enter toggles detail" 'handled
                          ((overlay-on-key ov) ov (char->integer #\newline) '()))
              (test-assert "detail shown" (modal-show-detail? ov))
              (test-equal "a allows once" 'close
                          ((overlay-on-key ov) ov (char->integer #\a) '()))
              (test-equal "decision recorded" 'allow-once decision)
              (test-equal "state decision" 'allow-once (guardrail-approval-decision st)))
            (let ((d2 #f))
              (let ((ov2 (make-guardrail-approval-overlay #:on-decision (lambda (d) (set! d2 d)))))
                ((overlay-on-key ov2) ov2 (char->integer #\A) '())
                (test-equal "A allows session" 'allow-session d2)))
            (let ((d3 #f))
              (let ((ov3 (make-guardrail-approval-overlay #:on-decision (lambda (d) (set! d3 d)))))
                ((overlay-on-key ov3) ov3 (char->integer #\d) '())
                (test-equal "d denies" 'deny d3))))

(test-group "diff-review-overlay"
            (let* ((reverted '())
                   (files '(((path . "src/a.scm") (status . "M"))
                            ((path . "src/b.scm") (status . "A"))))
                   (ov (make-diff-review-overlay
                         #:files files
                         #:diff-fn (lambda (e) (string-append "--- " (assq-ref e 'path)))
                         #:on-revert (lambda (e) (set! reverted (cons (assq-ref e 'path) reverted)))))
                   (st (modal-overlay-state ov))
                   (lines '()))
              (test-assert "is modal overlay" (modal-overlay? ov))
              (test-assert "state predicate" (diff-review-state? st))
              ((overlay-render ov) ov
               `((draw-ops! . ,(lambda (ops _r) (set! lines (render-ops->lines ops 80 24)))))
               24 80 0)
              (test-assert "rendered non-empty" (> (length lines) 0))
              (test-equal "j moves down" 'handled
                          ((overlay-on-key ov) ov (char->integer #\j) '()))
              (test-equal "selected b.scm" "src/b.scm"
                          (assq-ref (diff-review-selected-file st) 'path))
              (test-equal "first r prompts confirm" 'handled
                          ((overlay-on-key ov) ov (char->integer #\r) '()))
              (test-equal "not yet reverted" '() reverted)
              (test-equal "second r reverts" 'handled
                          ((overlay-on-key ov) ov (char->integer #\r) '()))
              (test-equal "reverted b.scm" '("src/b.scm") reverted)
              (test-equal "q closes" 'close
                          ((overlay-on-key ov) ov (char->integer #\q) '()))))

(test-group "tui-registration"
            (let ((cats '())
                  (cmds '()))
              (register-diff-review-tui!
                #:register-category! (lambda (p _d) (set! cats (cons p cats)))
                #:register-command! (lambda (n _d _a . _r) (set! cmds (cons n cmds)))
                #:open-overlay! (lambda (_ov) #t))
              (test-assert "Workspace category" (member '("Workspace") cats))
              (test-assert "/diff command" (member "diff" cmds))))

(spec-end)
(spec-exit)
