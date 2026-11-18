(define-module (test surfaces-context-inspector-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa surfaces-context-inspector plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin))

(spec-begin "surfaces-context-inspector")

(test-group "modal-render-and-tabs"
            (let* ((compacted? #f)
                   (ov (make-context-inspector-overlay
                         #:budget '((total . 10000)
                                    (used . 4200)
                                    (breakdown . (("System Prompt" . 1200)
                                                  ("Rules" . 800)
                                                  ("History" . 2200))))
                         #:items '(((id . r1) (kind . rule) (name . "geasa-kernel-reference")
                                    (tokens . 400) (detail . "Kernel invariants"))
                                   ((id . m1) (kind . memory) (name . "tui_modular_ux_suite_plan")
                                    (tokens . 400) (detail . "UX plan")))
                         #:usage '((prompt-tokens . 3000) (completion-tokens . 1200)
                                   (cached-tokens . 500) (turns . 4))
                         #:on-compact (lambda (_st) (set! compacted? #t))))
                   (st (modal-overlay-state ov))
                   (lines '()))
              (test-assert "is modal overlay" (modal-overlay? ov))
              (test-assert "state predicate" (context-inspector-state? st))
              ((overlay-render ov) ov
               `((draw-ops! . ,(lambda (ops _rect) (set! lines (render-ops->lines ops 80 24)))))
               24 80 0)
              (test-assert "rendered non-empty" (> (length lines) 0))
              ((overlay-on-key ov) ov (char->integer #\c) '())
              (test-assert "compact callback invoked" compacted?)
              (test-equal "status message set" "Compaction triggered"
                          (context-inspector-status-message st))))

(test-group "tui-registration"
            (let ((cmds '())
                  (pushed '()))
              (register-context-inspector-tui!
                #:register-command! (lambda (name desc handler)
                                      (set! cmds (cons (list name desc handler) cmds)))
                #:push-overlay! (lambda (ov) (set! pushed (cons ov pushed))))
              (test-equal "registered 2 commands" 2 (length cmds))
              ((caddr (assoc "usage" cmds)) "")
              (test-equal "pushed 1 overlay" 1 (length pushed))
              (test-assert "pushed overlay is modal" (modal-overlay? (car pushed)))))

(spec-end)
(spec-exit)
