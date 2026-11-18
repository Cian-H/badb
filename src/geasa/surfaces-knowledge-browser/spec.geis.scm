(define-module (test surfaces-knowledge-browser-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa surfaces-knowledge-browser plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin))

(spec-begin "surfaces-knowledge-browser")

(test-group "skills-and-memories-modal"
            (let* ((toggled '())
                   (deleted '())
                   (skills '(((name . "debugger") (enabled? . #t) (content . "# Debugger\nStep by step"))
                             ((name . "sbcl-pro") (enabled? . #f) (content . "# SBCL\nLisp"))))
                   (memories '(((name . "user-pref") (scope . "user") (content . "Prefer concise"))
                               ((name . "proj-arch") (scope . "project") (content . "Microkernel"))))
                   (ov (make-knowledge-browser-overlay
                         #:active-tab 'skills
                         #:skills skills
                         #:memories memories
                         #:on-toggle-skill (lambda (it val) (set! toggled (cons (cons (assq-ref it 'name) val) toggled)))
                         #:on-delete-memory (lambda (it) (set! deleted (cons (assq-ref it 'name) deleted)))))
                   (st (modal-overlay-state ov))
                   (lines '()))
              (test-assert "is modal overlay" (modal-overlay? ov))
              (test-assert "state predicate" (knowledge-browser-state? st))
              ((overlay-render ov) ov
               `((draw-ops! . ,(lambda (ops _r) (set! lines (render-ops->lines ops 80 24)))))
               24 80 0)
              (test-assert "rendered non-empty" (> (length lines) 0))
              (test-equal "space toggles skill" 'handled
                          ((overlay-on-key ov) ov (char->integer #\space) '()))
              (test-equal "toggle callback fired" '(("debugger" . #f)) toggled)
              ;; Switch to Memories tab via Tab
              (test-equal "Tab switches tab" 'handled
                          ((overlay-on-key ov) ov (char->integer #\tab) '()))
              (test-equal "active tab is memories" 'memories (modal-active-tab ov))
              (test-equal "first d prompts" 'handled
                          ((overlay-on-key ov) ov (char->integer #\d) '()))
              (test-equal "not deleted yet" '() deleted)
              (test-equal "second d deletes" 'handled
                          ((overlay-on-key ov) ov (char->integer #\d) '()))
              (test-equal "deleted callback fired" '("user-pref") deleted)
              (test-equal "q closes" 'close
                          ((overlay-on-key ov) ov (char->integer #\q) '()))))

(test-group "tui-registration"
            (let ((cats '())
                  (cmds '()))
              (register-knowledge-browser-tui!
                #:register-category! (lambda (p _d) (set! cats (cons p cats)))
                #:register-command! (lambda (n _d _a . _r) (set! cmds (cons n cmds)))
                #:open-overlay! (lambda (_ov) #t))
              (test-assert "Knowledge category" (member '("Knowledge") cats))
              (test-assert "/skills command" (member "skills" cmds))
              (test-assert "/memories command" (member "memories" cmds))))

(spec-end)
(spec-exit)
