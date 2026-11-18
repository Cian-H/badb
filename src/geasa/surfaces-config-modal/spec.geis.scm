(define-module (test surfaces-config-modal-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa surfaces-config-modal plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin))

(spec-begin "surfaces-config-modal")

(test-group "config-workbench-actions"
            (let* ((highlighted '())
                   (saved #f)
                   (ov (make-config-modal-overlay
                         #:providers '(((key . "model") (label . "Default Model") (value . "claude") (type . string)))
                         #:guardrails '(((key . "strict") (label . "Strict Mode") (value . #t) (type . boolean)))
                         #:themes '(((key . "dark") (label . "Dark") (value . "dark"))
                                    ((key . "light") (label . "Light") (value . "light")))
                         #:synthesizer '(((key . "stream") (label . "Stream") (value . #t) (type . boolean)))
                         #:on-highlight (lambda (it) (set! highlighted (cons (assq-ref it 'key) highlighted)))
                         #:on-save (lambda (sc snap) (set! saved (cons sc snap)))))
                   (st (modal-overlay-state ov))
                   (lines '()))
              (test-assert "is modal overlay" (modal-overlay? ov))
              (test-assert "state predicate" (config-modal-state? st))
              (test-equal "initial scope user" 'user (config-modal-scope st))
              ((overlay-render ov) ov
               `((draw-ops! . ,(lambda (ops _r) (set! lines (render-ops->lines ops 80 24)))))
               24 80 0)
              (test-assert "rendered non-empty" (> (length lines) 0))
              ;; Switch to Guardrails tab and toggle boolean
              (test-equal "Tab to guardrails" 'handled
                          ((overlay-on-key ov) ov (char->integer #\tab) '()))
              (test-equal "Space toggles bool" 'handled
                          ((overlay-on-key ov) ov (char->integer #\space) '()))
              (test-equal "strict toggled to #f" #f
                          (assq-ref (config-modal-selected-item ov) 'value))
              ;; Switch to Theme tab and move down to trigger on-highlight
              (test-equal "Tab to theme" 'handled
                          ((overlay-on-key ov) ov (char->integer #\tab) '()))
              (test-equal "j moves in theme" 'handled
                          ((overlay-on-key ov) ov (char->integer #\j) '()))
              (test-equal "on-highlight fired" '("light") highlighted)
              ;; Toggle scope and save
              (test-equal "w toggles scope" 'handled
                          ((overlay-on-key ov) ov (char->integer #\w) '()))
              (test-equal "scope is project" 'project (config-modal-scope st))
              (test-equal "s saves" 'handled
                          ((overlay-on-key ov) ov (char->integer #\s) '()))
              (test-assert "on-save called" (pair? saved))
              (test-equal "saved scope is project" 'project (car saved))
              (test-equal "q closes" 'close
                          ((overlay-on-key ov) ov (char->integer #\q) '()))))

(test-group "tui-registration"
            (let ((cats '())
                  (cmds '()))
              (register-config-modal-tui!
                #:register-category! (lambda (p _d) (set! cats (cons p cats)))
                #:register-command! (lambda (n _d _a . _r) (set! cmds (cons n cmds))))
              (test-assert "Settings category" (member '("Settings") cats))
              (test-assert "/config command" (member "config" cmds))))

(spec-end)
(spec-exit)
