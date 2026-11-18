(define-module (test surfaces-command-palette-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa surfaces-command-palette plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin))

(spec-begin "surfaces-command-palette")

(test-group "fuzzy-search-and-execute"
            (let* ((ran #f)
                   (ov (make-command-palette-overlay
                         #:commands `(((id . ctx) (label . "/context")
                                       (description . "Inspect context")
                                       (handler . ,(lambda (_c) (set! ran 'ctx))))
                                      ((id . keys) (label . "/keys")
                                       (description . "Keybindings")
                                       (handler . ,(lambda (_c) (set! ran 'keys)))))
                         #:active-tab 'commands))
                   (st (modal-overlay-state ov))
                   (lines '()))
              (test-assert "is modal overlay" (modal-overlay? ov))
              (test-assert "state predicate" (command-palette-state? st))
              (test-assert "fuzzy score matches" (number? (fuzzy-match-score "ctx" "/context")))
              ((overlay-render ov) ov
               `((draw-ops! . ,(lambda (ops _rect) (set! lines (render-ops->lines ops 80 24)))))
               24 80 0)
              (test-assert "rendered non-empty" (> (length lines) 0))
              ((overlay-on-key ov) ov (char->integer #\k) '())
              ((overlay-on-key ov) ov (char->integer #\e) '())
              (test-equal "query updated" "ke" (command-palette-query st))
              (let ((res ((overlay-on-key ov) ov 13 '())))
                (test-equal "enter closes palette" 'close res)
                (test-equal "command handler ran" 'keys ran))))

(test-group "keybinding-rebind-and-duplicate-check"
            (let* ((rebound #f)
                   (ov (make-command-palette-overlay
                         #:keybindings '(((id . a) (key . "a") (label . "Allow"))
                                         ((id . d) (key . "d") (label . "Deny")))
                         #:active-tab 'keybindings
                         #:on-rebind (lambda (id k) (set! rebound (cons id k)))))
                   (st (modal-overlay-state ov)))
              ((overlay-on-key ov) ov 13 '())
              (test-assert "capturing key" (command-palette-capturing-key? st))
              ((overlay-on-key ov) ov (char->integer #\d) '())
              (test-assert "duplicate key detected"
                           (string? (command-palette-status-message st)))
              ((overlay-on-key ov) ov (char->integer #\n) '())
              (test-assert "cancelled capture" (not (command-palette-capturing-key? st)))
              ((overlay-on-key ov) ov 13 '())
              ((overlay-on-key ov) ov (char->integer #\x) '())
              (test-equal "unique keybound" '(a . "x") rebound)))

(test-group "tui-registration"
            (let ((cmds '())
                  (keys '())
                  (pushed '()))
              (register-command-palette-tui!
                #:register-command! (lambda (n d h) (set! cmds (cons (list n d h) cmds)))
                #:register-key! (lambda (k d h) (set! keys (cons (list k d h) keys)))
                #:push-overlay! (lambda (ov) (set! pushed (cons ov pushed))))
              (test-equal "registered /keys" 1 (length cmds))
              (test-equal "registered C-k" 1 (length keys))
              ((caddr (car keys)))
              (test-equal "pushed 1 overlay" 1 (length pushed))))

(spec-end)
(spec-exit)
