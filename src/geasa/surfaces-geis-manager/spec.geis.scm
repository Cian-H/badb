(define-module (test surfaces-geis-manager-spec)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa surfaces-geis-manager plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (geasa runtimes-geis-pkg plugin))

(spec-begin "surfaces-geis-manager")

(test-group "extension-registry"
            (clear-geis-manager-extensions!)
            (register-geis-manager-column!
              'status
              #:header "Status"
              #:width 10
              #:render (lambda (scope entry) (status-badge-widget 'ok "current")))
            (register-geis-manager-action!
              #\u
              #:label "update"
              #:handler (lambda (scope entry ctx) 'updated))
            (register-geis-manager-banner!
              'summary
              #:render (lambda (scope) (text-widget "All up to date")))
            (test-equal "one column registered" 1 (length (list-geis-manager-columns)))
            (test-equal "one action registered" 1 (length (list-geis-manager-actions)))
            (test-equal "one banner registered" 1 (length (list-geis-manager-banners)))
            (clear-geis-manager-extensions!)
            (test-equal "cleared columns" 0 (length (list-geis-manager-columns))))

(test-group "modal-lifecycle-and-actions"
            (let*
                ((tmp (mkdtemp "/tmp/badb-gm-XXXXXX"))
                 (ws (string-append tmp "/ws"))
                 (lock-dir (string-append ws "/.badb")))
              (mkdir ws)
              (mkdir lock-dir)
              (geis-pkg-write-lockfile! 'project
                                        `((lock-version . 1) (last-check . #f)
                                          (repos . ())
                                          (geasa
                                            ("surfaces-tui" (version . "2026.11.18.0") (pinned? . #f) (enabled? . #t))
                                            ("capabilities-rtk" (version . "2026.11.18.1") (pinned? . #f) (enabled? . #t))))
                                        ws)
              (let*
                  ((ov (make-geis-manager-overlay #:scope 'project #:workspace ws))
                   (st (modal-overlay-state ov))
                   (lines '()))
                (test-assert "is modal overlay" (modal-overlay? ov))
                (test-equal "scope is project" 'project (geis-manager-scope st))
                (test-equal "two entries loaded" 2 (length (geis-manager-entries st)))
                ((overlay-render ov) ov
                 `((draw-ops! .
                              ,(lambda (ops _rect)
                                 (set! lines (render-ops->lines ops 80 24)))))
                 24
                 80
                 0)
                (test-assert "rendered non-empty" (> (length lines) 0))
                ;; Sorted alphabetically: capabilities-rtk (0), surfaces-tui (1)
                (test-equal "t disables non-core"
                            'handled
                            ((overlay-on-key ov) ov (char->integer #\t) '()))
                (test-equal "p pins" 'handled ((overlay-on-key ov) ov (char->integer #\p) '()))
                ;; Navigate down to surfaces-tui (core) and check core protection
                (test-equal "j moves down"
                            'handled
                            ((overlay-on-key ov) ov (char->integer #\j) '()))
                (test-equal "space on core"
                            'handled
                            ((overlay-on-key ov) ov (char->integer #\space) '()))
                (test-equal "core protection message"
                            "Core geis cannot be disabled"
                            (geis-manager-status-message st))
                (test-equal "Enter toggles detail"
                            'handled
                            ((overlay-on-key ov) ov (char->integer #\newline) '()))
                (test-assert "detail is shown" (modal-show-detail? ov))
                (test-equal "q closes" 'close ((overlay-on-key ov) ov (char->integer #\q) '())))
              (system* "rm" "-rf" tmp)))

(test-group "dogfood-filter-and-dialogs"
            (let*
                ((tmp (mkdtemp "/tmp/badb-gm-df-XXXXXX"))
                 (ws (string-append tmp "/ws"))
                 (lock-dir (string-append ws "/.badb")))
              (mkdir ws)
              (mkdir lock-dir)
              (geis-pkg-write-lockfile! 'project
                                        `((lock-version . 1) (last-check . #f)
                                          (repos . ())
                                          (geasa
                                            ("surfaces-tui" (version . "2026.11.18.0") (pinned? . #f) (enabled? . #t))
                                            ("capabilities-rtk" (version . "2026.11.18.1") (pinned? . #f) (enabled? . #t))))
                                        ws)
              (let*
                  ((stack (make-overlay-stack))
                   (ov (make-geis-manager-overlay #:scope 'project #:workspace ws))
                   (st (modal-overlay-state ov))
                   (lines '()))
                (overlay-stack-push-modal! stack ov)
                ;; Test / filter
                (test-equal "/ activates filter" 'handled (overlay-stack-handle-key stack (char->integer #\/) '()))
                (overlay-stack-handle-key stack (char->integer #\r) '())
                (overlay-stack-handle-key stack (char->integer #\t) '())
                (overlay-stack-handle-key stack (char->integer #\k) '())
                (overlay-stack-render stack
                                      `((draw-ops! . ,(lambda (ops _r)
                                                        (set! lines (render-ops->lines ops 80 24)))))
                                      24 80 0)
                (test-assert "filtered to capabilities-rtk"
                             (any (lambda (l) (string-contains l "capabilities-rtk")) lines))
                (test-assert "surfaces-tui filtered out"
                             (not (any (lambda (l) (string-contains l "surfaces-tui")) lines)))
                ;; Esc deactivates/clears filter without closing manager
                (overlay-stack-handle-key stack 27 '())
                (test-equal "manager still on stack" ov (overlay-stack-top-modal stack))
                ;; Test 'a' pushes input dialog; Esc cancels back to manager
                (overlay-stack-handle-key stack (char->integer #\a) '())
                (test-assert "input dialog pushed" (not (eq? ov (overlay-stack-top-modal stack))))
                (overlay-stack-handle-key stack 27 '())
                (test-equal "esc returns to manager" ov (overlay-stack-top-modal stack))
                ;; Test 'd' on capabilities-rtk (index 0) pushes confirm dialog; 'y' removes & pops
                (overlay-stack-handle-key stack (char->integer #\d) '())
                (test-assert "confirm dialog pushed" (not (eq? ov (overlay-stack-top-modal stack))))
                (overlay-stack-handle-key stack (char->integer #\y) '())
                (test-equal "confirm popped back to manager" ov (overlay-stack-top-modal stack))
                (test-equal "capabilities-rtk removed" 1 (length (geis-manager-entries st))))
              (system* "rm" "-rf" tmp)))

(test-group "tui-registration"
            (let ((cats '())
                  (cmds '()))
              (register-geis-manager-tui!
                #:register-category! (lambda (path desc) (set! cats (cons path cats)))
                #:register-command! (lambda (name desc action . rest) (set! cmds (cons name cmds)))
                #:open-overlay! (lambda (ov) #t))
              (test-assert "Geasa category registered" (member '("Geasa") cats))
              (test-assert "/geasa command registered" (member "geasa" cmds))))

(spec-end)
(spec-exit)
