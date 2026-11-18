(define-module (test surfaces-mcp-manager-spec)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa surfaces-mcp-manager plugin)
               #:use-module (geasa surfaces-notify plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin))

(spec-begin "surfaces-mcp-manager")

(test-group "modal-lifecycle-tools-and-actions"
            (clear-notifications!)
            (let* ((servers
                     (list
                       `((name . "filesystem")
                         (command . "npx server-fs")
                         (transport . "stdio")
                         (enabled? . #t)
                         (status . connected)
                         (tools . (((name . "read_file")
                                    (description . "Read file")
                                    (input-schema . ((properties . ((path . ((type . "string")))))
                                                     (required . ("path"))))))))
                       `((name . "github")
                         (command . "npx server-gh")
                         (transport . "stdio")
                         (enabled? . #f)
                         (status . stopped)
                         (tools . ()))))
                   (toggled '())
                   (reconnected '())
                   (added '())
                   (removed '())
                   (stack (make-overlay-stack))
                   (ov (make-mcp-manager-overlay
                         #:scope 'user
                         #:list-servers (lambda (_scope _ws) servers)
                         #:toggle-server! (lambda (_scope name en? _ws)
                                            (set! toggled (cons (cons name en?) toggled))
                                            (set! servers
                                                  (map (lambda (e)
                                                         (if (equal? (assq-ref e 'name) name)
                                                           (alist-cons 'enabled? en? (alist-delete 'enabled? e))
                                                           e))
                                                       servers)))
                         #:reconnect-server! (lambda (_scope name _ws)
                                               (set! reconnected (cons name reconnected))
                                               (set! servers
                                                     (map (lambda (e)
                                                            (if (equal? (assq-ref e 'name) name)
                                                              (alist-cons 'status 'connected (alist-delete 'status e))
                                                              e))
                                                          servers)))
                         #:add-server! (lambda (_scope spec _ws)
                                         (set! added (cons spec added))
                                         (set! servers
                                               (cons `((name . ,spec) (command . ,spec) (transport . "stdio")
                                                       (enabled? . #t) (status . stopped) (tools . ()))
                                                     servers)))
                         #:remove-server! (lambda (_scope name _ws)
                                            (set! removed (cons name removed))
                                            (set! servers
                                                  (remove (lambda (e) (equal? (assq-ref e 'name) name))
                                                          servers)))))
                   (st (modal-overlay-state ov))
                   (lines '()))
              (overlay-stack-push-modal! stack ov)
              (test-assert "is modal overlay" (modal-overlay? ov))
              (test-equal "two servers loaded" 2 (length (mcp-manager-ui-entries st)))
              ;; Sorted: filesystem (0), github (1)
              (overlay-stack-render stack
                                    `((draw-ops! . ,(lambda (ops _r)
                                                      (set! lines (render-ops->lines ops 90 24)))))
                                    24 90 0)
              (test-assert "renders connected badge"
                           (any (lambda (l) (string-contains l "connected")) lines))
              ;; Enter toggles detail pane with tool schema
              (overlay-stack-handle-key stack (char->integer #\newline) '())
              (test-assert "detail expanded" (modal-show-detail? ov))
              (overlay-stack-render stack
                                    `((draw-ops! . ,(lambda (ops _r)
                                                      (set! lines (render-ops->lines ops 90 24)))))
                                    24 90 0)
              (test-assert "detail shows read_file tool"
                           (any (lambda (l) (string-contains l "read_file")) lines))
              (test-assert "detail shows path* param"
                           (any (lambda (l) (string-contains l "path*")) lines))
              ;; Space toggles filesystem
              (overlay-stack-handle-key stack (char->integer #\space) '())
              (test-equal "toggled filesystem off" '(("filesystem" . #f)) toggled)
              ;; r reconnects & resolves notification
              (overlay-stack-handle-key stack (char->integer #\r) '())
              (test-equal "reconnected filesystem" '("filesystem") reconnected)
              (test-assert "notification recorded in history"
                           (any (lambda (n) (string-contains (notification-message n) "filesystem"))
                                (notify-history)))
              ;; / filter
              (overlay-stack-handle-key stack (char->integer #\/) '())
              (overlay-stack-handle-key stack (char->integer #\g) '())
              (overlay-stack-handle-key stack (char->integer #\i) '())
              (overlay-stack-render stack
                                    `((draw-ops! . ,(lambda (ops _r)
                                                      (set! lines (render-ops->lines ops 90 24)))))
                                    24 90 0)
              (test-assert "filtered to github"
                           (any (lambda (l) (string-contains l "github")) lines))
              (test-assert "filesystem filtered out"
                           (not (any (lambda (l) (string-contains l "server-fs")) lines)))
              ;; Esc clears filter
              (overlay-stack-handle-key stack 27 '())
              (test-equal "still on mcp manager" ov (overlay-stack-top-modal stack))
              ;; a opens input dialog -> type "slack", Enter -> adds server
              (overlay-stack-handle-key stack (char->integer #\a) '())
              (test-assert "input dialog pushed" (not (eq? ov (overlay-stack-top-modal stack))))
              (for-each (lambda (c) (overlay-stack-handle-key stack (char->integer c) '()))
                        (string->list "slack"))
              (overlay-stack-handle-key stack 10 '())
              (test-equal "input dialog popped" ov (overlay-stack-top-modal stack))
              (test-equal "added slack" '("slack") added)
              (test-equal "three servers now" 3 (length (mcp-manager-ui-entries st)))
              ;; d opens confirm dialog -> y removes selected (filesystem)
              (overlay-stack-handle-key stack (char->integer #\d) '())
              (test-assert "confirm dialog pushed" (not (eq? ov (overlay-stack-top-modal stack))))
              (overlay-stack-handle-key stack (char->integer #\y) '())
              (test-equal "confirm dialog popped" ov (overlay-stack-top-modal stack))
              (test-equal "removed filesystem" '("filesystem") removed)
              (test-equal "two servers left" 2 (length (mcp-manager-ui-entries st)))))

(test-group "tui-registration"
            (let ((cats '())
                  (cmds '()))
              (register-mcp-manager-tui!
                #:register-category! (lambda (path _desc) (set! cats (cons path cats)))
                #:register-command! (lambda (name _desc _act . _rest) (set! cmds (cons name cmds)))
                #:open-overlay! (lambda (_ov) #t))
              (test-assert "MCP category registered" (member '("MCP") cats))
              (test-assert "/mcp command registered" (member "mcp" cmds))))

(spec-end)
(spec-exit)
