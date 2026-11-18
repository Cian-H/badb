(define-module (test surfaces-session-browser-spec)
               #:use-module (srfi srfi-64)
               #:use-module (ice-9 rdelim)
               #:use-module (badb spec)
               #:use-module (badb kernel journal)
               #:use-module (geasa runtimes-rollback plugin)
               #:use-module (geasa surfaces-notify plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (geasa surfaces-session-browser plugin))

(spec-begin "surfaces-session-browser")

(test-group "sessions-tab-filter-and-dialogs"
            (clear-notifications!)
            (let* ((sessions (list `((id . "sess-1")
                                     (title . "Fix auth bug")
                                     (turns . 5)
                                     (timestamp . "2026-11-18 10:00")
                                     (recent-turns . ("user: fix auth" "assistant: patched")))
                                   `((id . "sess-2")
                                     (title . "Add tests")
                                     (turns . 2)
                                     (timestamp . "2026-11-18 11:00")
                                     (recent-turns . ("user: add tests")))))
                   (resumed #f)
                   (forked #f)
                   (deleted #f)
                   (stack (make-overlay-stack))
                   (ov (make-session-browser-overlay
                         #:active-tab 'sessions
                         #:sessions-provider (lambda () sessions)
                         #:on-resume (lambda (s) (set! resumed (assq-ref s 'id)))
                         #:on-fork (lambda (s name)
                                     (set! forked (cons (assq-ref s 'id) name))
                                     (set! sessions
                                           (append sessions
                                                   (list `((id . ,name) (title . ,name) (turns . 0) (timestamp . "now"))))))
                         #:on-delete (lambda (s)
                                       (set! deleted (assq-ref s 'id))
                                       (set! sessions
                                             (filter (lambda (x) (not (equal? (assq-ref x 'id) deleted)))
                                                     sessions)))))
                   (lines '())
                   (draw-ctx `((draw-ops! . ,(lambda (ops _rect)
                                               (set! lines (render-ops->lines ops 96 28))))))
                   (key-ctx `((key-up . 259)
                              (key-down . 258)
                              (key-enter . 10)
                              (key-tab . 9)
                              (key-backspace . 263)
                              (overlay-stack . ,stack))))
              (overlay-stack-push-modal! stack ov)
              (overlay-stack-render stack draw-ctx 28 96 0)
              (let ((txt (string-join lines "\n")))
                (test-assert "renders modal title" (string-contains txt "Sessions & Checkpoints"))
                (test-assert "renders sess-1" (string-contains txt "sess-1"))
                (test-assert "renders recent turn detail" (string-contains txt "user: fix auth")))
              ;; Resume sess-1
              (overlay-stack-handle-key stack (char->integer #\r) key-ctx)
              (test-equal "r resumes sess-1" "sess-1" resumed)
              (test-assert "resume emits notify-info!" (pair? (notify-history)))
              ;; Fork sess-1 via input dialog
              (overlay-stack-handle-key stack (char->integer #\f) key-ctx)
              (test-equal "fork pushes input dialog on stack" 2 (length (overlay-stack-modals stack)))
              (overlay-stack-handle-key stack 10 key-ctx)
              (test-equal "fork submitted default name" '("sess-1" . "sess-1-fork") forked)
              (test-equal "input dialog popped" 1 (length (overlay-stack-modals stack)))
              ;; Filter with /
              (overlay-stack-handle-key stack (char->integer #\/) key-ctx)
              (overlay-stack-handle-key stack (char->integer #\A) key-ctx)
              (overlay-stack-handle-key stack (char->integer #\d) key-ctx)
              (overlay-stack-handle-key stack (char->integer #\d) key-ctx)
              (overlay-stack-handle-key stack 10 key-ctx)
              (overlay-stack-render stack draw-ctx 28 96 0)
              (let ((txt (string-join lines "\n")))
                (test-assert "filter keeps Add tests" (string-contains txt "sess-2"))
                (test-assert "filter hides Fix auth bug" (not (string-contains txt "Fix auth bug"))))
              ;; Delete sess-2 via confirm dialog
              (overlay-stack-handle-key stack (char->integer #\d) key-ctx)
              (test-equal "delete pushes confirm dialog" 2 (length (overlay-stack-modals stack)))
              (overlay-stack-handle-key stack (char->integer #\y) key-ctx)
              (test-equal "y confirms delete" "sess-2" deleted)
              (test-equal "confirm dialog popped" 1 (length (overlay-stack-modals stack))))
            (clear-notifications!))

(test-group "checkpoints-tab-journal-and-rollback"
            (clear-notifications!)
            (let* ((ws (mkdtemp "/tmp/badb-sb-XXXXXX"))
                   (jpath (string-append ws "/journal.jsonl"))
                   (j (make-journal jpath))
                   (rm (make-rollback-manager #:workspace-dir ws #:journal j))
                   (stack (make-overlay-stack)))
              (with-output-to-file (string-append ws "/hello.txt")
                                   (lambda () (display "v1\n")))
              (let ((cp1 (create-checkpoint! rm "initial-v1")))
                (safe-journal-append!
                  j
                  (make-journal-entry #:timestamp "2026-11-18T12:00:00Z"
                                      #:iteration 1
                                      #:feature-id "demo-feat"
                                      #:commit (checkpoint-id cp1)
                                      #:test-verdict "pass"
                                      #:author "tester"
                                      #:details "created initial-v1"
                                      #:ast-diff-hash "ast123"
                                      #:spec-hash "spec456"
                                      #:quickcheck-seed 42
                                      #:quickcheck-verdict "pass"
                                      #:kernel-replay-verdict "pass")))
              (with-output-to-file (string-append ws "/hello.txt")
                                   (lambda () (display "v2\n")))
              (let* ((ov (make-session-browser-overlay
                           #:active-tab 'journal
                           #:rollback-manager rm
                           #:journal j))
                     (lines '())
                     (draw-ctx `((draw-ops! . ,(lambda (ops _rect)
                                                 (set! lines (render-ops->lines ops 100 28))))))
                     (key-ctx `((key-up . 259)
                                (key-down . 258)
                                (key-enter . 10)
                                (key-tab . 9)
                                (overlay-stack . ,stack))))
                (test-equal "active-tab 'journal normalizes to 'checkpoints"
                            'checkpoints
                            (modal-active-tab ov))
                (overlay-stack-push-modal! stack ov)
                (overlay-stack-render stack draw-ctx 28 100 0)
                (let ((txt (string-join lines "\n")))
                  (test-assert "renders checkpoint description" (string-contains txt "initial-v1"))
                  (test-assert "renders provenance feature" (string-contains txt "demo-feat")))
                ;; Create checkpoint via c + input dialog
                (overlay-stack-handle-key stack (char->integer #\c) key-ctx)
                (test-equal "c pushes input dialog" 2 (length (overlay-stack-modals stack)))
                (for-each (lambda (ch) (overlay-stack-handle-key stack (char->integer ch) key-ctx))
                          '(#\v #\2 #\- #\c #\k #\p #\t))
                (overlay-stack-handle-key stack 10 key-ctx)
                (test-equal "2 checkpoints now exist" 2 (length (list-checkpoints rm)))
                ;; Rollback to initial-v1
                (overlay-stack-handle-key stack (char->integer #\j) key-ctx)
                (overlay-stack-handle-key stack (char->integer #\r) key-ctx)
                (test-equal "r pushes confirm dialog" 2 (length (overlay-stack-modals stack)))
                (overlay-stack-handle-key stack (char->integer #\y) key-ctx)
                (test-equal "workspace file rolled back to v1"
                            "v1\n"
                            (call-with-input-file (string-append ws "/hello.txt")
                                                  (lambda (p) (read-string p)))))
              (system* "rm" "-rf" ws))
            (clear-notifications!))

(test-group "tui-registration"
            (let ((cats '())
                  (cmds '())
                  (opened '()))
              (register-session-browser-tui!
                #:register-category! (lambda (p d) (set! cats (cons (cons p d) cats)))
                #:register-command! (lambda (n d h . _rest) (set! cmds (cons (list n d h) cmds)))
                #:open-overlay! (lambda (ov) (set! opened (cons ov opened))))
              (test-assert "Sessions category registered" (pair? cats))
              (let ((s-cmd (assoc "sessions" cmds))
                    (j-cmd (assoc "journal" cmds)))
                (test-assert "sessions command registered" s-cmd)
                (test-assert "journal command registered" j-cmd)
                ((caddr s-cmd))
                ((caddr j-cmd))
                (test-equal "opened 2 modals" 2 (length opened))
                (test-equal "journal command opens checkpoints tab"
                            'checkpoints
                            (modal-active-tab (car opened)))
                (test-equal "sessions command opens sessions tab"
                            'sessions
                            (modal-active-tab (cadr opened))))))

(spec-end)
(spec-exit)
