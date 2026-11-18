(define-module (test surfaces-agent-monitor-spec)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa surfaces-agent-monitor plugin)
               #:use-module (geasa surfaces-notify plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin))

(spec-begin "surfaces-agent-monitor")

(test-group "mission-control-tabs-tail-steer-and-stop"
            (clear-notifications!)
            (let* ((agents
                     (list
                       `((id . "sub-root")
                         (role . "planner")
                         (depth . 0)
                         (status . thinking)
                         (iteration . 2)
                         (max-iterations . 8)
                         (events . (((kind . thought) (text . "Planning step 1"))
                                    ((kind . tool) (text . "read_file foo.scm")))))
                       `((id . "sub-leaf")
                         (role . "researcher")
                         (depth . 1)
                         (status . done)
                         (iteration . 4)
                         (max-iterations . 4)
                         (events . (((kind . observation) (text . "Found 3 matches")))))))
                   (steered '())
                   (stopped '())
                   (stack (make-overlay-stack))
                   (ov (make-agent-monitor-overlay
                         #:list-agents (lambda () agents)
                         #:steer-agent! (lambda (id msg)
                                          (set! steered (cons (cons id msg) steered)))
                         #:stop-agent! (lambda (id)
                                         (set! stopped (cons id stopped))
                                         (set! agents
                                               (map (lambda (e)
                                                      (if (equal? (assq-ref e 'id) id)
                                                        (alist-cons 'status 'stopped (alist-delete 'status e))
                                                        e))
                                                    agents)))))
                   (st (modal-overlay-state ov))
                   (lines '()))
              (overlay-stack-push-modal! stack ov)
              (test-assert "is modal overlay" (modal-overlay? ov))
              (test-equal "2 agents on All tab" 2 (length (agent-monitor-entries st)))
              (agent-monitor-tick! st)
              (test-equal "spinner frame incremented" 1 (agent-monitor-spinner-frame st))
              (overlay-stack-render stack
                                    `((draw-ops! . ,(lambda (ops _r)
                                                      (set! lines (render-ops->lines ops 90 24)))))
                                    24 90 0)
              (test-assert "renders indented child"
                           (any (lambda (l) (string-contains l "└─ sub-leaf")) lines))
              (test-assert "renders thinking status"
                           (any (lambda (l) (string-contains l "thinking")) lines))
              ;; Enter toggles detail pane with ReAct events
              (overlay-stack-handle-key stack (char->integer #\newline) '())
              (test-assert "detail expanded" (modal-show-detail? ov))
              (overlay-stack-render stack
                                    `((draw-ops! . ,(lambda (ops _r)
                                                      (set! lines (render-ops->lines ops 90 24)))))
                                    24 90 0)
              (test-assert "detail shows thought event"
                           (any (lambda (l) (string-contains l "Planning step 1")) lines))
              ;; Tab switches to Active (only sub-root)
              (overlay-stack-handle-key stack 9 '())
              (test-equal "Active tab has 1 agent" 1 (length (agent-monitor-entries st)))
              ;; s opens steer input dialog -> type "check tests" + Enter
              (overlay-stack-handle-key stack (char->integer #\s) '())
              (test-assert "steer input dialog pushed" (not (eq? ov (overlay-stack-top-modal stack))))
              (for-each (lambda (c) (overlay-stack-handle-key stack (char->integer c) '()))
                        (string->list "check tests"))
              (overlay-stack-handle-key stack 10 '())
              (test-equal "steer dialog popped" ov (overlay-stack-top-modal stack))
              (test-equal "steered sub-root" '(("sub-root" . "check tests")) steered)
              ;; k opens danger confirm dialog -> y force-stops sub-root & emits notify-warn!
              (overlay-stack-handle-key stack (char->integer #\k) '())
              (test-assert "confirm dialog pushed" (not (eq? ov (overlay-stack-top-modal stack))))
              (overlay-stack-handle-key stack (char->integer #\y) '())
              (test-equal "confirm dialog popped" ov (overlay-stack-top-modal stack))
              (test-equal "stopped sub-root" '("sub-root") stopped)
              (test-assert "warn notification recorded"
                           (any (lambda (n)
                                  (and (eq? (notification-level n) 'warn)
                                       (string-contains (notification-message n) "sub-root")))
                                (notify-history)))
              ;; Active tab now has 0 agents since sub-root is stopped
              (test-equal "Active tab now empty" 0 (length (agent-monitor-entries st)))))

(test-group "tui-registration"
            (let ((cats '())
                  (cmds '()))
              (register-agent-monitor-tui!
                #:register-category! (lambda (path _desc) (set! cats (cons path cats)))
                #:register-command! (lambda (name _desc _act . _rest) (set! cmds (cons name cmds)))
                #:open-overlay! (lambda (_ov) #t))
              (test-assert "Agents category registered" (member '("Agents") cats))
              (test-assert "/agents command registered" (member "agents" cmds))))

(spec-end)
(spec-exit)
