;;; test-agents-lifecycle.scm — Tests for Agent Lifecycle (geasa agents-lifecycle plugin)
;;;
;;; Verifies agent construction and validation, registration (duplicates
;;; rejected, unknown ids rejected), the state machine (created -> running
;;; -> stopped, start/stop failure handling, idempotent stop, restart),
;;; timestamps and result/error recording, stop-all ordering, and
;;; WITH-AGENT cleanup on both normal and throwing bodies.

(define-module (test test-agents-lifecycle)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (srfi srfi-1)
               #:use-module (geasa agents-lifecycle plugin))

(spec-begin "agents-lifecycle")

;;;
;;; 1. Agent Construction
;;;

(test-group "agent-construction"
            (let ((agent (make-agent "worker-1" #:description "A test worker")))
              (test-assert "agent? recognizes record" (agent? agent))
              (test-equal "id preserved" "worker-1" (agent-id agent))
              (test-equal "description preserved" "A test worker" (agent-description agent))
              (test-equal "fresh agent is created" 'created (agent-state agent))
              (test-assert "start thunk defaults to a procedure"
                           (procedure? (agent-start-thunk agent)))
              (test-assert "stop thunk defaults to a procedure"
                           (procedure? (agent-stop-thunk agent)))
              (test-assert "no started-at yet" (not (agent-started-at agent)))
              (test-assert "no stopped-at yet" (not (agent-stopped-at agent)))
              (test-assert "no result yet" (not (agent-result agent)))
              (test-assert "no error yet" (not (agent-error agent))))

            (test-assert "empty id rejected"
                         (catch #t (lambda () (make-agent "") #f) (lambda args #t)))
            (test-assert "non-string id rejected"
                         (catch #t (lambda () (make-agent 42) #f) (lambda args #t)))
            (test-assert "non-procedure start rejected"
                         (catch #t (lambda () (make-agent "x" #:start "nope") #f) (lambda args #t)))
            (test-assert "non-procedure stop rejected"
                         (catch #t (lambda () (make-agent "x" #:stop 'nope) #f) (lambda args #t))))

;;;
;;; 2. Registration
;;;

(test-group "agent-registration"
            (let* ((manager (make-agent-manager))
                   (agent (make-agent "alpha")))
              (test-assert "agent-manager? recognizes record" (agent-manager? manager))
              (test-equal "fresh manager has no agents" '() (agent-manager-ids manager))
              (test-assert "register returns the agent"
                           (eq? agent (agent-register! manager agent)))
              (test-equal "registered id listed" '("alpha") (agent-manager-ids manager))
              (test-assert "agent-ref finds by string id" (eq? agent (agent-ref manager "alpha")))
              (test-assert "agent-ref finds by symbol id" (eq? agent (agent-ref manager 'alpha)))
              (test-assert "agent-ref on unknown id returns #f"
                           (not (agent-ref manager "ghost")))

              (test-assert "duplicate id rejected"
                           (catch #t
                             (lambda () (agent-register! manager (make-agent "alpha")) #f)
                             (lambda args #t)))
              (test-assert "registering a non-agent rejected"
                           (catch #t
                             (lambda () (agent-register! manager "not-an-agent") #f)
                             (lambda args #t)))

              (let ((beta (agent-register! manager (make-agent "beta"))))
                (test-equal "registration order preserved"
                            '("alpha" "beta") (agent-manager-ids manager))
                (test-assert "second agent is a distinct record" (not (eq? beta agent))))))

;;;
;;; 3. Start / Stop state machine
;;;

(test-group "agent-start-stop"
            (let* ((events '())
                   (manager (make-agent-manager))
                   (agent (agent-register!
                            manager
                            (make-agent "srv"
                                        #:start (lambda () (set! events (cons 'start events)) 'started-value)
                                        #:stop (lambda () (set! events (cons 'stop events)) 'stopped-value)))))
              (test-assert "not running before start" (not (agent-running? manager "srv")))

              (let ((started (agent-start! manager "srv")))
                (test-assert "start! returns the agent" (eq? started agent))
                (test-equal "state is running" 'running (agent-state agent))
                (test-assert "running? is true" (agent-running? manager "srv"))
                (test-assert "started-at recorded" (number? (agent-started-at agent)))
                (test-equal "start result recorded" 'started-value (agent-result agent))
                (test-equal "no stopped-at while running" #f (agent-stopped-at agent))
                (test-equal "events show start" '(start) events))

              (test-assert "starting a running agent errors"
                           (catch #t
                             (lambda () (agent-start! manager "srv") #f)
                             (lambda args #t)))

              (let ((stopped (agent-stop! manager "srv")))
                (test-assert "stop! returns the agent" (eq? stopped agent))
                (test-equal "state is stopped" 'stopped (agent-state agent))
                (test-assert "not running after stop" (not (agent-running? manager "srv")))
                (test-assert "stopped-at recorded" (number? (agent-stopped-at agent)))
                (test-equal "events show start then stop" '(stop start) events))

              ;; Idempotent stop: no second stop-thunk invocation.
              (agent-stop! manager "srv")
              (test-equal "second stop does not re-invoke the stop thunk"
                          '(stop start) events)
              (test-equal "state remains stopped" 'stopped (agent-state agent))

              ;; Restart from stopped is allowed.
              (agent-start! manager "srv")
              (test-equal "restart from stopped is allowed"
                          '(start stop start) events)
              (test-equal "state is running again" 'running (agent-state agent))
              (agent-stop! manager "srv"))

            (test-assert "start on unknown id errors"
                         (catch #t
                           (lambda () (agent-start! (make-agent-manager) "ghost") #f)
                           (lambda args #t)))
            (test-assert "stop on unknown id errors"
                         (catch #t
                           (lambda () (agent-stop! (make-agent-manager) "ghost") #f)
                           (lambda args #t))))

;;;
;;; 4. Failure handling
;;;

(test-group "agent-failures"
            (let* ((manager (make-agent-manager))
                   (agent (agent-register!
                            manager
                            (make-agent "boom" #:start (lambda () (error "cannot start"))))))
              (test-equal "start failure rethrows with the original key"
                          'misc-error
                          (catch #t
                            (lambda () (agent-start! manager "boom"))
                            (lambda (key . args) key)))
              (test-equal "failed state recorded" 'failed (agent-state agent))
              (test-equal "error recorded" 'misc-error (car (agent-error agent)))
              (test-assert "stop transitions failed to stopped"
                           (eq? 'stopped (agent-state (agent-stop! manager "boom")))))

            (let* ((manager (make-agent-manager))
                   (agent (agent-register!
                            manager
                            (make-agent "clean" #:stop (lambda () (error "cleanup failed"))))))
              (agent-start! manager "clean")
              (test-equal "stop failure rethrows with the original key"
                          'misc-error
                          (catch #t
                            (lambda () (agent-stop! manager "clean"))
                            (lambda (key . args) key)))
              (test-equal "stop failure marks the agent failed"
                          'failed (agent-state agent))
              (test-equal "stop error recorded" 'misc-error (car (agent-error agent))))

            (let* ((stopped-log '())
                   (manager (make-agent-manager))
                   (agent (agent-register!
                            manager
                            (make-agent "never"
                                        #:stop (lambda () (set! stopped-log (cons 'never stopped-log)))))))
              ;; A never-started agent stops without invoking its stop thunk.
              (agent-stop! manager "never")
              (test-equal "stop thunk not invoked for a created agent" '() stopped-log)
              (test-equal "created agent transitions directly to stopped"
                          'stopped (agent-state agent))))

;;;
;;; 5. Stop-all and running sets
;;;

(test-group "agent-stop-all"
            (let* ((stop-log '())
                   (manager (make-agent-manager))
                   (mk (lambda (id)
                         (agent-register!
                           manager
                           (make-agent id #:stop
                                       (lambda () (set! stop-log (cons id stop-log))))))))
              (mk "one") (mk "two") (mk "three")
              (for-each (lambda (id) (agent-start! manager id))
                        '("one" "two" "three"))
              (test-equal "running ids in registration order"
                          '("one" "two" "three") (agent-manager-running-ids manager))
              (let ((stopped (agent-manager-stop-all! manager)))
                (test-equal "stop-all returns the stopped ids in reverse order"
                            '("three" "two" "one") stopped)
                (test-equal "stop thunks ran in reverse registration order"
                            '("one" "two" "three") stop-log)
                (test-equal "no agents left running" '() (agent-manager-running-ids manager))
                (test-equal "stop-all on empty manager is a no-op"
                            '() (agent-manager-stop-all! manager)))))

;;;
;;; 6. with-agent cleanup
;;;

(test-group "agent-with-agent"
            (let* ((log '())
                   (manager (make-agent-manager))
                   (agent (make-agent "scoped"
                                      #:start (lambda () (set! log (cons 'start log)))
                                      #:stop (lambda () (set! log (cons 'stop log))))))
              (test-equal "with-agent returns the body value"
                          'body-value
                          (with-agent manager agent (lambda () 'body-value)))
              (test-equal "with-agent runs start, body, then stop"
                          '(stop start) log)
              (test-equal "agent stopped after with-agent" 'stopped (agent-state agent))
              (test-assert "agent remains registered"
                           (agent-ref manager "scoped")))

            (let* ((log '())
                   (manager (make-agent-manager))
                   (agent (make-agent "throwing"
                                      #:start (lambda () (set! log (cons 'start log)))
                                      #:stop (lambda () (set! log (cons 'stop log))))))
              (test-equal "body error propagates unchanged"
                          'body-boom
                          (catch #t
                            (lambda ()
                              (with-agent manager agent (lambda () (throw 'body-boom "x"))))
                            (lambda (key . args) key)))
              (test-equal "with-agent stopped the agent despite the body error"
                          '(stop start) log)
              (test-equal "agent stopped after body error" 'stopped (agent-state agent))))

;;;
;;; 7. Harness awareness & parent/child links
;;;

(test-group "harness-awareness-and-links"
            (clear-subagent-links!)
            (test-assert "default current-harness-id is non-empty string"
                         (and (string? (current-harness-id))
                              (not (string-null? (current-harness-id)))))
            (test-assert "root harness is not a subagent by default"
                         (not (subagent-harness?)))
            (test-equal "default parent-harness-id is #f" #f (parent-harness-id))

            (let ((link (register-subagent-link! "sub-1" "root"
                                                 #:kind 'local
                                                 #:status 'running
                                                 #:label "Inspect core")))
              (test-assert "subagent-link? recognizes link" (subagent-link? link))
              (test-equal "child-id stored" "sub-1" (subagent-link-child-id link))
              (test-equal "parent-id stored" "root" (subagent-link-parent-id link))
              (test-equal "kind stored" 'local (subagent-link-kind link))
              (test-equal "status stored" 'running (subagent-link-status link))
              (test-equal "label stored" "Inspect core" (subagent-link-label link))
              (test-assert "subagent-harness? true for registered child id"
                           (subagent-harness? "sub-1"))
              (test-equal "harness-parent-id resolves parent"
                          "root" (harness-parent-id "sub-1"))

              (parameterize ((current-harness-id "sub-1")
                             (parent-harness-id "root"))
                            (test-equal "parameterized current-harness-id"
                                        "sub-1" (current-harness-id))
                            (test-equal "parameterized parent-harness-id"
                                        "root" (parent-harness-id))
                            (test-assert "subagent-harness? true inside child context"
                                         (subagent-harness?)))

              (update-subagent-link! "sub-1"
                                     #:active-tool "read_file"
                                     #:increment-turn? #t
                                     #:last-thought "checking exports")
              (test-equal "active tool updated" "read_file" (subagent-link-active-tool link))
              (test-equal "turn count incremented" 1 (subagent-link-turn-count link))
              (test-equal "last thought recorded" "checking exports" (subagent-link-last-thought link))

              (update-subagent-link! "sub-1" #:active-tool #f #:last-observation "ok")
              (test-equal "active tool cleared with #f" #f (subagent-link-active-tool link))
              (test-equal "last observation recorded" "ok" (subagent-link-last-observation link))

              (let ((children (subagent-children "root")))
                (test-equal "one child under root" 1 (length children))
                (test-equal "child matches link" "sub-1" (subagent-link-child-id (car children)))))

            (let ((seen-inside #f))
              (test-equal "call-with-harness-context returns body value"
                          'done
                          (call-with-harness-context
                            "sub-ctx"
                            (lambda ()
                              (set! seen-inside
                                    (list (current-harness-id)
                                          (parent-harness-id)
                                          (subagent-harness?)))
                              'done)
                            #:parent-id "root"
                            #:kind 'worktree
                            #:label "Scoped run"))
              (test-equal "context inside call-with-harness-context"
                          '("sub-ctx" "root" #t) seen-inside)
              (test-equal "status updated to completed on exit"
                          'completed (subagent-link-status (subagent-link-ref "sub-ctx"))))

            (clear-subagent-links!))

(spec-end)
(spec-exit)
