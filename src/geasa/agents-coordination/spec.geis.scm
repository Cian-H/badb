;;; test-agents-coordination.scm — Tests for Multi-Agent Coordination
;;; (geasa agents-coordination plugin)
;;;
;;; Verifies coordinator construction and registration, point-to-point
;;; send, tolerant broadcast with per-participant result records,
;;; first-success failover, and sequential pipelines (including stage
;;; failure context).

(define-module (test test-agents-coordination)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (srfi srfi-1)
               #:use-module (geasa agents-lifecycle plugin)
               #:use-module (geasa agents-coordination plugin))

(spec-begin "agents-coordination")

;;;
;;; 1. Construction & registration
;;;

(test-group "coordination-construction"
            (let ((coord (make-coordinator)))
              (test-assert "coordinator? recognizes record" (coordinator? coord))
              (test-equal "fresh coordinator has no participants"
                          '() (coordinator-participants coord))

              (test-equal "register returns the id"
                          "alpha" (coordinator-register! coord "alpha" (lambda (m) m)))
              (coordinator-register! coord 'beta (lambda (m) m))
              (test-equal "participants in registration order"
                          '("alpha" "beta") (coordinator-participant-ids coord))

              (test-assert "duplicate id rejected"
                           (catch #t
                             (lambda () (coordinator-register! coord "alpha" (lambda (m) m)) #f)
                             (lambda args #t)))
              (test-assert "non-procedure handler rejected"
                           (catch #t
                             (lambda () (coordinator-register! coord "gamma" 42) #f)
                             (lambda args #t)))
              (test-assert "empty id rejected"
                           (catch #t
                             (lambda () (coordinator-register! coord "" (lambda (m) m)) #f)
                             (lambda args #t)))

              (test-equal "unregister returns the id"
                          "beta" (coordinator-unregister! coord "beta"))
              (test-equal "unregistered participant is gone"
                          '("alpha") (coordinator-participant-ids coord))
              (test-assert "unregister on unknown id errors"
                           (catch #t
                             (lambda () (coordinator-unregister! coord "beta") #f)
                             (lambda args #t)))))

;;;
;;; 2. Point-to-point send
;;;

(test-group "coordination-send"
            (let* ((coord (make-coordinator))
                   (received '())
                   (_ (coordinator-register!
                        coord "echo"
                        (lambda (message)
                          (set! received (cons message received))
                          (list 'echo message)))))
              (test-equal "send returns the handler's response"
                          '(echo hello) (coordinator-send coord "echo" 'hello))
              (test-equal "handler received the message" '(hello) received)
              (test-equal "send resolves symbol ids"
                          '(echo hi) (coordinator-send coord 'echo 'hi))

              (test-assert "send to unknown participant errors"
                           (catch #t
                             (lambda () (coordinator-send coord "ghost" 'msg) #f)
                             (lambda args #t)))

              (coordinator-register! coord "broken" (lambda (m) (error "handler blew up")))
              (test-equal "handler failure propagates to the caller"
                          'misc-error
                          (catch #t
                            (lambda () (coordinator-send coord "broken" 'msg))
                            (lambda (key . args) key)))))

;;;
;;; 3. Tolerant broadcast
;;;

(test-group "coordination-broadcast"
            (let* ((coord (make-coordinator))
                   (order '())
                   (_ (coordinator-register!
                        coord "first"
                        (lambda (m) (set! order (cons 'first order)) 'r1)))
                   (_ (coordinator-register!
                        coord "broken"
                        (lambda (m) (set! order (cons 'broken order)) (error "nope"))))
                   (_ (coordinator-register!
                        coord "last"
                        (lambda (m) (set! order (cons 'last order)) 'r3)))
                   (results (coordinator-broadcast coord 'ping)))
              (test-equal "every participant ran, in order"
                          '(last broken first) order)
              (test-equal "one result per participant"
                          3 (length results))
              (test-assert "results are records"
                           (every coordination-result? results))

              (let ((r1 (car results)) (r2 (cadr results)) (r3 (caddr results)))
                (test-equal "first result agent-id" "first" (coordination-result-agent-id r1))
                (test-equal "first result ok" 'ok (coordination-result-status r1))
                (test-equal "first result value" 'r1 (coordination-result-value r1))
                (test-assert "ok result recognized" (coordination-result-ok? r1))

                (test-equal "failing participant captured, not thrown"
                            'error (coordination-result-status r2))
                (test-assert "error result not ok" (not (coordination-result-ok? r2)))
                (test-equal "error value carries the original key"
                            'misc-error (car (coordination-result-value r2)))

                (test-equal "later participants still ran after a failure"
                            'ok (coordination-result-status r3))
                (test-equal "last result value" 'r3 (coordination-result-value r3)))))

;;;
;;; 4. First-success failover
;;;

(test-group "coordination-first-success"
            (let* ((coord (make-coordinator))
                   (calls '())
                   (_ (coordinator-register!
                        coord "a" (lambda (m) (set! calls (cons 'a calls)) (error "a fails"))))
                   (_ (coordinator-register!
                        coord "b" (lambda (m) (set! calls (cons 'b calls)) 'from-b)))
                   (_ (coordinator-register!
                        coord "c" (lambda (m) (set! calls (cons 'c calls)) 'from-c))))
              (test-equal "first success wins"
                          'from-b (coordinator-first-success coord 'task))
              (test-equal "participants after the winner never ran"
                          '(b a) calls))

            (let ((coord (make-coordinator)))
              (coordinator-register! coord "x" (lambda (m) (error "x fails")))
              (coordinator-register! coord "y" (lambda (m) (throw 'boom "y fails")))
              (test-equal "all-fail throws coordination-failed"
                          '("x" "y")
                          (catch 'coordination-failed
                            (lambda () (coordinator-first-success coord 'task))
                            (lambda (key failed-ids) failed-ids)))))

;;;
;;; 5. Sequential pipeline
;;;

(test-group "coordination-pipeline"
            (let* ((coord (make-coordinator))
                   (_ (coordinator-register!
                        coord "upcase"
                        (lambda (m) (string-upcase m))))
                   (_ (coordinator-register!
                        coord "reverse"
                        (lambda (m) (string-reverse m)))))
              (test-equal "pipeline chains handler outputs"
                          "LLORT" (coordinator-pipeline coord '("upcase" "reverse") "troll"))
              (test-equal "single-stage pipeline"
                          "TROLL" (coordinator-pipeline coord '("upcase") "troll"))
              (test-assert "pipeline on unknown participant errors"
                           (catch #t
                             (lambda ()
                               (coordinator-pipeline coord '("upcase" "ghost") "x") #f)
                             (lambda args #t))))

            (let* ((coord (make-coordinator))
                   (_ (coordinator-register! coord "ok1" (lambda (m) (string-append m "-1"))))
                   (_ (coordinator-register!
                        coord "bad" (lambda (m) (throw 'stage-boom "exploded")))))
              (test-equal "stage failure carries the stage id and original key"
                          '("bad" stage-boom)
                          (catch 'coordination-pipeline-error
                            (lambda () (coordinator-pipeline coord '("ok1" "bad") "seed"))
                            (lambda (key stage-id original-key . rest)
                              (list stage-id original-key))))
              (test-equal "pipeline aborts at the failing stage"
                          'stage-boom
                          (catch 'coordination-pipeline-error
                            (lambda ()
                              (coordinator-pipeline coord '("bad" "ok1") "seed"))
                            (lambda (key stage-id original-key . rest) original-key)))))

;;;
;;; 6. Harness-to-harness event bus
;;;

(test-group "harness-event-bus"
            (clear-subagent-links!)
            (let ((bus (make-harness-event-bus #:capacity 3))
                  (received-root '())
                  (received-mid '()))
              (test-assert "harness-event-bus? recognizes bus" (harness-event-bus? bus))
              (register-subagent-link! "mid-1" "root-1" #:kind 'local #:status 'running)
              (register-subagent-link! "leaf-1" "mid-1" #:kind 'local #:status 'running)

              (let ((sub-root (harness-event-subscribe!
                                (lambda (ev)
                                  (set! received-root (cons (harness-event-kind ev) received-root)))
                                #:bus bus
                                #:target-id "root-1"
                                #:include-descendants? #t))
                    (sub-mid (harness-event-subscribe!
                               (lambda (ev)
                                 (set! received-mid (cons (harness-event-source-id ev) received-mid)))
                               #:bus bus
                               #:target-id "mid-1"
                               #:include-descendants? #f)))
                (parameterize ((current-harness-id "leaf-1")
                               (parent-harness-id "mid-1"))
                              (let ((ev1 (harness-event-bubble! 'thought '((text . "thinking") (iteration . 1)) #:bus bus)))
                                (test-assert "harness-event? recognizes event" (harness-event? ev1))
                                (test-equal "bubbled target resolves to mid-1"
                                            "mid-1" (harness-event-target-id ev1))
                                (test-equal "thought updated leaf link"
                                            "thinking" (subagent-link-last-thought (subagent-link-ref "leaf-1"))))
                              (harness-event-bubble! 'tool-call '((tool . "read_file")) #:bus bus)
                              (test-equal "active tool updated via event bus"
                                          "read_file" (subagent-link-active-tool (subagent-link-ref "leaf-1")))
                              (harness-event-bubble! 'observation '((tool . "read_file") (output . "ok")) #:bus bus)
                              (test-equal "active tool cleared on observation"
                                          #f (subagent-link-active-tool (subagent-link-ref "leaf-1")))
                              (harness-event-bubble! 'status '((status . completed)) #:bus bus))

                ;; Capacity=3 ring buffer retained the last 3 events (seqs 2, 3, 4)
                (let ((polled (harness-event-poll #:bus bus #:target-id "root-1")))
                  (test-equal "ring buffer keeps last 3 events" 3 (length polled))
                  (test-equal "polled seqs monotonic" '(2 3 4) (map harness-event-seq polled)))

                ;; Both root (descendants=#t) and mid (direct target) saw all 4 events
                (test-equal "root subscriber received all 4 descendant events"
                            '(status observation tool-call thought) received-root)
                (test-equal "mid subscriber received all 4 child events"
                            '("leaf-1" "leaf-1" "leaf-1" "leaf-1") received-mid)

                (test-assert "unsubscribe returns true"
                             (harness-event-unsubscribe! sub-root #:bus bus))
                (test-assert "unsubscribe second time returns false"
                             (not (harness-event-unsubscribe! sub-root #:bus bus)))
                (harness-event-unsubscribe! sub-mid #:bus bus)
                (harness-event-bus-clear! #:bus bus #:clear-subscribers? #t)
                (test-equal "bus cleared" '() (harness-event-poll #:bus bus))))
            (clear-subagent-links!))

;;;
;;; 7. Subagent hierarchy tree & overview formatters
;;;

(test-group "subagent-hierarchy-and-overview"
            (clear-subagent-links!)
            (let ((bus (make-harness-event-bus #:capacity 32)))
              ;; Empty root overview
              (let ((empty-ov (format-subagent-overview "root-ov" #:bus bus)))
                (test-assert "empty overview mentions root and none active"
                             (and (string-contains empty-ov "root-ov")
                                  (string-contains empty-ov "none active"))))

              ;; Build 2-level hierarchy: root-ov -> (sub-a -> sub-a1, sub-b)
              (register-subagent-link! "sub-a" "root-ov"
                                       #:kind 'local
                                       #:status 'running
                                       #:active-tool "read_file"
                                       #:turn-count 2
                                       #:label "Inspect core")
              (register-subagent-link! "sub-a1" "sub-a"
                                       #:kind 'worktree
                                       #:status 'completed
                                       #:turn-count 1
                                       #:label "Verify exports")
              (register-subagent-link! "sub-b" "root-ov"
                                       #:kind 'remote
                                       #:status 'failed
                                       #:turn-count 3
                                       #:label "Check tests")
              (harness-event-publish! "sub-a" 'tool-call '((tool . "read_file"))
                                      #:target-id "root-ov" #:bus bus)

              (let* ((tree (subagent-hierarchy-tree "root-ov"))
                     (children (assoc-ref tree 'children))
                     (first-child (car children))
                     (grandchildren (assoc-ref first-child 'children)))
                (test-equal "tree root id" "root-ov" (assoc-ref tree 'id))
                (test-equal "two direct children" 2 (length children))
                (test-equal "one grandchild under sub-a" 1 (length grandchildren))
                (test-equal "grandchild id" "sub-a1" (assoc-ref (car grandchildren) 'id))

                ;; Filtering stopped/completed/failed children
                (let* ((active-only (subagent-hierarchy-tree #:root-id "root-ov" #:include-stopped? #f))
                       (active-children (assoc-ref active-only 'children)))
                  (test-equal "only running child kept when include-stopped? #f"
                              1 (length active-children))
                  (test-equal "running child is sub-a"
                              "sub-a" (assoc-ref (car active-children) 'id))
                  (test-equal "completed grandchild filtered"
                              '() (assoc-ref (car active-children) 'children)))

                ;; Unicode connectors, status, active tool, turn count
                (let ((rendered (format-subagent-hierarchy tree)))
                  (test-assert "renders branch connector" (string-contains rendered "├── sub-a"))
                  (test-assert "renders nested connector" (string-contains rendered "│   └── sub-a1"))
                  (test-assert "renders last child connector" (string-contains rendered "└── sub-b"))
                  (test-assert "renders running status" (string-contains rendered "[running]"))
                  (test-assert "renders active tool" (string-contains rendered "tool:read_file"))
                  (test-assert "renders turn count" (string-contains rendered "turns:2"))
                  (test-assert "renders label" (string-contains rendered "Inspect core")))

                ;; ASCII connectors
                (let ((ascii (format-subagent-hierarchy tree #:ascii? #t)))
                  (test-assert "renders ascii branch connector" (string-contains ascii "|-- sub-a"))
                  (test-assert "renders ascii nested connector" (string-contains ascii "|   `-- sub-a1")))

                ;; Overview
                (let ((ov (format-subagent-overview "root-ov" #:bus bus #:recent-events 3)))
                  (test-assert "overview shows total:3" (string-contains ov "total:3"))
                  (test-assert "overview shows running:1" (string-contains ov "running:1"))
                  (test-assert "overview shows completed:1" (string-contains ov "completed:1"))
                  (test-assert "overview shows failed:1" (string-contains ov "failed:1"))
                  (test-assert "overview includes tree" (string-contains ov "├── sub-a"))
                  (test-assert "overview includes recent events" (string-contains ov "Recent events:"))
                  (test-assert "overview includes tool-call event" (string-contains ov "read_file")))))
            (clear-subagent-links!))

(spec-end)
(spec-exit)
