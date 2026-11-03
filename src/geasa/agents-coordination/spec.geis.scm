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

(spec-end)
(spec-exit)
