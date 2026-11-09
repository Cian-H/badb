;;; badb — Multi-Agent Coordination
;;;
;;; Message-passing coordination across multiple agents.  A
;;; <coordinator> holds participants — id/handler pairs where the
;;; handler receives a message and returns a response — and provides
;;; the coordination primitives:
;;;
;;;   COORDINATOR-SEND          point-to-point request/response
;;;   COORDINATOR-BROADCAST     fan-out to all, errors tolerated
;;;   COORDINATOR-FIRST-SUCCESS failover: first participant that succeeds
;;;   COORDINATOR-PIPELINE      sequential chaining of outputs
;;;
;;; Handlers are opaque procedures, so anything with a message-driven
;;; interface (including agents managed by (geasa agents-lifecycle
;;; plugin)) can participate.  Tolerant primitives capture per-agent
;;; failures as <coordination-result> records instead of aborting the
;;; round.

(define-module (geasa agents-coordination plugin)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (<coordinator>
                          make-coordinator
                          coordinator?
                          coordinator-participants
                          coordinator-participant-ids
                          coordinator-register!
                          coordinator-unregister!
                          <coordination-result>
                          coordination-result?
                          coordination-result-agent-id
                          coordination-result-status
                          coordination-result-value
                          coordination-result-ok?
                          coordinator-send
                          coordinator-broadcast
                          coordinator-first-success
                          coordinator-pipeline))

;;;
;;; Result records
;;;

(define-record-type <coordination-result>
                    (%make-coordination-result agent-id status value)
                    coordination-result?
                    (agent-id coordination-result-agent-id)
                    (status coordination-result-status)
                    (value coordination-result-value))

(define (coordination-result-ok? result)
        "True when RESULT carries a successful response."
        (eq? (coordination-result-status result) 'ok))

;;;
;;; Coordinator
;;;

(define-record-type <coordinator>
                    (%make-coordinator participants)
                    coordinator?
                    (participants coordinator-participants set-coordinator-participants!))

(define (make-coordinator)
        "Build an empty coordinator."
        (%make-coordinator '()))

(define (coordinator-find coord id)
        "The handler registered under ID (string or symbol), or #f."
        (let ((sid (if (string? id) id (format #f "~a" id))))
          (and=> (assoc sid (coordinator-participants coord)) cdr)))

(define (coordinator-participant-ids coord)
        "Participant ids in registration order."
        (map car (coordinator-participants coord)))

(define (coordinator-register! coord id handler)
        "Register HANDLER under ID.  Errors on duplicate id or non-procedure
handler.  Returns the participant id."
        (let ((sid (if (string? id) id (format #f "~a" id))))
          (unless (and (string? sid) (not (string-null? sid)))
            (error "coordinator-register!: id must be a non-empty string"))
          (unless (procedure? handler)
            (error "coordinator-register!: handler must be a procedure"))
          (when (coordinator-find coord sid)
            (error (format #f "coordinator-register!: duplicate participant: ~a" sid)))
          (set-coordinator-participants!
            coord (append (coordinator-participants coord) (list (cons sid handler))))
          sid))

(define (coordinator-unregister! coord id)
        "Remove the participant ID.  Errors when absent.  Returns the id."
        (let* ((sid (if (string? id) id (format #f "~a" id)))
               (handler (coordinator-find coord sid)))
          (unless handler
            (error (format #f "coordinator-unregister!: no such participant: ~a" sid)))
          (set-coordinator-participants!
            coord (filter (lambda (pair) (not (string=? (car pair) sid)))
                          (coordinator-participants coord)))
          sid))

;;;
;;; Primitives
;;;

(define (coordinator-send coord id message)
        "Send MESSAGE to participant ID; returns its handler's response.
A missing participant or a handler failure throws."
        (let* ((sid (if (string? id) id (format #f "~a" id)))
               (handler (coordinator-find coord sid)))
          (unless handler
            (error (format #f "coordinator-send: no such participant: ~a" sid)))
          (handler message)))

(define (coordinator-broadcast coord message)
        "Send MESSAGE to every participant in registration order.  Returns a
<coordination-result> per participant; handler failures are captured
as ('error . (KEY . IRRITANTS)) results instead of aborting the round."
        (map (lambda (pair)
               (let* ((id (car pair))
                      (handler (cdr pair))
                      (outcome (catch #t
                                 (lambda () (cons 'ok (handler message)))
                                 (lambda (key . args) (cons 'error (cons key args))))))
                 (%make-coordination-result id
                                            (car outcome)
                                            (cdr outcome))))
             (coordinator-participants coord)))

(define (coordinator-first-success coord message)
        "Failover: deliver MESSAGE to participants in registration order and
return the first successful response.  Throws 'coordination-failed
when every participant fails."
        (let loop ((participants (coordinator-participants coord))
                   (failures '()))
                  (cond
                    ((null? participants)
                      (throw 'coordination-failed
                             (reverse (map car failures))))
                    (else
                      (let* ((pair (car participants))
                             (id (car pair))
                             (handler (cdr pair))
                             (outcome (catch #t
                                        (lambda () (cons 'ok (handler message)))
                                        (lambda args (cons 'error args)))))
                        (if (eq? (car outcome) 'ok)
                          (cdr outcome)
                          (loop (cdr participants)
                                (acons id (cdr outcome) failures))))))))

(define (coordinator-pipeline coord ids message)
        "Chain MESSAGE through the handlers of IDS in order: each handler
receives the previous handler's output.  Returns the final output.
A handler failure throws 'coordination-pipeline-error with the stage
id, the original key, and the original irritants."
        (let loop ((remaining ids)
                   (current message))
                  (if (null? remaining)
                    current
                    (let* ((sid (if (string? (car remaining))
                                  (car remaining)
                                  (format #f "~a" (car remaining))))
                           (handler (coordinator-find coord sid)))
                      (cond
                        ((not handler)
                          (error (format #f "coordinator-pipeline: no such participant: ~a" sid)))
                        (else
                          (let* ((outcome (catch #t
                                            (lambda () (cons 'ok (handler current)))
                                            (lambda args (cons 'error args))))
                                 (args (cdr outcome)))
                            (if (eq? (car outcome) 'ok)
                              (loop (cdr remaining) args)
                              (throw 'coordination-pipeline-error
                                     sid (car args) (cdr args))))))))))
