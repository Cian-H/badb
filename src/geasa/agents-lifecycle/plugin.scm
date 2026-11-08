;;; badb — Agent Lifecycle
;;;
;;; Start/stop lifecycle management for agents.  An agent is a record
;;; pairing an identifier with a START thunk (its entry point) and an
;;; optional STOP thunk (its cleanup); the manager owns the registry and
;;; drives the state machine
;;;
;;;     created --start!--> running --stop!--> stopped
;;;        \                    |
;;;         \--> stopped        +--> failed (start or stop error)
;;;
;;; Failures are recorded on the agent (error key and irritants, plus
;;; timestamps) and rethrown to the caller; stopping is idempotent, and
;;; WITH-AGENT guarantees cleanup on any exit path.

(define-module (geasa agents-lifecycle plugin)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<agent>
            make-agent
            agent?
            agent-id
            agent-description
            agent-start-thunk
            agent-stop-thunk
            agent-state
            agent-started-at
            agent-stopped-at
            agent-result
            agent-error
            <agent-manager>
            make-agent-manager
            agent-manager?
            agent-manager-agents
            agent-ref
            agent-register!
            agent-start!
            agent-stop!
            agent-running?
            agent-manager-ids
            agent-manager-running-ids
            agent-manager-stop-all!
            with-agent))

;;;
;;; Agent record
;;;

(define-record-type <agent>
  (%make-agent id description start-thunk stop-thunk
              state started-at stopped-at result error)
  agent?
  (id agent-id)
  (description agent-description)
  (start-thunk agent-start-thunk)
  (stop-thunk agent-stop-thunk)
  (state agent-state set-agent-state!)
  (started-at agent-started-at set-agent-started-at!)
  (stopped-at agent-stopped-at set-agent-stopped-at!)
  (result agent-result set-agent-result!)
  (error agent-error set-agent-error!))

(define (%now-seconds)
  (car (gettimeofday)))

(define* (make-agent id #:key
                     (description "")
                     (start (lambda () #f))
                     (stop (lambda () #f)))
  "Build an agent in state 'created.  ID is a non-empty string; START
and STOP are zero-argument thunks."
  (unless (and (string? id) (not (string-null? id)))
    (error "make-agent: id must be a non-empty string"))
  (unless (string? description)
    (error "make-agent: description must be a string"))
  (unless (procedure? start)
    (error "make-agent: start must be a procedure"))
  (unless (procedure? stop)
    (error "make-agent: stop must be a procedure"))
  (%make-agent id description start stop 'created #f #f #f #f))

;;;
;;; Manager
;;;

(define-record-type <agent-manager>
  (%make-agent-manager agents)
  agent-manager?
  (agents agent-manager-agents set-agent-manager-agents!))

(define (make-agent-manager)
  "Build an empty agent manager."
  (%make-agent-manager '()))

(define (agent-ref manager id)
  "The <agent> registered under ID (string or symbol), or #f."
  (let ((sid (if (string? id) id (format #f "~a" id))))
    (and=> (assoc sid (agent-manager-agents manager)) cdr)))

(define (agent-register! manager agent)
  "Register AGENT with MANAGER.  Errors on duplicate id.  Returns AGENT."
  (unless (and (agent-manager? manager) (agent? agent))
    (error "agent-register!: expects an agent-manager and an agent"))
  (when (agent-ref manager (agent-id agent))
    (error (format #f "agent-register!: duplicate agent id: ~a"
                   (agent-id agent))))
  (set-agent-manager-agents!
   manager (append (agent-manager-agents manager)
                   (list (cons (agent-id agent) agent))))
  agent)

(define (agent-lookup manager id who)
  (or (agent-ref manager id)
      (error (format #f "~a: no such agent: ~a" who id))))

(define (agent-start! manager id)
  "Start the agent ID: invoke its start thunk, record the result and
started-at timestamp, and transition to 'running.  A start failure
marks the agent 'failed, records the error, and rethrows.  Returns the
agent."
  (let* ((agent (agent-lookup manager id "agent-start!"))
         (state (agent-state agent)))
    (when (eq? state 'running)
      (error (format #f "agent-start!: agent ~a is already running"
                     (agent-id agent))))
    (catch #t
      (lambda ()
        (let ((value ((agent-start-thunk agent))))
          (set-agent-result! agent value)
          (set-agent-error! agent #f)
          (set-agent-started-at! agent (%now-seconds))
          (set-agent-stopped-at! agent #f)
          (set-agent-state! agent 'running)
          agent))
      (lambda (key . args)
        (set-agent-error! agent (cons key args))
        (set-agent-state! agent 'failed)
        (apply throw key args)))))

(define (agent-stop! manager id)
  "Stop the agent ID: invoke its stop thunk (only when the agent was
running), record the stopped-at timestamp, and transition to 'stopped.
Idempotent for agents already stopped; a stop failure marks the agent
'failed and rethrows.  Returns the agent."
  (let* ((agent (agent-lookup manager id "agent-stop!"))
         (state (agent-state agent)))
    (cond
     ((memq state '(stopped created))
      (set-agent-state! agent 'stopped)
      (unless (agent-stopped-at agent)
        (set-agent-stopped-at! agent (%now-seconds)))
      agent)
     ((eq? state 'failed)
      (set-agent-state! agent 'stopped)
      (set-agent-stopped-at! agent (%now-seconds))
      agent)
     (else
      (catch #t
        (lambda ()
          ((agent-stop-thunk agent))
          (set-agent-stopped-at! agent (%now-seconds))
          (set-agent-state! agent 'stopped)
          agent)
        (lambda (key . args)
          (set-agent-error! agent (cons key args))
          (set-agent-state! agent 'failed)
          (apply throw key args)))))))

(define (agent-running? manager id)
  "True when the agent ID is currently running."
  (and=> (agent-ref manager id)
         (lambda (agent) (eq? (agent-state agent) 'running))))

(define (agent-manager-ids manager)
  "Ids of all registered agents, registration order."
  (map car (agent-manager-agents manager)))

(define (agent-manager-running-ids manager)
  "Ids of currently running agents, registration order."
  (filter (lambda (id) (agent-running? manager id))
          (agent-manager-ids manager)))

(define (agent-manager-stop-all! manager)
  "Stop every running agent (reverse registration order).  Returns the
ids that were stopped.  The first stop failure rethrows after every
agent has been attempted."
  (let ((to-stop (reverse (agent-manager-running-ids manager))))
    (let loop ((remaining to-stop)
               (stopped '())
               (failure #f))
      (if (null? remaining)
          (begin
            (when failure (apply throw (car failure) (cdr failure)))
            (reverse stopped))
          (let* ((id (car remaining))
                 (res (catch #t
                        (lambda ()
                          (agent-stop! manager id)
                          #f)
                        (lambda args args))))
            (loop (cdr remaining)
                  (if res stopped (cons id stopped))
                  (or failure res)))))))

(define* (with-agent manager agent thunk)
  "Register AGENT, start it, run THUNK, and stop it on any exit path.
Returns THUNK's value."
  (agent-register! manager agent)
  (dynamic-wind
    (lambda () (agent-start! manager (agent-id agent)))
    thunk
    (lambda ()
      (when (agent-running? manager (agent-id agent))
        (agent-stop! manager (agent-id agent))))))
