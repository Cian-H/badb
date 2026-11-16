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
               #:use-module (ice-9 threads)
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
                          with-agent
                          current-harness-id
                          parent-harness-id
                          subagent-harness?
                          <subagent-link>
                          subagent-link?
                          subagent-link-child-id
                          subagent-link-parent-id
                          subagent-link-kind
                          subagent-link-status
                          subagent-link-label
                          subagent-link-active-tool
                          subagent-link-turn-count
                          subagent-link-last-thought
                          subagent-link-last-observation
                          subagent-link-started-at
                          subagent-link-updated-at
                          subagent-link-metadata
                          subagent-link->alist
                          register-subagent-link!
                          unregister-subagent-link!
                          update-subagent-link!
                          subagent-link-ref
                          harness-parent-id
                          subagent-children
                          all-subagent-links
                          clear-subagent-links!
                          call-with-harness-context))

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

;;;
;;; Parent / Child Harness Awareness & Link Registry
;;;

(define (%non-empty-env name)
        (let ((val (getenv name)))
          (and (string? val) (not (string-null? val)) val)))

(define (%normalize-harness-id id)
        (cond
          ((string? id) id)
          ((symbol? id) (symbol->string id))
          (else (format #f "~a" id))))

(define current-harness-id
        (make-parameter (or (%non-empty-env "BADB_HARNESS_ID") "root")))

(define parent-harness-id
        (make-parameter (%non-empty-env "BADB_PARENT_HARNESS_ID")))

(define-record-type <subagent-link>
                    (%make-subagent-link child-id parent-id kind status label
                                         active-tool turn-count last-thought
                                         last-observation started-at updated-at
                                         metadata)
                    subagent-link?
                    (child-id subagent-link-child-id)
                    (parent-id subagent-link-parent-id set-subagent-link-parent-id!)
                    (kind subagent-link-kind set-subagent-link-kind!)
                    (status subagent-link-status set-subagent-link-status!)
                    (label subagent-link-label set-subagent-link-label!)
                    (active-tool subagent-link-active-tool set-subagent-link-active-tool!)
                    (turn-count subagent-link-turn-count set-subagent-link-turn-count!)
                    (last-thought subagent-link-last-thought set-subagent-link-last-thought!)
                    (last-observation subagent-link-last-observation set-subagent-link-last-observation!)
                    (started-at subagent-link-started-at set-subagent-link-started-at!)
                    (updated-at subagent-link-updated-at set-subagent-link-updated-at!)
                    (metadata subagent-link-metadata set-subagent-link-metadata!))

(define (subagent-link->alist link)
        "Convert LINK into a serializable alist."
        `((child-id . ,(subagent-link-child-id link))
          (parent-id . ,(subagent-link-parent-id link))
          (kind . ,(subagent-link-kind link))
          (status . ,(subagent-link-status link))
          (label . ,(subagent-link-label link))
          (active-tool . ,(subagent-link-active-tool link))
          (turn-count . ,(subagent-link-turn-count link))
          (last-thought . ,(subagent-link-last-thought link))
          (last-observation . ,(subagent-link-last-observation link))
          (started-at . ,(subagent-link-started-at link))
          (updated-at . ,(subagent-link-updated-at link))
          (metadata . ,(subagent-link-metadata link))))

(define %harness-links-lock (make-mutex))
(define %harness-links '())

(define %unchanged (list 'unchanged))

(define* (%register-subagent-link* child-id parent-id
                                   #:key
                                   (kind 'subagent)
                                   (status 'created)
                                   (label #f)
                                   (active-tool #f)
                                   (turn-count 0)
                                   (last-thought #f)
                                   (last-observation #f)
                                   (metadata '()))
         (let ((cid (%normalize-harness-id child-id))
               (pid (if parent-id
                      (%normalize-harness-id parent-id)
                      (current-harness-id)))
               (now (%now-seconds)))
           (unless (and (string? cid) (not (string-null? cid)))
             (error "register-subagent-link!: child-id must be a non-empty string"))
           (when (string=? cid pid)
             (error "register-subagent-link!: child-id cannot equal parent-id" cid))
           (with-mutex %harness-links-lock
                       (let ((existing (and=> (assoc cid %harness-links) cdr)))
                         (if existing
                           (begin
                             (set-subagent-link-parent-id! existing pid)
                             (set-subagent-link-kind! existing kind)
                             (set-subagent-link-status! existing status)
                             (when label (set-subagent-link-label! existing label))
                             (set-subagent-link-active-tool! existing active-tool)
                             (set-subagent-link-turn-count! existing turn-count)
                             (when last-thought (set-subagent-link-last-thought! existing last-thought))
                             (when last-observation
                               (set-subagent-link-last-observation! existing last-observation))
                             (unless (null? metadata) (set-subagent-link-metadata! existing metadata))
                             (set-subagent-link-updated-at! existing now)
                             existing)
                           (let ((link (%make-subagent-link cid pid kind status label
                                                            active-tool turn-count last-thought
                                                            last-observation now now metadata)))
                             (set! %harness-links (append %harness-links (list (cons cid link))))
                             link))))))

(define (register-subagent-link! child-id . rest)
        "Register a parent/child harness link for CHILD-ID under PARENT-ID.
Optional positional PARENT-ID defaults to (current-harness-id); keyword options
#:kind, #:status, #:label, #:active-tool, #:turn-count, #:last-thought,
#:last-observation, and #:metadata initialize the <subagent-link>."
        (if (and (pair? rest) (not (keyword? (car rest))))
          (apply %register-subagent-link* child-id (car rest) (cdr rest))
          (apply %register-subagent-link* child-id #f rest)))

(define (unregister-subagent-link! child-id)
        "Remove the link record for CHILD-ID.  Returns #t if removed, #f otherwise."
        (let ((cid (%normalize-harness-id child-id)))
          (with-mutex %harness-links-lock
                      (let ((found? (and (assoc cid %harness-links) #t)))
                        (when found?
                          (set! %harness-links
                                (filter (lambda (pair) (not (string=? (car pair) cid)))
                                        %harness-links)))
                        found?))))

(define* (update-subagent-link! child-id
                                #:key
                                (status %unchanged)
                                (active-tool %unchanged)
                                (turn-count %unchanged)
                                (increment-turn? #f)
                                (last-thought %unchanged)
                                (last-observation %unchanged)
                                (label %unchanged)
                                (metadata %unchanged))
         "Update fields on an existing <subagent-link> for CHILD-ID.  Returns the
updated <subagent-link>, or #f when CHILD-ID is not registered."
         (let ((cid (%normalize-harness-id child-id)))
           (with-mutex %harness-links-lock
                       (let ((link (and=> (assoc cid %harness-links) cdr)))
                         (when link
                           (unless (eq? status %unchanged)
                             (set-subagent-link-status! link status))
                           (unless (eq? active-tool %unchanged)
                             (set-subagent-link-active-tool! link active-tool))
                           (cond
                             ((not (eq? turn-count %unchanged))
                               (set-subagent-link-turn-count! link turn-count))
                             (increment-turn?
                               (set-subagent-link-turn-count!
                                 link (+ 1 (subagent-link-turn-count link)))))
                           (unless (eq? last-thought %unchanged)
                             (set-subagent-link-last-thought! link last-thought))
                           (unless (eq? last-observation %unchanged)
                             (set-subagent-link-last-observation! link last-observation))
                           (unless (eq? label %unchanged)
                             (set-subagent-link-label! link label))
                           (unless (eq? metadata %unchanged)
                             (set-subagent-link-metadata! link metadata))
                           (set-subagent-link-updated-at! link (%now-seconds)))
                         link))))

(define (subagent-link-ref child-id)
        "Return the <subagent-link> for CHILD-ID, or #f."
        (let ((cid (%normalize-harness-id child-id)))
          (with-mutex %harness-links-lock
                      (and=> (assoc cid %harness-links) cdr))))

(define* (harness-parent-id #:optional (id (current-harness-id)))
         "Return the parent harness id for ID, or #f when ID is a root harness."
         (let* ((hid (%normalize-harness-id id))
                (link (subagent-link-ref hid)))
           (cond
             (link (subagent-link-parent-id link))
             ((string=? hid (current-harness-id)) (parent-harness-id))
             (else #f))))

(define* (subagent-harness? #:optional (id #f))
         "True when ID (or the current harness) is running as a child subagent."
         (if id
           (let ((hid (%normalize-harness-id id)))
             (or (and (subagent-link-ref hid) #t)
                 (and (string=? hid (current-harness-id))
                      (and (parent-harness-id) #t))))
           (or (and (parent-harness-id) #t)
               (and (subagent-link-ref (current-harness-id)) #t))))

(define* (subagent-children #:optional (parent-id (current-harness-id)))
         "Return a list of <subagent-link> records whose parent-id matches PARENT-ID."
         (let ((pid (%normalize-harness-id parent-id)))
           (with-mutex %harness-links-lock
                       (filter-map (lambda (pair)
                                     (let ((link (cdr pair)))
                                       (and (equal? (subagent-link-parent-id link) pid)
                                            link)))
                                   %harness-links))))

(define (all-subagent-links)
        "Return a list of all registered <subagent-link> records in registration order."
        (with-mutex %harness-links-lock
                    (map cdr %harness-links)))

(define (clear-subagent-links!)
        "Remove all registered subagent links.  Primarily for isolated tests."
        (with-mutex %harness-links-lock
                    (set! %harness-links '()))
        #t)

(define* (call-with-harness-context child-id proc
                                    #:key
                                    (parent-id (current-harness-id))
                                    (kind 'subagent)
                                    (label #f)
                                    (metadata '()))
         "Register CHILD-ID under PARENT-ID, parameterize current/parent harness ids,
and run PROC while keeping link status updated."
         (let ((pid (or parent-id (current-harness-id))))
           (register-subagent-link! child-id pid
                                    #:kind kind
                                    #:status 'running
                                    #:label label
                                    #:metadata metadata)
           (catch #t
             (lambda ()
               (let ((res (parameterize ((current-harness-id child-id)
                                         (parent-harness-id pid))
                                        (proc))))
                 (update-subagent-link! child-id #:status 'completed #:active-tool #f)
                 res))
             (lambda (key . args)
               (update-subagent-link! child-id #:status 'failed #:active-tool #f)
               (apply throw key args)))))
