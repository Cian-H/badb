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
               #:use-module (geasa agents-lifecycle plugin)
               #:use-module (ice-9 format)
               #:use-module (ice-9 optargs)
               #:use-module (ice-9 threads)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:use-module (srfi srfi-13)
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
                          coordinator-pipeline
                          <harness-event>
                          harness-event?
                          harness-event-seq
                          harness-event-timestamp
                          harness-event-source-id
                          harness-event-target-id
                          harness-event-kind
                          harness-event-payload
                          harness-event->alist
                          <harness-event-bus>
                          make-harness-event-bus
                          harness-event-bus?
                          default-harness-event-bus
                          harness-event-subscribe!
                          harness-event-unsubscribe!
                          harness-event-publish!
                          harness-event-bubble!
                          harness-event-poll
                          harness-event-bus-clear!
                          subagent-hierarchy-tree
                          format-subagent-hierarchy
                          format-subagent-overview))

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

;;;
;;; Harness-to-Harness Event Bus (Ring Buffer + Subscribers)
;;;
(define-record-type <harness-event>
                    (%make-harness-event seq timestamp source-id target-id kind payload)
                    harness-event?
                    (seq harness-event-seq)
                    (timestamp harness-event-timestamp)
                    (source-id harness-event-source-id)
                    (target-id harness-event-target-id)
                    (kind harness-event-kind)
                    (payload harness-event-payload))

(define (harness-event->alist ev)
        "Convert EV into a JSON/protocol-serializable alist."
        `((seq . ,(harness-event-seq ev))
          (timestamp . ,(harness-event-timestamp ev))
          (source-id . ,(harness-event-source-id ev))
          (target-id . ,(harness-event-target-id ev))
          (kind . ,(harness-event-kind ev))
          (payload . ,(harness-event-payload ev))))

(define-record-type <harness-event-bus>
                    (%make-harness-event-bus capacity events next-seq subscribers next-sub-id lock)
                    harness-event-bus?
                    (capacity harness-event-bus-capacity)
                    (events harness-event-bus-events set-harness-event-bus-events!)
                    (next-seq harness-event-bus-next-seq set-harness-event-bus-next-seq!)
                    (subscribers harness-event-bus-subscribers set-harness-event-bus-subscribers!)
                    (next-sub-id harness-event-bus-next-sub-id set-harness-event-bus-next-sub-id!)
                    (lock harness-event-bus-lock))

(define* (make-harness-event-bus #:key (capacity 256))
         "Build an isolated harness-to-harness event bus with ring buffer CAPACITY."
         (unless (and (integer? capacity) (> capacity 0))
           (error "make-harness-event-bus: capacity must be a positive integer" capacity))
         (%make-harness-event-bus capacity '() 1 '() 1 (make-mutex)))

(define %default-harness-event-bus (make-harness-event-bus #:capacity 512))

(define (default-harness-event-bus)
        "Return the shared process-wide harness-to-harness event bus."
        %default-harness-event-bus)

(define (%normalize-id id)
        (cond
          ((not id) #f)
          ((string? id) id)
          ((symbol? id) (symbol->string id))
          (else (format #f "~a" id))))

(define (%harness-ancestor? candidate-ancestor node-id)
        "True when CANDIDATE-ANCESTOR is NODE-ID's parent or higher ancestor."
        (let loop ((cur (%normalize-id node-id))
                   (visited '()))
                  (if (or (not cur) (member cur visited))
                    #f
                    (let ((p (harness-parent-id cur)))
                      (cond
                        ((not p) #f)
                        ((string=? p candidate-ancestor) #t)
                        (else (loop p (cons cur visited))))))))

(define
  (%event-matches-target? ev target-id include-descendants?)
  (or (not target-id)
      (let
          ((ev-target (harness-event-target-id ev))
           (ev-source (harness-event-source-id ev)))
        (or (equal? ev-target target-id)
            (and include-descendants?
                 (or (equal? ev-source target-id) (%harness-ancestor? target-id ev-source)))))))

(define* (harness-event-subscribe! handler
                                   #:key
                                   (bus (default-harness-event-bus))
                                   (target-id #f)
                                   (include-descendants? #t))
         "Subscribe HANDLER (1-arg proc receiving <harness-event>) to BUS.
When TARGET-ID is given, receives events targeting TARGET-ID (and optionally
its descendant subagents when INCLUDE-DESCENDANTS? is true).  Returns sub-id."
         (unless (procedure? handler)
           (error "harness-event-subscribe!: handler must be a procedure"))
         (let ((tid (%normalize-id target-id)))
           (with-mutex (harness-event-bus-lock bus)
                       (let ((sub-id (harness-event-bus-next-sub-id bus)))
                         (set-harness-event-bus-next-sub-id! bus (+ sub-id 1))
                         (set-harness-event-bus-subscribers!
                           bus
                           (append (harness-event-bus-subscribers bus)
                                   (list (list sub-id tid include-descendants? handler))))
                         sub-id))))

(define* (harness-event-unsubscribe! sub-id #:key (bus (default-harness-event-bus)))
         "Remove subscription SUB-ID from BUS.  Returns #t if removed."
         (with-mutex (harness-event-bus-lock bus)
                     (let* ((subs (harness-event-bus-subscribers bus))
                            (found? (and (assoc sub-id subs) #t)))
                       (when found?
                         (set-harness-event-bus-subscribers!
                           bus
                           (filter (lambda (entry) (not (equal? (car entry) sub-id))) subs)))
                       found?)))

(define (%apply-event-to-link! source-id kind payload)
        (when (and source-id (subagent-link-ref source-id))
          (cond
            ((eq? kind 'iteration)
              (let ((turn (and (pair? payload) (assoc-ref payload 'iteration))))
                (if (integer? turn)
                  (update-subagent-link! source-id #:turn-count turn #:status 'running)
                  (update-subagent-link! source-id #:increment-turn? #t #:status 'running))))
            ((eq? kind 'thought)
              (let ((text (cond
                            ((string? payload) payload)
                            ((pair? payload) (or (assoc-ref payload 'text) ""))
                            (else #f)))
                    (turn (and (pair? payload) (assoc-ref payload 'iteration))))
                (if (integer? turn)
                  (update-subagent-link! source-id #:last-thought text #:turn-count turn)
                  (update-subagent-link! source-id #:last-thought text))))
            ((eq? kind 'tool-call)
              (let ((tool (and (pair? payload)
                               (or (assoc-ref payload 'tool)
                                   (assoc-ref payload 'name))))
                    (turn (and (pair? payload) (assoc-ref payload 'iteration))))
                (if (integer? turn)
                  (update-subagent-link! source-id #:active-tool tool #:status 'running #:turn-count turn)
                  (update-subagent-link! source-id #:active-tool tool #:status 'running))))
            ((or (eq? kind 'tool-result) (eq? kind 'observation))
              (let ((obs (cond
                           ((string? payload) payload)
                           ((pair? payload)
                             (or (assoc-ref payload 'output)
                                 (assoc-ref payload 'summary)
                                 (assoc-ref payload 'tool)))
                           (else #f))))
                (update-subagent-link! source-id #:active-tool #f #:last-observation obs)))
            ((eq? kind 'status)
              (let ((st (cond
                          ((symbol? payload) payload)
                          ((pair? payload) (assoc-ref payload 'status))
                          (else #f))))
                (when st
                  (if (memq st '(completed failed stopped cancelled))
                    (update-subagent-link! source-id #:status st #:active-tool #f)
                    (update-subagent-link! source-id #:status st))))))))

(define* (harness-event-publish! source-id kind payload
                                 #:key
                                 (target-id #f)
                                 (bus (default-harness-event-bus)))
         "Publish a structured <harness-event> from SOURCE-ID with KIND and PAYLOAD
onto BUS, updating any registered <subagent-link> state and notifying matching
subscribers.  If TARGET-ID is #f, resolves the parent harness of SOURCE-ID."
         (let* ((src (%normalize-id source-id))
                (tgt (or (%normalize-id target-id)
                         (and src (harness-parent-id src))))
                (ts (car (gettimeofday)))
                (ev #f)
                (matching-handlers '()))
           (%apply-event-to-link! src kind payload)
           (with-mutex (harness-event-bus-lock bus)
                       (let* ((seq (harness-event-bus-next-seq bus))
                              (cap (harness-event-bus-capacity bus))
                              (new-ev (%make-harness-event seq ts src tgt kind payload))
                              (updated (append (harness-event-bus-events bus) (list new-ev)))
                              (trimmed (if (> (length updated) cap)
                                         (drop updated (- (length updated) cap))
                                         updated)))
                         (set! ev new-ev)
                         (set-harness-event-bus-next-seq! bus (+ seq 1))
                         (set-harness-event-bus-events! bus trimmed)
                         (set! matching-handlers
                               (filter-map (lambda (entry)
                                             (let ((sub-target (cadr entry))
                                                   (inc-desc? (caddr entry))
                                                   (proc (cadddr entry)))
                                               (and (%event-matches-target? new-ev sub-target inc-desc?)
                                                    proc)))
                                           (harness-event-bus-subscribers bus)))))
           (for-each (lambda (proc)
                       (catch #t (lambda () (proc ev)) (lambda _ #f)))
                     matching-handlers)
           ev))

(define* (harness-event-bubble! kind payload
                                #:key
                                (source-id (current-harness-id))
                                (target-id #f)
                                (bus (default-harness-event-bus)))
         "Bubble an event of KIND with PAYLOAD from SOURCE-ID up to its parent
harness (or TARGET-ID when specified)."
         (let* ((src (%normalize-id source-id))
                (tgt (or (%normalize-id target-id)
                         (and src (harness-parent-id src)))))
           (harness-event-publish! src kind payload #:target-id tgt #:bus bus)))

(define* (harness-event-poll #:key
                             (bus (default-harness-event-bus))
                             (target-id #f)
                             (source-id #f)
                             (kind #f)
                             (since-seq 0)
                             (include-descendants? #t)
                             (limit #f))
         "Return matching <harness-event> records from BUS with seq > SINCE-SEQ,
ordered oldest-first."
         (let* ((tid (%normalize-id target-id))
                (sid (%normalize-id source-id))
                (snapshot (with-mutex (harness-event-bus-lock bus)
                                      (harness-event-bus-events bus)))
                (filtered
                  (filter (lambda (ev)
                            (and (> (harness-event-seq ev) since-seq)
                                 (or (not sid) (equal? (harness-event-source-id ev) sid))
                                 (or (not kind) (eq? (harness-event-kind ev) kind))
                                 (%event-matches-target? ev tid include-descendants?)))
                          snapshot)))
           (if (and (integer? limit) (>= limit 0) (> (length filtered) limit))
             (take-right filtered limit)
             filtered)))

(define* (harness-event-bus-clear! #:key
                                   (bus (default-harness-event-bus))
                                   (clear-subscribers? #f))
         "Clear buffered events (and optionally subscribers) on BUS."
         (with-mutex (harness-event-bus-lock bus)
                     (set-harness-event-bus-events! bus '())
                     (when clear-subscribers?
                       (set-harness-event-bus-subscribers! bus '())))
         #t)

;;;
;;; Subagent Hierarchy Tree & Overview Formatters
;;;
(define (%tree-node-alist? x)
        (and (list? x)
             (pair? (assq 'id x))
             (pair? (assq 'children x))))

(define (%parse-tree-call-args args)
        "Parse optional positional tree/root-id followed by keyword arguments."
        (if (and (pair? args) (not (keyword? (car args))))
          (values (car args) (cdr args))
          (values #f args)))

(define* (%build-hierarchy-node id parent-id link include-stopped? max-depth depth visited)
         (let* ((raw-children (if (>= depth max-depth)
                                '()
                                (subagent-children id)))
                (filtered-children
                  (filter (lambda (child-link)
                            (let ((cid (subagent-link-child-id child-link))
                                  (st (subagent-link-status child-link)))
                              (and (not (member cid visited))
                                   (or include-stopped?
                                       (memq st '(created running))))))
                          raw-children))
                (child-nodes
                  (map (lambda (child-link)
                         (let ((cid (subagent-link-child-id child-link)))
                           (%build-hierarchy-node cid
                                                  id
                                                  child-link
                                                  include-stopped?
                                                  max-depth
                                                  (+ depth 1)
                                                  (cons cid visited))))
                       filtered-children)))
           `((id . ,id)
             (parent-id . ,(if link (subagent-link-parent-id link) parent-id))
             (kind . ,(if link (subagent-link-kind link) 'root))
             (status . ,(if link (subagent-link-status link) 'running))
             (label . ,(and link (subagent-link-label link)))
             (active-tool . ,(and link (subagent-link-active-tool link)))
             (turn-count . ,(if link (subagent-link-turn-count link) 0))
             (last-thought . ,(and link (subagent-link-last-thought link)))
             (last-observation . ,(and link (subagent-link-last-observation link)))
             (started-at . ,(and link (subagent-link-started-at link)))
             (updated-at . ,(and link (subagent-link-updated-at link)))
             (metadata . ,(if link (subagent-link-metadata link) '()))
             (children . ,child-nodes))))

(define* (%subagent-hierarchy-tree* #:key
                                    (root-id (current-harness-id))
                                    (include-stopped? #t)
                                    (max-depth 16))
         (let* ((rid (%normalize-id (or root-id (current-harness-id))))
                (link (subagent-link-ref rid))
                (pid (if link (subagent-link-parent-id link) (harness-parent-id rid))))
           (%build-hierarchy-node rid pid link include-stopped? max-depth 0 (list rid))))

(define (subagent-hierarchy-tree . args)
        "Build a nested serializable hierarchy tree alist rooted at ROOT-ID
(default (current-harness-id)).  Supports optional positional ROOT-ID and
keyword options #:root-id, #:include-stopped?, and #:max-depth."
        (call-with-values
          (lambda () (%parse-tree-call-args args))
          (lambda (pos kw)
            (if pos
              (apply %subagent-hierarchy-tree* #:root-id pos kw)
              (apply %subagent-hierarchy-tree* kw)))))

(define (%truncate-line line max-width)
        (if (and (integer? max-width) (> max-width 3) (> (string-length line) max-width))
          (string-append (substring line 0 (- max-width 3)) "...")
          line))

(define
  (%format-node-summary node show-details? ascii?)
  (let*
      ((id (or (assoc-ref node 'id) "?"))
       (status (or (assoc-ref node 'status) 'unknown))
       (tool (assoc-ref node 'active-tool))
       (turns (or (assoc-ref node 'turn-count) 0))
       (label (assoc-ref node 'label)))
    (if show-details?
      (let
          ((parts
             (filter identity
                     (list (format #f "~a [~a]" id status)
                           (and (string? tool) (not (string-null? tool)) (format #f "tool:~a" tool))
                           (format #f "turns:~a" turns)
                           (and (string? label)
                                (not (string-null? label))
                                (format #f
                                        "~a ~a"
                                        (if ascii?
                                          "--"
                                          "—")
                                        label))))))
        (string-join parts " "))
      (format #f "~a [~a]" id status))))

(define (%render-hierarchy-lines node prefix is-root? is-last? show-details? ascii? max-width)
        (let* ((conn-branch (if ascii? "|-- " "├── "))
               (conn-last (if ascii? "`-- " "└── "))
               (pad-pipe (if ascii? "|   " "│   "))
               (pad-blank "    ")
               (connector (cond
                            (is-root? "")
                            (is-last? conn-last)
                            (else conn-branch)))
               (line (%truncate-line
                       (string-append prefix connector
                                      (%format-node-summary node show-details? ascii?))
                       max-width))
               (child-prefix (if is-root?
                               ""
                               (string-append prefix (if is-last? pad-blank pad-pipe))))
               (children (or (assoc-ref node 'children) '()))
               (n (length children)))
          (cons line
                (let loop ((rest children) (idx 0) (acc '()))
                          (if (null? rest)
                            acc
                            (let ((sub-lines (%render-hierarchy-lines
                                               (car rest)
                                               child-prefix
                                               #f
                                               (= idx (- n 1))
                                               show-details?
                                               ascii?
                                               max-width)))
                              (loop (cdr rest) (+ idx 1) (append acc sub-lines))))))))

(define* (%format-subagent-hierarchy* #:key
                                      (tree #f)
                                      (root-id (current-harness-id))
                                      (include-stopped? #t)
                                      (ascii? #f)
                                      (show-details? #t)
                                      (max-width #f))
         (let ((resolved-tree (or tree
                                  (subagent-hierarchy-tree #:root-id root-id
                                                           #:include-stopped? include-stopped?))))
           (string-join
             (%render-hierarchy-lines resolved-tree "" #t #t show-details? ascii? max-width)
             "\n")))

(define (format-subagent-hierarchy . args)
        "Render a subagent hierarchy tree with box-drawing tree connectors,
status, active tool, turn count, and task label.  Accepts an optional
positional TREE alist or ROOT-ID, plus #:tree, #:root-id, #:include-stopped?,
#:ascii?, #:show-details?, and #:max-width."
        (call-with-values
          (lambda () (%parse-tree-call-args args))
          (lambda (pos kw)
            (cond
              ((%tree-node-alist? pos)
                (apply %format-subagent-hierarchy* #:tree pos kw))
              (pos
                (apply %format-subagent-hierarchy* #:root-id pos kw))
              (else
                (apply %format-subagent-hierarchy* kw))))))

(define (%collect-descendants tree)
        (let ((children (or (assoc-ref tree 'children) '())))
          (append children (append-map %collect-descendants children))))

(define (%format-event-summary ev)
        (let* ((seq (harness-event-seq ev))
               (src (or (harness-event-source-id ev) "?"))
               (kind (harness-event-kind ev))
               (payload (harness-event-payload ev))
               (detail
                 (cond
                   ((string? payload) payload)
                   ((pair? payload)
                     (or (and (assoc-ref payload 'tool)
                              (if (assoc-ref payload 'summary)
                                (format #f "~a -> ~a"
                                        (assoc-ref payload 'tool)
                                        (assoc-ref payload 'summary))
                                (format #f "~a" (assoc-ref payload 'tool))))
                         (assoc-ref payload 'text)
                         (assoc-ref payload 'thought)
                         (and (assoc-ref payload 'status)
                              (format #f "~a" (assoc-ref payload 'status)))
                         (and (assoc-ref payload 'iteration)
                              (format #f "turn ~a (~a)"
                                      (assoc-ref payload 'iteration)
                                      (or (assoc-ref payload 'phase) 'step)))
                         (format #f "~s" payload)))
                   (else (format #f "~a" payload))))
               (trimmed (if (and (string? detail) (> (string-length detail) 72))
                          (string-append (substring detail 0 69) "...")
                          (or detail ""))))
          (format #f "  #~a [~a] ~a: ~a" seq src kind trimmed)))

(define* (%format-subagent-overview* #:key
                                     (tree #f)
                                     (root-id (current-harness-id))
                                     (include-stopped? #t)
                                     (bus (default-harness-event-bus))
                                     (recent-events 5)
                                     (ascii? #f)
                                     (max-width #f))
         (let* ((resolved-tree
                  (or tree
                      (subagent-hierarchy-tree #:root-id root-id
                                               #:include-stopped? include-stopped?)))
                (rid (or (assoc-ref resolved-tree 'id) "root"))
                (descendants (%collect-descendants resolved-tree))
                (total (length descendants))
                (count-status (lambda (s)
                                (count (lambda (n) (eq? (assoc-ref n 'status) s))
                                       descendants)))
                (n-running (count-status 'running))
                (n-completed (count-status 'completed))
                (n-failed (count-status 'failed))
                (n-stopped (count-status 'stopped))
                (n-cancelled (count-status 'cancelled))
                (dash (if ascii? "--" "—"))
                (header
                  (if (zero? total)
                    (format #f "Subagents (root: ~a) ~a none active" rid dash)
                    (string-append
                      (format #f "Subagents (root: ~a) ~a total:~a running:~a completed:~a failed:~a stopped:~a"
                              rid dash total n-running n-completed n-failed n-stopped)
                      (if (> n-cancelled 0)
                        (format #f " cancelled:~a" n-cancelled)
                        ""))))
                (tree-str (if (zero? total)
                            #f
                            (format-subagent-hierarchy resolved-tree
                                                       #:ascii? ascii?
                                                       #:max-width max-width)))
                (ev-list (if (and bus (integer? recent-events) (> recent-events 0))
                           (harness-event-poll #:bus bus
                                               #:target-id rid
                                               #:limit recent-events)
                           '()))
                (ev-section (if (null? ev-list)
                              #f
                              (string-join
                                (cons "Recent events:"
                                      (map (lambda (ev)
                                             (%truncate-line (%format-event-summary ev) max-width))
                                           ev-list))
                                "\n"))))
           (string-join (filter identity (list header tree-str ev-section)) "\n")))

(define (format-subagent-overview . args)
        "Render a compact overview of active and recent subagents including status
counts, hierarchy tree, active tools, turn counts, and recent bubbled events.
Accepts optional positional TREE or ROOT-ID, plus #:tree, #:root-id,
#:include-stopped?, #:bus, #:recent-events, #:ascii?, and #:max-width."
        (call-with-values
          (lambda () (%parse-tree-call-args args))
          (lambda (pos kw)
            (cond
              ((%tree-node-alist? pos)
                (apply %format-subagent-overview* #:tree pos kw))
              (pos
                (apply %format-subagent-overview* #:root-id pos kw))
              (else
                (apply %format-subagent-overview* kw))))))
