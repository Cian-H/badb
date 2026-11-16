;;; (geasa surfaces-tui protocol) — UI Server / Client Wire Protocol
;;;
;;; Separates harness UI state/event server from terminal rendering client
;;; using serializable JSON-line event and frame messages. Enables TUI over
;;; SSH/pipes and cleanly routes subagent ReAct progress events.

(define-module (geasa surfaces-tui protocol)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa surfaces-tui widgets)
               #:use-module (ice-9 format)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 threads)
               #:use-module (json)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:use-module (srfi srfi-43)
               #:export (%ui-protocol-version
                          %ui-client-message-types
                          %ui-server-message-types
                          <ui-message>
                          make-ui-message
                          ui-message?
                          ui-message-version
                          ui-message-type
                          ui-message-seq
                          ui-message-source
                          ui-message-payload
                          ui-message-timestamp
                          ui-message->alist
                          alist->ui-message
                          encode-ui-message
                          decode-ui-message
                          write-ui-frame!
                          read-ui-frame
                          draw-op->alist
                          alist->draw-op
                          <ui-server>
                          make-ui-server
                          ui-server?
                          ui-server-state
                          ui-server-seq
                          ui-server-clients
                          ui-server-pending-frames
                          ui-server-connect!
                          ui-server-disconnect!
                          ui-server-on-event!
                          ui-server-emit!
                          ui-server-handle-event!
                          ui-server-drain-frames!
                          ui-server-route-subagent-event!
                          <ui-client>
                          make-ui-client
                          ui-client?
                          ui-client-id
                          ui-client-cols
                          ui-client-rows
                          ui-client-last-frame
                          ui-client-status
                          ui-client-subagent-events
                          ui-client-notifications
                          ui-client-connected?
                          ui-client-attach-port!
                          ui-client-send-event
                          ui-client-apply-frame!
                          ui-client-render-snapshot
                          pump-ui-pair!))

;;;
;;; Protocol Constants & Message Envelope
;;;
(define %ui-protocol-version 1)

(define %ui-client-message-types
        '(key mouse resize command submit steer attach detach ping))

(define %ui-server-message-types
        '(frame transcript-append transcript-update status subagent-event
          menu-state selector-state notify pong goodbye error))

(define-record-type <ui-message>
                    (%make-ui-message version type seq source payload timestamp)
                    ui-message?
                    (version ui-message-version)
                    (type ui-message-type)
                    (seq ui-message-seq)
                    (source ui-message-source)
                    (payload ui-message-payload)
                    (timestamp ui-message-timestamp))

(define* (make-ui-message type
                          #:key (version %ui-protocol-version)
                          (seq 0)
                          (source 'local)
                          (payload '())
                          (timestamp (current-time)))
         "Construct an immutable <ui-message> protocol envelope."
         (%make-ui-message version
                           (ensure-symbol type)
                           seq
                           (ensure-symbol source)
                           (or payload '())
                           timestamp))

(define (draw-op->alist op)
        "Convert a <draw-op> record into a serializable alist."
        `((x . ,(draw-op-x op))
          (y . ,(draw-op-y op))
          (text . ,(draw-op-text op))
          (role . ,(draw-op-role op))
          (bold? . ,(draw-op-bold? op))
          (dim? . ,(draw-op-dim? op))
          (reverse? . ,(draw-op-reverse? op))))

(define (alist->draw-op alist)
        "Reconstruct a <draw-op> record from ALIST."
        (make-draw-op (or (prop-ref alist 'x) 0)
                      (or (prop-ref alist 'y) 0)
                      (or (prop-ref alist 'text) "")
                      #:role (ensure-symbol (or (prop-ref alist 'role) 'default))
                      #:bold? (and (prop-ref alist 'bold?) #t)
                      #:dim? (and (prop-ref alist 'dim?) #t)
                      #:reverse? (and (prop-ref alist 'reverse?) #t)))

(define (alist-structure? val)
        "Return #t when VAL is a non-empty proper list of (key . value) pairs with
symbol/string/keyword keys."
        (and (list? val)
             (not (null? val))
             (every (lambda (entry)
                      (and (pair? entry)
                           (or (symbol? (car entry))
                               (string? (car entry))
                               (keyword? (car entry)))))
                    val)))

(define (protocol-json-normalize val)
        "Normalize VAL for guile-json serialization: alists stay objects, plain lists
become vectors, symbols/chars/keywords become strings, <draw-op> becomes alist."
        (cond
          ((or (string? val) (number? val) (boolean? val)) val)
          ((null? val) #())
          ((symbol? val) (symbol->string val))
          ((keyword? val) (symbol->string (keyword->symbol val)))
          ((char? val) (string val))
          ((draw-op? val) (protocol-json-normalize (draw-op->alist val)))
          ((vector? val) (vector-map (lambda (_i x) (protocol-json-normalize x)) val))
          ((alist-structure? val)
            (map (lambda (pair)
                   (cons (cond
                           ((symbol? (car pair)) (symbol->string (car pair)))
                           ((keyword? (car pair)) (symbol->string (keyword->symbol (car pair))))
                           (else (ensure-string (car pair))))
                         (protocol-json-normalize (cdr pair))))
                 val))
          ((list? val)
            (list->vector (map protocol-json-normalize val)))
          ((pair? val)
            (vector (protocol-json-normalize (car val))
                    (protocol-json-normalize (cdr val))))
          (else (format #f "~a" val))))

(define (ui-message->alist msg)
        "Convert <ui-message> MSG into a plain alist."
        `((version . ,(ui-message-version msg))
          (type . ,(ui-message-type msg))
          (seq . ,(ui-message-seq msg))
          (source . ,(ui-message-source msg))
          (timestamp . ,(ui-message-timestamp msg))
          (payload . ,(ui-message-payload msg))))

(define (decode-json-value val)
        "Convert guile-json decoded values back into symbol-keyed alists and lists
where appropriate."
        (cond
          ((vector? val)
            (map decode-json-value (vector->list val)))
          ((alist-structure? val)
            (map (lambda (pair)
                   (cons (ensure-symbol (car pair))
                         (decode-json-value (cdr pair))))
                 val))
          (else val)))

(define (alist->ui-message alist)
        "Convert a decoded ALIST into a <ui-message>, or #f when required fields are missing."
        (and (pair? alist)
             (let ((raw-type (prop-ref alist 'type))
                   (ver (or (prop-ref alist 'version) %ui-protocol-version)))
               (and raw-type
                    (integer? ver)
                    (= ver %ui-protocol-version)
                    (let ((type-sym (ensure-symbol raw-type)))
                      (and (or (memq type-sym %ui-client-message-types)
                               (memq type-sym %ui-server-message-types))
                           (make-ui-message type-sym
                                            #:version ver
                                            #:seq (or (prop-ref alist 'seq) 0)
                                            #:source (ensure-symbol (or (prop-ref alist 'source) 'remote))
                                            #:payload (decode-json-value (or (prop-ref alist 'payload) '()))
                                            #:timestamp (or (prop-ref alist 'timestamp) 0))))))))

(define (encode-ui-message msg)
        "Serialize <ui-message> MSG into a single-line JSON string (no trailing newline)."
        (scm->json-string (protocol-json-normalize (ui-message->alist msg))))

(define (decode-ui-message line)
        "Decode a JSON-line string LINE into a validated <ui-message>, or #f on error."
        (let ((parsed (safe-json-parse line #f)))
          (and (pair? parsed)
               (alist->ui-message parsed))))

(define (write-ui-frame! port msg)
        "Write <ui-message> MSG as a newline-delimited JSON frame to PORT and flush."
        (display (encode-ui-message msg) port)
        (newline port)
        (force-output port))

(define (read-ui-frame port)
        "Read one newline-delimited JSON frame from PORT. Returns <ui-message>,
eof-object on EOF, or #f on malformed frame."
        (let ((line (read-line port)))
          (if (eof-object? line)
            line
            (decode-ui-message (string-trim-both line)))))

;;;
;;; UI Server State Machine
;;;
(define-record-type <ui-server>
                    (%make-ui-server mutex seq state clients handlers pending-frames)
                    ui-server?
                    (mutex ui-server-mutex)
                    (seq ui-server-seq set-ui-server-seq!)
                    (state ui-server-state set-ui-server-state!)
                    (clients ui-server-clients set-ui-server-clients!)
                    (handlers ui-server-handlers)
                    (pending-frames ui-server-pending-frames set-ui-server-pending-frames!))

(define* (make-ui-server #:key (initial-state '()))
         "Create a stateful <ui-server> managing connected UI clients, event handlers,
and outbound serializable frames."
         (%make-ui-server (make-mutex)
                          0
                          (or initial-state '())
                          '()
                          (make-hash-table 17)
                          '()))

(define (server-next-seq! srv)
        (let ((n (+ (ui-server-seq srv) 1)))
          (set-ui-server-seq! srv n)
          n))

(define (server-enqueue-frame! srv msg)
        (set-ui-server-pending-frames!
          srv
          (append (ui-server-pending-frames srv) (list msg)))
        ;; Deliver to any clients attached with an outbound transport port
        (for-each
          (lambda (client-entry)
            (let ((port (prop-ref (cdr client-entry) 'transport-port)))
              (when (output-port? port)
                (catch #t
                  (lambda () (write-ui-frame! port msg))
                  (lambda _ #f)))))
          (ui-server-clients srv))
        msg)

(define* (ui-server-connect! srv client-id
                             #:key (cols 80) (rows 24) (transport-port #f))
         "Register CLIENT-ID on SRV with viewport COLS x ROWS and optional TRANSPORT-PORT."
         (let ((cid (ensure-symbol client-id)))
           (with-mutex (ui-server-mutex srv)
                       (let* ((info `((cols . ,(max 1 cols))
                                      (rows . ,(max 1 rows))
                                      (transport-port . ,transport-port)))
                              (others (alist-delete cid (ui-server-clients srv) eq?)))
                         (set-ui-server-clients! srv (cons (cons cid info) others))
                         (let ((welcome (make-ui-message 'status
                                                         #:seq (server-next-seq! srv)
                                                         #:source 'server
                                                         #:payload `((client . ,cid)
                                                                     (connected? . #t)
                                                                     (cols . ,cols)
                                                                     (rows . ,rows)))))
                           (server-enqueue-frame! srv welcome))))))

(define (ui-server-disconnect! srv client-id)
        "Disconnect CLIENT-ID from SRV and emit a goodbye frame."
        (let ((cid (ensure-symbol client-id)))
          (with-mutex (ui-server-mutex srv)
                      (set-ui-server-clients! srv (alist-delete cid (ui-server-clients srv) eq?))
                      (server-enqueue-frame!
                        srv
                        (make-ui-message 'goodbye
                                         #:seq (server-next-seq! srv)
                                         #:source 'server
                                         #:payload `((client . ,cid)))))))

(define (ui-server-on-event! srv event-type handler)
        "Register HANDLER (lambda (srv msg) ...) for client EVENT-TYPE on SRV."
        (hashq-set! (ui-server-handlers srv) (ensure-symbol event-type) handler))

(define* (ui-server-emit! srv type payload #:key (source 'server))
         "Emit a server frame/event message of TYPE with PAYLOAD to all connected clients."
         (with-mutex (ui-server-mutex srv)
                     (server-enqueue-frame!
                       srv
                       (make-ui-message type
                                        #:seq (server-next-seq! srv)
                                        #:source source
                                        #:payload payload))))

(define (ui-server-handle-event! srv msg-or-json)
        "Process an incoming client <ui-message> (or JSON string) on SRV.
Handles built-in lifecycle/resize/ping events and invokes registered handlers."
        (let
            ((msg
               (cond
                 ((ui-message? msg-or-json) msg-or-json)
                 ((string? msg-or-json) (decode-ui-message msg-or-json))
                 (else #f))))
          (if msg
            (let
                ((etype (ui-message-type msg))
                 (payload (ui-message-payload msg))
                 (src (ui-message-source msg)))
              (case etype
                ((ping)
                  (ui-server-emit! srv
                                   'pong
                                   `((client-seq . ,(ui-message-seq msg)) (source . ,src))))
                ((attach)
                  (ui-server-connect! srv
                                      src
                                      #:cols
                                      (or (prop-ref payload 'cols) 80)
                                      #:rows
                                      (or (prop-ref payload 'rows) 24)))
                ((detach) (ui-server-disconnect! srv src))
                ((resize)
                  (with-mutex (ui-server-mutex srv)
                              (let
                                  ((cols (max 1 (or (prop-ref payload 'cols) 80)))
                                   (rows (max 1 (or (prop-ref payload 'rows) 24))))
                                (set-ui-server-state! srv
                                                      (prop-set (prop-set (ui-server-state srv) 'cols cols) 'rows rows)))))
                (else #f))
              (let
                  ((handler (hashq-ref (ui-server-handlers srv) etype)))
                (when handler (handler srv msg)))
              msg)
            (ui-server-emit! srv 'error '((reason . "invalid ui message"))))))

(define (ui-server-drain-frames! srv)
        "Atomically return and clear all queued outbound frames on SRV."
        (with-mutex (ui-server-mutex srv)
                    (let ((frames (ui-server-pending-frames srv)))
                      (set-ui-server-pending-frames! srv '())
                      frames)))

(define* (ui-server-route-subagent-event! srv harness-id parent-id kind detail
                                          #:key (iteration #f))
         "Route a subagent ReAct/lifecycle event into connected UI clients as a
structured 'subagent-event frame."
         (ui-server-emit!
           srv
           'subagent-event
           `((harness-id . ,(ensure-string harness-id))
             (parent-id . ,(and parent-id (ensure-string parent-id)))
             (kind . ,(ensure-symbol kind))
             (iteration . ,iteration)
             (detail . ,(or detail "")))
           #:source 'subagent))

;;;
;;; UI Client State Machine
;;;
(define-record-type <ui-client>
                    (%make-ui-client id cols rows seq connected? out-port
                                     last-frame status subagent-events notifications)
                    ui-client?
                    (id ui-client-id)
                    (cols ui-client-cols set-ui-client-cols!)
                    (rows ui-client-rows set-ui-client-rows!)
                    (seq ui-client-seq set-ui-client-seq!)
                    (connected? ui-client-connected? set-ui-client-connected?!)
                    (out-port ui-client-out-port set-ui-client-out-port!)
                    (last-frame ui-client-last-frame set-ui-client-last-frame!)
                    (status ui-client-status set-ui-client-status!)
                    (subagent-events ui-client-subagent-events set-ui-client-subagent-events!)
                    (notifications ui-client-notifications set-ui-client-notifications!))

(define* (make-ui-client #:key (id 'tui-client) (cols 80) (rows 24) (out-port #f))
         "Create a terminal rendering <ui-client> state handle."
         (%make-ui-client (ensure-symbol id)
                          (max 1 cols)
                          (max 1 rows)
                          0
                          #f
                          out-port
                          #f
                          '((status-text . "READY") (active? . #f))
                          '()
                          '()))

(define (ui-client-attach-port! client port)
        "Attach an outbound transport PORT (e.g., SSH pipe) to CLIENT."
        (set-ui-client-out-port! client port))

(define (ui-client-send-event client type payload)
        "Build an outgoing client <ui-message> of TYPE with PAYLOAD (and write to
attached out-port if present)."
        (let* ((next-seq (+ (ui-client-seq client) 1))
               (_ (set-ui-client-seq! client next-seq))
               (msg (make-ui-message type
                                     #:seq next-seq
                                     #:source (ui-client-id client)
                                     #:payload payload)))
          (when (eq? (ensure-symbol type) 'resize)
            (when (integer? (prop-ref payload 'cols))
              (set-ui-client-cols! client (max 1 (prop-ref payload 'cols))))
            (when (integer? (prop-ref payload 'rows))
              (set-ui-client-rows! client (max 1 (prop-ref payload 'rows)))))
          (let ((port (ui-client-out-port client)))
            (when (output-port? port)
              (write-ui-frame! port msg)))
          msg))

(define (ui-client-apply-frame! client msg-or-json)
        "Apply an incoming server <ui-message> (or JSON string) to CLIENT's local
renderable state."
        (let ((msg (cond
                     ((ui-message? msg-or-json) msg-or-json)
                     ((string? msg-or-json) (decode-ui-message msg-or-json))
                     (else #f))))
          (when msg
            (let ((mtype (ui-message-type msg))
                  (payload (ui-message-payload msg)))
              (case mtype
                ((frame)
                  (set-ui-client-last-frame! client payload)
                  (when (integer? (prop-ref payload 'cols))
                    (set-ui-client-cols! client (max 1 (prop-ref payload 'cols))))
                  (when (integer? (prop-ref payload 'rows))
                    (set-ui-client-rows! client (max 1 (prop-ref payload 'rows)))))
                ((status)
                  (set-ui-client-status! client payload)
                  (when (prop-ref payload 'connected?)
                    (set-ui-client-connected?! client #t)))
                ((subagent-event)
                  ;; Keep bounded recent subagent events (newest last, max 64)
                  (let* ((evts (append (ui-client-subagent-events client) (list payload)))
                         (trimmed (if (> (length evts) 64)
                                    (drop evts (- (length evts) 64))
                                    evts)))
                    (set-ui-client-subagent-events! client trimmed)))
                ((notify)
                  (set-ui-client-notifications!
                    client
                    (append (ui-client-notifications client) (list payload))))
                ((goodbye)
                  (set-ui-client-connected?! client #f))
                (else #f))))
          client))

(define (ui-client-render-snapshot client)
        "Return an alist representing CLIENT's current renderable snapshot."
        `((id . ,(ui-client-id client))
          (connected? . ,(ui-client-connected? client))
          (cols . ,(ui-client-cols client))
          (rows . ,(ui-client-rows client))
          (frame . ,(ui-client-last-frame client))
          (status . ,(ui-client-status client))
          (subagent-events . ,(ui-client-subagent-events client))
          (notifications . ,(ui-client-notifications client))))

(define (pump-ui-pair! server client events)
        "In-process bridge helper: send EVENTS through CLIENT -> SERVER, drain all
resulting frames from SERVER, and apply them to CLIENT. Returns frames applied."
        (for-each
          (lambda (ev)
            (let* ((msg (if (ui-message? ev)
                          ev
                          (ui-client-send-event client (car ev) (cdr ev))))
                   (wire (encode-ui-message msg)))
              (ui-server-handle-event! server wire)))
          events)
        (let ((frames (ui-server-drain-frames! server)))
          (for-each
            (lambda (frame)
              (ui-client-apply-frame! client (encode-ui-message frame)))
            frames)
          frames))
