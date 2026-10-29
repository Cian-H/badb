;;; badb — Autonomous ReAct Tool-Calling Engine
;;;
;;; Implements the multi-turn Thought -> Action -> Observation -> Thought
;;; execution loop. Dynamically inspects available tools across both the
;;; kernel registry and active MCP servers, parses tool calls from both
;;; native API schemas and embedded text/XML formats, enforces guardrails,
;;; logs execution provenance to the journal, and drives agent tasks to completion.

(define-module (badb react) #:use-module
               (badb util)
               #:use-module
               (badb model)
               #:use-module
               (badb context)
               #:use-module
               (badb registry)
               #:use-module
               (badb journal)
               #:use-module
               (badb transaction)
               #:use-module
               (badb mcp)
               #:use-module
               (json)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:export
               (make-react-session react-session?
                                   react-session-messages
                                   react-session-status
                                   react-session-iteration
                                   react-session-max-iterations
                                   react-session-client
                                   react-session-reg
                                   react-session-mcp-mgr
                                   react-session-jrn
                                   react-session-mgr
                                   react-session-hooks
                                   react-session-steering-queue
                                   react-session-max-tokens
                                   react-session-compactor
                                   set-react-session-messages!
                                   set-react-session-status!
                                   set-react-session-iteration!
                                   set-react-session-max-tokens!
                                   set-react-session-compactor!
                                   set-react-session-steering-queue!
                                   make-steering-queue
                                   steering-queue?
                                   steering-push!
                                   steering-drain!
                                   steering-empty?
                                   steering-count
                                   collect-available-tools
                                   registry-feature->tool-spec
                                   mcp-tool->tool-spec
                                   sanitize-tool-parameters
                                   parse-tool-calls
                                   determine-step-content
                                   reasoning-fallback-content
                                   stream-model-completion
                                   parse-xml-tool-calls
                                   execute-tool-call
                                   react-step!
                                   react-run!))

;;;
;;; Tool Schema Translators
;;;
(define (normalize-tool-name str)
  "Convert dots and dashes to underscores for strict tool name compatibility."
  (string-map (lambda (c) (if (or (char=? c #\.) (char=? c #\-)) #\_ c)) str))

(define (denormalize-tool-name str)
  "Return candidate feature IDs for STR (e.g. math_mul -> '(\"math_mul\" \"math.mul\" \"math-mul\"))."
  (let ((dots (string-map (lambda (c) (if (char=? c #\_) #\. c)) str))
        (dashes (string-map (lambda (c) (if (char=? c #\_) #\- c)) str)))
    (delete-duplicates (list str dots dashes))))

(define (lookup-registry-entry reg name lookup-fn)
        "Look up an entry in REG with LOOKUP-FN using the exact name or a
denormalized variant (dots/dashes for underscores)."
        (if reg
          (let loop
            ((candidates (denormalize-tool-name name)))
            (if (null? candidates)
              #f
              (or (lookup-fn reg (car candidates)) (loop (cdr candidates)))))
          #f))

(define (lookup-registry-feature reg name)
        "Look up procedure in REG using exact name or denormalized variants."
        (lookup-registry-entry reg name registry-lookup))

(define (lookup-registry-metadata reg name)
        "Look up metadata alist in REG using exact name or denormalized variants."
        (lookup-registry-entry reg name registry-metadata))

(define %empty-tool-parameters
  '((type . "object")
    (properties . ())
    (required . #())))

(define (sanitize-tool-parameters params)
        "Ensure PARAMS conforms to strict JSON Schema object specification.
If 'properties' is missing from an object schema, inject an empty properties alist."
        (let*
            ((p
               (if (and (list? params) (pair? params))
                 params
                 %empty-tool-parameters))
             (has-type? (or (assoc-ref p "type") (assoc-ref p 'type)))
             (with-type
               (if has-type?
                 p
                 (cons '(type . "object") p)))
             (has-props?
               (or (assoc-ref with-type "properties") (assoc-ref with-type 'properties))))
          (if has-props?
            with-type
            (cons '(properties . ()) with-type))))

(define (registry-feature->tool-spec feature)
        "Convert a kernel registry feature entry into an OpenAI-compatible tool spec."
        (let*
            ((id (assoc-ref feature 'id))
             (id-str (ensure-string id))
             (meta (or (assoc-ref feature 'metadata) '()))
             (desc
               (or (assoc-ref meta 'description)
                   (assoc-ref meta "description")
                   (format #f "Kernel feature: ~a" id-str)))
             (params
               (sanitize-tool-parameters
                 (or (assoc-ref meta 'parameters)
                     (assoc-ref meta "parameters")
                     %empty-tool-parameters))))
          `((type . "function")
            (function .
                      ((name . ,(normalize-tool-name id-str))
                       (description . ,desc)
                       (parameters . ,params)
                       (_badb_source . "registry")
                       (_badb_original_id . ,id-str))))))

(define (mcp-tool->tool-spec tool)
        "Convert an MCP discovered tool entry into an OpenAI-compatible tool spec."
        (let*
            ((name (or (assoc-ref tool "name") (assoc-ref tool 'name)))
             (server (or (assoc-ref tool 'server) (assoc-ref tool "server") "unknown"))
             (desc (or (assoc-ref tool "description") (assoc-ref tool 'description) ""))
             (schema
               (sanitize-tool-parameters
                 (or (assoc-ref tool "inputSchema")
                     (assoc-ref tool 'inputSchema)
                     %empty-tool-parameters))))
          `((type . "function")
            (function .
                      ((name . ,(normalize-tool-name name))
                       (description . ,desc)
                       (parameters . ,schema)
                       (_badb_source . "mcp")
                       (_badb_server . ,server)
                       (_badb_original_id . ,name))))))

(define* (collect-available-tools reg mcp-mgr #:optional (extra-tools '()))
  "Collect all tools from REG and MCP-MGR, plus any EXTRA-TOOLS."
  (let* ((reg-features (if reg (registry-list reg) '()))
         (reg-tools (map registry-feature->tool-spec reg-features))
         (mcp-tools (if mcp-mgr
                        (map mcp-tool->tool-spec (mcp-manager-list-tools mcp-mgr))
                        '())))
    (append reg-tools mcp-tools extra-tools)))

;;;
;;; Tool Call Parsing (Native API and Text/XML Fallback)
;;;
(define (parse-xml-tool-calls text)
        "Extract tool calls formatted as <tool_call>{...}</tool_call> from text."
        (if (non-empty-string? text)
          (let loop
            ((remaining text) (calls '()) (call-counter 0))
            (let
                ((start-tag (string-contains remaining "<tool_call>")))
              (if start-tag
                (let*
                    ((after-start (substring remaining (+ start-tag (string-length "<tool_call>"))))
                     (end-tag (string-contains after-start "</tool_call>")))
                  (if end-tag
                    (let*
                        ((json-str (string-trim-both (substring after-start 0 end-tag)))
                         (rest (substring after-start (+ end-tag (string-length "</tool_call>"))))
                         (parsed (safe-json-parse json-str #f))
                         (raw-name (and (pair? parsed) (prop-ref parsed 'name))))
                      (if raw-name
                        (let*
                            ((raw-args (or (prop-ref parsed 'arguments) '()))
                             (args (safe-json-parse raw-args raw-args))
                             (call-id (format #f "call_xml_~a" (+ call-counter 1)))
                             (tool-call `((id . ,call-id) (name . ,raw-name) (arguments . ,args))))
                          (loop rest (cons tool-call calls) (+ call-counter 1)))
                        (loop rest calls call-counter)))
                    (reverse calls)))
                (reverse calls))))
          '()))

(define (reasoning-fallback-content reasoning)
  "Render REASONING as step content for models that answer only in the
reasoning trace, matching determine-step-content's fallback policy:
the reasoning wrapped in a closed think block, then a plain note."
  (format #f "<think>~a</think>\n(Response provided in model reasoning trace)" reasoning))

(define* (stream-model-completion client messages tools max-tokens on-delta)
  "Run one streamed chat completion for the ReAct loop: accumulate content
and reasoning deltas and assemble streamed tool-call fragments
(merge-tool-call-fragment), invoking (ON-DELTA payload type) for every
chunk - guarded, so a throwing ON-DELTA never aborts the stream. Return
(values content reasoning tool-calls) in the same shapes as the
non-streaming path (finalize-tool-call-fragments matches
parse-single-tool-call). When the endpoint ignored streaming and
delivered nothing, fall back to a single model-complete call."
  (let ((content "") (reasoning "") (frag-acc #f) (complete-result #f))
    (model-stream client messages
      (lambda (payload type)
        (case type
          ((thought) (set! reasoning (string-append reasoning payload)))
          ((content) (set! content (string-append content payload)))
          ((tool-call)
            (set! frag-acc (merge-tool-call-fragment frag-acc payload)))
          ((complete-json)
            ;; Endpoint ignored stream:true: whole completion at once.
            ;; Its usage block, when present, rides out through the
            ;; delta channel (type 'usage) for session token accounting.
            (let ((usage (extract-usage payload)))
              (when (and usage (procedure? on-delta))
                (catch #t
                  (lambda () (on-delta usage 'usage))
                  (lambda _ #f))))
            (call-with-values
              (lambda () (determine-step-content payload))
              (lambda (c r)
                (set! complete-result (list c r (parse-tool-calls payload c))))))
          (else #f))
        (when (procedure? on-delta)
          (catch #t
            (lambda () (on-delta payload type))
            (lambda _ #f))))
      #:tools tools
      #:max-tokens max-tokens)
    (if complete-result
      (apply values complete-result)
      (if (and (string-null? (string-trim-both content))
               (string-null? (string-trim-both reasoning))
               (not (pair? frag-acc)))
        (let ((raw-resp (model-complete client messages #:tools tools #:max-tokens max-tokens #:raw? #t)))
          (let ((usage (extract-usage raw-resp)))
            (when (and usage (procedure? on-delta))
              (catch #t
                (lambda () (on-delta usage 'usage))
                (lambda _ #f))))
          (call-with-values
            (lambda () (determine-step-content raw-resp))
            (lambda (fb-content fb-reasoning)
              (values fb-content fb-reasoning (parse-tool-calls raw-resp fb-content)))))
        (let ((native-calls (finalize-tool-call-fragments frag-acc))
              (final-content
                (cond
                  ((not (string-null? (string-trim-both content))) content)
                  ((non-empty-string? reasoning) (reasoning-fallback-content reasoning))
                  (else ""))))
          (values final-content reasoning
                  (if (pair? native-calls)
                    native-calls
                    (parse-xml-tool-calls content))))))))

(define (parse-tool-calls raw-response content-text)
  "Extract tool calls from RAW-RESPONSE (native OpenAI format) or CONTENT-TEXT (XML tags)."
  (let ((native-calls (extract-chat-tool-calls raw-response)))
    (if (and (pair? native-calls) (not (null? native-calls)))
        native-calls
        (parse-xml-tool-calls content-text))))

;;;
;;; Tool Execution & Guardrail Enforcement
;;;
(define (log-tool-execution! jrn name args status extra-alist)
  "Log tool execution outcome to journal JRN if valid."
  (safe-journal-append! jrn
                        `((feature-id . ,name)
                          (checkpoint . ,(format #f "tool:~a" name))
                          (status . ,status)
                          (arguments . ,args)
                          ,@extra-alist)))

(define
  (invoke-registry-procedure impl meta args)
  "Invoke registry IMPL with ARGS, adapting alist arguments to schema properties if available."
  (cond
    ((null? args) (impl))
    ((and (pair? args) (pair? (car args)))
      (catch #t
        (lambda ()
          (let*
              ((params
                 (and meta (or (assoc-ref meta 'parameters) (assoc-ref meta "parameters"))))
               (props
                 (and (pair? params)
                      (or (assoc-ref params 'properties) (assoc-ref params "properties"))))
               (ordered-keys
                 (and (pair? props)
                      (map
                        (lambda (p)
                          (ensure-string (car p)))
                        props))))
            (if (and ordered-keys (pair? ordered-keys))
              (let
                  ((ordered-vals
                     (map
                       (lambda (k)
                         (or (assoc-ref args k) (assoc-ref args (string->symbol k))))
                       ordered-keys)))
                (apply impl ordered-vals))
              (apply impl (map cdr args)))))
        (lambda (k . a)
          (impl args))))
    ((list? args) (apply impl args))
    (else (impl args))))

(define*
  (execute-tool-call tool-call reg mcp-mgr #:key (jrn #f) (mgr #f) (guardrail #f))
  "Execute a single TOOL-CALL alist. Checks GUARDRAIL, dispatches to REGISTRY or MCP-MGR,
logs execution provenance to JRN, and returns (values result is-error?)."
  (let*
      ((name (prop-ref tool-call 'name))
       (args (or (prop-ref tool-call 'arguments) '()))
       ;; Check guardrail policy
       (permission
         (if (procedure? guardrail)
           (catch #t
             (lambda ()
               (guardrail name args))
             (lambda (k . a)
               (cons 'deny (format #f "Guardrail exception (~a): ~s" k a))))
           #t)))
    (cond
      ((or (eq? permission #f) (eq? permission 'deny))
        (values
          `((error .
                   ,(format #f "Permission denied for tool '~a' by guardrail policy" name)))
          #t))
      ((and (pair? permission) (eq? (car permission) 'deny))
        (values `((error . ,(cdr permission))) #t))
      (else
        (catch #t
          (lambda ()
            (let
                ((res
                   (cond
                     ;; 1. Check registry features (exact match or with dots/dashes restored)
                     ((and reg (lookup-registry-feature reg name)) =>
                                                                   (lambda (impl)
                                                                     (if (procedure? impl)
                                                                       (let ((meta (lookup-registry-metadata reg name)))
                                                                         (invoke-registry-procedure impl meta args))
                                                                       (error "Registry entry is not a procedure" name))))
                     ;; 2. Check MCP manager
                     ((and mcp-mgr
                           (find
                             (lambda (t)
                               (let ((tname (prop-ref t 'name)))
                                 (or (string=? tname name) (string=? (normalize-tool-name tname) name))))
                             (mcp-manager-list-tools mcp-mgr)))
                       =>
                       (lambda (matched)
                         (let ((orig-name (prop-ref matched 'name)))
                           (mcp-manager-call-tool mcp-mgr orig-name args))))
                     (else
                       (error (format #f "Tool '~a' not found in registry or active MCP servers" name))))))
              (log-tool-execution! jrn
                                   name
                                   args
                                   "success"
                                   `((result-summary . ,(object->string res))))
              (values res #f)))
          (lambda (key . err-args)
            (let
                ((err-msg (format #f "Execution of tool '~a' failed (~a): ~s" name key err-args)))
              (log-tool-execution! jrn name args "error" `((error . ,err-msg)))
              (values `((error . ,err-msg)) #t))))))))

;;;
;;; Steering Queue
;;;
(define-record-type <steering-queue>
                    (%make-steering-queue entries)
                    steering-queue?
                    (entries steering-queue-entries set-steering-queue-entries!))

(define (make-steering-queue)
        "Create a new empty mutable steering queue."
        (%make-steering-queue '()))

(define (steering-push! queue entry)
        "Push ENTRY onto the tail of QUEUE.
ENTRY is either a steering instruction string or the symbol 'interrupt."
        (set-steering-queue-entries! queue (cons entry (steering-queue-entries queue))))

(define (steering-drain! queue)
        "Return all queued entries in FIFO order and empty QUEUE."
        (let ((entries (reverse (steering-queue-entries queue))))
          (set-steering-queue-entries! queue '())
          entries))

(define (steering-empty? queue)
        "Return #t if QUEUE currently holds no entries."
        (null? (steering-queue-entries queue)))

(define (steering-count queue)
        "Return the number of entries currently held in QUEUE."
        (length (steering-queue-entries queue)))

;;;
;;; ReAct Session Record
;;;
(define-record-type <react-session>
                    (%make-react-session messages
                                         status
                                         iteration
                                         max-iterations
                                         hooks
                                         client
                                         reg
                                         mcp-mgr
                                         jrn
                                         mgr
                                         max-tokens
                                         compactor
                                         steering-queue)
                    react-session?
                    (messages react-session-messages set-react-session-messages!)
                    (status react-session-status set-react-session-status!)
                    (iteration react-session-iteration set-react-session-iteration!)
                    (max-iterations react-session-max-iterations set-react-session-max-iterations!)
                    (hooks react-session-hooks set-react-session-hooks!)
                    (client react-session-client)
                    (reg react-session-reg)
                    (mcp-mgr react-session-mcp-mgr)
                    (jrn react-session-jrn)
                    (mgr react-session-mgr)
                    (max-tokens react-session-max-tokens set-react-session-max-tokens!)
                    (compactor react-session-compactor set-react-session-compactor!)
                    (steering-queue react-session-steering-queue set-react-session-steering-queue!))

(define (make-react-session . args)
        "Construct a new <react-session> record.
Supports positional client: (make-react-session client #:initial-messages ...)
and pure keyword style: (make-react-session #:client client #:messages ... #:registry ...)"
        (let*
            ((has-pos-client? (and (pair? args) (not (keyword? (car args)))))
             (pos-client
               (if has-pos-client?
                 (car args)
                 #f))
             (kw-args
               (if has-pos-client?
                 (cdr args)
                 args))
             (get-kw
               (lambda (kw-list default)
                 (let loop
                   ((kws kw-list))
                   (if (null? kws)
                     default
                     (let*
                         ((target (car kws))
                          (probe
                            (let find-arg
                              ((rest kw-args))
                              (cond
                                ((or (null? rest) (null? (cdr rest))) #f)
                                ((eq? (car rest) target) (cadr rest))
                                (else (find-arg (cddr rest)))))))
                       (if probe
                         probe
                         (loop (cdr kws))))))))
             (client (or pos-client (get-kw '(#:client) #f)))
             (msgs (get-kw '(#:initial-messages #:messages) '()))
             (reg (get-kw '(#:reg #:registry) #f))
             (mcp-mgr (get-kw '(#:mcp-mgr #:mcp-manager) #f))
             (jrn (get-kw '(#:jrn #:journal) #f))
             (mgr (get-kw '(#:mgr #:transaction-manager) #f))
             (max-iter (get-kw '(#:max-iterations) 10))
             (max-tokens (get-kw '(#:max-tokens) 4096))
             (compactor
               (or (get-kw '(#:compactor) #f) (make-context-compactor #:max-tokens max-tokens)))
             (raw-hooks (get-kw '(#:hooks) '()))
             (guardrail-fn (get-kw '(#:guardrail) #f))
             (hooks
               (if
                   (and guardrail-fn (not (assoc-ref raw-hooks 'guardrail)))
                 (cons (cons 'guardrail guardrail-fn) raw-hooks)
                 raw-hooks))
             (steering-queue (get-kw '(#:steering-queue) #f)))
          (%make-react-session
            (if (list? msgs)
              msgs
              (list msgs))
            'idle
            0
            max-iter
            hooks
            client
            reg
            mcp-mgr
            jrn
            mgr
            max-tokens
            compactor
            steering-queue)))

;;;
;;; Step & Run Cycle
;;;
(define (determine-step-content raw-resp)
        "Extract normalized content and reasoning trace from model RAW-RESP.
Returns (values content reasoning)."
        (let*
            ((choices (json-ref raw-resp "choices"))
             (c0
               (and (vector? choices) (> (vector-length choices) 0) (vector-ref choices 0)))
             (orig-msg (and (pair? c0) (json-ref c0 "message")))
             (reasoning
               (and (pair? orig-msg)
                    (or (json-ref orig-msg "reasoning_content") (json-ref orig-msg "reasoning"))))
             (raw-content (extract-chat-content raw-resp))
             (orig-content (and (pair? orig-msg) (json-ref orig-msg "content")))
             (content
               (cond
                 ((and (string? orig-content)
                       (not (member orig-content '("null" ":null")))
                       (not (string-null? (string-trim-both orig-content))))
                   orig-content)
                 ((non-empty-string? reasoning)
                   (reasoning-fallback-content reasoning))
                 ((and (non-empty-string? raw-content)
                       (not (member raw-content '("null" ":null"))))
                   raw-content)
                 (else ""))))
          (values content reasoning)))

(define (build-assistant-tool-turn tool-calls content reasoning)
  "Construct the assistant message turn representing TOOL-CALLS with CONTENT and REASONING."
  (let*
      ((tool-calls-vec
         (list->vector
           (map
             (lambda (tc)
               (let*
                   ((tc-id (or (prop-ref tc 'id) "call_0"))
                    (tc-name (or (prop-ref tc 'name) ""))
                    (raw-args (or (prop-ref tc 'arguments) '()))
                    (args-str (safe-json-string raw-args)))
                 `((id . ,tc-id) (type . "function")
                   (function . ((name . ,tc-name) (arguments . ,args-str))))))
             tool-calls)))
       (eff-reasoning
         (if (non-empty-string? reasoning)
           reasoning
           (format #f
                   "Invoking ~a..."
                   (or
                     (and (pair? tool-calls)
                          (let ((first-tc (car tool-calls)))
                            (or (prop-ref first-tc 'name) "")))
                     "tool")))))
    `((role . "assistant")
      (reasoning_content . ,eff-reasoning)
      (content .
               ,(if
                    (and (string? content) (not (string-null? content)))
                  content
                  ""))
      (tool_calls . ,tool-calls-vec))))

(define* (run-isolated-hook! fn label #:rest hook-args)
  "Invoke FN with HOOK-ARGS when it is a procedure, reporting a failure on
stderr as a warning instead of letting it abort the caller."
  (catch #t
    (lambda () (when (procedure? fn) (apply fn hook-args)))
    (lambda (key . err-args)
      (format (current-error-port)
              "warning: ~a hook failed (~a): ~s~%" label key err-args))))

(define
  (execute-step-tools tool-calls
                      reg
                      mcp-mgr
                      jrn
                      mgr
                      guardrail-fn
                      on-tool-call-fn
                      on-tool-result-fn)
  "Execute TOOL-CALLS sequentially, invoking callbacks and returning pairs of (tool-call . tool-msg).
Hook exceptions are isolated: a failing on-tool-call or on-tool-result
callback is reported on stderr and skipped, never aborting the loop."
  (map
    (lambda (tc)
      (run-isolated-hook! on-tool-call-fn "on-tool-call" tc)
      (call-with-values
        (lambda ()
          (execute-tool-call tc reg mcp-mgr #:jrn jrn #:mgr mgr #:guardrail guardrail-fn))
        (lambda (res err?)
          (run-isolated-hook! on-tool-result-fn "on-tool-result" tc res err?)
          (let*
              ((content-str (safe-json-string res))
               (tc-id (or (prop-ref tc 'id) "call_0"))
               (tc-name (or (prop-ref tc 'name) ""))
               (tool-msg `((role . "tool") (tool_call_id . ,tc-id)
                           (name . ,tc-name)
                           (content . ,content-str))))
            (cons tc tool-msg)))))
    tool-calls))

(define (react-step! session)
        "Execute a single turn of the ReAct cycle.
Returns:
  (values 'completed content-string)
  (values 'continue list-of-tool-results)
  (values 'max-iterations-reached #f)
  (values 'error error-message)"
        (let*
            ((iter (react-session-iteration session))
             (max-iter (react-session-max-iterations session))
             (client (react-session-client session))
             (reg (react-session-reg session))
             (mcp-mgr (react-session-mcp-mgr session))
             (jrn (react-session-jrn session))
             (mgr (react-session-mgr session))
             (max-tokens (or (react-session-max-tokens session) 4096))
             (compactor (react-session-compactor session))
             (hooks (react-session-hooks session))
             (guardrail-fn (assoc-ref hooks 'guardrail))
             (on-step-fn (assoc-ref hooks 'on-step))
             (on-tool-call-fn (assoc-ref hooks 'on-tool-call))
             (on-tool-result-fn (assoc-ref hooks 'on-tool-result))
             (on-delta-fn (assoc-ref hooks 'on-delta)))
          (if (>= iter max-iter)
            (begin
              (set-react-session-status! session 'max-iterations-reached)
              (values 'max-iterations-reached #f))
            (begin
              (set-react-session-iteration! session (+ iter 1))
              (set-react-session-status! session 'running)
              (when (procedure? on-step-fn) (on-step-fn (+ iter 1) 'starting))
              (catch #t
                (lambda ()
                  (let*
                      ((tools (collect-available-tools reg mcp-mgr))
                       (raw-msgs (react-session-messages session))
                       (msgs
                         (if
                             (and compactor (compaction-needed? compactor raw-msgs))
                           (let
                               ((compacted (compact-messages compactor raw-msgs)))
                             (set-react-session-messages! session compacted)
                             compacted)
                           raw-msgs)))
                    (call-with-values
                      (lambda ()
                        (stream-model-completion client msgs tools max-tokens on-delta-fn))
                      (lambda (content reasoning tool-calls)
                        (if (null? tool-calls)
                            (let
                                ((final-msg ` ((role . "assistant") (content . ,content))))
                              (set-react-session-messages! session (append msgs (list final-msg)))
                              (set-react-session-status! session 'completed)
                              (when (procedure? on-step-fn) (on-step-fn (+ iter 1) 'completed))
                              (values 'completed content))
                            (begin
                              (set-react-session-status! session 'awaiting-tool)
                              (let*
                                  ((asst-msg (build-assistant-tool-turn tool-calls content reasoning))
                                   (results
                                     (execute-step-tools tool-calls
                                                         reg
                                                         mcp-mgr
                                                         jrn
                                                         mgr
                                                         guardrail-fn
                                                         on-tool-call-fn
                                                         on-tool-result-fn))
                                   (tool-msgs (map cdr results))
                                   (new-turn (cons asst-msg tool-msgs)))
                                (set-react-session-messages! session (append msgs new-turn))
                                (set-react-session-status! session 'running)
                                (when (procedure? on-step-fn) (on-step-fn (+ iter 1) 'tools-executed))
                                (values 'continue results))))))))
                (lambda (key . args)
                  (let
                      ((err-msg (format #f "ReAct loop error (~a): ~s" key args)))
                    (set-react-session-status! session 'error)
                    (values 'error err-msg))))))))

(define (latest-assistant-content session)
        "Return the content of the most recent assistant message in SESSION, or #f."
        (let loop
          ((msgs (reverse (react-session-messages session))))
          (cond
            ((null? msgs) #f)
            ((and (pair? (car msgs))
                  (equal? (assoc-ref (car msgs) 'role) "assistant")
                  (string? (assoc-ref (car msgs) 'content)))
             (assoc-ref (car msgs) 'content))
            (else (loop (cdr msgs))))))

(define (apply-steering! session)
        "Drain SESSION's steering queue at an iteration boundary.
Each drained string is appended to the session messages as a user turn;
the symbol 'interrupt halts the loop. Returns 'interrupted or #f."
        (let ((queue (react-session-steering-queue session)))
          (if (steering-queue? queue)
            (let loop
              ((entries (steering-drain! queue)))
              (cond
                ((null? entries) #f)
                ((eq? (car entries) 'interrupt) 'interrupted)
                ((string? (car entries))
                  (set-react-session-messages!
                    session
                    (append
                      (react-session-messages session)
                      (list `((role . "user") (content . ,(car entries))))))
                  (loop (cdr entries)))
                (else (loop (cdr entries)))))
            #f)))

(define* (react-run! session #:key (on-finish #f))
  "Execute the ReAct session loop until completion, error, interruption, or max iterations.
The session's steering queue is drained before each LLM call: queued strings are
injected as user messages, and a queued 'interrupt halts the loop with verdict
'interrupted."
  (let loop ()
    (if (eq? (apply-steering! session) 'interrupted)
      (let ((content (latest-assistant-content session)))
        (set-react-session-status! session 'interrupted)
        (when (procedure? on-finish)
          (on-finish 'interrupted content))
        (values 'interrupted content session))
      (call-with-values (lambda () (react-step! session))
        (lambda (status payload)
          (case status
            ((completed)
           (when (procedure? on-finish)
             (on-finish 'completed payload))
           (values 'completed payload session))
          ((continue)
           (loop))
          ((max-iterations-reached)
           (when (procedure? on-finish)
             (on-finish 'max-iterations-reached #f))
           (values 'max-iterations-reached #f session))
          ((error)
           (when (procedure? on-finish)
             (on-finish 'error payload))
           (values 'error payload session))))))))
