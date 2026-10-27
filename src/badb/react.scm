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
                                   react-session-max-tokens
                                   react-session-compactor
                                   set-react-session-messages!
                                   set-react-session-status!
                                   set-react-session-iteration!
                                   set-react-session-max-tokens!
                                   set-react-session-compactor!
                                   collect-available-tools
                                   registry-feature->tool-spec
                                   mcp-tool->tool-spec
                                   parse-tool-calls
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

(define (lookup-registry-feature reg name)
        "Look up procedure in REG using exact name or denormalized variants."
        (if reg
          (let loop
            ((candidates (denormalize-tool-name name)))
            (if (null? candidates)
              #f
              (or (registry-lookup reg (car candidates)) (loop (cdr candidates)))))
          #f))

(define (lookup-registry-metadata reg name)
        "Look up metadata alist in REG using exact name or denormalized variants."
        (if reg
          (let loop
            ((candidates (denormalize-tool-name name)))
            (if (null? candidates)
              #f
              (or (registry-metadata reg (car candidates)) (loop (cdr candidates)))))
          #f))

(define (registry-feature->tool-spec feature)
  "Convert a kernel registry feature entry into an OpenAI-compatible tool spec."
  (let* ((id (assoc-ref feature 'id))
         (id-str (if (symbol? id) (symbol->string id) id))
         (meta (or (assoc-ref feature 'metadata) '()))
         (desc (or (assoc-ref meta 'description)
                   (assoc-ref meta "description")
                   (format #f "Kernel feature: ~a" id-str)))
         (params (or (assoc-ref meta 'parameters)
                     (assoc-ref meta "parameters")
                     '((type . "object")
                       (properties . ())
                       (required . #())))))
    `((type . "function")
      (function . ((name . ,(normalize-tool-name id-str))
                   (description . ,desc)
                   (parameters . ,params)
                   (_badb_source . "registry")
                   (_badb_original_id . ,id-str))))))

(define (mcp-tool->tool-spec tool)
  "Convert an MCP discovered tool entry into an OpenAI-compatible tool spec."
  (let* ((name (or (assoc-ref tool "name") (assoc-ref tool 'name)))
         (server (or (assoc-ref tool 'server) (assoc-ref tool "server") "unknown"))
         (desc (or (assoc-ref tool "description") (assoc-ref tool 'description) ""))
         (schema (or (assoc-ref tool "inputSchema") (assoc-ref tool 'inputSchema)
                     '((type . "object")
                       (properties . ())
                       (required . #())))))
    `((type . "function")
      (function . ((name . ,(normalize-tool-name name))
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
        (if (or (not (string? text)) (string-null? text))
          '()
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
                         (parsed
                           (catch #t
                             (lambda ()
                               (json-string->scm json-str))
                             (lambda _
                               #f))))
                      (if
                          (and (pair? parsed) (or (assoc-ref parsed "name") (assoc-ref parsed 'name)))
                        (let*
                            ((raw-name (or (assoc-ref parsed "name") (assoc-ref parsed 'name)))
                             (raw-args (or (assoc-ref parsed "arguments") (assoc-ref parsed 'arguments) '()))
                             (args
                               (if (string? raw-args)
                                 (catch #t
                                   (lambda ()
                                     (json-string->scm raw-args))
                                   (lambda _
                                     raw-args))
                                 raw-args))
                             (call-id (format #f "call_xml_~a" (+ call-counter 1)))
                             (tool-call ` ((id . ,call-id) (name . ,raw-name) (arguments . ,args))))
                          (loop rest
                                (cons tool-call calls)
                                (+ call-counter 1)))
                        (loop rest calls call-counter)))
                    (reverse calls)))
                (reverse calls))))))

(define (parse-tool-calls raw-response content-text)
  "Extract tool calls from RAW-RESPONSE (native OpenAI format) or CONTENT-TEXT (XML tags)."
  (let ((native-calls (extract-chat-tool-calls raw-response)))
    (if (and (pair? native-calls) (not (null? native-calls)))
        native-calls
        (parse-xml-tool-calls content-text))))

;;;
;;; Tool Execution & Guardrail Enforcement
;;;
(define* (execute-tool-call tool-call reg mcp-mgr #:key (jrn #f) (mgr #f) (guardrail #f))
  "Execute a single TOOL-CALL alist. Checks GUARDRAIL, dispatches to REG or MCP-MGR,
and logs provenance to JRN if provided. Returns (values result-scm is-error?)."
  (let*
      ((id (assoc-ref tool-call 'id))
       (name (assoc-ref tool-call 'name))
       (args (or (assoc-ref tool-call 'arguments) '()))
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
        (values `((error .
                         ,(format #f "Permission denied for tool '~a' by guardrail policy" name)))
                #t))
      ((and (pair? permission) (eq? (car permission) 'deny))
        (values ` ((error . ,(cdr permission))) #t))
      (else ;; Execute tool
        (catch #t
          (lambda ()
            (let
                ((res
                   (cond
                     ;; 1. Check registry features (exact match or with dots/dashes restored)
                     ((and reg (lookup-registry-feature reg name))
                       (let ((impl (lookup-registry-feature reg name)))
                         (if (procedure? impl)
                           (cond
                             ((null? args) (impl))
                             ((and (pair? args) (pair? (car args)))
                               ;; args is an alist: try applying matching parameters based on metadata, or positional values, or pass alist
                               (catch #t
                                 (lambda ()
                                   (let*
                                       ((meta (lookup-registry-metadata reg name))
                                        (params
                                          (and meta (or (assoc-ref meta 'parameters) (assoc-ref meta "parameters"))))
                                        (props
                                          (and (pair? params)
                                               (or (assoc-ref params 'properties) (assoc-ref params "properties"))))
                                        (ordered-keys
                                          (if (pair? props)
                                            (map
                                              (lambda (p)
                                                (if (symbol? (car p))
                                                  (symbol->string (car p))
                                                  (car p)))
                                              props)
                                            #f)))
                                     (if (and ordered-keys (pair? ordered-keys))
                                       (let
                                           ((ordered-vals
                                              (map
                                                (lambda (k)
                                                  (or (assoc-ref args k) (assoc-ref args (string->symbol k))))
                                                ordered-keys)))
                                         (apply impl ordered-vals))
                                       (apply impl (map cdr args)))))
                                 (lambda _
                                   (impl args))))
                             ((list? args) (apply impl args))
                             (else (impl args)))
                           (error "Registry entry is not a procedure" name))))
                     ;; 2. Check MCP manager
                     ((and mcp-mgr
                           (let
                               ((all-tools (mcp-manager-list-tools mcp-mgr)))
                             (find
                               (lambda (t)
                                 (let
                                     ((tname (or (assoc-ref t "name") (assoc-ref t 'name))))
                                   (or (string=? tname name) (string=? (normalize-tool-name tname) name))))
                               all-tools)))
                       =>
                       (lambda (matched)
                         (let
                             ((orig-name (or (assoc-ref matched "name") (assoc-ref matched 'name))))
                           (mcp-manager-call-tool mcp-mgr orig-name args))))
                     (else
                       (error (format #f "Tool '~a' not found in registry or active MCP servers" name))))))
              ;; Log to journal if attached
              (when (and jrn (journal? jrn))
                (catch #t
                  (lambda ()
                    (journal-append! jrn
                                     `((feature-id . ,name)
                                       (checkpoint . ,(format #f "tool:~a" name))
                                       (status . "success")
                                       (arguments . ,args)
                                       (result-summary . ,(object->string res)))))
                  (lambda _
                    #f)))
              (values res #f)))
          (lambda (key . err-args)
            (let
                ((err-msg (format #f "Execution of tool '~a' failed (~a): ~s" name key err-args)))
              (when (and jrn (journal? jrn))
                (catch #t
                  (lambda ()
                    (journal-append! jrn
                                     `((feature-id . ,name)
                                       (checkpoint . ,(format #f "tool:~a" name))
                                       (status . "error")
                                       (arguments . ,args)
                                       (error . ,err-msg))))
                  (lambda _
                    #f)))
              (values ` ((error . ,err-msg)) #t))))))))

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
                                         compactor)
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
                    (compactor react-session-compactor set-react-session-compactor!))

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
                 raw-hooks)))
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
            compactor)))

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
                 ((and (string? reasoning) (not (string-null? (string-trim-both reasoning))))
                   (format #f
                           "<think>~a</think>
(Response provided in model reasoning trace)"
                           reasoning))
                 ((and (string? raw-content)
                       (not (member raw-content '("null" ":null")))
                       (not (string-null? (string-trim-both raw-content))))
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
         (if
             (and (string? reasoning) (not (string-null? (string-trim-both reasoning))))
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

(define
  (execute-step-tools tool-calls
                      reg
                      mcp-mgr
                      jrn
                      mgr
                      guardrail-fn
                      on-tool-call-fn
                      on-tool-result-fn)
  "Execute TOOL-CALLS sequentially, invoking callbacks and returning pairs of (tool-call . tool-msg)."
  (map
    (lambda (tc)
      (when (procedure? on-tool-call-fn) (on-tool-call-fn tc))
      (call-with-values
        (lambda ()
          (execute-tool-call tc reg mcp-mgr #:jrn jrn #:mgr mgr #:guardrail guardrail-fn))
        (lambda (res err?)
          (when (procedure? on-tool-result-fn) (on-tool-result-fn tc res err?))
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
             (on-tool-result-fn (assoc-ref hooks 'on-tool-result)))
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
                           raw-msgs))
                       (raw-resp
                         (model-complete client msgs #:tools tools #:max-tokens max-tokens #:raw? #t)))
                    (call-with-values
                      (lambda ()
                        (determine-step-content raw-resp))
                      (lambda (content reasoning)
                        (let
                            ((tool-calls (parse-tool-calls raw-resp content)))
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
                                (values 'continue results)))))))))
                (lambda (key . args)
                  (let
                      ((err-msg (format #f "ReAct loop error (~a): ~s" key args)))
                    (set-react-session-status! session 'error)
                    (values 'error err-msg))))))))

(define* (react-run! session #:key (on-finish #f))
  "Execute the ReAct session loop until completion, error, or max iterations."
  (let loop ()
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
           (values 'error payload session)))))))
