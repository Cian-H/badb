;;; badb — Autonomous ReAct Tool-Calling Engine
;;;
;;; Implements the multi-turn Thought -> Action -> Observation -> Thought
;;; execution loop. Dynamically inspects available tools across both the
;;; kernel registry and active MCP servers, parses tool calls from both
;;; native API schemas and embedded text/XML formats, enforces guardrails,
;;; logs execution provenance to the journal, and drives agent tasks to completion.

(define-module (badb react)
  #:use-module (badb model)
  #:use-module (badb registry)
  #:use-module (badb journal)
  #:use-module (badb transaction)
  #:use-module (badb mcp)
  #:use-module (json)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-react-session
            react-session?
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
            set-react-session-messages!
            set-react-session-status!
            set-react-session-iteration!

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
  (if (not reg)
      #f
      (let loop ((candidates (denormalize-tool-name name)))
        (if (null? candidates)
            #f
            (or (registry-lookup reg (car candidates))
                (loop (cdr candidates)))))))

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
      (let loop ((remaining text)
                 (calls '())
                 (call-counter 0))
        (let ((start-tag (string-contains remaining "<tool_call>")))
          (if (not start-tag)
              (reverse calls)
              (let* ((after-start (substring remaining (+ start-tag (string-length "<tool_call>"))))
                     (end-tag (string-contains after-start "</tool_call>")))
                (if (not end-tag)
                    (reverse calls)
                    (let* ((json-str (string-trim-both (substring after-start 0 end-tag)))
                           (rest (substring after-start (+ end-tag (string-length "</tool_call>"))))
                           (parsed (catch #t
                                     (lambda () (json-string->scm json-str))
                                     (lambda _ #f))))
                      (if (and (pair? parsed) (or (assoc-ref parsed "name") (assoc-ref parsed 'name)))
                          (let* ((raw-name (or (assoc-ref parsed "name") (assoc-ref parsed 'name)))
                                 (raw-args (or (assoc-ref parsed "arguments") (assoc-ref parsed 'arguments) '()))
                                 (args (if (string? raw-args)
                                           (catch #t (lambda () (json-string->scm raw-args)) (lambda _ raw-args))
                                           raw-args))
                                 (call-id (format #f "call_xml_~a" (+ call-counter 1)))
                                 (tool-call `((id . ,call-id)
                                              (name . ,raw-name)
                                              (arguments . ,args))))
                            (loop rest (cons tool-call calls) (+ call-counter 1)))
                          (loop rest calls call-counter))))))))))

(define (parse-tool-calls raw-response content-text)
  "Extract tool calls from RAW-RESPONSE (native OpenAI format) or CONTENT-TEXT (XML tags)."
  (let ((native-calls (extract-chat-tool-calls raw-response)))
    (if (and (pair? native-calls) (not (null? native-calls)))
        native-calls
        (parse-xml-tool-calls content-text))))

;;;
;;; Tool Execution & Guardrail Enforcement
;;;

(define* (execute-tool-call tool-call reg mcp-mgr
                           #:key
                           (jrn #f)
                           (mgr #f)
                           (guardrail #f))
  "Execute a single TOOL-CALL alist. Checks GUARDRAIL, dispatches to REG or MCP-MGR,
and logs provenance to JRN if provided. Returns (values result-scm is-error?)."
  (let* ((id (assoc-ref tool-call 'id))
         (name (assoc-ref tool-call 'name))
         (args (or (assoc-ref tool-call 'arguments) '()))
         ;; Check guardrail policy
         (permission (if (procedure? guardrail)
                         (guardrail name args)
                         #t)))
    (cond
     ((or (eq? permission #f) (eq? permission 'deny))
      (values `((error . ,(format #f "Permission denied for tool '~a' by guardrail policy" name)))
              #t))
     ((and (pair? permission) (eq? (car permission) 'deny))
      (values `((error . ,(cdr permission)))
              #t))
     (else
      ;; Execute tool
      (catch #t
        (lambda ()
          (let ((res
                 (cond
                  ;; 1. Check registry features (exact match or with dots/dashes restored)
                  ((and reg (lookup-registry-feature reg name))
                   (let ((impl (lookup-registry-feature reg name)))
                     (if (procedure? impl)
                         (cond
                          ((null? args) (impl))
                          ((and (pair? args) (pair? (car args)))
                           ;; args is an alist: try applying positional values, or pass alist
                           (catch #t
                             (lambda () (apply impl (map cdr args)))
                             (lambda _ (impl args))))
                          ((list? args)
                           (apply impl args))
                          (else (impl args)))
                         (error "Registry entry is not a procedure" name))))

                  ;; 2. Check MCP manager
                  ((and mcp-mgr
                        (any (lambda (t)
                               (let ((tname (or (assoc-ref t "name") (assoc-ref t 'name))))
                                 (or (string=? tname name)
                                     (string=? (normalize-tool-name tname) name))))
                             (mcp-manager-list-tools mcp-mgr)))
                   (let* ((all-tools (mcp-manager-list-tools mcp-mgr))
                          (matched (find (lambda (t)
                                           (let ((tname (or (assoc-ref t "name") (assoc-ref t 'name))))
                                             (or (string=? tname name)
                                                 (string=? (normalize-tool-name tname) name))))
                                         all-tools))
                          (orig-name (or (assoc-ref matched "name") (assoc-ref matched 'name))))
                     (mcp-manager-call-tool mcp-mgr orig-name args)))

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
                (lambda _ #f)))

            (values res #f)))
        (lambda (key . err-args)
          (let ((err-msg (format #f "Execution of tool '~a' failed (~a): ~s" name key err-args)))
            (when (and jrn (journal? jrn))
              (catch #t
                (lambda ()
                  (journal-append! jrn
                                   `((feature-id . ,name)
                                     (checkpoint . ,(format #f "tool:~a" name))
                                     (status . "error")
                                     (arguments . ,args)
                                     (error . ,err-msg))))
                (lambda _ #f)))
            (values `((error . ,err-msg)) #t))))))))

;;;
;;; ReAct Session Record
;;;

(define-record-type <react-session>
  (%make-react-session messages status iteration max-iterations hooks client reg mcp-mgr jrn mgr)
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
  (mgr react-session-mgr))

(define* (make-react-session client
                             #:key
                             (initial-messages '())
                             (reg #f)
                             (mcp-mgr #f)
                             (jrn #f)
                             (mgr #f)
                             (max-iterations 10)
                             (hooks '()))
  "Construct a new <react-session> record."
  (%make-react-session (if (list? initial-messages) initial-messages (list initial-messages))
                       'idle
                       0
                       max-iterations
                       hooks
                       client
                       reg
                       mcp-mgr
                       jrn
                       mgr))

;;;
;;; Step & Run Cycle
;;;

(define (react-step! session)
  "Execute a single turn of the ReAct cycle.
Returns:
  (values 'completed content-string)
  (values 'continue list-of-tool-results)
  (values 'max-iterations-reached #f)
  (values 'error error-message)"
  (let* ((iter (react-session-iteration session))
         (max-iter (react-session-max-iterations session))
         (client (react-session-client session))
         (reg (react-session-reg session))
         (mcp-mgr (react-session-mcp-mgr session))
         (jrn (react-session-jrn session))
         (mgr (react-session-mgr session))
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
          (when (procedure? on-step-fn)
            (on-step-fn (+ iter 1) 'starting))

          (catch #t
            (lambda ()
              (let* ((tools (collect-available-tools reg mcp-mgr))
                     (msgs (react-session-messages session))
                     (raw-resp (model-complete client msgs #:tools tools #:raw? #t))
                     (content (or (extract-chat-content raw-resp) ""))
                     (tool-calls (parse-tool-calls raw-resp content)))

                (if (null? tool-calls)
                    ;; No tools invoked: task completed by the assistant
                    (let ((final-msg `((role . "assistant") (content . ,content))))
                      (set-react-session-messages! session (append msgs (list final-msg)))
                      (set-react-session-status! session 'completed)
                      (when (procedure? on-step-fn)
                        (on-step-fn (+ iter 1) 'completed))
                      (values 'completed content))

                    ;; Model requested tool calls
                    (begin
                      (set-react-session-status! session 'awaiting-tool)
                      (let* ((tool-calls-vec
                              (list->vector
                               (map (lambda (tc)
                                      (let* ((tc-id (or (assoc-ref tc 'id) (assoc-ref tc "id") "call_0"))
                                             (tc-name (or (assoc-ref tc 'name) (assoc-ref tc "name") ""))
                                             (raw-args (or (assoc-ref tc 'arguments) (assoc-ref tc "arguments") '()))
                                             (args-str (if (string? raw-args) raw-args (scm->json-string raw-args))))
                                        `((id . ,tc-id)
                                          (type . "function")
                                          (function . ((name . ,tc-name)
                                                       (arguments . ,args-str))))))
                                    tool-calls)))
                             (asst-msg `((role . "assistant")
                                         (content . ,(if (string-null? content) #f content))
                                         (tool_calls . ,tool-calls-vec)))
                             (new-msgs (append msgs (list asst-msg)))
                             (results
                              (map (lambda (tc)
                                     (when (procedure? on-tool-call-fn)
                                       (on-tool-call-fn tc))
                                     (call-with-values
                                         (lambda ()
                                           (execute-tool-call tc reg mcp-mgr
                                                              #:jrn jrn
                                                              #:mgr mgr
                                                              #:guardrail guardrail-fn))
                                       (lambda (res err?)
                                         (when (procedure? on-tool-result-fn)
                                           (on-tool-result-fn tc res err?))
                                         (let* ((content-str (if (string? res) res (scm->json-string res)))
                                                (tool-msg `((role . "tool")
                                                            (tool_call_id . ,(assoc-ref tc 'id))
                                                            (name . ,(assoc-ref tc 'name))
                                                            (content . ,content-str))))
                                           (cons tc tool-msg)))))
                                   tool-calls))
                             (tool-msgs (map cdr results))
                             (all-msgs (append new-msgs tool-msgs)))
                        (set-react-session-messages! session all-msgs)
                        (set-react-session-status! session 'running)
                        (when (procedure? on-step-fn)
                          (on-step-fn (+ iter 1) 'tools-executed))
                        (values 'continue results))))))
            (lambda (key . args)
              (let ((err-msg (format #f "ReAct loop error (~a): ~s" key args)))
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
