;;; (geasa surfaces-acp plugin) — Agent Client Protocol (ACP) editor surface
;;;
;;; Exposes badb's kernel and geis-gate loop to editors over the Agent
;;; Client Protocol: JSON-RPC 2.0 carried as one JSON object per line over a
;;; pair of ports (stdio by default).
;;;
;;; Client -> agent requests: initialize, authenticate, session/new,
;;; session/prompt, session/set_mode; the session/cancel notification asks
;;; for the running turn to stop.  Agent -> client traffic: session/update
;;; notifications (agent_message_chunk, agent_thought_chunk, tool_call,
;;; tool_call_update) and session/request_permission requests.
;;;
;;; Each ACP session owns a badb ReAct session: a registry carrying the
;;; built-in tools (including the kernel-owned spec gate tools), a journal, a
;;; transaction manager and a steering queue.  The ReAct hooks bridge the
;;; runtime to the protocol, so an editor sees the same trajectory the TUI
;;; renders: streamed deltas become session updates, tool calls become
;;; tool_call notifications, and a cancellation pushes an interrupt onto the
;;; steering queue so the turn stops at the next iteration boundary.

(define-module (geasa surfaces-acp plugin)
               #:use-module
               (badb kernel ipc)
               #:use-module
               (badb kernel journal)
               #:use-module
               (badb kernel registry)
               #:use-module
               (badb kernel transaction)
               #:use-module
               (geasa capabilities-mcp plugin)
               #:use-module
               (geasa foundation-config plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (geasa providers-model common)
               #:use-module
               (geasa runtimes-react plugin)
               #:use-module
               (geasa runtimes-rollback plugin)
               #:use-module
               (geasa runtimes-verify plugin)
               #:use-module
               (geasa surfaces-acp client)
               #:use-module
               (geasa surfaces-tui tools)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 exceptions)
               #:use-module
               (ice-9 threads)
               #:use-module
               (json)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-13)
               #:use-module
               (srfi srfi-9)
               #:re-export
               (<acp-client> make-acp-client
                             acp-client?
                             acp-client-in-port
                             acp-client-out-port
                             acp-client-pid
                             acp-client-closed?
                             acp-client-initialized?
                             acp-client-protocol-version
                             acp-client-server-capabilities
                             acp-client-server-info
                             acp-client-session-id
                             acp-spawn-client
                             acp-connect-tcp
                             acp-connect-unix
                             acp-client-initialize!
                             acp-client-new-session!
                             acp-client-handshake!
                             acp-client-prompt!
                             acp-client-cancel!
                             acp-client-close!)
               #:export
               (<acp-server> <acp-session>
                             make-acp-server
                             acp-server?
                             acp-server-sessions
                             acp-server-notifier
                             set-acp-server-notifier!
                             acp-server-in-port
                             acp-server-out-port
                             acp-server-lock
                             acp-server-pending
                             acp-server-condition-variable
                             acp-server-client
                             acp-server-workspace
                             acp-server-permission-policy
                             acp-server-client-capabilities
                             set-acp-server-client-capabilities!
                             acp-session?
                             acp-session-id
                             acp-session-cwd
                             acp-session-client
                             acp-session-reg
                             acp-session-react
                             acp-session-steering
                             acp-session-cancel?
                             set-acp-session-cancel?!
                             acp-session-find
                             acp-protocol-version
                             acp-agent-name
                             acp-agent-version
                             acp-notification
                             acp-session-update
                             acp-message-chunk
                             acp-thought-chunk
                             acp-tool-call-notification
                             acp-tool-result-notification
                             acp-tool-kind
                             acp-prompt-text
                             acp-handle-request
                             acp-handle-notification
                             acp-handle-message
                             acp-model-client
                             acp-scope-registry-to-workspace!
                             acp-filter-registry-tools!
                             acp-new-session
                             acp-run-prompt
                             acp-cancel-session!
                             acp-serve-stdio
                             run-acp-server
                             acp-client-request!
                             acp-route-response!))

;;;
;;; Protocol constants
;;;
(define acp-protocol-version 1)
(define acp-agent-name "badb")
(define acp-agent-version "0.1.0")

;; Seconds an agent -> client request waits for the client's answer.
(define %acp-client-request-timeout 30)

;;;
;;; Records
;;;
(define-record-type <acp-server>
                    (%make-acp-server sessions notifier in-port out-port lock pending cv
                                      next-session-id next-client-id client workspace
                                      permission-policy client-capabilities)
                    acp-server?
                    (sessions acp-server-sessions)
                    (notifier acp-server-notifier set-acp-server-notifier!)
                    (in-port acp-server-in-port)
                    (out-port acp-server-out-port)
                    (lock acp-server-lock)
                    (pending acp-server-pending)
                    (cv acp-server-condition-variable)
                    (next-session-id acp-server-next-session-id set-acp-server-next-session-id!)
                    (next-client-id acp-server-next-client-id set-acp-server-next-client-id!)
                    (client acp-server-client)
                    (workspace acp-server-workspace)
                    (permission-policy acp-server-permission-policy)
                    (client-capabilities acp-server-client-capabilities set-acp-server-client-capabilities!))

(define-record-type <acp-session>
                    (%make-acp-session id cwd client reg react steering cancel? streamed?)
                    acp-session?
                    (id acp-session-id)
                    (cwd acp-session-cwd)
                    (client acp-session-client)
                    (reg acp-session-reg)
                    (react acp-session-react)
                    (steering acp-session-steering)
                    (cancel? acp-session-cancel? set-acp-session-cancel?!)
                    (streamed? acp-session-streamed? set-acp-session-streamed?!))

;;;
;;; JSON-RPC framing
;;;
(define (acp-notification method params)
        "Build a JSON-RPC 2.0 notification frame for METHOD carrying PARAMS."
        `((jsonrpc . "2.0") (method . ,method) (params . ,params)))

(define (acp-result-frame id result)
        "Build a JSON-RPC 2.0 response frame carrying RESULT for request ID."
        `((jsonrpc . "2.0") (id . ,id) (result . ,result)))

(define (acp-error-frame id code message)
        "Build a JSON-RPC 2.0 error response frame for request ID."
        `((jsonrpc . "2.0") (id . ,id) (error . ((code . ,code) (message . ,message)))))

(define (acp-write-frame! server frame)
        "Write FRAME to the server output port as one JSON line. Never raises."
        (with-mutex (acp-server-lock server)
                    (ipc-write-response! frame (acp-server-out-port server))))

(define
  (acp-default-notifier server notification)
  "Default notifier: write NOTIFICATION to the server output port."
  (acp-write-frame! server notification))

(define (acp-notify! server notification)
        "Deliver NOTIFICATION through the server notifier; never raises."
        (let ((notifier (acp-server-notifier server)))
          (when (procedure? notifier)
            (catch #t
              (lambda () (notifier server notification))
              (lambda _ #f)))
          #t))

;;;
;;; Session updates
;;;
(define (acp-session-update session-id update)
        "Build the session/update notification carrying UPDATE for SESSION-ID."
        (acp-notification "session/update"
                          `((sessionId . ,session-id) (update . ,update))))

(define (acp-message-chunk session-id text)
        "Build the agent_message_chunk session update carrying TEXT."
        (acp-session-update session-id
                            `((sessionUpdate . "agent_message_chunk")
                              (content . ((type . "text") (text . ,text))))))

(define (acp-thought-chunk session-id text)
        "Build the agent_thought_chunk session update carrying TEXT."
        (acp-session-update session-id
                            `((sessionUpdate . "agent_thought_chunk")
                              (content . ((type . "text") (text . ,text))))))

;;;
;;; Tool call mapping
;;;
(define %acp-tool-kinds
        '(("read_file" . "read") ("fs-read-file" . "read")
          ("symbols_overview" . "read") ("read_symbol" . "read")
          ("memory_read" . "read") ("skill_load" . "read")
          ("ctx_search" . "search") ("find_symbol" . "search") ("skill_list" . "search")
          ("write_file" . "edit") ("edit_file" . "edit") ("fs-write-file" . "edit")
          ("fs-edit-file" . "edit") ("replace_symbol_body" . "edit") ("memory_write" . "edit")
          ("bash" . "execute") ("sh" . "execute") ("execute_command" . "execute")
          ("ctx_exec" . "execute") ("ctx_batch_exec" . "execute") ("scheme_eval" . "execute")))

(define (acp-tool-kind name)
        "Map tool NAME to its ACP tool kind: read, search, edit, execute or other."
        (let ((entry (and (string? name) (assoc name %acp-tool-kinds))))
          (if entry (cdr entry) "other")))

(define (acp-tool-call-id tool-call)
        "Return the ACP tool call id for TOOL-CALL, deriving one when it has none."
        (let ((id (prop-ref tool-call 'id)))
          (if (and (string? id) (not (string-null? id)))
            id
            (format #f "call_~a" (or (prop-ref tool-call 'name) "tool")))))

(define (acp-tool-call-notification session-id tool-call)
        "Build the tool_call session update announcing TOOL-CALL."
        (let* ((name (or (prop-ref tool-call 'name) ""))
               (args (or (prop-ref tool-call 'arguments) '())))
          (acp-session-update session-id
                              `((sessionUpdate . "tool_call")
                                (toolCallId . ,(acp-tool-call-id tool-call))
                                (title . ,name)
                                (kind . ,(acp-tool-kind name))
                                (status . "in_progress")
                                (rawInput . ,args)))))

(define (acp-result-text result)
        "Render tool RESULT as text for the protocol."
        (cond ((string? result) result)
              ((null? result) "")
              (else (safe-json-string result))))

(define (acp-tool-result-notification session-id tool-call result err?)
        "Build the tool_call_update session update carrying RESULT for TOOL-CALL."
        (let ((status (if err? "failed" "completed")))
          (acp-session-update session-id
                              `((sessionUpdate . "tool_call_update")
                                (toolCallId . ,(acp-tool-call-id tool-call))
                                (status . ,status)
                                (content . #(((type . "text")
                                              (text . ,(acp-result-text result)))))))))

;;;
;;; Prompt payloads
;;;
(define (acp-prompt-text blocks)
        "Extract the plain text of an ACP prompt BLOCKS payload.
BLOCKS is a list or vector of content blocks, each with a text or content field."
        (cond
          ((string? blocks) blocks)
          ((not (or (pair? blocks) (vector? blocks))) "")
          (else
            (string-join
              (map
                (lambda (block)
                  (cond
                    ((string? block) block)
                    ((not (pair? block)) "")
                    ((string? (prop-ref block 'text)) (prop-ref block 'text))
                    ((string? (prop-ref block 'content)) (prop-ref block 'content))
                    (else "")))
                (if (vector? blocks)
                  (vector->list blocks)
                  blocks))
              "\n"))))

;;;
;;; Server construction
;;;
(define* (make-acp-server #:key
                          (in-port (current-input-port))
                          (out-port (current-output-port))
                          (client #f)
                          (workspace (getcwd))
                          (permission-policy 'allow)
                          (notifier #f)
                          (client-capabilities '()))
         "Create an ACP server bound to IN-PORT and OUT-PORT.
CLIENT is the model client used by new sessions, PERMISSION-POLICY is 'allow
or 'ask, and NOTIFIER receives every session/update notification."
         (let* ((server (%make-acp-server (make-hash-table 31)
                                          notifier
                                          in-port
                                          out-port
                                          (make-mutex)
                                          (make-hash-table 31)
                                          (make-condition-variable)
                                          0
                                          0
                                          client
                                          workspace
                                          permission-policy
                                          client-capabilities)))
           (unless (procedure? notifier)
             (set-acp-server-notifier! server acp-default-notifier))
           server))

(define (acp-session-find server session-id)
        "Return the ACP session named SESSION-ID, or #f when there is none."
        (and (string? session-id)
             (hash-ref (acp-server-sessions server) session-id #f)))

(define (acp-load-config)
        "Load the badb configuration, degrading to defaults when it cannot be read."
        (catch #t (lambda () (load-config)) (lambda _ #f)))

(define* (acp-model-client #:key (client #f) (endpoint #f) (model #f) (api-key #f) (config #f))
         "Return the model client for the ACP surface, built from CONFIG and env."
         (or client
             (let* ((ep (or endpoint
                            (getenv "BADB_MODEL_ENDPOINT")
                            (and config (badb-config-model-endpoint config))))
                    (md (or model
                            (getenv "BADB_MODEL_NAME")
                            (and config (badb-config-model-name config))))
                    (key (or api-key
                             (getenv "BADB_API_KEY")
                             (getenv "OPENAI_API_KEY")
                             (and config (badb-config-model-api-key config)))))
               (apply make-model-client
                      (append (if ep (list #:endpoint ep) '())
                              (if key (list #:api-key key) '())
                              (if md (list #:model md) '()))))))

;;;
;;; Session construction
;;;
;; DEV-DEBT [mcp-session-servers]: session/new's mcpServers entries are
;; accepted but not auto-connected (ACP-SESSION-004, ADVISORY). The session's
;; MCP manager only carries servers from the badb config; wiring per-session
;; stdio servers requires workspace-aware mcp-manager-start-server! calls at
;; session construction time.
(define (%acp-shell-quote s)
        (string-append "'"
                       (list->string
                         (fold-right append
                                     '()
                                     (map
                                       (lambda (c)
                                         (if (eqv? c #\')
                                           (list #\' #\\ #\' #\')
                                           (list c)))
                                       (string->list s))))
                       "'"))

(define (%acp-resolve-rel-path root p)
        (cond
          ((not (string? p)) p)
          ((string-null? p) root)
          ((string-prefix? "/" p) p)
          (else (string-append root "/" p))))

(define
  (%acp-rewrite-alist-key alist keys transform)
  (map
    (lambda (pair)
      (if (and (pair? pair) (member (car pair) keys))
        (cons (car pair) (transform (cdr pair)))
        pair))
    alist))

(define (%acp-wrap-path-tool reg name root)
        (let
            ((orig (registry-lookup reg name)) (meta (registry-metadata reg name)))
          (when orig
            (registry-attach! reg
                              name
                              (lambda args
                                (if (and (= (length args) 1) (list? (car args)))
                                  (apply orig
                                         (list
                                           (%acp-rewrite-alist-key (car args)
                                                                   '("path" path "file" file)
                                                                   (lambda (p)
                                                                     (%acp-resolve-rel-path root p)))))
                                  (apply orig
                                         (if (and (pair? args) (string? (car args)))
                                           (cons (%acp-resolve-rel-path root (car args)) (cdr args))
                                           args))))
                              #:metadata
                              (or meta '())))))

(define (%acp-wrap-shell-tool reg name root)
        (let
            ((orig (registry-lookup reg name))
             (meta (registry-metadata reg name))
             (prefix (string-append "cd " (%acp-shell-quote root) " && ")))
          (when orig
            (registry-attach! reg
                              name
                              (lambda args
                                (if (and (= (length args) 1) (list? (car args)))
                                  (let*
                                      ((a (car args))
                                       (has-cmd?
                                         (or (assoc "command" a) (assoc 'command a) (assoc "cmd" a) (assoc 'cmd a))))
                                    (apply orig
                                           (list
                                             (if has-cmd?
                                               (%acp-rewrite-alist-key a
                                                                       '("command" command "cmd" cmd)
                                                                       (lambda (c)
                                                                         (if (string? c)
                                                                           (string-append prefix c)
                                                                           c)))
                                               a))))
                                  (apply orig
                                         (if (and (pair? args) (string? (car args)))
                                           (cons (string-append prefix (car args)) (cdr args))
                                           args))))
                              #:metadata
                              (or meta '())))))

(define
  (acp-scope-registry-to-workspace! reg root)
  "Scope relative-path filesystem and shell tools in REG to workspace ROOT."
  (let
      ((clean-root
         (if (file-exists? root)
           (canonicalize-path root)
           root)))
    (for-each
      (lambda (name)
        (%acp-wrap-path-tool reg name clean-root))
      '("read_file" "fs-read-file"
        "write_file"
        "fs-write-file"
        "edit_file"
        "fs-edit-file"
        "edit_lines"
        "fs-edit-lines"
        "file_search"
        "symbols_overview"
        "find_symbol"
        "read_symbol"
        "replace_symbol_body"))
    (for-each
      (lambda (name)
        (%acp-wrap-shell-tool reg name clean-root))
      '("bash" "sh" "execute_command" "ctx_exec"))
    reg))

(define
  (acp-filter-registry-tools! reg allowed-tools)
  "Detach any tool in REG whose name is not in ALLOWED-TOOLS."
  (let
      ((allowed
         (map
           (lambda (x)
             (if (symbol? x)
               (symbol->string x)
               x))
           allowed-tools)))
    (for-each
      (lambda (entry)
        (let*
            ((raw-id (assoc-ref entry 'id))
             (name
               (if (symbol? raw-id)
                 (symbol->string raw-id)
                 raw-id)))
          (unless (member name allowed) (registry-detach! reg raw-id))))
      (registry-list reg))
    reg))

(define*
  (acp-new-session server
                   #:key
                   (cwd #f)
                   (client #f)
                   (id #f)
                   (allowed-tools #f)
                   (max-iterations #f)
                   (system-prompt #f)
                   (journal-path #f)
                   (workspace-scope? #t))
  "Create an ACP session rooted at CWD and register it with SERVER."
  (let*
      ((session-id
         (or id
             (format #f
                     "sess_~a"
                     (let ((n (acp-server-next-session-id server)))
                       (set-acp-server-next-session-id! server (+ n 1))
                       n))))
       (root (or cwd (acp-server-workspace server)))
       (model-client (or client (acp-server-client server)))
       (reg (make-registry))
       (jrn
         (make-journal (or journal-path (string-append root "/.badb/journal.jsonl"))))
       (mgr (make-transaction-manager reg #:journal jrn))
       (rollback-mgr
         (make-rollback-manager #:workspace-dir root #:registry reg #:journal jrn))
       (verifier (make-verifier))
       (mcp-mgr (make-mcp-manager (acp-load-config)))
       (steering (make-steering-queue))
       (sid session-id)
       (initial-msgs
         (if
             (and (string? system-prompt) (not (string-null? system-prompt)))
           (list `((role . "system") (content . ,system-prompt)))
           '())))
    (register-builtin-tui-tools! reg
                                 rollback-mgr
                                 verifier
                                 mcp-mgr
                                 (lambda ()
                                   '())
                                 #:client
                                 model-client
                                 #:workspace
                                 root)
    (when
        (and workspace-scope?
             (string? root)
             (file-exists? root)
             (not (string=? (canonicalize-path root) (canonicalize-path (getcwd)))))
      (acp-scope-registry-to-workspace! reg root))
    (when allowed-tools (acp-filter-registry-tools! reg allowed-tools))
    (let*
        ((react
           (make-react-session #:client
                               model-client
                               #:initial-messages
                               initial-msgs
                               #:reg
                               reg
                               #:jrn
                               jrn
                               #:mgr
                               mgr
                               #:steering-queue
                               steering
                               #:max-iterations
                               max-iterations
                               #:hooks
                               (list
                                 (cons 'on-delta
                                       (lambda (payload type)
                                         (acp-on-delta server sid payload type)))
                                 (cons 'on-tool-call
                                       (lambda (tc)
                                         (acp-on-tool-call server sid tc)))
                                 (cons 'on-tool-result
                                       (lambda (tc res err?)
                                         (acp-on-tool-result server sid tc res err?)))
                                 (cons 'intercept-tool-call
                                       (lambda (tc _sess)
                                         (acp-permission-veto server sid tc)))))))
      (let
          ((session
             (%make-acp-session session-id root model-client reg react steering #f #f)))
        (hash-set! (acp-server-sessions server) session-id session)
        session))))

;;;
;;; Runtime -> protocol bridge
;;;
(define
  (acp-on-delta server session-id payload type)
  "Forward a streamed ReAct delta to the client as a session update."
  (if (string? payload)
    (cond
      ((eq? type 'content)
        (let
            ((sess (acp-session-find server session-id)))
          (when (and sess (not (string-null? payload)))
            (set-acp-session-streamed?! sess #t)))
        (acp-notify! server (acp-message-chunk session-id payload)))
      ((eq? type 'thought)
        (acp-notify! server (acp-thought-chunk session-id payload)))
      (else #t))
    #t))

(define (acp-on-tool-call server session-id tool-call)
        "Announce TOOL-CALL to the client."
        (acp-notify! server (acp-tool-call-notification session-id tool-call)))

(define (acp-on-tool-result server session-id tool-call result err?)
        "Report the RESULT of TOOL-CALL to the client."
        (acp-notify! server (acp-tool-result-notification session-id tool-call result err?)))

(define
  (acp-permission-veto server session-id tool-call)
  "Return a veto string when the client denies TOOL-CALL, otherwise #f."
  (if
      (eq? (acp-server-permission-policy server) 'ask)
    (if
        (eq? (acp-request-permission! server session-id tool-call) 'allow)
      #f
      "the client denied this tool call")
    #f))

;;;
;;; Agent -> client requests
;;;
(define
  (acp-client-request! server method params)
  "Send a request to the client and wait for the matching response.
Returns the response message, or #f when the client does not answer in time.
The request id is a string so it can never collide with a client request id."
  (let*
      ((id
         (let ((n (acp-server-next-client-id server)))
           (set-acp-server-next-client-id! server (+ n 1))
           (format #f "acp-~a" n)))
       (cell (list #f))
       (lock (acp-server-lock server))
       (cv (acp-server-condition-variable server)))
    (lock-mutex lock)
    (hash-set! (acp-server-pending server) id cell)
    (unlock-mutex lock)
    (acp-write-frame! server
                      `((jsonrpc . "2.0") (id . ,id) (method . ,method) (params . ,params)))
    (let loop
      ((deadline (+ (car (gettimeofday)) %acp-client-request-timeout)))
      (lock-mutex lock)
      (let ((value (car cell)))
        (if value
          (begin (unlock-mutex lock) value)
          (if (>= (car (gettimeofday)) deadline)
            (begin (hash-remove! (acp-server-pending server) id) (unlock-mutex lock) #f)
            (begin (wait-condition-variable cv lock deadline)
                   (unlock-mutex lock)
                   (loop deadline))))))))

(define (acp-route-response! server message)
        "Deliver a client RESPONSE to the thread waiting for it; #t when routed."
        (let*
            ((id (prop-ref message 'id))
             (cell (and (string? id) (hash-ref (acp-server-pending server) id #f))))
          (if cell
            (begin
              (with-mutex (acp-server-lock server)
                          (set-car! cell message)
                          (signal-condition-variable (acp-server-condition-variable server)))
              (hash-remove! (acp-server-pending server) id)
              #t)
            #f)))

(define
  (acp-request-permission! server session-id tool-call)
  "Ask the client whether TOOL-CALL may run; return 'allow or 'deny.
A missing or unanswered request denies the call: the gate fails closed."
  (let*
      ((name (or (prop-ref tool-call 'name) ""))
       (params
         `((sessionId . ,session-id)
           (toolCall .
                     ((toolCallId . ,(acp-tool-call-id tool-call))
                      (title . ,name)
                      (kind . ,(acp-tool-kind name))
                      (rawInput . ,(or (prop-ref tool-call 'arguments) '()))))
           (options .
                    #(((optionId . "allow") (name . "Allow") (kind . "allow_once"))
                      ((optionId . "reject") (name . "Reject") (kind . "reject_once"))))))
       (response (acp-client-request! server "session/request_permission" params))
       (result (and response (prop-ref response "result")))
       (outcome (and result (prop-ref result "outcome")))
       (kind (and outcome (prop-ref outcome "outcome")))
       (option (and outcome (prop-ref outcome "optionId"))))
    (if
        (and (string? kind)
             (string=? kind "selected")
             (string? option)
             (string=? option "allow"))
      'allow
      'deny)))

;;;
;;; Prompt execution
;;;
(define (acp-run-prompt server session blocks)
        "Run one ACP prompt turn in SESSION; return the ACP stop reason."
        (let*
            ((react (acp-session-react session)) (text (acp-prompt-text blocks)))
          (set-acp-session-cancel?! session #f)
          (set-acp-session-streamed?! session #f)
          (set-react-session-messages! react
                                       (append (react-session-messages react)
                                               (list `((role . "user") (content . ,text)))))
          (let
              ((verdict
                 (catch #t
                   (lambda ()
                     (call-with-values
                       (lambda ()
                         (react-run! react))
                       (lambda (status content final)
                         (when
                             (and (not (acp-session-streamed? session))
                                  (string? content)
                                  (not (string-null? content)))
                           (acp-notify! server (acp-message-chunk (acp-session-id session) content)))
                         status)))
                   (lambda (key . args)
                     'error))))
            (cond
              ((eq? verdict 'completed) "end_turn")
              ((eq? verdict 'interrupted) "cancelled")
              ((eq? verdict 'max-iterations-reached) "max_turn_requests")
              ((eq? verdict 'loop-detected) "refusal")
              (else "refusal")))))

(define (acp-cancel-session! server session)
        "Interrupt the running turn of SESSION; #t when a session was cancelled."
        (if session
          (begin (set-acp-session-cancel?! session #t)
                 (let ((queue (acp-session-steering session)))
                   (when (steering-queue? queue) (steering-push! queue 'interrupt)))
                 #t)
          #f))

;;;
;;; Request handling
;;;
(define (acp-initialize-result server params)
        "Build the initialize result, recording the client's capabilities."
        (set-acp-server-client-capabilities! server
                                             (or (prop-ref params "clientCapabilities") '()))
        `((protocolVersion . ,acp-protocol-version)
          (agentCapabilities .
                             ((loadSession . #f)
                              (promptCapabilities . ((image . #f) (audio . #f) (embeddedContext . #t)))))
          (agentInfo . ((name . ,acp-agent-name) (version . ,acp-agent-version)))
          (authMethods . #())))

(define (acp-new-session-frame server id params)
        "Build the session/new response frame for request ID."
        (let
            ((cwd (prop-ref params "cwd"))
             (raw-allowed (prop-ref params "allowedTools"))
             (max-iter (prop-ref params "maxIterations"))
             (sys-prompt (prop-ref params "systemPrompt"))
             (jrn-path (prop-ref params "journalPath")))
          (if
              (and (string? cwd) (not (string-null? (string-trim-both cwd))))
            (let*
                ((allowed-tools
                   (cond
                     ((vector? raw-allowed) (vector->list raw-allowed))
                     ((list? raw-allowed) raw-allowed)
                     (else #f)))
                 (session
                   (catch #t
                     (lambda ()
                       (acp-new-session server
                                        #:cwd
                                        cwd
                                        #:allowed-tools
                                        allowed-tools
                                        #:max-iterations
                                        (and (integer? max-iter) max-iter)
                                        #:system-prompt
                                        (and (string? sys-prompt) sys-prompt)
                                        #:journal-path
                                        (and (string? jrn-path) jrn-path)))
                     (lambda (key . args)
                       #f))))
              (if session
                (acp-result-frame id
                                  `((sessionId . ,(acp-session-id session))
                                    (modes .
                                           ((currentModeId . "default")
                                            (availableModes . #(((id . "default") (name . "Default"))))))))
                (acp-error-frame id -32603 "could not create a session for this workspace")))
            (acp-error-frame id -32602 "session/new requires a non-empty cwd string"))))

(define (acp-prompt-frame server id params)
        "Build the session/prompt response frame for request ID."
        (let
            ((session (acp-session-find server (prop-ref params "sessionId"))))
          (if session
            (acp-result-frame id
                              `((stopReason .
                                            ,(acp-run-prompt server session (or (prop-ref params "prompt") '())))))
            (acp-error-frame id
                             -32602
                             (format #f "unknown session: ~a" (prop-ref params "sessionId"))))))

(define (acp-set-mode-frame server id params)
        "Build the session/set_mode response frame for request ID."
        (let*
            ((session-id (prop-ref params "sessionId"))
             (session (acp-session-find server session-id)))
          (if session
            (let
                ((mode (or (prop-ref params "modeId") "default")))
              (if (string=? mode "default")
                (acp-result-frame id '())
                (acp-error-frame id -32602 (format #f "unknown mode: ~a" mode))))
            (acp-error-frame id -32602 (format #f "unknown session: ~a" session-id)))))

(define (acp-cancel-frame server id params)
        "Build the session/cancel response frame for request ID."
        (let* ((session-id (prop-ref params "sessionId"))
               (session (acp-session-find server session-id)))
          (if session
            (begin
              (acp-cancel-session! server session)
              (acp-result-frame id '()))
            (acp-error-frame id -32602 (format #f "unknown session: ~a" session-id)))))

(define (acp-handle-request server id method params)
        "Handle a client request METHOD; return the JSON-RPC response frame."
        (cond
          ((string=? method "initialize")
            (acp-result-frame id (acp-initialize-result server params)))
          ((string=? method "authenticate")
            (acp-result-frame id '()))
          ((string=? method "session/new")
            (acp-new-session-frame server id params))
          ((string=? method "session/prompt")
            (acp-prompt-frame server id params))
          ((string=? method "session/set_mode")
            (acp-set-mode-frame server id params))
          ((string=? method "session/cancel")
            (acp-cancel-frame server id params))
          ((string=? method "session/load")
            (acp-error-frame id -32601 "session/load is not supported by this agent"))
          (else
            (acp-error-frame id -32601 (format #f "unknown method: ~a" method)))))

(define (acp-handle-notification server method params)
        "Handle a client notification; returns #f because notifications carry no reply."
        (cond
          ((string=? method "session/cancel")
            (let* ((session-id (prop-ref params "sessionId"))
                   (session (acp-session-find server session-id)))
              (acp-cancel-session! server session)))
          (else #f)))

(define (acp-handle-message server message)
        "Handle one decoded JSON-RPC MESSAGE; return a response frame or #f."
        (let*
            ((id (prop-ref message 'id))
             (method (or (prop-ref message "method") ""))
             (params (or (prop-ref message "params") '())))
          (if id
            (acp-handle-request server id method params)
            (acp-handle-notification server method params))))

;;;
;;; Serving
;;;
(define (acp-serve-message! server message)
        "Dispatch MESSAGE and write its response frame, when it has one."
        (let ((response (catch #t
                          (lambda () (acp-handle-message server message))
                          (lambda (key . args)
                            (acp-error-frame (prop-ref message 'id)
                                             -32603
                                             (format #f "~a: ~s" key args))))))
          (when (and (pair? response) (prop-ref response 'id))
            (acp-write-frame! server response))))

(define (acp-read-message in)
        "Read and decode the next JSON-RPC message from IN; #f at end of input."
        (let
            ((line
               (catch #t
                 (lambda ()
                   (read-line in))
                 (lambda _
                   #f))))
          (and line
               (not (eof-object? line))
               (not (string-null? (string-trim-both line)))
               (catch #t
                 (lambda ()
                   (json-string->scm (string-trim-both line)))
                 (lambda _
                   #f)))))

(define (acp-serve-incoming! server message)
        "Route MESSAGE: client responses wake the waiting agent, prompts run in
their own thread so a cancel can still be read, everything else is inline."
        (let ((id (prop-ref message 'id)))
          (cond
            ((not id) (acp-serve-message! server message))
            ((acp-route-response! server message) #t)
            ((string=? (or (prop-ref message "method") "") "session/prompt")
              (call-with-new-thread
                (lambda ()
                  (acp-serve-message! server message))))
            (else (acp-serve-message! server message)))))

(define (acp-serve-stdio server)
        "Serve the ACP protocol over the server input port until EOF.
A session/prompt request runs in its own thread so the reader keeps
answering session/cancel while a turn is in flight."
        (let ((in (acp-server-in-port server)))
          (let loop
            ()
            (let ((message (acp-read-message in)))
              (if message
                (begin (acp-serve-incoming! server message) (loop))
                #t)))))

;;;
;;; Entry point
;;;
(define* (run-acp-server #:key
                         (in-port (current-input-port))
                         (out-port (current-output-port))
                         (client #f)
                         (endpoint #f)
                         (model #f)
                         (api-key #f)
                         (workspace (getcwd))
                         (permission-policy 'allow))
         "Run the ACP editor surface over IN-PORT and OUT-PORT until EOF."
         (let* ((config (acp-load-config))
                (model-client (acp-model-client #:client client
                                                #:endpoint endpoint
                                                #:model model
                                                #:api-key api-key
                                                #:config config)))
           (acp-serve-stdio (make-acp-server #:in-port in-port
                                             #:out-port out-port
                                             #:client model-client
                                             #:workspace workspace
                                             #:permission-policy permission-policy))))
