;;; badb — MCP Client (JSON-RPC 2.0 over stdio)
;;;
;;; Implements a Model Context Protocol (MCP) client communicating over
;;; standard I/O ports or spawned subprocesses using JSON-RPC 2.0.

(define-module (badb mcp)
  #:use-module (json)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (badb config)
  #:export (make-mcp-client
            mcp-client?
            mcp-client-in-port
            mcp-client-out-port
            mcp-client-pid
            mcp-client-next-id
            set-mcp-client-in-port!
            set-mcp-client-out-port!
            set-mcp-client-pid!
            set-mcp-client-next-id!
            mcp-spawn-client
            mcp-send-request!
            mcp-send-notification!
            mcp-read-message
            mcp-call
            mcp-notify
            mcp-initialize
            mcp-list-tools
            mcp-call-tool
            mcp-close-client
            json-ref
            tools->list

            make-mcp-manager
            mcp-manager?
            mcp-manager-config
            mcp-manager-servers
            set-mcp-manager-servers!
            set-mcp-manager-config!
            mcp-manager-list-servers
            mcp-manager-get-server
            mcp-manager-add-server!
            mcp-manager-remove-server!
            mcp-manager-start-server!
            mcp-manager-stop-server!
            mcp-manager-stop-all!
            mcp-manager-list-tools
            mcp-manager-call-tool))

;;;
;;; Client Record
;;;

(define-record-type <mcp-client>
  (%make-mcp-client in-port out-port pid next-id closed?)
  mcp-client?
  (in-port mcp-client-in-port set-mcp-client-in-port!)
  (out-port mcp-client-out-port set-mcp-client-out-port!)
  (pid mcp-client-pid set-mcp-client-pid!)
  (next-id mcp-client-next-id set-mcp-client-next-id!)
  (closed? mcp-client-closed? set-mcp-client-closed?!))

(define* (make-mcp-client in-port out-port
                          #:optional (pid-pos #f)
                          #:key (pid #f))
  "Create an MCP client communicating through IN-PORT and OUT-PORT.
Optionally accepts a process ID PID if backed by a child process."
  (unless (and (port? in-port) (not (port-closed? in-port)))
    (error "make-mcp-client: in-port must be an open port" in-port))
  (unless (and (port? out-port) (not (port-closed? out-port)))
    (error "make-mcp-client: out-port must be an open port" out-port))
  (%make-mcp-client in-port out-port (or pid pid-pos) 1 #f))

;;;
;;; Utilities
;;;

(define (json-ref alist key)
  "Look up KEY (string or symbol) in ALIST. Returns the associated value,
or #f if not present."
  (if (not (list? alist))
      #f
      (let* ((str-key (if (string? key) key (symbol->string key)))
             (sym-key (if (symbol? key) key (string->symbol key)))
             (entry (or (assoc str-key alist)
                        (assoc sym-key alist))))
        (and entry (cdr entry)))))

(define (tools->list tools)
  "Normalize TOOLS (which may be a vector or list) into a Scheme list."
  (cond
   ((vector? tools) (vector->list tools))
   ((list? tools) tools)
   (else '())))

;;;
;;; Subprocess Spawning
;;;

(define (mcp-spawn-client command-args)
  "Spawn an MCP server subprocess for COMMAND-ARGS (a string or list of strings)
and return a new <mcp-client> connected to its stdio. Stderr is redirected to
.badb/mcp.log so that server logs and warnings do not corrupt curses TUI screens."
  (let* ((args (cond
                ((string? command-args) (list command-args))
                ((list? command-args) command-args)
                (else (error "mcp-spawn-client: command-args must be a list of strings or string"
                             command-args))))
         (log-dir (if (file-exists? ".badb") ".badb" "/tmp"))
         (log-file (string-append log-dir "/mcp.log"))
         (wrapper (list "sh" "-c" (format #f "exec \"$@\" 2>>~a" log-file) "sh"))
         (wrapped-args (append wrapper args))
         (pipe-port (apply open-pipe* OPEN_BOTH wrapped-args))
         (pid (hashq-ref port/pid-table pipe-port)))
    (make-mcp-client pipe-port pipe-port #:pid pid)))

;;;
;;; JSON-RPC 2.0 Protocol Operations
;;;

(define* (mcp-send-request! client method #:optional params)
  "Write a JSON-RPC 2.0 request line to the client's output port with an
incrementing integer ID, flush output, and return the request ID."
  (let* ((id (mcp-client-next-id client))
         (out (mcp-client-out-port client))
         (method-str (if (symbol? method) (symbol->string method) method))
         (msg `((jsonrpc . "2.0")
                (id . ,id)
                (method . ,method-str)
                ,@(if params `((params . ,params)) '()))))
    (set-mcp-client-next-id! client (+ id 1))
    (scm->json msg out)
    (newline out)
    (force-output out)
    id))

(define* (mcp-send-notification! client method #:optional params)
  "Write a JSON-RPC 2.0 notification line (no ID) to the client's output port
and flush output."
  (let* ((out (mcp-client-out-port client))
         (method-str (if (symbol? method) (symbol->string method) method))
         (msg `((jsonrpc . "2.0")
                (method . ,method-str)
                ,@(if params `((params . ,params)) '()))))
    (scm->json msg out)
    (newline out)
    (force-output out)
    *unspecified*))

(define* (mcp-notify client method #:optional params)
  "Send a JSON-RPC 2.0 notification."
  (mcp-send-notification! client method params))

(define (mcp-read-message client)
  "Read a line from the client's input port and parse it into an SCM structure
using json-string->scm. Skips empty lines. Returns EOF object on EOF."
  (let loop ()
    (let ((line (read-line (mcp-client-in-port client))))
      (cond
       ((eof-object? line) line)
       ((string-null? (string-trim-both line)) (loop))
       (else (json-string->scm line))))))

(define* (mcp-call client method #:optional params)
  "Send a JSON-RPC 2.0 request to CLIENT, read the response, verify that the
response ID matches, and return the 'result' member or raise an 'mcp-error
on a JSON-RPC error response."
  (let ((req-id (mcp-send-request! client method params)))
    (let loop ()
      (let ((msg (mcp-read-message client)))
        (cond
         ((eof-object? msg)
          (error "mcp-call: unexpected EOF waiting for response to request" req-id))
         ((not (list? msg))
          (error "mcp-call: expected JSON object from server" msg))
         ;; Skip notifications from server (which have no "id" member)
         ((not (or (assoc "id" msg) (assoc 'id msg)))
          (loop))
         (else
          (let* ((resp-id (let ((e (or (assoc "id" msg) (assoc 'id msg))))
                            (and e (cdr e))))
                 (err-entry (or (assoc "error" msg) (assoc 'error msg)))
                 (res-entry (or (assoc "result" msg) (assoc 'result msg))))
            (unless (equal? resp-id req-id)
              (error "mcp-call: response ID mismatch" req-id resp-id msg))
            (if err-entry
                (let* ((err (cdr err-entry))
                       (code (and (list? err)
                                  (let ((e (or (assoc "code" err) (assoc 'code err))))
                                    (and e (cdr e)))))
                       (message (and (list? err)
                                     (let ((e (or (assoc "message" err) (assoc 'message err))))
                                       (and e (cdr e))))))
                  (scm-error 'mcp-error
                             "mcp-call"
                             "JSON-RPC error ~a: ~a"
                             (list (or code 0) (or message "unknown error"))
                             (list err)))
                (if res-entry
                    (cdr res-entry)
                    (error "mcp-call: response missing both 'result' and 'error'" msg))))))))))

;;;
;;; MCP High-Level Protocol Operations
;;;

(define* (mcp-initialize client
                         #:optional (client-info-pos #f)
                         #:key
                         (client-info '((name . "badb") (version . "0.1.0")))
                         (protocol-version "2024-11-05")
                         (capabilities '()))
  "Send the 'initialize' request with CLIENT-INFO, wait for response,
then send the 'notifications/initialized' notification. Returns the
initialization result."
  (let* ((info (or client-info-pos client-info))
         (params `((protocolVersion . ,protocol-version)
                   (capabilities . ,capabilities)
                   (clientInfo . ,info)))
         (result (mcp-call client "initialize" params)))
    (mcp-notify client "notifications/initialized")
    result))

(define* (mcp-list-tools client #:optional params)
  "Call 'tools/list' and return the tools array/list from the response."
  (let ((result (mcp-call client "tools/list" params)))
    (if (list? result)
        (let ((entry (or (assoc "tools" result)
                         (assoc 'tools result))))
          (if entry
              (cdr entry)
              result))
        result)))

(define (mcp-call-tool client tool-name arguments)
  "Call 'tools/call' with NAME and ARGUMENTS and return the 'content' field."
  (let* ((name-str (if (symbol? tool-name) (symbol->string tool-name) tool-name))
         (params `((name . ,name-str)
                   (arguments . ,(or arguments '()))))
         (result (mcp-call client "tools/call" params)))
    (if (list? result)
        (let ((entry (or (assoc "content" result)
                         (assoc 'content result))))
          (if entry
              (cdr entry)
              result))
        result)))

(define (mcp-close-client client)
  "Close CLIENT ports and wait for child process if PID is present."
  (unless (mcp-client-closed? client)
    (set-mcp-client-closed?! client #t)
    (let ((in (mcp-client-in-port client))
          (out (mcp-client-out-port client))
          (pid (mcp-client-pid client)))
      (when (and (port? out) (not (port-closed? out)))
        (catch #t
          (lambda () (close-port out))
          (lambda _ #f)))
      (when (and (port? in) (not (eq? in out)) (not (port-closed? in)))
        (catch #t
          (lambda () (close-port in))
          (lambda _ #f)))
      (when pid
        (catch #t
          (lambda () (waitpid pid))
          (lambda _ #f))))))

;;;
;;; MCP Manager Record and Operations
;;;

(define-record-type <mcp-manager>
  (%make-mcp-manager servers config)
  mcp-manager?
  (servers mcp-manager-servers set-mcp-manager-servers!)
  (config mcp-manager-config set-mcp-manager-config!))

(define* (make-mcp-manager #:optional (config #f))
  "Create an MCP manager, optionally initialized from CONFIG."
  (let* ((configured-servers (if config (badb-config-mcp-servers config) '()))
         (initial-servers
          (map (lambda (entry)
                 (let ((name (car entry))
                       (spec (cdr entry)))
                   (cons name
                         `((spec . ,spec)
                           (client . #f)
                           (status . "stopped")
                           (tools . ())
                           (error . #f)))))
               configured-servers)))
    (%make-mcp-manager initial-servers config)))

(define (mcp-manager-list-servers mgr)
  "Return list of alists describing configured and active MCP servers."
  (map (lambda (entry)
         (let* ((name (car entry))
                (data (cdr entry))
                (spec (or (assoc-ref data 'spec) '()))
                (status (or (assoc-ref data 'status) "stopped"))
                (client (assoc-ref data 'client))
                (tools (or (assoc-ref data 'tools) '()))
                (err (assoc-ref data 'error))
                (pid (and client (mcp-client-pid client))))
           `((name . ,name)
             (command . ,(or (assoc-ref spec 'command) ""))
             (args . ,(or (assoc-ref spec 'args) '()))
             (status . ,status)
             (running? . ,(string=? status "running"))
             (pid . ,pid)
             (tools-count . ,(length tools))
             (tools . ,tools)
             (auto-start . ,(or (assoc-ref spec 'auto-start) #f))
             (error . ,err))))
       (mcp-manager-servers mgr)))

(define (mcp-manager-get-server mgr name)
  "Retrieve internal server state for NAME or #f."
  (let* ((name-str (if (symbol? name) (symbol->string name) name))
         (entry (assoc name-str (mcp-manager-servers mgr))))
    (and entry (cdr entry))))

(define* (mcp-manager-add-server! mgr name-or-spec
                                  #:optional (command #f)
                                  (args '())
                                  #:key (env '()) (auto-start #f))
  "Register or update an MCP server in MGR and persist to configuration if present.
Supports both (mcp-manager-add-server! mgr spec-alist) and
(mcp-manager-add-server! mgr name command [args] #:key env auto-start)."
  (let* ((is-spec? (and (pair? name-or-spec) (not (string? name-or-spec)) (not (symbol? name-or-spec))))
         (spec (if is-spec? name-or-spec '()))
         (name-raw (if is-spec? (or (assoc-ref spec 'name) "server") name-or-spec))
         (name-str (if (symbol? name-raw) (symbol->string name-raw) name-raw))
         (cmd (if is-spec? (or (assoc-ref spec 'command) "") command))
         (raw-args (if is-spec? (or (assoc-ref spec 'args) '()) args))
         (args-list (cond
                     ((list? raw-args) raw-args)
                     ((string? raw-args) (list raw-args))
                     (else '())))
         (env-list (if is-spec? (or (assoc-ref spec 'env) '()) env))
         (auto-val (if is-spec? (assoc-ref spec 'auto-start) auto-start))
         (full-spec `((name . ,name-str)
                      (command . ,cmd)
                      (args . ,args-list)
                      (env . ,env-list)
                      (auto-start . ,auto-val)))
         (servers (mcp-manager-servers mgr))
         (existing (assoc name-str servers)))
    ;; If existing server is running, stop it
    (when (and existing (assoc-ref (cdr existing) 'client))
      (catch #t
        (lambda () (mcp-close-client (assoc-ref (cdr existing) 'client)))
        (lambda _ #f)))
    (let* ((filtered (remove (lambda (e) (string=? (car e) name-str)) servers))
           (new-entry (cons name-str
                            `((spec . ,full-spec)
                              (client . #f)
                              (status . "stopped")
                              (tools . ())
                              (error . #f)))))
      (set-mcp-manager-servers! mgr (cons new-entry filtered))
      ;; Sync with configuration if attached
      (when (mcp-manager-config mgr)
        (badb-config-add-mcp-server! (mcp-manager-config mgr)
                                    name-str cmd args-list
                                    #:env env-list #:auto-start auto-val)
        (when (badb-config-source-path (mcp-manager-config mgr))
          (catch #t
            (lambda () (save-local-config! (mcp-manager-config mgr)
                                           (badb-config-source-path (mcp-manager-config mgr))))
            (lambda _ #f))))
      #t)))

(define (mcp-manager-remove-server! mgr name)
  "Remove MCP server NAME from MGR, stopping it if running, and update config."
  (let* ((name-str (if (symbol? name) (symbol->string name) name))
         (servers (mcp-manager-servers mgr))
         (existing (assoc name-str servers)))
    (when existing
      (when (assoc-ref (cdr existing) 'client)
        (catch #t
          (lambda () (mcp-close-client (assoc-ref (cdr existing) 'client)))
          (lambda _ #f)))
      (set-mcp-manager-servers! mgr
                                (remove (lambda (e) (string=? (car e) name-str)) servers))
      (when (mcp-manager-config mgr)
        (badb-config-remove-mcp-server! (mcp-manager-config mgr) name-str)
        (when (badb-config-source-path (mcp-manager-config mgr))
          (catch #t
            (lambda () (save-local-config! (mcp-manager-config mgr)
                                           (badb-config-source-path (mcp-manager-config mgr))))
            (lambda _ #f))))
      #t)))

(define (mcp-manager-start-server! mgr name)
  "Spawn, initialize, and discover tools for MCP server NAME.
Returns (values client tools-list error-or-#f)."
  (let* ((name-str (if (symbol? name) (symbol->string name) name))
         (entry (assoc name-str (mcp-manager-servers mgr))))
    (if (not entry)
        (values #f '() (format #f "MCP server '~a' is not configured" name-str))
        (let* ((data (cdr entry))
               (status (assoc-ref data 'status))
               (client (assoc-ref data 'client))
               (spec (assoc-ref data 'spec)))
          (if (and (string=? status "running") client)
              (values client (or (assoc-ref data 'tools) '()) #f)
              (let* ((cmd (or (assoc-ref spec 'command) ""))
                     (args (or (assoc-ref spec 'args) '()))
                     (env (or (assoc-ref spec 'env) '()))
                     (env-pairs (cond
                                 ((null? env) '())
                                 ((and (pair? env) (or (symbol? (car env)) (string? (car env))))
                                  (list env))
                                 ((list? env) env)
                                 (else '())))
                     (full-args (if (not (null? env-pairs))
                                    (append (list "env")
                                            (map (lambda (pair)
                                                   (let ((k (if (symbol? (car pair))
                                                                (symbol->string (car pair))
                                                                (car pair)))
                                                         (v (cdr pair)))
                                                     (format #f "~a=~a" k v)))
                                                 env-pairs)
                                            (cons cmd args))
                                    (cons cmd args))))
                (catch #t
                  (lambda ()
                    (let* ((new-client (mcp-spawn-client full-args))
                           (init-res (mcp-initialize new-client))
                           (raw-tools (catch #t
                                        (lambda () (mcp-list-tools new-client))
                                        (lambda _ '())))
                           (tools (tools->list raw-tools)))
                      ;; Update server record
                      (let ((updated-data `((spec . ,spec)
                                            (client . ,new-client)
                                            (status . "running")
                                            (tools . ,tools)
                                            (error . #f))))
                        (set-mcp-manager-servers!
                         mgr
                         (map (lambda (e) (if (string=? (car e) name-str) (cons name-str updated-data) e))
                              (mcp-manager-servers mgr)))
                        (values new-client tools #f))))
                  (lambda (key . err-args)
                    (let* ((err-msg (format #f "Failed to start server '~a' (~a): ~s" name-str key err-args))
                           (updated-data `((spec . ,spec)
                                           (client . #f)
                                           (status . "error")
                                           (tools . ())
                                           (error . ,err-msg))))
                      (set-mcp-manager-servers!
                       mgr
                       (map (lambda (e) (if (string=? (car e) name-str) (cons name-str updated-data) e))
                            (mcp-manager-servers mgr)))
                      (values #f '() err-msg))))))))))

(define (mcp-manager-stop-server! mgr name)
  "Stop running MCP server NAME and release resources."
  (let* ((name-str (if (symbol? name) (symbol->string name) name))
         (entry (assoc name-str (mcp-manager-servers mgr))))
    (if (not entry)
        #f
        (let* ((data (cdr entry))
               (client (assoc-ref data 'client))
               (spec (assoc-ref data 'spec)))
          (when client
            (catch #t (lambda () (mcp-close-client client)) (lambda _ #f)))
          (let ((updated-data `((spec . ,spec)
                                (client . #f)
                                (status . "stopped")
                                (tools . ())
                                (error . #f))))
            (set-mcp-manager-servers!
             mgr
             (map (lambda (e) (if (string=? (car e) name-str) (cons name-str updated-data) e))
                  (mcp-manager-servers mgr)))
            #t)))))

(define (mcp-manager-stop-all! mgr)
  "Stop all running MCP servers managed by MGR."
  (for-each (lambda (entry)
              (mcp-manager-stop-server! mgr (car entry)))
            (mcp-manager-servers mgr)))

(define* (mcp-manager-list-tools mgr #:optional (server-filter #f))
  "List available tools across active MCP servers, or for SERVER-FILTER if specified."
  (let ((running (filter (lambda (e) (string=? (or (assoc-ref (cdr e) 'status) "") "running"))
                         (mcp-manager-servers mgr))))
    (append-map
     (lambda (entry)
       (let* ((srv-name (car entry))
              (tools (or (assoc-ref (cdr entry) 'tools) '())))
         (if (or (not server-filter)
                 (string=? srv-name (if (symbol? server-filter)
                                        (symbol->string server-filter)
                                        server-filter)))
             (map (lambda (tool)
                    (cons (cons 'server srv-name) tool))
                  tools)
             '())))
     running)))

(define* (mcp-manager-call-tool mgr arg1 arg2 #:optional (arg3 'unspecified))
  "Call an MCP tool. Supports:
(mcp-manager-call-tool mgr server-name tool-name arguments)
or (mcp-manager-call-tool mgr tool-name arguments) discovering the owning server."
  (if (eq? arg3 'unspecified)
      ;; Called as (mcp-manager-call-tool mgr tool-name arguments)
      (let* ((tool-name (if (symbol? arg1) (symbol->string arg1) arg1))
             (arguments arg2)
             (running (filter (lambda (e) (string=? (or (assoc-ref (cdr e) 'status) "") "running"))
                              (mcp-manager-servers mgr)))
             (matching-server
              (find (lambda (e)
                      (let ((tools (or (assoc-ref (cdr e) 'tools) '())))
                        (any (lambda (t)
                               (let ((n (or (assoc-ref t "name") (assoc-ref t 'name))))
                                 (and n (string=? (if (symbol? n) (symbol->string n) n) tool-name))))
                             tools)))
                    running)))
        (if matching-server
            (let ((client (assoc-ref (cdr matching-server) 'client)))
              (mcp-call-tool client tool-name arguments))
            (error (format #f "mcp-manager-call-tool: tool '~a' not found on any running server" tool-name))))
      ;; Called as (mcp-manager-call-tool mgr server-name tool-name arguments)
      (let* ((server-name (if (symbol? arg1) (symbol->string arg1) arg1))
             (tool-name (if (symbol? arg2) (symbol->string arg2) arg2))
             (arguments arg3)
             (entry (assoc server-name (mcp-manager-servers mgr))))
        (unless entry
          (error (format #f "mcp-manager-call-tool: server '~a' not found" server-name)))
        (let* ((data (cdr entry))
               (status (assoc-ref data 'status))
               (client (assoc-ref data 'client)))
          (unless (and (string=? status "running") client)
            (error (format #f "mcp-manager-call-tool: server '~a' is not running" server-name)))
          (mcp-call-tool client tool-name arguments)))))
