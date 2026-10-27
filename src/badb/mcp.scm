;;; badb — MCP Client (JSON-RPC 2.0 over stdio)
;;;
;;; Implements a Model Context Protocol (MCP) client communicating over
;;; standard I/O ports or spawned subprocesses using JSON-RPC 2.0.

(define-module (badb mcp)
  #:use-module (json)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
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
            tools->list))

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
and return a new <mcp-client> connected to its stdio."
  (let* ((args (cond
                ((string? command-args) (list command-args))
                ((list? command-args) command-args)
                (else (error "mcp-spawn-client: command-args must be a list of strings or string"
                             command-args))))
         (pipe-port (apply open-pipe* OPEN_BOTH args))
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
