;;; (geasa surfaces-acp client) — Agent Client Protocol (ACP) Client
;;;
;;; Connects to an ACP server over stdio ports, a spawned subprocess,
;;; TCP socket, or Unix domain socket.  Implements the ACP handshake
;;; (initialize + session/new), prompt turn execution with streaming
;;; session/update accumulation and session/request_permission handling,
;;; cancellation, and clean teardown.

(define-module (geasa surfaces-acp client)
               #:use-module
               (badb kernel ipc)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 threads)
               #:use-module
               (json)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:export
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
                             acp-client-on-update
                             set-acp-client-on-update!
                             acp-client-permission-handler
                             set-acp-client-permission-handler!
                             acp-spawn-client
                             acp-connect-tcp
                             acp-connect-unix
                             acp-client-write-frame!
                             acp-client-read-frame
                             acp-client-request!
                             acp-client-notify!
                             acp-client-initialize!
                             acp-client-authenticate!
                             acp-client-new-session!
                             acp-client-handshake!
                             acp-client-prompt!
                             acp-client-cancel!
                             acp-client-set-mode!
                             acp-client-close!))

(define %acp-client-protocol-version 1)

(define-record-type <acp-client>
                    (%make-acp-client in-port
                                      out-port
                                      pid
                                      closed?
                                      next-id
                                      lock
                                      initialized?
                                      protocol-version
                                      server-capabilities
                                      server-info
                                      session-id
                                      on-update
                                      permission-handler
                                      client-info
                                      client-capabilities
                                      on-close)
                    acp-client?
                    (in-port acp-client-in-port)
                    (out-port acp-client-out-port)
                    (pid acp-client-pid)
                    (closed? acp-client-closed? set-acp-client-closed?!)
                    (next-id acp-client-next-id set-acp-client-next-id!)
                    (lock acp-client-lock)
                    (initialized? acp-client-initialized? set-acp-client-initialized?!)
                    (protocol-version acp-client-protocol-version set-acp-client-protocol-version!)
                    (server-capabilities
                      acp-client-server-capabilities
                      set-acp-client-server-capabilities!)
                    (server-info acp-client-server-info set-acp-client-server-info!)
                    (session-id acp-client-session-id set-acp-client-session-id!)
                    (on-update acp-client-on-update set-acp-client-on-update!)
                    (permission-handler acp-client-permission-handler
                                        set-acp-client-permission-handler!)
                    (client-info acp-client-client-info)
                    (client-capabilities acp-client-client-capabilities)
                    (on-close acp-client-on-close))

(define*
  (make-acp-client #:key
                   in-port
                   out-port
                   (pid #f)
                   (on-update #f)
                   (permission-handler #f)
                   (client-info '((name . "badb-client") (version . "0.1.0")))
                   (client-capabilities '())
                   (on-close #f))
  "Create an <acp-client> communicating over IN-PORT and OUT-PORT."
  (unless (and in-port out-port)
    (error "make-acp-client requires #:in-port and #:out-port"))
  (%make-acp-client in-port
                    out-port
                    pid
                    #f
                    1
                    (make-mutex)
                    #f
                    #f
                    '()
                    '()
                    #f
                    on-update
                    permission-handler
                    client-info
                    client-capabilities
                    on-close))

(define*
  (acp-spawn-client command-args
                    #:key
                    (cwd #f)
                    (on-update #f)
                    (permission-handler #f)
                    (client-info '((name . "badb-client") (version . "0.1.0")))
                    (client-capabilities '())
                    (on-close #f))
  "Spawn an ACP server subprocess for COMMAND-ARGS (string or list of strings)
and return a new <acp-client> connected to its stdio."
  (let*
      ((args
         (cond
           ((string? command-args) (list command-args))
           ((list? command-args) command-args)
           (else
             (error "acp-spawn-client: command-args must be a string or list" command-args))))
       (debug-on
         (let ((val (getenv "BADB_DEBUG")))
           (and (string? val) (> (string-length val) 0))))
       (log-file
         (if debug-on
           (string-append
             (if (file-exists? ".badb")
               ".badb"
               "/tmp")
             "/acp-subagent.log")
           "/dev/null"))
       (wrapper
         (if
             (and cwd (string? cwd) (not (string-null? cwd)))
           (list "sh"
                 "-c"
                 "cd \"$1\" && log=\"$2\" && shift 2 && exec \"$@\" 2>>\"$log\""
                 "sh"
                 cwd
                 log-file)
           (list "sh" "-c" "log=\"$1\"; shift; exec \"$@\" 2>>\"$log\"" "sh" log-file)))
       (wrapped-args (append wrapper args))
       (pipe-port (apply open-pipe* OPEN_BOTH wrapped-args))
       (pid (hashq-ref port/pid-table pipe-port)))
    (make-acp-client #:in-port
                     pipe-port
                     #:out-port
                     pipe-port
                     #:pid
                     pid
                     #:on-update
                     on-update
                     #:permission-handler
                     permission-handler
                     #:client-info
                     client-info
                     #:client-capabilities
                     client-capabilities
                     #:on-close
                     on-close)))

(define*
  (acp-connect-tcp host
                   port
                   #:key
                   (on-update #f)
                   (permission-handler #f)
                   (client-info '((name . "badb-client") (version . "0.1.0")))
                   (client-capabilities '())
                   (on-close #f))
  "Connect to a remote ACP server listening on TCP HOST:PORT."
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (catch #t
      (lambda ()
        (connect sock AF_INET (inet-pton AF_INET host) port)
        (make-acp-client #:in-port
                         sock
                         #:out-port
                         sock
                         #:on-update
                         on-update
                         #:permission-handler
                         permission-handler
                         #:client-info
                         client-info
                         #:client-capabilities
                         client-capabilities
                         #:on-close
                         (lambda ()
                           (catch #t
                             (lambda ()
                               (shutdown sock 2))
                             (lambda _
                               #f))
                           (when on-close (on-close)))))
      (lambda (key . args)
        (catch #t
          (lambda ()
            (close-port sock))
          (lambda _
            #f))
        (apply throw key args)))))

(define*
  (acp-connect-unix socket-path
                    #:key
                    (on-update #f)
                    (permission-handler #f)
                    (client-info '((name . "badb-client") (version . "0.1.0")))
                    (client-capabilities '())
                    (on-close #f))
  "Connect to a remote ACP server listening on Unix domain socket SOCKET-PATH."
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
    (catch #t
      (lambda ()
        (connect sock AF_UNIX socket-path)
        (make-acp-client #:in-port
                         sock
                         #:out-port
                         sock
                         #:on-update
                         on-update
                         #:permission-handler
                         permission-handler
                         #:client-info
                         client-info
                         #:client-capabilities
                         client-capabilities
                         #:on-close
                         (lambda ()
                           (catch #t
                             (lambda ()
                               (shutdown sock 2))
                             (lambda _
                               #f))
                           (when on-close (on-close)))))
      (lambda (key . args)
        (catch #t
          (lambda ()
            (close-port sock))
          (lambda _
            #f))
        (apply throw key args)))))

(define (acp-client-write-frame! client frame)
        "Write JSON-RPC FRAME to CLIENT's output port under its write lock."
        (when (acp-client-closed? client)
          (error "acp-client-write-frame!: client is closed"))
        (with-mutex (acp-client-lock client)
                    (ipc-write-response! frame (acp-client-out-port client))))

(define (acp-client-read-frame client)
        "Read and decode the next JSON-RPC message from CLIENT's input port, or #f on EOF."
        (let loop
          ()
          (let
              ((line
                 (catch #t
                   (lambda ()
                     (read-line (acp-client-in-port client)))
                   (lambda _
                     #f))))
            (cond
              ((or (not line) (eof-object? line)) #f)
              ((string-null? (string-trim-both line)) (loop))
              (else
                (let
                    ((parsed
                       (catch #t
                         (lambda ()
                           (json-string->scm (string-trim-both line)))
                         (lambda _
                           #f))))
                  (if parsed
                    parsed
                    (loop))))))))

(define (%next-client-req-id! client)
        (with-mutex (acp-client-lock client)
                    (let ((id (acp-client-next-id client)))
                      (set-acp-client-next-id! client (+ id 1))
                      id)))

(define*
  (acp-client-notify! client method #:optional (params '()))
  "Send a JSON-RPC notification METHOD with PARAMS to the server."
  (acp-client-write-frame! client
                           `((jsonrpc . "2.0") (method . ,method) (params . ,params)))
  #t)

(define
  (%handle-permission-request! client req-id params)
  "Invoke CLIENT's permission-handler for session/request_permission and send response."
  (let*
      ((handler (acp-client-permission-handler client))
       (decision
         (if handler
           (catch #t
             (lambda ()
               (handler params))
             (lambda _
               'deny))
           'allow))
       (option-id
         (cond
           ((or (eq? decision 'allow) (equal? decision "allow") (eq? decision #t)) "allow")
           (else "reject"))))
    (acp-client-write-frame! client
                             `((jsonrpc . "2.0") (id . ,req-id)
                               (result . ((outcome . ((outcome . "selected") (optionId . ,option-id)))))))))

(define
  (%handle-server-request! client req-id method params)
  "Respond to an incoming server->client JSON-RPC request."
  (if
      (string=? method "session/request_permission")
    (%handle-permission-request! client req-id params)
    (acp-client-write-frame! client
                             `((jsonrpc . "2.0") (id . ,req-id)
                               (error .
                                      ((code . -32601) (message . ,(format #f "unknown client method: ~a" method))))))))

(define (%record-session-update! acc update)
        "Append text/thought/tool_call data from UPDATE into accumulator box ACC."
        (when (pair? update)
          (let
              ((kind (or (prop-ref update "sessionUpdate") "")))
            (cond
              ((string=? kind "agent_message_chunk")
                (let*
                    ((content (prop-ref update "content"))
                     (text (and (pair? content) (prop-ref content "text"))))
                  (when (string? text) (vector-set! acc 0 (cons text (vector-ref acc 0))))))
              ((string=? kind "agent_thought_chunk")
                (let*
                    ((content (prop-ref update "content"))
                     (text (and (pair? content) (prop-ref content "text"))))
                  (when (string? text) (vector-set! acc 1 (cons text (vector-ref acc 1))))))
              ((or (string=? kind "tool_call") (string=? kind "tool_call_update"))
                (vector-set! acc 2 (cons update (vector-ref acc 2))))))))

(define
  (%await-response client req-id on-update acc)
  "Read messages until the response with ID = REQ-ID arrives, handling
interleaved server notifications and session/request_permission requests."
  (let loop
    ()
    (let ((msg (acp-client-read-frame client)))
      (if msg
        (let
            ((id (prop-ref msg 'id))
             (method (prop-ref msg "method"))
             (params (or (prop-ref msg "params") '())))
          (cond
            ((and id (not method) (equal? id req-id)) msg)
            ((and id method) (%handle-server-request! client id method params) (loop))
            ((and (not id) method)
              (when (string=? method "session/update")
                (let
                    ((sid (prop-ref params "sessionId")) (upd (prop-ref params "update")))
                  (when acc (%record-session-update! acc upd))
                  (let
                      ((cb (or on-update (acp-client-on-update client))))
                    (when cb
                      (catch #t
                        (lambda ()
                          (cb sid upd))
                        (lambda _
                          #f))))))
              (loop))
            (else (loop))))
        #f))))

(define*
  (acp-client-request! client
                       method
                       #:optional
                       (params '())
                       #:key
                       (on-update #f)
                       (acc #f))
  "Send JSON-RPC request METHOD with PARAMS and await its response frame."
  (let ((req-id (%next-client-req-id! client)))
    (acp-client-write-frame! client
                             `((jsonrpc . "2.0") (id . ,req-id) (method . ,method) (params . ,params)))
    (let
        ((resp (%await-response client req-id on-update acc)))
      (unless resp
        (error "acp-client-request!: connection closed before response" method))
      (let ((err (prop-ref resp "error")))
        (when err
          (error "acp-client-request!: JSON-RPC error"
                 method
                 (prop-ref err "code")
                 (prop-ref err "message"))))
      (or (prop-ref resp "result") '()))))

(define*
  (acp-client-initialize! client
                          #:key
                          (protocol-version %acp-client-protocol-version)
                          (capabilities #f)
                          (client-info #f))
  "Perform the ACP initialize handshake with the server."
  (let*
      ((params
         `((protocolVersion . ,protocol-version)
           (clientCapabilities .
                               ,(or capabilities (acp-client-client-capabilities client)))
           (clientInfo . ,(or client-info (acp-client-client-info client)))))
       (result (acp-client-request! client "initialize" params)))
    (set-acp-client-protocol-version! client (prop-ref result "protocolVersion"))
    (set-acp-client-server-capabilities! client
                                         (or (prop-ref result "agentCapabilities") '()))
    (set-acp-client-server-info! client (or (prop-ref result "agentInfo") '()))
    (set-acp-client-initialized?! client #t)
    result))

(define*
  (acp-client-authenticate! client #:optional (params '()))
  "Send ACP authenticate request."
  (acp-client-request! client "authenticate" params))

(define*
  (acp-client-new-session! client
                           #:key
                           (cwd (getcwd))
                           (mcp-servers #())
                           (allowed-tools #f)
                           (max-iterations #f)
                           (system-prompt #f)
                           (journal-path #f))
  "Create a new ACP session rooted at CWD and store its sessionId on CLIENT."
  (let*
      ((base-params `((cwd . ,cwd) (mcpServers . ,mcp-servers)))
       (params
         (append base-params
                 (if allowed-tools
                   `((allowedTools .
                                   ,(if (vector? allowed-tools)
                                      allowed-tools
                                      (list->vector allowed-tools))))
                   '())
                 (if max-iterations
                   `((maxIterations . ,max-iterations))
                   '())
                 (if system-prompt
                   `((systemPrompt . ,system-prompt))
                   '())
                 (if journal-path
                   `((journalPath . ,journal-path))
                   '())))
       (result (acp-client-request! client "session/new" params))
       (sid (prop-ref result "sessionId")))
    (unless (string? sid)
      (error "acp-client-new-session!: server did not return sessionId" result))
    (set-acp-client-session-id! client sid)
    sid))

(define*
  (acp-client-handshake! client
                         #:key
                         (cwd (getcwd))
                         (protocol-version %acp-client-protocol-version)
                         (capabilities #f)
                         (client-info #f)
                         (mcp-servers #())
                         (allowed-tools #f)
                         (max-iterations #f)
                         (system-prompt #f)
                         (journal-path #f))
  "Perform the full ACP handshake: initialize followed by session/new.
Returns the created session-id string."
  (unless (acp-client-initialized? client)
    (acp-client-initialize! client
                            #:protocol-version
                            protocol-version
                            #:capabilities
                            capabilities
                            #:client-info
                            client-info))
  (acp-client-new-session! client
                           #:cwd
                           cwd
                           #:mcp-servers
                           mcp-servers
                           #:allowed-tools
                           allowed-tools
                           #:max-iterations
                           max-iterations
                           #:system-prompt
                           system-prompt
                           #:journal-path
                           journal-path))

(define*
  (acp-client-prompt! client prompt-text #:key (session-id #f) (on-update #f))
  "Send session/prompt to the ACP server and return an alist containing
status, stop-reason, content, thoughts, tool-calls, and session-id."
  (let
      ((sid (or session-id (acp-client-session-id client))))
    (unless (string? sid)
      (error "acp-client-prompt!: no active ACP session; run handshake first"))
    (let*
        ((acc (vector '() '() '()))
         (params
           `((sessionId . ,sid) (prompt . #(((type . "text") (text . ,prompt-text))))))
         (result
           (acp-client-request! client
                                "session/prompt"
                                params
                                #:on-update
                                on-update
                                #:acc
                                acc))
         (stop-reason (or (prop-ref result "stopReason") "end_turn"))
         (content (string-concatenate (reverse (vector-ref acc 0))))
         (thoughts (string-concatenate (reverse (vector-ref acc 1))))
         (tool-calls (reverse (vector-ref acc 2))))
      `((status . ,stop-reason)
        (stop-reason . ,stop-reason)
        (content . ,content)
        (thoughts . ,thoughts)
        (tool-calls . ,tool-calls)
        (session-id . ,sid)))))

(define*
  (acp-client-cancel! client #:optional (session-id #f))
  "Send session/cancel notification for SESSION-ID (or active session)."
  (let
      ((sid (or session-id (acp-client-session-id client))))
    (when (string? sid)
      (acp-client-notify! client "session/cancel" `((sessionId . ,sid))))))

(define*
  (acp-client-set-mode! client mode-id #:optional (session-id #f))
  "Send session/set_mode request for SESSION-ID."
  (let
      ((sid (or session-id (acp-client-session-id client))))
    (unless (string? sid) (error "acp-client-set-mode!: no active ACP session"))
    (acp-client-request! client
                         "session/set_mode"
                         `((sessionId . ,sid) (modeId . ,mode-id)))))

(define (acp-client-close! client)
        "Close CLIENT ports, invoke any on-close hook, and reap spawned child PID. Idempotent."
        (unless (acp-client-closed? client)
          (set-acp-client-closed?! client #t)
          (let
              ((in (acp-client-in-port client))
               (out (acp-client-out-port client))
               (pid (acp-client-pid client))
               (on-close (acp-client-on-close client)))
            (when on-close
              (catch #t
                on-close
                (lambda _
                  #f)))
            (when (and out (not (port-closed? out)))
              (catch #t
                (lambda ()
                  (force-output out))
                (lambda _
                  #f))
              (catch #t
                (lambda ()
                  (close-port out))
                (lambda _
                  #f)))
            (when
                (and in (not (eq? in out)) (not (port-closed? in)))
              (catch #t
                (lambda ()
                  (close-port in))
                (lambda _
                  #f)))
            (when (and pid (integer? pid) (> pid 0))
              (catch #t
                (lambda ()
                  (let ((r1 (waitpid pid WNOHANG)))
                    (when (and (pair? r1) (zero? (car r1)))
                      (kill pid SIGTERM)
                      (usleep 20000)
                      (let ((r2 (waitpid pid WNOHANG)))
                        (when (and (pair? r2) (zero? (car r2)))
                          (kill pid SIGKILL)
                          (waitpid pid))))))
                (lambda _
                  #f)))))
        #t)