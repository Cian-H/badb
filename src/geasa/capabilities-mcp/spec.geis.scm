;;; badb — MCP Client Test Suite (SRFI-64)

(define-module (test test-mcp)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (geasa capabilities-mcp plugin)
  #:use-module (geasa foundation-config plugin)
  #:use-module (geasa foundation-util plugin)
  #:use-module (json)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads))

(spec-begin "capabilities-mcp")

;;;
;;; 1. Client Construction and Predicates
;;;

(test-group "client-construction"
  (let* ((in-p (open-input-string ""))
         (out-p (open-output-string))
         (client (make-mcp-client in-p out-p)))
    (test-assert "mcp-client? recognizes client" (mcp-client? client))
    (test-assert "mcp-client? rejects non-client" (not (mcp-client? "not-a-client")))
    (test-assert "mcp-client? rejects records of other types" (not (mcp-client? (cons in-p out-p))))
    (test-eq "ports match input"
      in-p (mcp-client-in-port client))
    (test-eq "ports match output"
      out-p (mcp-client-out-port client))
    (test-eq "initial next-id is 1"
      1 (mcp-client-next-id client))
    (test-eq "default pid is #f" #f (mcp-client-pid client)) (mcp-close-client client))

  (let* ((in-p (open-input-string ""))
         (out-p (open-output-string))
         (client (make-mcp-client in-p out-p #:pid 9999)))
    (test-eq "keyword #:pid sets pid" 9999 (mcp-client-pid client)) (mcp-close-client client))

  (let* ((in-p (open-input-string ""))
         (out-p (open-output-string))
         (client (make-mcp-client in-p out-p 8888)))
    (test-eq "positional pid sets pid" 8888 (mcp-client-pid client)) (mcp-close-client client)))

;;;
;;; 2. JSON-RPC 2.0 Request Formatting
;;;

(test-group "request-formatting"
  (let* ((in-p (open-input-string ""))
         (out-p (open-output-string))
         (client (make-mcp-client in-p out-p))
         (id1 (mcp-send-request! client "tools/list" '((cursor . "abc")))))
    (test-eq "first request ID is 1" 1 id1)
    (test-eq "next ID incremented to 2" 2 (mcp-client-next-id client))
    (let* ((json-str (get-output-string out-p))
           (parsed (json-string->scm json-str)))
      (test-equal "jsonrpc is 2.0"
        "2.0" (json-ref parsed "jsonrpc"))
      (test-eq "id is 1"
        1 (json-ref parsed "id"))
      (test-equal "method is tools/list"
        "tools/list" (json-ref parsed "method"))
      (test-equal "params matches"
        "abc" (json-ref (json-ref parsed "params") "cursor")))

    ;; Second request without params
    (let* ((out-p2 (open-output-string))
           (_ (set-mcp-client-out-port! client out-p2))
           (id2 (mcp-send-request! client "ping")))
      (test-eq "second request ID is 2" 2 id2)
      (test-eq "next ID incremented to 3" 3 (mcp-client-next-id client))
      (let* ((json-str (get-output-string out-p2))
             (parsed (json-string->scm json-str)))
        (test-equal "method is ping"
          "ping" (json-ref parsed "method"))
        (test-assert "params member omitted when #f" (not (assoc "params" parsed)))))
    (mcp-close-client client)))

;;;
;;; 3. JSON-RPC 2.0 Notification Formatting
;;;

(test-group "notification-formatting"
  (let* ((in-p (open-input-string ""))
         (out-p (open-output-string))
         (client (make-mcp-client in-p out-p)))
    (mcp-send-notification! client "notifications/initialized")
    (let* ((json-str (get-output-string out-p))
           (parsed (json-string->scm json-str)))
      (test-equal "jsonrpc is 2.0"
        "2.0" (json-ref parsed "jsonrpc"))
      (test-equal "method is notifications/initialized"
        "notifications/initialized" (json-ref parsed "method"))
      (test-assert "notification has NO id member" (not (assoc "id" parsed)))
      (test-assert "notification has NO params member when omitted" (not (assoc "params" parsed))))

    ;; mcp-notify with params
    (let* ((out-p2 (open-output-string))
           (_ (set-mcp-client-out-port! client out-p2)))
      (mcp-notify client "notifications/message" '((level . "info") (data . "hello")))
      (let* ((json-str (get-output-string out-p2))
             (parsed (json-string->scm json-str)))
        (test-equal "method is notifications/message"
          "notifications/message" (json-ref parsed "method"))
        (test-assert "no id in mcp-notify" (not (assoc "id" parsed)))
        (test-equal "params level is info"
          "info" (json-ref (json-ref parsed "params") "level")))) (mcp-close-client client)))

;;;
;;; 4. Reading Messages
;;;

(test-group "read-message"
  (let* ((raw "   \n\n{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"status\":\"ok\"}}\n")
         (in-p (open-input-string raw))
         (out-p (open-output-string))
         (client (make-mcp-client in-p out-p))
         (msg (mcp-read-message client)))
    (test-equal "jsonrpc parsed"
      "2.0" (json-ref msg "jsonrpc"))
    (test-eq "id parsed"
      1 (json-ref msg "id"))
    (test-equal "result status parsed"
      "ok" (json-ref (json-ref msg "result") "status"))
    ;; Reading at EOF returns EOF object
    (test-assert "mcp-read-message at end returns EOF" (eof-object? (mcp-read-message client)))
    (mcp-close-client client)))

;;;
;;; 5. Bidirectional Communication with Paired Pipes
;;;

(test-group "bidirectional-mcp-call"
  ;; Helper to create connected pipe client and server ports
  (define (make-pipe-pair)
    (let* ((c2s (pipe))
           (s2c (pipe)))
      (values (make-mcp-client (car s2c) (cdr c2s))
              (car c2s)  ; server-in
              (cdr s2c)))) ; server-out

  ;; Successful call
  (call-with-values make-pipe-pair
    (lambda (client s-in s-out)
      (let ((server-thread
             (call-with-new-thread
              (lambda ()
                (let* ((line (read-line s-in))
                       (req (json-string->scm line))
                       (id (json-ref req "id"))
                       (method (json-ref req "method")))
                  (if (string=? method "tools/list")
                      (begin
                        (display (string-append
                                  "{\"jsonrpc\":\"2.0\",\"id\":" (number->string id)
                                  ",\"result\":{\"tools\":[{\"name\":\"echo\",\"description\":\"Echo input\"}]}}\n")
                                 s-out)
                        (force-output s-out))
                      (display "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32601,\"message\":\"not found\"}}\n" s-out)))))))
        (let ((result (mcp-call client "tools/list")))
          (join-thread server-thread)
          (test-assert "result is a list" (list? result))
          (test-assert "result contains tools" (not (not (json-ref result "tools")))))
        (mcp-close-client client)
        (close-port s-in)
        (close-port s-out))))

  ;; Server sending notification prior to response
  (call-with-values make-pipe-pair
    (lambda (client s-in s-out)
      (let ((server-thread
             (call-with-new-thread
              (lambda ()
                (let* ((line (read-line s-in))
                       (req (json-string->scm line))
                       (id (json-ref req "id")))
                  ;; Send server notification first
                  (display "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/progress\",\"params\":{\"progress\":50}}\n" s-out)
                  (force-output s-out)
                  ;; Send response
                  (display (string-append
                            "{\"jsonrpc\":\"2.0\",\"id\":" (number->string id)
                            ",\"result\":\"finished\"}\n")
                           s-out)
                  (force-output s-out))))))
        (let ((result (mcp-call client "do-work")))
          (join-thread server-thread)
          (test-equal "mcp-call skips notifications and returns result"
            "finished" result)) (mcp-close-client client)
        (close-port s-in)
        (close-port s-out))))

  ;; Response ID mismatch raises error
  (call-with-values make-pipe-pair
    (lambda (client s-in s-out)
      (let ((server-thread
             (call-with-new-thread
              (lambda ()
                (read-line s-in)
                ;; Send response with mismatched ID (999 instead of 1)
                (display "{\"jsonrpc\":\"2.0\",\"id\":999,\"result\":\"bad-id\"}\n" s-out)
                (force-output s-out)))))
        (test-error "ID mismatch raises error" #t
          (mcp-call client "do-work"))
        (join-thread server-thread)
        (mcp-close-client client)
        (close-port s-in)
        (close-port s-out))))

  ;; Unexpected EOF raises error
  (call-with-values make-pipe-pair
    (lambda (client s-in s-out)
      (let ((server-thread
             (call-with-new-thread
              (lambda ()
                (read-line s-in)
                ;; Close server-out without sending response
                (close-port s-out)))))
        (test-error "Unexpected EOF raises error" #t
          (mcp-call client "do-work"))
        (join-thread server-thread)
        (mcp-close-client client)
        (close-port s-in)))))

;;;
;;; 6. JSON-RPC Error Response Handling
;;;

(test-group "json-rpc-error-handling"
  (let* ((c2s (pipe))
         (s2c (pipe))
         (client (make-mcp-client (car s2c) (cdr c2s)))
         (s-in (car c2s))
         (s-out (cdr s2c)))
    (let ((server-thread
           (call-with-new-thread
            (lambda ()
              (let* ((line (read-line s-in))
                     (req (json-string->scm line))
                     (id (json-ref req "id")))
                (display (string-append
                          "{\"jsonrpc\":\"2.0\",\"id\":" (number->string id)
                          ",\"error\":{\"code\":-32601,\"message\":\"Method not found\",\"data\":\"extra info\"}}\n")
                         s-out)
                (force-output s-out))))))
      ;; Test that mcp-call raises error on error response
      (test-error "mcp-call raises error on JSON-RPC error" 'mcp-error
        (mcp-call client "nonexistent_method"))
      (join-thread server-thread))

    ;; Test catching mcp-error key and inspecting error fields
    (let* ((server-thread2
           (call-with-new-thread
            (lambda ()
              (let* ((line (read-line s-in))
                     (req (json-string->scm line))
                     (id (json-ref req "id")))
                (display (string-append
                          "{\"jsonrpc\":\"2.0\",\"id\":" (number->string id)
                          ",\"error\":{\"code\":-32602,\"message\":\"Invalid params\"}}\n")
                         s-out)
                (force-output s-out)))))
          (caught #f)
          (error-code #f)
          (error-msg #f))
      (catch 'mcp-error
        (lambda ()
          (mcp-call client "bad_args"))
        (lambda (key subr fmt args extra)
          (set! caught #t)
          (set! error-code (car args))
          (set! error-msg (cadr args))))
      (join-thread server-thread2)
      (test-assert "caught 'mcp-error key" caught)
      (test-eq "error code matches" -32602 error-code)
      (test-equal "error message matches" "Invalid params" error-msg))

    (mcp-close-client client)
    (close-port s-in)
    (close-port s-out)))

;;;
;;; 7. High-Level MCP Protocol: Initialize
;;;

(test-group "mcp-initialize"
  (let* ((c2s (pipe))
         (s2c (pipe))
         (client (make-mcp-client (car s2c) (cdr c2s)))
         (s-in (car c2s))
         (s-out (cdr s2c)))
    (let* ((server-thread
            (call-with-new-thread
             (lambda ()
               ;; 1. Read initialize request
               (let* ((init-line (read-line s-in))
                      (req (json-string->scm init-line))
                      (id (json-ref req "id"))
                      (method (json-ref req "method"))
                      (params (json-ref req "params")))
                 (if (string=? method "initialize")
                     (begin
                       (display (string-append
                                 "{\"jsonrpc\":\"2.0\",\"id\":" (number->string id)
                                 ",\"result\":{\"protocolVersion\":\"2024-11-05\","
                                 "\"capabilities\":{},\"serverInfo\":{\"name\":\"mock-server\",\"version\":\"1.0\"}}}\n")
                                s-out)
                       (force-output s-out)
                       ;; 2. Read notifications/initialized
                       (let* ((notif-line (read-line s-in))
                              (notif (json-string->scm notif-line)))
                         (list (json-ref notif "method")
                               (json-ref (json-ref params "clientInfo") "name"))))
                     #f)))))
           (init-result (mcp-initialize client #:client-info '((name . "test-badb") (version . "0.9.0"))))
           (server-received (join-thread server-thread)))
      (test-equal "server received notifications/initialized"
        "notifications/initialized" (car server-received))
      (test-equal "server received clientInfo name"
        "test-badb" (cadr server-received))
      (test-equal "client received protocolVersion"
        "2024-11-05" (json-ref init-result "protocolVersion"))
      (test-equal "client received serverInfo name"
        "mock-server" (json-ref (json-ref init-result "serverInfo") "name")))
    (mcp-close-client client)
    (close-port s-in)
    (close-port s-out)))

;;;
;;; 8. High-Level MCP Protocol: Tools List & Call
;;;

(test-group "mcp-list-tools-and-call-tool"
  (let* ((c2s (pipe))
         (s2c (pipe))
         (client (make-mcp-client (car s2c) (cdr c2s)))
         (s-in (car c2s))
         (s-out (cdr s2c)))
    ;; Test mcp-list-tools
    (let* ((server-thread
            (call-with-new-thread
             (lambda ()
               (let* ((line (read-line s-in))
                      (req (json-string->scm line))
                      (id (json-ref req "id")))
                 (display (string-append
                           "{\"jsonrpc\":\"2.0\",\"id\":" (number->string id)
                           ",\"result\":{\"tools\":["
                           "{\"name\":\"add\",\"description\":\"Add two numbers\","
                           "\"inputSchema\":{\"type\":\"object\"}},"
                           "{\"name\":\"sub\",\"description\":\"Subtract numbers\","
                           "\"inputSchema\":{\"type\":\"object\"}}"
                           "]}}\n")
                          s-out)
                 (force-output s-out)))))
           (tools (mcp-list-tools client)))
      (join-thread server-thread)
      (test-assert "tools is vector or list" (or (vector? tools) (list? tools)))
      (let ((tool-list (tools->list tools)))
        (test-eq "2 tools returned" 2 (length tool-list))
        (test-equal "first tool name is add"
          "add" (json-ref (car tool-list) "name"))
        (test-equal "second tool name is sub"
          "sub" (json-ref (cadr tool-list) "name"))))

    ;; Test mcp-call-tool
    (let* ((server-thread
            (call-with-new-thread
             (lambda ()
               (let* ((line (read-line s-in))
                      (req (json-string->scm line))
                      (id (json-ref req "id"))
                      (params (json-ref req "params"))
                      (tool-name (json-ref params "name"))
                      (arguments (json-ref params "arguments")))
                 (if (and (string=? tool-name "add")
                          (= (json-ref arguments "a") 10)
                          (= (json-ref arguments "b") 20))
                     (begin
                       (display (string-append
                                 "{\"jsonrpc\":\"2.0\",\"id\":" (number->string id)
                                 ",\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"30\"}],\"isError\":false}}\n")
                                s-out)
                       (force-output s-out))
                     (display "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32602,\"message\":\"bad params\"}}\n" s-out))))))
           (content (mcp-call-tool client "add" '((a . 10) (b . 20)))))
      (join-thread server-thread)
      (test-assert "content returned" (not (not content)))
      (let ((content-list (tools->list content)))
        (test-eq "1 content item" 1 (length content-list))
        (test-equal "content type text"
          "text" (json-ref (car content-list) "type"))
        (test-equal "content text 30"
          "30" (json-ref (car content-list) "text"))))
    (mcp-close-client client)
    (close-port s-in)
    (close-port s-out)))

;;;
;;; 9. Subprocess Spawning and Closing
;;;

(test-group "subprocess-spawn-and-close"
  (let ((client (mcp-spawn-client '("cat"))))
    (test-assert "spawned client is an mcp-client" (mcp-client? client))
    (test-assert "spawned client has a valid pid" (and (number? (mcp-client-pid client))
           (> (mcp-client-pid client) 0)))
    ;; Send raw line through cat and read back
    (display "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"hello-cat\"}\n"
             (mcp-client-out-port client))
    (force-output (mcp-client-out-port client))
    (let ((msg (mcp-read-message client)))
      (test-equal "echoed message parsed"
        "hello-cat" (json-ref msg "result")))
    ;; Close client
    (mcp-close-client client)
    ;; Test idempotency of close
    (mcp-close-client client)
    (test-assert "client closed cleanly" #t)))

;;;
;;; 9b. Debug-gated mcp.log stderr redirect
;;;

;; Run in a scratch cwd so the real .badb/ is never touched: with
;; BADB_DEBUG unset the subprocess stderr goes to /dev/null and no
;; mcp.log appears; with BADB_DEBUG set it lands in .badb/mcp.log.

(test-group "mcp-log-debug-gating"
  (let* ((here (getcwd))
         (saved-debug (getenv "BADB_DEBUG"))
         (tmp-dir "/tmp/badb-mcp-log-gating"))
    (catch #t (lambda () (system* "rm" "-rf" tmp-dir)) (lambda _ #f))
    (mkdir tmp-dir)
    (dynamic-wind
      (lambda () (chdir tmp-dir))
      (lambda ()
        (mkdir ".badb")
        (dynamic-wind
          (lambda () (unsetenv "BADB_DEBUG"))
          (lambda ()
            (let ((client (mcp-spawn-client '("true"))))
              (test-assert "spawn without BADB_DEBUG returns a client"
                (mcp-client? client))
              (mcp-close-client client))
            (test-assert "no mcp.log without BADB_DEBUG"
              (not (file-exists? ".badb/mcp.log"))))
          (lambda () #f))
        (dynamic-wind
          (lambda () (setenv "BADB_DEBUG" "1"))
          (lambda ()
            (let ((client (mcp-spawn-client '("true"))))
              (mcp-close-client client))
            (test-assert "mcp.log written with BADB_DEBUG"
              (file-exists? ".badb/mcp.log")))
          (lambda ()
            (if saved-debug
                (setenv "BADB_DEBUG" saved-debug)
                (unsetenv "BADB_DEBUG")))))
      (lambda ()
        (chdir here)
        (catch #t (lambda () (system* "rm" "-rf" tmp-dir)) (lambda _ #f))))))

;;;
;;; 10. MCP Manager Lifecycle and Operations
;;;

(test-group "mcp-manager-lifecycle"
  (let* ((cfg (make-badb-config))
         (_ (badb-config-add-mcp-server! cfg "echo-srv" "sh" '("-c" "while read l; do echo '{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[{\"name\":\"echo\",\"description\":\"Echo text\"}]}}'; done")))
         (mgr (make-mcp-manager cfg)))

    (test-assert "mcp-manager? recognizes manager" (mcp-manager? mgr))

    (let ((servers (mcp-manager-list-servers mgr)))
      (test-equal "1 server configured" 1 (length servers))
      (test-equal "server name is echo-srv" "echo-srv" (assoc-ref (car servers) 'name))
      (test-equal "initial status is stopped" "stopped" (assoc-ref (car servers) 'status)))

    ;; Add server dynamically
    (mcp-manager-add-server! mgr "math-srv" "sh" '("-c" "exit 0"))
    (test-equal "2 servers after dynamic add" 2 (length (mcp-manager-list-servers mgr)))
    (test-assert "math-srv spec present" (mcp-manager-get-server mgr "math-srv"))

    ;; Remove server dynamically
    (mcp-manager-remove-server! mgr "math-srv")
    (test-equal "1 server after remove" 1 (length (mcp-manager-list-servers mgr)))
    (test-assert "math-srv removed" (not (mcp-manager-get-server mgr "math-srv")))

    ;; Stop server when already stopped is safe
    (test-assert "stopping stopped server is safe" (or (mcp-manager-stop-server! mgr "echo-srv") #t))

    ;; Stop non-existent server is safe
    (test-assert "stopping unknown server is safe" (not (mcp-manager-stop-server! mgr "non-existent")))

    ;; Stop all servers
    (mcp-manager-stop-all! mgr)
    (test-assert "stop-all completed" #t)))

(spec-end)
(spec-exit)
