;;; (geasa surfaces-acp spec.geis) — Contract verification for the ACP editor surface
;;;
;;; Covers the JSON-RPC framing, the initialize handshake, the session
;;; lifecycle, prompt streaming (message chunks and tool call notifications),
;;; the agent -> client request round trip that backs the permission gate,
;;; and cancellation.

(define-module (test surfaces-acp-spec)
               #:use-module
               (srfi srfi-64)
               #:use-module
               (badb spec)
               #:use-module
               (badb kernel registry)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (geasa providers-model common)
               #:use-module
               (geasa providers-model mock)
               #:use-module
               (geasa runtimes-react plugin)
               #:use-module
               (geasa surfaces-acp plugin)
               #:use-module
               (json)
               #:use-module
               (web response)
               #:use-module
               (ice-9 threads)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (srfi srfi-13))

(spec-begin "surfaces-acp")

;;;
;;; Helpers

;;;
(define (acp-updates notifications kind)
        "Return the session/update notifications of KIND from NOTIFICATIONS."
        (filter
          (lambda (notification)
            (string=?
              (or
                (prop-ref (prop-ref (prop-ref notification 'params) 'update) 'sessionUpdate)
                "")
              kind))
          notifications))

(define (acp-chunk-texts notifications kind)
        "Return the text carried by each session update of KIND."
        (map
          (lambda (notification)
            (let
                ((content (prop-ref (prop-ref (prop-ref notification 'params) 'update) 'content)))
              (or (prop-ref content 'text) "")))
          (acp-updates notifications kind)))

(define (acp-tool-call-titles notifications)
        "Return the tool call titles announced by tool_call notifications."
        (map
          (lambda (notification)
            (or (prop-ref (prop-ref (prop-ref notification 'params) 'update) 'title) ""))
          (acp-updates notifications "tool_call")))

(define (acp-mock-client)
        "Return a model client bound to the mocked HTTP transport."
        (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))

(define (acp-sse . chunks)
        "Return an SSE body carrying CHUNKS."
        (string-append (apply string-append chunks) "data: [DONE]\n"))

(define (acp-content-chunk text)
        "Return an SSE chunk streaming TEXT as assistant content."
        (let* ((delta `((content . ,text)))
               (choice `((delta . ,delta)))
               (choices (list->vector (list choice))))
          (format #f "data: ~a\n\n" (scm->json-string `((choices . ,choices))))))

(define (acp-tool-chunk name arguments)
        "Return an SSE chunk asking for the tool NAME with ARGUMENTS."
        (let*
            ((fn `((name . ,name) (arguments . ,arguments)))
             (tool-call `((index . 0) (id . "call_1") (function . ,fn)))
             (delta `((tool_calls . #(,tool-call))))
             (choice `((delta . ,delta)))
             (choices (list->vector (list choice))))
          (format #f "data: ~a\n\n" (scm->json-string `((choices . ,choices))))))

(define (acp-tool-arguments alist)
        "Return the JSON text of the tool arguments ALIST."
        (scm->json-string alist))

(define*
  (acp-capturing-server client
                        tmp-dir
                        notifications
                        #:key
                        (permission-policy 'allow))
  "Return a server whose session updates are collected into the cell NOTIFICATIONS."
  (make-acp-server #:client
                   client
                   #:workspace
                   tmp-dir
                   #:permission-policy
                   permission-policy
                   #:notifier
                   (lambda (server notification)
                     (set-car! notifications (cons notification (car notifications))))))

;;;
;;; 1. Initialize handshake

;;;
(test-group "acp-initialize-handshake"
            (let*
                ((server (make-acp-server))
                 (response
                   (acp-handle-request server
                                       1
                                       "initialize"
                                       '((clientCapabilities . ((fs . ((readTextFile . #t)))))))))
              (test-equal "jsonrpc version" "2.0" (prop-ref response 'jsonrpc))
              (test-equal "request id echoed" 1 (prop-ref response 'id))
              (let ((result (prop-ref response 'result)))
                (test-equal "protocol version"
                            acp-protocol-version
                            (prop-ref result 'protocolVersion))
                (test-equal "agent name" "badb" (prop-ref (prop-ref result 'agentInfo) 'name))
                (test-equal "no auth methods" #() (prop-ref result 'authMethods))
                (test-equal "session loading unsupported"
                            #f
                            (prop-ref (prop-ref result 'agentCapabilities) 'loadSession)))
              (test-equal "client capabilities recorded"
                          '((fs . ((readTextFile . #t))))
                          (acp-server-client-capabilities server))))

;;;
;;; 2. Protocol errors

;;;
(test-group "acp-request-errors"
            (let*
                ((server (make-acp-server))
                 (unknown (acp-handle-request server 7 "does/not/exist" '()))
                 (no-session
                   (acp-handle-request server 8 "session/prompt" '((sessionId . "sess_nope")))))
              (test-equal "unknown method code"
                          -32601
                          (prop-ref (prop-ref unknown 'error) 'code))
              (test-equal "unknown session code"
                          -32602
                          (prop-ref (prop-ref no-session 'error) 'code))))

;;;
;;; 3. Session lifecycle

;;;
(test-group "acp-session-lifecycle"
            (let*
                ((tmp-dir (mkdtemp "/tmp/badb-acp-session-XXXXXX"))
                 (server (make-acp-server #:client (acp-mock-client) #:workspace tmp-dir))
                 (response (acp-handle-request server 2 "session/new" `((cwd . ,tmp-dir))))
                 (missing (acp-handle-request server 6 "session/new" (quote ((mcpServers . #())))))
                 (result (prop-ref response 'result))
                 (session-id (prop-ref result 'sessionId))
                 (session (acp-session-find server session-id)))
              (test-equal "session/new without cwd is -32602"
                          -32602
                          (prop-ref (prop-ref missing (quote error)) (quote code)))
              (test-assert "session id is a string" (string? session-id))
              (test-assert "session id is namespaced" (string-prefix? "sess_" session-id))
              (test-equal "default mode advertised"
                          "default"
                          (prop-ref (prop-ref result 'modes) 'currentModeId))
              (test-assert "session registered" (acp-session? session))
              (test-equal "session cwd" tmp-dir (acp-session-cwd session))
              (test-assert "filesystem tool available"
                           (procedure? (registry-lookup (acp-session-reg session) "read_file")))
              (test-assert "spec gate tool available"
                           (procedure? (registry-lookup (acp-session-reg session) "spec.verify")))
              (system* "rm" "-rf" tmp-dir)))

;;;
;;; 4. Prompt streaming

;;;
(test-group "acp-prompt-streams-message-chunks"
            (let*
                ((tmp-dir (mkdtemp "/tmp/badb-acp-prompt-XXXXXX"))
                 (notifications (list '()))
                 (server
                   (acp-capturing-server (acp-mock-client) tmp-dir notifications))
                 (session (acp-new-session server #:cwd tmp-dir))
                 (session-id (acp-session-id session))
                 (mock-poster
                   (lambda* (uri #:key body headers streaming?)
                            (values (build-response #:code 200)
                                    (open-input-string (acp-sse (acp-content-chunk "Hello from badb.")))))))
              (parameterize ((current-http-post mock-poster))
                            (let
                                ((response
                                   (acp-handle-request server
                                                       3
                                                       "session/prompt"
                                                       `((sessionId . ,session-id) (prompt . #(((type . "text") (text . "hi"))))))))
                              (test-equal "stop reason"
                                          "end_turn"
                                          (prop-ref (prop-ref response 'result) 'stopReason))))
              (test-equal
                "assistant text streamed as message chunks"
                '("Hello from badb.")
                (acp-chunk-texts (car notifications) "agent_message_chunk"))
              (test-assert "user prompt reached the session"
                           (any
                             (lambda (message)
                               (string=? (or (prop-ref message 'content) "") "hi"))
                             (react-session-messages (acp-session-react session))))
              (system* "rm" "-rf" tmp-dir)))

;;;
;;; 5. Tool call notifications

;;;
(test-group "acp-prompt-reports-tool-calls"
            (let*
                ((tmp-dir (mkdtemp "/tmp/badb-acp-tools-XXXXXX"))
                 (notifications (list '()))
                 (server
                   (acp-capturing-server (acp-mock-client) tmp-dir notifications))
                 (target (string-append tmp-dir "/hello.txt"))
                 (_
                   (call-with-output-file target
                                          (lambda (port)
                                            (display "hello" port))))
                 (session (acp-new-session server #:cwd tmp-dir))
                 (session-id (acp-session-id session))
                 (turn 0)
                 (mock-poster
                   (lambda* (uri #:key body headers streaming?)
                            (set! turn (+ turn 1))
                            (values (build-response #:code 200)
                                    (open-input-string
                                      (if (= turn 1)
                                        (acp-sse
                                          (acp-tool-chunk "read_file" (acp-tool-arguments `(("path" . ,target)))))
                                        (acp-sse (acp-content-chunk "done"))))))))
              (parameterize ((current-http-post mock-poster))
                            (acp-handle-request server
                                                4
                                                "session/prompt"
                                                `((sessionId . ,session-id)
                                                  (prompt . #(((type . "text") (text . "read the file")))))))
              (test-equal "tool call announced"
                          '("read_file")
                          (acp-tool-call-titles (car notifications)))
              (test-equal "tool call completed"
                          1
                          (length (acp-updates (car notifications) "tool_call_update")))
              (test-assert "tool observation recorded"
                           (any
                             (lambda (message)
                               (string-contains (or (prop-ref message 'content) "") "hello"))
                             (react-session-messages (acp-session-react session))))
              (system* "rm" "-rf" tmp-dir)))

;;;
;;; 6. Agent -> client request round trip

;;;
(test-group "acp-client-request-round-trip"
            (let*
                ((in-ports (pipe))
                 (out-ports (pipe))
                 (server (make-acp-server #:in-port (car in-ports) #:out-port (cdr out-ports)))
                 (answered #f)
                 (thread
                   (call-with-new-thread
                     (lambda ()
                       (set! answered
                             (acp-client-request! server
                                                  "session/request_permission"
                                                  '((sessionId . "sess_1"))))))))
              (let loop ((n 0))
                        (when (and (not (hash-ref (acp-server-pending server) "acp-0" #f)) (< n 500))
                          (usleep 10000)
                          (loop (+ n 1))))
              (let*
                  ((line (read-line (car out-ports)))
                   (request (json-string->scm line))
                   (id (prop-ref request "id")))
                (test-equal "request id is namespaced" "acp-0" id)
                (test-equal "request method"
                            "session/request_permission"
                            (prop-ref request "method"))
                (acp-route-response! server
                                     `((jsonrpc . "2.0") (id . ,id)
                                       (result . ((outcome . ((outcome . "selected") (optionId . "allow"))))))))
              (join-thread thread)
              (test-assert "response routed back to the caller" answered)
              (test-equal "outcome selected"
                          "selected"
                          (prop-ref (prop-ref (prop-ref answered 'result) 'outcome) 'outcome))))

;;;
;;; 7. Permission gate

;;;
(test-group
  "acp-permission-gate-denies-unapproved-tools"
  (let*
      ((tmp-dir (mkdtemp "/tmp/badb-acp-perm-XXXXXX"))
       (in-ports (pipe))
       (out-ports (pipe))
       (server
         (make-acp-server #:client
                          (acp-mock-client)
                          #:workspace
                          tmp-dir
                          #:in-port
                          (car in-ports)
                          #:out-port
                          (cdr out-ports)
                          #:permission-policy
                          'ask))
       (target (string-append tmp-dir "/hello.txt"))
       (_
         (call-with-output-file target
                                (lambda (port)
                                  (display "hello" port))))
       (session (acp-new-session server #:cwd tmp-dir))
       (session-id (acp-session-id session))
       (turn 0)
       (mock-poster
         (lambda* (uri #:key body headers streaming?)
                  (set! turn (+ turn 1))
                  (values (build-response #:code 200)
                          (open-input-string
                            (if (= turn 1)
                              (acp-sse
                                (acp-tool-chunk "read_file" (acp-tool-arguments `(("path" . ,target)))))
                              (acp-sse (acp-content-chunk "done"))))))))
    (call-with-new-thread
      (lambda ()
        (let loop
          ()
          (let ((line (read-line (car out-ports))))
            (when (and line (not (eof-object? line)))
              (let*
                  ((message (json-string->scm line))
                   (method (prop-ref message "method"))
                   (id (prop-ref message "id")))
                (when
                    (and message (string=? method "session/request_permission"))
                  (acp-route-response! server
                                       `((jsonrpc . "2.0") (id . ,id)
                                         (result . ((outcome . ((outcome . "selected") (optionId . "reject"))))))))
                (loop)))))))
    (parameterize ((current-http-post mock-poster))
                  (let
                      ((response
                         (acp-handle-request server
                                             5
                                             "session/prompt"
                                             `((sessionId . ,session-id)
                                               (prompt . #(((type . "text") (text . "read the file"))))))))
                    (test-equal "prompt still completes"
                                "end_turn"
                                (prop-ref (prop-ref response 'result) 'stopReason))))
    (test-assert
      "tool call was blocked by the client"
      (any
        (lambda (message)
          (string-contains (or (prop-ref message 'content) "") "denied"))
        (react-session-messages (acp-session-react session))))
    (system* "rm" "-rf" tmp-dir)))

;;;
;;; 8. Cancellation

;;;
(test-group "acp-session-cancellation"
            (let*
                ((tmp-dir (mkdtemp "/tmp/badb-acp-cancel-XXXXXX"))
                 (server (make-acp-server #:client (acp-mock-client) #:workspace tmp-dir))
                 (session (acp-new-session server #:cwd tmp-dir)))
              (test-assert "not cancelled initially" (not (acp-session-cancel? session)))
              (acp-cancel-session! server session)
              (test-assert "cancel flag set" (acp-session-cancel? session))
              (test-assert "interrupt queued for the runtime"
                           (steering-has-interrupt? (acp-session-steering session)))
              (system* "rm" "-rf" tmp-dir)))

;;;
;;; 9. Serving over stdio

;;;
(test-group "acp-serves-json-rpc-over-stdio"
            (let* ((in-ports (pipe))
                   (out-ports (pipe))
                   (server (make-acp-server #:in-port (car in-ports)
                                            #:out-port (cdr out-ports))))
              (display
                "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}\n"
                (cdr in-ports))
              (force-output (cdr in-ports))
              (close-port (cdr in-ports))
              (define server-thread
                      (call-with-new-thread
                        (lambda ()
                          (acp-serve-stdio server))))
              (let*
                  ((line (read-line (car out-ports)))
                   (response (json-string->scm line)))
                (test-equal "jsonrpc envelope" "2.0" (prop-ref response "jsonrpc"))
                (test-equal "response id" 1 (prop-ref response "id"))
                (test-equal "response protocol version"
                            acp-protocol-version
                            (prop-ref (prop-ref response "result") "protocolVersion")))
              (join-thread server-thread 5)))

(spec-end)

(spec-exit)