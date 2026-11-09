;; test/dummy-mcp-server.scm — dummy MCP server for the test suite.
;;
;; A deliberately minimal Model Context Protocol server speaking
;; JSON-RPC 2.0 over stdio, used by the unit tests to exercise the
;; real subprocess path of the MCP client: spawn, initialize,
;; tools/list, and tools/call round-trips against a separate OS
;; process, exactly the way production MCP servers run. It advertises
;; two tools:
;;
;;   echo (text) — replies with the text prefixed by "echo: "
;;   add (a b)   — replies with the sum of two numbers
;;
;; Every request gets a response, notifications are ignored, an
;; unknown method yields a JSON-RPC error, and EOF on stdin exits
;; cleanly. Standard error carries only diagnostics: badb redirects a
;; spawned server's stderr away from the TUI screen, so a test run
;; may leave notes in .badb/mcp.log (or /tmp/mcp.log).
;;
;; Run directly by the tests as:
;;   guile --no-auto-compile test/dummy-mcp-server.scm

(use-modules (json)
             (ice-9 format)
             (ice-9 rdelim)
             (srfi srfi-13))

(define %protocol-version "2024-11-05")

(define (tool-list)
        "Advertised tools: echo and add, with strict JSON Schema inputs."
        `((tools
            .
            #(((name . "echo")
               (description . "Echo the provided text back, prefixed with 'echo:'")
               (inputSchema
                 .
                 ((type . "object")
                  (properties . ((text . ((type . "string")))))
                  (required . #("text")))))
              ((name . "add")
               (description . "Add two numbers")
               (inputSchema
                 .
                 ((type . "object")
                  (properties . ((a . ((type . "number"))) (b . ((type . "number")))))
                  (required . #("a" "b")))))))))

(define (call-tool name arguments)
        "Produce the tools/call result for tool NAME with JSON ARGUMENTS."
        (cond
          ((string=? name "echo")
            (let ((text (or (assoc-ref arguments "text") "")))
              `((content
                  .
                  #(((type . "text") (text . ,(string-append "echo: " text)))))
                (isError . #f))))
          ((string=? name "add")
            (let ((a (or (assoc-ref arguments "a") 0))
                  (b (or (assoc-ref arguments "b") 0)))
              `((content
                  .
                  #(((type . "text") (text . ,(number->string (+ a b))))))
                (isError . #f))))
          (else
            `((content
                .
                #(((type . "text")
                   (text . ,(string-append "unknown tool: " name)))))
              (isError . #t)))))

(define (write-msg msg)
        "Serialize MSG as one JSON-RPC line and flush it."
        (display (scm->json-string msg))
        (newline)
        (force-output))

(define (respond id result)
        (write-msg `((jsonrpc . "2.0") (id . ,id) (result . ,result))))

(define (respond-error id code message)
        (write-msg
          `((jsonrpc . "2.0") (id . ,id)
            (error . ((code . ,code) (message . ,message))))))

(define (handle req)
        "Dispatch one parsed JSON-RPC request REQ. Notifications (no id
member) are silently ignored."
        (let* ((id-pair (assoc "id" req))
               (id (and id-pair (cdr id-pair)))
               (method (or (assoc-ref req "method") ""))
               (params (or (assoc-ref req "params") '())))
          (cond
            ((not id-pair) #f)
            ((string=? method "initialize")
              (respond id
                       `((protocolVersion . ,%protocol-version)
                         (capabilities . ((tools . ())))
                         (serverInfo . ((name . "dummy-mcp") (version . "0.1.0"))))))
            ((string=? method "tools/list") (respond id (tool-list)))
            ((string=? method "tools/call")
              (respond id (call-tool (or (assoc-ref params "name") "")
                                     (or (assoc-ref params "arguments") '()))))
            ((string=? method "ping") (respond id '()))
            (else (respond-error id -32601 (string-append "Method not found: " method))))))

(define (serve)
        "Read JSON-RPC lines from stdin until EOF."
        (let loop ((line (read-line)))
                  (unless (eof-object? line)
                    (let ((trimmed (string-trim line)))
                      (unless (string-null? trimmed)
                        (catch #t
                          (lambda () (handle (json-string->scm trimmed)))
                          (lambda (key . args)
                            (format (current-error-port)
                                    "dummy-mcp: bad request (~a): ~s~%" key args)))))
                    (loop (read-line)))))

(format (current-error-port)
        "dummy-mcp-server: reading JSON-RPC on stdin~%")
(serve)
