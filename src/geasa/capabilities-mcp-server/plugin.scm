;;; badb/capabilities/mcp-server.scm — Minimal MCP (Model Context Protocol) server core.
;;;
;;; Serves MCP over stdio using line-delimited JSON-RPC 2.0, the same
;;; transport badb's own MCP client (geasa capabilities-mcp plugin) speaks. Servers built on
;;; this core (the filetree MCP, etc.) provide a TOOL list and
;;; handlers; the core owns the protocol: the initialize handshake,
;;; tools/list, tools/call dispatch, ping, and response framing.
;;;
;;; Every request gets exactly one response keyed by the request's id;
;;; notifications get none. Unknown methods get a JSON-RPC error. The
;;; server exits cleanly at EOF on stdin.

(define-module (geasa capabilities-mcp-server plugin)
  #:use-module (badb kernel ipc)
  #:use-module (json)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-9)
  #:export (<mcp-server>
            make-mcp-server
            mcp-server?
            mcp-server-name
            mcp-server-version
            mcp-server-tools
            mcp-server-handlers
            mcp-serve-stdio
            write-response!
            result-response
            error-response
            handle-message))

;; An MCP server record descriptor and constructor
(define-record-type <mcp-server>
  (make-mcp-server name version tools handlers)
  mcp-server?
  (name mcp-server-name)
  (version mcp-server-version)
  (tools mcp-server-tools)
  (handlers mcp-server-handlers))

(define (server-ref server key)
  "Access a field in SERVER, supporting both <mcp-server> records and alists."
  (if (mcp-server? server)
      (let ((k (if (string? key) (string->symbol key) key)))
        (case k
          ((name) (mcp-server-name server))
          ((version) (mcp-server-version server))
          ((tools) (mcp-server-tools server))
          ((handlers) (mcp-server-handlers server))
          (else #f)))
      (or (assoc-ref server key)
          (if (string? key)
              (assoc-ref server (string->symbol key))
              (assoc-ref server (symbol->string key))))))

(define (write-response! resp)
  (ipc-write-response! resp (current-output-port)))

(define (result-response id result)
  (ipc-result-response id result))

(define (error-response id code message)
  (ipc-error-response id code message))

(define (handle-message server id method params)
  (cond
    ;; Notifications (no id): no response, nothing to do.
    ((not id) #t)
    ((string=? method "initialize")
      (write-response!
        (result-response
          id
          `((protocolVersion . "2024-11-05")
            (capabilities . ((tools . ((listChanged . #f)))))
            (serverInfo . ((name . ,(server-ref server "name"))
                           (version . ,(server-ref server "version"))))))))
    ((string=? method "ping") (write-response! (result-response id '((status . "ok")))))
    ((string=? method "tools/list")
      (write-response!
        (result-response
          id
          `((tools . ,(or (server-ref server "tools") #()))))))
    ((string=? method "tools/call")
      (let* ((name (assoc-ref params "name"))
             (args (assoc-ref params "arguments"))
             (handlers (server-ref server "handlers"))
             (handler (and name handlers (assoc-ref handlers name))))
        (if handler
          (let ((text (catch #t
                        (lambda () (handler (or args '())))
                        (lambda (key . err-args)
                          (format #f "tool error: ~a ~s" key err-args)))))
            (write-response!
              (result-response
                id
                `((content . #(((type . "text") (text . ,text))))))))
          (write-response!
            (error-response id -32602 (format #f "unknown tool: ~a" name))))))
    (else
      (write-response!
        (error-response id -32601 (format #f "unknown method: ~a" method))))))

(define (mcp-serve-stdio server)
  "Serve SERVER over stdio forever (until stdin EOF), speaking
JSON-RPC 2.0 MCP: initialize, notifications/initialized, ping,
tools/list, and tools/call."
  (let loop ()
    (let ((line (catch #t (lambda () (read-line (current-input-port)))
                  (lambda _ #f))))
      (cond
        ((or (not line) (eof-object? line)) #t)
        ((string-null? (string-trim-both line)) (loop))
        (else
          (let* ((msg (catch #t
                        (lambda () (json-string->scm (string-trim-both line)))
                        (lambda _ #f)))
                 (id (and msg (assoc-ref msg "id")))
                 (method (and msg (assoc-ref msg "method"))))
            (when (and msg (string? method))
              (handle-message server id method
                              (or (assoc-ref msg "params") '())))
            (loop)))))))
