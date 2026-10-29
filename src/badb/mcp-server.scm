;;; badb/mcp-server.scm — Minimal MCP (Model Context Protocol) server core.
;;;
;;; Serves MCP over stdio using line-delimited JSON-RPC 2.0, the same
;;; transport badb's own MCP client (badb mcp) speaks. Servers built on
;;; this core (the filetree MCP, the lema MCP) provide a TOOL list and
;;; handlers; the core owns the protocol: the initialize handshake,
;;; tools/list, tools/call dispatch, ping, and response framing.
;;;
;;; Every request gets exactly one response keyed by the request's id;
;;; notifications get none. Unknown methods get a JSON-RPC error. The
;;; server exits cleanly at EOF on stdin.

(define-module (badb mcp-server)
  #:use-module (json)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:export (mcp-serve-stdio))

;; An MCP server definition: an alist with
;;   ("name" . SERVER-NAME)
;;   ("version" . VERSION)
;;   ("tools" . #(((name . "tool-name")
;;                 (description . "...")
;;                 (inputSchema . ((type . "object") ...)))
;;                ...))
;;   ("handlers" . (("tool-name" . (lambda (args) STRING-RESULT)) ...))
;; Tool definitions use SYMBOL keys (guile-json serializes
;; symbol-keyed alists; JSON string keys only appear on parsed input).
;; Handlers return the text content for the tool result or raise an
;; exception, which the core converts into a tool error response.
;; Inbound tools/call ARGUMENTS arrive as guile-json's parse of the
;; request: an alist with string keys.
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

(define (write-response! resp)
  (catch #t
    (lambda ()
      (display (scm->json-string resp) (current-output-port))
      (newline (current-output-port))
      (force-output (current-output-port)))
    (lambda _ #f)))

(define (result-response id result)
  `((jsonrpc . "2.0") (id . ,id) (result . ,result)))

(define (error-response id code message)
  `((jsonrpc . "2.0") (id . ,id)
    (error . ((code . ,code) (message . ,message)))))

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
            (serverInfo . ((name . ,(assoc-ref server "name"))
                           (version . ,(assoc-ref server "version"))))))))
    ((string=? method "ping") (write-response! (result-response id '((status . "ok")))))
    ((string=? method "tools/list")
      (write-response!
        (result-response
          id
          `((tools . ,(or (assoc-ref server "tools") #()))))))
    ((string=? method "tools/call")
      (let* ((name (assoc-ref params "name"))
             (args (assoc-ref params "arguments"))
             (handler (and name (assoc-ref
                                 (assoc-ref server "handlers") name))))
        (if handler
          (let ((text (catch #t
                        (lambda () (handler (or args '())))
                        (lambda (key . args)
                          (format #f "tool error: ~a ~s" key args)))))
            (write-response!
              (result-response
                id
                `((content . #(((type . "text") (text . ,text))))))))
          (write-response!
            (error-response id -32602 (format #f "unknown tool: ~a" name))))))
    (else
      (write-response!
        (error-response id -32601 (format #f "unknown method: ~a" method))))))
