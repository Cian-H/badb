;;; (geasa capabilities-lsp plugin) — LSP Diagnostic Feedback
;;;
;;; Bridges the kernel to Language Servers (LSP 3.17 over stdio): spawns
;;; a server, performs the initialize handshake, opens text documents
;;; and collects `textDocument/publishDiagnostics' results so agents get
;;; compiler-grade feedback on generated code.
;;;
;;; Design notes:
;;;  * Uses Guile's (json) module for JSON-RPC serialization and parsing.
;;;  * A missing server binary degrades to a structured "server
;;;    unavailable" result instead of a crash.

(define-module (geasa capabilities-lsp plugin)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 textual-ports)
               #:use-module (json)
               #:use-module (badb kernel registry)
               #:use-module (geasa foundation-util plugin)
               #:use-module (srfi srfi-1)
               #:export (make-lsp-client!
                          lsp-shutdown!
                          lsp-did-open!
                          lsp-collect-diagnostics!
                          lsp-diagnostics
                          register-lsp-tools!))

;; ---- LSP transport -------------------------------------------------------

(define (%header-value headers name)
        (let loop ((hs headers))
                  (cond
                    ((null? hs) #f)
                    (else
                      (let* ((line (car hs))
                             (colon (string-index line #\:)))
                        (if (and colon
                                 (string=? (string-trim (string-take line colon)) name))
                          (string-trim (string-take-right line
                                                          (- (string-length line) (+ colon 1))))
                          (loop (cdr hs))))))))

(define (%read-message! port)
        "Read one LSP message; returns its JSON body as a string, or #f on EOF."
        (let loop ((headers '()))
                  (let ((line (read-line port)))
                    (cond
                      ((eof-object? line) #f)
                      ((or (string=? line "") (string=? line "\r"))
                        (let* ((n (string->number
                                    (or (%header-value headers "content-length") "0"))))
                          (if (and n (> n 0))
                            (let ((body (make-string n)))
                              (do ((i 0 (+ i 1)))
                                  ((= i n) body)
                                (let ((c (read-char port)))
                                  (string-set! body i (if (char? c) c #\space)))))
                            "")))
                      (else (loop (cons line headers)))))))

(define (%lsp-write! client body)
        "Frame and send BODY (a complete JSON string) over the client pipe."
        (let ((port (cdr (assq 'pipe client))))
          (display (format #f "Content-Length: ~a\r\n\r\n~a"
                           (string-length body) body)
                   port)
          (force-output port)))

(define (%lsp-request! client method params)
        (%lsp-write! client
                     (scm->json-string
                       `((jsonrpc . "2.0")
                         (id . 1)
                         (method . ,method)
                         (params . ,params)))))

(define (%lsp-notify! client method params)
        (%lsp-write! client
                     (scm->json-string
                       `((jsonrpc . "2.0")
                         (method . ,method)
                         (params . ,params)))))

;; ---- client lifecycle ----------------------------------------------------

(define (make-lsp-client! server-cmd)
        "Spawn SERVER-CMD (list of argv strings) and perform the LSP
initialize handshake.  Returns a client handle."
        (let ((pipe (apply open-pipe* OPEN_BOTH server-cmd)))
          (%lsp-request! pipe "initialize" '((processId . #f) (rootUri . #f)))
          ;; Drain the initialize response before any document traffic.
          (read-line (open-input-string (%drain-response! pipe)))
          `((pipe . ,pipe))))

(define (%drain-response! pipe)
        ;; Read one message body; ignore its contents.
        (let ((msg (%read-message! pipe)))
          (or msg "")))

(define (lsp-shutdown! client)
        "Send shutdown/exit and close the transport."
        (false-if-exception
          (begin
            (%lsp-request! client "shutdown" '())
            (%lsp-notify! client "exit" '())
            (close-port (cdr (assq 'pipe client))))))

;; ---- document diagnostics --------------------------------------------------

(define (lsp-did-open! client uri text)
        "Publish an open-document notification for URI with contents TEXT."
        (%lsp-notify! client "textDocument/didOpen"
                      `((textDocument . ((uri . ,uri)
                                         (languageId . "c")
                                         (version . 1)
                                         (text . ,text))))))

(define* (lsp-collect-diagnostics! client #:key (timeout 5.0))
         "Read server messages until a publishDiagnostics notification or
TIMEOUT seconds elapse.  Returns the diagnostics list, or #f when the
server closed the transport first."
         (let ((deadline (+ (get-internal-real-time)
                            (* 1.0 internal-time-units-per-second
                               (if (real? timeout) timeout 5.0)))))
           (let loop ()
                     (cond
                       ((>= (get-internal-real-time) deadline) '())
                       (else
                         (let ((msg (%read-message! (cdr (assq 'pipe client)))))
                           (cond
                             ((eof-object? msg) '())
                             ((not msg) '())
                             ((%diagnostics-of msg) => values)
                             (else (loop)))))))))

(define (%diagnostics-of body)
        (and (string? body)
             (false-if-exception
               (let* ((obj (json-string->scm body))
                      (params (and (pair? obj) (assoc-ref obj "params")))
                      (diags (and (pair? params) (assoc-ref params "diagnostics"))))
                 (cond
                   ((vector? diags) (vector->list diags))
                   ((list? diags) diags)
                   (else #f))))))

;; ---- public tool surface ---------------------------------------------------

(define* (lsp-diagnostics file-path #:key (lang "c"))
         "Analyse FILE-PATH with the configured language server and return
diagnostics as a plain string report (one finding per line)."
         (catch #t
           (lambda ()
             (let* ((text (call-with-input-file file-path get-string-all))
                    (client (make-lsp-client! (list "clangd"))))
               (lsp-did-open! client file-path text)
               (let ((diags (lsp-collect-diagnostics! client)))
                 (lsp-shutdown! client)
                 (if (null? diags)
                   "no diagnostics"
                   (string-join (map %format-diagnostic diags) "\n")))))
           (lambda args
             "lsp: server unavailable or document unreadable")))

(define (%format-diagnostic d)
        (format #f "line ~a, col ~a: ~a"
                (assoc-ref d "line")
                (assoc-ref d "column")
                (assoc-ref d "message")))

;; ---- tool registration ------------------------------------------------------

(define (register-lsp-tools! reg)
        "Register the lsp capability tools in registry REG."
        (registry-attach!
          reg "lsp_diagnostics"
          (lambda (args)
            (with-tool-args args ((file file "")
                                  (lang lang "c"))
                            (lsp-diagnostics file #:lang lang)))
          #:metadata %lsp-diagnostics-schema))

(define %lsp-diagnostics-schema
        `((name . "lsp_diagnostics")
          (description . "Run the language server over a source file and return diagnostics.")
          (parameters . ((type . "object")
                         (properties . ((file . ((type . "string")))
                                        (lang . ((type . "string")))))
                         (required . #("file"))))
          (_badb_defer . #t)))
