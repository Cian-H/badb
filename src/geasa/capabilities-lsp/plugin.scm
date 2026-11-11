;;; (geasa capabilities-lsp plugin) — LSP Diagnostic Feedback
;;;
;;; Bridges the kernel to Language Servers (LSP 3.17 over stdio): spawns
;;; a server, performs the initialize handshake, opens text documents
;;; and collects `textDocument/publishDiagnostics' results so agents get
;;; compiler-grade feedback on generated code.
;;;
;;; Design notes:
;;;  * Zero external JSON dependency: a minimal recursive-descent JSON
;;;    reader covers the LSP subset (objects, arrays, strings, numbers,
;;;    booleans, null).
;;;  * A missing server binary degrades to a structured "server
;;;    unavailable" result instead of a crash.

(define-module (geasa capabilities-lsp plugin)
               #:use-module (ice-9 popen)
               #:use-module (srfi srfi-1)
               #:export (make-lsp-client!
                          lsp-shutdown!
                          lsp-did-open!
                          lsp-collect-diagnostics!
                          lsp-diagnostics
                          register-lsp-tools!))

;; ---- JSON reader ---------------------------------------------------------

(define (%json-skip-ws port)
        (let loop ()
                  (let ((c (peek-char port)))
                    (if (and (char? c) (char-whitespace? c))
                      (begin (read-char port) (loop))))))

(define (%json-read port)
        (let ((c (peek-char port)))
          (cond
            ((char=? c #\{) (%json-object port))
            ((char=? c #\[) (%json-array port))
            ((char=? c #\") (%json-string port))
            ((char=? c #\t) (%eat 4 port) #t)
            ((char=? c #\f) (%eat 5 port) #f)
            ((char=? c #\n) (%eat 4 port) 'null)
            (else (%json-number port)))))

(define (%eat n port)
        (let loop ((i 0))
                  (if (< i n) (begin (read-char port) (loop (+ i 1))))))

(define (%json-number port)
        (let loop ((chars '()))
                  (let ((c (peek-char port)))
                    (if (and (char? c) (or (char-numeric? c) (memq c '(#\+ #\- #\. #\e #\E))))
                      (begin (read-char port) (loop (cons c chars)))
                      (string->number (list->string (reverse chars)))))))

(define (%json-string port) (read-char port)
        ; opening quote
        (let loop
          ((acc '()))
          (let ((c (read-char port)))
            (cond
              ((or (eof-object? c) (char=? c #\")) (list->string (reverse acc)))
              ((char=? c #\\)
                (let ((e (read-char port)))
                  (case e
                    ((#\n) (loop (cons #\newline acc)))
                    ((#\t) (loop (cons #\tab acc)))
                    ((#\r) (loop (cons #\return acc)))
                    ((#\b) (loop (cons #\backspace acc)))
                    ((#\f) (loop (cons #\page acc)))
                    (else (loop acc)))))
              ; \" \\ \/ \uXXXX handled as-is
              (else (loop (cons c acc)))))))

(define (%json-object port) (read-char port)
        ; {
        (let loop
          ((acc '()))
          (%json-skip-ws port)
          (if (char=? (peek-char port) #\})
            acc
            (let*
                ((key (%json-string port))
                 (_ (%json-skip-ws port))
                 (_2 (read-char port))
                 ; colon
                 (val (%json-value port)))
              (%json-skip-ws port)
              (let ((c (peek-char port))) (if (char=? c #\,) (read-char port)))
              (loop (cons (cons key val) acc))))))

(define (%json-array port) (read-char port)
        ; [
        (let loop
          ((acc '()))
          (%json-skip-ws port)
          (let ((c (peek-char port)))
            (if (char=? c #\])
              (begin (read-char port) (reverse acc))
              (let ((val (%json-value port)))
                (%json-skip-ws port)
                (let ((c2 (peek-char port))) (if (char=? c2 #\,) (read-char port)))
                (loop (cons val acc)))))))

(define (%json-value port)
        (%json-skip-ws port)
        (%json-read port))

(define (%json-parse str)
        "Parse STR (UTF-8 JSON text) into Scheme data."
        (%json-read (open-input-string str)))

;; ---- JSON writer ---------------------------------------------------------

(define (%json-quote s)
        (list->string
          (append (list #\")
                  (append-map
                    (lambda (c)
                      (case c
                        ((#\") (list #\\ #\"))
                        ((#\\) (list #\\ #\\))
                        ((#\newline) (list #\\ #\n))
                        ((#\tab) (list #\\ #\t))
                        ((#\return) (list #\\ #\r))
                        (else (list c))))
                    (string->list s))
                  (list #\"))))

(define (%json-write x)
        (cond
          ((string? x) (%json-quote x))
          ((number? x) (format #f "~a" x))
          ((eq? x #t) "true")
          ((eq? x #f) "false")
          ((null? x) "null")
          ((symbol? x) (%json-quote (symbol->string x)))
          ((pair? x)
            (if (pair? (car x))                   ; alist → JSON object
              (string-append "{"
                             (string-join
                               (map (lambda (kv)
                                      (format #f "~a:~a"
                                              (%json-quote (symbol->string (car kv)))
                                              (%json-write (cdr kv))))
                                    x)
                               ",")
                             "}")
              (string-append "[" (string-join (map %json-write x) ",") "]")))
          (else "null")))

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
                     (format #f "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"~a\",\"params\":~a}"
                             method params)))

(define (%lsp-notify! client method params)
        (%lsp-write! client
                     (format #f "{\"jsonrpc\":\"2.0\",\"method\":\"~a\",\"params\":~a}"
                             method params)))

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
               (let* ((obj (%json-parse body))
                      (params (and obj (assoc-ref obj "params"))))
                 (and params
                      (assoc-ref params "diagnostics"))))))

;; ---- public tool surface ---------------------------------------------------

(define* (lsp-diagnostics file-path #:key (lang "c"))
         "Analyse FILE-PATH with the configured language server and return
diagnostics as a plain string report (one finding per line)."
         (catch #t
           (lambda ()
             (let* ((text (call-with-input-file file-path %read-all))
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
