;;; badb — Model Endpoint API (HTTP + SSE, JSON)
;;;
;;; Client implementation for HTTP + SSE model endpoints compatible with
;;; OpenAI, Anthropic, local Ollama, and OpenRouter streaming APIs.

(define-module (badb model)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (web http)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<model-client>
            make-model-client
            model-client?
            model-client-endpoint
            model-client-api-key
            model-client-model
            format-chat-request
            parse-sse-line
            read-sse-events
            extract-chat-delta
            extract-chat-delta-item
            extract-chat-content
            extract-chat-tool-calls
            current-http-post
            model-complete
            model-stream))

;;;
;;; Client Record Type
;;;

(define-record-type <model-client>
  (%make-model-client endpoint api-key model)
  model-client?
  (endpoint model-client-endpoint)
  (api-key  model-client-api-key)
  (model    model-client-model))

(define (make-model-client . args)
  "Create a model client record.
Can be invoked with keyword arguments:
  #:endpoint - HTTP(S) endpoint URL (string)
  #:api-key  - Optional API key string (or #f)
  #:model    - Optional model name string (or #f)
or with positional arguments:
  (make-model-client endpoint [api-key [model]])"
  (let loop ((rest args)
             (endpoint "http://localhost:11434/v1/chat/completions")
             (api-key #f)
             (model "default"))
    (cond
     ((null? rest)
      (%make-model-client endpoint api-key model))
     ((keyword? (car rest))
      (let ((k (car rest))
            (v (if (pair? (cdr rest)) (cadr rest) #f))
            (tail (if (pair? (cdr rest)) (cddr rest) '())))
        (case k
          ((#:endpoint) (loop tail v api-key model))
          ((#:api-key)  (loop tail endpoint v model))
          ((#:model)    (loop tail endpoint api-key v))
          (else (loop tail endpoint api-key model)))))
     (else
      (let ((ep (car args))
            (ak (if (pair? (cdr args)) (cadr args) #f))
            (md (if (and (pair? (cdr args)) (pair? (cddr args))) (caddr args) "default")))
        (%make-model-client ep ak md))))))

;;;
;;; Chat Request Formatting
;;;

(define (symbol-or-string->string x)
  (cond
   ((symbol? x) (symbol->string x))
   ((string? x) x)
   (else (object->string x))))

(define (single-message-alist? x)
  (and (pair? x)
       (pair? (car x))
       (or (assoc-ref x "role")
           (assoc-ref x 'role)
           (assoc-ref x "content")
           (assoc-ref x 'content))))

(define (normalize-message msg)
  (cond
   ;; Alist of key-value pairs representing an object:
   ;; e.g. ((role . "user") (content . "hello")) or (("role" . "user") ("content" . "hello"))
   ((and (pair? msg) (pair? (car msg)))
    (map (lambda (pair)
           (cons (symbol-or-string->string (car pair))
                 (cdr pair)))
         msg))
   ;; Pair of (role . content):
   ;; e.g. (user . "hello") or ("user" . "hello")
   ((and (pair? msg) (not (pair? (car msg))))
    (list (cons "role" (symbol-or-string->string (car msg)))
          (cons "content" (cdr msg))))
   ;; Bare string: assume user role
   ((string? msg)
    (list (cons "role" "user")
          (cons "content" msg)))
   (else msg)))

(define (normalize-messages msgs)
  (cond
   ((vector? msgs)
    (list->vector (map normalize-message (vector->list msgs))))
   ((single-message-alist? msgs)
    (vector (normalize-message msgs)))
   ((list? msgs)
    (list->vector (map normalize-message msgs)))
   ((string? msgs)
    (vector (list (cons "role" "user") (cons "content" msgs))))
   (else msgs)))

(define* (format-chat-request messages
                              #:key
                              (model #f)
                              (temperature #f)
                              (stream #f)
                              (tools #f)
                              (tool-choice #f)
                              (max-tokens #f)
                              (as-alist #f))
  "Format a chat completion request payload.
MESSAGES can be a string prompt, a list of message alists, a single message alist,
a list of (role . text) pairs, or a vector of messages.
Optional keyword arguments:
  #:model       - Model name string
  #:temperature - Float temperature value
  #:stream      - Boolean indicating streaming mode
  #:tools       - List or vector of tool specification alists
  #:tool-choice - Tool choice policy string or object (e.g. \"auto\")
  #:max-tokens  - Integer maximum tokens to generate
  #:as-alist    - When true, returns the Scheme alist instead of JSON string."
  (let* ((msgs (normalize-messages messages))
         (entries (list (cons "messages" msgs)))
         (entries (if model (cons (cons "model" model) entries) entries))
         (entries (if (number? temperature)
                      (cons (cons "temperature" temperature) entries)
                      entries))
         (entries (if (number? max-tokens)
                      (cons (cons "max_tokens" max-tokens) entries)
                      entries))
         (entries (if (and tools (or (pair? tools) (vector? tools)))
                      (cons (cons "tools" (if (list? tools) (list->vector tools) tools)) entries)
                      entries))
         (entries (if tool-choice
                      (cons (cons "tool_choice" tool-choice) entries)
                      entries))
         (entries (cons (cons "allow_missing_thinking" #t) entries))
         (entries (cons (cons "stream" (if stream #t #f)) entries))
         (alist (reverse entries)))
    (if as-alist
        alist
        (scm->json-string alist))))

;;;
;;; Server-Sent Events (SSE) Parsing
;;;

(define (clean-sse-line line)
  (if (string? line)
      (string-trim-right line (lambda (c) (or (char=? c #\return) (char=? c #\newline))))
      ""))

(define (parse-sse-line line)
  "Parse a single SSE line.
Returns:
  - 'done if the line indicates stream completion ([DONE])
  - string payload if the line is a data event (\"data: ...\")
  - #f for empty lines, comments (\": ...\"), or non-data lines."
  (let ((cleaned (clean-sse-line line)))
    (cond
     ;; Empty line
     ((string-null? cleaned) #f)
     ;; Comment line (starts with colon per SSE spec)
     ((string-prefix? ":" cleaned) #f)
     ;; Detect bare [DONE]
     ((string=? cleaned "[DONE]") 'done)
     ;; Data lines
     ((string-prefix? "data:" cleaned)
      (let* ((rest (substring cleaned 5))
             (payload (if (and (> (string-length rest) 0)
                               (char=? (string-ref rest 0) #\space))
                          (substring rest 1)
                          rest)))
        (if (string=? payload "[DONE]")
            'done
            payload)))
     ;; Other SSE fields (event, id, retry)
     (else #f))))

(define (read-sse-events port callback)
  "Read lines from PORT, parse SSE 'data: ' events, and invoke
(CALLBACK data-json-or-string) for each event until EOF or '[DONE]'.
Returns 'done when [DONE] is encountered, or 'eof upon reaching EOF."
  (let loop ()
    (let ((line (read-line port)))
      (cond
       ((eof-object? line)
        'eof)
       (else
        (let* ((cleaned (clean-sse-line line))
               (parsed (parse-sse-line line)))
          (cond
           ((eq? parsed 'done)
            'done)
           ((string? parsed)
            (let ((data (catch #t
                          (lambda () (json-string->scm parsed))
                          (lambda _ parsed))))
              (callback data)
              (loop)))
           ((and (string-prefix? "{" cleaned)
                 (or (string-contains cleaned "\"error\"")
                     (string-contains cleaned "'error'")))
            (let ((err-obj (catch #t
                             (lambda () (json-string->scm cleaned))
                             (lambda _ #f))))
              (error "SSE stream received error response"
                     (or (and (pair? err-obj)
                              (let ((err-inner (assoc-ref err-obj "error")))
                                (and (pair? err-inner) (assoc-ref err-inner "message"))))
                         cleaned))))
           (else
            (loop)))))))))

;;;
;;; Content and Delta Extractors
;;;

(define (json-ref obj key)
  "Lookup KEY in OBJ where KEY may be a string or symbol, and OBJ is an alist."
  (if (not (pair? obj))
      #f
      (or (assoc-ref obj key)
          (and (string? key) (assoc-ref obj (string->symbol key)))
          (and (symbol? key) (assoc-ref obj (symbol->string key))))))

(define (extract-chat-delta chunk)
  "Extract the textual delta string from an SSE chunk (parsed JSON alist or JSON string).
Supports OpenAI, Anthropic, Ollama, and OpenRouter formats.
Returns the string delta, or #f if no content is found."
  (let ((obj (if (string? chunk)
                 (catch #t (lambda () (json-string->scm chunk)) (lambda _ chunk))
                 chunk)))
    (if (not (pair? obj))
        (if (string? obj) obj #f)
        (or
         ;; OpenAI style: choices[0].delta.content or reasoning_content or text
         (let ((choices (json-ref obj "choices")))
           (and (vector? choices)
                (> (vector-length choices) 0)
                (let* ((choice (vector-ref choices 0))
                       (delta (json-ref choice "delta")))
                  (and (pair? delta)
                       (or (json-ref delta "content")
                           (json-ref delta "reasoning_content")
                           (json-ref delta "text"))))))
         ;; Anthropic style: delta.text or delta.content
         (let ((delta (json-ref obj "delta")))
           (and (pair? delta)
                (or (json-ref delta "text")
                    (json-ref delta "content"))))
         ;; Ollama native style: message.content
         (let ((msg (json-ref obj "message")))
           (and (pair? msg)
                (json-ref msg "content")))
         ;; Direct content or text
         (json-ref obj "content")
         (json-ref obj "text")))))

(define (extract-chat-delta-item chunk)
  "Extract structured delta info from an SSE chunk (parsed JSON alist or JSON string).
Returns a pair '(thought . text) or '(content . text) or #f."
  (let ((obj (if (string? chunk)
                 (catch #t (lambda () (json-string->scm chunk)) (lambda _ chunk))
                 chunk)))
    (if (not (pair? obj))
        (and (string? obj) (cons 'content obj))
        (or
         ;; OpenAI style: choices[0].delta
         (let ((choices (json-ref obj "choices")))
           (and (vector? choices)
                (> (vector-length choices) 0)
                (let* ((choice (vector-ref choices 0))
                       (delta (json-ref choice "delta")))
                  (and (pair? delta)
                       (cond
                        ((or (json-ref delta "reasoning_content")
                             (json-ref delta "reasoning"))
                         => (lambda (rc) (cons 'thought rc)))
                        ((json-ref delta "content")
                         => (lambda (c) (cons 'content c)))
                        ((json-ref delta "text")
                         => (lambda (t) (cons 'content t)))
                        (else #f))))))
         ;; Anthropic style: delta.thinking or delta.text or delta.content
         (let ((delta (json-ref obj "delta")))
           (and (pair? delta)
                (cond
                 ((or (json-ref delta "thinking") (json-ref delta "thought"))
                  => (lambda (th) (cons 'thought th)))
                 ((or (json-ref delta "text") (json-ref delta "content"))
                  => (lambda (c) (cons 'content c)))
                 (else #f))))
         ;; Ollama style: message.thinking or message.content
         (let ((msg (json-ref obj "message")))
           (and (pair? msg)
                (cond
                 ((or (json-ref msg "reasoning_content") (json-ref msg "thinking"))
                  => (lambda (rc) (cons 'thought rc)))
                 ((json-ref msg "content")
                  => (lambda (c) (cons 'content c)))
                 (else #f))))
         ;; Direct content or text
         (cond
          ((or (json-ref obj "reasoning_content") (json-ref obj "thinking"))
           => (lambda (rc) (cons 'thought rc)))
          ((json-ref obj "content")
           => (lambda (c) (cons 'content c)))
          ((json-ref obj "text")
           => (lambda (t) (cons 'content t)))
          (else #f))))))

(define (clean-json-string val)
  "Convert JSON string value to string or #f (treating null/nil as #f)."
  (cond
   ((string? val) val)
   ((or (not val) (eq? val 'null) (null? val)) #f)
   (else (format #f "~a" val))))

(define (extract-chat-content response-obj)
  "Extract the assistant message text from a non-streaming chat response.
Supports OpenAI, Anthropic, Ollama, and OpenRouter formats.
Returns the string content, or #f if not found."
  (let ((obj (if (string? response-obj)
                 (catch #t (lambda () (json-string->scm response-obj)) (lambda _ response-obj))
                 response-obj)))
    (if (not (pair? obj))
        (clean-json-string obj)
        (clean-json-string
         (or
          ;; OpenAI style: choices[0].message.content
          (let ((choices (json-ref obj "choices")))
            (and (vector? choices)
                 (> (vector-length choices) 0)
                 (let* ((c0 (vector-ref choices 0))
                        (msg (json-ref c0 "message")))
                   (or (and (pair? msg)
                            (let ((c (json-ref msg "content")))
                              (if (or (not c) (eq? c 'null) (null? c))
                                  (or (json-ref msg "reasoning_content")
                                      (json-ref msg "reasoning"))
                                  c)))
                       (json-ref c0 "text")))))
          ;; Anthropic style: content[0].text or content string
          (let ((content (json-ref obj "content")))
            (cond
             ((string? content) content)
             ((and (vector? content) (> (vector-length content) 0))
              (let ((b0 (vector-ref content 0)))
                (and (pair? b0) (json-ref b0 "text"))))
             (else #f)))
          ;; Ollama style: message.content or response
          (let ((msg (json-ref obj "message")))
            (and (pair? msg) (json-ref msg "content")))
          (json-ref obj "response")
          ;; Direct content or text
          (json-ref obj "content")
          (json-ref obj "text"))))))

(define (extract-chat-tool-calls response-obj)
  "Extract tool calls from a chat completion response (parsed JSON alist or JSON string).
Returns a list of tool call alists: ((id . \"...\") (name . \"...\") (arguments . ...)), or '() if none found."
  (let ((obj (if (string? response-obj)
                 (catch #t (lambda () (json-string->scm response-obj)) (lambda _ response-obj))
                 response-obj)))
    (if (not (pair? obj))
        '()
        (let* ((choices (json-ref obj "choices"))
               (msg (and (vector? choices)
                         (> (vector-length choices) 0)
                         (json-ref (vector-ref choices 0) "message")))
               (raw-calls (or (and (pair? msg) (json-ref msg "tool_calls"))
                              (json-ref obj "tool_calls"))))
          (cond
           ((vector? raw-calls)
            (map (lambda (tc)
                   (let* ((fn (json-ref tc "function"))
                          (id (or (json-ref tc "id") "call_0"))
                          (name (if (pair? fn) (or (json-ref fn "name") "") ""))
                          (raw-args (if (pair? fn) (or (json-ref fn "arguments") "{}") "{}"))
                          (parsed-args (cond
                                        ((string? raw-args)
                                         (catch #t
                                           (lambda () (json-string->scm raw-args))
                                           (lambda _ raw-args)))
                                        (else raw-args))))
                     `((id . ,id)
                       (name . ,name)
                       (arguments . ,parsed-args))))
                 (vector->list raw-calls)))
           ((list? raw-calls)
            raw-calls)
           (else '()))))))

;;;
;;; HTTP Request Execution
;;;

(define* (curl-http-post uri #:key body headers (streaming? #f))
  "Execute an HTTP POST using curl subprocess. Used as a transparent fallback
when Guile GnuTLS is not available for HTTPS requests.
Writes payload to a self-cleaning temporary file to handle arbitrarily large payloads
without argument list limits, captures real HTTP status codes, and silences curl stderr."
  (let* ((url (if (string? uri) uri (uri->string uri)))
         (header-args
          (append-map
           (lambda (h)
             (let* ((key (car h))
                    (val (cdr h))
                    (str-val (cond
                              ((string? val) val)
                              ((and (eq? key 'authorization) (pair? val))
                               (format #f "Bearer ~a" (cadr val)))
                              ((eq? key 'content-type)
                               "application/json; charset=utf-8")
                              ((eq? key 'accept)
                               (if streaming? "text/event-stream" "application/json"))
                              (else (format #f "~a" val)))))
               (list "-H" (format #f "~a: ~a" key str-val))))
           (or headers '())))
         (body-str (cond
                    ((string? body) body)
                    ((bytevector? body) (utf8->string body))
                    (else "")))
         (body-tmpl (string-copy "/tmp/badb-req-XXXXXX"))
         (body-port (mkstemp! body-tmpl)))
    (display body-str body-port)
    (close-port body-port)
    (if streaming?
        (let* ((cmd (append (list "sh" "-c"
                                  "TEMP=\"$1\"; shift; curl -s -N \"$@\" --data-binary @\"$TEMP\" 2>/dev/null; rm -f \"$TEMP\""
                                  "sh" body-tmpl "-X" "POST" url)
                            header-args))
               (pipe (apply open-pipe* OPEN_READ cmd)))
          (values (build-response #:code 200) pipe))
        (let* ((cmd (append (list "sh" "-c"
                                  "TEMP=\"$1\"; shift; curl -s -w \"\\n%{http_code}\" \"$@\" --data-binary @\"$TEMP\" 2>/dev/null; rm -f \"$TEMP\""
                                  "sh" body-tmpl "-X" "POST" url)
                            header-args))
               (pipe (apply open-pipe* OPEN_READ cmd))
               (out (read-string pipe))
               (status (close-pipe pipe))
               (trimmed (or out ""))
               (last-nl (string-rindex trimmed #\newline))
               (code-str (if last-nl (substring trimmed (+ last-nl 1)) "200"))
               (body-text (if last-nl (substring trimmed 0 last-nl) trimmed))
               (status-code (or (string->number (string-trim-both code-str)) 200)))
          (values (build-response #:code status-code) body-text)))))

(define* (default-http-post uri #:key body headers (streaming? #f))
  "Dispatch HTTP POST to web client, falling back to curl if gnutls is not available."
  (catch #t
    (lambda ()
      (http-post uri #:body body #:headers headers #:streaming? streaming?))
    (lambda (key . args)
      (if (or (eq? key 'gnutls-not-available)
              (and (pair? args) (string? (car args)) (string-contains (car args) "gnutls")))
          (curl-http-post uri #:body body #:headers headers #:streaming? streaming?)
          (apply throw key args)))))

(define current-http-post
  (make-parameter default-http-post))

(define (build-headers api-key streaming?)
  (let ((base (list (cons 'content-type
                          (parse-header 'content-type "application/json; charset=utf-8"))
                    (cons 'accept
                          (parse-header 'accept
                                        (if streaming?
                                            "text/event-stream"
                                            "application/json"))))))
    (if (and api-key (string? api-key) (not (string-null? api-key)))
        (cons (cons 'authorization
                    (parse-header 'authorization (string-append "Bearer " api-key)))
              base)
        base)))

(define (resolve-endpoint-uri endpoint)
  "Normalize ENDPOINT string or uri to an effective chat completions URI.
If the path does not end with /chat/completions, /completions, /messages, or /chat,
append /chat/completions."
  (let* ((uri (if (string? endpoint) (string->uri endpoint) endpoint))
         (path (or (uri-path uri) "/")))
    (if (or (string-suffix? "/chat/completions" path)
            (string-suffix? "/completions" path)
            (string-suffix? "/messages" path)
            (string-suffix? "/chat" path))
        uri
        (let* ((clean-path (string-trim-right path #\/))
               (new-path (string-append clean-path "/chat/completions")))
          (build-uri (uri-scheme uri)
                     #:userinfo (uri-userinfo uri)
                     #:host (uri-host uri)
                     #:port (uri-port uri)
                     #:path new-path
                     #:query (uri-query uri)
                     #:fragment (uri-fragment uri))))))

(define* (model-complete client messages
                        #:key
                        (temperature 0.7)
                        (max-tokens #f)
                        (tools #f)
                        (tool-choice #f)
                        (raw? #f)
                        (http-poster #f))
  "Perform a non-streaming chat completion request.
Returns the response content text string, or parsed JSON alist if raw? is #t."
  (let* ((endpoint (model-client-endpoint client))
         (api-key  (model-client-api-key client))
         (model    (model-client-model client))
         (uri      (resolve-endpoint-uri endpoint))
         (payload  (format-chat-request messages
                                        #:model model
                                        #:temperature temperature
                                        #:max-tokens max-tokens
                                        #:stream #f
                                        #:tools tools
                                        #:tool-choice tool-choice))
         (headers  (build-headers api-key #f))
         (poster   (or http-poster (current-http-post))))
    (call-with-values
        (lambda ()
          (poster uri #:body payload #:headers headers #:streaming? #f))
      (lambda (response body)
        (let ((code (response-code response)))
          (unless (<= 200 code 299)
            (error "model-complete HTTP request failed" code body))
          (let* ((text (cond
                        ((bytevector? body) (utf8->string body))
                        ((string? body) body)
                        (else body)))
                 (parsed (if (string? text)
                             (catch #t
                               (lambda () (json-string->scm text))
                               (lambda _ text))
                             text)))
            (if raw?
                parsed
                (or (extract-chat-content parsed) parsed))))))))

(define* (model-stream client messages chunk-callback
                       #:key
                       (temperature 0.7)
                       (max-tokens #f)
                       (http-poster #f))
  "Initiate a streaming chat request, calling (CHUNK-CALLBACK text-delta)
as SSE chunks arrive. Returns 'done or 'eof."
  (let* ((endpoint (model-client-endpoint client))
         (api-key  (model-client-api-key client))
         (model    (model-client-model client))
         (uri      (resolve-endpoint-uri endpoint))
         (payload  (format-chat-request messages
                                        #:model model
                                        #:temperature temperature
                                        #:max-tokens max-tokens
                                        #:stream #t))
         (headers  (build-headers api-key #t))
         (poster   (or http-poster (current-http-post))))
    (call-with-values
        (lambda ()
          (poster uri #:body payload #:headers headers #:streaming? #t))
      (lambda (response body-port)
        (let ((code (response-code response)))
          (unless (<= 200 code 299)
            (let ((err-text (if (port? body-port)
                                (catch #t
                                  (lambda () (read-delimited "" body-port))
                                  (lambda _ "failed to read error body"))
                                body-port)))
              (when (and (port? body-port) (not (port-closed? body-port)))
                (close-port body-port))
              (error "model-stream HTTP request failed" code err-text)))
          (dynamic-wind
            (lambda () #t)
            (lambda ()
              (read-sse-events
               body-port
               (lambda (chunk)
                 (let ((item (extract-chat-delta-item chunk)))
                   (when (and (pair? item)
                              (string? (cdr item))
                              (not (string-null? (cdr item))))
                     (catch #t
                       (lambda () (chunk-callback (cdr item) (car item)))
                       (lambda _ (chunk-callback (cdr item)))))))))
            (lambda ()
              (when (and (port? body-port) (not (port-closed? body-port)))
                (close-port body-port)))))))))
