;;; badb — Model Endpoint API (HTTP + SSE, JSON)
;;;
;;; Client implementation for HTTP + SSE model endpoints compatible with
;;; OpenAI, Anthropic, local Ollama, and OpenRouter streaming APIs.

(define-module (badb model)
  #:use-module (badb util)
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
            valid-api-message?
            normalize-messages
            format-chat-request
            parse-sse-line
            read-sse-events
            extract-chat-delta
            extract-chat-delta-item
            extract-chat-content
            extract-chat-tool-calls
            current-http-post
            current-http-get
            %request-timeout-seconds
            call-with-request-timeout
            extract-tool-call-deltas
            merge-tool-call-fragment
            finalize-tool-call-fragments
            parse-models-response
            choose-model
            list-models
            model-available?
            auto-detect-model
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
(begin
  (define (message-alist? x) (and (pair? x) (pair? (car x))))
  (define (role-content-pair? x) (and (pair? x) (not (pair? (car x)))))
  (define (single-message-alist? x)
          (and (message-alist? x) (or (prop-ref x "role") (prop-ref x "content")))))

(define (valid-api-message? msg)
        "Predicate to check if MSG is a valid OpenAI/Anthropic chat protocol message.
Rejects internal UI logs or tool responses missing required tool_call_id."
        (cond
          ((role-content-pair? msg)
            (let ((r (ensure-string (car msg))))
              (and (member r '("system" "user" "assistant")) #t)))
          ((message-alist? msg)
            (let*
                ((role (prop-ref msg 'role))
                 (role-str
                   (if role
                     (ensure-string role)
                     "")))
              (cond
                ((member role-str '("system" "user" "assistant")) #t)
                ((string=? role-str "tool") (non-empty-string? (prop-ref msg 'tool_call_id)))
                (else #f))))
          ((string? msg) #t)
          (else #f)))

(define (normalize-message msg)
        (cond
          ((message-alist? msg)
            (let*
                ((stringified
                   (map
                     (lambda (pair)
                       (cons (ensure-string (car pair)) (cdr pair)))
                     msg))
                 (role (assoc-ref stringified "role"))
                 (tc (assoc-ref stringified "tool_calls"))
                 (rc (assoc-ref stringified "reasoning_content")))
              (if
                  (and role
                       (string=? role "assistant")
                       tc
                       (or (not rc) (and (string? rc) (string-null? (string-trim-both rc)))))
                (cons (cons "reasoning_content" "Thinking...")
                      (filter
                        (lambda (p)
                          (not (string=? (car p) "reasoning_content")))
                        stringified))
                stringified)))
          ((role-content-pair? msg)
            (list (cons "role" (ensure-string (car msg))) (cons "content" (cdr msg))))
          ((string? msg) (list (cons "role" "user") (cons "content" msg)))
          (else msg)))

(define (normalize-messages msgs)
  (cond
   ((vector? msgs)
    (list->vector (map normalize-message (filter valid-api-message? (vector->list msgs)))))
   ((single-message-alist? msgs)
    (if (valid-api-message? msgs)
        (vector (normalize-message msgs))
        (vector)))
   ((list? msgs)
    (list->vector (map normalize-message (filter valid-api-message? msgs))))
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
         (alist (filter pair?
                        (list
                         (cons "messages" msgs)
                         (and model (cons "model" model))
                         (and (number? temperature) (cons "temperature" temperature))
                         (and (number? max-tokens) (cons "max_tokens" max-tokens))
                         (and (and tools (or (pair? tools) (vector? tools)))
                              (cons "tools" (if (list? tools) (list->vector tools) tools)))
                         (and tool-choice (cons "tool_choice" tool-choice))
                         (cons "allow_missing_thinking" #t)
                         (cons "stream" (if stream #t #f))))))
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
Returns 'done when [DONE] is encountered, 'eof upon reaching EOF, or the
parsed alist when the body is a non-SSE complete JSON response (an
endpoint that ignored stream:true)."
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
           ;; Non-SSE complete JSON response: some endpoints ignore
           ;; stream:true and answer with a normal completion body.
           ;; Return the parsed object to the caller and stop.
           ((and (string-prefix? "{" cleaned)
                 (catch #t
                   (lambda () (json-string->scm cleaned))
                   (lambda _ #f)))
            => (lambda (obj)
                 (if (and (pair? obj) (json-ref obj "choices"))
                   obj
                   (loop))))
           (else
            (loop)))))))))

;;;
;;; Content and Delta Extractors
;;;
(define (json-ref obj key)
  "Lookup KEY in OBJ where KEY may be a string or symbol, and OBJ is an alist."
  (prop-ref obj key))

(define (extract-openai-delta-item obj)
  "Extract delta item from OpenAI-format choice payload."
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
                 (else #f)))))))

(define (extract-anthropic-delta-item obj)
  "Extract delta item from Anthropic-format delta payload."
  (let ((delta (json-ref obj "delta")))
    (and (pair? delta)
         (cond
          ((or (json-ref delta "thinking") (json-ref delta "thought"))
           => (lambda (th) (cons 'thought th)))
          ((or (json-ref delta "text") (json-ref delta "content"))
           => (lambda (c) (cons 'content c)))
          (else #f)))))

(define (extract-ollama-delta-item obj)
  "Extract delta item from Ollama-format message payload."
  (let ((msg (json-ref obj "message")))
    (and (pair? msg)
         (cond
          ((or (json-ref msg "reasoning_content") (json-ref msg "thinking"))
           => (lambda (rc) (cons 'thought rc)))
          ((json-ref msg "content")
           => (lambda (c) (cons 'content c)))
          (else #f)))))

(define (extract-chat-delta-item chunk)
  "Extract structured delta info from an SSE chunk (parsed JSON alist or JSON string).
Returns a pair '(thought . text) or '(content . text) or #f."
  (let ((obj (if (string? chunk)
                 (catch #t (lambda () (json-string->scm chunk)) (lambda _ chunk))
                 chunk)))
    (if (pair? obj)
        (or (extract-openai-delta-item obj)
            (extract-anthropic-delta-item obj)
            (extract-ollama-delta-item obj)
            (cond
             ((or (json-ref obj "reasoning_content") (json-ref obj "thinking"))
              => (lambda (rc) (cons 'thought rc)))
             ((json-ref obj "content")
              => (lambda (c) (cons 'content c)))
             ((json-ref obj "text")
              => (lambda (t) (cons 'content t)))
             (else #f)))
        (and (string? obj) (cons 'content obj)))))

(define (extract-chat-delta chunk)
  "Extract the textual delta string from an SSE chunk (parsed JSON alist or JSON string).
Supports OpenAI, Anthropic, Ollama, and OpenRouter formats.
Returns the string delta, or #f if no content is found."
  (let ((item (extract-chat-delta-item chunk)))
    (and (pair? item) (cdr item))))

(define (clean-json-string val)
  "Convert JSON string value to string or #f (treating null/nil as #f)."
  (cond
   ((or (not val)
        (null? val)
        (eq? val 'null)
        (and (symbol? val) (member (symbol->string val) '("null" ":null")))
        (and (keyword? val) (string=? (symbol->string (keyword->symbol val)) "null")))
    #f)
   ((string? val)
    (if (or (string=? val "null") (string=? val ":null"))
        #f
        val))
   (else (format #f "~a" val))))

(define (extract-openai-content obj)
        "Extract content from OpenAI-format choices array."
        (let ((choices (json-ref obj "choices")))
          (and (vector? choices)
               (> (vector-length choices) 0)
               (let*
                   ((c0 (vector-ref choices 0)) (msg (json-ref c0 "message")))
                 (or
                   (and (pair? msg)
                        (let ((c (json-ref msg "content")))
                          (if
                              (or (not c) (eq? c 'null) (null? c) (and (string? c) (string-null? c)))
                            (or (json-ref msg "reasoning_content") (json-ref msg "reasoning") c)
                            c)))
                   (json-ref c0 "text"))))))

(define (extract-anthropic-content obj)
  "Extract content from Anthropic-format response payload."
  (let ((content (json-ref obj "content")))
    (cond
     ((string? content) content)
     ((and (vector? content) (> (vector-length content) 0))
      (let ((b0 (vector-ref content 0)))
        (and (pair? b0) (json-ref b0 "text"))))
     (else #f))))

(define (extract-ollama-content obj)
  "Extract content from Ollama-format message payload."
  (let ((msg (json-ref obj "message")))
    (and (pair? msg) (json-ref msg "content"))))

(define (extract-chat-content response-obj)
        "Extract the assistant message text from a non-streaming chat response.
Supports OpenAI, Anthropic, Ollama, and OpenRouter formats.
Returns the string content, or #f if not found."
        (let
            ((obj (safe-json-parse response-obj response-obj)))
          (if (pair? obj)
            (clean-json-string
              (or (extract-openai-content obj)
                  (extract-anthropic-content obj)
                  (extract-ollama-content obj)
                  (json-ref obj "response")
                  (json-ref obj "content")
                  (json-ref obj "text")))
            (clean-json-string obj))))

(define (parse-single-tool-call tc)
        "Parse a single raw tool call object from an API response."
        (let*
            ((fn (json-ref tc "function"))
             (id (or (json-ref tc "id") "call_0"))
             (name
               (if (pair? fn)
                 (or (json-ref fn "name") "")
                 ""))
             (raw-args
               (if (pair? fn)
                 (or (json-ref fn "arguments") "{}")
                 "{}"))
             (parsed-args (safe-json-parse raw-args raw-args)))
          `((id . ,id) (name . ,name) (arguments . ,parsed-args))))

;;;
;;; Streamed Tool-Call Fragments
;;;
(define (fragment-index fragment)
  "Extract the 0-based call index from a streamed tool-call FRAGMENT,
defaulting to 0 when the endpoint omits it."
  (let ((idx (or (json-ref fragment "index") (assoc-ref fragment 'index))))
    (cond
      ((number? idx) (inexact->exact idx))
      ((string? idx) (or (string->number idx) 0))
      (else 0))))

(define (fragment-function-field fragment json-key)
  "Fetch JSON-KEY from FRAGMENT's function object, or #f."
  (let ((fn (json-ref fragment "function")))
    (and (pair? fn) (json-ref fn json-key))))

(define (extract-tool-call-deltas chunk)
  "Extract the list of raw streamed tool-call FRAGMENTS from an SSE CHUNK
(parsed JSON alist or JSON string). OpenAI-compatible endpoints stream
tool calls inside choices[0].delta.tool_calls: each fragment carries the
call index, the id and function name on its first occurrence, and
argument string pieces that must be concatenated in arrival order.
Returns the raw fragment alists, or '() when the chunk has none."
  (let* ((obj (if (string? chunk) (safe-json-parse chunk #f) chunk))
         (choices (and (pair? obj) (json-ref obj "choices")))
         (c0 (and (vector? choices) (> (vector-length choices) 0)
                  (vector-ref choices 0)))
         (delta (and (pair? c0) (json-ref c0 "delta")))
         (raw (and (pair? delta) (json-ref delta "tool_calls"))))
    (cond
      ((vector? raw) (vector->list raw))
      ((list? raw) raw)
      (else '()))))

(define (merge-tool-call-fragment acc fragment)
  "Merge a streamed tool-call FRAGMENT into accumulator ACC (an alist
mapping call index to a (id name args-string) triple, or #f on the first
fragment). The first fragment for an index contributes id and name;
argument string pieces concatenate in arrival order. Returns the new
accumulator."
  (let* ((idx (fragment-index fragment))
         (id (json-ref fragment "id"))
         (name (fragment-function-field fragment "name"))
         (args-frag (fragment-function-field fragment "arguments"))
         (args-str (if (string? args-frag) args-frag ""))
         (prev (and (pair? acc) (assv idx acc)))
         (prev-val (and (pair? prev) (cdr prev)))
         (prev-id (and (pair? prev-val) (car prev-val)))
         (prev-name (and (pair? prev-val) (cadr prev-val)))
         (prev-args (and (pair? prev-val) (caddr prev-val)))
         (entry (list (or (and (string? id) (not (string-null? id)) id) (or prev-id "call_0"))
                      (or (and (string? name) (not (string-null? name)) name) (or prev-name ""))
                      (string-append (or prev-args "") args-str))))
    (if (pair? prev)
        (let rebuild ((rest (or acc '())))
          (cond
            ((null? rest) (list (cons idx entry)))
            ((= (caar rest) idx) (cons (cons idx entry) (cdr rest)))
            (else (cons (car rest) (rebuild (cdr rest))))))
        (cons (cons idx entry) (or acc '())))))

(define (finalize-tool-call-fragments acc)
  "Turn an accumulated fragment ACC into the ordered list of complete
tool-call alists ((id . ...) (name . ...) (arguments . parsed-args)),
matching parse-single-tool-call's shape so streamed and non-streamed
paths are interchangeable. Returns '() when nothing accumulated."
  (if (pair? acc)
      (let ((max-idx (fold (lambda (e m) (max m (car e))) 0 acc)))
        (let build ((i 0) (out '()))
          (if (> i max-idx)
              (reverse out)
              (let ((entry (assv i acc)))
                (build (+ i 1)
                       (if (pair? entry)
                           (let ((triple (cdr entry)))
                             (cons `((id . ,(car triple))
                                     (name . ,(cadr triple))
                                     (arguments . ,(safe-json-parse (caddr triple) (caddr triple))))
                                   out))
                           out))))))
      '()))

(define (extract-chat-tool-calls response-obj)
        "Extract tool calls from a chat completion response (parsed JSON alist or JSON string).
Returns a list of tool call alists: ((id . \"...\") (name . \"...\") (arguments . ...)), or '() if none found."
        (let
            ((obj (safe-json-parse response-obj response-obj)))
          (if (pair? obj)
            (let*
                ((choices (json-ref obj "choices"))
                 (msg
                   (and (vector? choices)
                        (> (vector-length choices) 0)
                        (json-ref (vector-ref choices 0) "message")))
                 (raw-calls
                   (or (and (pair? msg) (json-ref msg "tool_calls")) (json-ref obj "tool_calls"))))
              (cond
                ((vector? raw-calls) (map parse-single-tool-call (vector->list raw-calls)))
                ((list? raw-calls) raw-calls)
                (else '())))
            '())))

;;;
;;; HTTP Request Execution
;;;
(define* (curl-http-post uri #:key body headers (streaming? #f))
  "Execute an HTTP POST using curl subprocess via stdin pipeline. Used as a transparent fallback
when Guile GnuTLS is not available for HTTPS requests.
Streams payload directly over standard input without temporary files, captures real HTTP status codes."
  (let*
      ((url
         (if (string? uri)
           uri
           (uri->string uri)))
       (header-args
         (append-map
           (lambda (h)
             (let*
                 ((key (car h)) (val (cdr h))
                  (str-val
                    (cond
                      ((string? val) val)
                      ((and (eq? key 'authorization) (pair? val)) (format #f "Bearer ~a" (cadr val)))
                      ((eq? key 'content-type) "application/json; charset=utf-8")
                      ((eq? key 'accept)
                        (if streaming?
                          "text/event-stream"
                          "application/json"))
                      (else (format #f "~a" val)))))
               (list "-H" (format #f "~a: ~a" key str-val))))
           (or headers '())))
       (body-str
         (cond
           ((string? body) body)
           ((bytevector? body) (utf8->string body))
           (else ""))))
    (if streaming?
      (let
          ((cmd
             (append
               (list "curl" "-s" "-N" "-X" "POST" url "--data-binary" "@-")
               header-args)))
        (call-with-values
          (lambda ()
            (pipeline (list cmd)))
          (lambda (in out pids)
            (display body-str out)
            (close-port out)
            (values (build-response #:code 200) in))))
      (let
          ((cmd
             (append
               (list "curl"
                     "-s"
                     "-w"
                     (format #f "~a%{http_code}" #\newline)
                     "-X"
                     "POST"
                     url
                     "--data-binary"
                     "@-")
               header-args)))
        (call-with-values
          (lambda ()
            (pipeline (list cmd)))
          (lambda (in out pids)
            (display body-str out)
            (close-port out)
            (let* ((out-str (read-string in)))
              (close-port in)
              (for-each
                (lambda (p)
                  (catch 'system-error
                    (lambda ()
                      (waitpid p))
                    (lambda _
                      #f)))
                pids)
              (let*
                  ((trimmed (or out-str ""))
                   (last-nl (string-rindex trimmed #\newline))
                   (code-str
                     (if last-nl
                       (substring trimmed (+ last-nl 1))
                       "200"))
                   (body-text
                     (if last-nl
                       (substring trimmed 0 last-nl)
                       trimmed))
                   (status-code (or (string->number (string-trim-both code-str)) 200)))
                (values (build-response #:code status-code) body-text)))))))))

(define* (default-http-post uri #:key body headers (streaming? #f))
  "Dispatch HTTP POST to web client, falling back to curl if gnutls is not
available. Bounded by the request timeout (BADB_REQUEST_TIMEOUT seconds)."
  (call-with-request-timeout
    (lambda ()
      (catch #t
        (lambda ()
          (http-post uri #:body body #:headers headers #:streaming? streaming?))
        (lambda (key . args)
          (if (or (eq? key 'gnutls-not-available)
                  (and (pair? args) (string? (car args)) (string-contains (car args) "gnutls")))
              (curl-http-post uri #:body body #:headers headers #:streaming? streaming?)
              (apply throw key args)))))))

(define %request-timeout-seconds
  (make-parameter
    (or (and=> (getenv "BADB_REQUEST_TIMEOUT") string->number)
        600)))

(define (call-with-request-timeout thunk)
  "Call THUNK bounded by the request timeout: a request that stalls longer
than BADB_REQUEST_TIMEOUT seconds (default 600, <= 0 disables) is
interrupted with a 'request-timeout error, so a hung endpoint surfaces as
a visible error instead of freezing the harness forever. The SIGALRM
handler and timer are always restored afterwards."
  (let ((secs (%request-timeout-seconds)))
    (if (or (not secs) (<= secs 0))
      (thunk)
      (let ((prev #f))
        (dynamic-wind
          (lambda ()
            (set! prev (sigaction SIGALRM))
            (sigaction SIGALRM
                       ;; Guile invokes signal handlers with the signal
                       ;; number as an argument.
                       (lambda _
                         (throw 'request-timeout secs)))
            (alarm secs))
          thunk
          (lambda ()
            (alarm 0)
            (if (pair? prev)
              (sigaction SIGALRM (car prev) (cdr prev))
              (sigaction SIGALRM SIG_DFL))))))))

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

(define*
  (model-complete client
                  messages
                  #:key
                  (temperature 0.7)
                  (max-tokens #f)
                  (tools #f)
                  (tool-choice #f)
                  (raw? #f)
                  (http-poster #f)
                  (max-retries #f)
                  (retry-delay-sec #f))
  "Perform a non-streaming chat completion request.
Returns the response content text string, or parsed JSON alist if raw? is #t."
  (let*
      ((endpoint (model-client-endpoint client))
       (api-key (model-client-api-key client))
       (model (model-client-model client))
       (uri (resolve-endpoint-uri endpoint))
       (payload
         (format-chat-request messages
                              #:model
                              model
                              #:temperature
                              temperature
                              #:max-tokens
                              max-tokens
                              #:stream
                              #f
                              #:tools
                              tools
                              #:tool-choice
                              tool-choice))
       (headers (build-headers api-key #f))
       (poster (or http-poster (current-http-post))))
    (let loop
      ((retries
         (or max-retries
             (if http-poster
               0
               2)))
       (delay-sec (or retry-delay-sec 2)))
      (call-with-values
        (lambda ()
          (poster uri #:body payload #:headers headers #:streaming? #f))
        (lambda (response body)
          (let ((code (response-code response)))
            (cond
              ((and (or (= code 402) (= code 429) (= code 503)) (> retries 0))
                (when (> delay-sec 0) (sleep delay-sec))
                (loop (- retries 1) (* delay-sec 2)))
              ((not (<= 200 code 299))
                (let*
                    ((body-str
                       (cond
                         ((bytevector? body) (utf8->string body))
                         ((string? body) body)
                         (else "")))
                     (parsed-err
                       (catch #t
                         (lambda ()
                           (let ((obj (json-string->scm body-str)))
                             (and (pair? obj) (json-ref obj "error"))))
                         (lambda _
                           #f)))
                     (err-msg
                       (cond
                         ((string? parsed-err) parsed-err)
                         ((and (pair? parsed-err) (json-ref parsed-err "message"))
                           (json-ref parsed-err "message"))
                         ((not (string-null? body-str)) body-str)
                         (else (format #f "HTTP ~a error" code)))))
                  (error "model-complete HTTP request failed" code err-msg)))
              (else
                (let*
                    ((text
                       (cond
                         ((bytevector? body) (utf8->string body))
                         ((string? body) body)
                         (else body)))
                     (parsed
                       (if (string? text)
                         (catch #t
                           (lambda ()
                             (json-string->scm text))
                           (lambda _
                             text))
                         text)))
                  (if raw?
                    parsed
                    (or (extract-chat-content parsed) parsed)))))))))))

(define*
  (model-stream client
                messages
                chunk-callback
                #:key
                (temperature 0.7)
                (max-tokens #f)
                (tools #f)
                (tool-choice #f)
                (http-poster #f))
  "Initiate a streaming chat request, calling (CHUNK-CALLBACK payload type)
as SSE chunks arrive. PAYLOAD is a text delta string with TYPE 'content or
'thought, or a raw streamed tool-call fragment alist with TYPE 'tool-call
(assemble fragments with merge-tool-call-fragment / finalize-tool-call-fragments).
Returns 'done or 'eof."
  (let*
      ((endpoint (model-client-endpoint client))
       (api-key (model-client-api-key client))
       (model (model-client-model client))
       (uri (resolve-endpoint-uri endpoint))
       (payload
         (format-chat-request messages
                              #:model
                              model
                              #:temperature
                              temperature
                              #:max-tokens
                              max-tokens
                              #:tools
                              tools
                              #:tool-choice
                              tool-choice
                              #:stream
                              #t))
       (headers (build-headers api-key #t))
       (poster (or http-poster (current-http-post))))
    (call-with-values
      (lambda ()
        (poster uri #:body payload #:headers headers #:streaming? #t))
      (lambda (response body-port)
        ;; Some callers and mock posters deliver the body as a plain
        ;; string; normalize to a port so SSE reading is uniform.
        (let ((port (if (port? body-port)
                        body-port
                        (open-input-string (if (string? body-port) body-port "")))))
        (dynamic-wind
          (lambda ()
            #t)
          (lambda ()
            (let ((code (response-code response)))
              (unless (<= 200 code 299)
                (let
                    ((err-text
                       (if (port? body-port)
                         (catch #t
                           (lambda ()
                             (read-delimited "" body-port))
                           (lambda _
                             "failed to read error body"))
                         body-port)))
                  (error "model-stream HTTP request failed" code err-text)))
              (let ((result (read-sse-events port
                               (lambda (chunk)
                                 (let ((fragments (extract-tool-call-deltas chunk)))
                                   (if (pair? fragments)
                                       (for-each
                                         (lambda (frag)
                                           (catch #t
                                             (lambda ()
                                               (chunk-callback frag 'tool-call))
                                             (lambda _ #f)))
                                         fragments)
                                       (let ((item (extract-chat-delta-item chunk)))
                                         (when
                                             (and (pair? item) (string? (cdr item)) (not (string-null? (cdr item))))
                                           (catch #t
                                             (lambda ()
                                               (chunk-callback (cdr item) (car item)))
                                             (lambda _
                                               (chunk-callback (cdr item))))))))))))
                (let ((complete-res (and (pair? result)
                                         (json-ref result "choices")
                                         result)))
                  (when complete-res
                    (catch #t
                      (lambda ()
                        (chunk-callback complete-res 'complete-json))
                      (lambda _ #f)))
                  (if complete-res 'done result)))))
          (lambda ()
            (when
                (and (port? port) (not (port-closed? port)))
              (close-port port)))))))))

;;;
;;; Model Listing & Auto-Detection
;;;
(define (model-entry-id entry)
  "Extract the model id string from a /models data entry, or #f."
  (let ((id (and (pair? entry) (json-ref entry "id"))))
    (and (string? id) (not (string-null? id)) id)))

(define (parse-models-response response)
  "Parse an OpenAI-compatible GET /models response into the list of model id
strings. RESPONSE may be a raw JSON body string or an already-parsed JSON
alist. Returns '() when the payload is malformed or contains no models."
  (let ((obj (if (string? response) (safe-json-parse response #f) response)))
    (if (pair? obj)
        (let ((data (json-ref obj "data")))
          (cond
           ((vector? data) (filter-map model-entry-id (vector->list data)))
           ((list? data) (filter-map model-entry-id data))
           (else '())))
        '())))

(define (generic-model? model)
  "Return #t when MODEL is an unset or generic placeholder
(#f, empty, or the literal \"default\")."
  (or (not (non-empty-string? model))
      (string=? (string-trim-both model) "default")))

(define (choose-model current-model ids)
  "Pure selection policy for automatic model detection.
Returns CURRENT-MODEL unchanged when it is a specific model present in IDS,
the first entry of IDS when CURRENT-MODEL is generic (#f, empty, or
\"default\") or absent from IDS, and #f when IDS is empty."
  (cond
   ((null? ids) #f)
   ((and (not (generic-model? current-model))
         (member current-model ids))
    current-model)
   (else (car ids))))

(define* (curl-http-get uri #:key headers)
  "Execute an HTTP GET using a curl subprocess. Used as a transparent fallback
when Guile GnuTLS is not available for HTTPS requests.
Captures real HTTP status codes."
  (let* ((url (if (string? uri) uri (uri->string uri)))
         (header-args
          (append-map
           (lambda (h)
             (let* ((key (car h)) (val (cdr h))
                    (str-val
                     (cond
                      ((string? val) val)
                      ((and (eq? key 'authorization) (pair? val))
                       (format #f "Bearer ~a" (cadr val)))
                      ((eq? key 'content-type) "application/json; charset=utf-8")
                      ((eq? key 'accept) "application/json")
                      (else (format #f "~a" val)))))
               (list "-H" (format #f "~a: ~a" key str-val))))
           (or headers '())))
         (cmd (append (list "curl"
                            "-s"
                            "-w"
                            (format #f "~a%{http_code}" #\newline)
                            url)
                      header-args)))
    (call-with-values
        (lambda () (pipeline (list cmd)))
      (lambda (in out pids)
        (close-port out)
        (let* ((out-str (read-string in)))
          (close-port in)
          (for-each
           (lambda (p)
             (catch 'system-error
               (lambda () (waitpid p))
               (lambda _ #f)))
           pids)
          (let* ((trimmed (or out-str ""))
                 (last-nl (string-rindex trimmed #\newline))
                 (code-str (if last-nl (substring trimmed (+ last-nl 1)) "200"))
                 (body-text (if last-nl (substring trimmed 0 last-nl) trimmed))
                 (status-code (or (string->number (string-trim-both code-str)) 200)))
            (values (build-response #:code status-code) body-text)))))))

(define* (default-http-get uri #:key headers)
  "Dispatch HTTP GET to web client, falling back to curl if gnutls is not available."
  (catch #t
    (lambda ()
      (http-get uri #:headers headers))
    (lambda (key . args)
      (if (or (eq? key 'gnutls-not-available)
              (and (pair? args) (string? (car args)) (string-contains (car args) "gnutls")))
          (curl-http-get uri #:headers headers)
          (apply throw key args)))))

(define current-http-get
  (make-parameter default-http-get))

(define (resolve-models-uri endpoint)
  "Normalize ENDPOINT string or uri to a models listing URI.
Strips any chat-completions style suffix (/chat/completions, /completions,
/messages, or /chat) from the path and appends /models."
  (let* ((uri (if (string? endpoint) (string->uri endpoint) endpoint))
         (path (or (uri-path uri) "/"))
         (base-path
          (cond
           ((string-suffix? "/chat/completions" path)
            (substring path 0 (- (string-length path) 17)))
           ((string-suffix? "/completions" path)
            (substring path 0 (- (string-length path) 12)))
           ((string-suffix? "/messages" path)
            (substring path 0 (- (string-length path) 9)))
           ((string-suffix? "/chat" path)
            (substring path 0 (- (string-length path) 5)))
           (else (string-trim-right path #\/)))))
    (build-uri (uri-scheme uri)
               #:userinfo (uri-userinfo uri)
               #:host (uri-host uri)
               #:port (uri-port uri)
               #:path (string-append base-path "/models")
               #:query (uri-query uri)
               #:fragment (uri-fragment uri))))

(define* (list-models client #:key (http-getter #f))
  "Query the OpenAI-compatible GET /models endpoint for CLIENT.
Returns (values model-ids error-string-or-#f), where model-ids is the list
of available model id strings, or #f on failure."
  (let* ((uri (resolve-models-uri (model-client-endpoint client)))
         (headers (build-headers (model-client-api-key client) #f))
         (getter (or http-getter (current-http-get))))
    (catch #t
      (lambda ()
        (call-with-values
            (lambda () (getter uri #:headers headers))
          (lambda (response body)
            (let* ((code (response-code response))
                   (body-str
                    (cond
                     ((bytevector? body) (utf8->string body))
                     ((string? body) body)
                     (else ""))))
              (if (not (<= 200 code 299))
                  (values #f (format #f "HTTP ~a error: ~a" code body-str))
                  (let ((obj (safe-json-parse body-str #f)))
                    (if (pair? obj)
                        (values (parse-models-response obj) #f)
                        (values #f "failed to parse /models response"))))))))
      (lambda (key . args)
        (values #f (format #f "list-models request failed: ~a"
                           (format-exception key args)))))))

(define* (model-available? client id #:key (http-getter #f))
  "Return #t when model ID is listed by CLIENT's /models endpoint.
Issues a live HTTP request; returns #f on error or when ID is not found."
  (and id
       (call-with-values
           (lambda () (list-models client #:http-getter http-getter))
         (lambda (ids err)
           (and (not err) (member id ids) #t)))))

(define* (auto-detect-model client #:key (http-getter #f))
  "Automatically select a model for CLIENT from its /models endpoint.
Returns (values model-id-or-#f error-string-or-#f): the client's current model
is kept when it is non-generic and present in the list, otherwise the first
available entry is chosen. Returns #f with an error message on
connection/parse failure or when no models are listed."
  (call-with-values
      (lambda () (list-models client #:http-getter http-getter))
    (lambda (ids err)
      (cond
       (err (values #f err))
       ((null? ids) (values #f "no models available"))
       (else (values (choose-model (model-client-model client) ids) #f))))))