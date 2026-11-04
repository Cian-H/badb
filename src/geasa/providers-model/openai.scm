;;; badb — OpenAI Provider Integration & Model Protocols
;;;
;;; OpenAI-compatible chat completion, tool call parsing, streaming SSE handlers,
;;; and GET /models auto-detection.

(define-module (geasa providers-model openai)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (geasa providers-model common)
               #:use-module
               (geasa providers-model anthropic)
               #:use-module
               (geasa providers-model mock)
               #:use-module
               (web uri)
               #:use-module
               (web response)
               #:use-module
               (web http)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 optargs)
               #:use-module
               (rnrs bytevectors)
               #:use-module
               (json)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:export
               (strip-internal-tool-keys format-chat-request
                                         extract-chat-delta
                                         extract-chat-delta-item
                                         extract-chat-content
                                         extract-chat-tool-calls
                                         extract-usage
                                         extract-tool-call-deltas
                                         merge-tool-call-fragment
                                         finalize-tool-call-fragments
                                         parse-models-response
                                         generic-model?
                                         generic-model-name?
                                         choose-model
                                         list-models
                                         model-available?
                                         auto-detect-model
                                         model-complete
                                         model-stream
                                         model-entry-context-limit
                                         parse-models-context-limits
                                         fetch-model-context-limit))

;;;
;;; Chat Request Formatting
;;;
(define (strip-internal-tool-keys tool)
        "Strip badb-internal _badb_* provenance keys from a TOOL spec (top
level and its function object).  The harness carries them for tool-call
dispatch routing, but strict OpenAI-compatible APIs (e.g.
api.mistral.ai) reject unknown body fields with 422 extra_forbidden.
Tool specs carry the function entry under either a string or symbol
key, so both spellings are handled."
        (define (badb-internal-key? key)
                (and (or (symbol? key) (string? key))
                     (string-prefix? "_badb_"
                                     (if (symbol? key)
                                       (symbol->string key)
                                       key))))
        (define (strip-level alist)
                (filter
                  (lambda (pair)
                    (not (and (pair? pair) (badb-internal-key? (car pair)))))
                  (if (list? alist)
                    alist
                    '())))
        (define (function-entry? pair)
                (and (pair? pair)
                     (or (eq? (car pair) (quote function)) (equal? (car pair) "function"))))
        (let*
            ((tool* (strip-level tool))
             (fn (or (assoc-ref tool* "function") (assq-ref tool* (quote function)))))
          (if (pair? fn)
            (let ((fn* (strip-level fn)))
              (map
                (lambda (pair)
                  (if (function-entry? pair)
                    (cons (car pair) fn*)
                    pair))
                tool*))
            tool*)))

(define*
  (format-chat-request messages
                       #:key
                       (model #f)
                       (temperature #f)
                       (stream #f)
                       (tools #f)
                       (tool-choice #f)
                       (max-tokens #f)
                       (as-alist #f)
                       (allow-missing-thinking #t))
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
  (let*
      ((msgs (normalize-messages messages))
       (alist
         (filter pair?
                 (list (cons "messages" msgs)
                       (and model (cons "model" model))
                       (and (number? temperature) (cons "temperature" temperature))
                       (and (number? max-tokens) (cons "max_tokens" max-tokens))
                       (and
                         (and tools (or (pair? tools) (vector? tools)))
                         (cons "tools"
                               (list->vector
                                 (map strip-internal-tool-keys
                                      (filter pair?
                                              (cond
                                                ((vector? tools) (vector->list tools))
                                                ((list? tools) tools)
                                                (else '())))))))
                       (and tool-choice (cons "tool_choice" tool-choice))
                       (and allow-missing-thinking (cons "allow_missing_thinking" #t))
                       (cons "stream"
                             (if stream
                               #t
                               #f))))))
    (if as-alist
      alist
      (scm->json-string alist))))

;;;
;;; Content and Delta Extractors
;;;
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

(define (extract-usage response)
  "Extract the token usage block from a parsed completion RESPONSE: the
alist ((prompt . N) (completion . N)) built from the OpenAI-style
usage.prompt_tokens / usage.completion_tokens fields, or #f when the
response carries no usable usage data."
  (let ((usage (and (pair? response) (json-ref response "usage"))))
    (if (pair? usage)
        (let ((prompt (json-ref usage "prompt_tokens"))
              (completion (json-ref usage "completion_tokens")))
          (if (and (number? prompt) (number? completion))
              `((prompt . ,prompt) (completion . ,completion))
              #f))
        #f)))

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
         (let* ((c0 (vector-ref choices 0))
                (msg (json-ref c0 "message")))
           (or
             (and (pair? msg)
                  (let ((c (json-ref msg "content")))
                    (if (or (not c) (eq? c 'null) (null? c) (and (string? c) (string-null? c)))
                        (or (json-ref msg "reasoning_content") (json-ref msg "reasoning") c)
                        c)))
             (json-ref c0 "text"))))))

(define (extract-ollama-content obj)
  "Extract content from Ollama-format message payload."
  (let ((msg (json-ref obj "message")))
    (and (pair? msg) (json-ref msg "content"))))

(define (extract-chat-content response-obj)
  "Extract the assistant message text from a non-streaming chat response.
Supports OpenAI, Anthropic, Ollama, and OpenRouter formats.
Returns the string content, or #f if not found."
  (let ((obj (safe-json-parse response-obj response-obj)))
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
  (let* ((fn (json-ref tc "function"))
         (id (or (json-ref tc "id") "call_0"))
         (name (if (pair? fn) (or (json-ref fn "name") "") ""))
         (raw-args (if (pair? fn) (or (json-ref fn "arguments") "{}") "{}"))
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
  (let ((obj (safe-json-parse response-obj response-obj)))
    (if (pair? obj)
        (let* ((choices (json-ref obj "choices"))
               (msg (and (vector? choices)
                         (> (vector-length choices) 0)
                         (json-ref (vector-ref choices 0) "message")))
               (raw-calls (or (and (pair? msg) (json-ref msg "tool_calls"))
                              (json-ref obj "tool_calls"))))
          (cond
            ((vector? raw-calls) (map parse-single-tool-call (vector->list raw-calls)))
            ((list? raw-calls) raw-calls)
            (else '())))
        '())))

;;;
;;; HTTP Request Formatting & Execution
;;;
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

(define (response-body->string body fallback)
  "Coerce an HTTP response BODY (bytevector or string) to a string,
returning FALLBACK when it is neither."
  (cond
   ((bytevector? body) (utf8->string body))
   ((string? body) body)
   (else fallback)))

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
         (parameterize
           ((current-send-reasoning? (model-client-allow-missing-thinking? client)))
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
                                tool-choice
                                #:allow-missing-thinking
                                (model-client-allow-missing-thinking? client))))
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
                    ((body-str (response-body->string body ""))
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
                    ((text (response-body->string body body))
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
         (parameterize
           ((current-send-reasoning? (model-client-allow-missing-thinking? client)))
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
                                #t
                                #:allow-missing-thinking
                                (model-client-allow-missing-thinking? client))))
       (headers (build-headers api-key #t))
       (poster (or http-poster (current-http-post))))
    (call-with-values
      (lambda ()
        (poster uri #:body payload #:headers headers #:streaming? #t))
      (lambda (response body-port)
        ;; Some callers and mock posters deliver the body as a plain
        ;; string; normalize to a port so SSE reading is uniform.
        (let
            ((port
               (if (port? body-port)
                 body-port
                 (open-input-string
                   (if (string? body-port)
                     body-port
                     "")))))
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
                (let*
                    ((result
                       (read-sse-events port
                                        (lambda (chunk)
                                          (let
                                              ((fragments (extract-tool-call-deltas chunk)))
                                            (if (pair? fragments)
                                              (for-each
                                                (lambda (frag)
                                                  (catch #t
                                                    (lambda ()
                                                      (chunk-callback frag 'tool-call))
                                                    (lambda _
                                                      #f)))
                                                fragments)
                                              (let ((item (extract-chat-delta-item chunk)))
                                                (when
                                                    (and (pair? item) (string? (cdr item)) (not (string-null? (cdr item))))
                                                  (catch #t
                                                    (lambda ()
                                                      (chunk-callback (cdr item) (car item)))
                                                    (lambda _
                                                      (chunk-callback (cdr item)))))))))))
                     (complete-res (and (pair? result) (json-ref result "choices") result)))
                  (when complete-res
                    (catch #t
                      (lambda ()
                        (chunk-callback complete-res 'complete-json))
                      (lambda _
                        #f)))
                  (if complete-res
                    'done
                    result))))
            (lambda ()
              (when (and (port? port) (not (port-closed? port))) (close-port port)))))))))

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

(define generic-model-name? generic-model?)

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
                   (body-str (response-body->string body "")))
              (if (<= 200 code 299)
                  (let ((obj (safe-json-parse body-str #f)))
                    (if (pair? obj)
                        (values (parse-models-response obj) #f)
                        (values #f "failed to parse /models response")))
                  (values #f (format #f "HTTP ~a error: ~a" code body-str)))))))
      (lambda (key . args)
        (values #f (format #f "list-models request failed: ~a"
                           (format-exception key args)))))))

(define (model-entry-context-limit entry)
  (let ((keys '("max_context_length" "max_model_context_length" "context_length" "context_window" "max_completion_tokens")))
    (let loop ((keys keys))
      (cond
       ((null? keys) #f)
       ((and (pair? entry)
             (let ((val (json-ref entry (car keys))))
               (and (number? val) (positive? val))))
        (inexact->exact (floor (json-ref entry (car keys)))))
       (else (loop (cdr keys)))))))

(define (parse-models-context-limits response)
  (let ((obj (if (string? response) (safe-json-parse response #f) response)))
    (if (pair? obj)
        (let ((data (json-ref obj "data")))
          (cond
           ((vector? data)
            (filter-map
             (lambda (entry)
               (let ((id (model-entry-id entry)))
                 (and id (cons id (model-entry-context-limit entry)))))
             (vector->list data)))
           ((list? data)
            (filter-map
             (lambda (entry)
               (let ((id (model-entry-id entry)))
                 (and id (cons id (model-entry-context-limit entry)))))
             data))
           (else '())))
        '())))

(define* (fetch-model-context-limit client #:key (model #f) (http-getter #f))
  "Query GET /models exactly like list-models does (resolve-models-uri
on (model-client-endpoint client), same http-getter/default request
pattern and auth header). Return the integer context limit for the
entry whose id equals MODEL, or — when MODEL is #f or generic
(generic-model?) — for (model-client-model client). #f when the entry
is missing, metadata absent, or the request/parse fails. MUST catch
all errors, return #f instead of throwing."
  (let* ((target-model (or (and model (not (generic-model? model)) model)
                           (model-client-model client)))
         (uri (resolve-models-uri (model-client-endpoint client)))
         (headers (build-headers (model-client-api-key client) #f))
         (getter (or http-getter (current-http-get))))
    (catch #t
      (lambda ()
        (call-with-values
            (lambda () (getter uri #:headers headers))
          (lambda (response body)
            (let* ((code (response-code response))
                   (body-str (response-body->string body "")))
              (if (<= 200 code 299)
                  (let ((limits (parse-models-context-limits body-str)))
                    (and target-model
                         (let ((entry (assoc target-model limits)))
                           (and entry (cdr entry)))))
                  #f)))))
      (lambda (key . args)
        #f))))
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
