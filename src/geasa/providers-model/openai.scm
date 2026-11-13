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
               (geasa providers-model cache)
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
               (ice-9 regex)
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
                                         current-http-retry-sleeper
                                         current-on-http-retry
                                         parse-retry-delay-seconds
                                         classify-http-error
                                         http-post-with-retry
                                         model-complete
                                         model-stream
                                         model-entry-context-limit
                                         parse-models-context-limits
                                         fetch-model-context-limit))

;;;
;;; Chat Request Formatting
;;;
(define (strip-internal-tool-keys tool)
        "Keep only standard OpenAI tool specification keys on TOOL and its
function object.  Strips harness-internal provenance/routing keys (_badb_*,
category, server, etc.) that strict OpenAI-compatible endpoints (Gemini,
Mistral, LM Studio, vLLM) reject.  Handles both string and symbol keys,
and wraps flat tool specs into a (type . \"function\") envelope."
        (define (key-in? key allowed)
                (and (or (symbol? key) (string? key))
                     (member
                       (if (symbol? key)
                         (symbol->string key)
                         key)
                       allowed)))
        (define (filter-keys alist allowed)
                (filter
                  (lambda (pair)
                    (and (pair? pair) (key-in? (car pair) allowed)))
                  (if (list? alist)
                    alist
                    '())))
        (define (function-entry? pair)
                (and (pair? pair)
                     (or (eq? (car pair) 'function) (equal? (car pair) "function"))))
        (let*
            ((allowed-top '("type" "function"))
             (allowed-fn '("name" "description" "parameters" "strict"))
             (fn
               (and (list? tool) (or (assoc-ref tool "function") (assq-ref tool 'function))))
             (tool* (filter-keys tool allowed-top))
             (has-type? (or (assoc-ref tool* "type") (assq-ref tool* 'type))))
          (cond
            ((pair? fn)
              (let*
                  ((fn* (filter-keys fn allowed-fn))
                   (mapped
                     (map
                       (lambda (pair)
                         (if (function-entry? pair)
                           (cons (car pair) fn*)
                           pair))
                       tool*)))
                (if has-type?
                  mapped
                  (cons (cons "type" "function") mapped))))
            ((and (list? tool) (or (assoc-ref tool "name") (assq-ref tool 'name)))
              `((type . "function") (function . ,(filter-keys tool allowed-fn))))
            (else tool*))))

(define (dedup-tool-specs tools)
        "Drop duplicate tool specs with the same function name, keeping the first."
        (let ((seen (make-hash-table)))
          (filter
            (lambda (tool)
              (let*
                  ((fn (or (assoc-ref tool "function") (assq-ref tool 'function)))
                   (raw
                     (cond
                       ((pair? fn) (or (assoc-ref fn "name") (assq-ref fn 'name)))
                       ((pair? tool) (or (assoc-ref tool "name") (assq-ref tool 'name)))
                       (else #f)))
                   (name
                     (cond
                       ((symbol? raw) (symbol->string raw))
                       ((string? raw) raw)
                       (else #f))))
                (cond
                  ((or (not name) (string-null? name)) #t)
                  ((hash-ref seen name) #f)
                  (else (hash-set! seen name #t) #t))))
            tools)))

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
                       (allow-missing-thinking #t)
                       (cache-markers? #f))
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
  #:as-alist    - When true, returns the Scheme alist instead of JSON string.
  #:cache-markers? - Annotate the stable leading system-message prefix with
    an explicit ephemeral cache_control breakpoint (see (geasa
    providers-model cache)); no markers are attached when #f (default)."
  (let*
      ((msgs
         (if cache-markers?
           (apply-cache-breakpoints (normalize-messages messages))
           (normalize-messages messages)))
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
                                 (dedup-tool-specs
                                   (map strip-internal-tool-keys (filter pair? (ensure-list tools)))))))
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
        "Parse a single raw tool call object from an API response, preserving
provider-specific extra_content (e.g. Gemini thought_signature) when present."
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
             (parsed-args (safe-json-parse raw-args raw-args))
             (extra (json-ref tc "extra_content")))
          `((id . ,id) (name . ,name)
            (arguments . ,parsed-args)
            ,@(if (pair? extra)
                `((extra_content . ,extra))
                '()))))

;;;
;;; Streamed Tool-Call Fragments
;;;
(define (fragment-index fragment)
        "Extract the 0-based call index from a streamed tool-call FRAGMENT,
or #f when the endpoint omits it."
        (let
            ((idx (or (json-ref fragment "index") (assoc-ref fragment 'index))))
          (cond
            ((number? idx) (inexact->exact idx))
            ((string? idx) (string->number idx))
            (else #f))))

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
          (ensure-list raw)))

(define (merge-tool-call-fragment acc fragment)
        "Merge a streamed tool-call FRAGMENT into accumulator ACC (an alist
mapping call index to a (id name args-string extra-content) tuple, or #f on
the first fragment). Handles endpoints (e.g. Gemini) that omit \"index\" on
parallel tool calls by keying distinct non-empty IDs to fresh indices, and
preserves extra_content (e.g. thought_signature). Returns the new accumulator."
        (let*
            ((explicit-idx (fragment-index fragment))
             (id (json-ref fragment "id"))
             (has-id? (and (string? id) (not (string-null? id))))
             (latest-idx
               (if (pair? acc)
                 (fold
                   (lambda (e m)
                     (max m (car e)))
                   0
                   acc)
                 0))
             (next-idx
               (if (pair? acc)
                 (+ 1 latest-idx)
                 0))
             (id-hit
               (and has-id?
                    (pair? acc)
                    (find
                      (lambda (e)
                        (equal? (cadr e) id))
                      acc)))
             (idx
               (cond
                 (explicit-idx
                   (let
                       ((existing (and (pair? acc) (assv explicit-idx acc))))
                     (if
                         (and existing
                              has-id?
                              (string? (cadr existing))
                              (not (string-null? (cadr existing)))
                              (not (string=? (cadr existing) "call_0"))
                              (not (string=? id (cadr existing))))
                       (if id-hit
                         (car id-hit)
                         next-idx)
                       explicit-idx)))
                 (has-id?
                   (if id-hit
                     (car id-hit)
                     next-idx))
                 (else latest-idx)))
             (name (fragment-function-field fragment "name"))
             (args-frag (fragment-function-field fragment "arguments"))
             (args-str
               (if (string? args-frag)
                 args-frag
                 ""))
             (frag-extra (json-ref fragment "extra_content"))
             (prev (and (pair? acc) (assv idx acc)))
             (prev-val (and (pair? prev) (cdr prev)))
             (prev-id (and (pair? prev-val) (car prev-val)))
             (prev-name (and (pair? prev-val) (cadr prev-val)))
             (prev-args (and (pair? prev-val) (caddr prev-val)))
             (prev-extra (and (pair? prev-val) (> (length prev-val) 3) (cadddr prev-val)))
             (entry
               (list (or (and has-id? id) (or prev-id "call_0"))
                     (or (and (string? name) (not (string-null? name)) name) (or prev-name ""))
                     (string-append (or prev-args "") args-str)
                     (or (and (pair? frag-extra) frag-extra) prev-extra))))
          (if (pair? prev)
            (let rebuild
              ((rest (or acc '())))
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
          (let
              ((max-idx
                 (fold
                   (lambda (e m)
                     (max m (car e)))
                   0
                   acc)))
            (let build
              ((i 0) (out '()))
              (if (> i max-idx)
                (reverse out)
                (let ((entry (assv i acc)))
                  (build (+ i 1)
                         (if (pair? entry)
                           (let*
                               ((triple (cdr entry)) (extra (and (> (length triple) 3) (cadddr triple))))
                             (cons
                               `((id . ,(car triple)) (name . ,(cadr triple))
                                 (arguments . ,(safe-json-parse (caddr triple) (caddr triple)))
                                 ,@(if (pair? extra)
                                     `((extra_content . ,extra))
                                     '()))
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

(define (strict-openai-endpoint? endpoint)
        "Return #t when ENDPOINT targets a strict cloud OpenAI-compatible API
known to reject Qwen3/LM Studio non-protocol fields (allow_missing_thinking,
reasoning_content)."
        (let*
            ((u
               (cond
                 ((uri? endpoint) endpoint)
                 ((string? endpoint) (string->uri endpoint))
                 (else #f)))
             (host (and u (uri-host u))))
          (and (string? host)
               (or
                 (equal? host "generativelanguage.googleapis.com")
                 (equal? host "api.mistral.ai")
                 (equal? host "api.anthropic.com")
                 (equal? host "api.openai.com")))))

(define current-http-retry-sleeper
        (make-parameter
          (lambda (seconds)
            (let
                ((us (max 0 (inexact->exact (round (* 1000000 seconds))))))
              (when (> us 0) (usleep us))))))

(define current-on-http-retry (make-parameter #f))

(define (extract-error-object body-str)
        "Parse an OpenAI or Gemini error JSON payload (object or single-element array)
and return the inner error alist/string, or #f."
        (catch #t
          (lambda ()
            (let*
                ((obj (json-string->scm body-str))
                 (root
                   (cond
                     ((pair? obj) obj)
                     ((and (vector? obj) (> (vector-length obj) 0) (pair? (vector-ref obj 0)))
                       (vector-ref obj 0))
                     (else #f))))
              (and (pair? root) (json-ref root "error"))))
          (lambda _
            #f)))

(define %retry-in-rx
        (make-regexp "retry in ([0-9]+(\\.[0-9]+)?)s" regexp/icase))

(define %retry-delay-str-rx
        (make-regexp "^([0-9]+(\\.[0-9]+)?)(ms|s)$" regexp/icase))

(define (parse-duration-token str)
        (let
            ((m (and (string? str) (regexp-exec %retry-delay-str-rx (string-trim-both str)))))
          (and m
               (let
                   ((num (string->number (match:substring m 1)))
                    (unit (string-downcase (match:substring m 3))))
                 (and num
                      (if (string=? unit "ms")
                        (/ num 1000.0)
                        num))))))

(define
  (parse-retry-delay-seconds headers body-str)
  "Extract provider-supplied retry delay in seconds from HTTP response HEADERS
(Retry-After) or JSON error BODY-STR (Gemini RetryInfo.retryDelay or 'retry in Ns').
Returns a positive number or #f."
  (let*
      ((ra (and (list? headers) (assoc-ref headers 'retry-after)))
       (ra-sec
         (cond
           ((number? ra) ra)
           ((string? ra) (string->number (string-trim-both ra)))
           (else #f)))
       (err-obj (and (string? body-str) (extract-error-object body-str)))
       (details (and (pair? err-obj) (json-ref err-obj "details")))
       (detail-delay
         (and (vector? details)
              (let loop
                ((i 0))
                (if (>= i (vector-length details))
                  #f
                  (let ((d (vector-ref details i)))
                    (or
                      (and (pair? d)
                           (let ((rd (json-ref d "retryDelay")))
                             (and (string? rd) (parse-duration-token rd))))
                      (loop (+ i 1))))))))
       (msg-match (and (string? body-str) (regexp-exec %retry-in-rx body-str)))
       (msg-delay (and msg-match (string->number (match:substring msg-match 1))))
       (chosen (or ra-sec detail-delay msg-delay)))
    (and (number? chosen) (> chosen 0) chosen)))

(define (string-contains-any? haystack needles)
        (let ((low (string-downcase haystack)))
          (any
            (lambda (n)
              (string-contains low n))
            needles)))

(define
  (classify-http-error code headers body-str)
  "Classify a non-2xx HTTP error response (CODE, HEADERS, BODY-STR).
Returns (values retryable? kind provider-delay err-msg) where kind is one of
'transient-rate-limit, 'billing-exhausted, 'server-unavailable, or 'http-error."
  (let*
      ((err-obj (extract-error-object body-str))
       (err-msg
         (cond
           ((string? err-obj) err-obj)
           ((and (pair? err-obj) (json-ref err-obj "message"))
             (json-ref err-obj "message"))
           ((and (string? body-str) (not (string-null? body-str))) body-str)
           (else (format #f "HTTP ~a error" code))))
       (provider-delay (parse-retry-delay-seconds headers body-str))
       (combined (string-append (or body-str "") " " (or err-msg "")))
       (transient-markers?
         (or provider-delay
             (string-contains-any? combined
                                   '("perminute" "per minute"
                                     "rate_limit"
                                     "rate limit"
                                     "too many requests"
                                     "in_flight_budget_exhausted"
                                     "retryinfo"
                                     "retrydelay"
                                     "please retry in"))))
       (billing-markers?
         (string-contains-any? combined
                               '("insufficient_quota" "billing_hard_limit"
                                 "billing_not_active"
                                 "credit balance"
                                 "out of credits"
                                 "prepaid"
                                 "balance exhausted"
                                 "payment required"
                                 "account_deactivated"))))
    (cond
      ((and (or (= code 402) (= code 429)) billing-markers? (not transient-markers?))
        (values #f 'billing-exhausted provider-delay err-msg))
      ((= code 402)
        (if transient-markers?
          (values #t 'transient-rate-limit provider-delay err-msg)
          (values #f 'billing-exhausted provider-delay err-msg)))
      ((= code 429) (values #t 'transient-rate-limit provider-delay err-msg))
      ((= code 503) (values #t 'server-unavailable provider-delay err-msg))
      (else (values #f 'http-error provider-delay err-msg)))))

(define
  (compute-retry-wait-seconds attempt base-delay max-delay provider-delay)
  "Compute bounded exponential backoff with +/-20% jitter, floored at
PROVIDER-DELAY when supplied and capped at MAX-DELAY."
  (let*
      ((exp-base (min max-delay (* base-delay (expt 2 attempt))))
       (spread (* exp-base 0.2 (- (* 2 (random 1.0)) 1)))
       (jittered (max 0 (+ exp-base spread)))
       (with-provider
         (if
             (and (number? provider-delay) (> provider-delay 0))
           (max jittered provider-delay)
           jittered)))
    (min max-delay (max 0 with-provider))))

(define (read-error-body-string body)
        (cond
          ((port? body)
            (let
                ((text
                   (catch #t
                     (lambda ()
                       (read-delimited "" body))
                     (lambda _
                       ""))))
              (catch #t
                (lambda ()
                  (close-port body))
                (lambda _
                  #f))
              text))
          (else (response-body->string body ""))))

(define*
  (http-post-with-retry poster
                        uri
                        payload
                        headers
                        streaming?
                        #:key
                        (max-retries 3)
                        (retry-delay-sec 2)
                        (max-delay-sec 60)
                        (error-label "HTTP request failed"))
  "Execute POSTER with bounded exponential backoff, jitter, and provider
retry-delay support on transient 429/503/402 responses. Fails fast on
billing/balance exhaustion. Returns (values response body) on 2xx."
  (let loop
    ((attempt 0))
    (call-with-values
      (lambda ()
        (poster uri #:body payload #:headers headers #:streaming? streaming?))
      (lambda (response body)
        (let ((code (response-code response)))
          (if (<= 200 code 299)
            (values response body)
            (let
                ((body-str (read-error-body-string body))
                 (resp-headers (response-headers response)))
              (call-with-values
                (lambda ()
                  (classify-http-error code resp-headers body-str))
                (lambda (retryable? kind provider-delay err-msg)
                  (if (and retryable? (< attempt max-retries))
                    (let
                        ((wait-sec
                           (compute-retry-wait-seconds attempt
                                                       retry-delay-sec
                                                       max-delay-sec
                                                       provider-delay))
                         (on-retry (current-on-http-retry)))
                      (when (procedure? on-retry)
                        (catch #t
                          (lambda ()
                            (on-retry (+ attempt 1) wait-sec code kind err-msg))
                          (lambda _
                            #f)))
                      ((current-http-retry-sleeper) wait-sec)
                      (loop (+ attempt 1)))
                    (error error-label code err-msg)))))))))))

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
       (allow-thinking?
         (and
           (model-client-allow-missing-thinking? client)
           (not (strict-openai-endpoint? endpoint))))
       (payload
         (parameterize
           ((current-send-reasoning? allow-thinking?))
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
                                allow-thinking?
                                #:cache-markers?
                                (model-client-cache-markers? client))))
       (headers (build-headers api-key #f))
       (poster (or http-poster (current-http-post)))
       (retries
         (or max-retries
             (if http-poster
               0
               3)))
       (delay-sec (or retry-delay-sec 2)))
    (call-with-values
      (lambda ()
        (http-post-with-retry poster
                              uri
                              payload
                              headers
                              #f
                              #:max-retries
                              retries
                              #:retry-delay-sec
                              delay-sec
                              #:error-label
                              "model-complete HTTP request failed"))
      (lambda (_response body)
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
            (or (extract-chat-content parsed) parsed)))))))

(define*
  (model-stream client
                messages
                chunk-callback
                #:key
                (temperature 0.7)
                (max-tokens #f)
                (tools #f)
                (tool-choice #f)
                (http-poster #f)
                (max-retries #f)
                (retry-delay-sec #f))
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
       (allow-thinking?
         (and
           (model-client-allow-missing-thinking? client)
           (not (strict-openai-endpoint? endpoint))))
       (payload
         (parameterize
           ((current-send-reasoning? allow-thinking?))
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
                                allow-thinking?
                                #:cache-markers?
                                (model-client-cache-markers? client))))
       (headers (build-headers api-key #t))
       (poster (or http-poster (current-http-post)))
       (retries
         (or max-retries
             (if http-poster
               0
               3)))
       (delay-sec (or retry-delay-sec 2)))
    (call-with-values
      (lambda ()
        (http-post-with-retry poster
                              uri
                              payload
                              headers
                              #t
                              #:max-retries
                              retries
                              #:retry-delay-sec
                              delay-sec
                              #:error-label
                              "model-stream HTTP request failed"))
      (lambda (_response body-port)
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
              (let*
                  ((result
                     (read-sse-events port
                                      (lambda (chunk)
                                        (let
                                            ((stop? #f) (fragments (extract-tool-call-deltas chunk)))
                                          (if (pair? fragments)
                                            (for-each
                                              (lambda (frag)
                                                (unless stop?
                                                  (let
                                                      ((res
                                                         (catch #t
                                                           (lambda ()
                                                             (chunk-callback frag 'tool-call))
                                                           (lambda _
                                                             #f))))
                                                    (when (eq? res 'stop) (set! stop? #t)))))
                                              fragments)
                                            (let ((item (extract-chat-delta-item chunk)))
                                              (when
                                                  (and (pair? item) (string? (cdr item)) (not (string-null? (cdr item))))
                                                (let
                                                    ((res
                                                       (catch #t
                                                         (lambda ()
                                                           (chunk-callback (cdr item) (car item)))
                                                         (lambda _
                                                           (chunk-callback (cdr item))))))
                                                  (when (eq? res 'stop) (set! stop? #t))))))
                                          (if stop?
                                            'stop
                                            #f)))))
                   (complete-res (and (pair? result) (json-ref result "choices") result)))
                (when complete-res
                  (catch #t
                    (lambda ()
                      (chunk-callback complete-res 'complete-json))
                    (lambda _
                      #f)))
                (if complete-res
                  'done
                  result)))
            (lambda ()
              (when (and (port? port) (not (port-closed? port))) (close-port port)))))))))

;;;
;;; Model Listing & Auto-Detection
;;;
(define (model-entry-id entry)
        "Extract the model id/key string from a /models data entry, or #f."
        (let ((id (and (pair? entry)
                       (or (json-ref entry "id")
                           (json-ref entry "key")
                           (json-ref entry "name")
                           (json-ref entry "model")))))
          (and (string? id) (not (string-null? id)) id)))

(define (models-data-of obj)
        (cond
          ((pair? obj)
            (or (json-ref obj "data")
                (json-ref obj "models")))
          ((vector? obj) obj)
          ((list? obj) obj)
          (else #f)))

(define (parse-models-response response)
        "Parse an OpenAI-compatible GET /models response into the list of model id
strings. RESPONSE may be a raw JSON body string or an already-parsed JSON
alist. Returns '() when the payload is malformed or contains no models."
        (let* ((obj (if (string? response) (safe-json-parse response #f) response))
               (data (models-data-of obj)))
          (cond
            ((vector? data) (filter-map model-entry-id (vector->list data)))
            ((list? data) (filter-map model-entry-id data))
            (else '()))))

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

(define (first-context-limit entries)
        (cond
          ((null? entries) #f)
          ((not (list? entries)) #f)
          (else
            (let ((limit (model-entry-context-limit (car entries))))
              (if limit limit (first-context-limit (cdr entries)))))))

(define (model-entry-context-limit entry)
        (cond
          ((not (pair? entry)) #f)
          (else
            (let*
                ((direct-loaded
                   (let
                       ((val (json-ref entry "loaded_context_length")))
                     (and (number? val) (positive? val) (inexact->exact (floor val)))))
                 (config-limit
                   (let ((cfg (json-ref entry "config")))
                     (and (pair? cfg) (model-entry-context-limit cfg))))
                 (loaded-instances-limit
                   (let ((li (json-ref entry "loaded_instances")))
                     (cond
                       ((vector? li) (first-context-limit (vector->list li)))
                       ((list? li) (first-context-limit li))
                       (else #f))))
                 (meta-limit
                   (let ((m (json-ref entry "meta")))
                     (and (pair? m) (model-entry-context-limit m)))))
              (or direct-loaded
                  config-limit
                  loaded-instances-limit
                  meta-limit
                  (let loop
                    ((keys
                       '("max_model_context_length" "max_context_length"
                         "context_length"
                         "context_window"
                         "inputTokenLimit"
                         "input_token_limit"
                         "max_completion_tokens"
                         "n_ctx")))
                    (cond
                      ((null? keys) #f)
                      (else
                        (let ((val (json-ref entry (car keys))))
                          (if (and (number? val) (positive? val))
                            (inexact->exact (floor val))
                            (loop (cdr keys))))))))))))

(define (parse-models-context-limits response)
        (let*
            ((obj
               (if (string? response)
                 (safe-json-parse response #f)
                 response))
             (data (models-data-of obj)))
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
            ((and (pair? obj) (model-entry-id obj)) =>
                                                    (lambda (id)
                                                      (list (cons id (model-entry-context-limit obj)))))
            (else '()))))

(define (candidate-model-uris endpoint)
        (let* ((standard (resolve-models-uri endpoint))
               (u (if (string? endpoint) (string->uri endpoint) endpoint))
               (origin (build-uri (uri-scheme u)
                                  #:userinfo (uri-userinfo u)
                                  #:host (uri-host u)
                                  #:port (uri-port u))))
          (list standard
                (build-uri (uri-scheme origin) #:userinfo (uri-userinfo origin)
                           #:host (uri-host origin) #:port (uri-port origin)
                           #:path "/api/v0/models")
                (build-uri (uri-scheme origin) #:userinfo (uri-userinfo origin)
                           #:host (uri-host origin) #:port (uri-port origin)
                           #:path "/api/v1/models"))))

(define (strip-models-prefix id)
        (if
            (and (string? id) (string-prefix? "models/" id))
          (substring id 7)
          id))

(define
  (lookup-model-context-limit target-model limits)
  (and target-model
       (let
           ((alt
              (if (string-prefix? "models/" target-model)
                (strip-models-prefix target-model)
                (string-append "models/" target-model))))
         (or
           (let ((e (assoc target-model limits))) (and e (cdr e)))
           (let ((e (assoc alt limits))) (and e (cdr e)))))))

(define (gemini-endpoint? uri)
        (let
            ((host (and uri (uri-host uri)))
             (path (string-trim-right (or (and uri (uri-path uri)) "") #\/)))
          (or
            (equal? host "generativelanguage.googleapis.com")
            (string-suffix? "/v1beta/openai" path)
            (string-suffix? "/v1beta/openai/chat/completions" path)
            (string-suffix? "/v1beta/openai/completions" path))))

(define (gemini-model-uri endpoint target-model)
        (let*
            ((u
               (if (string? endpoint)
                 (string->uri endpoint)
                 endpoint))
             (short-model (strip-models-prefix target-model)))
          (and u
               (non-empty-string? short-model)
               (gemini-endpoint? u)
               (let*
                   ((path (or (uri-path u) "/"))
                    (base
                      (cond
                        ((string-suffix? "/chat/completions" path)
                          (substring path 0 (- (string-length path) 17)))
                        ((string-suffix? "/completions" path)
                          (substring path 0 (- (string-length path) 12)))
                        ((string-suffix? "/messages" path)
                          (substring path 0 (- (string-length path) 9)))
                        ((string-suffix? "/chat" path) (substring path 0 (- (string-length path) 5)))
                        (else (string-trim-right path #\/))))
                    (native-base
                      (if (string-suffix? "/openai" base)
                        (substring base 0 (- (string-length base) 7))
                        base))
                    (version-base
                      (if (string-null? native-base)
                        "/v1beta"
                        native-base)))
                 (build-uri (uri-scheme u)
                            #:userinfo
                            (uri-userinfo u)
                            #:host
                            (uri-host u)
                            #:port
                            (uri-port u)
                            #:path
                            (string-append version-base "/models/" short-model)
                            #:query
                            (uri-query u)
                            #:fragment
                            (uri-fragment u))))))

(define (build-gemini-headers api-key)
        (let ((base (build-headers #f #f)))
          (if
              (and api-key (string? api-key) (not (string-null? api-key)))
            (cons (cons 'x-goog-api-key api-key) base)
            base)))

(define*
  (fetch-model-context-limit client #:key (model #f) (http-getter #f))
  "Query model listing endpoints for CLIENT and return the integer
context limit for the entry whose id equals MODEL, or — when MODEL is #f
or generic (generic-model?) — for (model-client-model client). Tries the
standard OpenAI-compatible endpoint first, then Gemini's native model
metadata endpoint when applicable, then LMStudio's native endpoints.
#f when the entry is missing, metadata absent, or every request/parse
fails. MUST catch all errors, return #f instead of throwing."
  (let*
      ((target-model
         (or (and model (not (generic-model? model)) model) (model-client-model client)))
       (api-key (model-client-api-key client))
       (openai-headers (build-headers api-key #f))
       (getter (or http-getter (current-http-get)))
       (endpoint (model-client-endpoint client))
       (uris (candidate-model-uris endpoint))
       (gem-uri (and target-model (gemini-model-uri endpoint target-model)))
       (requests
         (append (list (cons (car uris) openai-headers))
                 (if gem-uri
                   (list (cons gem-uri (build-gemini-headers api-key)))
                   '())
                 (map
                   (lambda (u)
                     (cons u openai-headers))
                   (cdr uris)))))
    (let loop
      ((reqs requests))
      (cond
        ((null? reqs) #f)
        (else
          (let
              ((limit
                 (catch #t
                   (lambda ()
                     (call-with-values
                       (lambda ()
                         (getter (caar reqs) #:headers (cdar reqs)))
                       (lambda (response body)
                         (let*
                             ((code (response-code response)) (body-str (response-body->string body "")))
                           (if (<= 200 code 299)
                             (let
                                 ((limits (parse-models-context-limits body-str)))
                               (lookup-model-context-limit target-model limits))
                             #f)))))
                   (lambda (key . args)
                     #f))))
            (if (and (integer? limit) (> limit 0))
              limit
              (loop (cdr reqs)))))))))

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
