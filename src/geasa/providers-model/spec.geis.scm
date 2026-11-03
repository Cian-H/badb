;;; test-model.scm — Test suite for (geasa providers-model plugin)
;;;
;;; Comprehensive SRFI-64 tests covering client records, payload formatting,
;;; SSE line and event parsing, delta extraction, and model-complete / model-stream.

(define-module (test test-model) #:use-module
               (srfi srfi-64)
  #:use-module (badb spec)
               #:use-module
               (json)
               #:use-module
               (web uri)
               #:use-module
               (web response)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 optargs)
               #:use-module
               (rnrs bytevectors)
               #:use-module
               (geasa providers-model plugin))

(spec-begin "providers-model")

;;;
;;; 1. Client Record Creation and Accessors
;;;
(test-group "client-record"
  (let ((client (make-model-client #:endpoint "https://api.openai.com/v1/chat/completions"
                                   #:api-key "sk-test-12345"
                                   #:model "gpt-4o")))
    (test-assert "model-client? predicate matches" (model-client? client))
    (test-assert "model-client? returns #f on non-client" (not (model-client? "not-a-client")))
    (test-assert "model-client? returns #f on list" (not (model-client? '(endpoint "foo"))))
    (test-equal "model-client-endpoint returns configured endpoint"
      "https://api.openai.com/v1/chat/completions" (model-client-endpoint client))
    (test-equal "model-client-api-key returns configured key" "sk-test-12345" (model-client-api-key client))
    (test-equal "model-client-model returns configured model" "gpt-4o" (model-client-model client)))

  ;; Defaults
  (let ((default-client (make-model-client)))
    (test-assert "make-model-client with defaults returns client" (model-client? default-client))
    (test-equal "default endpoint is localhost ollama"
      "http://localhost:11434/v1/chat/completions" (model-client-endpoint default-client))
    (test-equal "default api-key is #f" #f (model-client-api-key default-client))
    (test-equal "default model is 'default'" "default" (model-client-model default-client))
    (test-assert "allow-missing-thinking defaults to #t"
      (model-client-allow-missing-thinking? default-client)))

  ;; Strict OpenAI-compatible APIs (e.g. api.mistral.ai) reject extra body fields.
  (let ((strict-client (make-model-client #:endpoint "https://api.mistral.ai/v1"
                                          #:api-key "mstrl-test"
                                          #:model "mistral-large-latest"
                                          #:allow-missing-thinking? #f)))
    (test-assert "allow-missing-thinking? can be disabled per client"
      (not (model-client-allow-missing-thinking? strict-client)))
    (test-assert "flag does not affect the model field"
      (string=? "mistral-large-latest" (model-client-model strict-client))))

  ;; Positional arguments
  (let ((pos-client (make-model-client "http://custom:8000/v1" "pos-key" "llama3")))
    (test-equal "positional endpoint" "http://custom:8000/v1" (model-client-endpoint pos-client))
    (test-equal "positional api-key" "pos-key" (model-client-api-key pos-client))
    (test-equal "positional model" "llama3" (model-client-model pos-client))))

;;;
;;; 2. Chat Request Formatting
;;;
(test-group "format-chat-request"
  ;; Non-streaming payload with single message alist
  (let* ((json-str (format-chat-request '(("role" . "user") ("content" . "Hello world"))
                                        #:model "gpt-4o"
                                        #:temperature 0.7
                                        #:stream #f))
         (parsed (json-string->scm json-str)))
    (test-assert "returns valid json string" (string? json-str))
    (test-equal "model is set" "gpt-4o" (assoc-ref parsed "model"))
    (test-equal "temperature is set" 0.7 (assoc-ref parsed "temperature"))
    (test-equal "stream is false for non-streaming" #f (assoc-ref parsed "stream"))
    (test-assert "messages array is present" (vector? (assoc-ref parsed "messages")))
    (let* ((msgs (assoc-ref parsed "messages"))
           (m0 (vector-ref msgs 0)))
      (test-equal "message role is user" "user" (assoc-ref m0 "role"))
      (test-equal "message content matches" "Hello world" (assoc-ref m0 "content"))))

  ;; Streaming payload with string prompt
  (let* ((json-str (format-chat-request "Stream test"
                                        #:model "llama3"
                                        #:stream #t))
         (parsed (json-string->scm json-str)))
    (test-equal "stream is true for streaming" #t (assoc-ref parsed "stream"))
    (test-equal "model matches" "llama3" (assoc-ref parsed "model"))
    (test-assert "temperature omitted when #f" (not (assoc-ref parsed "temperature"))))

  ;; allow_missing_thinking is a Qwen3-style chat-template flag: sent by
  ;; default (local reasoning-model servers need it) but omittable for
  ;; strict OpenAI-compatible APIs.
  (test-assert "allow_missing_thinking sent by default"
    (assoc-ref (json-string->scm
                (format-chat-request "x" #:model "qwen3"))
               "allow_missing_thinking"))
  (test-assert "allow_missing_thinking omitted when disabled"
    (not (assoc-ref (json-string->scm
                     (format-chat-request "x" #:model "mistral-large-latest"
                                          #:allow-missing-thinking #f))
                    "allow_missing_thinking")))


  ;; Payload with max-tokens
  (let* ((json-str (format-chat-request "Token test"
                                        #:model "reflection/beam-1.0-501b-a23b"
                                        #:max-tokens 2048))
         (parsed (json-string->scm json-str)))
    (test-equal "max_tokens is set in payload" 2048 (assoc-ref parsed "max_tokens")))

  ;; Multi-message with system and user roles
  (let* ((messages (list '((role . "system") (content . "Be concise."))
                         '((role . "user") (content . "Hi!"))))
         (json-str (format-chat-request messages #:model "claude-3-opus"))
         (parsed (json-string->scm json-str))
         (msgs (assoc-ref parsed "messages")))
    (test-equal "two messages converted" 2 (vector-length msgs))
    (test-equal "first message role is system" "system" (assoc-ref (vector-ref msgs 0) "role"))
    (test-equal "second message role is user" "user" (assoc-ref (vector-ref msgs 1) "role")))

  ;; as-alist option
  (let ((alist (format-chat-request "Bare prompt" #:as-alist #t)))
    (test-assert "as-alist returns pair" (pair? alist))
    (test-assert "as-alist contains messages key" (assoc-ref alist "messages")))

  ;; Tool message validation and UI card filtering
  (let* ((valid-tool `((role . "tool") (tool_call_id . "call_abc123") (name . "read_file") (content . "hello file")))
         (invalid-tool-no-id `((role . "tool") (content . "⚙ Invoking tool: read_file")))
         (invalid-tool-empty-id `((role . "tool") (tool_call_id . "") (content . "empty id")))
         (ui-log-call `((role . "tool-call") (content . "⚙ Invoking tool...")))
         (ui-log-result `((role . "tool-result") (content . "✓ Tool returned...")))
         (ui-notice `((role . "notice") (content . "welcome")))
         (ui-error `((role . "error") (content . "something failed")))
         (raw-msgs (list `((role . "user") (content . "test"))
                         invalid-tool-no-id
                         invalid-tool-empty-id
                         ui-log-call
                         ui-log-result
                         ui-notice
                         ui-error
                         valid-tool))
         (normalized (normalize-messages raw-msgs)))
    (test-assert "valid tool message satisfies valid-api-message?" (valid-api-message? valid-tool))
    (test-assert "tool message missing tool_call_id fails valid-api-message?"
      (not (valid-api-message? invalid-tool-no-id)))
    (test-assert "tool message with empty tool_call_id fails valid-api-message?"
      (not (valid-api-message? invalid-tool-empty-id)))
    (test-assert "tool-call UI role fails valid-api-message?" (not (valid-api-message? ui-log-call)))
    (test-assert "tool-result UI role fails valid-api-message?" (not (valid-api-message? ui-log-result)))
    (test-assert "notice UI role fails valid-api-message?" (not (valid-api-message? ui-notice)))
    (test-assert "error UI role fails valid-api-message?" (not (valid-api-message? ui-error)))
    (test-equal "normalize-messages filters out all invalid and UI messages" 2 (vector-length normalized))
    (test-equal "first preserved message is user" "user" (assoc-ref (vector-ref normalized 0) "role"))
    (test-equal "second preserved message is valid tool" "tool" (assoc-ref (vector-ref normalized 1) "role"))
    (test-equal "preserved tool message has tool_call_id"
      "call_abc123" (assoc-ref (vector-ref normalized 1) "tool_call_id"))))

;;;
;;; 3. SSE Line Parsing (parse-sse-line)
;;;
(test-group "parse-sse-line"
  ;; Normal data lines
  (test-equal "parse normal data line with space"
    "{\"choices\": [1]}" (parse-sse-line "data: {\"choices\": [1]}"))

  (test-equal "parse normal data line without space after colon"
    "{\"foo\": 42}" (parse-sse-line "data:{\"foo\": 42}"))

  (test-equal "parse plain text data line" "hello world" (parse-sse-line "data: hello world"))

  (test-equal "parse data line with CRLF trailing" "test payload" (parse-sse-line "data: test payload\r\n"))

  (test-equal "parse data line preserving secondary leading space"
    "  two leading spaces" (parse-sse-line "data:   two leading spaces"))

  ;; Empty lines and whitespace
  (test-equal "empty string returns #f" #f (parse-sse-line ""))

  (test-equal "whitespace string returns #f" #f (parse-sse-line "   "))

  (test-equal "CRLF only returns #f" #f (parse-sse-line "\r\n"))

  (test-equal "LF only returns #f" #f (parse-sse-line "\n"))

  ;; Comments (lines beginning with ':')
  (test-equal "comment line returns #f" #f (parse-sse-line ": ping"))

  (test-equal "bare colon returns #f" #f (parse-sse-line ":"))

  (test-equal "comment with no space returns #f" #f (parse-sse-line ":keepalive"))

  (test-equal "comment with CRLF returns #f" #f (parse-sse-line ": comment\r"))

  ;; Stream completion [DONE]
  (test-equal "data: [DONE] returns 'done" 'done (parse-sse-line "data: [DONE]"))

  (test-equal "data: [DONE] with CRLF returns 'done" 'done (parse-sse-line "data: [DONE]\r\n"))

  (test-equal "data:[DONE] without space returns 'done" 'done (parse-sse-line "data:[DONE]"))

  (test-equal "bare [DONE] returns 'done" 'done (parse-sse-line "[DONE]"))

  ;; Other SSE event fields ignored
  (test-equal "event: field returns #f" #f (parse-sse-line "event: ping"))

  (test-equal "id: field returns #f" #f (parse-sse-line "id: 1234"))

  (test-equal "retry: field returns #f" #f (parse-sse-line "retry: 1000")))

;;;
;;; 4. SSE Events Reading (read-sse-events)
;;;
(test-group "read-sse-events"
  ;; Normal stream with comments, empty lines, and [DONE]
  (let* ((sse-input (string-append
                     ": ping keepalive\r\n"
                     "\r\n"
                     "data: {\"id\": \"1\", \"choices\": [{\"delta\": {\"content\": \"Hello\"}}]}\r\n"
                     "\r\n"
                     ": another comment\r\n"
                     "data: {\"id\": \"2\", \"choices\": [{\"delta\": {\"content\": \" badb!\"}}]}\r\n"
                     "\r\n"
                     "data: [DONE]\r\n"
                     "data: should-not-be-reached\r\n"))
         (port (open-input-string sse-input))
         (collected '())
         (result (read-sse-events port (lambda (chunk)
                                         (set! collected (cons chunk collected))))))
    (test-equal "read-sse-events returns 'done when encountering [DONE]" 'done result)
    (test-equal "two events were collected before [DONE]" 2 (length collected))
    (let* ((ordered (reverse collected))
           (first-ev (car ordered))
           (second-ev (cadr ordered)))
      ;; First event
      (test-assert "first event is parsed JSON alist" (pair? first-ev))
      (test-equal "first event id is 1" "1" (assoc-ref first-ev "id"))
      ;; Second event
      (test-assert "second event is parsed JSON alist" (pair? second-ev))
      (test-equal "second event id is 2" "2" (assoc-ref second-ev "id"))))

  ;; Stream ending at EOF without [DONE]
  (let* ((sse-input (string-append
                     "data: first-line\n"
                     "\n"
                     "data: second-line\n"))
         (port (open-input-string sse-input))
         (collected '())
         (result (read-sse-events port (lambda (chunk)
                                         (set! collected (cons chunk collected))))))
    (test-equal "read-sse-events returns 'eof on port exhaustion" 'eof result)
    (test-equal "plain text lines delivered as strings" '("first-line" "second-line") (reverse collected)))

  ;; Stream receiving JSON error payload raises exception
  (let* ((err-input "{\"error\":{\"message\":\"Rate limit exceeded\",\"code\":429}}\r\n")
         (port (open-input-string err-input)))
    (test-assert "read-sse-events throws error on JSON error payload"
      (catch #t
        (lambda ()
          (read-sse-events port (lambda _ #f))
          #f)
        (lambda (key . args)
          #t)))))

;;;
;;; 5. Delta Extraction (extract-chat-delta)
;;;
(test-group "extract-chat-delta"
  ;; OpenAI format with content
  (let ((chunk (json-string->scm "{\"choices\": [{\"delta\": {\"content\": \"Hello\"}}]}")))
    (test-equal "OpenAI delta content extraction" "Hello" (extract-chat-delta chunk)))

  ;; OpenAI format from raw JSON string directly
  (let ((chunk-str "{\"choices\": [{\"delta\": {\"content\": \"From string\"}}]}"))
    (test-equal "OpenAI delta from json string" "From string" (extract-chat-delta chunk-str)))

  ;; OpenAI format with reasoning_content (e.g. DeepSeek / o1)
  (let ((chunk (json-string->scm "{\"choices\": [{\"delta\": {\"reasoning_content\": \"Thinking step\"}}]}")))
    (test-equal "OpenAI reasoning_content extraction" "Thinking step" (extract-chat-delta chunk)))

  ;; OpenAI initial chunk with role only (no content)
  (let ((chunk (json-string->scm "{\"choices\": [{\"delta\": {\"role\": \"assistant\"}}]}")))
    (test-equal "OpenAI role-only chunk yields #f" #f (extract-chat-delta chunk)))

  ;; OpenAI final chunk with finish_reason only
  (let ((chunk (json-string->scm "{\"choices\": [{\"delta\": {}, \"finish_reason\": \"stop\"}]}")))
    (test-equal "OpenAI finish chunk yields #f" #f (extract-chat-delta chunk)))

  ;; Anthropic format
  (let ((chunk (json-string->scm "{\"type\": \"content_block_delta\", \"delta\": {\"type\": \"text_delta\", \"text\": \"Claude text\"}}")))
    (test-equal "Anthropic delta text extraction" "Claude text" (extract-chat-delta chunk)))

  ;; Ollama format
  (let ((chunk (json-string->scm "{\"message\": {\"content\": \"Ollama token\"}}")))
    (test-equal "Ollama delta message content extraction" "Ollama token" (extract-chat-delta chunk)))

  ;; Plain string
  (test-equal "plain string delta returned as-is" "raw delta" (extract-chat-delta "raw delta")))

;;;
;;; 5b. Structured Delta Extraction (extract-chat-delta-item)
;;;
(test-group "extract-chat-delta-item"
  (let ((content-chunk (json-string->scm "{\"choices\": [{\"delta\": {\"content\": \"Hello world\"}}]}")))
    (test-equal "extract content type and text"
      '(content . "Hello world") (extract-chat-delta-item content-chunk)))

  (let ((thought-chunk (json-string->scm "{\"choices\": [{\"delta\": {\"reasoning_content\": \"Analyzing code\"}}]}")))
    (test-equal "extract reasoning/thought type and text"
      '(thought . "Analyzing code") (extract-chat-delta-item thought-chunk)))

  (let ((anthropic-thought (json-string->scm "{\"delta\": {\"thinking\": \"Pondering step\"}}")))
    (test-equal "extract Anthropic thinking delta"
      '(thought . "Pondering step") (extract-chat-delta-item anthropic-thought))))

;;;
;;; 6. Content Extraction (extract-chat-content)
;;;
(test-group "extract-chat-content"
  ;; OpenAI non-streaming
  (let ((res (json-string->scm "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": \"Full OpenAI response\"}}]}")))
    (test-equal "OpenAI full response extraction" "Full OpenAI response" (extract-chat-content res)))

  ;; Anthropic non-streaming
  (let ((res (json-string->scm "{\"content\": [{\"type\": \"text\", \"text\": \"Full Claude response\"}]}")))
    (test-equal "Anthropic full response extraction" "Full Claude response" (extract-chat-content res)))

  ;; Ollama non-streaming
  (let ((res (json-string->scm "{\"message\": {\"role\": \"assistant\", \"content\": \"Full Ollama response\"}}")))
    (test-equal "Ollama full response extraction" "Full Ollama response" (extract-chat-content res))))

;;;
;;; 7. Non-Streaming model-complete with Mock HTTP
;;;
(begin
  (define
    (capture-request uri body headers streaming?)
    "Helper to bundle mock HTTP request parameters into an alist."
    (list (cons 'uri uri)
          (cons 'body body)
          (cons 'headers headers)
          (cons 'streaming? streaming?)))
  (define*
    (make-mock-poster code response-body #:optional (response-headers '()))
    (lambda*
      (uri #:key (body #f) (headers '()) (streaming? #f))
      (values (build-response #:code code #:headers response-headers) response-body))))

(test-group "model-complete"
            (let*
                ((client
                   (make-model-client #:endpoint
                                      "http://test-server/v1/chat/completions"
                                      #:api-key
                                      "test-secret-key"
                                      #:model
                                      "test-model"))
                 (captured-req #f)
                 (mock-poster
                   (lambda*
                     (uri #:key (body #f) (headers '()) (streaming? #f))
                     (set! captured-req (capture-request uri body headers streaming?))
                     (values
                       (build-response #:code
                                       200
                                       #:headers
                                       '((content-type application/json (charset . "utf-8"))))
                       "{\"choices\": [{\"message\": {\"content\": \"Hello from mock!\"}}]}"))))
              (let
                  ((result (model-complete client "Hello" #:http-poster mock-poster)))
                (test-equal
                  "model-complete returns extracted string content"
                  "Hello from mock!"
                  result)
                (test-assert
                  "captured request uri matches client endpoint"
                  (string-contains (uri->string (assq-ref captured-req 'uri)) "test-server"))
                (test-assert "captured request streaming? is #f"
                             (not (assq-ref captured-req 'streaming?)))
                (let*
                    ((body-str (assq-ref captured-req 'body))
                     (body-json (json-string->scm body-str)))
                  (test-equal
                    "captured request body has correct model"
                    "test-model"
                    (assoc-ref body-json "model"))
                  (test-equal
                    "captured request body stream is false"
                    #f
                    (assoc-ref body-json "stream")))))

            ;; The client-level allow-missing-thinking flag flows into
            ;; the request body built by model-complete.
            (let*
                ((strict (make-model-client #:endpoint "https://api.mistral.ai/v1"
                                            #:api-key "mstrl-test"
                                            #:model "mistral-large-latest"
                                            #:allow-missing-thinking? #f))
                 (lenient (make-model-client #:endpoint "http://127.0.0.1:1234/v1"
                                             #:model "qwen3.6-35b-a3b"))
                 (seen '())
                 (poster
                   (lambda*
                     (uri #:key (body #f) (headers '()) (streaming? #f))
                     (set! seen (cons (assoc-ref (json-string->scm body)
                                                 "allow_missing_thinking")
                                      seen))
                     (values
                       (build-response #:code
                                       200
                                       #:headers
                                       '((content-type application/json (charset . "utf-8"))))
                       "{\"choices\": [{\"message\": {\"content\": \"ok\"}}]}")))
                 (%strict-call (model-complete strict "hi" #:http-poster poster))
                 (%lenient-call (model-complete lenient "hi" #:http-poster poster)))
              (test-equal "strict client request omits allow_missing_thinking"
                '(#t #f) seen))
            ;; model-complete with raw? #t returns parsed JSON
            (let*
                ((client (make-model-client #:endpoint "http://test-server/v1" #:model "m"))
                 (mock-poster
                   (make-mock-poster 200
                                     "{\"id\": \"chat-123\", \"choices\": [{\"message\": {\"content\": \"ok\"}}]}"
                                     '((content-type application/json)))))
              (let
                  ((result (model-complete client "Hi" #:raw? #t #:http-poster mock-poster)))
                (test-assert "raw? returns alist" (pair? result))
                (test-equal "raw? contains id" "chat-123" (assoc-ref result "id"))))
            ;; HTTP error handling
            (let*
                ((client (make-model-client #:endpoint "http://test-server/v1"))
                 (mock-poster (make-mock-poster 401 "Unauthorized access")))
              (test-assert "HTTP 401 raises exception"
                           (catch #t
                             (lambda ()
                               (model-complete client "test" #:http-poster mock-poster)
                               #f)
                             (lambda _
                               #t))))
            ;; HTTP 402/429 retry logic
            (let*
                ((client (make-model-client #:endpoint "http://test-server/v1"))
                 (call-count 0)
                 (retry-poster
                   (lambda*
                     (uri #:key (body #f) (headers '()) (streaming? #f))
                     (set! call-count (+ call-count 1))
                     (if (= call-count 1)
                       (values (build-response #:code 402 #:headers '())
                               "{\"error\":{\"message\":\"in_flight_budget_exhausted\"}}")
                       (values (build-response #:code 200 #:headers '())
                               "{\"choices\":[{\"message\":{\"content\":\"Retry succeeded!\"}}]}")))))
              (let
                  ((res
                     (model-complete client
                                     "test"
                                     #:http-poster
                                     retry-poster
                                     #:max-retries
                                     2
                                     #:retry-delay-sec
                                     0)))
                (test-equal
                  "model-complete retries on HTTP 402 and succeeds"
                  "Retry succeeded!"
                  res)
                (test-equal "retry took 2 attempts" 2 call-count)))
            ;; Clean error message extraction from structured JSON
            (let*
                ((client (make-model-client #:endpoint "http://test-server/v1"))
                 (err-poster
                   (make-mock-poster 400
                                     "{\"error\":{\"message\":\"Reasoning required on assistant turns\"}}")))
              (test-equal
                "model-complete extracts clean JSON error message"
                "Reasoning required on assistant turns"
                (catch #t
                  (lambda ()
                    (model-complete client "test" #:http-poster err-poster)
                    "no-error")
                  (lambda (key . args)
                    (let ((sub-args (and (pair? args) (caddr args))))
                      (and (list? sub-args) (caddr sub-args)))))))
            (let*
                ((client (make-model-client #:endpoint "http://test-server/v1"))
                 (err-bv (string->utf8 "{\"error\":\"request exceeds available context size\"}"))
                 (err-poster (make-mock-poster 400 err-bv)))
              (test-equal
                "model-complete extracts error from bytevector response"
                "request exceeds available context size"
                (catch #t
                  (lambda ()
                    (model-complete client "test" #:http-poster err-poster)
                    "no-error")
                  (lambda (key . args)
                    (let ((sub-args (and (pair? args) (caddr args))))
                      (and (list? sub-args) (caddr sub-args)))))))
            ;; Defensive reasoning_content injection in normalize-messages
            (let*
                ((raw-asst-msg
                   '((role . "assistant") (content . "")
                     (tool_calls . #(((id . "call_1") (type . "function"))))))
                 (normalized (vector-ref (normalize-messages (list raw-asst-msg)) 0)))
              (test-equal
                "normalize-messages injects fallback reasoning_content"
                "Thinking..."
                (assoc-ref normalized "reasoning_content"))))

;;;
;;; 8. Streaming model-stream with Mock HTTP
;;;
(test-group "model-stream"
            (let*
                ((client
                   (make-model-client #:endpoint
                                      "http://test-server/v1/chat/completions"
                                      #:api-key
                                      "test-stream-key"
                                      #:model
                                      "stream-model"))
                 (stream-data
                   (string-append
                     "data: {\"choices\": [{\"delta\": {\"role\": \"assistant\"}}]}\n\n"
                     "data: {\"choices\": [{\"delta\": {\"content\": \"The \"}}]}\n\n"
                     "data: {\"choices\": [{\"delta\": {\"content\": \"badb \"}}]}\n\n"
                     "data: {\"choices\": [{\"delta\": {\"content\": \"kernel!\"}}]}\n\n"
                     "data: [DONE]\n"))
                 (captured-req #f)
                 (mock-poster
                   (lambda*
                     (uri #:key (body #f) (headers '()) (streaming? #f))
                     (set! captured-req (capture-request uri body headers streaming?))
                     (values
                       (build-response #:code 200 #:headers '((content-type text/event-stream)))
                       (open-input-string stream-data))))
                 (accumulated '())
                 (final-status
                   (model-stream client
                                 "Tell me about badb"
                                 (lambda (delta)
                                   (set! accumulated (cons delta accumulated)))
                                 #:http-poster
                                 mock-poster)))
              (test-equal "model-stream returns 'done" 'done final-status)
              (test-assert
                "streaming? flag was #t in HTTP call"
                (assq-ref captured-req 'streaming?))
              (test-equal "stream chunks delivered in order"
                          '("The " "badb " "kernel!")
                          (reverse accumulated))
              (test-equal
                "concatenated chunks form complete response text"
                "The badb kernel!"
                (string-concatenate (reverse accumulated))))
            ;; model-stream error handling
            (let*
                ((client (make-model-client #:endpoint "http://test-server/v1"))
                 (mock-poster (make-mock-poster 500 (open-input-string "Internal Server Error"))))
              (test-assert
                "HTTP 500 raises error in model-stream"
                (catch #t
                  (lambda ()
                    (model-stream client
                                  "test"
                                  (lambda (_)
                                    #f)
                                  #:http-poster
                                  mock-poster)
                    #f)
                  (lambda _
                    #t)))))

;;;
;;; 9. Model Listing & Auto-Detection
;;;
(begin
  (define (make-mock-getter code response-body)
    "Helper building a mock HTTP GET procedure returning (response body)."
    (lambda* (uri #:key (headers '()))
      (values (build-response #:code code) response-body))))

(test-group "parse-models-response"
  ;; Realistic OpenAI-compatible /models payload
  (let* ((payload "{\"object\":\"list\",\"data\":[{\"id\":\"qwen3-1.7b\",\"object\":\"model\",\"created\":1748102400},{\"id\":\"llama-3.2-3b-instruct\",\"object\":\"model\",\"created\":1748102401},{\"id\":\"gpt-4o-mini\",\"object\":\"model\",\"created\":1748102402}]}")
         (ids (parse-models-response payload)))
    (test-equal "realistic payload yields ordered model ids"
      '("qwen3-1.7b" "llama-3.2-3b-instruct" "gpt-4o-mini") ids))

  ;; Already-parsed alist input
  (test-equal "accepts already-parsed alist"
    '("m1" "m2")
    (parse-models-response
      (json-string->scm "{\"data\":[{\"id\":\"m1\"},{\"id\":\"m2\"}]}")))

  ;; Empty list
  (test-equal "empty data list yields '()" '() (parse-models-response "{\"object\":\"list\",\"data\":[]}"))

  ;; Malformed JSON error path
  (test-equal "malformed JSON yields '()" '() (parse-models-response "{\"data\":[{\"id\":\"broke"))

  ;; Missing or non-list data
  (test-equal "missing data key yields '()" '() (parse-models-response "{\"object\":\"list\"}"))
  (test-equal "non-alist payload yields '()" '() (parse-models-response 42)))

(test-group "choose-model"
  (let ((ids '("qwen3-1.7b" "llama-3.2-3b-instruct" "gpt-4o-mini")))
    (test-equal "current model kept when present"
      "gpt-4o-mini" (choose-model "gpt-4o-mini" ids))
    (test-equal "generic 'default' picks first id"
      "qwen3-1.7b" (choose-model "default" ids))
    (test-equal "#f current model picks first id"
      "qwen3-1.7b" (choose-model #f ids))
    (test-equal "empty current model picks first id"
      "qwen3-1.7b" (choose-model "" ids))
    (test-equal "absent current model picks first id"
      "qwen3-1.7b" (choose-model "claude-4-opus" ids)))
  (test-equal "empty id list with generic model yields #f"
    #f (choose-model "default" '()))
  (test-equal "empty id list with specific model yields #f"
    #f (choose-model "gpt-4o" '())))

(test-group "list-models"
  ;; Success path, URI derivation, and auth header
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1/chat/completions"
                                    #:api-key "sk-list-key"))
         (captured #f)
         (getter (lambda* (uri #:key (headers '()))
                    (set! captured (list uri headers))
                    (values (build-response #:code 200)
                            "{\"object\":\"list\",\"data\":[{\"id\":\"qwen3-1.7b\"},{\"id\":\"gpt-4o-mini\"}]}"))))
    (call-with-values
        (lambda () (list-models client #:http-getter getter))
      (lambda (ids err)
        (test-equal "list-models returns model ids" '("qwen3-1.7b" "gpt-4o-mini") ids)
        (test-equal "list-models returns no error on success" #f err)
        (test-equal "models uri derived from chat-completions endpoint"
          "http://127.0.0.1:1234/v1/models" (uri->string (car captured)))
        (test-assert "authorization header is sent"
          (assq-ref (cadr captured) 'authorization)))))

  ;; Base endpoint (no chat/completions suffix) and empty list
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1"))
         (captured-uri #f)
         (getter (lambda* (uri #:key (headers '()))
                    (set! captured-uri uri)
                    (values (build-response #:code 200) "{\"object\":\"list\",\"data\":[]}"))))
    (call-with-values
        (lambda () (list-models client #:http-getter getter))
      (lambda (ids err)
        (test-equal "base endpoint also derives /v1/models"
          "http://127.0.0.1:1234/v1/models" (uri->string captured-uri))
        (test-equal "empty data list yields empty ids" '() ids)
        (test-equal "empty data list yields no error" #f err))))

  ;; HTTP error path
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1"))
         (getter (make-mock-getter 404 "{\"error\":{\"message\":\"not found\"}}")))
    (call-with-values
        (lambda () (list-models client #:http-getter getter))
      (lambda (ids err)
        (test-equal "HTTP error yields #f ids" #f ids)
        (test-assert "HTTP error yields error string containing status"
          (and (string? err) (string-contains err "404"))))))

  ;; Malformed JSON body
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1"))
         (getter (make-mock-getter 200 "<html>gateway error</html>")))
    (call-with-values
        (lambda () (list-models client #:http-getter getter))
      (lambda (ids err)
        (test-equal "malformed body yields #f ids" #f ids)
        (test-assert "malformed body yields error string" (string? err)))))

  ;; Connection failure
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1"))
         (getter (lambda* (uri #:key (headers '()))
                    (throw 'system-error "connect-failed" '()))))
    (call-with-values
        (lambda () (list-models client #:http-getter getter))
      (lambda (ids err)
        (test-equal "connection failure yields #f ids" #f ids)
        (test-assert "connection failure yields error string" (string? err))))))

(test-group "model-available?"
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1"))
         (getter (make-mock-getter 200
                                    "{\"data\":[{\"id\":\"qwen3-1.7b\"},{\"id\":\"gpt-4o-mini\"}]}")))
    (test-assert "listed model is available"
      (model-available? client "qwen3-1.7b" #:http-getter getter))
    (test-assert "unlisted model is not available"
      (not (model-available? client "claude-4-opus" #:http-getter getter))))
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1"))
         (getter (make-mock-getter 500 "boom")))
    (test-assert "error response means model is not available"
      (not (model-available? client "qwen3-1.7b" #:http-getter getter)))))

(test-group "auto-detect-model"
  (let ((body "{\"object\":\"list\",\"data\":[{\"id\":\"qwen3-1.7b\"},{\"id\":\"gpt-4o-mini\"},{\"id\":\"llama-3.2-3b-instruct\"}]}"))
    ;; Non-generic current model present in the list is kept unchanged
    (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1"
                                      #:api-key "sk-key" #:model "gpt-4o-mini"))
           (getter (make-mock-getter 200 body)))
      (call-with-values
          (lambda () (auto-detect-model client #:http-getter getter))
        (lambda (model err)
          (test-equal "present non-generic current model is kept" "gpt-4o-mini" model)
          (test-equal "kept model yields no error" #f err))))
    ;; Generic 'default' current model replaced with first available
    (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1" #:model "default"))
           (getter (make-mock-getter 200 body)))
      (call-with-values
          (lambda () (auto-detect-model client #:http-getter getter))
        (lambda (model err)
          (test-equal "generic current model replaced with first id" "qwen3-1.7b" model)
          (test-equal "auto-picked model yields no error" #f err))))
    ;; Absent current model replaced with first available
    (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1" #:model "mistral-large"))
           (getter (make-mock-getter 200 body)))
      (call-with-values
          (lambda () (auto-detect-model client #:http-getter getter))
        (lambda (model err)
          (test-equal "absent current model replaced with first id" "qwen3-1.7b" model)
          (test-equal "replaced model yields no error" #f err)))))
  ;; Connection failure yields #f model and error message
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1"))
         (getter (lambda* (uri #:key (headers '()))
                    (throw 'system-error "connect-failed" '()))))
    (call-with-values
        (lambda () (auto-detect-model client #:http-getter getter))
      (lambda (model err)
        (test-equal "connection failure yields #f model" #f model)
        (test-assert "connection failure yields error string" (string? err)))))
  ;; Empty model list yields #f model with error message
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:1234/v1"))
         (getter (make-mock-getter 200 "{\"object\":\"list\",\"data\":[]}")))
    (call-with-values
        (lambda () (auto-detect-model client #:http-getter getter))
      (lambda (model err)
        (test-equal "empty model list yields #f model" #f model)
        (test-assert "empty model list yields error string" (string? err))))))

;;;
;;; Request Timeout (curl transport bounding)
;;;
(test-group "model-request-timeout"
  (test-equal "default timeout is 600 seconds"
    600 (%request-timeout-seconds))
  (test-equal "parameterize overrides the timeout"
    42 (parameterize ((%request-timeout-seconds 42))
          (%request-timeout-seconds)))
  (test-assert "timeout disabled means unbounded web-client transport"
    (parameterize ((%request-timeout-seconds 0))
      (not (> (%request-timeout-seconds) 0))))
  ;; End-to-end: a request against a listening-but-silent socket hangs;
  ;; curl --max-time cuts it and model-complete surfaces an HTTP error
  ;; within the bound instead of freezing forever.
  (test-assert "hung endpoint surfaces as an error within the timeout"
    (let* ((server (socket AF_INET SOCK_STREAM 0))
           (_ (setsockopt server SOL_SOCKET SO_REUSEADDR 1))
           (_ (bind server AF_INET INADDR_LOOPBACK 0))
           (_ (listen server 1))
           (port (sockaddr:port (getsockname server)))
           (client (make-model-client
                     #:endpoint (format #f "http://127.0.0.1:~a/v1" port)
                     #:model "m"))
           (start (get-internal-run-time))
           (outcome
             (parameterize ((%request-timeout-seconds 2))
               (catch #t
                 (lambda ()
                   (model-complete client
                                   '(((role . "user") (content . "hi")))
                                   #:max-tokens 10
                                   #:raw? #t)
                   'no-error)
                 (lambda args args)))))
      (close-port server)
      (format (current-error-port) "hung outcome: ~s~%" outcome)
      (and (pair? outcome)
           (< (- (get-internal-run-time) start)
              (* 10 internal-time-units-per-second))))))

;;;
;;; 9. Streamed Tool-Call Fragments
;;;
(test-group "streamed-tool-call-fragments"
  (let ((chunk
          (json-string->scm
            "{\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"call_1\", \"type\": \"function\", \"function\": {\"name\": \"read_file\", \"arguments\": \"{}\"}}]}}]}")))
    (test-equal "tool-call fragments extracted from chunk"
      1 (length (extract-tool-call-deltas chunk)))
    (test-equal "chunks without tool_calls yield nothing"
      '() (extract-tool-call-deltas
            (json-string->scm
              "{\"choices\": [{\"delta\": {\"content\": \"hi\"}}]}")))
    (test-equal "string chunks are parsed too"
      1 (length (extract-tool-call-deltas
                  "{\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0}]}}]}"))))

  (let* ((f1 (json-string->scm
               "{\"index\": 0, \"id\": \"call_9\", \"function\": {\"name\": \"write_file\", \"arguments\": \"abc\"}}"))
         (f2 (json-string->scm
               "{\"index\": 0, \"function\": {\"arguments\": \"def\"}}"))
         (f3 (json-string->scm
               "{\"index\": 1, \"id\": \"call_x\", \"function\": {\"name\": \"bash\", \"arguments\": \"{\\\"cmd\\\":\"}}"))
         (f4 (json-string->scm
               "{\"index\": 1, \"function\": {\"arguments\": \"1}\"}}"))
         (acc1 (merge-tool-call-fragment #f f1))
         (acc2 (merge-tool-call-fragment acc1 f2))
         (acc3 (merge-tool-call-fragment acc2 f3))
         (acc4 (merge-tool-call-fragment acc3 f4)))
    (test-assert "first fragment creates the accumulator" (pair? acc1))
    (test-equal "argument pieces concatenate in arrival order"
      "abcdef"
      (caddr (cdr (assv 0 acc4))))
    (test-equal "second index accumulates independently"
      "bash" (cadr (cdr (assv 1 acc4))))
    (test-equal "missing index defaults to 0"
      0 (caar (merge-tool-call-fragment #f (json-string->scm "{\"function\": {\"arguments\": \"x\"}}"))))
    (test-equal "missing id defaults to call_0"
      "call_0" (car (cdr (assv 0 (merge-tool-call-fragment #f (json-string->scm "{\"index\": 0, \"function\": {\"name\": \"t\"}}"))))))
    (let ((final (finalize-tool-call-fragments acc4)))
      (test-equal "two calls finalized in index order" 2 (length final))
      (test-equal "id preserved" "call_9" (assoc-ref (car final) 'id))
      (test-equal "name preserved" "write_file" (assoc-ref (car final) 'name))
      (test-assert "arguments parsed to an alist"
        (equal? (assoc-ref (cadr final) 'arguments) '(("cmd" . 1))))
      (test-equal "empty accumulator finalizes to nothing"
        '() (finalize-tool-call-fragments #f)))))

;;;
;;; 10. model-stream with tools
;;;
(test-group "model-stream-tools"
  (let* ((client (make-model-client #:endpoint "http://t/v1/chat/completions" #:model "m"))
         (sse
           (string-append
             "data: {\"choices\": [{\"delta\": {\"content\": \"Plan\"}}]}\n\n"
             "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"c1\", \"function\": {\"name\": \"ping\", \"arguments\": \"{}\"}}]}}]}\n\n"
             "data: [DONE]\n"))
         (events '())
         (captured-req #f)
         (mock-poster
           (lambda* (uri #:key body headers (streaming? #f))
             (set! captured-req (capture-request uri body headers streaming?))
             (values (build-response #:code 200) (open-input-string sse)))))
    (parameterize ((current-http-post mock-poster))
      (let ((status (model-stream client
                                  "go"
                                  (lambda (payload type)
                                    (set! events (cons (list type payload) events)))
                                  #:tools
                                  '(((type . "function")
                                     (function . ((name . "ping")
                                                  (description . "ping")
                                                  (parameters . ((type . "object") (properties . ())))))))
                                  #:tool-choice
                                  "auto")))
        (test-equal "stream completes" 'done status)
        (test-assert "request carried the tools"
          (string-contains (assq-ref captured-req 'body) "ping"))
        (test-assert "request carried tool_choice"
          (string-contains (assq-ref captured-req 'body) "tool_choice"))
        (test-assert "request asked for streaming"
          (string-contains (assq-ref captured-req 'body) "\"stream\":true"))
        (test-equal "content and fragment delivered" 2 (length events))
        (test-equal "tool-call fragment typed correctly"
          'tool-call (car (car events)))
        (test-assert "fragment carries the function name"
          (string-contains
            (object->string (cadr (car events))) "ping"))))))

(test-group "extract-usage"
  (let ((response (json-string->scm
                    "{\"choices\":[],\"usage\":{\"prompt_tokens\":12,\"completion_tokens\":34,\"total_tokens\":46}}")))
    (test-equal "extracts prompt and completion tokens"
      12 (assoc-ref (extract-usage response) 'prompt))
    (test-equal "extracts completion tokens"
      34 (assoc-ref (extract-usage response) 'completion)))

  (test-equal "missing usage block yields #f"
    #f (extract-usage (json-string->scm "{\"choices\":[]}")))
  (test-equal "non-numeric usage fields yield #f"
    #f (extract-usage (json-string->scm "{\"usage\":{\"prompt_tokens\":\"many\"}}")))
  (test-equal "empty usage object yields #f"
    #f (extract-usage (json-string->scm "{\"usage\":{}}")))
  (test-equal "non-alist response yields #f" #f (extract-usage "not json"))
  (test-equal "raw string response yields #f" #f (extract-usage "plain text")))

(spec-end)
(spec-exit)
