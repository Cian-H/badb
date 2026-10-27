;;; test-model.scm — Test suite for (badb model)
;;;
;;; Comprehensive SRFI-64 tests covering client records, payload formatting,
;;; SSE line and event parsing, delta extraction, and model-complete / model-stream.

(define-module (test test-model)
  #:use-module (srfi srfi-64)
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 optargs)
  #:use-module (badb model))

(test-begin "badb-model")

;;;
;;; 1. Client Record Creation and Accessors
;;;

(test-group "client-record"
  (let ((client (make-model-client #:endpoint "https://api.openai.com/v1/chat/completions"
                                   #:api-key "sk-test-12345"
                                   #:model "gpt-4o")))
    (test-assert "model-client? predicate matches"
      (model-client? client))
    (test-assert "model-client? returns #f on non-client"
      (not (model-client? "not-a-client")))
    (test-assert "model-client? returns #f on list"
      (not (model-client? '(endpoint "foo"))))
    (test-equal "model-client-endpoint returns configured endpoint"
      "https://api.openai.com/v1/chat/completions"
      (model-client-endpoint client))
    (test-equal "model-client-api-key returns configured key"
      "sk-test-12345"
      (model-client-api-key client))
    (test-equal "model-client-model returns configured model"
      "gpt-4o"
      (model-client-model client)))

  ;; Defaults
  (let ((default-client (make-model-client)))
    (test-assert "make-model-client with defaults returns client"
      (model-client? default-client))
    (test-equal "default endpoint is localhost ollama"
      "http://localhost:11434/v1/chat/completions"
      (model-client-endpoint default-client))
    (test-equal "default api-key is #f"
      #f
      (model-client-api-key default-client))
    (test-equal "default model is 'default'"
      "default"
      (model-client-model default-client)))

  ;; Positional arguments
  (let ((pos-client (make-model-client "http://custom:8000/v1" "pos-key" "llama3")))
    (test-equal "positional endpoint"
      "http://custom:8000/v1"
      (model-client-endpoint pos-client))
    (test-equal "positional api-key"
      "pos-key"
      (model-client-api-key pos-client))
    (test-equal "positional model"
      "llama3"
      (model-client-model pos-client))))

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
    (test-assert "returns valid json string"
      (string? json-str))
    (test-equal "model is set"
      "gpt-4o"
      (assoc-ref parsed "model"))
    (test-equal "temperature is set"
      0.7
      (assoc-ref parsed "temperature"))
    (test-equal "stream is false for non-streaming"
      #f
      (assoc-ref parsed "stream"))
    (test-assert "messages array is present"
      (vector? (assoc-ref parsed "messages")))
    (let* ((msgs (assoc-ref parsed "messages"))
           (m0 (vector-ref msgs 0)))
      (test-equal "message role is user"
        "user"
        (assoc-ref m0 "role"))
      (test-equal "message content matches"
        "Hello world"
        (assoc-ref m0 "content"))))

  ;; Streaming payload with string prompt
  (let* ((json-str (format-chat-request "Stream test"
                                        #:model "llama3"
                                        #:stream #t))
         (parsed (json-string->scm json-str)))
    (test-equal "stream is true for streaming"
      #t
      (assoc-ref parsed "stream"))
    (test-equal "model matches"
      "llama3"
      (assoc-ref parsed "model"))
    (test-assert "temperature omitted when #f"
      (not (assoc-ref parsed "temperature"))))

  ;; Payload with max-tokens
  (let* ((json-str (format-chat-request "Token test"
                                        #:model "reflection/beam-1.0-501b-a23b"
                                        #:max-tokens 2048))
         (parsed (json-string->scm json-str)))
    (test-equal "max_tokens is set in payload"
      2048
      (assoc-ref parsed "max_tokens")))

  ;; Multi-message with system and user roles
  (let* ((messages (list '((role . "system") (content . "Be concise."))
                         '((role . "user") (content . "Hi!"))))
         (json-str (format-chat-request messages #:model "claude-3-opus"))
         (parsed (json-string->scm json-str))
         (msgs (assoc-ref parsed "messages")))
    (test-equal "two messages converted"
      2
      (vector-length msgs))
    (test-equal "first message role is system"
      "system"
      (assoc-ref (vector-ref msgs 0) "role"))
    (test-equal "second message role is user"
      "user"
      (assoc-ref (vector-ref msgs 1) "role")))

  ;; as-alist option
  (let ((alist (format-chat-request "Bare prompt" #:as-alist #t)))
    (test-assert "as-alist returns pair"
      (pair? alist))
    (test-assert "as-alist contains messages key"
      (assoc-ref alist "messages"))))

;;;
;;; 3. SSE Line Parsing (parse-sse-line)
;;;

(test-group "parse-sse-line"
  ;; Normal data lines
  (test-equal "parse normal data line with space"
    "{\"choices\": [1]}"
    (parse-sse-line "data: {\"choices\": [1]}"))

  (test-equal "parse normal data line without space after colon"
    "{\"foo\": 42}"
    (parse-sse-line "data:{\"foo\": 42}"))

  (test-equal "parse plain text data line"
    "hello world"
    (parse-sse-line "data: hello world"))

  (test-equal "parse data line with CRLF trailing"
    "test payload"
    (parse-sse-line "data: test payload\r\n"))

  (test-equal "parse data line preserving secondary leading space"
    "  two leading spaces"
    (parse-sse-line "data:   two leading spaces"))

  ;; Empty lines and whitespace
  (test-equal "empty string returns #f"
    #f
    (parse-sse-line ""))

  (test-equal "whitespace string returns #f"
    #f
    (parse-sse-line "   "))

  (test-equal "CRLF only returns #f"
    #f
    (parse-sse-line "\r\n"))

  (test-equal "LF only returns #f"
    #f
    (parse-sse-line "\n"))

  ;; Comments (lines beginning with ':')
  (test-equal "comment line returns #f"
    #f
    (parse-sse-line ": ping"))

  (test-equal "bare colon returns #f"
    #f
    (parse-sse-line ":"))

  (test-equal "comment with no space returns #f"
    #f
    (parse-sse-line ":keepalive"))

  (test-equal "comment with CRLF returns #f"
    #f
    (parse-sse-line ": comment\r"))

  ;; Stream completion [DONE]
  (test-equal "data: [DONE] returns 'done"
    'done
    (parse-sse-line "data: [DONE]"))

  (test-equal "data: [DONE] with CRLF returns 'done"
    'done
    (parse-sse-line "data: [DONE]\r\n"))

  (test-equal "data:[DONE] without space returns 'done"
    'done
    (parse-sse-line "data:[DONE]"))

  (test-equal "bare [DONE] returns 'done"
    'done
    (parse-sse-line "[DONE]"))

  ;; Other SSE event fields ignored
  (test-equal "event: field returns #f"
    #f
    (parse-sse-line "event: ping"))

  (test-equal "id: field returns #f"
    #f
    (parse-sse-line "id: 1234"))

  (test-equal "retry: field returns #f"
    #f
    (parse-sse-line "retry: 1000")))

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
    (test-equal "read-sse-events returns 'done when encountering [DONE]"
      'done
      result)
    (test-equal "two events were collected before [DONE]"
      2
      (length collected))
    (let ((ordered (reverse collected)))
      (let ((first-ev (car ordered))
            (second-ev (cadr ordered)))
        ;; First event
        (test-assert "first event is parsed JSON alist"
          (pair? first-ev))
        (test-equal "first event id is 1"
          "1"
          (assoc-ref first-ev "id"))
        ;; Second event
        (test-assert "second event is parsed JSON alist"
          (pair? second-ev))
        (test-equal "second event id is 2"
          "2"
          (assoc-ref second-ev "id")))))

  ;; Stream ending at EOF without [DONE]
  (let* ((sse-input (string-append
                     "data: first-line\n"
                     "\n"
                     "data: second-line\n"))
         (port (open-input-string sse-input))
         (collected '())
         (result (read-sse-events port (lambda (chunk)
                                         (set! collected (cons chunk collected))))))
    (test-equal "read-sse-events returns 'eof on port exhaustion"
      'eof
      result)
    (test-equal "plain text lines delivered as strings"
      '("first-line" "second-line")
      (reverse collected)))

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
    (test-equal "OpenAI delta content extraction"
      "Hello"
      (extract-chat-delta chunk)))

  ;; OpenAI format from raw JSON string directly
  (let ((chunk-str "{\"choices\": [{\"delta\": {\"content\": \"From string\"}}]}"))
    (test-equal "OpenAI delta from json string"
      "From string"
      (extract-chat-delta chunk-str)))

  ;; OpenAI format with reasoning_content (e.g. DeepSeek / o1)
  (let ((chunk (json-string->scm "{\"choices\": [{\"delta\": {\"reasoning_content\": \"Thinking step\"}}]}")))
    (test-equal "OpenAI reasoning_content extraction"
      "Thinking step"
      (extract-chat-delta chunk)))

  ;; OpenAI initial chunk with role only (no content)
  (let ((chunk (json-string->scm "{\"choices\": [{\"delta\": {\"role\": \"assistant\"}}]}")))
    (test-equal "OpenAI role-only chunk yields #f"
      #f
      (extract-chat-delta chunk)))

  ;; OpenAI final chunk with finish_reason only
  (let ((chunk (json-string->scm "{\"choices\": [{\"delta\": {}, \"finish_reason\": \"stop\"}]}")))
    (test-equal "OpenAI finish chunk yields #f"
      #f
      (extract-chat-delta chunk)))

  ;; Anthropic format
  (let ((chunk (json-string->scm "{\"type\": \"content_block_delta\", \"delta\": {\"type\": \"text_delta\", \"text\": \"Claude text\"}}")))
    (test-equal "Anthropic delta text extraction"
      "Claude text"
      (extract-chat-delta chunk)))

  ;; Ollama format
  (let ((chunk (json-string->scm "{\"message\": {\"content\": \"Ollama token\"}}")))
    (test-equal "Ollama delta message content extraction"
      "Ollama token"
      (extract-chat-delta chunk)))

  ;; Plain string
  (test-equal "plain string delta returned as-is"
    "raw delta"
    (extract-chat-delta "raw delta")))

;;;
;;; 5b. Structured Delta Extraction (extract-chat-delta-item)
;;;

(test-group "extract-chat-delta-item"
  (let ((content-chunk (json-string->scm "{\"choices\": [{\"delta\": {\"content\": \"Hello world\"}}]}")))
    (test-equal "extract content type and text"
      '(content . "Hello world")
      (extract-chat-delta-item content-chunk)))

  (let ((thought-chunk (json-string->scm "{\"choices\": [{\"delta\": {\"reasoning_content\": \"Analyzing code\"}}]}")))
    (test-equal "extract reasoning/thought type and text"
      '(thought . "Analyzing code")
      (extract-chat-delta-item thought-chunk)))

  (let ((anthropic-thought (json-string->scm "{\"delta\": {\"thinking\": \"Pondering step\"}}")))
    (test-equal "extract Anthropic thinking delta"
      '(thought . "Pondering step")
      (extract-chat-delta-item anthropic-thought))))

;;;
;;; 6. Content Extraction (extract-chat-content)
;;;

(test-group "extract-chat-content"
  ;; OpenAI non-streaming
  (let ((res (json-string->scm "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": \"Full OpenAI response\"}}]}")))
    (test-equal "OpenAI full response extraction"
      "Full OpenAI response"
      (extract-chat-content res)))

  ;; Anthropic non-streaming
  (let ((res (json-string->scm "{\"content\": [{\"type\": \"text\", \"text\": \"Full Claude response\"}]}")))
    (test-equal "Anthropic full response extraction"
      "Full Claude response"
      (extract-chat-content res)))

  ;; Ollama non-streaming
  (let ((res (json-string->scm "{\"message\": {\"role\": \"assistant\", \"content\": \"Full Ollama response\"}}")))
    (test-equal "Ollama full response extraction"
      "Full Ollama response"
      (extract-chat-content res))))

;;;
;;; 7. Non-Streaming model-complete with Mock HTTP
;;;

(test-group "model-complete"
  (let* ((client (make-model-client #:endpoint "http://test-server/v1/chat/completions"
                                    #:api-key "test-secret-key"
                                    #:model "test-model"))
         (captured-req #f)
         (mock-poster (lambda* (uri #:key (body #f) (headers '()) (streaming? #f))
                        (set! captured-req (list (cons 'uri uri)
                                                 (cons 'body body)
                                                 (cons 'headers headers)
                                                 (cons 'streaming? streaming?)))
                        (values (build-response #:code 200
                                                #:headers '((content-type application/json (charset . "utf-8"))))
                                "{\"choices\": [{\"message\": {\"content\": \"Hello from mock!\"}}]}"))))
    (let ((result (model-complete client "Hello" #:http-poster mock-poster)))
      (test-equal "model-complete returns extracted string content"
        "Hello from mock!"
        result)
      (test-assert "captured request uri matches client endpoint"
        (string-contains (uri->string (assq-ref captured-req 'uri)) "test-server"))
      (test-assert "captured request streaming? is #f"
        (not (assq-ref captured-req 'streaming?)))
      (let* ((body-str (assq-ref captured-req 'body))
             (body-json (json-string->scm body-str)))
        (test-equal "captured request body has correct model"
          "test-model"
          (assoc-ref body-json "model"))
        (test-equal "captured request body stream is false"
          #f
          (assoc-ref body-json "stream")))))

  ;; model-complete with raw? #t returns parsed JSON
  (let* ((client (make-model-client #:endpoint "http://test-server/v1" #:model "m"))
         (mock-poster (lambda* (uri #:key (body #f) (headers '()) (streaming? #f))
                        (values (build-response #:code 200
                                                #:headers '((content-type application/json)))
                                "{\"id\": \"chat-123\", \"choices\": [{\"message\": {\"content\": \"ok\"}}]}"))))
    (let ((result (model-complete client "Hi" #:raw? #t #:http-poster mock-poster)))
      (test-assert "raw? returns alist"
        (pair? result))
      (test-equal "raw? contains id"
        "chat-123"
        (assoc-ref result "id"))))

  ;; HTTP error handling
  (let* ((client (make-model-client #:endpoint "http://test-server/v1"))
         (mock-poster (lambda* (uri #:key (body #f) (headers '()) (streaming? #f))
                        (values (build-response #:code 401 #:headers '())
                                "Unauthorized access"))))
    (test-assert "HTTP 401 raises exception"
      (catch #t
        (lambda ()
          (model-complete client "test" #:http-poster mock-poster)
          #f)
        (lambda _ #t)))))

;;;
;;; 8. Streaming model-stream with Mock HTTP
;;;

(test-group "model-stream"
  (let* ((client (make-model-client #:endpoint "http://test-server/v1/chat/completions"
                                    #:api-key "test-stream-key"
                                    #:model "stream-model"))
         (stream-data (string-append
                       "data: {\"choices\": [{\"delta\": {\"role\": \"assistant\"}}]}\n\n"
                       "data: {\"choices\": [{\"delta\": {\"content\": \"The \"}}]}\n\n"
                       "data: {\"choices\": [{\"delta\": {\"content\": \"badb \"}}]}\n\n"
                       "data: {\"choices\": [{\"delta\": {\"content\": \"kernel!\"}}]}\n\n"
                       "data: [DONE]\n"))
         (captured-req #f)
         (mock-poster (lambda* (uri #:key (body #f) (headers '()) (streaming? #f))
                        (set! captured-req (list (cons 'uri uri)
                                                 (cons 'body body)
                                                 (cons 'headers headers)
                                                 (cons 'streaming? streaming?)))
                        (values (build-response #:code 200
                                                #:headers '((content-type text/event-stream)))
                                (open-input-string stream-data))))
         (accumulated '())
         (final-status (model-stream client "Tell me about badb"
                                     (lambda (delta)
                                       (set! accumulated (cons delta accumulated)))
                                     #:http-poster mock-poster)))
    (test-equal "model-stream returns 'done"
      'done
      final-status)
    (test-assert "streaming? flag was #t in HTTP call"
      (assq-ref captured-req 'streaming?))
    (test-equal "stream chunks delivered in order"
      '("The " "badb " "kernel!")
      (reverse accumulated))
    (test-equal "concatenated chunks form complete response text"
      "The badb kernel!"
      (string-concatenate (reverse accumulated))))

  ;; model-stream error handling
  (let* ((client (make-model-client #:endpoint "http://test-server/v1"))
         (mock-poster (lambda* (uri #:key (body #f) (headers '()) (streaming? #f))
                        (values (build-response #:code 500 #:headers '())
                                (open-input-string "Internal Server Error")))))
    (test-assert "HTTP 500 raises error in model-stream"
      (catch #t
        (lambda ()
          (model-stream client "test" (lambda (_) #f) #:http-poster mock-poster)
          #f)
        (lambda _ #t)))))

(test-end "badb-model")
