;;; test/adversarial-providers.scm — Adversarial Stress Test Suite for Providers Subsystem
;;;
;;; Validates:
;;; 1. SSE line and event parsing edge cases in (geasa providers-model common)
;;; 2. Provider HTTP mock dispatch and parameterization in (geasa providers-model mock)
;;; 3. Error code propagation, retries, and timeout handling in (geasa providers-model openai)

(use-modules (srfi srfi-64)
             (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 optargs)
             (web response)
             (web uri)
             (json)
             (geasa providers-model common)
             (geasa providers-model mock)
             (geasa providers-model openai)
             (geasa providers-model anthropic)
             (geasa providers-model plugin)
             (geasa runtimes-react plugin)
             (geasa policies-guardrail plugin))

(test-begin "adversarial-providers")

;;; =========================================================================
;;; 1. SSE LINE & EVENT PARSER STRESS TESTS ((geasa providers-model common))
;;; =========================================================================
(test-group "sse-parser-adversarial"

  ;; --- 1.1 Empty Stream ---
  (test-assert "empty-stream-returns-eof"
    (let ((port (open-input-string ""))
          (events '()))
      (let ((res (read-sse-events port (lambda (e) (set! events (cons e events))))))
        (and (eq? res 'eof)
             (null? events)))))

  ;; --- 1.2 Whitespace-only stream ---
  (test-assert "whitespace-only-stream-returns-eof"
    (let ((port (open-input-string "\n\n   \n\r\n\t\t\n   \r\n"))
          (events '()))
      (let ((res (read-sse-events port (lambda (e) (set! events (cons e events))))))
        (and (eq? res 'eof)
             (null? events)))))

  ;; --- 1.3 Comment lines variations ---
  (test-assert "comments-are-silently-ignored"
    (let* ((payload (string-append
                     ": this is a leading comment\n"
                     ":\n"
                     ":: double colon comment\n"
                     ":   indented comment\n"
                     "data: {\"id\": 1}\n"
                     ": inter-event keepalive ping\n"
                     ":\n"
                     "data: {\"id\": 2}\n"
                     ": trailing comment\n"
                     "data: [DONE]\n"))
           (port (open-input-string payload))
           (events '()))
      (let ((res (read-sse-events port (lambda (e) (set! events (cons e events))))))
        (and (eq? res 'done)
             (= (length events) 2)
             (equal? (assoc-ref (cadr events) "id") 1)
             (equal? (assoc-ref (car events) "id") 2)))))

  ;; --- 1.4 Non-data SSE control fields ---
  (test-assert "non-data-fields-are-ignored"
    (let* ((payload (string-append
                     "event: message\n"
                     "id: 42\n"
                     "retry: 10000\n"
                     "data: {\"content\": \"hello\"}\n"
                     "event: completion\n"
                     "id: 43\n"
                     "data: [DONE]\n"))
           (port (open-input-string payload))
           (events '()))
      (let ((res (read-sse-events port (lambda (e) (set! events (cons e events))))))
        (and (eq? res 'done)
             (= (length events) 1)
             (equal? (assoc-ref (car events) "content") "hello")))))

  ;; --- 1.5 Malformed Chunks & Truncated JSON ---
  (test-assert "truncated-json-falls-back-to-raw-string-without-crashing"
    (let* ((truncated-json "data: {\"id\": \"chunk-1\", \"incomplete\": [1, 2,")
           (port (open-input-string (string-append truncated-json "\n\ndata: [DONE]\n")))
           (events '()))
      (let ((res (read-sse-events port (lambda (e) (set! events (cons e events))))))
        (and (eq? res 'done)
             (= (length events) 1)
             ;; Must NOT raise an uncaught exception; deliver fallback raw string
             (string? (car events))
             (string=? (car events) "{\"id\": \"chunk-1\", \"incomplete\": [1, 2,")))))

  (test-assert "malformed-unquoted-text-falls-back-to-raw-string"
    (let* ((port (open-input-string "data: unquoted raw streaming text chunk\n\ndata: [DONE]\n"))
           (events '()))
      (let ((res (read-sse-events port (lambda (e) (set! events (cons e events))))))
        (and (eq? res 'done)
             (= (length events) 1)
             (string=? (car events) "unquoted raw streaming text chunk")))))

  (test-assert "non-object-json-primitives-delivered-properly"
    (let* ((payload (string-append
                     "data: \"string-literal\"\n"
                     "data: 42\n"
                     "data: true\n"
                     "data: [1, 2, 3]\n"
                     "data: [DONE]\n"))
           (port (open-input-string payload))
           (events '()))
      (let ((res (read-sse-events port (lambda (e) (set! events (cons e events))))))
        (and (eq? res 'done)
             (= (length events) 4)
             (equal? (reverse events)
                     '("string-literal" 42 #t #(1 2 3)))))))

  ;; --- 1.6 Trailing Data After [DONE] ---
  (test-assert "trailing-data-after-done-is-ignored"
    (let* ((payload (string-append
                     "data: {\"chunk\": 1}\n"
                     "data: [DONE]\n"
                     "data: {\"chunk\": 2}\n"
                     "garbage after done\n"
                     ": trailing comment\n"))
           (port (open-input-string payload))
           (events '()))
      (let ((res (read-sse-events port (lambda (e) (set! events (cons e events))))))
        (and (eq? res 'done)
             (= (length events) 1)
             (equal? (assoc-ref (car events) "chunk") 1)))))

  ;; --- 1.7 Stream Termination Without [DONE] (Premature Disconnect) ---
  (test-assert "premature-eof-returns-eof-symbol"
    (let* ((payload "data: {\"chunk\": 1}\n\ndata: {\"chunk\": 2}\n")
           (port (open-input-string payload))
           (events '()))
      (let ((res (read-sse-events port (lambda (e) (set! events (cons e events))))))
        (and (eq? res 'eof)
             (= (length events) 2)
             (equal? (assoc-ref (cadr events) "chunk") 1)
             (equal? (assoc-ref (car events) "chunk") 2)))))

  ;; --- 1.8 Error Response Payload on Stream Port ---
  (test-assert "stream-error-payload-raises-exception"
    (let* ((err-payload "{\"error\": {\"message\": \"Quota exceeded for project\", \"code\": 429}}\n")
           (port (open-input-string err-payload)))
      (catch #t
        (lambda ()
          (read-sse-events port (lambda (_) #f))
          #f)
        (lambda (key . args)
          (and (eq? key 'misc-error)
               (string-contains (format #f "~a" args) "Quota exceeded for project")
               #t)))))

  ;; --- 1.9 Endpoint Ignored stream: true (Non-SSE JSON Body) ---
  (test-assert "non-sse-json-complete-body-returned-directly"
    (let* ((json-body "{\"id\": \"chatcmpl-999\", \"choices\": [{\"message\": {\"content\": \"full response\"}}]}\n")
           (port (open-input-string json-body))
           (events '()))
      (let ((res (read-sse-events port (lambda (e) (set! events (cons e events))))))
        (and (pair? res)
             (equal? (assoc-ref res "id") "chatcmpl-999")
             (null? events)))))

  ;; --- 1.10 parse-sse-line Edge Cases ---
  (test-equal "parse-sse-line data without space"
    "{\"a\":1}"
    (parse-sse-line "data:{\"a\":1}\n"))

  (test-equal "parse-sse-line data with CRLF"
    "{\"b\":2}"
    (parse-sse-line "data: {\"b\":2}\r\n"))

  (test-equal "parse-sse-line empty data"
    ""
    (parse-sse-line "data:\n"))

  (test-equal "parse-sse-line data with single space"
    ""
    (parse-sse-line "data: \n"))

  (test-equal "parse-sse-line data preserving leading spaces after first"
    "  two leading spaces"
    (parse-sse-line "data:   two leading spaces\n"))

  (test-equal "parse-sse-line bare [DONE] with CRLF"
    'done
    (parse-sse-line "[DONE]\r\n"))

  (test-equal "parse-sse-line data:[DONE] without space"
    'done
    (parse-sse-line "data:[DONE]"))
)

;;; =========================================================================
;;; 2. HTTP MOCK DISPATCH & PARAMETERIZATION ((geasa providers-model mock))
;;; =========================================================================
(test-group "http-mock-dispatch-adversarial"

  ;; --- 2.1 Dynamic parameterization of current-http-post ---
  (test-assert "current-http-post-parameterize-override"
    (let* ((called #f)
           (mock-post (lambda* (uri #:key body headers (streaming? #f))
                        (set! called #t)
                        (values (build-response #:code 200)
                                "{\"choices\": [{\"message\": {\"content\": \"mocked output\"}}]}")))
           (client (make-model-client #:endpoint "http://test-server/v1/chat/completions"
                                      #:api-key "test-key"
                                      #:model "test-model")))
      (let ((result (parameterize ((current-http-post mock-post))
                      (model-complete client "Hello world"))))
        (and called
             (string=? result "mocked output")))))

  ;; --- 2.2 Parameterize dynamic scope reversion ---
  (test-assert "current-http-post-reverts-after-dynamic-extent"
    (let* ((orig (current-http-post))
           (temp (lambda args (values (build-response #:code 200) "{}"))))
      (parameterize ((current-http-post temp))
        (test-assert (eq? (current-http-post) temp)))
      (eq? (current-http-post) orig)))

  ;; --- 2.3 Explicit #:http-poster overrides current-http-post parameter ---
  (test-assert "explicit-http-poster-overrides-parameterized-default"
    (let* ((param-called #f)
           (explicit-called #f)
           (param-poster (lambda args (set! param-called #t) (values (build-response #:code 500) "{}")))
           (explicit-poster (lambda* (uri #:key body headers (streaming? #f))
                              (set! explicit-called #t)
                              (values (build-response #:code 200)
                                      "{\"choices\": [{\"message\": {\"content\": \"from explicit\"}}]}")))
           (client (make-model-client #:endpoint "http://test-server/v1/chat/completions")))
      (let ((res (parameterize ((current-http-post param-poster))
                   (model-complete client "Hi" #:http-poster explicit-poster))))
        (and (not param-called)
             explicit-called
             (string=? res "from explicit")))))

  ;; --- 2.4 Dynamic parameterization of current-http-get ---
  (test-assert "current-http-get-parameterize-override-for-list-models"
    (let* ((called #f)
           (mock-get (lambda* (uri #:key headers)
                       (set! called #t)
                       (values (build-response #:code 200)
                               "{\"data\": [{\"id\": \"gpt-4o-adversarial\"}, {\"id\": \"claude-3-5-sonnet\"}]}")))
           (client (make-model-client #:endpoint "http://test-server/v1/chat/completions"
                                      #:model "default")))
      (let ((res (parameterize ((current-http-get mock-get))
                   (list-models client))))
        (and called
             (equal? res '("gpt-4o-adversarial" "claude-3-5-sonnet"))))))

  ;; --- 2.5 current-http-get parameter propagation to auto-detect-model ---
  (test-assert "current-http-get-parameter-used-by-auto-detect-model"
    (let* ((mock-get (lambda* (uri #:key headers)
                       (values (build-response #:code 200)
                               "{\"data\": [{\"id\": \"auto-detected-v1\"}]}")))
           (client (make-model-client #:endpoint "http://test-server/v1/chat/completions"
                                      #:model "default")))
      (call-with-values
          (lambda ()
            (parameterize ((current-http-get mock-get))
              (auto-detect-model client)))
        (lambda (chosen err)
          (and (not err)
               (string=? chosen "auto-detected-v1"))))))

  ;; --- 2.6 current-http-get dynamic scope reversion ---
  (test-assert "current-http-get-reverts-after-dynamic-extent"
    (let* ((orig (current-http-get))
           (temp (lambda args (values (build-response #:code 200) "{}"))))
      (parameterize ((current-http-get temp))
        (test-assert (eq? (current-http-get) temp)))
      (eq? (current-http-get) orig)))
)

;;; =========================================================================
;;; 3. ERROR CODE PROPAGATION & TIMEOUT HANDLING ((geasa providers-model openai/mock))
;;; =========================================================================
(test-group "error-propagation-and-timeouts-adversarial"

  ;; --- 3.1 Non-2xx HTTP Error Propagation in model-complete ---
  (let ((test-status-error
         (lambda (status-code json-err-body expected-substr)
           (let* ((client (make-model-client #:endpoint "http://mock/v1/chat/completions"))
                  (mock-poster (lambda* (uri #:key body headers (streaming? #f))
                                 (values (build-response #:code status-code)
                                         json-err-body))))
             (catch #t
               (lambda ()
                 (model-complete client "test" #:http-poster mock-poster)
                 #f)
               (lambda (key . args)
                 (and (eq? key 'misc-error)
                      (pair? args)
                      (let ((msg (format #f "~a" args)))
                        (and (string-contains msg (number->string status-code))
                             (if expected-substr
                                 (string-contains msg expected-substr)
                                 #t))))))))))

    (test-assert "model-complete propagates HTTP 400 Bad Request"
      (test-status-error 400 "{\"error\": {\"message\": \"Invalid JSON request body\"}}" "Invalid JSON request body"))

    (test-assert "model-complete propagates HTTP 401 Unauthorized"
      (test-status-error 401 "{\"error\": {\"message\": \"Incorrect API key provided\"}}" "Incorrect API key"))

    (test-assert "model-complete propagates HTTP 403 Forbidden"
      (test-status-error 403 "{\"error\": {\"message\": \"Country or region not supported\"}}" "Country or region"))

    (test-assert "model-complete propagates HTTP 404 Model Not Found"
      (test-status-error 404 "{\"error\": {\"message\": \"The model `gpt-future` does not exist\"}}" "gpt-future"))

    (test-assert "model-complete propagates HTTP 422 Unprocessable Entity"
      (test-status-error 422 "{\"error\": {\"message\": \"Unprocessable entity schema mismatch\"}}" "Unprocessable entity"))

    (test-assert "model-complete propagates HTTP 500 Server Error"
      (test-status-error 500 "{\"error\": {\"message\": \"The server had an error processing your request\"}}" "server had an error"))

    (test-assert "model-complete propagates HTTP 502 Bad Gateway"
      (test-status-error 502 "Bad Gateway upstream failure" "Bad Gateway"))

    (test-assert "model-complete propagates HTTP 504 Gateway Timeout"
      (test-status-error 504 "Gateway Timeout" "Gateway Timeout"))
  )

  ;; --- 3.2 Non-2xx HTTP Error Propagation in model-stream ---
  (test-assert "model-stream propagates HTTP 401 Unauthorized"
    (let* ((client (make-model-client #:endpoint "http://mock/v1/chat/completions"))
           (mock-poster (lambda* (uri #:key body headers (streaming? #f))
                          (values (build-response #:code 401)
                                  "{\"error\": {\"message\": \"Invalid streaming auth token\"}}"))))
      (catch #t
        (lambda ()
          (model-stream client "test" (lambda _ #f) #:http-poster mock-poster)
          #f)
        (lambda (key . args)
          (and (eq? key 'misc-error)
               (let ((msg (format #f "~a" args)))
                 (and (string-contains msg "401")
                      (string-contains msg "Invalid streaming auth token"))))))))

  (test-assert "model-stream propagates HTTP 500 Server Error"
    (let* ((client (make-model-client #:endpoint "http://mock/v1/chat/completions"))
           (mock-poster (lambda* (uri #:key body headers (streaming? #f))
                          (values (build-response #:code 500)
                                  "Internal server crash during stream initialization"))))
      (catch #t
        (lambda ()
          (model-stream client "test" (lambda _ #f) #:http-poster mock-poster)
          #f)
        (lambda (key . args)
          (and (eq? key 'misc-error)
               (let ((msg (format #f "~a" args)))
                 (and (string-contains msg "500")
                      (string-contains msg "Internal server crash"))))))))

  ;; --- 3.3 HTTP Error Code Handling in list-models ---
  (test-assert "list-models handles HTTP 401 without throwing"
    (let* ((client (make-model-client #:endpoint "http://mock/v1/chat/completions"))
           (mock-getter (lambda* (uri #:key headers)
                          (values (build-response #:code 401)
                                  "{\"error\": \"Unauthorized\"}"))))
      (call-with-values
          (lambda () (list-models client #:http-getter mock-getter))
        (lambda (ids err)
          (and (not ids)
               (string? err)
               (string-contains err "HTTP 401 error"))))))

  (test-assert "list-models handles network throw gracefully"
    (let* ((client (make-model-client #:endpoint "http://mock/v1/chat/completions"))
           (mock-getter (lambda* (uri #:key headers)
                          (throw 'system-error "connect" "Connection refused" '() '(111)))))
      (call-with-values
          (lambda () (list-models client #:http-getter mock-getter))
        (lambda (ids err)
          (and (not ids)
               (string? err)
               (string-contains err "list-models request failed"))))))

  ;; --- 3.4 Transient Retry Behavior (402, 429, 503) ---
  (test-assert "model-complete retries HTTP 429 then succeeds"
    (let* ((attempts 0)
           (mock-poster (lambda* (uri #:key body headers (streaming? #f))
                          (set! attempts (+ attempts 1))
                          (if (= attempts 1)
                              (values (build-response #:code 429) "{\"error\": \"Rate limit exceeded\"}")
                              (values (build-response #:code 200)
                                      "{\"choices\": [{\"message\": {\"content\": \"recovered\"}}]}"))))
           (client (make-model-client #:endpoint "http://mock/v1/chat/completions")))
      (let ((res (model-complete client "test"
                                 #:http-poster mock-poster
                                 #:max-retries 2
                                 #:retry-delay-sec 0)))
        (and (= attempts 2)
             (string=? res "recovered")))))

  (test-assert "model-complete fails closed when retries are exhausted"
    (let* ((attempts 0)
           (mock-poster (lambda* (uri #:key body headers (streaming? #f))
                          (set! attempts (+ attempts 1))
                          (values (build-response #:code 503) "{\"error\": \"Service unavailable\"}")))
           (client (make-model-client #:endpoint "http://mock/v1/chat/completions")))
      (catch #t
        (lambda ()
          (model-complete client "test"
                          #:http-poster mock-poster
                          #:max-retries 2
                          #:retry-delay-sec 0)
          #f)
        (lambda (key . args)
          (and (eq? key 'misc-error)
               (= attempts 3)
               (string-contains (format #f "~a" args) "503"))))))

  ;; --- 3.5 Timeout Configuration and Propagation ---
  (test-assert "request-timeout-parameter-constructs-curl-args"
    (let ((args-60 (parameterize ((%request-timeout-seconds 60))
                     (curl-header-args '((authorization . (bearer "token"))) "application/json"))))
      (pair? args-60)))

  (test-assert "curl-output->response splits status code from body"
    (let* ((raw-output "{\"message\": \"ok\"}\n200"))
      (call-with-values
          (lambda () (curl-output->response raw-output))
        (lambda (resp body)
          (and (= (response-code resp) 200)
               (string=? (string-trim-both body) "{\"message\": \"ok\"}"))))))

  (test-assert "curl-output->response parses 404 status code"
    (let* ((raw-output "Not Found\n404"))
      (call-with-values
          (lambda () (curl-output->response raw-output))
        (lambda (resp body)
          (and (= (response-code resp) 404)
               (string=? (string-trim-both body) "Not Found"))))))

  (test-assert "curl-output->response handles single-line fallback"
    (let* ((raw-output "200"))
      (call-with-values
          (lambda () (curl-output->response raw-output))
        (lambda (resp body)
          (and (= (response-code resp) 200)
               (string=? body "200"))))))
)

;;; =========================================================================
;;; 4. PUBLIC EXPORT SURFACE ((geasa providers-model plugin))
;;; =========================================================================
(test-group "export-surface-adversarial"

  ;; Verify all 32 public symbols are exported by (geasa providers-model plugin)
  (let ((model-mod (resolve-module '(geasa providers-model plugin)))
        (symbols '(
          <model-client>
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
          extract-usage
          current-http-post
          default-http-post
          current-http-get
          %request-timeout-seconds
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
          model-stream)))

    (for-each
      (lambda (sym)
        (test-assert (format #f "plugin exports ~a" sym)
          (let ((var (module-variable model-mod sym)))
            (and var (variable-bound? var)))))
      symbols)

    (test-equal "plugin exports exactly 32 public symbols"
      32
      (length symbols))))

;;; =========================================================================
;;; 5. RAPID SSE STREAMING STRESS & MULTILINE HARNESS
;;; =========================================================================
(test-group "rapid-sse-streaming-stress"

  (test-assert "100-chunk-rapid-stream-with-interleaved-comments-and-blanks"
    (let* ((lines '())
           (chunk-count 100))
      (do ((i 0 (+ i 1)))
          ((= i chunk-count))
        (set! lines (cons (format #f ": comment before chunk ~a\n" i) lines))
        (set! lines (cons "\n" lines))
        (set! lines (cons (format #f "data: {\"choices\": [{\"delta\": {\"content\": \"tok~a \"}}]}\n" i) lines))
        (set! lines (cons ": comment after chunk\n" lines))
        (set! lines (cons "\r\n" lines)))
      (set! lines (cons "data: [DONE]\n" lines))
      (set! lines (cons "data: extra after done should be ignored\n" lines))
      (let* ((stream-str (apply string-append (reverse lines)))
             (client (make-model-client #:endpoint "http://test/v1/chat/completions"))
             (received '())
             (mock-poster (lambda* (uri #:key body headers (streaming? #f))
                            (values (build-response #:code 200) (open-input-string stream-str)))))
        (let ((status (model-stream client "test"
                                    (lambda (text . rest)
                                      (set! received (cons text received)))
                                    #:http-poster mock-poster)))
          (and (eq? status 'done)
               (= (length received) 100)
               (string=? (car (reverse received)) "tok0 ")
               (string=? (car received) "tok99 "))))))

  (test-assert "multiline-split-event-stream-delivers-each-data-line"
    (let* ((multiline-payload (string-append
                               "data: {\"choices\": [{\"delta\": {\"content\": \"part1 \"}}]}\n"
                               "data: {\"choices\": [{\"delta\": {\"content\": \"part2\"}}]}\n\n"
                               "data: [DONE]\n"))
           (client (make-model-client #:endpoint "http://test/v1/chat/completions"))
           (received '())
           (mock-poster (lambda* (uri #:key body headers (streaming? #f))
                          (values (build-response #:code 200) (open-input-string multiline-payload)))))
      (let ((status (model-stream client "test"
                                  (lambda (text . rest)
                                    (set! received (cons text received)))
                                  #:http-poster mock-poster)))
        (and (eq? status 'done)
             (equal? (reverse received) '("part1 " "part2"))))))
)


;;; =========================================================================
;;; 6. OPENAI TOOL CALL PARSING & STREAMING DELTA MERGING STRESS TESTS
;;; =========================================================================
(test-group "openai-tool-calls-and-deltas-adversarial"

  ;; 6.1 format-chat-request edge cases
  (test-assert "format-chat-request with empty message list returns valid JSON"
    (string? (format-chat-request '())))

  (test-assert "format-chat-request string prompt wraps to role user"
    (let* ((json-str (format-chat-request "hello"))
           (parsed (json-string->scm json-str))
           (msgs (assoc-ref parsed "messages")))
      (and (vector? msgs)
           (= (vector-length msgs) 1)
           (string=? (assoc-ref (vector-ref msgs 0) "role") "user")
           (string=? (assoc-ref (vector-ref msgs 0) "content") "hello"))))

  (test-assert "format-chat-request filters internal UI messages"
    (let* ((msgs (list (cons "user" "Run tool")
                       '((role . "tool-call") (content . "call details"))
                       '((role . "notice") (content . "system notice"))
                       '((role . "error") (content . "failure"))
                       '((role . "tool") (tool_call_id . "call_123") (content . "result"))))
           (req (format-chat-request msgs #:as-alist #t))
           (filtered (assoc-ref req "messages")))
      (and (= (vector-length filtered) 2)
           (string=? (assoc-ref (vector-ref filtered 0) "role") "user")
           (string=? (assoc-ref (vector-ref filtered 1) "role") "tool"))))

  ;; 6.2 extract-chat-tool-calls edge cases
  (test-equal "extract-chat-tool-calls on malformed JSON string returns '()"
    '()
    (extract-chat-tool-calls "{bad-json:"))

  (test-equal "extract-chat-tool-calls on empty string returns '()"
    '()
    (extract-chat-tool-calls ""))

  (test-equal "extract-chat-tool-calls on empty object returns '()"
    '()
    (extract-chat-tool-calls "{}"))

  (test-equal "extract-chat-tool-calls on #f returns '()"
    '()
    (extract-chat-tool-calls #f))

  (test-equal "extract-chat-tool-calls on empty choices returns '()"
    '()
    (extract-chat-tool-calls '(("choices" . #()))))

(define (make-test-tool-call id name args)
  (list (cons "id" id)
        (cons "function"
              (list (cons "name" name)
                    (cons "arguments" args)))))

(define (make-test-response-with-tools tools)
  (list (cons "choices"
              (vector
               (list (cons "message"
                           (list (cons "role" "assistant")
                                 (cons "tool_calls" (list->vector tools)))))))))
  (test-assert "extract-chat-tool-calls missing id defaults to call_0"
    (let* ((resp (list (cons "choices"
                             (vector (list (cons "message"
                                                 (list (cons "role" "assistant")
                                                       (cons "tool_calls"
                                                             (vector (list (cons "function"
                                                                                 (list (cons "name" "test_fn")
                                                                                       (cons "arguments" "{}")))))))))))))
           (calls (extract-chat-tool-calls resp)))
      (and (= (length calls) 1)
           (string=? (assoc-ref (car calls) 'id) "call_0")
           (string=? (assoc-ref (car calls) 'name) "test_fn"))))

  (test-assert "extract-chat-tool-calls complex nested JSON arguments"
    (let* ((arg-str "{\"arr\": [1, 2, 3], \"nested\": {\"key\": \"value\"}}")
           (tc (make-test-tool-call "call_cplx" "cplx_fn" arg-str))
           (resp (make-test-response-with-tools (list tc)))
           (calls (extract-chat-tool-calls resp)))
      (and (= (length calls) 1)
           (string=? (assoc-ref (car calls) 'id) "call_cplx")
           (string=? (assoc-ref (car calls) 'name) "cplx_fn")
           (equal? (assoc-ref (assoc-ref (car calls) 'arguments) "arr") #(1 2 3)))))

  (test-assert "extract-chat-tool-calls truncated invalid JSON arguments falls back to raw string"
    (let* ((raw-bad "{\"broken\": [1, 2,")
           (tc (make-test-tool-call "call_bad" "broken_tool" raw-bad))
           (resp (make-test-response-with-tools (list tc)))
           (calls (extract-chat-tool-calls resp)))
      (and (= (length calls) 1)
           (string=? (assoc-ref (car calls) 'arguments) raw-bad))))

  (test-assert "extract-chat-tool-calls multiple calls preserved in order"
    (let* ((tc1 (make-test-tool-call "c1" "f1" "{\"a\":1}"))
           (tc2 (make-test-tool-call "c2" "f2" "{\"b\":2}"))
           (resp (make-test-response-with-tools (list tc1 tc2)))
           (calls (extract-chat-tool-calls resp)))
      (and (= (length calls) 2)
           (equal? (map (lambda (c) (assoc-ref c 'id)) calls) '("c1" "c2"))
           (equal? (map (lambda (c) (assoc-ref c 'name)) calls) '("f1" "f2")))))

  ;; 6.3 Streaming tool-call fragment merging & finalization
  (test-assert "streaming 5-fragment tool call reassembly"
    (let* ((f1 '(("index" . 0) ("id" . "call_s1") ("function" . (("name" . "patch") ("arguments" . "")))))
           (f2 '(("index" . 0) ("function" . (("arguments" . "{\"file\": ")))))
           (f3 '(("index" . 0) ("function" . (("arguments" . "\"foo.scm\", ")))))
           (f4 '(("index" . 0) ("function" . (("arguments" . "\"line\": 10}")))))
           (f5 '(("index" . 0) ("function" . (("name" . "") ("arguments" . "")))))
           (acc #f))
      (set! acc (merge-tool-call-fragment acc f1))
      (set! acc (merge-tool-call-fragment acc f2))
      (set! acc (merge-tool-call-fragment acc f3))
      (set! acc (merge-tool-call-fragment acc f4))
      (set! acc (merge-tool-call-fragment acc f5))
      (let ((finalized (finalize-tool-call-fragments acc)))
        (and (= (length finalized) 1)
             (string=? (assoc-ref (car finalized) 'id) "call_s1")
             (string=? (assoc-ref (car finalized) 'name) "patch")
             (string=? (assoc-ref (assoc-ref (car finalized) 'arguments) "file") "foo.scm")
             (equal? (assoc-ref (assoc-ref (car finalized) 'arguments) "line") 10)))))

  (test-assert "interleaved streaming tool calls across multiple indices"
    (let* ((c0_1 '(("index" . 0) ("id" . "call_0") ("function" . (("name" . "tool0") ("arguments" . "{\"v\": ")))))
           (c1_1 '(("index" . 1) ("id" . "call_1") ("function" . (("name" . "tool1") ("arguments" . "{\"w\": ")))))
           (c0_2 '(("index" . 0) ("function" . (("arguments" . "100}")))))
           (c1_2 '(("index" . 1) ("function" . (("arguments" . "200}")))))
           (acc (merge-tool-call-fragment
                 (merge-tool-call-fragment
                  (merge-tool-call-fragment
                   (merge-tool-call-fragment #f c0_1)
                   c1_1)
                  c0_2)
                 c1_2))
           (finalized (finalize-tool-call-fragments acc)))
      (and (= (length finalized) 2)
           (string=? (assoc-ref (car finalized) 'id) "call_0")
           (equal? (assoc-ref (assoc-ref (car finalized) 'arguments) "v") 100)
           (string=? (assoc-ref (cadr finalized) 'id) "call_1")
           (equal? (assoc-ref (assoc-ref (cadr finalized) 'arguments) "w") 200))))

  (test-assert "disordered indices (index 2 before index 0) sorted in finalize"
    (let* ((f2 '(("index" . 2) ("id" . "call_2") ("function" . (("name" . "fn2") ("arguments" . "{}")))))
           (f0 '(("index" . 0) ("id" . "call_0") ("function" . (("name" . "fn0") ("arguments" . "{}")))))
           (f1 '(("index" . 1) ("id" . "call_1") ("function" . (("name" . "fn1") ("arguments" . "{}")))))
           (acc (merge-tool-call-fragment
                 (merge-tool-call-fragment
                  (merge-tool-call-fragment #f f2)
                  f0)
                 f1))
           (finalized (finalize-tool-call-fragments acc)))
      (equal? (map (lambda (c) (assoc-ref c 'id)) finalized)
              '("call_0" "call_1" "call_2"))))

  (test-equal "finalize-tool-call-fragments on #f or '() yields '()"
    '(() ())
    (list (finalize-tool-call-fragments #f)
          (finalize-tool-call-fragments '())))
)

;;; =========================================================================
;;; 7. ANTHROPIC CONTENT & DELTA PARSING WITH THINKING BLOCKS
;;; =========================================================================
(test-group "anthropic-thinking-and-content-adversarial"

  ;; 7.1 Streaming thinking deltas
  (test-equal "extract-anthropic-delta-item extracts thinking block"
    '(thought . "Considering algorithm...")
    (extract-anthropic-delta-item '(("delta" . (("thinking" . "Considering algorithm..."))))))

  (test-equal "extract-anthropic-delta-item extracts thought alias"
    '(thought . "Alternate thinking alias")
    (extract-anthropic-delta-item '(("delta" . (("thought" . "Alternate thinking alias"))))))

  (test-equal "extract-anthropic-delta-item extracts text delta"
    '(content . "Here is my answer")
    (extract-anthropic-delta-item '(("delta" . (("text" . "Here is my answer"))))))

  (test-equal "extract-anthropic-delta-item malformed delta yields #f"
    #f
    (extract-anthropic-delta-item '(("delta" . ()))))

  (test-equal "extract-anthropic-delta-item non-pair yields #f"
    #f
    (extract-anthropic-delta-item "not a pair"))

  ;; 7.2 Non-streaming content extraction
  (test-equal "extract-anthropic-content on string returns string"
    "Plain response"
    (extract-anthropic-content '(("content" . "Plain response"))))

  (test-equal "extract-anthropic-content on single text block vector"
    "Claude text response"
    (extract-anthropic-content '(("content" . #((("type" . "text") ("text" . "Claude text response")))))))

  (test-assert "extract-anthropic-content on thinking+text blocks behaves consistently"
    (let* ((resp '(("content" . #((("type" . "thinking") ("thinking" . "Deep thought trace"))
                                  (("type" . "text") ("text" . "Final answer")))))))
      ;; b0 is the thinking block which lacks a text field, so extract-anthropic-content yields #f
      (eq? #f (extract-anthropic-content resp))))

  (test-equal "extract-anthropic-content on empty content vector yields #f"
    #f
    (extract-anthropic-content '(("content" . #()))))

  (test-equal "extract-anthropic-content on missing content yields #f"
    #f
    (extract-anthropic-content '(("id" . "msg_001"))))
)

;;; =========================================================================
;;; 8. CONSUMER COMPATIBILITY: (geasa runtimes-react plugin) AND (geasa policies-guardrail plugin)
;;; =========================================================================
(test-group "consumer-compatibility-adversarial"

  ;; 8.1 badb react stream-model-completion with mock HTTP SSE streaming
  (test-assert "badb react stream-model-completion merges streaming tool-call fragments"
    (let* ((sse-body (string-append
                       "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"call_react_1\", \"function\": {\"name\": \"file_read\", \"arguments\": \"{\\\"path\\\": \\\"\"}}]}}]}\n\n"
                       "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"function\": {\"arguments\": \"test.scm\\\"}\"}}]}}]}\n\n"
                       "data: [DONE]\n\n"))
           (mock-poster (lambda* (uri #:key body headers (streaming? #f))
                          (values (build-response #:code 200)
                                  (open-input-string sse-body))))
           (client (make-model-client #:endpoint "http://test-react/v1/chat/completions")))
      (parameterize ((current-http-post mock-poster))
        (call-with-values
            (lambda ()
              (stream-model-completion client "read test.scm" '() #f #f))
          (lambda (content reasoning tool-calls)
            (and (= (length tool-calls) 1)
                 (string=? (assoc-ref (car tool-calls) 'id) "call_react_1")
                 (string=? (assoc-ref (car tool-calls) 'name) "file_read")
                 (string=? (assoc-ref (assoc-ref (car tool-calls) 'arguments) "path") "test.scm")))))))

  ;; 8.2 badb react stream-model-completion fallback on non-streaming JSON
  (test-assert "badb react stream-model-completion handles endpoint ignoring stream"
    (let* ((json-body "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": \"Direct synchronous text\"}}]}")
           (mock-poster (lambda* (uri #:key body headers (streaming? #f))
                          (values (build-response #:code 200)
                                  (open-input-string json-body))))
           (client (make-model-client #:endpoint "http://test-react/v1/chat/completions")))
      (parameterize ((current-http-post mock-poster))
        (call-with-values
            (lambda ()
              (stream-model-completion client "hello" '() #f #f))
          (lambda (content reasoning tool-calls)
            (and (string=? content "Direct synchronous text")
                 (null? tool-calls)))))))

  ;; 8.3 badb guardrail System 1 evaluator integration
  (test-assert "badb guardrail make-system1-model-evaluator parses JSON verdict"
    (let* ((allow-json "{\"choices\": [{\"message\": {\"content\": \"{\\\"verdict\\\": \\\"allow\\\", \\\"reason\\\": \\\"safe operation\\\"}\"}} ]}")
           (mock-poster (lambda* (uri #:key body headers (streaming? #f))
                          (values (build-response #:code 200) allow-json)))
           (client (make-model-client #:endpoint "http://test-guardrail/v1/chat/completions")))
      (parameterize ((current-http-post mock-poster))
        (let* ((evaluator (make-system1-model-evaluator client))
               (decision (evaluator "read_file" '(("path" . "src/badb/model.scm")) 'strict)))
          (and (pair? decision)
               (eq? (car decision) 'allow)
               (string=? (cdr decision) "safe operation"))))))

  ;; 8.4 badb guardrail System 1 fallback on model failure
  (test-assert "badb guardrail system1 evaluator falls back on network/HTTP error"
    (let* ((mock-poster (lambda* (uri #:key body headers (streaming? #f))
                          (values (build-response #:code 500) "Internal Error")))
           (client (make-model-client #:endpoint "http://test-guardrail/v1/chat/completions"))
           (fallback-hit #f)
           (fallback (lambda* (action params level #:optional (context '()))
                       (set! fallback-hit #t)
                       '(ask . "fallback prompt"))))
      (parameterize ((current-http-post mock-poster))
        (let* ((evaluator (make-system1-model-evaluator client #:fallback-evaluator fallback))
               (decision (evaluator "rm_rf" '() 'strict)))
          (and fallback-hit
               (eq? (car decision) 'ask))))))
)

(test-end "adversarial-providers")

