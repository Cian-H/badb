;;; test-react-stream.scm — Streaming ReAct step tests
;;;
;;; Verifies the streaming completion consumer (stream-model-completion):
;;; streamed tool-call fragment assembly, the two-step streamed tool loop
;;; driven through react-run!, and the model-complete fallback for
;;; endpoints that ignore stream:true.

(define-module (test test-react-stream)
  #:use-module (srfi srfi-1)
  #:use-module (badb spec)
  #:use-module (srfi srfi-64)
  #:use-module (json)
  #:use-module (web response)
  #:use-module (badb providers common)
  #:use-module (badb providers openai)
  #:use-module (badb providers mock)
  #:use-module (badb kernel registry)
  #:use-module (badb runtimes react))

(spec-begin "runtimes-react-stream")

(test-group "stream-model-completion-assembly"
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "s"))
         (sse (string-append
                "data: {\"choices\": [{\"delta\": {\"reasoning_content\": \"pondering\"}}]}\n\n"
                "data: {\"choices\": [{\"delta\": {\"content\": \"Working on it.\"}}]}\n\n"
                "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"c1\", \"function\": {\"name\": \"echo\", \"arguments\": \"12\"}}]}}]}

"
                "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"function\": {\"arguments\": \"34\"}}]}}]}

"
                "data: [DONE]\n"))
         (deltas '())
         (mock-poster
           (lambda* (uri #:key body headers streaming?)
             (values (build-response #:code 200) (open-input-string sse)))))
    (parameterize ((current-http-post mock-poster))
      (call-with-values
        (lambda ()
          (stream-model-completion client '() '() 4096
                                    (lambda (payload type)
                                      (set! deltas (cons (list type payload) deltas)))))
        (lambda (content reasoning tool-calls)
          (test-equal "content accumulated" "Working on it." content)
          (test-equal "reasoning accumulated" "pondering" reasoning)
          (test-equal "one tool call assembled" 1 (length tool-calls))
          (test-equal "tool name assembled" "echo" (assoc-ref (car tool-calls) 'name))
          (test-equal "tool argument pieces assembled and parsed"
                      1234 (assoc-ref (car tool-calls) 'arguments))
          (test-assert "on-delta saw thought chunks"
                       (any (lambda (e) (eq? (car e) 'thought)) deltas))
          (test-assert "on-delta saw tool-call fragments"
                       (any (lambda (e) (eq? (car e) 'tool-call)) deltas)))))))

(test-group "streamed-tool-loop"
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "s"))
         (call-count 0)
         (sse-tool (string-append
                     "data: {\"choices\": [{\"delta\": {\"content\": \"Calling the tool.\"}}]}\n\n"
                     "data: {\"choices\": [{\"delta\": {\"tool_calls\": [{\"index\": 0, \"id\": \"c1\", \"function\": {\"name\": \"ping\", \"arguments\": \"{}\"}}]}}]}\n\n"
                     "data: [DONE]\n"))
         (sse-final (string-append
                      "data: {\"choices\": [{\"delta\": {\"content\": \"All done.\"}}]}\n\n"
                      "data: [DONE]\n"))
         (mock-poster
           (lambda* (uri #:key body headers streaming?)
             (set! call-count (1+ call-count))
             (values (build-response #:code 200)
                     (open-input-string (if (= call-count 1) sse-tool sse-final)))))
         (reg (make-registry))
         (delta-events 0))
    (registry-attach! reg "ping" (lambda () "pong") '((description . "Ping")))
    (parameterize ((current-http-post mock-poster))
      (let* ((session (make-react-session client
                                          #:initial-messages '(((role . "user") (content . "call it")))
                                          #:reg reg
                                          #:max-iterations 5
                                          #:hooks
                                          (list
                                            (cons 'on-delta
                                                  (lambda (payload type)
                                                    (set! delta-events (1+ delta-events)))))))
             (verdict-result
               (call-with-values
                 (lambda () (react-run! session))
                 (lambda (verdict content sess) (list verdict content sess)))))
        (test-equal "streamed tool loop completes" 'completed (car verdict-result))
        (test-equal "final content from the stream" "All done." (cadr verdict-result))
        (test-assert "on-delta fired during generation" (> delta-events 0))
        (test-assert "tool observation recorded in the transcript"
                     (any (lambda (m)
                            (and (equal? (assoc-ref m 'role) "tool")
                                 (string-contains (assoc-ref m 'content) "pong")))
                          (react-session-messages (caddr verdict-result))))
        (test-assert "assistant tool turn recorded"
                     (any (lambda (m)
                            (and (equal? (assoc-ref m 'role) "assistant")
                                 (assoc-ref m 'tool_calls)))
                          (react-session-messages (caddr verdict-result))))))))

(test-group "streaming-fallback"
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "s"))
         (call-count 0)
         (complete-body
           (scm->json-string
             '((choices . #(((message . ((role . "assistant")
                                          (content . "Fallback answer.")
                                          (tool_calls . #())))))))))
         (mock-poster
           (lambda* (uri #:key body headers streaming?)
             (set! call-count (1+ call-count))
             (values (build-response #:code 200)
                     (if streaming?
                       (open-input-string "not-an-sse-body at all\n")
                       complete-body))))
         (reg (make-registry)))
    (parameterize ((current-http-post mock-poster))
      (let* ((session (make-react-session client
                                          #:initial-messages '(((role . "user") (content . "hi")))
                                          #:reg reg
                                          #:max-iterations 3))
             (verdict-result
               (call-with-values
                 (lambda () (react-run! session))
                 (lambda (verdict content sess) (list verdict content sess)))))
        (test-equal "fallback completes the step" 'completed (car verdict-result))
        (test-equal "fallback content extracted" "Fallback answer." (cadr verdict-result))
        (test-equal "both the streamed and fallback requests happened" 2 call-count)))))


(spec-end)
(spec-exit)
