;;; test-react.scm — Tests for Autonomous ReAct Engine (geasa runtimes-react plugin)
;;;
;;; Verifies tool schema conversion, tool call parsing, guardrail enforcement,
;;; tool dispatch (registry and MCP), provenance logging, and multi-turn ReAct loops.

(define-module (test test-react)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (web response)
  #:use-module (geasa providers-model common)
  #:use-module (geasa providers-model openai)
  #:use-module (geasa providers-model mock)
  #:use-module (geasa foundation-config plugin)
  #:use-module (badb kernel registry)
  #:use-module (badb kernel journal)
  #:use-module (badb kernel transaction)
  #:use-module (geasa capabilities-mcp plugin)
  #:use-module (geasa runtimes-react plugin)
  #:use-module (ice-9 optargs))

(spec-begin "runtimes-react")

;;;
;;; 1. Tool Schema Conversion
;;;
(test-group "react-tool-schemas"
            (let*
                ((reg (make-registry))
                 (_
                   (registry-attach! reg
                                     "calc.add"
                                     (lambda (a b)
                                       (+ a b))
                                     '((description . "Add two numbers")
                                       (parameters .
                                        ((type . "object")
                                         (properties . ((a . ((type . "number"))) (b . ((type . "number")))))
                                         (required . #("a" "b")))))))
                 (features (registry-list reg))
                 (feat-spec (registry-feature->tool-spec (car features))))
              (test-equal "tool spec type is function" "function" (assoc-ref feat-spec 'type))
              (let ((fn (assoc-ref feat-spec 'function)))
                (test-equal "normalized name replaces dots" "calc_add" (assoc-ref fn 'name))
                (test-equal "description preserved"
                            "Add two numbers"
                            (assoc-ref fn 'description))
                (test-assert "parameters schema present" (assoc-ref fn 'parameters))))
            ;; MCP Tool Conversion
            (let*
                ((mcp-raw-tool
                   '((name . "read_file")
                     (description . "Read file from disk")
                     (server . "fs")
                     (inputSchema .
                      ((type . "object") (properties . ((path . ((type . "string")))))))))
                 (mcp-spec (mcp-tool->tool-spec mcp-raw-tool)))
              (test-equal "mcp tool spec type is function"
                          "function"
                          (assoc-ref mcp-spec 'type))
              (let ((fn (assoc-ref mcp-spec 'function)))
                (test-equal "mcp tool name preserved" "read_file" (assoc-ref fn 'name))
                (test-equal "mcp tool description preserved"
                            "Read file from disk"
                            (assoc-ref fn 'description))))
            (let*
                ((zero-arg-tool
                   '((name . "no_arg_tool")
                     (description . "Zero args tool")
                     (server . "test")
                     (inputSchema . ((type . "object") (additionalProperties . #f)))))
                 (spec (mcp-tool->tool-spec zero-arg-tool))
                 (fn (assoc-ref spec 'function))
                 (params (assoc-ref fn 'parameters)))
              (test-equal "mcp zero-arg tool gets object type"
                          "object"
                          (or (assoc-ref params "type") (assoc-ref params 'type)))
              (test-assert
                "mcp zero-arg tool injects empty properties"
                (or (assoc-ref params "properties") (assoc-ref params 'properties)))))

;;;
;;; 2. Tool Call Parsing (Native & XML)
;;;
(test-group "react-tool-parsing"
  ;; Native OpenAI format
  (let* ((native-raw
          '((choices . #(((message . ((role . "assistant")
                                      (content . #f)
                                      (tool_calls . #(((id . "call_123")
                                                       (type . "function")
                                                       (function . ((name . "git_status")
                                                                    (arguments . "{\"repo\":\"badb\"}")))))))))))))
         (parsed (parse-tool-calls native-raw "")))
    (test-equal "parses 1 native tool call" 1 (length parsed))
    (let ((c (car parsed)))
      (test-equal "call id matches" "call_123" (assoc-ref c 'id))
      (test-equal "call name matches" "git_status" (assoc-ref c 'name))
      (test-equal "call argument matches" "badb" (assoc-ref (assoc-ref c 'arguments) "repo"))))

  ;; Embedded XML format fallback
  (let* ((content-xml "I will check the status now.\n<tool_call>{\"name\": \"git_status\", \"arguments\": {\"repo\": \"badb\"}}</tool_call>\nWaiting for output.")
         (parsed (parse-tool-calls #f content-xml)))
    (test-equal "parses 1 xml tool call" 1 (length parsed))
    (let ((c (car parsed)))
      (test-equal "parsed xml tool name" "git_status" (assoc-ref c 'name))
      (test-equal "parsed xml arguments" "badb" (assoc-ref (assoc-ref c 'arguments) "repo"))))

  ;; Content without tool calls
  (let ((no-calls (parse-tool-calls '((choices . #(((message . ((role . "assistant") (content . "Hello world")))))))
                                    "Hello world")))
    (test-assert "empty list when no tool calls" (null? no-calls))))

;;;
;;; 3. Guardrail Enforcement & Execution
;;;
(test-group "react-guardrails"
  (let* ((reg (make-registry))
         (_ (registry-attach! reg "danger.delete"
                              (lambda (target) (string-append "Deleted " target))
                              '((description . "Delete target"))))
         (tool-call '((id . "c1")
                      (name . "danger_delete")
                      (arguments . (("target" . "/tmp/important"))))))

    ;; Denied by guardrail
    (call-with-values
        (lambda ()
          (execute-tool-call tool-call reg #f
                             #:guardrail (lambda (name args)
                                           (if (string-prefix? "danger" name)
                                               'deny
                                               #t))))
      (lambda (res err?)
        (test-assert "denied call returns error true" err?)
        (test-assert "error mentions guardrail" (string-contains (assoc-ref res 'error) "guardrail"))))

    ;; Allowed by guardrail
    (call-with-values
        (lambda ()
          (execute-tool-call tool-call reg #f
                             #:guardrail (lambda (name args) #t)))
      (lambda (res err?)
        (test-assert "allowed call executes successfully" (not err?))
        (test-equal "execution result" "Deleted /tmp/important" res)))))

;;;
;;; 4. Tool Execution with Provenance Logging
;;;
(test-group "react-tool-dispatch-and-journal"
  (let* ((reg (make-registry))
         (jrn-path "/tmp/badb-test-react-jrn.jsonl")
         (_ (when (file-exists? jrn-path) (delete-file jrn-path)))
         (jrn (make-journal jrn-path))
         (_ (registry-attach! reg "math.mul"
                              (lambda (x y) (* x y))
                              '((description . "Multiply two numbers"))))
         (tool-call '((id . "c2")
                      (name . "math_mul")
                      (arguments . (("x" . 6) ("y" . 7))))))

    (call-with-values
        (lambda ()
          (execute-tool-call tool-call reg #f #:jrn jrn))
      (lambda (res err?)
        (test-assert "multiplication succeeds" (not err?))
        (test-equal "multiplication result" 42 res)))

    ;; Verify journal recorded the tool execution
    (test-equal "journal count is 1" 1 (journal-count jrn))
    (let ((entry (car (journal-entries jrn))))
      (test-equal "journal status is success" "success" (assoc-ref entry "status"))
      (test-equal "journal feature is math_mul" "math_mul" (assoc-ref entry "feature-id")))))

;;;
;;; 5. Multi-turn Autonomous ReAct Loop
;;;
(test-group "react-autonomous-loop"
  (let* ((reg (make-registry))
         (_ (registry-attach! reg "get.weather"
                              (lambda (city) (format #f "The weather in ~a is sunny and 22C" city))
                              '((description . "Get current weather"))))
         (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
         (turn 0)
         ;; Mock poster simulating multi-turn ReAct:
         ;; Turn 1: Model issues tool call get_weather("Dublin")
         ;; Turn 2: Model sees observation and outputs final answer
         (mock-poster
          (lambda* (uri #:key body headers streaming?)
            (set! turn (+ turn 1))
            (if (= turn 1)
                ;; Turn 1 response: Tool call
                (let ((resp-body (scm->json-string
                                  '((choices . #(((message . ((role . "assistant")
                                                              (content . "Let me check the weather.")
                                                              (tool_calls . #(((id . "call_w1")
                                                                               (type . "function")
                                                                               (function . ((name . "get_weather")
                                                                                            (arguments . "{\"city\":\"Dublin\"}")))))))))))))))
                  (values (build-response #:code 200) resp-body))
                ;; Turn 2 response: Final answer
                (let ((resp-body (scm->json-string
                                  '((choices . #(((message . ((role . "assistant")
                                                              (content . "The weather in Dublin is sunny and 22C."))))))))))
                  (values (build-response #:code 200) resp-body))))))

    (parameterize ((current-http-post mock-poster))
      (let* ((session (make-react-session client
                                          #:initial-messages '(((role . "user") (content . "What's the weather in Dublin?")))
                                          #:reg reg
                                          #:max-iterations 5)))

        ;; Step 1: Model invokes tool
        (call-with-values (lambda () (react-step! session))
          (lambda (status results)
            (test-equal "turn 1 status is continue" 'continue status)
            (test-equal "1 tool call executed" 1 (length results))))

        ;; Messages should now contain user + assistant(tool_calls) + tool observation
        (test-equal "messages count is 3" 3 (length (react-session-messages session)))

        (test-equal "assistant message has effective reasoning_content"
          "Invoking get_weather..." (assoc-ref (cadr (react-session-messages session)) 'reasoning_content))

        ;; Step 2: Model provides final answer
        (call-with-values (lambda () (react-step! session))
          (lambda (status answer)
            (test-equal "turn 2 status is completed" 'completed status)
            (test-equal "final answer matches" "The weather in Dublin is sunny and 22C." answer)))

        (test-equal "session status is completed" 'completed (react-session-status session))))))

;;;
;;; 6. Reasoning-only model completion fallback
;;;
(test-group "react-reasoning-only-completion"
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "reasoning-llm"))
         (mock-poster
          (lambda* (uri #:key body headers streaming?)
            (let ((resp-body (scm->json-string
                              '((choices . #(((message . ((role . "assistant")
                                                          (content . :null)
                                                          (reasoning_content . "Detailed chain of thought."))))))))))
              (values (build-response #:code 200) resp-body)))))
    (parameterize ((current-http-post mock-poster))
      (let ((session (make-react-session client
                                         #:initial-messages '(((role . "user") (content . "Analyze.")))
                                         #:reg (make-registry))))
        (call-with-values (lambda () (react-step! session))
          (lambda (status answer)
            (test-equal "reasoning-only turn completed" 'completed status)
            (test-assert "answer contains reasoning in think block"
              (string-contains answer "<think>Detailed chain of thought.</think>"))))))))

;;;
;;; 6. Safety Bound: Max Iterations Reached
;;;
(test-group "react-unlimited-by-default"
  ;; No explicit #:max-iterations -> no cap: the loop keeps stepping
  ;; past the old default of 10 instead of tripping max-iterations.
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "looping-llm"))
         (mock-poster
          (lambda* (uri #:key body headers streaming?)
            (let ((resp-body (scm->json-string
                              '((choices . #(((message . ((role . "assistant")
                                                          (content . "Looping...")
                                                          (tool_calls . #(((id . "call_loop")
                                                                           (type . "function")
                                                                           (function . ((name . "ping")
                                                                                        (arguments . "{}")))))))))))))) )
              (values (build-response #:code 200) resp-body))))
         (reg (make-registry))
         (_ (registry-attach! reg "ping" (lambda () "pong"))))
    (test-assert "default session carries no iteration cap"
      (not (react-session-max-iterations
            (make-react-session client #:reg reg))))
    (parameterize ((current-http-post mock-poster))
      (let* ((session (make-react-session client
                                          #:initial-messages '(((role . "user") (content . "infinite loop")))
                                          #:reg reg)))
        (let loop ((n 0) (ever-reached? #f))
          (if (>= n 12)
              (begin
                (test-assert "12 steps never trip max-iterations-reached"
                  (not ever-reached?))
                (test-equal "session iteration counted past the old default"
                  12 (react-session-iteration session)))
              (let ((verdict (catch #t
                               (lambda ()
                                 (call-with-values
                                   (lambda () (react-step! session))
                                   (lambda (v . _) v)))
                               (lambda args 'error))))
                (loop (+ n 1)
                      (or ever-reached?
                          (eq? verdict 'max-iterations-reached))))))))))

(test-group "react-max-iterations"
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "looping-llm"))
         (mock-poster
          (lambda* (uri #:key body headers streaming?)
            (let ((resp-body (scm->json-string
                              '((choices . #(((message . ((role . "assistant")
                                                          (content . "Looping...")
                                                          (tool_calls . #(((id . "call_loop")
                                                                           (type . "function")
                                                                           (function . ((name . "ping")
                                                                                        (arguments . "{}")))))))))))))))
              (values (build-response #:code 200) resp-body))))
         (reg (make-registry))
         (_ (registry-attach! reg "ping" (lambda () "pong"))))

    (parameterize ((current-http-post mock-poster))
      (let* ((session (make-react-session client
                                          #:initial-messages '(((role . "user") (content . "infinite loop")))
                                          #:reg reg
                                          #:max-iterations 3)))
        (call-with-values (lambda () (react-run! session))
          (lambda (status final-text sess)
            (test-equal "status hits max-iterations-reached" 'max-iterations-reached status)
            (test-equal "iteration count matches limit" 3 (react-session-iteration sess))
            ;; Verify that all tool messages in the session transcript have valid non-empty tool_call_id
            (let* ((msgs (react-session-messages sess))
                   (tool-msgs (filter (lambda (m) (equal? (assoc-ref m 'role) "tool")) msgs)))
              (test-assert "tool messages are present in transcript" (pair? tool-msgs))
              (test-assert "every tool message has non-empty tool_call_id" (every (lambda (m)
                         (let ((tid (assoc-ref m 'tool_call_id)))
                           (and (string? tid) (not (string-null? tid)))))
                       tool-msgs)))

            ;; Now simulate typing 'Continue': create a new session resuming with full transcript
            (let* ((continued-msgs (append (react-session-messages sess)
                                           '(((role . "user") (content . "Continue")))))
                   (completer-poster
                    (lambda* (uri #:key body headers streaming?)
                      (let ((resp-body (scm->json-string
                                        '((choices . #(((message . ((role . "assistant")
                                                                    (content . "Continued and finished!"))))))))))
                        (values (build-response #:code 200) resp-body))))
                   (cont-session (parameterize ((current-http-post completer-poster))
                                   (make-react-session client
                                                       #:initial-messages continued-msgs
                                                       #:reg reg
                                                       #:max-iterations 3))))
              (parameterize ((current-http-post completer-poster))
                (call-with-values (lambda () (react-run! cont-session))
                  (lambda (c-status c-final c-sess)
                    (test-equal "resumed session completes successfully" 'completed c-status)
                    (test-equal "resumed final text matches" "Continued and finished!" c-final)))))))))))

;;;
;;; 7. Flexible Keyword Session Construction
;;;
(test-group "react-session-keyword-construction"
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
         (reg (make-registry))
         (guard-called #f)
         (guard-fn (lambda (n a) (set! guard-called #t) #t))
         (session (make-react-session
                   #:client client
                   #:messages '(((role . "user") (content . "test message")))
                   #:registry reg
                   #:max-iterations 8
                   #:max-tokens 2048
                   #:guardrail guard-fn)))
    (test-assert "session record created" (react-session? session))
    (test-equal "messages retrieved" 1 (length (react-session-messages session)))
    (test-equal "reg matches" reg (react-session-reg session))
    (test-equal "max-iterations matches" 8 (react-session-max-iterations session))
    (test-equal "max-tokens matches" 2048 (react-session-max-tokens session))
    (test-assert "guardrail registered in hooks"
      (procedure? (assoc-ref (react-session-hooks session) 'guardrail)))))

;;;
;;; 8. Steering Queue Unit Tests
;;;
(test-group "react-steering-queue"
  (let ((q (make-steering-queue)))
    (test-assert "new queue is a steering-queue" (steering-queue? q))
    (test-assert "new queue is empty" (steering-empty? q))
    (test-equal "new queue count is 0" 0 (steering-count q))
    (steering-push! q "first instruction")
    (steering-push! q "second instruction")
    (steering-push! q 'interrupt)
    (test-equal "count is 3 after pushes" 3 (steering-count q))
    (test-assert "loaded queue is not empty" (not (steering-empty? q)))
    (test-equal "drain preserves FIFO order"
                (list "first instruction" "second instruction" 'interrupt)
                (steering-drain! q))
    (test-assert "drain empties the queue" (steering-empty? q))
    (test-equal "count is 0 after drain" 0 (steering-count q))
    (test-equal "draining empty queue yields nil" '() (steering-drain! q))
    ;; Push after drain works again (queue remains usable)
    (steering-push! q "post-drain instruction")
    (test-equal "queue reusable after drain" 1 (steering-count q))
    (test-equal "second drain returns new entry"
                (list "post-drain instruction")
                (steering-drain! q))))

;;;
;;; 9. Steering Mid-Loop: Injected Course Correction
;;;
(test-group "react-steering-mid-loop"
  (let* ((reg (make-registry))
         (_ (registry-attach! reg "get.weather"
                              (lambda (city) (format #f "The weather in ~a is sunny and 22C" city))
                              '((description . "Get current weather"))))
         (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
         (queue (make-steering-queue))
         (request-bodies '())
         (turn 0)
         (mock-poster
          (lambda* (uri #:key body headers streaming?)
            (set! turn (+ turn 1))
            (set! request-bodies (cons body request-bodies))
            (if (= turn 1)
                ;; Turn 1: tool call
                (let ((resp-body (scm->json-string
                                  '((choices . #(((message . ((role . "assistant")
                                                              (content . "Let me check the weather.")
                                                              (tool_calls . #(((id . "call_st1")
                                                                               (type . "function")
                                                                               (function . ((name . "get_weather")
                                                                                            (arguments . "{\"city\":\"Dublin\"}")))))))))))))))
                  (values (build-response #:code 200) resp-body))
                ;; Turn 2: final answer
                (let ((resp-body (scm->json-string
                                  '((choices . #(((message . ((role . "assistant")
                                                              (content . "Steered final answer."))))))))))
                  (values (build-response #:code 200) resp-body))))))
    (parameterize ((current-http-post mock-poster))
      (let* ((hooks `((on-step . ,(lambda (iter phase)
                                    (when (eq? phase 'tools-executed)
                                      (steering-push! queue "Prefer metric units."))))))
             (session (make-react-session client
                                          #:initial-messages '(((role . "user") (content . "What's the weather in Dublin?")))
                                          #:reg reg
                                          #:max-iterations 5
                                          #:steering-queue queue
                                          #:hooks hooks)))
        (call-with-values (lambda () (react-run! session))
          (lambda (status final-text sess)
            (test-equal "steered run still completes" 'completed status)
            (test-equal "steered final text matches" "Steered final answer." final-text)
            (test-equal "exactly two LLM calls made" 2 turn)
            ;; The second LLM call must see the injected steering user message
            (let* ((second-msgs (vector->list
                                  (assoc-ref (json-string->scm (car request-bodies)) "messages")))
                   (steer-msgs (filter (lambda (m)
                                         (and (equal? (assoc-ref m "role") "user")
                                              (equal? (assoc-ref m "content") "Prefer metric units.")))
                                       second-msgs)))
              (test-assert "steering user message visible to second LLM call"
                           (pair? steer-msgs)))
            ;; The steering message is also persisted in the session transcript
            (test-assert "steering message persisted in session transcript"
                         (any (lambda (m)
                                (and (equal? (assoc-ref m 'role) "user")
                                     (equal? (assoc-ref m 'content) "Prefer metric units.")))
                              (react-session-messages sess)))
            (test-assert "steering queue fully drained after run" (steering-empty? queue))))))))

;;;
;;; 10. Steering Interrupt: Halt the Running Loop
;;;
(test-group "react-steering-interrupt"
  (let* ((reg (make-registry))
         (_ (registry-attach! reg "get.weather"
                              (lambda (city) (format #f "The weather in ~a is sunny and 22C" city))
                              '((description . "Get current weather"))))
         (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
         (queue (make-steering-queue))
         (llm-calls 0)
         (mock-poster
          (lambda* (uri #:key body headers streaming?)
            (set! llm-calls (+ llm-calls 1))
            ;; Every turn issues a tool call; only the interrupt stops the loop
            (let ((resp-body (scm->json-string
                              '((choices . #(((message . ((role . "assistant")
                                                          (content . "Let me check the weather.")
                                                          (tool_calls . #(((id . "call_in1")
                                                                           (type . "function")
                                                                           (function . ((name . "get_weather")
                                                                                        (arguments . "{\"city\":\"Dublin\"}")))))))))))))))
              (values (build-response #:code 200) resp-body)))))
    (parameterize ((current-http-post mock-poster))
      (let* ((hooks `((on-step . ,(lambda (iter phase)
                                    (when (eq? phase 'tools-executed)
                                      (steering-push! queue 'interrupt))))))
             (session (make-react-session client
                                          #:initial-messages '(((role . "user") (content . "What's the weather in Dublin?")))
                                          #:reg reg
                                          #:max-iterations 5
                                          #:steering-queue queue
                                          #:hooks hooks)))
        (call-with-values (lambda () (react-run! session))
          (lambda (status final-text sess)
            (test-equal "interrupt returns interrupted verdict" 'interrupted status)
            (test-equal "no LLM call after interrupt" 1 llm-calls)
            (test-equal "session status is interrupted" 'interrupted (react-session-status sess))
            (test-equal "final content is last assistant content"
                        "Let me check the weather."
                        final-text)
            (test-equal "loop stopped after first iteration" 1 (react-session-iteration sess))
            (test-assert "queue drained on interrupt" (steering-empty? queue)))))

      ;; Interrupt queued before the run starts halts before the first LLM call
      (let* ((pre-queue (make-steering-queue))
             (_ (steering-push! pre-queue 'interrupt))
             (pre-session (make-react-session client
                                              #:initial-messages '(((role . "user") (content . "What's the weather in Dublin?")))
                                              #:reg reg
                                              #:max-iterations 5
                                              #:steering-queue pre-queue)))
        (call-with-values (lambda () (react-run! pre-session))
          (lambda (status final-text sess)
            (test-equal "pre-queued interrupt verdict is interrupted" 'interrupted status)
            (test-equal "still no additional LLM call" 1 llm-calls)
            (test-equal "no content accumulated before first call" #f final-text)
            (test-equal "iteration untouched by pre-run interrupt" 0 (react-session-iteration sess))))))))

;;;
;;; 11. Backward Compatibility: No Steering Queue
;;;
(test-group "react-no-steering-queue"
  (let* ((client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
         (session (make-react-session client
                                      #:initial-messages '(((role . "user") (content . "hello")))
                                      #:max-iterations 2)))
    (test-equal "session without steering queue has #f queue"
                #f
                (react-session-steering-queue session))
    (test-equal "iteration starts at 0" 0 (react-session-iteration session))))

(test-group "react-hook-failure-isolation"
            ;; A crashing on-tool-call / on-tool-result hook must never abort the
            ;; ReAct loop: the failure is reported and the tool observation is still
            ;; fed back to the next model call.
            (let*
                ((client
                   (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "hook-test"))
                 (call-count 0)
                 (turn1
                   (scm->json-string
                     '((choices .
                        #(((message .
                            ((role . "assistant") (content . "Calling.")
                             (tool_calls .
                              #(((id . "call_h1") (type . "function")
                                 (function . ((name . "ping") (arguments . "{}"))))))))))))))
                 (turn2
                   (scm->json-string
                     '((choices . #(((message . ((role . "assistant") (content . "Recovered.")))))))))
                 (mock-poster
                   (lambda* (uri #:key body headers streaming?)
                            (set! call-count (1+ call-count))
                            (values (build-response #:code 200)
                                    (if (= call-count 1)
                                      turn1
                                      turn2))))
                 (reg (make-registry)))
              (registry-attach! reg
                                "ping"
                                (lambda ()
                                  "pong")
                                '((description . "Ping")))
              (parameterize ((current-http-post mock-poster))
                            (let*
                                ((session
                                   (make-react-session client
                                                       #:initial-messages
                                                       '(((role . "user") (content . "call it")))
                                                       #:reg
                                                       reg
                                                       #:max-iterations
                                                       5
                                                       #:hooks
                                                       (list
                                                         (cons 'on-tool-result
                                                               (lambda (tc res err?)
                                                                 (error "hook explosion"))))))
                                 (verdict-result
                                   (call-with-values
                                     (lambda ()
                                       (react-run! session))
                                     (lambda (verdict content sess)
                                       (list verdict content sess)))))
                              (test-equal "loop completes despite failing hook"
                                          'completed
                                          (car verdict-result))
                              (test-equal "final answer produced" "Recovered." (cadr verdict-result))
                              (test-equal "both LLM calls happened" 2 call-count)
                              (test-assert "tool observation still recorded"
                                           (any
                                             (lambda (m)
                                               (and (equal? (assoc-ref m 'role) "tool")
                                                    (string-contains (assoc-ref m 'content) "pong")))
                                             (react-session-messages (caddr verdict-result))))))))

;;;
;;; 9. Agentic Tool Call Through a Spawned Dummy MCP Server
;;;
;;; Spawns test/dummy-mcp-server.scm as a real subprocess via the MCP
;;; manager (the same spawn/initialize/discover path production uses),
;;; then runs the full ReAct loop with a mock model: turn 1 issues a
;;; native tool call for the server's echo tool, the loop must execute
;;; it over stdio JSON-RPC, and turn 2 must receive the observation
;;; back as a tool message before the model answers.
;;;
(test-group "react-mcp-agentic-tool-call"
  (let* ((script-path
          (or (search-path %load-path "test/dummy-mcp-server.scm")
              "test/dummy-mcp-server.scm"))
         (cfg (make-badb-config))
         (_ (badb-config-add-mcp-server!
             cfg "dummy-mcp" "guile" (list "--no-auto-compile" script-path)))
         (mgr (make-mcp-manager cfg))
         (client (make-model-client #:endpoint "http://127.0.0.1:10100"
                                    #:model "mock-llm"))
         (turn 0)
         (model-saw-echo-tool #f)
         (model-saw-observation #f))
    (call-with-values (lambda () (mcp-manager-start-server! mgr "dummy-mcp"))
      (lambda (srv-client tools error)
        (test-assert "dummy MCP server started without error"
          (and srv-client (not error)))
        (test-equal "server advertises 2 tools" 2 (length tools))
        (test-assert "echo tool advertised"
          (any (lambda (t) (equal? (assoc-ref t "name") "echo")) tools))

        (when srv-client
          ;; Direct tool call on the spawned server, bypassing the model.
          (let ((content (mcp-manager-call-tool mgr "add"
                                                 '(("a" . 19) ("b" . 23)))))
            (test-equal "direct add call via dummy server returns 42"
              "42" (assoc-ref (car (tools->list content)) "text")))

          ;; Mock model: turn 1 calls the echo tool over MCP; turn 2
          ;; receives the observation and produces the final answer.
          (let ((mock-poster
                 (lambda* (uri #:key body headers streaming?)
                   (set! turn (+ turn 1))
                   (if (= turn 1)
                       (let* ((parsed (json-string->scm body))
                              (tools-offered
                               (or (assoc-ref parsed "tools") #())))
                         (set! model-saw-echo-tool
                               (any (lambda (t)
                                      (equal?
                                       (assoc-ref
                                        (assoc-ref t "function") "name")
                                       "echo"))
                                    (vector->list tools-offered)))
                         (values
                          (build-response #:code 200)
                          (scm->json-string
                           '((choices
                              .
                              #(((message
                                  .
                                  ((role . "assistant")
                                   (content . "Let me echo that.")
                                   (tool_calls
                                    .
                                    #(((id . "call_m1")
                                       (type . "function")
                                       (function
                                        .
                                        ((name . "echo")
                                         (arguments
                                          .
                                          "{\"text\":\"hello from dummy mcp\"}")))))))))))))))
                       (let* ((parsed (json-string->scm body))
                              (msgs
                               (vector->list
                                (or (assoc-ref parsed "messages") #())))
                              (tool-msgs
                               (filter (lambda (m)
                                         (equal? (assoc-ref m "role") "tool"))
                                       msgs)))
                         (set! model-saw-observation
                               (and (= 1 (length tool-msgs))
                                    (equal? (assoc-ref (car tool-msgs)
                                                       "tool_call_id")
                                            "call_m1")
                                    (equal? (assoc-ref (car tool-msgs) "name")
                                            "echo")
                                    (string-contains
                                     (assoc-ref (car tool-msgs) "content")
                                     "echo: hello from dummy mcp")))
                         (values
                          (build-response #:code 200)
                          (scm->json-string
                           '((choices
                              .
                              #(((message
                                  .
                                  ((role . "assistant")
                                   (content
                                    .
                                    "The dummy MCP server replied: echo: hello from dummy mcp."))))))))))))))

            (parameterize ((current-http-post mock-poster))
              (let* ((session
                      (make-react-session client
                                          #:initial-messages
                                          '(((role . "user")
                                             (content . "Echo 'hello from dummy mcp' through the dummy MCP server.")))
                                          #:mcp-manager mgr
                                          #:max-iterations 5))
                     (verdict-result
                      (call-with-values
                          (lambda () (react-run! session))
                        (lambda (verdict content sess)
                          (list verdict content sess)))))
                (test-equal "agentic loop completed" 'completed
                  (car verdict-result))
                (test-equal "final answer reflects dummy MCP observation"
                  "The dummy MCP server replied: echo: hello from dummy mcp."
                  (cadr verdict-result))
                (test-assert "model was offered the MCP echo tool"
                  model-saw-echo-tool)
                (test-assert "model received the dummy server's observation"
                  model-saw-observation)
                (test-equal "both model turns happened" 2 turn)
                (let ((msgs (react-session-messages (caddr verdict-result))))
                  (test-equal "transcript has 4 messages" 4 (length msgs))
                  (let ((tool-msg (caddr msgs)))
                    (test-equal "tool message name is echo" "echo"
                      (assoc-ref tool-msg 'name))
                    (test-assert "tool message carries server response"
                      (string-contains (assoc-ref tool-msg 'content)
                                       "echo: hello from dummy mcp"))))))))

        ;; Release the spawned server process in every outcome.
        (mcp-manager-stop-all! mgr)))))

(spec-end)
(spec-exit)
