;;; test-react.scm — Tests for Autonomous ReAct Engine (badb react)
;;;
;;; Verifies tool schema conversion, tool call parsing, guardrail enforcement,
;;; tool dispatch (registry and MCP), provenance logging, and multi-turn ReAct loops.

(define-module (test test-react)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (web response)
  #:use-module (badb model)
  #:use-module (badb registry)
  #:use-module (badb journal)
  #:use-module (badb transaction)
  #:use-module (badb mcp)
  #:use-module (badb react)
  #:use-module (ice-9 optargs))

(test-begin "badb-react")

;;;
;;; 1. Tool Schema Conversion
;;;

(test-group "react-tool-schemas"
  (let* ((reg (make-registry))
         (_ (registry-attach! reg "calc.add"
                              (lambda (a b) (+ a b))
                              '((description . "Add two numbers")
                                (parameters . ((type . "object")
                                               (properties . ((a . ((type . "number")))
                                                              (b . ((type . "number")))))
                                               (required . #("a" "b")))))))
         (features (registry-list reg))
         (feat-spec (registry-feature->tool-spec (car features))))

    (test-equal "tool spec type is function"
      "function"
      (assoc-ref feat-spec 'type))

    (let ((fn (assoc-ref feat-spec 'function)))
      (test-equal "normalized name replaces dots"
        "calc_add"
        (assoc-ref fn 'name))
      (test-equal "description preserved"
        "Add two numbers"
        (assoc-ref fn 'description))
      (test-assert "parameters schema present"
        (assoc-ref fn 'parameters))))

  ;; MCP Tool Conversion
  (let* ((mcp-raw-tool '((name . "read_file")
                         (description . "Read file from disk")
                         (server . "fs")
                         (inputSchema . ((type . "object")
                                         (properties . ((path . ((type . "string")))))))))
         (mcp-spec (mcp-tool->tool-spec mcp-raw-tool)))
    (test-equal "mcp tool spec type is function"
      "function"
      (assoc-ref mcp-spec 'type))
    (let ((fn (assoc-ref mcp-spec 'function)))
      (test-equal "mcp tool name preserved"
        "read_file"
        (assoc-ref fn 'name))
      (test-equal "mcp tool description preserved"
        "Read file from disk"
        (assoc-ref fn 'description)))))

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
    (test-equal "parses 1 native tool call"
      1
      (length parsed))
    (let ((c (car parsed)))
      (test-equal "call id matches" "call_123" (assoc-ref c 'id))
      (test-equal "call name matches" "git_status" (assoc-ref c 'name))
      (test-equal "call argument matches" "badb" (assoc-ref (assoc-ref c 'arguments) "repo"))))

  ;; Embedded XML format fallback
  (let* ((content-xml "I will check the status now.\n<tool_call>{\"name\": \"git_status\", \"arguments\": {\"repo\": \"badb\"}}</tool_call>\nWaiting for output.")
         (parsed (parse-tool-calls #f content-xml)))
    (test-equal "parses 1 xml tool call"
      1
      (length parsed))
    (let ((c (car parsed)))
      (test-equal "parsed xml tool name" "git_status" (assoc-ref c 'name))
      (test-equal "parsed xml arguments" "badb" (assoc-ref (assoc-ref c 'arguments) "repo"))))

  ;; Content without tool calls
  (let ((no-calls (parse-tool-calls '((choices . #(((message . ((role . "assistant") (content . "Hello world")))))))
                                    "Hello world")))
    (test-assert "empty list when no tool calls"
      (null? no-calls))))

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
        (test-assert "error mentions guardrail"
          (string-contains (assoc-ref res 'error) "guardrail"))))

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
        (test-equal "messages count is 3"
          3
          (length (react-session-messages session)))

        (test-equal "assistant message has effective reasoning_content"
          "Invoking get_weather..."
          (assoc-ref (cadr (react-session-messages session)) 'reasoning_content))

        ;; Step 2: Model provides final answer
        (call-with-values (lambda () (react-step! session))
          (lambda (status answer)
            (test-equal "turn 2 status is completed" 'completed status)
            (test-equal "final answer matches"
              "The weather in Dublin is sunny and 22C."
              answer)))

        (test-equal "session status is completed"
          'completed
          (react-session-status session))))))

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
            (test-equal "status hits max-iterations-reached"
              'max-iterations-reached
              status)
            (test-equal "iteration count matches limit"
              3
              (react-session-iteration sess))
            ;; Verify that all tool messages in the session transcript have valid non-empty tool_call_id
            (let* ((msgs (react-session-messages sess))
                   (tool-msgs (filter (lambda (m) (equal? (assoc-ref m 'role) "tool")) msgs)))
              (test-assert "tool messages are present in transcript"
                (pair? tool-msgs))
              (test-assert "every tool message has non-empty tool_call_id"
                (every (lambda (m)
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
                    (test-equal "resumed session completes successfully"
                      'completed
                      c-status)
                    (test-equal "resumed final text matches"
                      "Continued and finished!"
                      c-final)))))))))))

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

(test-end "badb-react")
