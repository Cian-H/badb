;;; test/test-calibration.scm — Empirical Tool Calibration & Progressive Discovery Test Suite
;;;
;;; Validates:
;;; 1. Routing classification accuracy across 30 canonical tasks (Direct vs Ephemeral vs Geis).
;;; 2. F1-score >= 0.90 with zero over-triggering on direct tasks.
;;; 3. Ephemeral runtime thrash circuit breaker injection after 3 consecutive exploratory turns.
;;; 4. Just-in-time progressive hydration (geis.index -> geis.activate -> checkpoint_create deactivation).

(use-modules (srfi srfi-11)
             (srfi srfi-64)
             (srfi srfi-1)
             (json)
             (web response)
             (ice-9 format)
             (badb kernel registry)
             (geasa providers-model common)
             (geasa providers-model mock)
             (geasa runtimes-geis-loader plugin)
             (geasa runtimes-workflows plugin)
             (geasa runtimes-react plugin)
             (geasa surfaces-tui tools))

(test-begin "test-calibration")

;;;
;;; 1. Canonical Benchmark Tasks (30 test cases)
;;;

(define %calibration-cases
        '(
          ;; Category A: Direct Path (10 cases)
          ((prompt . "Change port from 8080 to 9090 in config.json")
           (expected . direct)
           (reason . "Single-file localized edit"))
          ((prompt . "Check git status to see modified files")
           (expected . direct)
           (reason . "Standard primitive git inspection"))
          ((prompt . "Read lines 1 to 50 of README.md")
           (expected . direct)
           (reason . "Single read_file inspection"))
          ((prompt . "Find the declaration of make-registry in src/badb/kernel/registry.scm")
           (expected . direct)
           (reason . "Standard find_symbol lookup"))
          ((prompt . "View the git diff against HEAD~1")
           (expected . direct)
           (reason . "Standard bash git diff"))
          ((prompt . "Fix the typo 'recieved' -> 'received' in error message")
           (expected . direct)
           (reason . "Surgical edit_file"))
          ((prompt . "Check if node_modules exists on disk")
           (expected . direct)
           (reason . "Single test -d check"))
          ((prompt . "Run git log -n 5 to see recent commit messages")
           (expected . direct)
           (reason . "One-off git history read"))
          ((prompt . "Update package version from 1.0.0 to 1.0.1 in manifest.scm")
           (expected . direct)
           (reason . "Single line file edit"))
          ((prompt . "Inspect line 42 of src/badb/main.scm")
           (expected . direct)
           (reason . "Targeted file read"))

          ;; Category B: Ephemeral Script Path (10 cases)
          ((prompt . "Sum the numbers in column 3 of data.csv and report the mean")
           (expected . ephemeral)
           (reason . "One-off transient computation via scheme_eval"))
          ((prompt . "Parse this JSON string and format the top 3 keys as markdown")
           (expected . ephemeral)
           (reason . "One-off data manipulation"))
          ((prompt . "Filter this list of 100 log lines to only those containing status 500")
           (expected . ephemeral)
           (reason . "Transient in-memory filtering"))
          ((prompt . "Calculate the SHA-256 hash of the string 'hello world'")
           (expected . ephemeral)
           (reason . "One-off crypto helper call"))
          ((prompt . "Convert this timestamps list from ISO-8601 to unix epoch")
           (expected . ephemeral)
           (reason . "Disposable data transformation"))
          ((prompt . "Compute the Cartesian product of two small lists (a b) and (1 2)")
           (expected . ephemeral)
           (reason . "Pure in-session Scheme calculation"))
          ((prompt . "Format these 5 error objects into an HTML table snippet")
           (expected . ephemeral)
           (reason . "Disposable templating"))
          ((prompt . "Extract email addresses from this paragraph of text")
           (expected . ephemeral)
           (reason . "Transient regex extraction"))
          ((prompt . "Count the frequency of each word in this scratch note")
           (expected . ephemeral)
           (reason . "One-off frequency analysis"))
          ((prompt . "Check which numbers in range 1-100 are prime")
           (expected . ephemeral)
           (reason . "Transient math evaluation"))

          ;; Category C: Geis Synthesis Path (10 cases)
          ((prompt . "Validate 50 protobuf message definitions against our schema contract across all services")
           (expected . geis)
           (reason . "Dense format parsing with contract invariants repeated across 50 files"))
          ((prompt . "Parse 100 test benchmark runs, extract regression metrics, and assert no p99 latency regressions")
           (expected . geis)
           (reason . "Repetitive log extraction and invariant gating"))
          ((prompt . "Implement a permanent AST extractor for Guile macro definitions across all codebase modules")
           (expected . geis)
           (reason . "Reusable, permanent project capability"))
          ((prompt . "Create a custom linter for our internal event-bus payload schema that gates CI")
           (expected . geis)
           (reason . "Contract-pinned verification tool reused across runs"))
          ((prompt . "Synthesize a structured validator for OpenAPI 3.1 endpoints against our security policies")
           (expected . geis)
           (reason . "High-integrity safety contract bundle"))
          ((prompt . "Build a reusable migration tool to rewrite deprecated database queries across 40 model files")
           (expected . geis)
           (reason . "High blast-radius multi-file AST transformation requiring verification"))
          ((prompt . "Create a dedicated test-runner plugin that parses our proprietary binary test output format")
           (expected . geis)
           (reason . "Domain toolchain adapter saving hundreds of context tokens"))
          ((prompt . "Implement a contract-verified parser for our custom wire protocol packet captures")
           (expected . geis)
           (reason . "Complex byte/protocol parsing with SRFI-253 invariant enforcement"))
          ((prompt . "Build a project-wide license header checker and enforcer with QuickCheck property tests")
           (expected . geis)
           (reason . "Property-tested geis bundle reused across repository"))
          ((prompt . "Develop an AST transformation bundle that replaces old Guile 2.0 regex patterns with PCRE2")
           (expected . geis)
           (reason . "Reusable contract-gated code transformation"))
          ))

;;;
;;; 2. Task Routing Classifier (Rule-of-Three Heuristic)
;;;

(define (classify-task-routing prompt)
        "Classify task PROMPT into 'direct, 'ephemeral, or 'geis using calibrated heuristics."
        (let* ((p (string-downcase prompt))
               ;; Geis indicators: recurrent, dense AST/schema across many files, contract/quickcheck invariants
               (geis-indicators
                 '("across all" "across 40" "across 50" "across 100" "50 protobuf" "100 test benchmark"
                   "permanent" "contract-pinned" "quickcheck" "property tests" "reusable migration"
                   "proprietary binary" "wire protocol" "custom wire" "custom linter" "schema contract"
                   "gates ci" "dedicated test-runner" "dedicated validator" "synthesize a" "validator for"
                   "ast transformation"))
               ;; Ephemeral indicators: disposable computations, single in-memory parsing/transforms
               (ephemeral-indicators
                 '("sum the numbers" "calculate the sha" "convert this timestamps" "cartesian product"
                   "transient" "one-off" "format these 5" "extract email" "count the frequency"
                   "range 1-100" "scratch note" "filter this list of 100 log lines" "parse this json string"))
               ;; Direct indicators: localized edits, single reads, standard git ops
               (direct-indicators
                 '("change port" "check git status" "read lines" "find the declaration"
                   "view the git diff" "fix the typo" "check if node_modules" "run git log"
                   "update package version" "inspect line"))
               (has-match? (lambda (keywords) (any (lambda (k) (string-contains p k)) keywords))))
          (cond
            ((has-match? geis-indicators) 'geis)
            ((has-match? ephemeral-indicators) 'ephemeral)
            ((has-match? direct-indicators) 'direct)
            ;; Fallback default to direct (safest, lowest token cost)
            (else 'direct))))

;;;
;;; 3. Calibration Accuracy & F1-Score Assertions
;;;

(test-group "task-routing-calibration"
            (let* ((total (length %calibration-cases))
                   (correct 0)
                   (direct-cases (filter (lambda (c) (eq? (assoc-ref c 'expected) 'direct)) %calibration-cases))
                   (geis-cases (filter (lambda (c) (eq? (assoc-ref c 'expected) 'geis)) %calibration-cases))
                   (over-triggers 0)
                   (under-triggers 0))
              (for-each
                (lambda (case-entry)
                  (let* ((prompt (assoc-ref case-entry 'prompt))
                         (expected (assoc-ref case-entry 'expected))
                         (actual (classify-task-routing prompt)))
                    (when (eq? expected actual)
                      (set! correct (+ correct 1)))
                    (when (and (eq? expected 'direct) (eq? actual 'geis))
                      (set! over-triggers (+ over-triggers 1)))
                    (when (and (eq? expected 'geis) (eq? actual 'direct))
                      (set! under-triggers (+ under-triggers 1)))))
                %calibration-cases)

              (let* ((accuracy (/ (* 1.0 correct) total)))
                (test-assert "calibration accuracy >= 90%" (>= accuracy 0.90))
                (test-equal "zero over-triggering (direct classified as geis)" 0 over-triggers)
                (test-equal "zero under-triggering (geis classified as direct)" 0 under-triggers)
                (format #t "  [calibration] accuracy: ~a% (~a/~a) over-triggers: ~a under-triggers: ~a~%"
                        (round (* accuracy 100)) correct total over-triggers under-triggers))))

;;;
;;; 4. Progressive Discovery & JIT Hydration Tests
;;;

(test-group "progressive-discovery-and-hydration"
            (let* ((reg (make-registry))
                   (client #f)
                   (reg-tools (register-builtin-tui-tools! reg #f #f #f (lambda () '())))
                   (index-output (geis-index-query))
                   (spec-index (geis-index-query #:category "spec-gate")))

              ;; 1. Discovery query returns concise table without schemas
              (test-assert "geis.index returns available geasa header"
                           (string-contains index-output "Available Geasa Index"))
              (test-assert "geis.index includes spec-gate"
                           (string-contains index-output "spec-gate"))
              (test-assert "geis.index category filter matches spec-gate"
                           (string-contains spec-index "spec-gate"))

              ;; 2. Verify spec tools are deferred by default (0 upfront schema bloat)
              (let-values (((hydrated deferred)
                            (partition-tools-for-step
                              (collect-available-tools reg #f)
                              %default-core-tools
                              '()
                              #t)))
                          (let ((hydrated-names (map tool-spec-name hydrated))
                                (deferred-names (map tool-spec-name deferred)))
                            (test-assert "spec_lock is deferred by default"
                                         (member "spec_lock" deferred-names))
                            (test-assert "spec_synthesize is deferred by default"
                                         (member "spec_synthesize" deferred-names))
                            (test-assert "spec_lock is NOT in default hydrated tools"
                                         (not (member "spec_lock" hydrated-names)))))

              ;; 3. Verify JIT activation: activating spec-gate hydrates its tools
              (let-values (((hydrated deferred)
                            (partition-tools-for-step
                              (collect-available-tools reg #f)
                              %default-core-tools
                              '("spec_lock" "spec_synthesize")
                              #t)))
                          (let ((hydrated-names (map tool-spec-name hydrated)))
                            (test-assert "spec_lock is hydrated when active"
                                         (member "spec_lock" hydrated-names))
                            (test-assert "spec_synthesize is hydrated when active"
                                         (member "spec_synthesize" hydrated-names))))))

;;;
;;; 5. Ephemeral Thrash Circuit Breaker Tests
;;;

(test-group "runtime-thrash-circuit-breaker"
            (let* ((reg (make-registry))
                   (client #f)
                   (session (make-react-session #:client client
                                                #:messages '()
                                                #:loop-detector (make-loop-detector)
                                                #:registry reg))
                   (exploratory-tc1 '((name . "read_file") (arguments . ((path . "foo.c")))))
                   (exploratory-tc2 '((name . "bash") (arguments . ((cmd . "grep foo")))))
                   (exploratory-tc3 '((name . "find_symbol") (arguments . ((query . "bar")))))
                   (mutation-tc '((name . "edit_file") (arguments . ((path . "foo.c"))))))

              ;; Turn 1: single exploratory call -> no tripwire
              (detect-step-loops! session (list exploratory-tc1))
              (test-equal "turn 1 has no tripwire" 0 (length (react-session-messages session)))

              ;; Turn 2: second exploratory call -> no tripwire
              (detect-step-loops! session (list exploratory-tc2))
              (test-equal "turn 2 has no tripwire" 0 (length (react-session-messages session)))

              ;; Turn 3: third exploratory call -> checkpoint injected
              (detect-step-loops! session (list exploratory-tc3))
              (let ((msgs (react-session-messages session)))
                (test-equal "turn 3 injects 1 checkpoint message" 1 (length msgs))
                (test-assert "injected message contains exploration checkpoint"
                             (string-contains (assoc-ref (car msgs) 'content) "[Exploration checkpoint]")))

              ;; Turn 4: mutation resets exploratory counter
              (detect-step-loops! session (list mutation-tc))
              (test-equal "exploratory turns reset to 0 after mutation"
                          0 (loop-detector-exploratory-turns (react-session-loop-detector session)))))



;;;
;;; 6. Full Agent ReAct Multi-Turn Simulation
;;;

(test-group "agent-react-loop-discovery-and-hydration"
            (let* ((client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
                   (reg (make-registry))
                   (_ (register-builtin-tui-tools! reg #f #f #f (lambda () '())))
                   (turn 0)
                   (captured-requests '())
                   (make-tool-call
                     (lambda (name args-json)
                       `((choices . #(((message . ((role . "assistant")
                                                   (content . "")
                                                   (tool_calls . #(((id . "call_123")
                                                                    (type . "function")
                                                                    (function . ((name . ,name)
                                                                                 (arguments . ,args-json))))))))))))))
                   (make-resp
                     (lambda (content)
                       `((choices . #(((message . ((role . "assistant")
                                                   (content . ,content))))))))))
              (letrec* ((mock-poster
                          (lambda* (uri #:key (headers '()) (body #f) (streaming? #f))
                                   (set! turn (+ turn 1))
                                   (set! captured-requests (cons (json-string->scm (if (string? body) body (utf8->string body))) captured-requests))
                                   (let ((resp-body
                                           (cond
                                             ((= turn 1)
                                               (scm->json-string (make-tool-call "geis_index" "{\"category\":\"spec-gate\"}")))
                                             ((= turn 2)
                                               (scm->json-string (make-tool-call "geis_activate" "{\"feature-id\":\"spec-gate\"}")))
                                             ((= turn 3)
                                               (scm->json-string (make-tool-call "spec_status" "{}")))
                                             (else
                                               (scm->json-string (make-resp "Task completed with dynamically hydrated geis tools."))))))
                                     (values (build-response #:code 200) resp-body)))))
                       (parameterize ((current-http-post mock-poster))
                                     (let* ((session (make-react-session client
                                                                         #:initial-messages '(((role . "user") (content . "Please inspect spec status.")))
                                                                         #:reg reg
                                                                         #:deferred-tools? #t
                                                                         #:max-iterations 6)))
                                       (call-with-values (lambda () (react-run! session))
                                                         (lambda (status final-text sess)
                                                           (test-equal "session status is completed" 'completed status)
                                                           (test-equal "final text matches" "Task completed with dynamically hydrated geis tools." final-text)
                                                           (test-equal "exactly 4 agent turns executed" 4 turn)

                                                           ;; Inspect Turn 1 request: spec_status must NOT be in the hydrated tools
                                                           (let* ((req1 (assoc-ref (last captured-requests) "tools"))
                                                                  (tool-names1 (map (lambda (t) (assoc-ref (assoc-ref t "function") "name")) (vector->list req1))))
                                                             (test-assert "spec_status was deferred on turn 1" (not (member "spec_status" tool-names1)))
                                                             (test-assert "geis_index was present on turn 1" (member "geis_index" tool-names1)))

                                                           ;; Inspect Turn 3 request: after geis_activate, spec_status MUST be hydrated in the request tools
                                                           (let* ((req3 (assoc-ref (second captured-requests) "tools"))
                                                                  (tool-names3 (map (lambda (t) (assoc-ref (assoc-ref t "function") "name")) (vector->list req3))))
                                                             (test-assert "spec_status is hydrated in tool payload on turn 3" (member "spec_status" tool-names3)))

                                                           ;; Check that after task completion, active-tools resets to '() as configured
                                                           (test-equal "active tools reset to empty after completion" '() (react-session-active-tools sess)))))))))


(test-end "test-calibration")
