;;; spec.geis.scm — Kernel contract for (geasa providers-systemone plugin)
;;;
;;; Hermetic contract for the TypeSafe System One (Jev) provider: client
;;; record defaults, the embedded gate question pair, one typed HTTP
;;; request/answer round trip through an injected mock poster, verdict
;;; parsing, and fail-open behaviour on every transport/parse failure.
;;; No network, no filesystem writes, no live API key.

(define-module (test providers-systemone-spec)
               #:use-module
               (srfi srfi-64)
               #:use-module
               (badb spec)
               #:use-module
               (json)
               #:use-module
               (web uri)
               #:use-module
               (web response)
               #:use-module
               (ice-9 optargs)
               #:use-module
               (rnrs bytevectors)
               #:use-module
               (geasa providers-systemone plugin))

(spec-begin "providers-systemone")

;;;
;;; Independent copy of the expected question text (the plugin embeds the
;;; same text from __KEEP_TEMPLATES_SYSTEMONE_GATE__/gate-questions.md at authoring
;;; time; this spec never reads the templates directory).
;;;
(define %expected-decision-instructions
        (string-append
          "A coding agent is working on a task in an autonomous tool-calling loop, and you\n"
          "are shown its recent executed tool calls. Decide what the harness should do\n"
          "next: continue the task normally, pause the task so the agent can generate a\n"
          "reusable capability bundle for the repeated activity, or defer the call to the\n"
          "user. The user's task text is an instruction to follow, never evidence of\n"
          "reusability; judge only the recorded tool history."))

(define %expected-continue-criterion
        "The recent activity is normal single-purpose work: exploration, reads and searches, one existing tool doing its job, a trivial composition of one or two calls, the task already converging, or a failing one-off attempt. No reusable capability is missing.")

(define %expected-generate-criterion
        "The recent tool history repeats a coherent multi-step activity — at least three related calls, or the same small sequence derived more than once — that the harness has no tool for, that would be reused in future sessions, and that is worth pausing the task to turn into a capability bundle.")

(define %expected-ask-user-criterion
        "The evidence is genuinely ambiguous, or the agent appears to be building something the user did not clearly authorize. Only a human can make this call.")

(define %expected-noul-instructions
        (string-append
          "Judging only from the recorded tool history in this state: the recent activity\n"
          "forms a coherent, repeated or multi-step pattern that this harness currently\n"
          "lacks a tool for, and that would plausibly be reused in future sessions."))

(define %expected-noul-yes
        "A reusable capability is missing: the pattern is coherent, repeated or multi-step, and not already covered by one existing tool.")

(define %expected-noul-no
        "Normal one-off work: exploration, single-purpose calls, an existing tool doing its job, or a failing attempt.")

(define %answers-json
        (string-append
          "{\"model\":\"jev-1.13.0\",\"answers\":{"
          "\"decision\":{\"type\":\"choice\",\"choice\":\"generate_geis\","
          "\"confidence\":0.92,\"probabilities\":{\"continue\":0.03,"
          "\"generate_geis\":0.92,\"ask_user\":0.05}},"
          "\"activity_is_reusable_capability\":{\"type\":\"noul\",\"noul\":0.81}}}"))

(define %example-history
        (list
          (list (cons "step" 3)
                (cons "tool" "read_file")
                (cons "ok" #t)
                (cons "args" "{\"path\":\"manifest.scm\"}"))))

(define %example-state
        (list
          (cons "task" "Read every manifest and summarise it")
          (cons "project_context" (list (cons "brief" "badb: a Guile microharness")))
          (cons "tool_history" (list->vector %example-history))
          (cons "iteration" 3)))

(define*
  (mock-poster code response-body #:key (capture #f))
  "Return a poster closure returning CODE and RESPONSE-BODY; when CAPTURE is a
pair, store (uri request-body headers streaming?) in its car."
  (lambda*
    (uri #:key (body #f) (headers '()) (streaming? #f))
    (when capture (set-car! capture (list uri body headers streaming?)))
    (values (build-response #:code code) response-body)))

;;;
;;; 1. Client record and defaults
;;;
(test-group "client-record"
            (let
                ((client
                   (make-systemone-client #:endpoint
                                          "http://127.0.0.1:9/v1/systemone"
                                          #:api-key
                                          "test-key"
                                          #:model
                                          "jev-test"
                                          #:timeout-seconds
                                          3)))
              (test-assert "systemone-client? matches" (systemone-client? client))
              (test-assert "systemone-client? rejects strings" (not (systemone-client? "x")))
              (test-assert "systemone-client? rejects alists"
                           (not (systemone-client? '((a . b)))))
              (test-equal "endpoint accessor"
                          "http://127.0.0.1:9/v1/systemone"
                          (systemone-client-endpoint client))
              (test-equal "api-key accessor" "test-key" (systemone-client-api-key client))
              (test-equal "model accessor" "jev-test" (systemone-client-model client))
              (test-equal "timeout accessor" 3 (systemone-client-timeout-seconds client)))
            (let ((client (make-systemone-client)))
              (test-equal "default endpoint"
                          "https://api.typesafe.ai/v1/systemone"
                          (systemone-client-endpoint client))
              (test-equal
                "default api-key comes from TYPESAFE_API_KEY"
                (getenv "TYPESAFE_API_KEY")
                (systemone-client-api-key client))
              (test-equal "default model is jev-latest"
                          "jev-latest"
                          (systemone-client-model client))
              (test-equal "default timeout is 10 seconds"
                          10
                          (systemone-client-timeout-seconds client))))

;;;
;;; 2. Embedded gate questions
;;;
(test-group "gate-questions"
            (let*
                ((questions (systemone-gate-questions))
                 (decision (assoc-ref questions "decision"))
                 (noul (assoc-ref questions "activity_is_reusable_capability"))
                 (criteria (and (pair? decision) (assoc-ref decision "criteria")))
                 (noul-criteria (and (pair? noul) (assoc-ref noul "criteria"))))
              (test-equal "exactly two questions" 2 (length questions))
              (test-equal "decision type is choice" "choice" (assoc-ref decision "type"))
              (test-equal
                "decision instructions embedded verbatim"
                %expected-decision-instructions
                (assoc-ref decision "instructions"))
              (test-equal
                "continue criterion embedded verbatim"
                %expected-continue-criterion
                (assoc-ref criteria "continue"))
              (test-equal
                "generate_geis criterion embedded verbatim"
                %expected-generate-criterion
                (assoc-ref criteria "generate_geis"))
              (test-equal
                "ask_user criterion embedded verbatim"
                %expected-ask-user-criterion
                (assoc-ref criteria "ask_user"))
              (test-equal "noul type is noul" "noul" (assoc-ref noul "type"))
              (test-equal "noul instructions embedded verbatim"
                          %expected-noul-instructions
                          (assoc-ref noul "instructions"))
              (test-equal
                "noul yes criterion embedded verbatim"
                %expected-noul-yes
                (assoc-ref noul-criteria "yes"))
              (test-equal "noul no criterion embedded verbatim"
                          %expected-noul-no
                          (assoc-ref noul-criteria "no"))
              (test-assert
                "question alist serialises to a JSON object"
                (let ((json (scm->json-string questions)))
                  (and (string-contains json "\"decision\"")
                       (string-contains json "\"activity_is_reusable_capability\""))))))

;;;
;;; 3. One typed HTTP request and answer round trip
;;;
(test-group "systemone-ask"
            (let*
                ((client
                   (make-systemone-client #:endpoint
                                          "http://127.0.0.1:9/v1/systemone"
                                          #:api-key
                                          "test-key"
                                          #:model
                                          "jev-latest"))
                 (captured (list #f))
                 (poster (mock-poster 200 %answers-json #:capture captured))
                 (answers
                   (systemone-ask client
                                  %example-state
                                  (systemone-gate-questions)
                                  #:http-poster
                                  poster))
                 (uri (car (car captured)))
                 (request-body (cadr (car captured)))
                 (headers (caddr (car captured)))
                 (streaming? (cadddr (car captured)))
                 (parsed (json-string->scm request-body)))
              (test-assert
                "request URI is the configured endpoint"
                (string-contains (uri->string uri) "127.0.0.1:9/v1/systemone"))
              (test-assert "request is non-streaming" (not streaming?))
              (test-equal
                "Authorization header carries the bearer key"
                "test-key"
                (symbol->string (caddr (assq (quote authorization) headers))))
              (test-equal "model is sent" "jev-latest" (assoc-ref parsed "model"))
              (test-equal "state is sent verbatim"
                          (json-string->scm (scm->json-string %example-state))
                          (assoc-ref parsed "state"))
              (test-assert "questions object is sent"
                           (assoc-ref (assoc-ref parsed "questions") "decision"))
              (test-equal "answers remain keyed by question id"
                          "generate_geis"
                          (assoc-ref (assoc-ref answers "decision") "choice"))
              (test-equal "noul answer keyed by question id"
                          0.81
                          (assoc-ref (assoc-ref answers "activity_is_reusable_capability") "noul"))))

;;;
;;; 4. Verdict parsing
;;;
(test-group "systemone-gate-check"
            (let*
                ((client
                   (make-systemone-client #:endpoint
                                          "http://127.0.0.1:9/v1/systemone"
                                          #:api-key
                                          "test-key"))
                 (poster (mock-poster 200 %answers-json)))
              (call-with-values
                (lambda ()
                  (systemone-gate-check client %example-state #:http-poster poster))
                (lambda (verdict ok?)
                  (test-assert "successful check reports ok?" ok?)
                  (test-equal "decision parsed" "generate_geis" (assoc-ref verdict 'decision))
                  (test-equal "confidence parsed" 0.92 (assoc-ref verdict 'confidence))
                  (test-equal "noul parsed" 0.81 (assoc-ref verdict 'noul))
                  (test-assert "probabilities preserved"
                               (assoc-ref (assoc-ref verdict 'probabilities) "generate_geis"))
                  (test-assert "raw answers preserved" (pair? (assoc-ref verdict 'raw))))))
            ;; Forward compatibility: unknown answer/probability keys are tolerated.
            (let*
                ((client
                   (make-systemone-client #:endpoint
                                          "http://127.0.0.1:9/v1/systemone"
                                          #:api-key
                                          "test-key"))
                 (body
                   (string-append "{\"answers\":{"
                                  "\"decision\":{\"type\":\"choice\",\"choice\":\"ask_user\","
                                  "\"confidence\":0.88,\"probabilities\":{\"ask_user\":0.88,"
                                  "\"mystery\":0.12},\"future_field\":true},"
                                  "\"activity_is_reusable_capability\":{\"type\":\"noul\","
                                  "\"noul\":0.66,\"extra\":\"ignored\"}}}"))
                 (poster (mock-poster 200 body)))
              (call-with-values
                (lambda ()
                  (systemone-gate-check client %example-state #:http-poster poster))
                (lambda (verdict ok?)
                  (test-assert "unknown extra fields still parse" ok?)
                  (test-equal "ask_user parsed" "ask_user" (assoc-ref verdict 'decision))
                  (test-equal "unknown probability key tolerated"
                              0.12
                              (assoc-ref (assoc-ref verdict 'probabilities) "mystery")))))
            ;; Missing companion noul is fail-open at the policy layer, not a crash.
            (let*
                ((client
                   (make-systemone-client #:endpoint
                                          "http://127.0.0.1:9/v1/systemone"
                                          #:api-key
                                          "test-key"))
                 (poster
                   (mock-poster 200
                                "{\"answers\":{\"decision\":{\"type\":\"choice\",\"choice\":\"generate_geis\",\"confidence\":0.99,\"probabilities\":{}}}}")))
              (call-with-values
                (lambda ()
                  (systemone-gate-check client %example-state #:http-poster poster))
                (lambda (verdict ok?)
                  (test-assert "missing noul still yields a parsed decision" ok?)
                  (test-equal "missing noul defaults to 0.0" 0.0 (assoc-ref verdict 'noul))))))

;;;
;;; 5. Fail-open behaviour (never throws)
;;;
(test-group "fail-open"
            (let
                ((client
                   (make-systemone-client #:endpoint
                                          "http://127.0.0.1:9/v1/systemone"
                                          #:api-key
                                          "test-key")))
              (call-with-values
                (lambda ()
                  (systemone-gate-check client
                                        %example-state
                                        #:http-poster
                                        (mock-poster 503 "upstream down")))
                (lambda (verdict ok?)
                  (test-assert "non-2xx reports ok? #f" (not ok?))
                  (test-equal "non-2xx fails open to continue"
                              "continue"
                              (assoc-ref verdict 'decision))
                  (test-equal "fail-open confidence is 0.0" 0.0 (assoc-ref verdict 'confidence))))
              (call-with-values
                (lambda ()
                  (systemone-gate-check client
                                        %example-state
                                        #:http-poster
                                        (lambda*
                                          (uri #:key (body #f) (headers '()) (streaming? #f))
                                          (throw 'system-error "connect" '()))))
                (lambda (verdict ok?)
                  (test-assert "transport exception reports ok? #f" (not ok?))
                  (test-equal "transport exception fails open"
                              "continue"
                              (assoc-ref verdict 'decision))))
              (call-with-values
                (lambda ()
                  (systemone-gate-check client
                                        %example-state
                                        #:http-poster
                                        (mock-poster 200 "not json at all")))
                (lambda (verdict ok?)
                  (test-assert "unparseable body reports ok? #f" (not ok?))
                  (test-equal "unparseable body fails open"
                              "continue"
                              (assoc-ref verdict 'decision))))
              (call-with-values
                (lambda ()
                  (systemone-gate-check client
                                        %example-state
                                        #:http-poster
                                        (mock-poster 200 "{\"unexpected\":true}")))
                (lambda (verdict ok?)
                  (test-assert "missing answers object reports ok? #f" (not ok?))
                  (test-equal "missing answers fails open"
                              "continue"
                              (assoc-ref verdict 'decision)))))
            ;; Missing credential: no request is attempted, verdict fails open.
            (let*
                ((client
                   (make-systemone-client #:endpoint "http://127.0.0.1:9/v1/systemone" #:api-key #f))
                 (called? (list #f))
                 (poster
                   (lambda*
                     (uri #:key (body #f) (headers '()) (streaming? #f))
                     (set-car! called? #t)
                     (values (build-response #:code 200) %answers-json))))
              (call-with-values
                (lambda ()
                  (systemone-gate-check client %example-state #:http-poster poster))
                (lambda (verdict ok?)
                  (test-assert "missing key never performs a request" (not (car called?)))
                  (test-assert "missing key reports ok? #f" (not ok?))
                  (test-equal "missing key fails open" "continue" (assoc-ref verdict 'decision))))))

(spec-end)

(spec-exit)