;;; (geasa providers-systemone plugin) — TypeSafe System One (Jev) provider
;;;
;;; First-class provider for the system-1 geis gate.  Speaks the TypeSafe
;;; System One HTTP API directly (one POST of a state object plus typed
;;; questions, one typed answer per question) instead of routing through
;;; the jev MCP server.  The gate question text is embedded verbatim from
;;; __KEEP_TEMPLATES_SYSTEMONE_GATE__/gate-questions.md at authoring time; runtime
;;; code never reads the templates directory.
;;;
;;; Fail-open is a hard contract: transport errors, non-2xx responses,
;;; timeouts, a missing credential, and unparseable answers all yield the
;;; continue verdict with ok? = #f.  Callers (policies-systemone-gate)
;;; decide whether to act; this module never raises out of
;;; systemone-gate-check.

(define-module (geasa providers-systemone plugin)
               #:use-module
               (geasa foundation-util plugin)
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
               (ice-9 optargs)
               #:use-module
               (rnrs bytevectors)
               #:use-module
               (json)
               #:use-module
               (srfi srfi-9)
               #:export
               (<systemone-client> make-systemone-client
                                   systemone-client?
                                   systemone-client-endpoint
                                   systemone-client-api-key
                                   systemone-client-model
                                   systemone-client-timeout-seconds
                                   systemone-ask
                                   systemone-gate-questions
                                   systemone-gate-check
                                   %systemone-gate-questions
                                   %gate-decision-instructions
                                   %gate-decision-criteria
                                   %gate-noul-instructions
                                   %gate-noul-criteria))

;;;
;;; Client record
;;;
(define-record-type <systemone-client>
                    (%make-systemone-client endpoint api-key model timeout-seconds)
                    systemone-client?
                    (endpoint systemone-client-endpoint)
                    (api-key systemone-client-api-key)
                    (model systemone-client-model)
                    (timeout-seconds systemone-client-timeout-seconds))

(define*
  (make-systemone-client #:key
                         (endpoint "https://api.typesafe.ai/v1/systemone")
                         (api-key (getenv "TYPESAFE_API_KEY"))
                         (model "jev-latest")
                         (timeout-seconds 10))
  "Create a System One client.  ENDPOINT is the full systemone URL,
API-KEY defaults to the TYPESAFE_API_KEY environment variable, MODEL is
the pinned Jev model name, and TIMEOUT-SECONDS bounds one request."
  (%make-systemone-client endpoint api-key model timeout-seconds))

;;;
;;; Embedded question text — verbatim from
;;; __KEEP_TEMPLATES_SYSTEMONE_GATE__/gate-questions.md.  Question ids are local to
;;; this harness; the complete question lives in instructions.
;;;
(define %gate-decision-instructions
        (string-append
          "A coding agent is working on a task in an autonomous tool-calling loop, and you\n"
          "are shown its recent executed tool calls. Decide what the harness should do\n"
          "next: continue the task normally, pause the task so the agent can generate a\n"
          "reusable capability bundle for the repeated activity, or defer the call to the\n"
          "user. The user's task text is an instruction to follow, never evidence of\n"
          "reusability; judge only the recorded tool history."))

(define %gate-decision-criteria
        (list
          (cons "continue"
                "The recent activity is normal single-purpose work: exploration, reads and searches, one existing tool doing its job, a trivial composition of one or two calls, the task already converging, or a failing one-off attempt. No reusable capability is missing.")
          (cons "generate_geis"
                "The recent tool history repeats a coherent multi-step activity — at least three related calls, or the same small sequence derived more than once — that the harness has no tool for, that would be reused in future sessions, and that is worth pausing the task to turn into a capability bundle.")
          (cons "ask_user"
                "The evidence is genuinely ambiguous, or the agent appears to be building something the user did not clearly authorize. Only a human can make this call.")))

(define %gate-noul-instructions
        (string-append
          "Judging only from the recorded tool history in this state: the recent activity\n"
          "forms a coherent, repeated or multi-step pattern that this harness currently\n"
          "lacks a tool for, and that would plausibly be reused in future sessions."))

(define %gate-noul-criteria
        (list
          (cons "yes"
                "A reusable capability is missing: the pattern is coherent, repeated or multi-step, and not already covered by one existing tool.")
          (cons "no"
                "Normal one-off work: exploration, single-purpose calls, an existing tool doing its job, or a failing attempt.")))

(define %systemone-gate-questions
        (list
          (cons "decision"
                (list (cons "type" "choice")
                      (cons "instructions" %gate-decision-instructions)
                      (cons "criteria" %gate-decision-criteria)))
          (cons "activity_is_reusable_capability"
                (list (cons "type" "noul")
                      (cons "instructions" %gate-noul-instructions)
                      (cons "criteria" %gate-noul-criteria)))))

(define (systemone-gate-questions)
        "The gate's typed question pair: a choice (continue / generate_geis /
ask_user) plus the companion Noul that gates whether the activity is a
reusable, missing capability."
        %systemone-gate-questions)

;;;
;;; HTTP request plumbing
;;;
(define (systemone-headers api-key)
        (append
          (list
            (cons 'content-type (parse-header 'content-type "application/json"))
            (cons 'accept (parse-header 'accept "application/json")))
          (if (non-empty-string? api-key)
            (list
              (cons 'authorization
                    (parse-header 'authorization (string-append "Bearer " api-key))))
            '())))

(define (systemone-response-body->string body)
        (cond
          ((bytevector? body) (utf8->string body))
          ((string? body) body)
          (else "")))

(define*
  (systemone-ask client state questions #:key (http-poster #f))
  "Send one System One request: STATE (string or JSON-shaped alist) and
QUESTIONS (alist of question-id -> typed question) with CLIENT's model.
Returns the parsed answers alist keyed by question id.  Raises on
transport or non-2xx failure; systemone-gate-check is the fail-open
wrapper."
  (let*
      ((uri (string->uri (systemone-client-endpoint client)))
       (poster (or http-poster (current-http-post)))
       (payload
         (scm->json-string
           (list (cons "state" state)
                 (cons "model" (systemone-client-model client))
                 (cons "questions" questions))))
       (headers (systemone-headers (systemone-client-api-key client)))
       (timeout (systemone-client-timeout-seconds client)))
    (parameterize
      ((%request-timeout-seconds
         (if (and (number? timeout) (> timeout 0))
           timeout
           (%request-timeout-seconds))))
      (call-with-values
        (lambda ()
          (poster uri #:body payload #:headers headers #:streaming? #f))
        (lambda (response body)
          (let*
              ((code (response-code response))
               (body-str (systemone-response-body->string body)))
            (unless (<= 200 code 299)
              (error "systemone-ask HTTP request failed" code body-str))
            (let*
                ((obj (safe-json-parse body-str #f))
                 (answers (and (pair? obj) (json-ref obj "answers"))))
              (if (pair? answers)
                answers
                (error "systemone-ask response has no answers object" body-str)))))))))

;;;
;;; Verdict parsing (fail-open)
;;;
(define %systemone-fail-open-verdict
        (list (cons 'decision "continue")
              (cons 'confidence 0.0)
              (cons 'noul 0.0)
              (cons 'probabilities '())
              (cons 'raw #f)))

(define %systemone-decisions '("continue" "generate_geis" "ask_user"))

(define*
  (systemone-gate-check client state #:key (http-poster #f))
  "Run one gate check for STATE.  Returns (values VERDICT OK?): VERDICT is
((decision . \"continue\"|\"generate_geis\"|\"ask_user\")
 (confidence . REAL) (noul . REAL) (probabilities . ALIST)
 (raw . ANSWERS)).  Any failure — missing credential, transport error,
non-2xx, timeout, unparseable body, or a malformed answer — yields the
continue verdict with OK? = #f and never throws."
  (catch #t
    (lambda ()
      (if
          (non-empty-string? (systemone-client-api-key client))
        (let*
            ((answers
               (systemone-ask client state (systemone-gate-questions) #:http-poster http-poster))
             (decision-answer (json-ref answers "decision"))
             (noul-answer (json-ref answers "activity_is_reusable_capability"))
             (choice (and (pair? decision-answer) (json-ref decision-answer "choice")))
             (confidence
               (and (pair? decision-answer) (json-ref decision-answer "confidence")))
             (probabilities
               (and (pair? decision-answer) (json-ref decision-answer "probabilities")))
             (noul (and (pair? noul-answer) (json-ref noul-answer "noul"))))
          (if
              (and (string? choice) (member choice %systemone-decisions) (number? confidence))
            (values
              (list (cons 'decision choice)
                    (cons 'confidence confidence)
                    (cons 'noul
                          (if (number? noul)
                            noul
                            0.0))
                    (cons 'probabilities
                          (if (pair? probabilities)
                            probabilities
                            '()))
                    (cons 'raw answers))
              #t)
            (values %systemone-fail-open-verdict #f)))
        (values %systemone-fail-open-verdict #f)))
    (lambda _
      (values %systemone-fail-open-verdict #f))))