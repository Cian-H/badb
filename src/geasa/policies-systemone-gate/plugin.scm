;;; (geasa policies-systemone-gate plugin) — optional System-1 Geis Gate
;;;
;;; The gate consults a cheap "system 1" judgment model after tool calls
;;; have executed, asking whether the recent activity is a reusable
;;; capability the harness lacks.  It acts only when the typed choice
;;; answer is confident AND the companion Noul agrees; every other path
;;; (low confidence, failed Noul, transport error, timeout, unparseable
;;; answer, disabled config) fails open to "continue".  The gate itself
;;; never constructs the provider: the TUI injects a #:check closure of
;;; the shape (lambda (state) (values verdict ok?)), which keeps this
;;; bundle hermetic and dependency-directional.
;;;
;;; Two trigger points share one consult pipeline:
;;;
;;;   'post-turn               once per ReAct step that executed tools,
;;;                            throttled by every-nth-step;
;;;   'exploration-checkpoint  the runtimes-react tripwire (3 consecutive
;;;                            exploratory turns), bypassing the throttle.
;;;
;;; Both respect a cooldown after a stop and skip evaluation while the
;;; agent is inside the geis synthesis workflow (spec.lock /
;;; spec.synthesize).  Stop/escalate behaviour is transcript surgery: the
;;; embedded template becomes a user message plus one journal row; the
;;; main model then follows the contract-gated synthesis workflow.

(define-module
  (geasa policies-systemone-gate plugin) #:use-module
  (geasa foundation-util plugin)
  #:use-module
  (geasa foundation-config plugin)
  #:use-module
  (geasa runtimes-react plugin)
  #:use-module
  (ice-9 format)
  #:use-module
  (ice-9 optargs)
  #:use-module
  (json)
  #:use-module
  (srfi srfi-1)
  #:use-module
  (srfi srfi-9)
  #:use-module
  (badb kernel journal)
  #:export
  (<systemone-gate-options> make-systemone-gate-options
                            systemone-gate-options?
                            systemone-gate-options-enabled?
                            systemone-gate-options-provider
                            systemone-gate-options-endpoint
                            systemone-gate-options-api-key
                            systemone-gate-options-model
                            systemone-gate-options-auto-accept
                            systemone-gate-options-noul-floor
                            systemone-gate-options-every-nth-step
                            systemone-gate-options-max-history
                            systemone-gate-options-cooldown-steps
                            systemone-gate-options-max-context-chars
                            systemone-gate-config->options
                            <systemone-gate>
                            make-systemone-gate
                            systemone-gate?
                            systemone-gate-options
                            systemone-gate-decider
                            systemone-gate-journal
                            systemone-gate-project-brief
                            systemone-gate-model
                            systemone-gate-active-geasa
                            systemone-gate-steps-with-tools
                            systemone-gate-cooldown-left
                            systemone-gate-last-check-iteration
                            make-systemone-gate-hooks
                            systemone-gate-history
                            systemone-gate-state
                            systemone-gate-verdict->action
                            systemone-gate-evidence-digest
                            systemone-gate-post-turn!
                            systemone-gate-checkpoint!
                            %systemone-gate-stop-message
                            %systemone-gate-escalate-message
                            %systemone-gate-continue-nudge
                            %systemone-gate-cooldown-nudge
                            %synthesis-tool-names))

;;;
;;; Options record and config parsing
;;;
(define-record-type <systemone-gate-options>
                    (%make-systemone-gate-options enabled
                                                  provider
                                                  endpoint
                                                  api-key
                                                  model
                                                  auto-accept
                                                  noul-floor
                                                  every-nth-step
                                                  max-history
                                                  cooldown-steps
                                                  max-context-chars)
                    systemone-gate-options?
                    (enabled systemone-gate-options-enabled?)
                    (provider systemone-gate-options-provider)
                    (endpoint systemone-gate-options-endpoint)
                    (api-key systemone-gate-options-api-key)
                    (model systemone-gate-options-model)
                    (auto-accept systemone-gate-options-auto-accept)
                    (noul-floor systemone-gate-options-noul-floor)
                    (every-nth-step systemone-gate-options-every-nth-step)
                    (max-history systemone-gate-options-max-history)
                    (cooldown-steps systemone-gate-options-cooldown-steps)
                    (max-context-chars systemone-gate-options-max-context-chars))

(define* (make-systemone-gate-options #:key
                                      (enabled #f)
                                      (provider #f)
                                      (endpoint #f)
                                      (api-key #f)
                                      (model "jev-latest")
                                      (auto-accept 0.8)
                                      (noul-floor 0.5)
                                      (every-nth-step 1)
                                      (max-history 24)
                                      (cooldown-steps 5)
                                      (max-context-chars 4000))
         "Construct normalized gate options.  ENABLED is false by default so an
absent config block changes nothing; PROVIDER names an (options (providers
...)) preset; ENDPOINT/API-KEY override the preset; MODEL defaults to the
pinned Jev model."
         (%make-systemone-gate-options enabled
                                       provider
                                       endpoint
                                       api-key
                                       model
                                       auto-accept
                                       noul-floor
                                       every-nth-step
                                       max-history
                                       cooldown-steps
                                       max-context-chars))

(define (options-raw-ref lst key)
        "Raw alist lookup for KEY in LST, returning the cdr without
GET-PROP's single-entry unwrapping (needed for the providers catalog)."
        (let* ((sym (ensure-symbol key))
               (str (symbol->string sym))
               (kw (symbol->keyword sym)))
          (let loop ((rest lst))
                    (cond
                      ((null? rest) #f)
                      ((not (pair? rest)) #f)
                      ((and (pair? (car rest))
                            (let ((k (caar rest)))
                              (or (eq? k sym)
                                  (eq? k kw)
                                  (and (string? k) (string=? k str)))))
                        (cdar rest))
                      (else (loop (cdr rest)))))))

(define (find-raw-provider options name)
        "Resolve provider preset NAME from the raw options alist OPTIONS."
        (let ((provs (options-raw-ref options 'providers)))
          (and (pair? provs)
               (let loop ((rest provs))
                         (cond
                           ((null? rest) #f)
                           ((not (pair? (car rest))) (loop (cdr rest)))
                           ((let ((k (caar rest)))
                              (or (eq? (ensure-symbol k) (ensure-symbol name))
                                  (string=? (ensure-string k) (ensure-string name))))
                             (cdar rest))
                           (else (loop (cdr rest))))))))

(define (preset-model preset)
        (let ((models (and preset (get-prop preset 'models #f))))
          (cond
            ((string? models) models)
            ((and (pair? models) (string? (car models))) (car models))
            (else #f))))

(define (option-number block key default)
        (let ((value (get-prop block key default)))
          (if (number? value) value default)))

(define (option-integer block key default)
        (let ((value (get-prop block key default)))
          (if (and (integer? value) (>= value 0)) value default)))

(define (truthy? value)
        (and value (not (eq? value #f))))

(define (default-gate-warn message)
        (format (current-error-port) "warning: systemone-gate: ~a~%" message))

(define*
  (systemone-gate-config->options source #:key (warn default-gate-warn))
  "Parse SOURCE into <systemone-gate-options>.  SOURCE is either a
<badb-config> record or the raw options alist (the value of
(badb-config-options cfg)); the gate block is (systemone-gate ...) under
options.  Unknown keys are ignored and a mismatched provider kind only
warns (fail-open)."
  (let*
      ((config (and (badb-config? source) source))
       (options
         (if config
           (badb-config-options config)
           source))
       (block
         (or (options-raw-ref options 'systemone-gate)
             (options-raw-ref options 'systemone_gate)
             '())))
    (if (pair? block)
      (let*
          ((enabled (truthy? (get-prop block 'enabled #f)))
           (provider (get-prop block 'provider #f))
           (preset
             (and provider
                  (if config
                    (badb-config-get-provider config provider)
                    (find-raw-provider options provider))))
           (kind (and preset (get-prop preset 'kind #f)))
           (direct-endpoint (get-prop block 'endpoint #f))
           (direct-key (get-prop block 'api-key #f))
           (direct-model (get-prop block 'model #f))
           (preset-endpoint (and preset (get-prop preset 'endpoint #f)))
           (preset-key (and preset (get-prop preset 'api-key #f)))
           (preset-model-name (and preset (preset-model preset))))
        (when
            (and kind (not (string-ci=? (ensure-string kind) "systemone")))
          (warn
            (format #f
                    "provider preset '~a' carries kind '~a'; expected 'systemone' (continuing anyway)"
                    provider
                    kind)))
        (make-systemone-gate-options #:enabled
                                     enabled
                                     #:provider
                                     provider
                                     #:endpoint
                                     (or direct-endpoint preset-endpoint)
                                     #:api-key
                                     (or direct-key preset-key)
                                     #:model
                                     (or direct-model preset-model-name "jev-latest")
                                     #:auto-accept
                                     (option-number block 'auto-accept 0.8)
                                     #:noul-floor
                                     (option-number block 'noul-floor 0.5)
                                     #:every-nth-step
                                     (max 1 (option-integer block 'every-nth-step 1))
                                     #:max-history
                                     (max 1 (option-integer block 'max-history 24))
                                     #:cooldown-steps
                                     (option-integer block 'cooldown-steps 5)
                                     #:max-context-chars
                                     (max 200 (option-integer block 'max-context-chars 4000))))
      (make-systemone-gate-options))))

;;;
;;; Embedded message text — verbatim from
;;; __KEEP_TEMPLATES_SYSTEMONE_GATE__/{stop-geis-message,escalate-message}.md at
;;; authoring time.  Runtime code never reads the templates directory.
;;;
(define %systemone-gate-stop-message
        (string-append
          "[badb systemone-gate] Checkpoint verdict: generate_geis (confidence ~a, reviewed by the system-1 model against your recent tool activity).\n"
          "\n"
          "Your recent tool activity: ~a\n"
          "\n"
          "You should pause the current line of work and turn this activity into a\n"
          "reusable geis bundle, then resume. Follow the contract-gated dynamic synthesis\n"
          "workflow:\n"
          "\n"
          "1. Finish or cleanly abandon the in-flight tool sequence; do not lose state.\n"
          "2. Pick a feature-id for the capability yourself, following the naming rules\n"
          "   in AGENTS.md: <area>-<name> with an area prefix (capabilities, runtimes,\n"
          "   providers, surfaces, policies, persona, foundation, agents), e.g.\n"
          "   capabilities-manifest-scan — and a short description of what the bundle\n"
          "   would do.\n"
          "3. Design and write a declarative contract specification (SRFI-64) for the\n"
          "   capability — requirements, exported procedures, invariants, test cases —\n"
          "   and save it as src/geasa/<feature-id>/spec.geis.scm.\n"
          "4. Lock the specification with the 'spec.lock' tool (feature-id + spec-path).\n"
          "   Locking makes the contract binding; the kernel re-reads the locked copy at\n"
          "   commit time.\n"
          "5. Synthesize the implementation with 'spec.synthesize' for your feature-id\n"
          "   (it iterates with counterexample feedback until all contracts pass, then\n"
          "   commits and hot-attaches the module).\n"
          "6. Verify the new feature appears in the kernel registry ('spec.status') and\n"
          "   confirm your original task resumes and finishes with the new tool.\n"
          "\n"
          "If stopping is wrong or premature — the activity was one-off, the harness\n"
          "already covers it, or you are close to finishing the task — say so briefly in\n"
          "your next message and continue the task instead. Do not create an empty or\n"
          "trivial bundle just to satisfy this checkpoint."))

(define %systemone-gate-escalate-message
        (string-append
          "[badb systemone-gate] Checkpoint verdict: ask_user (confidence ~a).\n"
          "\n"
          "The system-1 gate reviewed your recent tool activity (~a) and could not\n"
          "confidently classify it: either the evidence was ambiguous, or the activity\n"
          "appears to go beyond what the user clearly authorized.\n"
          "\n"
          "Continue the current task only if you can state, in one sentence, that it is\n"
          "exactly what the user asked for; otherwise stop, summarize what you were about\n"
          "to do, and ask the user before proceeding further."))

(define %systemone-gate-continue-nudge
        (string-append
          "[badb systemone-gate] Exploration checkpoint: the system-1 model reviewed your recent tool activity and judged that no reusable capability is missing (decision ~a, confidence ~a).\n"
          "\n"
          "Briefly summarize what you've learned and name the next question or action. If more exploration is needed, continue with a specific question to resolve; don't make an edit or create a geis bundle just to satisfy this reminder."))

(define %systemone-gate-cooldown-nudge
        "[badb systemone-gate] Exploration checkpoint: the gate is in cooldown after a recent stop or during the geis synthesis workflow, so it skipped this checkpoint. Briefly summarize what you've learned and name the next question or action; continue with a specific question to resolve.")

(define %synthesis-tool-names
        '("spec.synthesize" "spec_synthesize" "spec.lock" "spec_lock"))

(define %default-project-brief
        "badb: a Guile Scheme agentic microharness (microkernel + contract-verified geasa bundles). Users add features as bundles with executable specs; the agent can pause to synthesize a new bundle when a reusable capability is missing.")

;;;
;;; History assembly
;;;
(define (take-last-n lst n)
        (if (or (not (integer? n)) (<= n 0))
          '()
          (let ((len (length lst)))
            (if (<= len n) lst (list-tail lst (- len n))))))

(define (find-tool-result id following)
        "Find the tool result message for call ID among FOLLOWING messages,
stopping at the next assistant turn.  Returns #f for an orphaned call."
        (and id
             (let loop ((rest following))
                       (cond
                         ((null? rest) #f)
                         ((not (pair? (car rest))) (loop (cdr rest)))
                         ((equal? (assoc-ref (car rest) 'role) "assistant") #f)
                         ((and (equal? (assoc-ref (car rest) 'role) "tool")
                               (equal? (assoc-ref (car rest) 'tool_call_id) id))
                           (car rest))
                         (else (loop (cdr rest)))))))

(define (tool-result-ok? content)
        (cond
          ((not (string? content)) #f)
          ((string-contains content "DuplicateToolCallException") #f)
          ((string-contains content "\"status\":\"ERROR\"") #f)
          (else
            (let ((parsed (safe-json-parse content #f)))
              (not (and (pair? parsed)
                        (or (assoc-ref parsed 'error)
                            (assoc-ref parsed "error")
                            (equal? (assoc-ref parsed 'status) "ERROR")
                            (equal? (assoc-ref parsed "status") "ERROR"))))))))

(define (tool-call-list tc)
        (cond
          ((vector? tc) (vector->list tc))
          ((list? tc) tc)
          (else '())))

(define (history-entry tc step following)
        "Build one history entry for tool call TC executed in STEP, pairing
it with its result in FOLLOWING; return #f for an orphaned call."
        (let* ((id (assoc-ref tc 'id))
               (function (assoc-ref tc 'function))
               (name (and (pair? function) (assoc-ref function 'name)))
               (args (and (pair? function) (assoc-ref function 'arguments)))
               (result (find-tool-result id following)))
          (and id
               (string? name)
               (not (string-null? name))
               result
               (let ((content (assoc-ref result 'content)))
                 `(("step" . ,step)
                   ("tool" . ,name)
                   ("args" . ,(truncate-string (safe-json-string (or args "")) 200))
                   ("ok" . ,(tool-result-ok? content))
                   ("result_summary" .
                    ,(if (string? content) (truncate-string content 200) "")))))))

(define* (systemone-gate-history msgs #:key (max-history 24))
         "Return the most recent MAX-HISTORY executed tool calls from MSGS, oldest
first.  Each entry is a string-keyed alist (step tool args ok
result_summary); STEP is the 1-based ordinal of the tool-calling assistant
turn (an approximation of the ReAct iteration).  Orphaned tool calls and
orphaned tool messages are skipped."
         (let loop ((rest msgs) (step 0) (acc '()))
                   (cond
                     ((null? rest) (take-last-n acc max-history))
                     ((and (pair? (car rest))
                           (equal? (assoc-ref (car rest) 'role) "assistant")
                           (assoc-ref (car rest) 'tool_calls))
                       (let* ((tcs (tool-call-list (assoc-ref (car rest) 'tool_calls)))
                              (next-step (+ step 1))
                              (entries
                                (filter-map
                                  (lambda (tc) (history-entry tc next-step (cdr rest)))
                                  tcs)))
                         (loop (cdr rest) next-step (append acc entries))))
                     (else (loop (cdr rest) step acc)))))

(define (current-step-entries history)
        (if (null? history)
          '()
          (let ((max-step
                  (apply max
                         (map (lambda (entry) (assoc-ref entry "step")) history))))
            (filter (lambda (entry) (equal? (assoc-ref entry "step") max-step))
                    history))))

(define (any-synthesis-tool? entries)
        (any (lambda (entry)
               (member (assoc-ref entry "tool") %synthesis-tool-names string=))
             entries))

(define (systemone-gate-evidence-digest history)
        "Return a short digest of the 3 most frequent tool names in HISTORY
with counts, e.g. \"read_file x2, bash x1\"."
        (let* ((counts
                 (let loop ((rest history) (acc '()))
                           (if (null? rest) acc
                                            (let* ((name (assoc-ref (car rest) "tool"))
                                                   (pair (assoc name acc)))
                                              (if pair
                                                (begin (set-cdr! pair (+ 1 (cdr pair)))
                                                       (loop (cdr rest) acc))
                                                (loop (cdr rest) (cons (cons name 1) acc)))))))
               (sorted
                 (sort counts
                       (lambda (a b)
                         (if (= (cdr a) (cdr b))
                           (string<? (car a) (car b))
                           (> (cdr a) (cdr b)))))))
          (if (null? sorted)
            "no recorded tool activity"
            (format #f
                    "~{~a~^, ~}"
                    (map (lambda (pair) (format #f "~a x~a" (car pair) (cdr pair)))
                         (take sorted (min 3 (length sorted))))))))

;;;
;;; State assembly
;;;
(define %default-task "(none)")

(define (first-user-task msgs)
        (let*
            ((msg
               (find
                 (lambda (m)
                   (and (pair? m) (equal? (assoc-ref m 'role) "user")))
                 msgs))
             (content (and msg (assoc-ref msg 'content))))
          (if
              (and (string? content) (not (string-null? (string-trim-both content))))
            (truncate-string content 300)
            %default-task)))

(define (bounded-geasa active-geasa)
        (map ensure-string
             (take (ensure-list active-geasa)
                   (min 8 (length (ensure-list active-geasa))))))

(define* (systemone-gate-state session #:key
                               (max-history 24)
                               (max-context-chars 4000)
                               (brief %default-project-brief)
                               (model #f)
                               (active-geasa '()))
         "Build the request state alist for SESSION per
__KEEP_TEMPLATES_SYSTEMONE_GATE__/gate-request.schema.json.  Trims the oldest history
entries until the serialized state fits MAX-CONTEXT-CHARS."
         (let* ((msgs (react-session-messages session))
                (task (first-user-task msgs))
                (history (systemone-gate-history msgs #:max-history max-history))
                (iteration (max 1 (or (react-session-iteration session) 1)))
                (geasa (bounded-geasa active-geasa))
                (context
                  `(("brief" . ,(truncate-string (if (string? brief) brief "") 400))
                    ,@(if (and model (not (string-null? (ensure-string model))))
                        `(("model" . ,(ensure-string model)))
                        '())
                    ,@(if (pair? geasa)
                        `(("active_geasa" . ,(list->vector geasa)))
                        '())))
                (build
                  (lambda (entries)
                    `(("task" . ,task)
                      ("project_context" . ,context)
                      ("tool_history" . ,(list->vector entries))
                      ("iteration" . ,iteration)))))
           (let loop ((entries history))
                     (let ((state (build entries)))
                       (if (or (<= (string-length (scm->json-string state))
                                   max-context-chars)
                               (null? entries))
                         state
                         (loop (cdr entries)))))))

;;;
;;; Verdict policy
;;;
(define %continue-verdict
        '((decision . "continue")
          (confidence . 0.0)
          (noul . 0.0)
          (probabilities . ())
          (raw . #f)))

(define*
  (systemone-gate-verdict->action verdict
                                  ok?
                                  #:key
                                  (auto-accept 0.8)
                                  (noul-floor 0.5))
  "Map VERDICT to 'stop (generate_geis), 'escalate (ask_user), or
'continue.  Acts only when OK? is true, the choice is generate_geis or
ask_user, confidence is at least AUTO-ACCEPT, and the companion Noul is at
least NOUL-FLOOR; everything else fails open."
  (if ok?
    (let*
        ((decision (and (pair? verdict) (prop-ref verdict 'decision #f)))
         (decision-str (and decision (ensure-string decision)))
         (confidence (and (pair? verdict) (prop-ref verdict 'confidence 0.0)))
         (noul (and (pair? verdict) (prop-ref verdict 'noul 0.0))))
      (if
          (and decision-str
               (number? confidence)
               (number? noul)
               (>= confidence auto-accept)
               (>= noul noul-floor))
        (cond
          ((string=? decision-str "generate_geis") 'stop)
          ((string=? decision-str "ask_user") 'escalate)
          (else 'continue))
        'continue))
    'continue))

;;;
;;; Gate record
;;;
(define-record-type <systemone-gate>
                    (%make-systemone-gate options
                                          check
                                          journal
                                          on-verdict
                                          project-brief
                                          model
                                          active-geasa
                                          steps-with-tools
                                          cooldown-left
                                          last-history-count
                                          last-check-iteration)
                    systemone-gate?
                    (options systemone-gate-options)
                    (check systemone-gate-decider)
                    (journal systemone-gate-journal)
                    (on-verdict systemone-gate-on-verdict)
                    (project-brief systemone-gate-project-brief)
                    (model systemone-gate-model)
                    (active-geasa systemone-gate-active-geasa)
                    (steps-with-tools systemone-gate-steps-with-tools
                                      set-systemone-gate-steps-with-tools!)
                    (cooldown-left systemone-gate-cooldown-left set-systemone-gate-cooldown-left!)
                    (last-history-count
                      systemone-gate-last-history-count
                      set-systemone-gate-last-history-count!)
                    (last-check-iteration
                      systemone-gate-last-check-iteration
                      set-systemone-gate-last-check-iteration!))

(define* (make-systemone-gate #:key
                              (options (make-systemone-gate-options))
                              (check #f)
                              (journal #f)
                              (on-verdict (lambda (action verdict) #f))
                              (project-brief %default-project-brief)
                              (model #f)
                              (active-geasa '()))
         "Construct a gate.  OPTIONS is a <systemone-gate-options>; CHECK is the
injected decider (lambda (state) (values verdict ok?)); JOURNAL receives
one row per evaluated/skipped step; ON-VERDICT is an optional status-line
callback (lambda (action verdict) ...)."
         (%make-systemone-gate options
                               check
                               journal
                               on-verdict
                               project-brief
                               model
                               active-geasa
                               0
                               0
                               0
                               -1))

(define (session-iteration session)
        (or (react-session-iteration session) 1))

(define (gate-disabled? gate)
        (or (not (systemone-gate? gate))
            (not (systemone-gate-options-enabled? (systemone-gate-options gate)))))

(define (gate-consult gate state)
        (let ((check (systemone-gate-decider gate)))
          (if (procedure? check)
            (catch #t
              (lambda ()
                (call-with-values
                  (lambda ()
                    (check state))
                  (lambda (verdict ok?)
                    (values
                      (if (pair? verdict)
                        verdict
                        %continue-verdict)
                      (and ok? #t)))))
              (lambda _
                (values %continue-verdict #f)))
            (values %continue-verdict #f))))

(define (verdict-ref verdict key default)
        (or (and (pair? verdict) (prop-ref verdict key #f)) default))

(define (verdict-number verdict key)
        (let ((value (verdict-ref verdict key 0.0)))
          (if (number? value) value 0.0)))

(define (verdict-decision verdict)
        (ensure-string (verdict-ref verdict 'decision "continue")))

(define (gate-inject! session content)
        (set-react-session-messages!
          session
          (append (react-session-messages session)
                  (list `((role . "user") (content . ,content))))))

(define (action->status action)
        (case action
          ((stop) "stop")
          ((escalate) "escalate")
          (else "continue")))

(define (gate-notify! gate action verdict)
        (let ((fn (systemone-gate-on-verdict gate)))
          (when (procedure? fn)
            (catch #t
              (lambda () (fn action verdict))
              (lambda _ #f)))))

(define (gate-summary history iteration)
        (let
            ((tools
               (delete-duplicates
                 (map
                   (lambda (entry)
                     (assoc-ref entry "tool"))
                   history)
                 string=)))
          `((iteration . ,iteration)
            (tool-count . ,(length history))
            (tools .
                   ,(if (null? tools)
                      ""
                      (format #f "~{~a~^, ~}" tools)))
            (evidence . ,(systemone-gate-evidence-digest history)))))

(define
  (gate-journal! gate checkpoint status summary verdict)
  (safe-journal-append! (systemone-gate-journal gate)
                        `((feature-id . "systemone-gate")
                          (checkpoint . ,(symbol->string checkpoint))
                          (status . ,status)
                          (arguments . ,summary)
                          (result . ,(or verdict %continue-verdict)))))

(define* (gate-evaluate! gate session checkpoint)
         "Consult the decider for SESSION and apply the verdict: inject the
embedded template, journal one row, and notify the status callback."
         (let* ((opts (systemone-gate-options gate))
                (history
                  (systemone-gate-history (react-session-messages session)
                                          #:max-history (systemone-gate-options-max-history opts)))
                (state
                  (systemone-gate-state session
                                        #:max-history (systemone-gate-options-max-history opts)
                                        #:max-context-chars
                                        (systemone-gate-options-max-context-chars opts)
                                        #:brief (systemone-gate-project-brief gate)
                                        #:model (systemone-gate-model gate)
                                        #:active-geasa (systemone-gate-active-geasa gate)))
                (iteration (session-iteration session))
                (digest (systemone-gate-evidence-digest history)))
           (receive (verdict ok?) (gate-consult gate state)
                    (let* ((action
                             (systemone-gate-verdict->action
                               verdict
                               ok?
                               #:auto-accept (systemone-gate-options-auto-accept opts)
                               #:noul-floor (systemone-gate-options-noul-floor opts)))
                           (confidence (verdict-number verdict 'confidence)))
                      (set-systemone-gate-last-check-iteration! gate iteration)
                      (case action
                        ((stop)
                          (gate-inject!
                            session
                            (format #f %systemone-gate-stop-message confidence digest))
                          (set-systemone-gate-cooldown-left!
                            gate
                            (systemone-gate-options-cooldown-steps opts)))
                        ((escalate)
                          (gate-inject!
                            session
                            (format #f %systemone-gate-escalate-message confidence digest)))
                        (else
                          (when (eq? checkpoint 'exploration-checkpoint)
                            (gate-inject!
                              session
                              (format #f
                                      %systemone-gate-continue-nudge
                                      (verdict-decision verdict)
                                      confidence)))))
                      (gate-journal! gate
                                     checkpoint
                                     (action->status action)
                                     (gate-summary history iteration)
                                     verdict)
                      (gate-notify! gate action verdict)
                      action))))

(define (gate-skip! gate session checkpoint status)
        (let* ((opts (systemone-gate-options gate))
               (history
                 (systemone-gate-history
                   (react-session-messages session)
                   #:max-history (systemone-gate-options-max-history opts))))
          (gate-journal! gate
                         checkpoint
                         status
                         (gate-summary history (session-iteration session))
                         #f)))

;;;
;;; Hooks
;;;
(define (systemone-gate-post-turn! gate session)
        "Post-turn hook: evaluate at most once per tool-executing step.
Returns #f always; all failures are swallowed (fail-open)."
        (catch #t
          (lambda ()
            (unless (gate-disabled? gate)
              (let* ((opts (systemone-gate-options gate))
                     (history
                       (systemone-gate-history
                         (react-session-messages session)
                         #:max-history (systemone-gate-options-max-history opts)))
                     (length* (length history))
                     (iteration (session-iteration session))
                     (new-tools? (> length* (systemone-gate-last-history-count gate)))
                     (step-entries (current-step-entries history))
                     (nth (max 1 (systemone-gate-options-every-nth-step opts))))
                (when new-tools?
                  (set-systemone-gate-last-history-count! gate length*)
                  (set-systemone-gate-steps-with-tools!
                    gate
                    (+ 1 (systemone-gate-steps-with-tools gate)))
                  (cond
                    ((= iteration (systemone-gate-last-check-iteration gate)) #f)
                    ((any-synthesis-tool? step-entries)
                      (gate-skip! gate session 'post-turn "cooldown"))
                    ((> (systemone-gate-cooldown-left gate) 0)
                      (set-systemone-gate-cooldown-left!
                        gate
                        (- (systemone-gate-cooldown-left gate) 1))
                      (gate-skip! gate session 'post-turn "cooldown"))
                    ((not (zero? (modulo (systemone-gate-steps-with-tools gate) nth)))
                      (gate-skip! gate session 'post-turn "deferred"))
                    (else (gate-evaluate! gate session 'post-turn))))))
            #f)
          (lambda _ #f)))

(define (systemone-gate-checkpoint! gate session)
        "Exploration-checkpoint hook ((FN SESSION) -> #t | #f).  Returns #t
when the gate handled the checkpoint (consulted and injected, or skipped
during cooldown/synthesis); #f when the hook could not handle it, letting
runtimes-react fall back to its hardcoded message.  Bypasses
every-nth-step but respects cooldown."
        (catch #t
          (lambda ()
            (if (gate-disabled? gate)
              #f
              (let* ((opts (systemone-gate-options gate))
                     (history
                       (systemone-gate-history
                         (react-session-messages session)
                         #:max-history (systemone-gate-options-max-history opts)))
                     (iteration (session-iteration session))
                     (step-entries (current-step-entries history)))
                (if (= iteration (systemone-gate-last-check-iteration gate))
                  #t
                  (begin
                    (set-systemone-gate-last-check-iteration! gate iteration)
                    (cond
                      ((or (any-synthesis-tool? step-entries)
                           (> (systemone-gate-cooldown-left gate) 0))
                        (when (> (systemone-gate-cooldown-left gate) 0)
                          (set-systemone-gate-cooldown-left!
                            gate
                            (- (systemone-gate-cooldown-left gate) 1)))
                        (gate-inject! session %systemone-gate-cooldown-nudge)
                        (gate-skip! gate
                                    session
                                    'exploration-checkpoint
                                    "cooldown"))
                      (else
                        (gate-evaluate! gate session 'exploration-checkpoint)))
                    #t)))))
          (lambda _ #f)))

(define (make-systemone-gate-hooks gate)
        "Return the session hooks alist for GATE: 'post-turn and
'exploration-checkpoint.  A disabled gate (or absent config) returns '(),
which keeps the runtimes-react session byte-identical to today."
        (if (gate-disabled? gate)
          '()
          `((post-turn . ,(lambda (session) (systemone-gate-post-turn! gate session)))
            (exploration-checkpoint .
                                    ,(lambda (session) (systemone-gate-checkpoint! gate session))))))
