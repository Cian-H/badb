;;; spec.geis.scm — Kernel contract for (geasa policies-systemone-gate plugin)
;;;
;;; Hermetic contract for the optional system-1 geis gate: config parsing,
;;; history/state assembly, the act-only-when-confident verdict policy,
;;; cooldown after a stop and during synthesis, the 'post-turn hook, the
;;; 'exploration-checkpoint tripwire hook, and the disabled-gate invariant.
;;; The decider is always an injected fake procedure: no network, no live
;;; key, no real model.  Only /tmp is written (a scratch journal file).

(define-module (test policies-systemone-gate-spec)
               #:use-module
               (srfi srfi-64)
               #:use-module
               (badb spec)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (json)
               #:use-module
               (ice-9 format)
               #:use-module
               (geasa foundation-config plugin)
               #:use-module
               (badb kernel journal)
               #:use-module
               (geasa runtimes-react plugin)
               #:use-module
               (geasa policies-systemone-gate plugin))

(spec-begin "policies-systemone-gate")

;;;
;;; Transcript construction helpers
;;;
(define %user0
        '((role . "user")
          (content . "Scan every bundle manifest under src/geasa and summarize it.")))

(define (call-entry id name args)
        `((id . ,id)
          (type . "function")
          (function . ((name . ,name)
                       (arguments . ,(if (string? args) args (scm->json-string args)))))))

(define (assistant-turn . entries)
        `((role . "assistant") (content . "") (tool_calls . ,(list->vector entries))))

(define (tool-msg id name content)
        `((role . "tool") (tool_call_id . ,id) (name . ,name) (content . ,content)))

(define (tool-turn id name args content)
        (list (assistant-turn (call-entry id name args)) (tool-msg id name content)))

(define (three-turn-msgs)
        (append (list %user0)
                (tool-turn "c1" "read_file" '((path . "a.scm")) "manifest A")
                (tool-turn "c2" "read_file" '((path . "b.scm")) "manifest B")
                (tool-turn "c3" "bash" '((command . "ls src/geasa")) "a b c")))

(define (session-at msgs iteration)
        (let ((session (make-react-session #:messages msgs)))
          (set-react-session-iteration! session iteration)
          session))

(define %journal-path "/tmp/badb-systemone-gate-spec.jsonl")

(define (fresh-journal)
        (when (file-exists? %journal-path)
          (delete-file %journal-path))
        (make-journal %journal-path))

(define (gate-entries jrn)
        (filter (lambda (entry)
                  (equal? (assoc-ref entry "feature-id") "systemone-gate"))
                (journal-entries jrn)))

(define (last-gate-entry jrn)
        (let ((entries (gate-entries jrn)))
          (and (pair? entries) (last entries))))

(define (verdict decision confidence noul)
        `((decision . ,decision)
          (confidence . ,confidence)
          (noul . ,noul)
          (probabilities . ())
          (raw . #f)))

(define (enabled-options . args)
        (apply make-systemone-gate-options #:enabled #t args))

(define (fake-check result)
        "Return (values check count-cell): CHECK records invocations and
returns RESULT, a (VERDICT . OK?) pair."
        (let ((calls (list 0)))
          (values (lambda (state)
                    (set-car! calls (+ 1 (car calls)))
                    (values (car result) (cdr result)))
                  calls)))

;;;
;;; 1. Config parsing
;;;
(test-group "gate-config"
            (let ((opts (make-systemone-gate-options)))
              (test-assert "options record" (systemone-gate-options? opts))
              (test-assert "disabled by default" (not (systemone-gate-options-enabled? opts)))
              (test-equal "provider unset by default" #f (systemone-gate-options-provider opts))
              (test-equal "endpoint unset by default" #f (systemone-gate-options-endpoint opts))
              (test-equal "api key unset by default" #f (systemone-gate-options-api-key opts))
              (test-equal "default model" "jev-latest" (systemone-gate-options-model opts))
              (test-equal "default auto accept" 0.8 (systemone-gate-options-auto-accept opts))
              (test-equal "default noul floor" 0.5 (systemone-gate-options-noul-floor opts))
              (test-equal "default every nth step" 1 (systemone-gate-options-every-nth-step opts))
              (test-equal "default max history" 24 (systemone-gate-options-max-history opts))
              (test-equal "default cooldown" 5 (systemone-gate-options-cooldown-steps opts))
              (test-equal "default max context chars"
                          4000
                          (systemone-gate-options-max-context-chars opts)))
            (let ((opts (systemone-gate-config->options '((model-name . "x")))))
              (test-assert "absent block is disabled" (not (systemone-gate-options-enabled? opts))))
            (let ((warnings '()))
              (let* ((raw
                       '((providers
                          (typesafe
                           (endpoint "https://s1.example/v1/systemone")
                           (api-key "secret")
                           (kind systemone)
                           (models "jev-preset")))
                         (systemone-gate
                          (enabled #t)
                          (provider typesafe)
                          (auto-accept 0.9)
                          (noul-floor 0.6)
                          (every-nth-step 2)
                          (max-history 10)
                          (cooldown-steps 3)
                          (max-context-chars 2000))))
                     (opts
                       (systemone-gate-config->options
                         raw
                         #:warn (lambda (msg) (set! warnings (cons msg warnings))))))
                (test-assert "enabled block enables the gate"
                             (systemone-gate-options-enabled? opts))
                (test-equal "provider preset name" 'typesafe (systemone-gate-options-provider opts))
                (test-equal "endpoint comes from the preset"
                            "https://s1.example/v1/systemone"
                            (systemone-gate-options-endpoint opts))
                (test-equal "api key comes from the preset"
                            "secret"
                            (systemone-gate-options-api-key opts))
                (test-equal "model comes from the preset"
                            "jev-preset"
                            (systemone-gate-options-model opts))
                (test-equal "auto accept overridden" 0.9 (systemone-gate-options-auto-accept opts))
                (test-equal "noul floor overridden" 0.6 (systemone-gate-options-noul-floor opts))
                (test-equal "throttle overridden" 2 (systemone-gate-options-every-nth-step opts))
                (test-equal "history cap overridden" 10 (systemone-gate-options-max-history opts))
                (test-equal "cooldown overridden" 3 (systemone-gate-options-cooldown-steps opts))
                (test-equal "context cap overridden"
                            2000
                            (systemone-gate-options-max-context-chars opts))
                (test-equal "matching kind does not warn" '() warnings)))
            (let ((warnings '()))
              (let* ((raw
                       '((providers (typesafe (endpoint "https://s1.example") (kind chat)))
                         (systemone-gate (enabled #t) (provider typesafe))))
                     (opts
                       (systemone-gate-config->options
                         raw
                         #:warn (lambda (msg) (set! warnings (cons msg warnings))))))
                (test-assert "mismatched kind still enables the gate"
                             (systemone-gate-options-enabled? opts))
                (test-equal "endpoint still resolved" "https://s1.example"
                            (systemone-gate-options-endpoint opts))
                (test-assert "mismatched kind warns"
                             (and (pair? warnings) (string-contains (car warnings) "kind")))))
            (let* ((raw
                     '((systemone-gate
                        (enabled #t)
                        (endpoint "https://direct.example")
                        (api-key "direct-key")
                        (model "jev-direct"))))
                   (opts (systemone-gate-config->options raw)))
              (test-equal "direct endpoint" "https://direct.example"
                          (systemone-gate-options-endpoint opts))
              (test-equal "direct api key" "direct-key" (systemone-gate-options-api-key opts))
              (test-equal "direct model" "jev-direct" (systemone-gate-options-model opts))
              (test-equal "direct options need no provider" #f
                          (systemone-gate-options-provider opts)))
            (let* ((cfg (make-badb-config
                          #:options
                          '((providers
                             (typesafe
                              (endpoint "https://cfg.example")
                              (api-key "cfg-key")))
                            (systemone-gate (enabled #t) (provider typesafe)))))
                   (opts (systemone-gate-config->options cfg)))
              (test-assert "a config record is accepted too"
                           (systemone-gate-options-enabled? opts))
              (test-equal "config provider resolution uses foundation-config"
                          "https://cfg.example"
                          (systemone-gate-options-endpoint opts))
              (test-equal "config provider api key" "cfg-key"
                          (systemone-gate-options-api-key opts))))

;;;
;;; 2. History assembly
;;;
(test-group "gate-history"
            (let* ((msgs (three-turn-msgs))
                   (history (systemone-gate-history msgs)))
              (test-equal "one entry per executed call" 3 (length history))
              (test-equal "oldest first" "read_file" (assoc-ref (car history) "tool"))
              (test-equal "step ordinals are 1-based"
                          '(1 2 3)
                          (map (lambda (entry) (assoc-ref entry "step")) history))
              (test-equal "arguments are recorded"
                          "{\"path\":\"a.scm\"}"
                          (assoc-ref (car history) "args"))
              (test-equal "result summary recorded"
                          "manifest A"
                          (assoc-ref (car history) "result_summary"))
              (test-assert "successful results are ok" (assoc-ref (car history) "ok")))
            (let* ((msgs
                     (append (list %user0)
                             (tool-turn "c1" "read_file" '((path . "a")) "{\"error\":\"boom\"}")
                             (tool-turn "c2" "bash" '((command . "x"))
                                        "{\"status\":\"ERROR\",\"error_type\":\"DuplicateToolCallException\"}")
                             (tool-turn "c3" "read_file" '((path . "b")) "fine")))
                   (history (systemone-gate-history msgs)))
              (test-equal "error payload is not ok" #f (assoc-ref (car history) "ok"))
              (test-equal "blocked marker is not ok"
                          #f
                          (assoc-ref (cadr history) "ok"))
              (test-equal "plain result is ok" #t (assoc-ref (caddr history) "ok")))
            (let* ((msgs
                     (list %user0
                           (assistant-turn (call-entry "c1" "read_file" '((path . "a"))))
                           (tool-msg "c1" "read_file" "a")
                           (assistant-turn (call-entry "c2" "read_file" '((path . "b"))))
                           (assistant-turn (call-entry "c3" "bash" '((command . "ls"))))
                           (tool-msg "c3" "bash" "a b")))
                   (history (systemone-gate-history msgs)))
              (test-equal "orphan tool-call without a result is skipped"
                          2
                          (length history)))
            (let* ((msgs (list %user0 (tool-msg "orphan" "read_file" "x")))
                   (history (systemone-gate-history msgs)))
              (test-equal "orphan tool message is skipped" '() history))
            (let* ((msgs (three-turn-msgs))
                   (history (systemone-gate-history msgs #:max-history 2)))
              (test-equal "history cap applied" 2 (length history))
              (test-equal "cap keeps the newest entries"
                          '("read_file" "bash")
                          (map (lambda (entry) (assoc-ref entry "tool")) history)))
            (let* ((msgs (three-turn-msgs))
                   (history (systemone-gate-history msgs)))
              (test-assert "evidence digest names frequent tools"
                           (string-contains
                             (systemone-gate-evidence-digest history)
                             "read_file x2"))))

;;;
;;; 3. State assembly
;;;
(test-group "gate-state"
            (let*
                ((session (session-at (three-turn-msgs) 7))
                 (state
                   (systemone-gate-state session
                                         #:brief
                                         "badb test brief"
                                         #:model
                                         "tiel-test"
                                         #:active-geasa
                                         '("runtimes-react" "surfaces-tui"))))
              (test-equal
                "task comes from the first user message"
                "Scan every bundle manifest under src/geasa and summarize it."
                (assoc-ref state "task"))
              (test-equal "iteration recorded" 7 (assoc-ref state "iteration"))
              (let ((ctx (assoc-ref state "project_context")))
                (test-equal "brief recorded" "badb test brief" (assoc-ref ctx "brief"))
                (test-equal "model recorded" "tiel-test" (assoc-ref ctx "model"))
                (test-equal "active geasa capped and recorded"
                            '("runtimes-react" "surfaces-tui")
                            (vector->list (assoc-ref ctx "active_geasa"))))
              (let ((history (assoc-ref state "tool_history")))
                (test-assert "tool history is a JSON array" (vector? history))
                (test-equal "tool history count" 3 (vector-length history))
                (test-assert "tool history entries are objects" (pair? (vector-ref history 0)))))
            (let*
                ((long-task (make-string 900 #\x))
                 (msgs (list (cons 'role "user") (cons 'content long-task)))
                 (session (session-at msgs 1))
                 (state (systemone-gate-state session)))
              (test-assert "long task is truncated"
                           (<= (string-length (assoc-ref state "task")) 320)))
            (let*
                ((session (session-at (three-turn-msgs) 3))
                 (state (systemone-gate-state session #:max-context-chars 400)))
              (test-assert
                "serialized state respects the context cap"
                (<= (string-length (scm->json-string state)) 400))
              (test-assert
                "trimming keeps at most the history that fits"
                (<= (vector-length (assoc-ref state "tool_history")) 3)))
            (let*
                ((session (session-at (list %user0) 1)) (state (systemone-gate-state session)))
              (test-equal "empty history is an empty array"
                          '()
                          (vector->list (assoc-ref state "tool_history")))))

;;;
;;; 4. Verdict policy
;;;
(test-group "gate-verdict-policy"
            (test-equal "confident generate stops"
                        'stop
                        (systemone-gate-verdict->action (verdict "generate_geis" 0.9 0.8) #t))
            (test-equal "confident ask_user escalates"
                        'escalate
                        (systemone-gate-verdict->action (verdict "ask_user" 0.9 0.8) #t))
            (test-equal "low confidence continues"
                        'continue
                        (systemone-gate-verdict->action (verdict "generate_geis" 0.5 0.9) #t))
            (test-equal "low noul continues"
                        'continue
                        (systemone-gate-verdict->action (verdict "generate_geis" 0.9 0.2) #t))
            (test-equal "failed check continues"
                        'continue
                        (systemone-gate-verdict->action (verdict "generate_geis" 0.99 0.99) #f))
            (test-equal "continue decision continues"
                        'continue
                        (systemone-gate-verdict->action (verdict "continue" 0.99 0.99) #t))
            (test-equal "malformed verdict continues"
                        'continue
                        (systemone-gate-verdict->action '() #t))
            (test-equal "custom thresholds are honoured"
                        'continue
                        (systemone-gate-verdict->action
                          (verdict "generate_geis" 0.85 0.9)
                          #t
                          #:auto-accept 0.9)))

;;;
;;; 5. Post-turn hook: consult, inject, journal, cooldown
;;;
(test-group "gate-post-turn"
            (let* ((jrn (fresh-journal))
                   (result (cons (verdict "generate_geis" 0.92 0.81) #t)))
              (call-with-values (lambda () (fake-check result))
                                (lambda (check calls)
                                  (let* ((opts (enabled-options #:cooldown-steps 2))
                                         (gate (make-systemone-gate #:options opts #:check check #:journal jrn))
                                         (hooks (make-systemone-gate-hooks gate))
                                         (session (session-at (three-turn-msgs) 3)))
                                    (test-assert "post-turn hook is present"
                                                 (procedure? (assoc-ref hooks 'post-turn)))
                                    (test-assert "exploration hook is present"
                                                 (procedure? (assoc-ref hooks 'exploration-checkpoint)))
                                    ((assoc-ref hooks 'post-turn) session)
                                    (test-equal "decider consulted once" 1 (car calls))
                                    (let ((injected (last (react-session-messages session)))
                                          (entry (last-gate-entry jrn)))
                                      (test-equal "injected message is a user message"
                                                  "user"
                                                  (assoc-ref injected 'role))
                                      (test-assert "injected message carries the gate marker"
                                                   (string-contains (assoc-ref injected 'content)
                                                                    "[badb systemone-gate]"))
                                      (test-assert "injected message carries the stop template"
                                                   (string-contains (assoc-ref injected 'content)
                                                                    "reusable geis bundle"))
                                      (test-equal "journal feature id" "systemone-gate"
                                                  (assoc-ref entry "feature-id"))
                                      (test-equal "journal checkpoint" "post-turn"
                                                  (assoc-ref entry "checkpoint"))
                                      (test-equal "journal status" "stop" (assoc-ref entry "status"))
                                      (test-equal "journal result decision" "generate_geis"
                                                  (assoc-ref (assoc-ref entry "result") "decision")))
                                    ;; two cooldown steps must not consult again
                                    (set-react-session-messages!
                                      session
                                      (append (react-session-messages session)
                                              (tool-turn "c4" "read_file" '((path . "c")) "manifest C")))
                                    (set-react-session-iteration! session 4)
                                    ((assoc-ref hooks 'post-turn) session)
                                    (test-equal "cooldown step does not consult" 1 (car calls))
                                    (test-equal "cooldown step journals cooldown" "cooldown"
                                                (assoc-ref (last-gate-entry jrn) "status"))
                                    (set-react-session-messages!
                                      session
                                      (append (react-session-messages session)
                                              (tool-turn "c5" "read_file" '((path . "d")) "manifest D")))
                                    (set-react-session-iteration! session 5)
                                    ((assoc-ref hooks 'post-turn) session)
                                    (test-equal "second cooldown step does not consult" 1 (car calls))
                                    ;; after cooldown expires the gate evaluates again
                                    (set-react-session-messages!
                                      session
                                      (append (react-session-messages session)
                                              (tool-turn "c6" "read_file" '((path . "e")) "manifest E")))
                                    (set-react-session-iteration! session 6)
                                    ((assoc-ref hooks 'post-turn) session)
                                    (test-equal "gate evaluates after cooldown" 2 (car calls))))))
            (let* ((jrn (fresh-journal))
                   (result (cons (verdict "generate_geis" 0.92 0.81) #t)))
              (call-with-values (lambda () (fake-check result))
                                (lambda (check calls)
                                  (let* ((gate
                                           (make-systemone-gate
                                             #:options (enabled-options)
                                             #:check check
                                             #:journal jrn))
                                         (session (session-at (three-turn-msgs) 3)))
                                    ;; post-turn must skip once the tripwire already consulted
                                    (set-react-session-iteration! session 3)
                                    ((assoc-ref (make-systemone-gate-hooks gate) 'exploration-checkpoint)
                                     session)
                                    ((assoc-ref (make-systemone-gate-hooks gate) 'post-turn) session)
                                    (test-equal "tripwire consult suppresses the post-turn consult"
                                                1
                                                (car calls))
                                    (test-equal "only one journal row for the step"
                                                1
                                                (length (gate-entries jrn)))))))
            (let* ((jrn (fresh-journal))
                   (result (cons (verdict "generate_geis" 0.92 0.81) #t)))
              (call-with-values (lambda () (fake-check result))
                                (lambda (check calls)
                                  (let* ((gate
                                           (make-systemone-gate
                                             #:options (enabled-options)
                                             #:check check
                                             #:journal jrn))
                                         (msgs
                                           (append (three-turn-msgs)
                                                   (tool-turn "c4" "spec_lock" '((feature-id . "x")) "locked")))
                                         (session (session-at msgs 4)))
                                    ((assoc-ref (make-systemone-gate-hooks gate) 'post-turn) session)
                                    (test-equal "synthesis tool skips evaluation" 0 (car calls))
                                    (test-equal "synthesis tool journals cooldown" "cooldown"
                                                (assoc-ref (last-gate-entry jrn) "status"))))))
            (let* ((jrn (fresh-journal))
                   (result (cons (verdict "continue" 0.7 0.1) #t)))
              (call-with-values (lambda () (fake-check result))
                                (lambda (check calls)
                                  (let* ((gate
                                           (make-systemone-gate
                                             #:options (enabled-options)
                                             #:check check
                                             #:journal jrn))
                                         (session (session-at (three-turn-msgs) 3))
                                         (before (length (react-session-messages session))))
                                    ((assoc-ref (make-systemone-gate-hooks gate) 'post-turn) session)
                                    (test-equal "continue leaves the transcript untouched"
                                                before
                                                (length (react-session-messages session)))
                                    (test-equal "continue journal status" "continue"
                                                (assoc-ref (last-gate-entry jrn) "status"))))))
            (let* ((jrn (fresh-journal))
                   (result (cons (verdict "ask_user" 0.95 0.9) #t)))
              (call-with-values (lambda () (fake-check result))
                                (lambda (check calls)
                                  (let* ((gate
                                           (make-systemone-gate
                                             #:options (enabled-options)
                                             #:check check
                                             #:journal jrn))
                                         (session (session-at (three-turn-msgs) 3)))
                                    ((assoc-ref (make-systemone-gate-hooks gate) 'post-turn) session)
                                    (test-assert "escalate injects the ask-user template"
                                                 (string-contains
                                                   (assoc-ref (last (react-session-messages session)) 'content)
                                                   "Checkpoint verdict: ask_user"))
                                    (test-equal "escalate journal status" "escalate"
                                                (assoc-ref (last-gate-entry jrn) "status"))))))
            (let* ((jrn (fresh-journal))
                   (result (cons (verdict "generate_geis" 0.99 0.99) #t)))
              (call-with-values (lambda () (fake-check result))
                                (lambda (check calls)
                                  (let* ((gate
                                           (make-systemone-gate
                                             #:options (enabled-options)
                                             #:check check
                                             #:journal jrn))
                                         (session (session-at (three-turn-msgs) 3)))
                                    ;; A throwing decider must fail open, never escape.
                                    (let ((thrower (lambda (state) (error "boom"))))
                                      (set! gate (make-systemone-gate
                                                   #:options (enabled-options)
                                                   #:check thrower
                                                   #:journal jrn)))
                                    (test-assert "throwing decider does not escape"
                                                 (begin
                                                   ((assoc-ref (make-systemone-gate-hooks gate) 'post-turn)
                                                    session)
                                                   #t))
                                    (test-equal "throwing decider journals continue" "continue"
                                                (assoc-ref (last-gate-entry jrn) "status")))))))

;;;
;;; 6. Exploration-checkpoint hook
;;;
(test-group "gate-exploration-checkpoint"
            (let*
                ((jrn (fresh-journal)) (result (cons (verdict "continue" 0.6 0.1) #t)))
              (call-with-values
                (lambda ()
                  (fake-check result))
                (lambda (check calls)
                  (let*
                      ((gate
                         (make-systemone-gate #:options (enabled-options) #:check check #:journal jrn))
                       (session (session-at (three-turn-msgs) 3)))
                    (test-assert "continue consult reports handled"
                                 (eq? #t
                                      ((assoc-ref (make-systemone-gate-hooks gate) 'exploration-checkpoint) session)))
                    (test-assert
                      "continue consult injects a progress nudge"
                      (string-contains
                        (assoc-ref (last (react-session-messages session)) 'content)
                        "[badb systemone-gate]"))
                    (test-equal "continue journal checkpoint"
                                "exploration-checkpoint"
                                (assoc-ref (last-gate-entry jrn) "checkpoint"))
                    (test-equal "continue journal status"
                                "continue"
                                (assoc-ref (last-gate-entry jrn) "status"))))))
            (let*
                ((jrn (fresh-journal)) (result (cons (verdict "generate_geis" 0.95 0.9) #t)))
              (call-with-values
                (lambda ()
                  (fake-check result))
                (lambda (check calls)
                  (let*
                      ((gate
                         (make-systemone-gate #:options (enabled-options) #:check check #:journal jrn))
                       (session (session-at (three-turn-msgs) 3)))
                    (test-assert "stop consult reports handled"
                                 ((assoc-ref (make-systemone-gate-hooks gate) 'exploration-checkpoint) session))
                    (test-assert
                      "stop consult injects the stop template"
                      (string-contains
                        (assoc-ref (last (react-session-messages session)) 'content)
                        "reusable geis bundle"))
                    (test-equal "stop journal checkpoint"
                                "exploration-checkpoint"
                                (assoc-ref (last-gate-entry jrn) "checkpoint"))))))
            (let*
                ((jrn (fresh-journal)) (result (cons (verdict "generate_geis" 0.95 0.9) #t)))
              (call-with-values
                (lambda ()
                  (fake-check result))
                (lambda (check calls)
                  (let*
                      ((gate
                         (make-systemone-gate #:options
                                              (enabled-options #:cooldown-steps 3)
                                              #:check
                                              check
                                              #:journal
                                              jrn))
                       (session (session-at (three-turn-msgs) 3)))
                    ((assoc-ref (make-systemone-gate-hooks gate) 'post-turn) session)
                    ;; a tripwire during cooldown is still handled, without
                    ;; a second consult
                    (set-react-session-messages! session
                                                 (append (react-session-messages session)
                                                         (tool-turn "c4" "read_file" '((path . "c")) "manifest C")))
                    (set-react-session-iteration! session 4)
                    (test-assert
                      "tripwire during cooldown reports handled"
                      ((assoc-ref (make-systemone-gate-hooks gate) 'exploration-checkpoint) session))
                    (test-equal "tripwire during cooldown does not consult" 1 (car calls))
                    (test-equal
                      "tripwire during cooldown journals cooldown"
                      "cooldown"
                      (assoc-ref (last-gate-entry jrn) "status"))))))
            (let*
                ((jrn (fresh-journal)) (result (cons (verdict "generate_geis" 0.95 0.9) #t)))
              (call-with-values
                (lambda ()
                  (fake-check result))
                (lambda (check calls)
                  (let*
                      ((gate
                         (make-systemone-gate #:options (enabled-options) #:check check #:journal jrn))
                       (session (session-at (three-turn-msgs) 3)))
                    ;; Throwing tripwire hook degrades to "not handled" so
                    ;; runtimes-react can inject its hardcoded message.
                    (set! gate
                          (make-systemone-gate #:options
                                               (enabled-options)
                                               #:check
                                               (lambda (state)
                                                 (error "boom"))
                                               #:journal
                                               jrn))
                    (test-equal
                      "throwing decider fails open and is handled"
                      #t
                      ((assoc-ref (make-systemone-gate-hooks gate) (quote exploration-checkpoint))
                       session))))))
            (let*
                ((jrn (fresh-journal)) (result (cons (verdict "generate_geis" 0.95 0.9) #t)))
              (call-with-values
                (lambda ()
                  (fake-check result))
                (lambda (check calls)
                  (let*
                      ((gate
                         (make-systemone-gate #:options (enabled-options) #:check check #:journal jrn))
                       (msgs
                         (append (three-turn-msgs)
                                 (tool-turn "c4" "spec_synthesize" '((feature-id . "x")) "ok")))
                       (session (session-at msgs 4)))
                    (test-assert "synthesis tripwire reports handled"
                                 ((assoc-ref (make-systemone-gate-hooks gate) 'exploration-checkpoint) session))
                    (test-equal "synthesis tripwire does not consult" 0 (car calls))
                    (test-equal
                      "synthesis tripwire journals cooldown"
                      "cooldown"
                      (assoc-ref (last-gate-entry jrn) "status")))))))

;;;
;;; 7. Disabled gate and embedded messages
;;;
(test-group "gate-disabled"
            (let ((gate (make-systemone-gate #:options (make-systemone-gate-options))))
              (test-equal "disabled gate exposes no hooks"
                          '()
                          (make-systemone-gate-hooks gate)))
            (let ((gate (make-systemone-gate #:options (systemone-gate-config->options '()))))
              (test-equal "absent config exposes no hooks"
                          '()
                          (make-systemone-gate-hooks gate))))
(test-group "gate-messages"
            (test-assert "stop message is embedded with two placeholders"
                         (and (string-contains %systemone-gate-stop-message
                                               "[badb systemone-gate]")
                              (string-contains %systemone-gate-stop-message "~a")))
            (test-assert "escalate message is embedded with two placeholders"
                         (and (string-contains %systemone-gate-escalate-message
                                               "[badb systemone-gate]")
                              (string-contains %systemone-gate-escalate-message
                                               "confidence ~a")
                              (string-contains %systemone-gate-escalate-message
                                               "activity (~a)"))))

(spec-end)
(spec-exit)
