;;; spec.geis.scm — Kernel contract for (geasa runtimes-loop-guard plugin)
;;;
;;; Hermetic contract for the tool-loop breaking guard.  All verification
;;; runs against mock model endpoints and locally built sessions — no
;;; network, no filesystem writes.  The integration group reproduces the
;;; observed dogfood failure (a model re-issuing one identical read call)
;;; and verifies the full escalation ladder: memoized ERROR block,
;;; context surgery with in-place annotation, forced tool-less
;;; reflection, dynamic tool masking, best-of-N re-rolls, and the final
;;; steering-queue halt backstop.

(define-module (test runtimes-loop-guard-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (srfi srfi-1)
               #:use-module (json)
               #:use-module (web response)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa providers-model common)
               #:use-module (geasa providers-model openai)
               #:use-module (geasa providers-model mock)
               #:use-module (badb kernel registry)
               #:use-module (geasa runtimes-react plugin)
               #:use-module (geasa runtimes-loop-guard plugin))

(spec-begin "runtimes-loop-guard")

;;;
;;; Transcript construction helpers
;;;
(define %user0 '((role . "user") (content . "Read the config and summarize.")))

(define (tool-spec name)
        `((type . "function")
          (function . ((name . ,name) (description . "a tool")))))

(define (call-entry id name args)
        `((id . ,id)
          (type . "function")
          (function . ((name . ,name) (arguments . ,(scm->json-string args))))))

(define (assistant-turn . entries)
        `((role . "assistant") (content . "") (tool_calls . ,(list->vector entries))))

(define (tool-msg id name content)
        `((role . "tool") (tool_call_id . ,id) (name . ,name) (content . ,content)))

(define (blocked-content name)
        (safe-json-string (blocked-tool-payload name "Execution blocked: identical call (unit)")))

(define (session-at msgs iteration)
        (let ((session (make-react-session #:messages msgs)))
          (set-react-session-iteration! session iteration)
          session))

(define (prime-with-first-turn name args . guard-opt)
        "Return (values guard session): SESSION's transcript ends in one
EXECUTED (assistant tool call + tool result) turn of NAME with ARGS at
turn 1, and GUARD (a fresh guard, or the optional first argument) has
observed that single emission."
        (let* ((guard (if (pair? guard-opt) (car guard-opt) (make-loop-guard)))
               (msgs (list %user0
                           (assistant-turn (call-entry "c1" name args))
                           (tool-msg "c1" name "THE REAL OUTPUT")))
               (session (session-at msgs 1)))
          (guard-post-turn guard session)
          (values guard session)))

(define (append-turn! session name args)
        "Append one BLOCKED duplicate (assistant call + ERROR result) turn to
SESSION with a fresh call id."
        (let* ((id (format #f "c~a" (+ 1 (length (react-session-messages session)))))
               (turn (list (assistant-turn (call-entry id name args))
                           (tool-msg id name (blocked-content name)))))
          (set-react-session-messages! session (append (react-session-messages session) turn))))

(define (dup-args) (list (cons "path" "x.json")))

(define (dup-tc) (list (cons 'name "read_file") (cons 'arguments (dup-args))))

;;;
;;; 1. Idempotence classification & signature canonicalization
;;;
(test-group "loop-guard-classification"
            (test-assert "read tools classify as idempotent"
                         (default-idempotent? "read_file"))
            (test-assert "search and index tools classify as idempotent"
                         (and (default-idempotent? "ctx_search")
                              (default-idempotent? "symbols_overview")
                              (default-idempotent? "git_status")))
            (test-assert "mutation tools never classify as idempotent"
                         (not (or (default-idempotent? "write_file")
                                  (default-idempotent? "edit_file")
                                  (default-idempotent? "bash")
                                  (default-idempotent? "ctx_exec")
                                  (default-idempotent? "scheme_eval")
                                  (default-idempotent? "git_commit")
                                  (default-idempotent? "run_tests"))))
            (test-assert "config classifier overrides the default"
                         (let ((guard (make-loop-guard #:idempotent? (lambda (n) (string=? n "only_this")))))
                           (and (not ((loop-guard-config-ref guard 'idempotent?) "read_file"))
                                ((loop-guard-config-ref guard 'idempotent?) "only_this"))))
            (let ((tc-a (list (cons 'name "read_file")
                              (cons 'arguments (list (cons "path" "x.json") (cons "offset" 0)))))
                  (tc-b (list (cons 'name "read_file")
                              (cons 'arguments (list (cons "offset" 0) (cons "path" "x.json"))))))
              (test-equal "argument order never changes identity"
                          (signature-of tc-a) (signature-of tc-b))
              (test-assert "different arguments change identity"
                           (not (string= (signature-of tc-a)
                                         (signature-of
                                           (list (cons 'name "read_file")
                                                 (cons 'arguments (list (cons "path" "y.json")))))))))
            (let ((guard (make-loop-guard)))
              (test-equal "fresh guard carries an empty memo" '() (loop-guard-seen-signatures guard))
              (test-equal "fresh guard requests no halt" #f (loop-guard-halt-requested? guard))
              (test-assert "hooks bundle wires the three session seams"
                           (let ((hooks (make-loop-guard-hooks guard)))
                             (and (procedure? (assoc-ref hooks 'intercept-tool-call))
                                  (procedure? (assoc-ref hooks 'completion))
                                  (procedure? (assoc-ref hooks 'post-turn)))))))

;;;
;;; 2. Memoized blocking (mechanism 3): the 2nd identical idempotent
;;;    emission is vetoed before execution with a structured ERROR
;;;    referencing the original turn and a result preview.
;;;
(test-group "loop-guard-memoized-block"
            ;; before any observation a fresh guard passes everything through
            (let* ((msgs (list %user0
                               (assistant-turn (call-entry "c1" "read_file" (dup-args)))
                               (tool-msg "c1" "read_file" "THE REAL OUTPUT")))
                   (bare-session (session-at msgs 1)))
              (test-equal "first emission passes through before any observation"
                          #f (guard-intercept (make-loop-guard) (dup-tc) bare-session)))
            (call-with-values
              (lambda () (prime-with-first-turn "read_file" (dup-args)))
              (lambda (guard session)
                (test-equal "emission observed and recorded"
                            1 (loop-guard-emissions guard (signature-of (dup-tc))))
                (let ((block (guard-intercept guard (dup-tc) session)))
                  (test-assert "second identical emission is blocked" (string? block))
                  (test-assert "block references the original turn"
                               (string-contains block "turn 1"))
                  (test-assert "block embeds the memoized result preview"
                               (string-contains block "THE REAL OUTPUT"))
                  (test-assert "react renders the structured ERROR envelope"
                               (string-contains
                                 (safe-json-string (blocked-tool-payload "read_file" block))
                                 "DuplicateToolCallException"))
                  (test-equal "different arguments pass" #f
                              (guard-intercept guard
                                               (list (cons 'name "read_file")
                                                     (cons 'arguments (list (cons "path" "other.json"))))
                                               session))
                  (test-equal "mutation tools always pass" #f
                              (guard-intercept guard
                                               (list (cons 'name "run_tests")
                                                     (cons 'arguments (list (cons "suite" "all"))))
                                               session))))))

;;;
;;; 3. Context surgery (mechanism 1): once the trailing turn consists
;;;    entirely of blocked duplicates past the surgery threshold, the
;;;    block is pruned from the transcript and the ORIGINAL tool output
;;;    is annotated in place.
;;;
(test-group "loop-guard-context-surgery"
            (call-with-values
              (lambda () (prime-with-first-turn "read_file" (dup-args)))
              (lambda (guard session)
                ;; emission 2: the ERROR block stays in context for exactly one step
                ;; (the model must SEE the structured failure)
                (append-turn! session "read_file" (dup-args))
                (set-react-session-iteration! session 2)
                (guard-post-turn guard session)
                (test-equal "emission 2 recorded"
                            2 (loop-guard-emissions guard (signature-of (dup-tc))))
                (test-equal "below surgery threshold the blocked block persists"
                            5 (length (react-session-messages session)))
                ;; emission 3: trailing duplicate block pruned, original tool output
                ;; annotated in place
                (append-turn! session "read_file" (dup-args))
                (set-react-session-iteration! session 3)
                (guard-post-turn guard session)
                (let ((msgs (react-session-messages session)))
                  (test-equal "blocked duplicate block pruned from history"
                              5 (length msgs))
                  (test-equal "repeated structural block never accumulates in context"
                              2 (length (filter (lambda (m)
                                                  (and (equal? (assoc-ref m 'role) "assistant")
                                                       (assoc-ref m 'tool_calls)))
                                                msgs)))
                  (test-equal "at most one ERROR block stays in context"
                              1 (length (filter (lambda (m)
                                                  (and (equal? (assoc-ref m 'role) "tool")
                                                       (string-contains (assoc-ref m 'content)
                                                                        "DuplicateToolCallException")))
                                                msgs)))
                  (test-assert "original tool output annotated in place with the guard note"
                               (any (lambda (m)
                                      (and (equal? (assoc-ref m 'role) "tool")
                                           (string-contains (assoc-ref m 'content) "[badb loop-guard")))
                                    msgs))
                  (test-assert "original output content preserved under the note"
                               (any (lambda (m)
                                      (and (equal? (assoc-ref m 'tool_call_id) "c1")
                                           (string-contains (assoc-ref m 'content) "THE REAL OUTPUT")))
                                    msgs))))))

;;;
;;; 4. Forced reflection (mechanism 2): a pending reflection forces one
;;;    tool_choice="none" turn whose reflection exchange is persisted as
;;;    pre-turns, then tool calling resumes.
;;;
(test-group "loop-guard-forced-reflection"
            (call-with-values
              (lambda ()
                (prime-with-first-turn "read_file"
                                       (dup-args)
                                       (make-loop-guard #:reflect-after 2 #:mask-after 6 #:roll-attempts 1)))
              (lambda (guard session)
                ;; second (blocked) emission reaches reflect-after
                (append-turn! session "read_file" (dup-args))
                (set-react-session-iteration! session 2)
                (guard-post-turn guard session)
                (let*
                    ((base-calls '()) (last-msgs #f)
                     (messages (react-session-messages session))
                     (tools (list (tool-spec "read_file")))
                     (fake-base
                       (lambda*
                         (msgs eff-tools #:key (temperature #f) (tool-choice #f))
                         (set! last-msgs msgs)
                         (set! base-calls
                               (cons
                                 (list (map tool-spec-name eff-tools) tool-choice temperature (length msgs))
                                 base-calls))
                         (if (equal? tool-choice "none")
                           (values "Missing: nothing. The executed call sufficed." "" '())
                           (values "Final answer from the original data."
                                   ""
                                   (list
                                     (cons (cons 'name "read_file")
                                           (cons 'arguments (list (cons "path" "other.json"))))))))))
                  (call-with-values
                    (lambda ()
                      (guard-completion guard fake-base messages tools))
                    (lambda (content reasoning tool-calls pre-turns)
                      (let ((calls (reverse base-calls)))
                        (test-equal "one reflection turn plus one real turn" 2 (length calls))
                        (test-equal
                          "reflection turn is tool-less via tool_choice"
                          "none"
                          (cadr (car calls)))
                        (test-assert
                          "reflection tool-less turn advertises no tools"
                          (null? (car (car calls))))
                        (test-equal
                          "reflection prompt rides as ephemeral message"
                          (+ 1 (length messages))
                          (cadddr (car calls)))
                        (test-equal
                          "reflection exchange + resume user prompt feed subsequent real call"
                          (+ 3 (length messages))
                          (cadddr (cadr calls)))
                        (test-equal
                          "subsequent real call ends on user turn, never model turn"
                          "user"
                          (assoc-ref (last last-msgs) 'role))
                        (test-equal
                          "reflection exchange + resume prompt persisted as pre-turns"
                          3
                          (length pre-turns))
                        (test-assert "reflection user prompt persisted"
                                     (string-contains (assoc-ref (car pre-turns) 'content) "temporarily disabled"))
                        (test-assert
                          "reflection assistant answer persisted"
                          (equal? (assoc-ref (cadr pre-turns) 'role) "assistant"))
                        (test-assert
                          "trailing resume user prompt persisted"
                          (equal? (assoc-ref (caddr pre-turns) 'role) "user")))))
                  ;; pending-reflect was consumed: the next completion is single-shot
                  (let ((before (length base-calls)))
                    (call-with-values
                      (lambda ()
                        (guard-completion guard fake-base messages tools))
                      (lambda (_c _r _t pre-turns2)
                        (test-equal "no reflection on the following turn" 0 (length pre-turns2))
                        (test-equal
                          "single completion after reflection was used"
                          (+ 1 before)
                          (length base-calls)))))))))

;;;
;;; 5. Dynamic tool masking (mechanism 4b): past the mask threshold the
;;;    looping tool's definition vanishes from the advertised schema.
;;;
(test-group "loop-guard-tool-masking"
            (call-with-values
              (lambda () (prime-with-first-turn
                           "read_file"
                           (dup-args)
                           (make-loop-guard #:mask-after 2
                                            #:reflect-after 100
                                            #:roll-attempts 1)))
              (lambda (guard session)
                (append-turn! session "read_file" (dup-args))
                (set-react-session-iteration! session 2)
                (guard-post-turn guard session)
                (test-assert "mask recorded for the looping tool"
                             (member "read_file" (loop-guard-active-masks guard)))
                (let* ((seen '())
                       (fake-base
                         (lambda* (msgs eff-tools #:key (temperature #f) (tool-choice #f))
                                  (set! seen (cons (map tool-spec-name eff-tools) seen))
                                  (values "Answer." "" '())))
                       (tools (list (tool-spec "read_file") (tool-spec "other_tool"))))
                  (call-with-values
                    (lambda () (guard-completion guard fake-base '() tools))
                    (lambda (_c _r _t _p)
                      (test-equal "masked tool filtered from the advertised schema"
                                  '("other_tool") (car seen)))))))

            ;; control: without a mask the full schema passes through
            (let* ((seen2 '())
                   (fresh (make-loop-guard))
                   (fake-base2
                     (lambda* (msgs eff-tools #:key (temperature #f) (tool-choice #f))
                              (set! seen2 (cons (map tool-spec-name eff-tools) seen2))
                              (values "Answer." "" '())))
                   (tools2 (list (tool-spec "read_file") (tool-spec "other_tool"))))
              (call-with-values
                (lambda () (guard-completion fresh fake-base2 '() tools2))
                (lambda (_c _r _t _p)
                  (test-equal "unmasked completion advertises the full schema"
                              '("read_file" "other_tool") (car seen2))))))

;;;
;;; 6. Turn-level rejection sampling (mechanism 4a): candidates whose
;;;    tool calls repeat a memoized signature are discarded and
;;;    re-requested with stepped temperatures.
;;;
(test-group "loop-guard-best-of-n"
            (call-with-values
              (lambda () (prime-with-first-turn
                           "read_file"
                           (dup-args)
                           (make-loop-guard #:reflect-after 100
                                            #:mask-after 100
                                            #:surgery-after 100)))
              (lambda (guard session)
                (let* ((temps '())
                       (fake-base
                         (lambda* (msgs eff-tools #:key (temperature #f) (tool-choice #f))
                                  (set! temps (cons temperature temps))
                                  (if (= 1 (length temps))
                                    (values "Looping candidate." "" (list (dup-tc)))
                                    (values "Pivoted candidate." ""
                                            (list (cons (cons 'name "read_file")
                                                        (cons 'arguments (list (cons "path" "other.json"))))))))))
                  (call-with-values
                    (lambda () (guard-completion guard fake-base '() '()))
                    (lambda (_c _r tool-calls _p)
                      (test-equal "duplicate candidate rejected and re-requested"
                                  2 (length temps))
                      (test-equal "re-roll runs at the stepped temperature"
                                  0.8 (car temps))
                      (test-assert "rejected candidate discarded for the fresh one"
                                   (not (string=? (signature-of (car tool-calls))
                                                  (signature-of (dup-tc))))))))
                ;; all candidates duplicate: the budget accepts the last and the
                ;; interception ladder handles it downstream
                (let* ((calls 0)
                       (fake-base
                         (lambda* (msgs eff-tools #:key (temperature #f) (tool-choice #f))
                                  (set! calls (+ calls 1))
                                  (values "Same again." "" (list (dup-tc))))))
                  (call-with-values
                    (lambda () (guard-completion guard fake-base '() '()))
                    (lambda (_c _r tool-calls _p)
                      (test-equal "bounded re-roll budget respected" 3 calls)
                      (test-equal "last candidate accepted when every roll duplicates"
                                  (list (signature-of (dup-tc)))
                                  (map signature-of tool-calls))))))))

;;;
;;; 7. Full ReAct integration: the dogfood loop scenario
;;;
(test-group "loop-guard-react-integration"
            (let* ((exec-count 0)
                   (reg (make-registry))
                   (_ (registry-attach! reg
                                        "read.config"
                                        (lambda (path)
                                          (set! exec-count (+ exec-count 1))
                                          "THE CONFIG CONTENTS")
                                        '((description . "Read the config"))))
                   (_ (registry-attach! reg
                                        "other.tool"
                                        (lambda () "OTHER")
                                        '((description . "Other tool"))))
                   (client (make-model-client #:endpoint "http://127.0.0.1:10100"
                                              #:model "looping-llm"))
                   (steering (make-steering-queue))
                   (guard (make-loop-guard))
                   (bodies '())
                   (llm-calls 0)
                   (dup-response
                     (scm->json-string
                       '((choices . #(((message . ((role . "assistant")
                                                   (content . "Let me read the config.")
                                                   (tool_calls . #(((id . "call_loop")
                                                                    (type . "function")
                                                                    (function . ((name . "read_config")
                                                                                 (arguments . "{\"path\":\"config.json\"}"))))))))))))))
                   (reflection-response
                     (scm->json-string
                       '((choices . #(((message . ((role . "assistant")
                                                   (content . "Reflection: the original data suffices; next I will summarize.")))))))))
                   (mock-poster
                     (lambda* (uri #:key body headers streaming?)
                              (set! llm-calls (+ llm-calls 1))
                              (set! bodies (cons body bodies))
                              (let* ((parsed (catch #t
                                               (lambda () (json-string->scm body))
                                               (lambda _ '())))
                                     (tool-choice (assoc-ref parsed "tool_choice")))
                                (values
                                  (build-response #:code 200)
                                  (if (equal? tool-choice "none") reflection-response dup-response)))))
                   (session
                     (make-react-session client
                                         #:initial-messages (list %user0)
                                         #:reg reg
                                         #:max-iterations 20
                                         #:steering-queue steering
                                         #:hooks (make-loop-guard-hooks guard))))
              (parameterize ((current-http-post mock-poster))
                            (call-with-values
                              (lambda () (react-run! session))
                              (lambda (verdict final-text sess)
                                (let ((msgs (react-session-messages sess)))
                                  (test-equal "backstop halts the pathological looping run"
                                              'interrupted verdict)
                                  (test-equal "idempotent tool executed exactly once"
                                              1 exec-count)
                                  (test-assert "original tool output annotated in place"
                                               (any (lambda (m)
                                                      (and (equal? (assoc-ref m 'role) "tool")
                                                           (string-contains (assoc-ref m 'content)
                                                                            "[badb loop-guard")))
                                                    msgs))
                                  (test-assert "forced reflection exchange persisted"
                                               (and (any (lambda (m)
                                                           (and (equal? (assoc-ref m 'role) "user")
                                                                (string-contains (assoc-ref m 'content)
                                                                                 "temporarily disabled")))
                                                         msgs)
                                                    (any (lambda (m)
                                                           (and (equal? (assoc-ref m 'role) "assistant")
                                                                (string-contains (assoc-ref m 'content)
                                                                                 "Reflection: the original data suffices")))
                                                         msgs)))
                                  (let ((parsed-bodies
                                          (map (lambda (b)
                                                 (catch #t
                                                   (lambda () (json-string->scm b))
                                                   (lambda _ '())))
                                               bodies)))
                                    (test-assert "wire saw a tool_choice=none reflection turn"
                                                 (any (lambda (p)
                                                        (equal? (assoc-ref p "tool_choice") "none"))
                                                      parsed-bodies))
                                    (test-assert "wire saw stepped re-roll temperatures"
                                                 (any (lambda (p)
                                                        (let ((t (assoc-ref p "temperature")))
                                                          (or (equal? t 0.8) (equal? t 0.95))))
                                                      parsed-bodies))
                                    (test-assert "masking hid the looping tool from the schema"
                                                 (any (lambda (p)
                                                        (let* ((tools (vector->list
                                                                        (or (assoc-ref p "tools") #())))
                                                               (names (map (lambda (t)
                                                                             (assoc-ref
                                                                               (assoc-ref t "function")
                                                                               "name"))
                                                                           tools)))
                                                          (and (member "other_tool" names)
                                                               (not (member "read_config" names)))))
                                                      parsed-bodies))
                                    (let ((marker-counts
                                            (map
                                              (lambda (p)
                                                (length
                                                  (filter
                                                    (lambda (m)
                                                      (and (equal? (assoc-ref m "role") "tool")
                                                           (string-contains
                                                             (or (assoc-ref m "content") "")
                                                             "DuplicateToolCallException")))
                                                    (vector->list (or (assoc-ref p "messages") #())))))
                                              parsed-bodies)))
                                      (test-assert "model saw the structured ERROR at least once"
                                                   (any (lambda (n) (>= n 1)) marker-counts))
                                      (test-assert "at most one ERROR block ever accumulated in context"
                                                   (every (lambda (n) (<= n 1)) marker-counts))))))))))

(test-group
  "loop-guard-static-analysis-and-failed-retries"
  (test-assert
    "ast_modify is classified as mutation"
    (not (default-idempotent? "ast_modify")))
  (test-assert
    "workspace_manage is classified as mutation"
    (not (default-idempotent? "workspace_manage")))
  (test-assert
    "commit_workspace is classified as mutation"
    (not (default-idempotent? "commit_workspace")))
  (call-with-values
    (lambda ()
      (prime-with-first-turn "ast_lint" '(("path" . #(0 1)) ("dialect" . ":scheme"))))
    (lambda (guard session)
      (let
          ((verdict
             (guard-intercept guard
                              '((name . "ast_lint") (arguments . (("path" . #(0 1)) ("dialect" . ":scheme"))))
                              session)))
        (test-assert
          "ast_lint returns cached pair instead of blocking"
          (and (pair? verdict) (eq? (car verdict) 'cached)))
        (test-equal
          "cached result matches original output"
          "THE REAL OUTPUT"
          (cdr verdict)))
      (let
          ((mut-turn
             (list
               (assistant-turn (call-entry "c_mut" "ast_modify" '(("path" . #(0 1 2)))))
               (tool-msg "c_mut" "ast_modify" "Modified node"))))
        (set-react-session-messages! session
                                     (append (react-session-messages session) mut-turn))
        (set-react-session-iteration! session 2)
        (guard-post-turn guard session)
        (test-equal
          "mutation clears seen signatures for fresh verification"
          '()
          (loop-guard-seen-signatures guard))
        (test-equal
          "ast_lint passes through fresh after mutation"
          #f
          (guard-intercept guard
                           '((name . "ast_lint") (arguments . (("path" . #(0 1)) ("dialect" . ":scheme"))))
                           session)))))
  (let*
      ((guard (make-loop-guard))
       (fail-msgs
         (list %user0
               (assistant-turn (call-entry "c_fail" "read_file" (dup-args)))
               (tool-msg "c_fail"
                         "read_file"
                         "Execution of tool 'read_file' failed (misc-error): transient")))
       (fail-sess (session-at fail-msgs 1)))
    (guard-post-turn guard fail-sess)
    (test-equal "failed call is not recorded in seen"
                0
                (loop-guard-emissions guard (signature-of (dup-tc))))
    (test-equal
      "retry after failed call is not blocked"
      #f
      (guard-intercept guard (dup-tc) fail-sess))))

(spec-end)
(spec-exit)
