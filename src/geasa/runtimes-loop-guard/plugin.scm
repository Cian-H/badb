;;; (geasa runtimes-loop-guard plugin) — Tool-Loop Breaking Guard
;;;
;;; Contract-verified guardrails against in-context tool-calling loops,
;;; implemented on the runtimes-react session hook seams.  Small
;;; active-parameter models (e.g. qwen3.6-35b-a3b, ~3B active parameters)
;;; fall into identical (assistant tool-call -> tool result) repetition
;;; once the pattern appears 3+ times in the KV cache: induction heads
;;; lock onto the structural sequence and prose warnings cannot
;;; overcome the logit prior.  Research on agent trajectory management
;;; (context pruning, turn-level rejection sampling) shows that
;;; modifying the CONTEXT and DECODING CONSTRAINTS — not pleading with
;;; the model in prose — reliably breaks these loops.  This bundle
;;; implements four such mechanisms as an escalation ladder keyed on
;;; per-signature emission counts:
;;;
;;;   emissions 2  -> Memoized DuplicateToolCallException: identical
;;;                  idempotent calls are never re-executed; the tool
;;;                  role receives a structured ERROR payload (with a
;;;                  result preview and the original turn number) that
;;;                  post-trained tool models react to reliably.
;;;
;;;   emissions 3  -> Context surgery: fully-blocked duplicate turns are
;;;                  pruned from the transcript so repeated structural
;;;                  blocks never accumulate in context, and the
;;;                  ORIGINAL tool output is annotated in place with a
;;;                  harness guardrail note.
;;;
;;;   emissions 4  -> Forced reflection: exactly one tool_choice="none"
;;;                  turn is forced with a reflection prompt (what is
;;;                  missing, why did the previous call not provide it,
;;;                  what different action comes next); the reflection
;;;                  turn is persisted as pre-turns, tool calling is
;;;                  restored immediately afterwards.
;;;
;;;   emissions 5  -> Dynamic tool masking: the looping tool's
;;;                  definition is filtered out of the advertised tools
;;;                  array for a configurable number of turns (renewed
;;;                  on further attempts).
;;;
;;;   emissions 6+ -> Turn-level rejection sampling: whenever the model
;;;                  emits a memoized (blocked) signature, the
;;;                  candidate completion is discarded and re-requested
;;;                  with a stepped-up temperature, up to a small
;;;                  best-of-N budget, before the ladder's ERROR block
;;;                  and masking handle what survives.
;;;
;;;   hard cap     -> After too many blocked emissions of one
;;;                  signature, the run is halted via the session's
;;;                  steering queue ('interrupt) as a final backstop.
;;;
;;; Raw temperature spikes and warm prose warnings are deliberately
;;; avoided: uniform logit flattening corrupts tool-call JSON at least
;;; as often as it breaks the loop, and polite messages appended to the
;;; end of the prompt cannot be attended to past the repeated blocks.
;;; Only calls classified as IDEMPOTENT (typically read-only) are ever
;;; blocked, so legitimate repeated mutations (run_tests between edits,
;;; checkpoints) are never vetoed; those remain the runtimes-react loop
;;; detector's responsibility.

(define-module (geasa runtimes-loop-guard plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa runtimes-react plugin)
               #:use-module (ice-9 format)
               #:use-module (ice-9 optargs)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (<loop-guard>
                          make-loop-guard
                          loop-guard?
                          loop-guard-config-ref
                          loop-guard-turn
                          loop-guard-signature-entry
                          loop-guard-emissions
                          loop-guard-seen-signatures
                          loop-guard-active-masks
                          loop-guard-halt-requested?
                          make-loop-guard-hooks
                          default-idempotent?
                          signature-of
                          assistant-turn-signatures
                          split-trailing-turn
                          guard-block-message
                          guard-reflect-prompt
                          guard-surgery!
                          guard-intercept
                          guard-completion
                          guard-post-turn))

;;;
;;; Idempotence classification
;;;
(define %mutation-name-fragments
        '("write" "edit"
          "delete"
          "remove"
          "checkpoint"
          "rollback"
          "commit"
          "exec"
          "eval"
          "bash"
          "shell"
          "start"
          "stop"
          "restart"
          "create"
          "synthesize"
          "lock"
          "activate"
          "modify"
          "manage"
          "apply"
          "insert"
          "overwrite"
          "replace"
          "refactor"
          "rename"
          "format"
          "patch"
          "update"
          "reset"
          "merge"
          "rebase"))

(define %hard-mutation-names
        '("write_file" "edit_file"
          "replace_symbol_body"
          "run_tests"
          "bash"
          "sh"
          "ctx_exec"
          "scheme_eval"
          "git_commit"
          "spec_synthesize"
          "spec_lock"
          "geis_activate"
          "geis_deactivate"
          "ast_modify"
          "ast_rename"
          "ast_remove"
          "ast_relocate"
          "ast_replace_pattern"
          "ast_extract_function"
          "ast_extract_variable"
          "commit_workspace"
          "workspace_manage"
          "workspace_create_file"
          "workspace_merge"
          "workspace_rebase"))

(define %static-analysis-tool-names
        '("ast_lint" "ast_analyze_bindings"
          "ast_complexity_metrics"
          "ast_find_duplicates"
          "ast_suggest_refactorings"
          "symbols_overview"
          "find_symbol"))

(define (static-analysis-tool? name)
        (and (string? name) (member name %static-analysis-tool-names) #t))

(define (default-idempotent? name)
        "Conservative default classifier: NAME is treated as idempotent
(session-memoizable) unless it names a known mutation tool or one of a
set of mutation-suggesting substrings (write/edit/exec/bash/...).  An
unknown MCP tool that happens to mutate may slip through; pass
#:idempotent? to make-loop-guard to override."
        (and (string? name)
             (not (string-null? name))
             (not (member name %hard-mutation-names))
             (not (any (lambda (frag) (string-contains name frag))
                       %mutation-name-fragments))))

;;;
;;; Guard record
;;;
(define-record-type <loop-guard>
                    (%make-loop-guard config seen masks pending-reflect turn halt-requested?)
                    loop-guard?
                    (config loop-guard-config)
                    (seen loop-guard-seen set-loop-guard-seen!)
                    (masks loop-guard-masks set-loop-guard-masks!)
                    (pending-reflect loop-guard-pending-reflect set-loop-guard-pending-reflect!)
                    (turn loop-guard-turn set-loop-guard-turn!)
                    (halt-requested? loop-guard-halt-requested? set-loop-guard-halt-requested?!))

(define*
  (make-loop-guard
    #:key
    (block-after 2)
    (surgery-after 3)
    (reflect-after 4)
    (mask-after 5)
    (mask-turns 3)
    (roll-attempts 3)
    (roll-temperatures '(0.8 0.95))
    (preview-chars 400)
    (halt-after-blocked 8)
    (idempotent? default-idempotent?))
  "Create a loop guard with the escalation ladder keyed on per-signature
emission counts: BLOCK-AFTER 2nd identical idempotent emission yields a
memoized ERROR; SURGERY-AFTER 3rd prunes the duplicate turn; REFLECT-AFTER
4th forces one tool-less reflection turn; MASK-AFTER 5th masks the tool
from the advertised schema for MASK-TURNS iterations; ROLL-ATTEMPTS with
ROLL-TEMPERATURES bounds best-of-N rejection sampling against memoized
signatures; HALT-AFTER-BLOCKED finally requests a session interrupt.
IDEMPOTENT? classifies tool names for memoization."
  (%make-loop-guard
    `((block-after . ,block-after)
      (surgery-after . ,surgery-after)
      (reflect-after . ,reflect-after)
      (mask-after . ,mask-after)
      (mask-turns . ,mask-turns)
      (roll-attempts . ,roll-attempts)
      (roll-temperatures . ,roll-temperatures)
      (preview-chars . ,preview-chars)
      (halt-after-blocked . ,halt-after-blocked)
      (idempotent? . ,idempotent?))
    '()
    '()
    #f
    0
    #f))

(define (loop-guard-config-ref guard key)
        (assoc-ref (loop-guard-config guard) key))

(define (loop-guard-turn-of guard)
        (or (loop-guard-turn guard) 0))

;;;
;;; Signatures & transcript anatomy
;;;
(define (signature-of tc)
        "Canonical identity of TOOL-CALL alist TC (name + sorted args)."
        (tool-call-signature tc))

(define (history-signature name args-str)
        "Canonical identity from a persisted assistant turn: NAME plus
ARGS-STR (the JSON-encoded arguments string of a stored tool_calls
entry), parsed back to an alist so it matches emission-time signatures."
        (let ((args (or (safe-json-parse args-str args-str) '())))
          (signature-of `((name . ,name) (arguments . ,args)))))

(define (assistant-turn-signatures asst-msg)
        "Return the list of (SIGNATURE . CALL-ID) pairs for every tool
call in persisted assistant turn ASST-MSG."
        (let ((tcs (assoc-ref asst-msg 'tool_calls)))
          (if (vector? tcs)
            (filter-map
              (lambda (entry)
                (let* ((fn (assoc-ref entry 'function))
                       (name (and (pair? fn) (prop-ref fn 'name)))
                       (args-str (and (pair? fn) (prop-ref fn 'arguments)))
                       (cid (prop-ref entry 'id)))
                  (and name cid (cons (history-signature name args-str) cid))))
              (vector->list tcs))
            '())))

(define (split-trailing-turn msgs)
        "Split MSGS into (values assistant-msg tool-msgs leading) where
the trailing (assistant tool_calls + tool result) block ends the list.
Returns (values #f '() msgs) when no trailing tool block exists."
        (let loop ((rest (reverse msgs)) (tools '()))
                  (cond
                    ((null? rest) (values #f '() msgs))
                    ((and (pair? (car rest)) (equal? (assoc-ref (car rest) 'role) "tool"))
                      (loop (cdr rest) (cons (car rest) tools)))
                    ((and (pair? (car rest))
                          (equal? (assoc-ref (car rest) 'role) "assistant")
                          (assoc-ref (car rest) 'tool_calls))
                      (values (car rest) tools (reverse (cdr rest))))
                    (else (values #f '() msgs)))))

(define (tool-msg-blocked? tool-msg)
        (let ((content (assoc-ref tool-msg 'content)))
          (and (string? content)
               (string-contains content %duplicate-blocked-marker))))

(define (tool-msg-failed? tool-msg)
        "Return #t when TOOL-MSG represents a failed or incomplete tool execution
(error, conflict, aborted, permission denied), excluding duplicate-blocked
envelopes."
        (let
            ((content (and (pair? tool-msg) (assoc-ref tool-msg 'content))))
          (and (string? content)
               (not (tool-msg-blocked? tool-msg))
               (or (string-prefix? "Execution of tool " content)
                   (string-prefix? "Tool execution aborted" content)
                   (string-prefix? "Permission denied" content)
                   (string-prefix? "Conflict:" content)
                   (string-contains content "\"status\":\"ERROR\"")
                   (let ((parsed (safe-json-parse content #f)))
                     (and (pair? parsed)
                          (or (assoc-ref parsed 'error)
                              (assoc-ref parsed "error")
                              (equal? (assoc-ref parsed 'status) "ERROR")
                              (equal? (assoc-ref parsed "status") "ERROR"))))))))

;;;
;;; Memo table (mechanism 3: memoized DuplicateToolCallException)
;;;
(define (loop-guard-signature-entry guard sig)
        (assoc sig (loop-guard-seen guard) string=))

(define (loop-guard-emissions guard sig)
        (let ((entry (loop-guard-signature-entry guard sig)))
          (if entry (or (assoc-ref entry 'emissions) 0) 0)))

(define (loop-guard-seen-signatures guard)
        (map car (loop-guard-seen guard)))

(define (guard-seen-update! guard sig proc)
        "Update SEEN entry for SIG with (PROC ENTRY) -> new entry alist."
        (let* ((existing (loop-guard-signature-entry guard sig))
               (entry (or existing `((turn . #f) (preview . "") (emissions . 0))))
               (updated (proc entry))
               (rest (filter (lambda (e) (not (string= (car e) sig)))
                             (loop-guard-seen guard))))
          (set-loop-guard-seen! guard (cons (cons sig updated) rest))))

;;; Masks (mechanism 4b: dynamic tool masking)
(define (loop-guard-active-masks guard)
        "Names currently masked from the advertised tools schema
(mask entries whose expiry turn is still ahead of the loop turn)."
        (map car
             (filter (lambda (m) (> (cdr m) (loop-guard-turn guard)))
                     (loop-guard-masks guard))))

(define (loop-guard-masked? guard name)
        (member name (loop-guard-active-masks guard)))

(define (guard-mask-extend! guard name until-turn)
        (set-loop-guard-masks!
          guard
          (cons (cons name until-turn)
                (filter (lambda (m) (not (equal? (car m) name)))
                        (loop-guard-masks guard)))))

;;;
;;; Mechanism texts
;;;
(define (truncate-string s n)
        (if (and (string? s) (> (string-length s) n))
          (string-append (substring s 0 n) "...")
          (or s "")))

(define (guard-block-message guard tc entry)
        "The message body of the structured ERROR payload returned in the
tool role for a vetoed duplicate call: references the original turn and
embeds a preview of the memoized result so low-parameter models can
recover the information without re-invoking the tool."
        (let* ((name (or (assoc-ref tc 'name) ""))
               (args (assoc-ref tc 'arguments))
               (turn (assoc-ref entry 'turn))
               (preview (assoc-ref entry 'preview))
               (preview-chars (loop-guard-config-ref guard 'preview-chars))
               (args-str (truncate-string (format #f "~a" (or args '())) 160)))
          (format
            #f
            "Execution blocked: identical call to '~a' with args ~a already executed in turn ~a. Reuse that result, or change your arguments or your next action. Result preview from turn ~a: ~s"
            name args-str (or turn "?") (or turn "?")
            (truncate-string preview preview-chars))))

(define %guard-note-marker "[badb loop-guard")

(define %guard-note-text
        "\n\n[badb loop-guard: a later attempt to repeat this exact call was blocked as a duplicate and pruned from this conversation. The output above is still current. Use a different tool, pass different arguments, or answer from this result.]")

(define (guard-reflect-prompt guard)
        "The injected prompt of the single forced tool-less reflection
turn. Deliberately generic: re-naming the looping tool inside the
message would boost its token activation."
        (string-append
          %guard-note-marker
          " tool-loop] Tool calling is temporarily disabled for this one turn: "
          "recent attempts repeated an already-executed call. In plain text only, briefly state: "
          "(1) what information you still need, "
          "(2) why the already-executed calls did not provide it, and "
          "(3) the different action you will take next. "
          "Tool calling is restored on your next turn."))

;;;
;;; Mechanism 1: context surgery via the post-turn hook
;;;
(define (annotate-original-output! leading-msgs sig)
        "Append the harness guardrail note to the content of the FIRST
tool message in LEADING-MSGS that answers SIG's original call, so the
model re-reads the original observation with the deposit note instead of
seeking a repeat. Returns #t when an annotation was applied."
        (let loop ((rest leading-msgs))
                  (cond
                    ((null? rest) #f)
                    ((not (equal? (assoc-ref (car rest) 'role) "assistant"))
                      (loop (cdr rest)))
                    (else
                      (let* ((sig-pairs (assistant-turn-signatures (car rest)))
                             (match (find (lambda (p) (string= (car p) sig)) sig-pairs)))
                        (if (not match)
                          (loop (cdr rest))
                          (let ((cid (cdr match)))
                            (let scan ((rest2 (cdr rest)))
                                      (cond
                                        ((null? rest2) #f)
                                        ((and (equal? (assoc-ref (car rest2) 'role) "tool")
                                              (equal? (assoc-ref (car rest2) 'tool_call_id) cid))
                                          (let* ((m (car rest2))
                                                 (content-pair (assq 'content m)))
                                            (if (and (pair? content-pair)
                                                     (string? (cdr content-pair))
                                                     (not (string-contains (cdr content-pair)
                                                                           "[badb loop-guard")))
                                              (begin
                                                (set-cdr! content-pair
                                                          (string-append (cdr content-pair) %guard-note-text))
                                                #t)
                                              #f)))
                                        ((equal? (assoc-ref (car rest2) 'role) "assistant")
                                          #f)
                                        (else (scan (cdr rest2))))))))))))

(define (guard-surgery! session)
        "Rewrite SESSION's transcript: when the trailing (assistant tool
calls + tool results) block consists entirely of duplicate-blocked
calls, remove that block from history — repeated structural blocks never
accumulate in the KV cache — and annotate the original tool output of
each pruned signature with the harness guardrail note."
        (receive (asst tool-msgs leading) (split-trailing-turn (react-session-messages session))
                 (if (and asst (not (null? tool-msgs)) (every tool-msg-blocked? tool-msgs))
                   (let* ((sig-pairs (and asst (assistant-turn-signatures asst)))
                          (blocked-sigs (map car sig-pairs)))
                     (for-each (lambda (sig) (annotate-original-output! leading sig))
                               blocked-sigs)
                     (set-react-session-messages! session leading)
                     #t)
                   #f)))

;;;
;;; Post-turn bookkeeping & escalation
;;;
(define
  (guard-record-emission! guard sig name blocked? tool-msg turn)
  "Update SEEN and escalation state for one observed emission of SIG
in the trailing turn. Blocked emissions never refresh the memo (the
original turn/preview stay authoritative) but do advance the ladder."
  (let*
      ((config (loop-guard-config guard))
       (preview-chars (or (assoc-ref config 'preview-chars) 400))
       (reflect-after (or (assoc-ref config 'reflect-after) 4))
       (mask-after (or (assoc-ref config 'mask-after) 5))
       (mask-turns (or (assoc-ref config 'mask-turns) 3))
       (halt-after-blocked (or (assoc-ref config 'halt-after-blocked) 8))
       (idempotent? (or (assoc-ref config 'idempotent?) default-idempotent?))
       (raw-content (or (and tool-msg (assoc-ref tool-msg 'content)) "")))
    (guard-seen-update! guard
                        sig
                        (lambda (entry)
                          (let
                              ((emissions (+ 1 (or (assoc-ref entry 'emissions) 0))))
                            (if blocked?
                              `((turn . ,(or (assoc-ref entry 'turn) turn))
                                (preview . ,(or (assoc-ref entry 'preview) ""))
                                (result . ,(assoc-ref entry 'result))
                                (emissions . ,emissions))
                              `((turn . ,turn)
                                (preview . ,(truncate-string raw-content preview-chars))
                                (result . ,raw-content)
                                (emissions . ,emissions))))))
    (let
        ((emissions (loop-guard-emissions guard sig)))
      (when
          (and (>= emissions mask-after) (idempotent? name))
        (guard-mask-extend! guard name (+ turn mask-turns)))
      (when
          (and (>= emissions reflect-after) (< emissions mask-after))
        (set-loop-guard-pending-reflect! guard sig))
      (when (>= (- emissions 1) halt-after-blocked)
        (set-loop-guard-halt-requested?! guard #t)))))

(define (guard-post-turn guard session)
        "Post-turn hook: observe the trailing turn's emissions, escalate
the ladder (masking / forced reflection / halt request), and perform
context surgery on fully-blocked duplicate turns. Mutations invalidate
memoized read/analysis entries so post-edit verification runs fresh;
failed/incomplete invocations are never memoized so deliberate retries
remain allowed."
        (receive (asst tool-msgs leading)
                 (split-trailing-turn (react-session-messages session))
                 (let
                     ((turn (or (react-session-iteration session) 0))
                      (idempotent?
                        (or (loop-guard-config-ref guard 'idempotent?) default-idempotent?)))
                   (set-loop-guard-turn! guard turn)
                   (when asst
                     (let*
                         ((sig-pairs (assistant-turn-signatures asst))
                          (has-mutation?
                            (any
                              (lambda (p)
                                (let*
                                    ((cid (cdr p))
                                     (tool-msg
                                       (find
                                         (lambda (m)
                                           (equal? (assoc-ref m 'tool_call_id) cid))
                                         tool-msgs))
                                     (tname (or (and tool-msg (assoc-ref tool-msg 'name)) "")))
                                  (and (not (string-null? tname))
                                       (not (idempotent? tname))
                                       (not (and tool-msg (tool-msg-blocked? tool-msg)))
                                       (not (and tool-msg (tool-msg-failed? tool-msg))))))
                              sig-pairs)))
                       (if has-mutation?
                         (begin (set-loop-guard-seen! guard '()) (set-loop-guard-masks! guard '()))
                         (begin
                           (for-each
                             (lambda (p)
                               (let*
                                   ((sig (car p)) (cid (cdr p))
                                    (tool-msg
                                      (find
                                        (lambda (m)
                                          (equal? (assoc-ref m 'tool_call_id) cid))
                                        tool-msgs))
                                    (tname (or (and tool-msg (assoc-ref tool-msg 'name)) ""))
                                    (blocked? (and tool-msg (tool-msg-blocked? tool-msg)))
                                    (failed? (and tool-msg (tool-msg-failed? tool-msg))))
                                 (unless failed? (guard-record-emission! guard sig tname blocked? tool-msg turn))))
                             sig-pairs)
                           (let
                               ((surgery-after (or (loop-guard-config-ref guard 'surgery-after) 3)))
                             (when
                                 (and (not (null? tool-msgs))
                                      (every tool-msg-blocked? tool-msgs)
                                      (any
                                        (lambda (p)
                                          (>= (loop-guard-emissions guard (car p)) surgery-after))
                                        sig-pairs))
                               (guard-surgery! session)))
                           (when (loop-guard-halt-requested? guard)
                             (let
                                 ((queue (react-session-steering-queue session)))
                               (when (steering-queue? queue) (steering-push! queue 'interrupt)))))))))))

;;;
;;; Mechanism 2 + 4: completion override (reflection, masking, re-rolls)
(define
  (guard-completion guard base messages tools)
  "Completion hook: filter masked tools out of the advertised
schema, force exactly one tool_choice=\"none\" reflection turn when the
ladder requests it (persisting the reflection exchange plus a trailing
user resume prompt as pre-turns so the outbound history never ends on a
model turn), and apply turn-level rejection sampling."
  (let*
      ((eff-tools (eff-tools-for-step guard tools))
       (reflect-sig (loop-guard-pending-reflect guard))
       (pre-turns '())
       (run-msgs messages))
    (when reflect-sig
      (set-loop-guard-pending-reflect! guard #f)
      (let*
          ((reflect-user `((role . "user") (content . ,(guard-reflect-prompt guard))))
           (refl-msgs (append messages (list reflect-user))))
        (receive (r-content _r-reasoning _r-calls)
                 (base refl-msgs '() #:tool-choice "none")
                 (let
                     ((reflect-assistant
                        `((role . "assistant")
                          (content .
                                   ,(if (string? r-content)
                                      r-content
                                      ""))))
                      (reflect-resume
                        '((role . "user")
                          (content .
                           "Tool calling is restored. Now proceed with the different action you just described."))))
                   (set! pre-turns (list reflect-user reflect-assistant reflect-resume))
                   (set! run-msgs (append refl-msgs (list reflect-assistant reflect-resume)))))))
    ;; Turn-level rejection / best-of-N re-roll
    (let*
        ((roll-temps (cons #f (loop-guard-config-ref guard 'roll-temperatures)))
         (roll-budget (max 1 (or (loop-guard-config-ref guard 'roll-attempts) 3))))
      (let loop
        ((temps roll-temps) (attempt 1))
        (receive (content reasoning tool-calls)
                 (base run-msgs eff-tools #:temperature (car temps))
                 (if
                     (and (find-duplicated-signature guard tool-calls)
                          (> roll-budget attempt)
                          (pair? (cdr temps)))
                   (loop (cdr temps) (+ attempt 1))
                   (values content reasoning tool-calls pre-turns)))))))

(define (find-duplicated-signature guard tool-calls)
        "Return the first signature in TOOL-CALLS that the guard already
memoized (would block), or #f."
        (let ((seen (loop-guard-seen guard)))
          (find (lambda (tc)
                  (and (pair? tc)
                       (assoc (signature-of tc) seen string=)))
                (or tool-calls '()))))

(define (eff-tools-for-step guard tools)
        "TOOLS filtered against active masks (mechanism: dynamic tool
masking — the looping tool's definition disappears from the schema)."
        (let ((masked (loop-guard-active-masks guard)))
          (if (null? masked)
            tools
            (filter (lambda (t)
                      (not (member (tool-spec-name t) masked string=)))
                    tools))))

(define
  (guard-completion guard base messages tools)
  "Completion hook: filter masked tools out of the advertised
schema, force exactly one tool_choice='none' reflection turn when the
ladder requests it (persisting the reflection exchange plus a trailing
user resume prompt as pre-turns so the outbound history never ends on a
model turn), and apply turn-level rejection sampling."
  (let*
      ((eff-tools (eff-tools-for-step guard tools))
       (reflect-sig (loop-guard-pending-reflect guard))
       (pre-turns '())
       (run-msgs messages))
    (when reflect-sig
      (set-loop-guard-pending-reflect! guard #f)
      (let*
          ((reflect-user `((role . "user") (content . ,(guard-reflect-prompt guard))))
           (refl-msgs (append messages (list reflect-user))))
        (receive (r-content _r-reasoning _r-calls)
                 (base refl-msgs '() #:tool-choice "none")
                 (let
                     ((reflect-assistant
                        `((role . "assistant")
                          (content .
                                   ,(if (string? r-content)
                                      r-content
                                      ""))))
                      (reflect-resume
                        '((role . "user")
                          (content .
                           "Tool calling is restored. Now proceed with the different action you just described."))))
                   (set! pre-turns (list reflect-user reflect-assistant reflect-resume))
                   (set! run-msgs (append refl-msgs (list reflect-assistant reflect-resume)))))))
    ;; Turn-level rejection / best-of-N re-roll
    (let*
        ((roll-temps (cons #f (loop-guard-config-ref guard 'roll-temperatures)))
         (roll-budget (max 1 (or (loop-guard-config-ref guard 'roll-attempts) 3))))
      (let loop
        ((temps roll-temps) (attempt 1))
        (receive (content reasoning tool-calls)
                 (base run-msgs eff-tools #:temperature (car temps))
                 (if
                     (and (find-duplicated-signature guard tool-calls)
                          (> roll-budget attempt)
                          (pair? (cdr temps)))
                   (loop (cdr temps) (+ attempt 1))
                   (values content reasoning tool-calls pre-turns)))))))

;;;
;;; Mechanism 3: interception
;;;
(define (guard-intercept guard tc session)
        "Intercept hook ((FN TC SESSION) -> block message | (cached . result) | #f).
A call is vetoed when its canonical signature was already executed earlier in
the current epoch, its tool name classifies as idempotent, and the attempted
emission count has reached the block-after threshold. Static analysis tools
reuse their cached result on early repeats in the same epoch instead of
failing with DuplicateToolCallException. First emissions and mutation tools
always pass through."
        (let*
            ((name (or (assoc-ref tc 'name) ""))
             (idempotent?
               (or (loop-guard-config-ref guard 'idempotent?) default-idempotent?))
             (block-after (or (loop-guard-config-ref guard 'block-after) 2))
             (halt-after (or (loop-guard-config-ref guard 'halt-after-blocked) 8))
             (entry
               (and (string? name)
                    (not (string-null? name))
                    (idempotent? name)
                    (loop-guard-signature-entry guard (signature-of tc)))))
          (if entry
            (let*
                ((meta (cdr entry))
                 (emissions (or (assoc-ref meta 'emissions) 0))
                 (next-emissions (+ 1 emissions))
                 (cached-res (assoc-ref meta 'result)))
              (cond
                ((and (static-analysis-tool? name)
                      (string? cached-res)
                      (not (string-null? cached-res))
                      (< emissions halt-after))
                  (cons 'cached cached-res))
                ((>= next-emissions block-after) (guard-block-message guard tc meta))
                (else #f)))
            #f)))

;;;
;;; Hook bundle
;;;
(define (make-loop-guard-hooks guard)
        "Session hooks wiring GUARD into a runtimes-react session:

  (make-react-session #:hooks (append (make-loop-guard-hooks (make-loop-guard))
                                      (list ...existing hooks...)))

Returns an alist with 'intercept-tool-call, 'completion, and 'post-turn
entries. These keys do not collide with the standard hook keys
(guardrail, on-step, on-tool-call, on-tool-result, on-delta); this
alist must be appended BEFORE the session's own hooks so user hooks
keep their existing keys."
        `((intercept-tool-call . ,(lambda (tc session) (guard-intercept guard tc session)))
          (completion . ,(lambda (base messages tools) (guard-completion guard base messages tools)))
          (post-turn . ,(lambda (session) (guard-post-turn guard session)))))
