;;; badb — Terminal User Interface (TUI)
;;;
;;; Interactive full-screen terminal interface for badb powered by
;;; notcurses, bound through the (geasa surfaces-tui nc) Guile FFI module.
;;; Provides live streaming LLM chat with separated thought reasoning blocks,
;;; modal prompt switching (! for shell, ( or , for Scheme, backspace to return),
;;; interactive slash-command selector palette, menu-driven configuration
;;; flows (/config, /model, /endpoint open interactive menus), and a quiet
;;; modern agent-CLI layout: flat borderless message blocks with role labels
;;; and accent gutters, a slim header line over a thin rule, and floating
;;; rounded pop-ups. All colors resolve through the (geasa surfaces-theme plugin) module, so
;;; the six built-in themes restyle the whole interface. Message blocks are
;;; collapsible: reasoning collapses to a single summary line by default;
;;; Ctrl+Up/Ctrl+Down select a block and Ctrl+E toggles collapse. While the
;;; autonomous ReAct loop runs, the terminal is polled non-blocking so the
;;; user can type steering messages (Enter sends, Ctrl+I interrupts) that
;;; the running agent sees at the next iteration boundary.

(define-module (geasa surfaces-tui plugin)
               #:use-module (geasa providers-model common)
               #:use-module (geasa providers-model openai)
               #:use-module (badb kernel registry)
               #:use-module (badb kernel transaction)
               #:use-module (badb kernel journal)
               #:use-module (badb kernel geis-gate)
               #:use-module (badb geis bundle)
               #:use-module (badb kernel digest)
               #:use-module (geasa foundation-config plugin)
               #:use-module (geasa capabilities-mcp plugin)
               #:use-module (geasa capabilities-history-tools plugin)
               #:use-module (geasa runtimes-react plugin)
               #:use-module (geasa policies-guardrail plugin)
               #:use-module (geasa runtimes-context plugin)
               #:use-module (geasa runtimes-prompt plugin)
               #:use-module (geasa runtimes-rollback plugin)
               #:use-module (geasa capabilities-context-index plugin)
               #:use-module (geasa capabilities-skills plugin)
               #:use-module (geasa capabilities-code-nav plugin)
               #:use-module (geasa runtimes-geis-loader plugin)
               #:use-module (geasa runtimes-workflows plugin)
               #:use-module (geasa runtimes-verify plugin)
               #:use-module (geasa surfaces-theme plugin)
               #:use-module (geasa foundation-log plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa surfaces-tui editor)
               #:use-module (geasa surfaces-tui session)
               #:use-module (geasa surfaces-tui transcript)
               #:use-module (geasa surfaces-tui menus)
               #:use-module (geasa surfaces-tui tools)
               #:use-module (geasa surfaces-tui commands)
               #:use-module (json)
               #:use-module (ice-9 format)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 optargs)
               #:use-module (ice-9 regex)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:use-module (srfi srfi-11)
               #:use-module (srfi srfi-19)
               #:export (run-tui)
               #:re-export (format-conversation-transcript
                             wrap-line
                             wrap-text
                             parse-command
                             classify-input
                             make-repl-mod
                             normalize-scheme-expr
                             eval-scheme-code
                             execute-shell-command
                             split-thoughts-and-content
                             %slash-commands
                             matching-slash-commands
                             detect-model-decision
                             build-model-menu-items
                             selector-active?
                             %selector-max-visible
                             selector-visible-slice
                             %menu-max-visible
                             make-menu
                             menu?
                             menu-title
                             menu-items
                             menu-selected-index
                             menu-count
                             menu-move
                             menu-selected-item
                             menu-visible-slice
                             menu-item-label
                             menu-item-description
                             menu-item-action
                             get-prompt-indicator
                             format-header-segments
                             format-rule-line
                             format-keymap-line
                             steering-key-handle
                             steering-pending-line
                             agent-run-active?
                             segment-line
                             clamp-cursor
                             insert-at
                             backspace-at
                             delete-at
                             input-visible-window
                             input-wrapped-rows
                             %max-input-rows
                             make-row-cache
                             row-cache?
                             row-cache-rows
                             row-cache-prune!
                             make-history
                             input-history?
                             %history-max-entries
                             history-cap-entries
                             history-entries
                             history-length
                             history-add
                             history-recall
                             history-recall-position
                             history-reset-nav
                             history-entry->jsonl-line
                             jsonl-line->history-entry
                             default-history-file-path
                             load-history-file
                             append-history-file-entry!
                             turn->jsonl-line
                             default-sessions-dir
                             session-file-path
                             persist-session-file!
                             load-session-file
                             recent-session-files
                             message-collapsed?
                             make-collapsed-message
                             message-with-collapsed
                             message-toggle-collapsed
                             default-collapsed-role?
                             message-collapsible?
                             default-collapse-message
                             thoughts-preview
                             format-collapsed-summary-line
                             message-role-label
                             message-role->theme-role
                             system-message-label
                             color-symbol->ncurses-constant
                             key->code
                             resolve-modified-key
                             spec-tool-lock
                             spec-tool-verify
                             spec-tool-status
                             register-spec-tools!
                             build-geis-add-prompt
                             format-message-entry
                             <input-history>
                             <row-cache>
                             <menu>
                             builtin-read-file
                             jsonl-line->turn
                             truncate-string))

;;;
;;; Main TUI Controller (run-tui)
;;;
;; DEV-DEBT [complexity]: run-tui is the largest form in the repo (9209 AST nodes,
;; cyclomatic ~447 per ast_complexity_metrics). Covered by the 582-assertion bundle
;; spec, but the prime candidate for decomposition into smaller units.
(define*
  (run-tui #:key
           (endpoint #f)
           (model #f)
           (api-key #f)
           (journal-path #f)
           (config #f))
  "Launch the interactive badb curses TUI."
  (unless
      (catch #t
        (lambda ()
          (false-if-exception (resolve-interface '(geasa surfaces-tui nc)))
          #t)
        (lambda _
          #f))
    (format (current-error-port)
            "Error: (geasa surfaces-tui nc) notcurses backend is not available.~%~
             Please run badb within the Guix dev environment:~%~
               ./scripts/devenv -- ./scripts/badb~%")
    (exit 1))
  (let*
      ((ncurses-mod
         (or
           (false-if-exception (resolve-module '(geasa surfaces-tui nc)))
           (resolve-module '(geasa surfaces-tui nc))))
       (nref
         (lambda (s)
           (module-ref ncurses-mod s)))
       (initscr (nref 'initscr))
       (endwin (nref 'endwin))
       (cbreak! (nref 'cbreak!))
       (noecho! (nref 'noecho!))
       (keypad! (nref 'keypad!))
       (curs-set (nref 'curs-set))
       (erase (nref 'erase))
       (refresh (nref 'refresh))
       (getmaxy (nref 'getmaxy))
       (getmaxx (nref 'getmaxx))
       (move (nref 'move))
       (addstr (nref 'addstr))
       (getch (nref 'getch))
       (attr-on! (nref 'attr-on!))
       (attr-off! (nref 'attr-off!))
       (has-colors? (nref 'has-colors?))
       (start-color! (nref 'start-color!))
       (init-pair! (nref 'init-pair!))
       (color-pair (nref 'color-pair))
       (A_BOLD (nref 'A_BOLD))
       (A_REVERSE (nref 'A_REVERSE))
       (A_DIM (nref 'A_DIM))
       (KEY_ENTER (nref 'KEY_ENTER))
       (KEY_BACKSPACE (nref 'KEY_BACKSPACE))
       (KEY_UP (nref 'KEY_UP))
       (KEY_DOWN (nref 'KEY_DOWN))
       (KEY_PPAGE (nref 'KEY_PPAGE))
       (KEY_NPAGE (nref 'KEY_NPAGE))
       (KEY_LEFT (nref 'KEY_LEFT))
       (KEY_RIGHT (nref 'KEY_RIGHT))
       (KEY_HOME (nref 'KEY_HOME))
       (KEY_END (nref 'KEY_END))
       (KEY_DC (nref 'KEY_DC))
       (KEY_RESIZE (nref 'KEY_RESIZE))
       (key-defined (nref 'key-defined))
       (define-key (nref 'define-key))
       (beep (nref 'beep))
       (cursor-shape (nref 'cursor-shape))
       (drain-mouse-events (nref 'drain-mouse-events))
       ;; Paste-burst API from the nc backend (with graceful fallbacks:
       ;; without them input-pending? is always #f and paste bursts are
       ;; simply not collected — typing still works).
       (input-pending?
         (or
           (catch #t
             (lambda ()
               (nref 'input-pending?))
             (lambda _
               #f))
           (lambda _
             #f)))
       (drain-input-burst!
         (or
           (catch #t
             (lambda ()
               (nref 'drain-input-burst!))
             (lambda _
               #f))
           (lambda ()
             '())))
       (KEY_FIND (nref 'KEY_FIND))
       (KEY_OPEN (nref 'KEY_OPEN))
       ;; Non-blocking input support: the (geasa surfaces-tui nc) backend exports nodelay!
       ;; (kept also as the alias nodelay). When neither resolves, mid-run
       ;; polling is disabled and steering stays available between runs
       ;; through /steer and /interrupt.
       (nodelay-proc
         (or
           (catch #t
             (lambda ()
               (nref 'nodelay!))
             (lambda _
               #f))
           (catch #t
             (lambda ()
               (nref 'nodelay))
             (lambda _
               #f))))
       (steering-err-val
         (catch #t
           (lambda ()
             (nref 'ERR))
           (lambda _
             -1)))
       (ctrl-up-key 0)
       (ctrl-down-key 0))
    ;; Initialize Configuration & Kernel State
    (define cfg (or config (load-config)))
    ;; Resolve the active UI theme from the configuration, falling back to
    ;; the default theme; every draw site resolves colors through its roles.
    (define ui-theme
            (let ((configured (badb-config-ui-theme cfg)))
              (or (and (string? configured) (lookup-theme configured)) (default-theme))))
    (define cur-endpoint
            (or endpoint (getenv "BADB_MODEL_ENDPOINT") (badb-config-model-endpoint cfg)))
    (define cur-model
            (or model (getenv "BADB_MODEL_NAME") (badb-config-model-name cfg)))
    (define cur-api-key
            (or api-key
                (getenv "BADB_API_KEY")
                (getenv "OPENAI_API_KEY")
                (badb-config-model-api-key cfg)))
    (define cur-journal-path
            (or journal-path (getenv "BADB_JOURNAL_PATH") (badb-config-journal-path cfg)))
    (define reg (make-registry))
    (define jrn (make-journal cur-journal-path))
    (define mgr (make-transaction-manager reg #:journal jrn))
    (define mcp-mgr (make-mcp-manager cfg))
    (define policy (make-guardrail-policy #:level 'cautious))
    (define compactor
            (make-context-compactor #:max-tokens
                                    16384
                                    #:reserve-tokens
                                    2048
                                    #:strategy
                                    (strategy-cascading #:keep-recent-turns
                                                        6
                                                        #:max-tool-chars
                                                        1200
                                                        #:pin-first-user
                                                        #t)
                                    #:watermark
                                    0.75))
    (define rollback-mgr
            (make-rollback-manager #:workspace-dir (getcwd) #:registry reg #:journal jrn))
    (define verifier (make-verifier #:check-syntax? #t #:run-tests? #f))
    (define composer (make-prompt-composer))
    (define client
            (make-model-client #:endpoint
                               cur-endpoint
                               #:api-key
                               cur-api-key
                               #:model
                               cur-model
                               #:allow-missing-thinking?
                               (badb-config-allow-missing-thinking? cfg cur-endpoint)))
    (define ctx-idx (make-bm25-index))
    (define skill-reg (make-skill-registry))
    (catch #t
      (lambda ()
        (discover-skills! skill-reg)
        (discover-and-mount-project-geasa! reg #:workspace (getcwd))
        (mount-bundle-workflows! reg #:workspace (getcwd)))
      (lambda _ #f))
    ;; Register Built-in Autonomous Agent Tools & Auto-start MCP servers
    (define llm-history '())
    (register-builtin-tui-tools! reg
                                 rollback-mgr
                                 verifier
                                 mcp-mgr
                                 (lambda ()
                                   llm-history)
                                 #:ctx-idx ctx-idx
                                 #:skill-reg skill-reg
                                 #:client client
                                 #:config cfg
                                 #:policy policy)
    (auto-start-mcp-servers! cfg mcp-mgr)
    (define (semantic-summarize-fn older-msgs)
            "Summarize OLDER-MSGS into the pinned schema state block via the
configured model client. Derives only from raw turns, keeps identifiers
verbatim, and fails open to a verbatim truncation."
            (let*
                ((rendered
                   (string-join
                     (map
                       (lambda (m)
                         (format #f "~a: ~a" (or (assoc-ref m 'role) "?") (or (assoc-ref m 'content) "")))
                       older-msgs)
                     "\n"))
                 (truncate-to
                   (lambda (s n)
                     (if (> (string-length s) n)
                       (string-append (substring s 0 n) "\n[...verbatim fallback truncated...]")
                       s))))
              (catch #t
                (lambda ()
                  (let*
                      ((sys-msg
                         `((role . "system")
                           (content .
                                    "You are a context-compaction engine. Produce a state block with EXACTLY these sections: Pinned Constraints; Verified State / File Map; Rejected Approaches; Exact Identifiers. Keep identifiers, file paths, hashes and error codes verbatim. Never claim work completed without evidence.")))
                       (req-msg
                         `((role . "user")
                           (content .
                                    ,(format #f
                                             "Summarize the following ~a conversation turns into the schema state block:\n\n~a"
                                             (length older-msgs)
                                             rendered))))
                       (resp (model-complete client (list sys-msg req-msg) #:max-tokens 1024)))
                    (if
                        (and (string? resp) (not (string-null? (string-trim-both resp))))
                      (format #f "[Context Summary of preceding turns]:\n~a" resp)
                      (truncate-to
                        (format #f "[Context Summary of preceding turns]:\n~a" rendered)
                        3000))))
                (lambda (k . a)
                  (truncate-to
                    (format #f "[Context Summary of preceding turns]:\n~a" rendered)
                    3000)))))
    (set-context-compactor-strategy! compactor
                                     (strategy-cascading #:keep-recent-turns
                                                         6
                                                         #:max-tool-chars
                                                         1200
                                                         #:pin-first-user
                                                         #t
                                                         #:summarize-fn
                                                         semantic-summarize-fn))
    (define discovered-context-limit
            (catch #t
              (lambda ()
                (fetch-model-context-limit client))
              (lambda _
                #f)))
    (when
        (and (integer? discovered-context-limit) (> discovered-context-limit 4096))
      (set-context-compactor-max-tokens! compactor discovered-context-limit))
    (define repl-mod
            (make-repl-mod reg
                           jrn
                           mgr
                           client
                           cfg
                           mcp-mgr
                           policy
                           compactor
                           rollback-mgr
                           verifier
                           composer))
    ;; UI State
    (define (make-system-turn)
            `((role . "system")
              (content .
                       ,(assemble-system-prompt composer
                                                #:workspace-dir (getcwd)
                                                #:tools
                                                (collect-available-tools reg mcp-mgr)
                                                #:skills
                                                (skill-catalog-summary skill-reg)
                                                #:memories
                                                (catch #t (lambda () (memory-read "all")) (lambda _ #f))
                                                #:static-only? #f))))
    ;; The system prompt stays in llm-history for the model but is not
    ;; displayed as a transcript card.
    (define messages
            (list
              `((role . "notice")
                (content .
                         ,(string-append "badb microharness\n"
                                         "• ! shell · ( scheme · / commands · /help for keys"
                                         (if (file-exists? ".badb/geasa")
                                           ""
                                           "\n• Tip: run /geis onboard to detect project toolchain & synthesize geasa"))))))
    (set! llm-history (list (make-system-turn)))
    ;; Session log for /resume: the whole llm-history is rewritten to
    ;; this JSONL file after every chat turn (system prompt first).
    (define session-file (session-file-path))
    ;; Per-session token accounting for /usage: exact totals accumulate
    ;; from endpoint-reported usage blocks; turns that report none fall
    ;; back to a context-growth estimate tracked separately and labeled
    ;; "estimated" on the card.
    (define session-prompt-tokens 0)
    (define session-completion-tokens 0)
    (define session-estimated-tokens 0)
    (define session-turns 0)
    (define turn-usage-seen? #f)
    (define turn-pre-estimate 0)
    (define input-buffer "")
    (define cursor-pos 0)
    (define input-history
            (make-history (load-history-file (default-history-file-path))))
    (define history-draft "")
    (define history-draft-pos 0)
    (define scroll-offset 0)
    (define max-scroll-seen 0)
    (define status-text "READY")
    (define selector-index 0)
    ;; Active interactive menu (a <menu> record or #f). While set it renders
    ;; as a floating pop-up and takes key priority: Up/Down move, Enter
    ;; applies the selected item, Esc closes.
    (define active-menu #f)
    ;; Viewport message selection index (#f = inactive) for collapse control.
    ;; Key bindings (single control chars, non-conflicting with existing codes):
    ;;   Ctrl+Up / Ctrl+Down = select previous / next message card; the
    ;;                      codes come from terminfo, resolved at startup
    ;;                      with a define-key fallback on bare entries, so
    ;;                      plain Up/Down keep scrolling the transcript
    ;;   Ctrl+E (char 5)  = toggle collapse on the selected message
    ;;                      (falls back to cursor-to-end when no selection,
    ;;                      preserving the pre-existing Ctrl+E binding)
    (define msgsel-index #f)
    (define prompt-mode 'chat)
    (define stdscr #f)
    (define colors-enabled? #f)
    ;; Signature of the most recent surfaced render error, so repeat
    ;; failures are reported once instead of flooding the transcript.
    (define last-render-error-sig #f)
    ;; Live-stream state for the ReAct loop: accumulated deltas of the
    ;; in-flight model call and the timestamp of the last streamed paint.
    (define react-stream-content "")
    (define react-stream-thoughts "")
    (define react-stream-last-render 0)
    ;; Incremental rendering: formatted block rows are memoized per
    ;; message object so each frame only formats new messages.
    (define row-cache (make-row-cache))
    (define asm-messages #f)
    (define asm-width #f)
    (define asm-lines '())
    (define asm-counts '())
    (define asm-total 0)
    (define asm-msg-count 0)
    ;; Mouse hit map: screen row -> click target, rebuilt by every
    ;; render. Transcript rows map to their owning message index;
    ;; popup rows map to ('menu-item . ABS-INDEX) or
    ;; ('palette-item . INDEX); handle-mouse-event! dispatches on it.
    (define %click-targets (make-hash-table 61))
    ;; Agent steering state: the queue is shared with every react session
    ;; and persists across runs (and /clear, which only resets the
    ;; transcript) so /steer and /interrupt can pre-queue entries between
    ;; runs — react-run! drains before its first LLM call too. The buffer
    ;; holds text typed mid-run; interrupt-requested? drives the pending
    ;; pill until a run consumes the queued 'interrupt.
    (define steering-queue (make-steering-queue))
    (define steering-buffer "")
    (define interrupt-requested? #f)
    ;; Message appending helpers
    (define (add-msg! role content)
            (set! messages (append messages (list `((role . ,role) (content . ,content))))))
    (define (add-sys-msg! content) (add-msg! "system" content))
    (define (add-err-msg! content)
            (log-error!
              (if (string? content)
                content
                (format #f "~s" content))
              "tui")
            (add-msg! "error" content))
    (define (add-cmd-msg! content) (add-msg! "shell-cmd" content))
    (define (add-scm-msg! content) (add-msg! "scheme-in" content))
    (define (add-user-msg! content) (add-msg! "user" content))
    (define (record-session-usage! usage)
            "Accumulate one model call's USAGE alist ((prompt . N) (completion . N))
into the session counters and mark the current turn exactly accounted."
            (let
                ((p (assoc-ref usage 'prompt)) (c (assoc-ref usage 'completion)))
              (when (and (number? p) (number? c))
                (set! session-prompt-tokens (+ session-prompt-tokens p))
                (set! session-completion-tokens (+ session-completion-tokens c))
                (set! turn-usage-seen? #t))))
    ;; Poll the terminal without blocking while the react loop runs: drain
    ;; every pending keypress into the steering buffer / queue. Called from
    ;; the react hooks (on-step, on-tool-call, on-tool-result) so input
    ;; typed mid-run is captured at iteration boundaries. Requires nodelay
    ;; support; without it this is a no-op and /steer between runs is the
    ;; fallback. Escape sequences from arrow keys may bleed in as stray
    ;; characters when only partially arrived — accepted for simplicity.
    (define (poll-steering-input!)
            (when (and nodelay-proc stdscr)
              (let poll-loop
                ()
                (let
                    ((ch
                       (catch #t
                         (lambda ()
                           (getch stdscr))
                         (lambda _
                           #f))))
                  (when
                      (and ch (not (and (integer? ch) (or (= ch -1) (eqv? ch steering-err-val)))))
                    (let
                        ((norm-ch
                           (cond
                             ((eqv? ch KEY_ENTER) 10)
                             ((eqv? ch KEY_BACKSPACE) 127)
                             (else ch))))
                      (call-with-values
                        (lambda ()
                          (steering-key-handle steering-buffer norm-ch))
                        (lambda (new-buffer action)
                          (let ((prev-buffer steering-buffer))
                            (set! steering-buffer new-buffer)
                            (case action
                              ((queue)
                                (let ((text (string-trim-both prev-buffer)))
                                  (steering-push! steering-queue text)
                                  (add-msg! "steer" (format #f "Steering queued: ~a" text))))
                              ((interrupt)
                                (steering-push! steering-queue 'interrupt)
                                (set! interrupt-requested? #t)
                                (add-msg! "steer"
                                          "Interrupt requested — halting at the next iteration boundary."))
                              (else #f)))
                          (poll-loop)))))))))
    ;; Toggle collapse of the currently selected message card. Selection may
    ;; point at any message, but only collapsible ones (with content or
    ;; thoughts) change state; the toggle itself is a pure alist update.
    (define (toggle-selected-collapse!)
            (let*
                ((idx (min msgsel-index (- (length messages) 1))) (msg (list-ref messages idx)))
              (when (message-collapsible? msg)
                (set! messages (replace-nth messages idx (message-toggle-collapsed msg))))))
    (define (record-history! input mode)
            "Record a submitted INPUT under MODE in the shared input history
and append it to the persistent history journal file."
            (let
                ((trimmed
                   (string-trim-both
                     (if (string? input)
                       input
                       ""))))
              (when (history-add input-history trimmed)
                (append-history-file-entry! (default-history-file-path) trimmed mode))))
    (define (try-save-config!)
            (catch #t
              (lambda ()
                (save-local-config! cfg))
              (lambda _
                #f)))
    (define (refresh-client!)
            (set! client
                  (make-model-client #:endpoint
                                     cur-endpoint
                                     #:api-key
                                     cur-api-key
                                     #:model
                                     cur-model
                                     #:allow-missing-thinking?
                                     (badb-config-allow-missing-thinking? cfg cur-endpoint)))
            (module-define! repl-mod 'client client))
    ;; Startup model auto-detection: when the active model is a generic
    ;; placeholder (#f, empty or "default"), ask the endpoint's /models
    ;; listing to pick one and adopt it. The request is wrapped in catch so
    ;; a dead endpoint never blocks startup, and a specific configured model
    ;; skips detection entirely.
    (let
        ((config-model-was-generic? (generic-model-name? (badb-config-model-name cfg))))
      (call-with-values
        (lambda ()
          (if (generic-model-name? cur-model)
            (catch #t
              (lambda ()
                (auto-detect-model client))
              (lambda (key . args)
                (values #f (format #f "Detection error (~a): ~s" key args))))
            (values #f #f)))
        (lambda (detected-model detection-error)
          (call-with-values
            (lambda ()
              (detect-model-decision cur-model detected-model detection-error))
            (lambda (new-model kind)
              (case kind
                ((adopted) (set! cur-model new-model)
                           (badb-config-set-model-name! cfg new-model)
                           (refresh-client!)
                           (when config-model-was-generic? (try-save-config!))
                           (add-msg! "notice" (format #f "model auto-detected: ~a" new-model)))
                ((failed)
                  (add-msg! "notice" (format #f "model detection failed: ~a" detection-error)))
                ((unavailable)
                  (add-msg! "notice" "model detection found no models on the endpoint"))
                (else #f)))))))
    ;; Theme-driven color engine: each unique (fg bg) spec of the active UI
    ;; theme is memoized to one ncurses color pair on first use, so all draw
    ;; sites resolve colors through theme roles instead of hardcoded pairs.
    ;; Theme attribute specs (bold / dim / reverse) apply even on terminals
    ;; without color support, keeping the monochrome fallback meaningful.
    (define role-pair-cache '())
    (define next-pair-id 1)
    (define %max-color-pairs 64)
    (define (theme-attr-constant attr)
            (case attr
              ((bold) A_BOLD)
              ((dim) A_DIM)
              ((reverse) A_REVERSE)
              (else #f)))
    (define (role-attrs role)
            (let ((a (caddr (theme-ref ui-theme role))))
              (cond
                ((memq a '(bold dim reverse)) (list a))
                ((list? a) a)
                (else '()))))
    ;; Theme colors arrive as named curses colors (resolved against the
    ;; backend module) or as #rrggbb hex strings (resolved to raw RGB
    ;; integers the backend accepts directly).
    (define (theme-color->backend-value color)
            (if (string? color)
              (string->number (substring color 1) 16)
              (nref (color-symbol->ncurses-constant color))))
    (define (ensure-color-pair! fg bg)
            (let ((key (cons fg bg)))
              (or (assoc-ref role-pair-cache key)
                  (and (< next-pair-id %max-color-pairs)
                       (let ((id next-pair-id))
                         (catch #t
                           (lambda ()
                             (init-pair! id (theme-color->backend-value fg) (theme-color->backend-value bg)))
                           (lambda _
                             #f))
                         (set! role-pair-cache (acons key id role-pair-cache))
                         (set! next-pair-id (+ next-pair-id 1))
                         id))
                  0)))
    (define (draw-role! role on?)
            (let
                ((spec (theme-ref ui-theme role)) (attrs (role-attrs role)))
              (define (pair-off?)
                      (and colors-enabled?
                           (not (and (eq? (car spec) 'default) (eq? (cadr spec) 'default)))))
              (if on?
                (begin
                  (when (pair-off?)
                    (attr-on! stdscr (color-pair (ensure-color-pair! (car spec) (cadr spec)))))
                  (for-each
                    (lambda (a)
                      (let ((c (theme-attr-constant a))) (when c (attr-on! stdscr c))))
                    attrs))
                (begin
                  (for-each
                    (lambda (a)
                      (let ((c (theme-attr-constant a))) (when c (attr-off! stdscr c))))
                    attrs)
                  (when (pair-off?)
                    (attr-off! stdscr (color-pair (ensure-color-pair! (car spec) (cadr spec)))))))))
    (define (safe-addstr win y x str max-len)
            (when (and (>= y 0) (>= x 0) (> max-len 0))
              (let
                  ((safe-str
                     (if (> (string-length str) max-len)
                       (substring str 0 max-len)
                       str)))
                (catch #t
                  (lambda ()
                    (move win y x)
                    (addstr win safe-str))
                  (lambda _
                    #f)))))
    ;; Render a block body text with semantic token highlights: role-tinted
    ;; rows (error / user / thought) draw as a single chunk, every other role
    ;; runs the segment engine so status pills, bullets and key labels pick
    ;; up their theme accents inside plain text.
    (define
      (draw-styled-interior y start-x text inner-w theme-role)
      (let*
          ((text-len (string-length text))
           (pad-len (max 0 (- inner-w text-len)))
           (padded
             (if (> pad-len 0)
               (string-append text (make-string pad-len #\space))
               (if (> text-len inner-w)
                 (substring text 0 inner-w)
                 text))))
        (define (draw-simple) (draw-role! theme-role #t)
                (safe-addstr stdscr y start-x padded inner-w)
                (draw-role! theme-role #f))
        (cond
          ((memq theme-role '(error user thought)) (draw-simple))
          (else
            (let loop
              ((segs (segment-line padded)) (cur-x start-x))
              (unless
                  (or (null? segs) (>= cur-x (+ start-x inner-w)))
                (let*
                    ((seg (car segs)) (str (car seg))
                     (seg-role (cadr seg))
                     (bold? (caddr seg))
                     (dim? (cadddr seg))
                     (len (min (string-length str) (max 0 (- (+ start-x inner-w) cur-x)))))
                  (when (> len 0)
                    (draw-role! seg-role #t)
                    (when bold? (attr-on! stdscr A_BOLD))
                    (when dim? (attr-on! stdscr A_DIM))
                    (safe-addstr stdscr y cur-x str len)
                    (when dim? (attr-off! stdscr A_DIM))
                    (when bold? (attr-off! stdscr A_BOLD))
                    (draw-role! seg-role #f))
                  (loop (cdr segs) (+ cur-x len)))))))))
    ;; Left accent gutter shared by every row of a message block: a thin
    ;; role-tinted bar, upgraded to a strong 2-char bar when SELECTED?.
    (define (draw-gutter! y theme-role selected?)
            (draw-role! theme-role #t)
            (when selected? (attr-on! stdscr A_BOLD))
            (safe-addstr stdscr
                         y
                         0
                         (if selected?
                           "██"
                           "▏ ")
                         2)
            (when selected? (attr-off! stdscr A_BOLD))
            (draw-role! theme-role #f))
    ;; Draw a single flat block row (label, body, summary, or spacer).
    (define
      (draw-block-line y entry content-width selected?)
      (let ((kind (car entry)))
        (cond
          ((string=? kind "spacer") #t)
          ((string=? kind "block-label")
            (let*
                ((label (cadr entry)) (theme-role (caddr entry)))
              (draw-gutter! y theme-role selected?)
              (draw-role! theme-role #t)
              (let*
                  ((label-str
                     (if (> (string-length label) content-width)
                       (substring label 0 content-width)
                       label))
                   (pad-len (max 0 (- content-width (string-length label-str))))
                   (padded (string-append label-str (make-string pad-len #\space))))
                (safe-addstr stdscr y 2 padded content-width))
              (draw-role! theme-role #f)))
          ((string=? kind "block-body")
            (let*
                ((theme-role (cadr entry)) (text (caddr entry)))
              (draw-gutter! y theme-role selected?)
              (draw-styled-interior y 2 text content-width theme-role)))
          ;; Collapsed-message summary row: role-tinted chevron
          ;; followed by a single dim line (label, count, expand hint)
          ((string=? kind "block-summary")
            (let*
                ((theme-role (cadr entry)) (text (caddr entry)))
              (draw-gutter! y theme-role selected?)
              (draw-role! theme-role #t)
              (safe-addstr stdscr y 2 "› " 2)
              (draw-role! theme-role #f)
              (draw-role! 'dim #t)
              (let*
                  ((text-len (string-length text))
                   (avail (max 0 (- content-width 2)))
                   (text-str
                     (if (> text-len avail)
                       (substring text 0 avail)
                       text))
                   (pad-len (max 0 (- avail (string-length text-str))))
                   (padded (string-append text-str (make-string pad-len #\space))))
                (safe-addstr stdscr y 4 padded avail))
              (draw-role! 'dim #f))))))
    ;; Draw a floating rounded pop-up (slash selector palette, menus): an
    ;; accent-colored border, dim descriptions, and a bold selected row
    ;; (reversed on terminals without colors). ROWS is a list of
    ;; (label . description); SEL-POS is the selected index within ROWS or a
    ;; position outside them for no highlight; ABOVE-COUNT and BELOW-COUNT
    ;; drive the scroll indicators in the borders.
    (define
      (draw-popup! title rows sel-pos box-w start-y start-x above-count below-count)
      (let*
          ((inner-w (- box-w 4)) (vis-count (length rows))
           (label-w
             (fold
               (lambda (r acc)
                 (max acc (string-length (car r))))
               0
               rows))
           (top-tag
             (string-append "╭─ "
                            title
                            (if (> above-count 0)
                              (format #f " ↑~a " above-count)
                              " ")))
           (top-str
             (string-append top-tag
                            (make-string (max 0 (- box-w (string-length top-tag) 1)) #\─)
                            "╮"))
           (bot-tag
             (if (> below-count 0)
               (format #f "╰─ ↓ ~a more " below-count)
               "╰"))
           (bot-str
             (string-append bot-tag
                            (make-string (max 0 (- box-w (string-length bot-tag) 1)) #\─)
                            "╯")))
        ;; Blank the rectangle behind the pop-up first: without this,
        ;; underlying message text shows through between the drawn
        ;; fragments (a clear rendering error on busy screens).
        (let blank-rows
          ((by start-y) (bi 0))
          (when (< bi (+ vis-count 2))
            (safe-addstr stdscr by start-x (make-string box-w #\space) box-w)
            (blank-rows (+ by 1) (+ bi 1))))
        (draw-role! 'accent #t)
        (safe-addstr stdscr start-y start-x top-str box-w)
        (draw-role! 'accent #f)
        (let loop
          ((items rows) (idx 0))
          (unless (null? items)
            (let*
                ((label (caar items)) (desc (cdar items))
                 (is-sel? (= idx sel-pos))
                 (row-y (+ start-y 1 idx))
                 (cursor
                   (if is-sel?
                     "❯ "
                     "  "))
                 (label-pad (make-string (max 1 (- label-w (string-length label))) #\space))
                 (content (string-append cursor label label-pad " " desc))
                 (clipped
                   (if (> (string-length content) inner-w)
                     (substring content 0 inner-w)
                     content))
                 (pad (make-string (max 0 (- inner-w (string-length clipped))) #\space))
                 (row-text (string-append "│ " clipped pad " │")))
              (if is-sel?
                (begin (draw-role! 'selection #t)
                       (attr-on! stdscr A_BOLD)
                       (unless colors-enabled? (attr-on! stdscr A_REVERSE))
                       (safe-addstr stdscr row-y start-x row-text box-w)
                       (unless colors-enabled? (attr-off! stdscr A_REVERSE))
                       (attr-off! stdscr A_BOLD)
                       (draw-role! 'selection #f))
                (begin (draw-role! 'accent #t)
                       (safe-addstr stdscr row-y start-x "│ " 2)
                       (draw-role! 'accent #f)
                       (draw-role! 'default #t)
                       (safe-addstr stdscr
                                    row-y
                                    (+ start-x 2)
                                    (string-append cursor label label-pad)
                                    (+ 2 (string-length cursor) (string-length label) (string-length label-pad)))
                       (draw-role! 'default #f)
                       (draw-role! 'dim #t)
                       (let*
                           ((desc-x (+ start-x 2 (string-length cursor) label-w 1))
                            (desc-len (min (string-length desc) (max 0 (- (+ start-x box-w -2) desc-x)))))
                         (when (> desc-len 0) (safe-addstr stdscr row-y desc-x desc desc-len))
                         (draw-role! 'dim #f))
                       (draw-role! 'accent #t)
                       (safe-addstr stdscr row-y (+ start-x box-w -2) " │" 2)
                       (draw-role! 'accent #f)))
              (loop (cdr items) (+ idx 1)))))
        (draw-role! 'accent #t)
        (safe-addstr stdscr (+ start-y 1 vis-count) start-x bot-str box-w)
        (draw-role! 'accent #f)))
    ;; Render entire screen
    (define (render!)
            (catch #t
              (lambda ()
                (let*
                    ((maxy (getmaxy stdscr)) (maxx (getmaxx stdscr))
                     (avail-width (max 1 (- maxx 3)))
                     (input-layout
                       (call-with-values
                         (lambda ()
                           (input-wrapped-rows input-buffer cursor-pos avail-width %max-input-rows))
                         list))
                     (input-lines (car input-layout))
                     (input-cur-row (cadr input-layout))
                     (input-cur-col (caddr input-layout))
                     (input-rows (length input-lines))
                     (prompt-top (- maxy 2 (- input-rows 1)))
                     (view-top 2)
                     (view-bottom (- maxy 3 (- input-rows 1)))
                     (view-height (max 1 (- view-bottom view-top)))
                     (content-width (max 10 (- maxx 3))))
                  (set! %click-targets (make-hash-table 61))
                  (erase stdscr)
                  ;; 1. Slim header line (wordmark, model, status, counts)
                  ;;    over a thin horizontal rule
                  (let*
                      ((active-count
                         (length
                           (filter
                             (lambda (s)
                               (assoc-ref s 'running?))
                             (mcp-manager-list-servers mcp-mgr))))
                       (total-count (length (badb-config-mcp-servers cfg)))
                       (mcp-info (and (> total-count 0) (format #f "~a/~a" active-count total-count))))
                    (for-each
                      (lambda (seg)
                        (let
                            ((x (car seg)) (text (cadr seg)) (role (caddr seg)) (bold? (cadddr seg)))
                          (draw-role! role #t)
                          (when bold? (attr-on! stdscr A_BOLD))
                          (safe-addstr stdscr 0 x text (max 0 (- maxx x)))
                          (when bold? (attr-off! stdscr A_BOLD))
                          (draw-role! role #f)))
                      (format-header-segments maxx
                                              status-text
                                              cur-model
                                              (registry-count reg)
                                              (journal-count-cached)
                                              mcp-info
                                              (context-gauge-info)))
                    (draw-role! 'dim #t)
                    (safe-addstr stdscr 1 0 (format-rule-line maxx) maxx)
                    (draw-role! 'dim #f))
                  ;; 2. Flat message history blocks
                  ;; Rows are tagged with their owning message index so the
                  ;; selection marker and auto-scroll can address messages,
                  ;; not screen lines. Formatted rows come from the
                  ;; per-message cache: unchanged messages are never
                  ;; re-wrapped, so frames are O(new messages).
                  (let*
                      ((asm-valid? (and (eq? asm-messages messages) (eqv? asm-width content-width)))
                       (msg-count
                         (if asm-valid?
                           asm-msg-count
                           (length messages)))
                       (sel-idx (and (integer? msgsel-index) (< msgsel-index msg-count) msgsel-index)))
                    ;; Rebuild the assembled transcript view only when the messages list
                    ;; or the content width changed — every mutation installs a fresh
                    ;; list, so list identity is a sound cache key. Scrolling renders
                    ;; reuse the cached rows, per-message counts, and total instead of
                    ;; re-walking the whole transcript every frame.
                    (unless asm-valid?
                      (row-cache-prune! row-cache messages)
                      (let*
                          ((indexed-rows
                             (map
                               (lambda (idx msg)
                                 (map
                                   (lambda (entry)
                                     (cons idx entry))
                                   (row-cache-rows row-cache msg content-width)))
                               (iota msg-count)
                               messages)))
                        (set! asm-lines (apply append indexed-rows))
                        (set! asm-counts (map length indexed-rows))
                        (set! asm-total (length asm-lines))
                        (set! asm-msg-count msg-count)
                        (set! asm-messages messages)
                        (set! asm-width content-width)))
                    (let*
                        ((total-lines asm-total) (max-scroll (max 0 (- total-lines view-height))))
                      ;; Clamp scroll after collapse state changed line counts
                      (begin
                        (set! scroll-offset (min scroll-offset max-scroll))
                        (set! max-scroll-seen max-scroll))
                      ;; Keep the selected message inside the visible window
                      (when sel-idx
                        (let*
                            ((row-counts asm-counts)
                             (starts
                               (let loop
                                 ((counts row-counts) (pos 0))
                                 (if (null? counts)
                                   '()
                                   (cons pos (loop (cdr counts) (+ pos (car counts)))))))
                             (sel-start (list-ref starts sel-idx))
                             (sel-end (+ sel-start (list-ref row-counts sel-idx)))
                             (base-start (max 0 (- total-lines view-height scroll-offset)))
                             (win-end (+ base-start view-height)))
                          (cond
                            ((< sel-start base-start)
                              (set! scroll-offset (min max-scroll (+ scroll-offset (- base-start sel-start)))))
                            ((> sel-end win-end)
                              (set! scroll-offset (max 0 (- scroll-offset (- sel-end win-end))))))))
                      (let*
                          ((start-line (max 0 (- total-lines view-height scroll-offset)))
                           (visible-lines
                             (take (drop asm-lines start-line) (min view-height (- total-lines start-line)))))
                        (let loop
                          ((lines visible-lines) (y view-top))
                          (unless (or (null? lines) (>= y view-bottom))
                            (let ((tagged (car lines)))
                              (hashq-set! %click-targets y (car tagged))
                              (draw-block-line y
                                               (cdr tagged)
                                               content-width
                                               (and sel-idx (= (car tagged) sel-idx))))
                            (loop (cdr lines) (+ y 1)))))))
                  ;; Pending steering line: dim feedback above the prompt
                  ;; for text being typed mid-run, queued steering
                  ;; entries, and pending interrupt requests
                  (let
                      ((pending
                         (steering-pending-line steering-buffer
                                                (steering-count steering-queue)
                                                interrupt-requested?)))
                    (when pending
                      (draw-role! 'dim #t)
                      (safe-addstr stdscr view-bottom 0 pending maxx)
                      (draw-role! 'dim #f)))
                  ;; 3. Prompt Input Block: themed indicator glyph leading
                  ;;    the first row of the wrapped input, continuation
                  ;;    rows below. Long inputs wrap instead of scrolling
                  ;;    off the right edge.
                  (call-with-values
                    (lambda ()
                      (get-prompt-indicator input-buffer status-text prompt-mode))
                    (lambda (glyph theme-role bold?)
                      (when (string=? status-text "STREAMING...")
                        (set! glyph (streaming-spinner-glyph)))
                      (draw-role! theme-role #t)
                      (when bold? (attr-on! stdscr A_BOLD))
                      (safe-addstr stdscr prompt-top 0 glyph 1)
                      (when bold? (attr-off! stdscr A_BOLD))
                      (draw-role! theme-role #f)
                      (draw-role! 'default #t)
                      (let draw-lines
                        ((lines input-lines) (y prompt-top))
                        (unless (null? lines)
                          (safe-addstr stdscr y 2 (car lines) avail-width)
                          (draw-lines (cdr lines) (+ y 1))))
                      (draw-role! 'default #f)))
                  ;; 4. Bottom keymap hint line in dim, per active mode
                  (let
                      ((hint
                         (format-keymap-line
                           (cond
                             (active-menu 'menu)
                             ((selector-active? input-buffer) 'selector)
                             ((and (agent-run-active? status-text) nodelay-proc) 'steering)
                             (else prompt-mode)))))
                    (draw-role! 'dim #t)
                    (safe-addstr stdscr (- maxy 1) 0 hint maxx)
                    (draw-role! 'dim #f))
                  ;; 5. Floating Slash Command Selector Popup Overlay
                  (when (selector-active? input-buffer)
                    (let*
                        ((matches (matching-slash-commands input-buffer))
                         (box-w (min 62 (max 20 (- maxx 4)))))
                      (unless (null? matches)
                        (call-with-values
                          (lambda ()
                            (selector-visible-slice matches
                                                    (min %selector-max-visible (max 1 (- maxy 5)))
                                                    selector-index))
                          (lambda
                              (vis-items offset sel-pos above-count below-count)
                            (let*
                                ((vis-count (length vis-items))
                                 (box-h (+ vis-count 2))
                                 (start-y (- prompt-top 1 box-h))
                                 (rows
                                   (map
                                     (lambda (item)
                                       (cons (format #f "/~a" (car item)) (cdr item)))
                                     vis-items)))
                              (when (>= start-y 2)
                                (draw-popup! "commands" rows sel-pos box-w start-y 2 above-count below-count)
                                ;; Mouse hit map: palette rows are items;
                                ;; clicking one selects it.
                                (let li
                                  ((i 0))
                                  (when (< i vis-count)
                                    (hashq-set! %click-targets (+ start-y 1 i) (cons 'palette-item (+ offset i)))
                                    (li (+ i 1)))))))))))
                  ;; 6. Floating Menu Popup Overlay (same visual language as
                  ;; the slash selector: accent rounded box, bold selected
                  ;; row, capped visible rows with scroll indicators)
                  (when active-menu
                    (let ((m-count (menu-count active-menu)))
                      (when (> m-count 0)
                        (call-with-values
                          (lambda ()
                            (menu-visible-slice active-menu (min %menu-max-visible (max 1 (- maxy 5)))))
                          (lambda (vis-items offset)
                            (let*
                                ((vis-count (length vis-items))
                                 (label-w
                                   (fold
                                     (lambda (it acc)
                                       (max acc (string-length (menu-item-label it))))
                                     0
                                     vis-items))
                                 (desc-w
                                   (fold
                                     (lambda (it acc)
                                       (max acc (string-length (menu-item-description it))))
                                     0
                                     vis-items))
                                 (box-w (min (max 24 (+ 8 label-w desc-w)) (max 20 (- maxx 4))))
                                 (box-h (+ vis-count 2))
                                 (start-y (- prompt-top 1 box-h))
                                 (rows
                                   (map
                                     (lambda (it)
                                       (cons (menu-item-label it) (menu-item-description it)))
                                     vis-items)))
                              (when (>= start-y 2)
                                (draw-popup! (menu-title active-menu)
                                             rows
                                             (- (menu-selected-index active-menu) offset)
                                             box-w
                                             start-y
                                             2
                                             offset
                                             (- m-count offset vis-count))
                                (let mi
                                  ((i 0))
                                  (when (< i vis-count)
                                    (hashq-set! %click-targets (+ start-y 1 i) (cons 'menu-item (+ offset i)))
                                    (mi (+ i 1)))))))))))
                  ;; Position the terminal cursor at the editing position
                  ;; LAST: row draws below the input line (keymap hint,
                  ;; pop-ups) displace the cursor, so reposition right
                  ;; before refreshing or the hardware cursor visibly
                  ;; rests after the last drawn row.
                  (move stdscr (+ prompt-top input-cur-row) (+ 2 input-cur-col))
                  ;; Cursor compatibility: the terminal cursor's shape
                  ;; signals the active mode - a bar for chat typing,
                  ;; an underline in the modal REPLs, a block while a
                  ;; message card is selected - and it hides entirely
                  ;; while the agent runs (the spinner conveys the
                  ;; activity). The backend caches shapes, so styling
                  ;; every frame is free; endwin restores the default.
                  (cond
                    ((agent-run-active? status-text) (safe-curs-set! 0))
                    ((integer? msgsel-index) (safe-curs-set! 1) (cursor-shape 'block))
                    ((memq prompt-mode '(shell scheme))
                      (safe-curs-set! 1)
                      (cursor-shape 'underline))
                    (else (safe-curs-set! 1) (cursor-shape 'bar)))
                  (refresh stdscr)))
              (lambda (key . args)
                ;; Never swallow render failures silently: surface each new
                ;; error once in the transcript so a broken draw path is
                ;; visible instead of freezing the UI with no diagnostics.
                (let ((sig (format #f "~s ~s" key args)))
                  (unless (equal? sig last-render-error-sig)
                    (set! last-render-error-sig sig)
                    (add-err-msg! (format #f "render error: ~a ~s" key args))))
                #f)))
    ;; Live streaming display: one flagged assistant card grows in the
    ;; transcript as deltas arrive ('streaming flag); later steps replace
    ;; exactly that placeholder, commit it permanently, or remove it.
    (define (react-stream-placeholder-index)
            (list-index
              (lambda (m)
                (assoc-ref m 'streaming))
              messages))
    (define (update-react-stream-message! delta type)
            (cond
              ((eq? type 'thought)
                (set! react-stream-thoughts (string-append react-stream-thoughts delta)))
              ((eq? type 'content)
                (set! react-stream-content (string-append react-stream-content delta)))
              ((eq? type 'usage) (record-session-usage! delta))
              (else #f))
            (call-with-values
              (lambda ()
                (split-thoughts-and-content react-stream-content))
              (lambda (inline-th inline-ct)
                (let*
                    ((total-th
                       (cond
                         ((and (non-empty-string? react-stream-thoughts) inline-th)
                           (string-append react-stream-thoughts "\n\n" inline-th))
                         ((non-empty-string? react-stream-thoughts) react-stream-thoughts)
                         (else inline-th)))
                     (updated
                       (cons (cons 'streaming #t)
                             (default-collapse-message
                               `((role . "assistant") (thoughts . ,total-th) (content . ,inline-ct))))))
                  (let ((idx (react-stream-placeholder-index)))
                    (if idx
                      (set! messages (replace-nth messages idx updated))
                      (set! messages (append messages (list updated)))))))))
    (define (commit-react-stream-message!)
            "Keep the streamed assistant card permanently but drop its
placeholder flag, so the next streamed call starts a fresh card."
            (set! messages
                  (map
                    (lambda (m)
                      (if (assoc-ref m 'streaming)
                        (filter
                          (lambda (p)
                            (not (eq? (car p) 'streaming)))
                          m)
                        m))
                    messages)))
    (define (remove-react-stream-message!)
            (set! messages
                  (filter
                    (lambda (m)
                      (not (assoc-ref m 'streaming)))
                    messages)))
    (define (reset-react-stream-state!)
            (set! react-stream-content "")
            (set! react-stream-thoughts ""))
    ;; Live context gauge for the header: estimated conversation tokens
    ;; over the compactor ceiling, e.g. "ctx 12.4k/16k". Cheap enough to
    ;; compute per frame; tells the user when /compact is due.
    (define %ctx-cache-tokens #f)
    (define %ctx-cache-key #f)
    (define %jrn-count-cache -1)
    (define %jrn-count-time 0)
    (define (journal-count-cached)
            "Journal entry count for the header, refreshed at most every two
seconds: journal-count re-reads the whole journal file (~150ms on a
grown journal), which the per-frame header render must not pay."
            (let ((now (get-internal-run-time)))
              (when
                  (>= (- now %jrn-count-time) (* 2 internal-time-units-per-second))
                (set! %jrn-count-time now)
                (set! %jrn-count-cache (journal-count jrn)))
              %jrn-count-cache))
    (define (context-gauge-info)
            (let ((key (length llm-history)))
              ;; The estimate only changes when history grows; cache it.
              (unless (eq? key %ctx-cache-key)
                (set! %ctx-cache-key key)
                (set! %ctx-cache-tokens (estimate-conversation-tokens llm-history)))
              (let*
                  ((used %ctx-cache-tokens)
                   (ceiling (context-compactor-effective-ceiling compactor))
                   (k format-token-kilo))
                (if (and (number? used) (> ceiling 0))
                  (format #f "ctx ~a/~a" (k used) (k ceiling))
                  #f))))
    (define (throttled-stream-render!)
            ;; Repaint at most ~15fps while streaming: every delta updates
            ;; state cheaply, full-frame paints are rate-limited.
            (let ((now (get-internal-run-time)))
              (when
                  (>= (- now react-stream-last-render)
                      (quotient internal-time-units-per-second 15))
                (set! react-stream-last-render now)
                (render!))))
    ;; Execute streaming query against LLM with separated thoughts
    (define (execute-chat-stream! user-prompt)
            (set! scroll-offset 0)
            ;; Session accounting: a new turn starts, and until an
            ;; endpoint usage block arrives the growth of the
            ;; conversation provides the fallback estimate.
            (set! session-turns (+ session-turns 1))
            (set! turn-usage-seen? #f)
            (set! turn-pre-estimate (estimate-conversation-tokens llm-history))
            (catch #t
              (lambda ()
                (set! llm-history
                      (append llm-history (list `((role . "user") (content . ,user-prompt)))))
                (when (compaction-needed? compactor llm-history)
                  (set! llm-history (compact-messages compactor llm-history)))
                (let
                    ((tools (collect-available-tools reg mcp-mgr)))
                  (if (pair? tools)
                    ;; Autonomous ReAct Tool-Calling Loop
                    (catch #t
                      (lambda ()
                        (let*
                            ((max-react-iters (badb-config-react-max-iterations cfg))
                             (session
                               (make-react-session #:client
                                                   client
                                                   #:messages
                                                   llm-history
                                                   #:registry
                                                   reg
                                                   #:mcp-manager
                                                   mcp-mgr
                                                   #:journal
                                                   jrn
                                                   #:transaction-manager
                                                   mgr
                                                   #:max-iterations
                                                   max-react-iters
                                                   #:steering-queue
                                                   steering-queue
                                                   #:max-tokens
                                                   (or (badb-config-model-max-tokens cfg) 4096)
                                                   #:deferred-tools?
                                                   #t
                                                   #:offload-hook
                                                   (make-ctx-offload-hook ctx-idx)
                                                   #:guardrail
                                                   (lambda (t-name t-args)
                                                     (evaluate-guardrail policy t-name t-args))
                                                   #:hooks
                                                   (list
                                                     (cons 'on-step
                                                           (lambda (iter phase)
                                                             (when (eq? phase 'starting)
                                                               (commit-react-stream-message!)
                                                               (reset-react-stream-state!))
                                                             (set! status-text (format #f "REACT [~a/~a]" iter max-react-iters))
                                                             (poll-steering-input!)
                                                             (render!)))
                                                     (cons 'on-tool-call
                                                           (lambda (tc)
                                                             (let
                                                                 ((t-name (prop-ref tc 'name)) (t-args (prop-ref tc 'arguments)))
                                                               (set! status-text (format #f "TOOL: ~a..." t-name))
                                                               (add-msg! "tool-call" (format #f "Invoking tool: ~a (~s)" t-name t-args))
                                                               (poll-steering-input!)
                                                               (render!))))
                                                     (cons 'on-tool-result
                                                           (lambda (tc res err?)
                                                             (let*
                                                                 ((t-name (prop-ref tc 'name))
                                                                  (res-str
                                                                    (if (string? res)
                                                                      res
                                                                      (safe-json-string res)))
                                                                  (preview
                                                                    (if (> (string-length res-str) 140)
                                                                      (string-append (substring res-str 0 140) "...")
                                                                      res-str)))
                                                               (add-msg! "tool-result"
                                                                         (if err?
                                                                           (format #f "Tool ~a failed: ~a" t-name preview)
                                                                           (format #f "Tool ~a returned: ~a" t-name preview)))
                                                               (poll-steering-input!)
                                                               (render!))))
                                                     (cons 'on-delta
                                                           (lambda (delta type)
                                                             (update-react-stream-message! delta type)
                                                             (poll-steering-input!)
                                                             (throttled-stream-render!))))
                                                   #:compactor
                                                   compactor
                                                   #:loop-detector
                                                   (make-loop-detector)
                                                   #:recitation?
                                                   #t)))
                          (call-with-values
                            (lambda ()
                              ;; Non-blocking key capture spans the whole
                              ;; run: nodelay on while the loop executes so
                              ;; the hooks can poll getch, restored after
                              (dynamic-wind
                                (lambda ()
                                  (when nodelay-proc
                                    (catch #t
                                      (lambda ()
                                        (nodelay-proc stdscr #t))
                                      (lambda _
                                        #f))))
                                (lambda ()
                                  (react-run! session))
                                (lambda ()
                                  (when nodelay-proc
                                    (catch #t
                                      (lambda ()
                                        (nodelay-proc stdscr #f))
                                      (lambda _
                                        #f))))))
                            (lambda (verdict final-content sess)
                              (set! llm-history (react-session-messages sess))
                              (cond
                                ((eq? verdict 'completed)
                                  (remove-react-stream-message!)
                                  (call-with-values
                                    (lambda ()
                                      (split-thoughts-and-content (or final-content "")))
                                    (lambda (th ct)
                                      (set! messages
                                            (append messages
                                                    (list
                                                      (default-collapse-message
                                                        `((role . "assistant") (thoughts . ,th) (content . ,ct)))))))))
                                ((eq? verdict 'max-iterations-reached)
                                  (commit-react-stream-message!)
                                  (add-sys-msg!
                                    (format #f
                                            "ReAct iteration ceiling reached (~a iterations). Type 'Continue' or follow up to proceed."
                                            max-react-iters)))
                                ((eq? verdict 'interrupted)
                                  (commit-react-stream-message!)
                                  (add-sys-msg!
                                    "Agent run interrupted — the loop halted at the current iteration boundary. Type a message to continue."))
                                ((eq? verdict 'loop-detected)
                                  (commit-react-stream-message!)
                                  (add-sys-msg!
                                    "Loop detected: the identical tool call was repeated too many times, so the agent run halted. Check the loop-detector warning in the transcript, then steer the agent with a follow-up message."))
                                ((eq? verdict 'error)
                                  (commit-react-stream-message!)
                                  (add-err-msg! (format #f "ReAct error: ~a" final-content))))))))
                      (lambda (key . args)
                        (add-err-msg! (format #f "Agent loop failure: ~a ~s" key args))))
                    ;; Direct Streaming Mode when no tools registered
                    (begin (set! status-text "STREAMING...")
                           (set! messages
                                 (append messages
                                         (list
                                           (default-collapse-message
                                             `((role . "assistant") (thoughts . #f) (content . ""))))))
                           (render!)
                           (catch #t
                             (lambda ()
                               (let
                                   ((raw-api-thoughts "") (raw-api-content "") (target-cell (last-pair messages)))
                                 (model-stream client
                                               llm-history
                                               (lambda (delta . type-args)
                                                 (let
                                                     ((chunk-type
                                                        (if (pair? type-args)
                                                          (car type-args)
                                                          'content)))
                                                   (cond
                                                     ((eq? chunk-type 'complete-json)
                                                       ;; Endpoint ignored stream:true:
                                                       ;; deliver the whole completion
                                                       ;; as one update; its usage
                                                       ;; block, when present, feeds
                                                       ;; the session accounting.
                                                       (record-session-usage! (extract-usage delta))
                                                       (call-with-values
                                                         (lambda ()
                                                           (determine-step-content delta))
                                                         (lambda (ct rc)
                                                           (set! raw-api-content (string-append raw-api-content ct))
                                                           (set! raw-api-thoughts (string-append raw-api-thoughts (or rc ""))))))
                                                     ((eq? chunk-type 'thought)
                                                       (set! raw-api-thoughts (string-append raw-api-thoughts delta)))
                                                     (else (set! raw-api-content (string-append raw-api-content delta))))
                                                   (call-with-values
                                                     (lambda ()
                                                       (split-thoughts-and-content raw-api-content))
                                                     (lambda (inline-th inline-ct)
                                                       (let*
                                                           ((total-th
                                                              (cond
                                                                ((and (not (string-null? raw-api-thoughts)) inline-th)
                                                                  (string-append raw-api-thoughts "\n\n" inline-th))
                                                                ((not (string-null? raw-api-thoughts)) raw-api-thoughts)
                                                                (else inline-th)))
                                                            (updated-msg
                                                              (default-collapse-message
                                                                `((role . "assistant") (thoughts . ,total-th) (content . ,inline-ct)))))
                                                         (if (pair? target-cell)
                                                           (set-car! target-cell updated-msg)
                                                           (set! messages (append (drop-right messages 1) (list updated-msg))))
                                                         (throttled-stream-render!))))))
                                               #:max-tokens
                                               (or (badb-config-model-max-tokens cfg) 4096))
                                 (set! llm-history
                                       (append llm-history (list `((role . "assistant") (content . ,raw-api-content)))))))
                             (lambda (key . args)
                               (add-err-msg! (format #f "Stream error (~a): ~s" key args))))))))
              (lambda (key . args)
                (add-err-msg! (format #f "Execution error (~a): ~s" key args))))
            ;; No endpoint usage block this turn: account an estimate of
            ;; the conversation growth instead, labeled on the /usage card.
            (unless turn-usage-seen?
              (let
                  ((post (estimate-conversation-tokens llm-history)))
                (when (number? post)
                  (set! session-estimated-tokens
                        (+ session-estimated-tokens (max 0 (- post (or turn-pre-estimate 0))))))))
            ;; Persist the whole conversation to the session log after
            ;; every chat turn so /resume can pick it up later.
            (persist-session-file! session-file llm-history)
            ;; Mid-run steering state resets with the run; queued entries
            ;; in steering-queue persist for the next run by design
            (set! steering-buffer "")
            (set! interrupt-requested? #f)
            (set! status-text "READY")
            (render!))
    (define (execute-shell! cmd)
            (set! status-text "EXEC...")
            (render!)
            (if (string-null? cmd)
              (add-sys-msg! "Usage: !<command> (e.g. !git status, !ls -la)")
              (call-with-values
                (lambda ()
                  (execute-shell-command cmd))
                (lambda (out exit-val err)
                  (cond
                    (err (add-err-msg! err))
                    ((= exit-val 0)
                      (add-msg! "shell-out"
                                (if (string-null? out)
                                  "(process completed with exit status 0)"
                                  out)))
                    (else
                      (add-err-msg!
                        (if (string-null? out)
                          (format #f "[process exited with status ~a]" exit-val)
                          (format #f "[exit status ~a]\n~a" exit-val out))))))))
            (set! status-text "READY")
            (render!))
    (define (execute-scheme! code-str)
            (set! status-text "EVAL...")
            (render!)
            (if (string-null? code-str)
              (add-sys-msg!
                "Usage: ,<expr> or (<expr>) (e.g. ,(+ 1 2) or (registry-list reg))")
              (call-with-values
                (lambda ()
                  (eval-scheme-code code-str repl-mod))
                (lambda (res err)
                  (if err
                    (add-err-msg! err)
                    (add-msg! "scheme-out" res)))))
            (set! status-text "READY")
            (render!))
    ;; Handle MCP subcommands
    (define (handle-mcp-cmd! arg)
            (execute-mcp-subcommand! mcp-mgr arg add-sys-msg! add-err-msg!))
    ;; Handle configuration subcommands
    (define (handle-config-cmd! arg)
            (let*
                ((tokens (split-words arg))
                 (subcmd
                   (if (null? tokens)
                     "show"
                     (car tokens)))
                 (rest
                   (if (null? tokens)
                     '()
                     (cdr tokens))))
              (string-case (if (string-null? arg) "" subcmd)
                           (""
                            (set! active-menu
                                  (make-menu "Configuration"
                                             (config-menu-items-for-state cfg cur-model cur-endpoint policy))))
                           ("show"
                            (add-sys-msg!
                              (format-config-summary cfg cur-model cur-endpoint cur-journal-path)))
                           ("reload"
                            (catch #t
                              (lambda ()
                                (set! cfg (load-config))
                                (set! cur-model (badb-config-model-name cfg))
                                (set! cur-endpoint (badb-config-model-endpoint cfg))
                                (refresh-client!)
                                (set! mcp-mgr (make-mcp-manager cfg))
                                (module-define! repl-mod 'cfg cfg)
                                (module-define! repl-mod 'mcp-mgr mcp-mgr)
                                (add-sys-msg! "Configuration reloaded from disk successfully."))
                              (lambda (k . a)
                                (add-err-msg! (format #f "Failed to reload config: ~a ~s" k a)))))
                           ("save"
                            (catch #t
                              (lambda ()
                                (save-local-config! cfg)
                                (add-sys-msg!
                                  (format #f "Configuration saved to ~a" (default-local-config-path))))
                              (lambda (k . a)
                                (add-err-msg! (format #f "Failed to save config: ~a ~s" k a)))))
                           ("set"
                            (if (< (length rest) 2)
                              (add-err-msg! "Usage: /config set <model|endpoint|temperature> <value>")
                              (let ((prop (car rest)) (val (cadr rest)))
                                (string-case prop
                                             ("model"
                                              (set! cur-model val)
                                              (badb-config-set-model-name! cfg val)
                                              (refresh-client!)
                                              (try-save-config!)
                                              (add-sys-msg! (format #f "Model set to '~a' and saved." val)))
                                             ("endpoint"
                                              (set! cur-endpoint val)
                                              (badb-config-set-model-endpoint! cfg val)
                                              (refresh-client!)
                                              (try-save-config!)
                                              (add-sys-msg! (format #f "Endpoint set to '~a' and saved." val)))
                                             ("temperature"
                                              (let ((temp (string->number val)))
                                                (if (and temp (number? temp))
                                                  (begin
                                                    (badb-config-set-model-temperature! cfg temp)
                                                    (try-save-config!)
                                                    (add-sys-msg! (format #f "Temperature set to ~a and saved." temp)))
                                                  (add-err-msg! "Invalid temperature: must be a number."))))
                                             (else (add-err-msg! (format #f "Unknown config property: ~a" prop)))))))
                           (else
                             (add-err-msg!
                               (format #f
                                       "Unknown /config subcommand '~a'. Valid: show, reload, save, set"
                                       subcmd))))))
    ;; Handle guardrail subcommands
    (define (handle-guardrail-cmd! arg)
            (let ((trimmed (string-trim-both arg)))
              (if (string-null? trimmed)
                (add-sys-msg!
                  (format #f
                          "Guardrail Policy Level: ~a\nUsage: /guardrail <disabled|permissive|cautious|strict|paranoid>"
                          (guardrail-policy-level policy)))
                (let ((sym (string->symbol trimmed)))
                  (if
                      (member sym '(disabled permissive cautious strict paranoid))
                    (begin (set-guardrail-policy-level! policy sym)
                           (add-sys-msg! (format #f "Guardrail policy level set to '~a'." sym)))
                    (add-err-msg! "Valid levels: disabled, permissive, cautious, strict, paranoid"))))))
    ;; Interactive menu wiring: item lists for the config, model, endpoint,
    ;; theme and guardrail menus plus selection dispatch. Items carry pure
    ;; action data (symbols or (tag . value) pairs) interpreted by
    ;; apply-menu-selection!, keeping construction and effects separated
    ;; exactly like the pure menu model.
    (define (prefill-input! text)
            "Place TEXT in the input buffer with the cursor at its end and
close any open menu, ready for the user to complete the command."
            (set! active-menu #f)
            (set! prompt-mode 'chat)
            (set! input-buffer text)
            (set! cursor-pos (string-length text)))
    (define (switch-provider-preset! name)
            "Switch the active model configuration to provider preset NAME,
refreshing the client, saving, and reporting the outcome."
            (if (badb-config-switch-provider! cfg name)
              (begin
                (set! cur-model (badb-config-model-name cfg))
                (set! cur-endpoint (badb-config-model-endpoint cfg))
                (set! cur-api-key (badb-config-model-api-key cfg))
                (refresh-client!)
                (try-save-config!)
                (add-sys-msg!
                  (format #f
                          "Switched to provider '~a': model ~a at ~a (saved to config)"
                          name
                          cur-model
                          cur-endpoint)))
              (add-err-msg!
                (format #f "Unknown provider preset '~a'. Use /provider to list presets." name))))
    (define (handle-provider-cmd! arg)
            "List configured provider presets or switch to the one named in ARG."
            (let ((trimmed (string-trim-both arg)))
              (if (string-null? trimmed)
                (add-sys-msg! (format-provider-presets (badb-config-providers cfg)))
                (switch-provider-preset! trimmed))))
    (define (handle-models-cmd!)
            "Show the models available on the active endpoint as a system
message, or a clear error notice when the listing fails."
            (call-with-values
              (lambda ()
                (fetch-live-models client))
              (lambda (ids err)
                (if ids
                  (add-sys-msg!
                    (string-append "Models available on endpoint:\n"
                                   (string-join
                                     (map
                                       (lambda (id)
                                         (if
                                             (and (string? cur-model) (string=? id cur-model))
                                           (format #f "• ~a (active)" id)
                                           (format #f "• ~a" id)))
                                       ids)
                                     "\n")))
                  (add-err-msg! (format #f "Failed to list models: ~a" err))))))
    (define (apply-menu-selection! item)
            "Apply the action data carried by the selected menu ITEM, closing
any open menu first; submenu actions open the next menu in its place."
            (set! active-menu #f)
            (let ((action (menu-item-action item)))
              (cond
                ((symbol? action)
                  (case action
                    ((open-config-menu)
                      (set! active-menu
                            (make-menu "Configuration"
                                       (config-menu-items-for-state cfg cur-model cur-endpoint policy))))
                    ((open-model-menu)
                      (set! active-menu
                            (make-menu "Model"
                                       (build-live-or-recorded-model-menu-items client cfg cur-model))))
                    ((open-endpoint-menu)
                      (set! active-menu (make-menu "Endpoint" (endpoint-menu-items-for-config cfg))))
                    ((open-theme-menu) (set! active-menu (make-menu "Theme" (theme-menu-items))))
                    ((open-guardrail-menu)
                      (set! active-menu (make-menu "Guardrail" (guardrail-menu-items))))
                    ((prefill-model) (prefill-input! "/model "))
                    ((prefill-endpoint) (prefill-input! "/endpoint "))
                    ((prefill-temperature) (prefill-input! "/config set temperature "))
                    ((config-reload) (handle-config-cmd! "reload"))
                    ((config-save) (handle-config-cmd! "save"))
                    (else (add-err-msg! (format #f "Unknown menu action: ~a" action)))))
                ((and (pair? action) (eq? (car action) 'set-model))
                  (let ((m (cdr action)))
                    (set! cur-model m)
                    (badb-config-set-model-name! cfg m)
                    (refresh-client!)
                    (try-save-config!)
                    (add-sys-msg! (format #f "Model switched to: ~a (saved to config)" m))))
                ((and (pair? action) (eq? (car action) 'switch-provider))
                  (switch-provider-preset! (cdr action)))
                ((and (pair? action) (eq? (car action) 'set-theme))
                  (let*
                      ((name (cdr action)) (th (lookup-theme name)))
                    (badb-config-set-ui-theme! cfg name)
                    ;; Colors resolve through ui-theme on every draw, so
                    ;; switching the theme restyles the interface live.
                    (when th (set! ui-theme th))
                    (try-save-config!)
                    (add-sys-msg! (format #f "UI theme set to '~a' and saved." name))))
                ((and (pair? action) (eq? (car action) 'set-guardrail))
                  (handle-guardrail-cmd! (symbol->string (cdr action))))
                ((and (pair? action) (eq? (car action) 'resume-session))
                  (let*
                      ((path (cdr action))
                       (loaded (load-session-file path))
                       (turn-count
                         (length
                           (filter
                             (lambda (m)
                               (not (string=? (assoc-ref m 'role) "system")))
                             loaded))))
                    (if (null? loaded)
                      (add-err-msg!
                        (format #f "Session file ~a is empty or unreadable." (basename path)))
                      (begin (set! llm-history loaded)
                             ;; Force the header context gauge to re-estimate.
                             (set! %ctx-cache-key #f)
                             (set! scroll-offset 0)
                             (add-sys-msg!
                               (format #f
                                       "Resumed session ~a (~a turns)"
                                       (truncate-string (basename path) %session-filename-max-length)
                                       turn-count))))))
                (else (add-err-msg! "Unsupported menu action.")))))
    ;; Handle /theme: with a NAME switch instantly through the same
    ;; path as the theme menu (applies live, persists to config);
    ;; without one, open the theme picker menu.
    (define (handle-theme-cmd! arg)
            (let ((name (string-trim-both arg)))
              (cond
                ((string-null? name) (set! active-menu (make-menu "Theme" (theme-menu-items))))
                ((lookup-theme name) =>
                                     (lambda (th)
                                       (badb-config-set-ui-theme! cfg name)
                                       (set! ui-theme th)
                                       (try-save-config!)
                                       (add-sys-msg! (format #f "UI theme set to '~a' and saved." name))))
                (else
                  (add-err-msg!
                    (format #f
                            "Unknown theme '~a'. /theme alone opens the picker with all ~a themes."
                            name
                            (length (all-theme-names))))))))
    (define (handle-mouse-event! ev)
            (let ((button (car ev)) (y (cadr ev)))
              (cond
                ((= button 4)
                  (let
                      ((new (min max-scroll-seen (+ scroll-offset 3))))
                    (if (= new scroll-offset)
                      #f
                      (begin (set! scroll-offset new) #t))))
                ((= button 5)
                  (let ((new (max 0 (- scroll-offset 3))))
                    (if (= new scroll-offset)
                      #f
                      (begin (set! scroll-offset new) #t))))
                ((= button 1)
                  (let ((target (hashq-ref %click-targets y)))
                    (cond
                      ((and active-menu (pair? target) (eq? (car target) 'menu-item))
                        (set! active-menu
                              (menu-move active-menu (- (cdr target) (menu-selected-index active-menu))))
                        (let ((item (menu-selected-item active-menu)))
                          (when item (apply-menu-selection! item)))
                        #t)
                      ((and (selector-active? input-buffer)
                            (pair? target)
                            (eq? (car target) 'palette-item))
                        (set! selector-index (cdr target))
                        #t)
                      ((integer? target) (set! msgsel-index target) #t)
                      (else
                        (if msgsel-index
                          (begin (set! msgsel-index #f) #t)
                          #f)))))
                (else #f))))
    ;; Handle context compaction command
    (define (handle-compact-cmd! _arg)
            (let*
                ((orig-tokens (estimate-conversation-tokens llm-history))
                 (orig-count (length llm-history))
                 (compacted (compact-messages compactor llm-history))
                 (new-tokens (estimate-conversation-tokens compacted))
                 (new-count (length compacted)))
              (set! llm-history compacted)
              (add-sys-msg!
                (format #f
                        "Conversation context compacted:\n• Messages: ~a -> ~a\n• Estimated tokens: ~a -> ~a"
                        orig-count
                        new-count
                        orig-tokens
                        new-tokens))))
    ;; Handle checkpoint command
    (define (handle-checkpoint-cmd! arg)
            (let*
                ((desc
                   (if (string-null? (string-trim-both arg))
                     "User manual checkpoint"
                     (string-trim-both arg)))
                 (chkpt (create-checkpoint! rollback-mgr desc)))
              (add-sys-msg!
                (format #f
                        "Created checkpoint '~a' (~a).\nFiles recorded: ~a, Registry entries: ~a."
                        (checkpoint-id chkpt)
                        (checkpoint-description chkpt)
                        (length (checkpoint-files chkpt))
                        (length (checkpoint-registry-state chkpt))))))
    ;; Handle rollback command
    (define (handle-rollback-cmd! arg)
            (let*
                ((trimmed (string-trim-both arg))
                 (res
                   (if (string-null? trimmed)
                     (rollback-latest! rollback-mgr)
                     (rollback-to-checkpoint! rollback-mgr trimmed))))
              (if
                  (equal? (assoc-ref res 'status) "rolled_back")
                (add-sys-msg!
                  (format #f
                          "Successfully rolled back to checkpoint '~a' (~a).\nRestored ~a file(s), removed ~a new file(s)."
                          (assoc-ref res 'checkpoint-id)
                          (assoc-ref res 'description)
                          (length (assoc-ref res 'restored-files))
                          (length (assoc-ref res 'removed-files))))
                (add-err-msg! (format #f "Rollback failed: ~a" (assoc-ref res 'error))))))
    ;; Handle project rules inspection
    (define (handle-rules-cmd! _arg)
            (add-sys-msg! (format-rules-and-tree (getcwd))))
    ;; Handle conversation context metrics
    (define (handle-context-cmd! _arg)
            (add-sys-msg! (format-context-metrics compactor llm-history messages)))
    ;; Handle per-session token usage report
    (define (handle-usage-cmd!)
            (add-sys-msg!
              (format-usage-report compactor
                                   llm-history
                                   session-prompt-tokens
                                   session-completion-tokens
                                   session-estimated-tokens
                                   session-turns
                                   #:ctx-idx ctx-idx)))
    ;; Handle session resume: interactive picker over recent session logs
    (define (handle-resume-cmd! _arg)
            (let
                ((sessions (recent-session-files (default-sessions-dir))))
              (if (null? sessions)
                (add-sys-msg!
                  "No saved sessions found. A session file is written after every chat turn.")
                (set! active-menu
                      (make-menu "Resume Session"
                                 (map
                                   (lambda (path)
                                     (cons
                                       (cons
                                         (truncate-string (basename path) %session-filename-max-length)
                                         (session-mtime-string path))
                                       (cons 'resume-session path)))
                                   sessions))))))
    ;; Handle slash commands
    (define (handle-dump-cmd! arg)
            (let*
                ((trimmed (string-trim-both arg))
                 (path
                   (if (string-null? trimmed)
                     (string-append ".badb/conversation-"
                                    (date->string (current-date 0) "~Y~m~dT~H~M~SZ")
                                    ".txt")
                     trimmed)))
              (catch #t
                (lambda ()
                  (ensure-directory! (dirname path))
                  (call-with-output-file path
                                         (lambda (port)
                                           (display (format-conversation-transcript messages) port)))
                  (add-sys-msg! (format #f "Conversation dumped to ~a" path)))
                (lambda (key . args)
                  (add-err-msg! (format #f "Dump failed (~a): ~s" key args))))))
    ;; Handle geis command (contract gate and dynamic feature synthesis)
    (define (handle-geis-cmd! arg)
            (let*
                ((trimmed (string-trim-both arg))
                 (space-pos (string-index trimmed #\space))
                 (sub
                   (if space-pos
                     (substring trimmed 0 space-pos)
                     trimmed))
                 (rest-arg
                   (if space-pos
                     (string-trim-both (substring trimmed (+ space-pos 1)))
                     "")))
              (cond
                ((or (string-null? trimmed) (string=? sub "help"))
                  (add-sys-msg!
                    (string-append
                      "Geis (Contract Gate & Dynamic Synthesis):\n"
                      "  /geis add <description>            - Prompt agent to draft spec, lock contract, synthesize module, and hot-attach\n"
                      "  /geis model [name]                 - Inspect or set default model for geis contract synthesis\n"
                      "  /geis list                         - List all locked feature contracts (geasa)\n"
                      "  /geis status [feature-id]          - Check status/hash of locked contract(s)\n"
                      "  /geis verify <feature-id>          - Replay locked contract in a disposable kernel subprocess\n"
                      "  /geis synthesize <feature-id> ...  - Run Sema plugin synthesis for locked feature\n\n"
                      "Example:\n"
                      "  /geis add retrieval for the numpy docs\n")))
                ((string=? sub "add")
                  (if (string-null? rest-arg)
                    (add-sys-msg!
                      "Usage: /geis add <feature description>\nExample: /geis add retrieval for the numpy docs")
                    (let
                        ((user-line (format #f "/geis add ~a" rest-arg)))
                      (record-history! user-line 'chat)
                      (add-user-msg! user-line)
                      (set! status-text "SYNTHESIZING...")
                      (render!)
                      (catch #t
                        (lambda ()
                          (begin
                            ;; capture the /geis add provenance (request + rendered prompt) for the
                            ;; bundle assembly step; defensive: never break the chat flow
                            (catch #t
                              (lambda ()
                                (geis-capture-pending-prompt! rest-arg (build-geis-add-prompt rest-arg)))
                              (lambda _
                                #f))
                            (execute-chat-stream! (build-geis-add-prompt rest-arg)))
                          (beep))
                        (lambda (key . args)
                          (add-err-msg! (format #f "Geis synthesis error (~a): ~s" key args))
                          (set! status-text "READY")
                          (render!))))))
                ((string=? sub "model")
                  (if (string-null? rest-arg)
                    (let* ((synth-model (badb-config-synthesis-model cfg #f)))
                      (add-sys-msg!
                        (if synth-model
                          (format #f "Geis synthesis model: ~a (configured in .badb/config.scm)\nUsage: /geis model [NAME]" synth-model)
                          (format #f "Geis synthesis model: ~a (default)\nUsage: /geis model [NAME]" (badb-config-synthesis-model cfg)))))
                    (let ((new-model (string-trim-both rest-arg)))
                      (badb-config-set-synthesis-model! cfg new-model)
                      (try-save-config!)
                      (add-sys-msg!
                        (format #f "Geis synthesis model set to: ~a (saved to config)" new-model)))))
                ((string=? sub "list") (add-sys-msg! (spec-tool-status)))
                ((string=? sub "status")
                  (add-sys-msg!
                    (apply spec-tool-status
                           (if (string-null? rest-arg)
                             '()
                             (list rest-arg)))))
                ((string=? sub "verify")
                  (if (string-null? rest-arg)
                    (add-sys-msg! "Usage: /geis verify <feature-id>")
                    (add-sys-msg! (spec-tool-verify rest-arg))))
                ((string=? sub "bootstrap")
                  (execute-geis-bootstrap! add-sys-msg! add-err-msg!))
                ((string=? sub "onboard")
                  (execute-geis-onboard! reg add-sys-msg! add-err-msg!))
                ((string=? sub "distill")
                  (execute-geis-distill! rest-arg add-sys-msg! add-err-msg!))
                ((string=? sub "synthesize")
                  (if (string-null? rest-arg)
                    (add-sys-msg! "Usage: /geis synthesize <feature-id> [target-path] [spec-path]")
                    (let*
                        ((parts
                           (filter
                             (lambda (s)
                               (not (string-null? s)))
                             (string-split rest-arg #\space)))
                         (fid (car parts))
                         (target-p (and (pair? (cdr parts)) (cadr parts)))
                         (spec-p (and (pair? (cdr parts)) (pair? (cddr parts)) (caddr parts))))
                      (set! status-text "SYNTHESIZING...")
                      (render!)
                      (catch #t
                        (lambda ()
                          (let*
                              ((synth-fn
                                 (module-ref
                                   (false-if-exception (resolve-module '(geasa runtimes-synthesis plugin)))
                                   'run-synthesis-workflow))
                               (res (synth-fn fid #:target-path target-p #:spec-path spec-p))
                               (status (assoc-ref res 'status))
                               (wf-result (assoc-ref res 'result)))
                            (if (eq? status 'passed)
                              (add-sys-msg!
                                (format #f
                                        "Geis synthesis passed for '~a'!\n~a"
                                        fid
                                        (if wf-result
                                          (scm->json-string wf-result)
                                          (or (assoc-ref res 'stdout) ""))))
                              (add-err-msg!
                                (format #f
                                        "Geis synthesis ~a for '~a'.\n~a\n~a"
                                        status
                                        fid
                                        (or (assoc-ref res 'error) "")
                                        (or (assoc-ref res 'stderr) ""))))))
                        (lambda (k . a)
                          (add-err-msg! (format #f "Geis synthesis error (~a): ~s" k a))))
                      (set! status-text "READY"))))
                (else
                  (add-err-msg!
                    (format #f
                            "Unknown /geis subcommand '~a'. Type /geis help for available commands."
                            sub))))))
    (define (handle-command! cmd-pair)
            (let ((cmd (car cmd-pair)) (arg (cdr cmd-pair)))
              (cond
                ((or (string=? cmd "help") (string=? cmd "?")) (add-sys-msg! %tui-help-message))
                ((string=? cmd "guardrail") (handle-guardrail-cmd! arg))
                ((string=? cmd "compact") (handle-compact-cmd! arg))
                ((string=? cmd "resume") (handle-resume-cmd! arg))
                ((string=? cmd "checkpoint") (handle-checkpoint-cmd! arg))
                ((string=? cmd "rollback") (handle-rollback-cmd! arg))
                ((string=? cmd "rules") (handle-rules-cmd! arg))
                ((string=? cmd "geis") (handle-geis-cmd! arg))
                ((string=? cmd "context") (handle-context-cmd! arg))
                ((string=? cmd "ctx") (execute-ctx-subcommand! ctx-idx arg add-sys-msg! add-err-msg!))
                ((string=? cmd "skills") (execute-skills-subcommand! skill-reg arg add-sys-msg! add-err-msg!))
                ((string=? cmd "memories") (execute-memories-subcommand! arg add-sys-msg! add-err-msg!))
                ((string=? cmd "usage") (handle-usage-cmd!))
                ((string=? cmd "models") (handle-models-cmd!))
                ((string=? cmd "model")
                  (if (string-null? arg)
                    (set! active-menu
                          (make-menu "Model"
                                     (build-live-or-recorded-model-menu-items client cfg cur-model)))
                    (begin (set! cur-model arg)
                           (badb-config-set-model-name! cfg arg)
                           (refresh-client!)
                           (try-save-config!)
                           (add-sys-msg! (format #f "Model switched to: ~a (saved to config)" cur-model)))))
                ((string=? cmd "endpoint")
                  (if (string-null? arg)
                    (set! active-menu (make-menu "Endpoint" (endpoint-menu-items-for-config cfg)))
                    (begin (set! cur-endpoint arg)
                           (badb-config-set-model-endpoint! cfg arg)
                           (refresh-client!)
                           (try-save-config!)
                           (add-sys-msg!
                             (format #f "Endpoint updated to: ~a (saved to config)" cur-endpoint)))))
                ((string=? cmd "provider") (handle-provider-cmd! arg))
                ((string=? cmd "mcp") (handle-mcp-cmd! arg))
                ((string=? cmd "config") (handle-config-cmd! arg))
                ((string=? cmd "theme") (handle-theme-cmd! arg))
                ((or (string=? cmd "sh") (string=? cmd "shell"))
                  (if (string-null? arg)
                    (add-sys-msg! "Usage: /sh <command> (or !<command>)")
                    (begin (add-cmd-msg! arg) (execute-shell! arg))))
                ((or (string=? cmd "eval") (string=? cmd "scheme"))
                  (if (string-null? arg)
                    (add-sys-msg!
                      "Usage: /eval <scheme-code> (or ,<scheme-code> or (<scheme-code>))")
                    (begin (add-scm-msg! arg) (execute-scheme! arg))))
                ((string=? cmd "features") (add-sys-msg! (format-features-list reg)))
                ((string=? cmd "journal") (add-sys-msg! (format-journal-recent jrn)))
                ((string=? cmd "test")
                  (set! status-text "TESTING...")
                  (render!)
                  (catch #t
                    (lambda ()
                      (let*
                          ((test-cmd
                             (if (string-null? (string-trim-both arg))
                               "./scripts/test"
                               (string-trim-both arg)))
                           (res (verify-test-suite #:command test-cmd))
                           (passed? (assoc-ref res 'passed?))
                           (exit-code (assoc-ref res 'exit-code))
                           (out (or (assoc-ref res 'output) "")))
                        (if passed?
                          (add-sys-msg!
                            (format #f
                                    "Test suite PASSED (exit ~a).\n~a"
                                    exit-code
                                    (truncate-string (string-trim-both out) 600)))
                          (add-err-msg!
                            (format #f
                                    "Test suite FAILED (exit ~a):\n~a"
                                    exit-code
                                    (truncate-string (string-trim-both out) 600))))))
                    (lambda (k . a)
                      (add-err-msg! (format #f "Test error: ~a ~s" k a))))
                  (set! status-text "READY"))
                ((string=? cmd "steer")
                  (let ((text (string-trim-both arg)))
                    (if (string-null? text)
                      (add-sys-msg!
                        "Usage: /steer <message> (queued for the running or next agent run)")
                      (begin (steering-push! steering-queue text)
                             (add-msg! "steer"
                                       (if (agent-run-active? status-text)
                                         (format #f "Steering queued for the running agent: ~a" text)
                                         (format #f "Steering queued for the next agent run: ~a" text)))))))
                ((string=? cmd "interrupt")
                  (steering-push! steering-queue 'interrupt)
                  (set! interrupt-requested? #t)
                  (add-msg! "steer"
                            (if (agent-run-active? status-text)
                              "Interrupt requested — the agent halts at the next iteration boundary."
                              "Interrupt queued — the next agent run halts before its first LLM call.")))
                ((string=? cmd "clear") (set! messages '())
                                        (set! llm-history (list (make-system-turn)))
                                        (set! scroll-offset 0)
                                        (set! msgsel-index #f))
                ((string=? cmd "dump") (handle-dump-cmd! arg))
                ((or (string=? cmd "quit") (string=? cmd "exit")) 'exit)
                (else
                  (add-err-msg! (format #f "Unknown command: /~a (type /help for commands)" cmd))))))
    ;; Cleanup handler
    (define (safe-curs-set! v)
            (catch #t
              (lambda ()
                (curs-set v))
              (lambda _
                #f)))
    (define (shutdown!)
            (catch #t
              (lambda ()
                (mcp-manager-stop-all! mcp-mgr))
              (lambda _
                #f))
            (safe-curs-set! 1)
            (catch #t
              (lambda ()
                (endwin))
              (lambda _
                #f)))
    ;; Initialize Curses
    (set! stdscr (initscr))
    (cbreak!)
    (noecho!)
    (keypad! stdscr #t)
    ;; Message selection keys: Ctrl+Up / Ctrl+Down. The (geasa surfaces-tui nc)
    ;; notcurses backend decodes the modifier natively and synthesizes
    ;; the KEY_FIND / KEY_OPEN sentinel codes for these sequences, so
    ;; resolution never fails under notcurses; the resolve-modified-key
    ;; dance keeps both the startup logic and any future backend
    ;; honest. 0 means unresolvable; no keypress ever delivers it.
    (set! ctrl-up-key
          (resolve-modified-key
            (string #\esc #\[ #\1 #\; #\5 #\A)
            key-defined
            define-key
            KEY_FIND))
    (set! ctrl-down-key
          (resolve-modified-key
            (string #\esc #\[ #\1 #\; #\5 #\B)
            key-defined
            define-key
            KEY_OPEN))
    (safe-curs-set! 1)
    (set! colors-enabled? (has-colors?))
    (when colors-enabled? (start-color!))
    ;; Color pairs are allocated lazily from the active theme by
    ;; draw-role!, memoized per unique (fg bg) spec.
    ;; Main input loop wrapped in dynamic-wind for crash-proof terminal restoration
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (define (clamp-selector-index!)
                (when (selector-active? input-buffer)
                  (let
                      ((matches (matching-slash-commands input-buffer)))
                    (set! selector-index
                          (if (null? matches)
                            0
                            (max 0 (min selector-index (- (length matches) 1))))))))
        (define (selected-command-name matches)
                (and (pair? matches)
                     (let
                         ((safe-idx (max 0 (min selector-index (- (length matches) 1)))))
                       (car (list-ref matches safe-idx)))))
        (define (insert-burst! ids)
                "Insert a burst of input codepoint ids into the buffer at the
cursor, newlines (10/13) as literal line breaks: pasted text lands as
one edit and renders once, instead of submitting on the first newline."
                (for-each
                  (lambda (id)
                    (let
                        ((text
                           (if
                               (or (eqv? id 10) (eqv? id 13) (eqv? id KEY_ENTER))
                             (string #\newline)
                             (string (integer->char id)))))
                      (call-with-values
                        (lambda ()
                          (insert-at input-buffer cursor-pos text))
                        apply-edit!)))
                  ids)
                (clamp-selector-index!))
        (define (apply-edit! new-buffer new-pos)
                "Commit an edit result (NEW-BUFFER NEW-POS) from any of the pure
buffer-editing helpers: replace the input buffer and move the cursor."
                (set! input-buffer new-buffer)
                (set! cursor-pos new-pos))
        ;; The input loop as an internal define: needs-render? defaults
        ;; to #t, so every plain (loop) call site keeps rendering. A
        ;; named let would give the recursion a fixed arity and break
        ;; them all.
        (define loop
                (lambda* (#:optional (needs-render? #t))
                         ;; Drain mouse events BEFORE rendering so a click's effects
                         ;; draw in this same iteration (the click map is rebuilt by
                         ;; every render; handlers act on the displayed frame).
                         (define mouse-dirty?
                                 (let ((events (drain-mouse-events)))
                                   (if (null? events)
                                     #f
                                     (any identity (map handle-mouse-event! events)))))
                         ;; Rendering is skipped only for no-op scroll keys in a key
                         ;; burst: a held or pasted run of scroll keys must not trigger
                         ;; a full-transcript render per key.
                         (when (or needs-render? mouse-dirty?) (render!))
                         (let ((ch (key->code (getch stdscr))))
                           (unless (or (eqv? ch KEY_UP) (eqv? ch KEY_DOWN))
                             (history-reset-nav input-history))
                           (cond
                             ;; Active menu popup: takes priority over the selector, input
                             ;; and scroll keys. Up/Down move the selection, Enter applies
                             ;; it, Esc closes; other keys are swallowed while the menu is
                             ;; open, except Ctrl+C / Ctrl+D which still exit below.
                             ((and active-menu (not (or (eqv? ch 3) (eqv? ch 4))))
                               (cond
                                 ((eqv? ch KEY_UP) (set! active-menu (menu-move active-menu -1)))
                                 ((eqv? ch KEY_DOWN) (set! active-menu (menu-move active-menu 1)))
                                 ((eqv? ch 27) (set! active-menu #f))
                                 ((or (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13))
                                   (let ((item (menu-selected-item active-menu)))
                                     (when item (apply-menu-selection! item))))
                                 (else #f))
                               (loop))
                             ;; Enter key: process buffer, execute modal command, or
                             ;; complete selector. Under cbreak the tty's ICRNL maps the
                             ;; Enter key to 10, so 10 must submit (Ctrl+N is select-next)
                             ((or (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13))
                               (if (input-pending?)
                                 ;; Enter with input still queued is a pasted newline,
                                 ;; not a submit: collect the burst and insert it as
                                 ;; multiline text.
                                 (begin (insert-burst! (cons ch (drain-input-burst!))) (loop))
                                 (if (selector-active? input-buffer)
                                   (let
                                       ((matches (matching-slash-commands input-buffer)))
                                     (if (null? matches)
                                       (let ((trimmed (string-trim-both input-buffer)))
                                         (set! input-buffer "")
                                         (set! cursor-pos 0)
                                         (set! selector-index 0)
                                         (if (string-null? trimmed)
                                           (loop)
                                           (let*
                                               ((action (classify-input trimmed))
                                                (res
                                                  (if (and action (eq? (car action) 'slash))
                                                    (handle-command! (cons (cadr action) (caddr action)))
                                                    #f)))
                                             (unless (eq? res 'exit) (loop)))))
                                       (let ((chosen (selected-command-name matches)))
                                         (cond
                                           ;; Commands that still need a typed argument get
                                           ;; prefilled; model/endpoint/config now open their
                                           ;; interactive menus via the no-argument flow.
                                           ((member chosen '("provider" "sh" "eval" "steer" "geis"))
                                             (set! input-buffer (string-append "/" chosen " "))
                                             (set! cursor-pos (string-length input-buffer))
                                             (set! selector-index 0)
                                             (loop))
                                           (else (set! input-buffer "")
                                                 (set! cursor-pos 0)
                                                 (set! selector-index 0)
                                                 (let ((res (handle-command! (cons chosen "")))) (unless (eq? res 'exit) (loop))))))))
                                   (let ((trimmed (string-trim-both input-buffer)))
                                     (set! input-buffer "")
                                     (set! cursor-pos 0)
                                     (set! selector-index 0)
                                     (cond
                                       ((string-null? trimmed) (loop))
                                       ((eq? prompt-mode 'shell)
                                         (record-history! trimmed 'shell)
                                         (add-cmd-msg! trimmed)
                                         (execute-shell! trimmed)
                                         (loop))
                                       ((eq? prompt-mode 'scheme)
                                         (record-history! trimmed 'scheme)
                                         (add-scm-msg! trimmed)
                                         (execute-scheme! trimmed)
                                         (loop))
                                       (else
                                         (let ((action (classify-input trimmed)))
                                           (case (car action)
                                             ((slash)
                                               (let*
                                                   ((cmd (cadr action)) (arg (caddr action))
                                                    (res (handle-command! (cons cmd arg))))
                                                 (unless (eq? res 'exit) (loop))))
                                             ((shell)
                                               (let ((shell-cmd (cdr action)))
                                                 (record-history! shell-cmd 'shell)
                                                 (add-cmd-msg! shell-cmd)
                                                 (execute-shell! shell-cmd)
                                                 (loop)))
                                             ((scheme)
                                               (let ((code-str (cdr action)))
                                                 (record-history! code-str 'scheme)
                                                 (add-scm-msg! code-str)
                                                 (execute-scheme! code-str)
                                                 (loop)))
                                             ((chat)
                                               (let ((user-text (cdr action)))
                                                 (record-history! user-text 'chat)
                                                 (add-user-msg! user-text)
                                                 (catch #t
                                                   (lambda ()
                                                     (execute-chat-stream! user-text)
                                                     ;; The ReAct loop ran for a while:
                                                     ;; ring the terminal bell so an
                                                     ;; away-from-keyboard user knows badb
                                                     ;; is waiting for them again.
                                                     (beep))
                                                   (lambda (key . args)
                                                     (add-err-msg! (format #f "Chat error (~a): ~s" key args))
                                                     (set! status-text "READY")
                                                     (render!)))
                                                 (loop)))))))))))
                             ;; Tab key: autocomplete selected slash command
                             ((eqv? ch 9)
                               (when (selector-active? input-buffer)
                                 (let*
                                     ((matches (matching-slash-commands input-buffer))
                                      (chosen (selected-command-name matches)))
                                   (when chosen
                                     (if
                                         (member chosen
                                                 '("model" "endpoint" "provider" "mcp" "config" "sh" "eval" "steer"))
                                       (set! input-buffer (string-append "/" chosen " "))
                                       (set! input-buffer (string-append "/" chosen)))
                                     (set! cursor-pos (string-length input-buffer))
                                     (set! selector-index 0))))
                               (loop))
                             ;; Escape key: close selector palette, return to normal chat
                             ;; mode, or clear the message selection on an empty buffer
                             ((eqv? ch 27)
                               (cond
                                 ((selector-active? input-buffer)
                                   (set! input-buffer "")
                                   (set! cursor-pos 0)
                                   (set! selector-index 0))
                                 ((not (eq? prompt-mode 'chat))
                                   (set! prompt-mode 'chat)
                                   (set! input-buffer "")
                                   (set! cursor-pos 0))
                                 ((and (integer? msgsel-index) (string-null? input-buffer))
                                   (set! msgsel-index #f)))
                               (loop))
                             ;; Up Arrow: selector navigation or input history recall.
                             ;; Arrows no longer scroll the transcript — history cycling
                             ;; is their job; scrolling stays on PageUp/PageDown and the
                             ;; mouse wheel.
                             ((eqv? ch KEY_UP)
                               (cond
                                 ((selector-active? input-buffer)
                                   (let
                                       ((matches (matching-slash-commands input-buffer)))
                                     (unless (null? matches)
                                       (let*
                                           ((count (length matches)) (cur (max 0 (min selector-index (- count 1)))))
                                         (set! selector-index
                                               (if (<= cur 0)
                                                 (- count 1)
                                                 (- cur 1)))))))
                                 (else
                                   (unless (history-recall-position input-history)
                                     (set! history-draft input-buffer)
                                     (set! history-draft-pos cursor-pos))
                                   (let ((entry (history-recall input-history -1)))
                                     (when entry
                                       (set! input-buffer entry)
                                       (set! cursor-pos (string-length entry))))))
                               (loop))
                             ;; Down Arrow: selector navigation or history recall
                             ;; forward; at the newest entry the draft is restored.
                             ((eqv? ch KEY_DOWN)
                               (cond
                                 ((selector-active? input-buffer)
                                   (let
                                       ((matches (matching-slash-commands input-buffer)))
                                     (unless (null? matches)
                                       (let*
                                           ((count (length matches)) (cur (max 0 (min selector-index (- count 1)))))
                                         (set! selector-index (remainder (+ cur 1) count))))))
                                 (else
                                   (let*
                                       ((was-recalling? (history-recall-position input-history))
                                        (entry (history-recall input-history 1)))
                                     (if entry
                                       (begin (set! input-buffer entry) (set! cursor-pos (string-length entry)))
                                       (when was-recalling?
                                         (set! input-buffer history-draft)
                                         (set! cursor-pos history-draft-pos))))))
                               (loop))
                             ;; Backspace: delete character or return to normal mode if
                             ;; prompt is empty (127 on modern terminals, 8 on older ones)
                             ((or (eqv? ch KEY_BACKSPACE) (eqv? ch 127) (eqv? ch 8))
                               (cond
                                 ((> (string-length input-buffer) 0)
                                   (call-with-values
                                     (lambda ()
                                       (backspace-at input-buffer cursor-pos))
                                     apply-edit!)
                                   (clamp-selector-index!))
                                 ((not (eq? prompt-mode 'chat)) (set! prompt-mode 'chat)))
                               (loop))
                             ;; Page Up / Page Down: half-viewport jumps
                             ((eqv? ch KEY_PPAGE)
                               (let
                                   ((new
                                      (min max-scroll-seen
                                           (+ scroll-offset (max 1 (quotient (- (getmaxy stdscr) 6) 2))))))
                                 (if (= new scroll-offset)
                                   (loop #f)
                                   (begin (set! scroll-offset new) (loop)))))
                             ((eqv? ch KEY_NPAGE)
                               (let
                                   ((new (max 0 (- scroll-offset (max 1 (quotient (- (getmaxy stdscr) 6) 2))))))
                                 (if (= new scroll-offset)
                                   (loop #f)
                                   (begin (set! scroll-offset new) (loop)))))
                             ;; Ctrl+Up: select the previous message card, starting from
                             ;; the newest message (code resolved at startup)
                             ((eqv? ch ctrl-up-key)
                               (unless (null? messages)
                                 (set! msgsel-index
                                       (if (integer? msgsel-index)
                                         (max 0 (- msgsel-index 1))
                                         (- (length messages) 1))))
                               (loop))
                             ;; Ctrl+Down: advance the message selection to the next
                             ;; card (no-op without selection; code resolved at startup)
                             ((eqv? ch ctrl-down-key)
                               (when
                                   (and (integer? msgsel-index) (pair? messages))
                                 (set! msgsel-index (min (- (length messages) 1) (+ msgsel-index 1))))
                               (loop))
                             ;; Left Arrow: move editing cursor left
                             ((eqv? ch KEY_LEFT)
                               (set! cursor-pos (clamp-cursor (- cursor-pos 1) input-buffer))
                               (loop))
                             ;; Right Arrow: move editing cursor right
                             ((eqv? ch KEY_RIGHT)
                               (set! cursor-pos (clamp-cursor (+ cursor-pos 1) input-buffer))
                               (loop))
                             ;; Home or Ctrl+A: move editing cursor to start of input
                             ((or (eqv? ch KEY_HOME) (eqv? ch 1)) (set! cursor-pos 0) (loop))
                             ;; End: move editing cursor to end of input
                             ((eqv? ch KEY_END) (set! cursor-pos (string-length input-buffer)) (loop))
                             ;; Ctrl+E (control char 5): toggle collapse of the selected
                             ;; message card, or move editing cursor to end of input when
                             ;; no message is selected (pre-existing binding)
                             ((eqv? ch 5)
                               (if
                                   (and (integer? msgsel-index)
                                        (pair? messages)
                                        (< msgsel-index (length messages)))
                                 (toggle-selected-collapse!)
                                 (set! cursor-pos (string-length input-buffer)))
                               (loop))
                             ;; Delete: delete character at editing cursor
                             ((eqv? ch KEY_DC)
                               (call-with-values
                                 (lambda ()
                                   (delete-at input-buffer cursor-pos))
                                 apply-edit!)
                               (clamp-selector-index!)
                               (loop))
                             ;; Ctrl+C or Ctrl+D: exit
                             ((or (eqv? ch 3) (eqv? ch 4)) 'done)
                             ;; Window resize: ncurses reports the new size as the
                             ;; synthetic KEY_RESIZE code (410). Consume it and redraw
                             ;; at the top of the loop; it must never reach the
                             ;; printable clause below, where it would insert the
                             ;; character U+019A into the input buffer.
                             ((eqv? ch KEY_RESIZE) (loop))
                             ;; Printable character input (codepoints >= 32). Unbound
                             ;; control codes fall through to the else branch and are
                             ;; discarded rather than inserted into the buffer.
                             ;; A run of pending input following the key means a paste:
                             ;; the whole burst is inserted in one pass (one render, no
                             ;; per-character latency, newlines preserved as data).
                             ((and (integer? ch) (>= ch 32))
                               (let
                                   ((burst
                                      (if (input-pending?)
                                        (cons ch (drain-input-burst!))
                                        (list ch))))
                                 (if (> (length burst) 1)
                                   (begin (insert-burst! burst) (loop))
                                   (let ((c (integer->char ch)))
                                     (cond
                                       ((and (string-null? input-buffer) (eq? prompt-mode 'chat))
                                         (cond
                                           ((eqv? c #\!) (set! prompt-mode 'shell) (loop))
                                           ;; "(" enters scheme mode and STAYS in the entry
                                           ;; field: it is the opening paren of the sexp being
                                           ;; typed, so dropping it (like the old "," trigger)
                                           ;; would corrupt the code under evaluation.
                                           ((eqv? c #\() (set! prompt-mode 'scheme)
                                                         (set! input-buffer "(")
                                                         (set! cursor-pos 1)
                                                         (loop))
                                           (else (set! input-buffer (string c))
                                                 (set! cursor-pos (string-length input-buffer))
                                                 (loop))))
                                       (else
                                         (call-with-values
                                           (lambda ()
                                             (insert-at input-buffer cursor-pos (string c)))
                                           apply-edit!)
                                         (clamp-selector-index!)
                                         (loop)))))))
                             (else (loop))))))
        (loop))
      (lambda ()
        (shutdown!)))))