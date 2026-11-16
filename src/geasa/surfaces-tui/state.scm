;;; (geasa surfaces-tui state) — Explicit TUI Session & UI State Record
;;;
;;; Holds the mutable session, kernel, rendering, and agent-steering state for
;;; run-tui in a single <tui-state> record so renderer, overlay, actions,
;;; agent-loop, and event-router modules can operate without closing over
;;; run-tui's lexical scope.

(define-module (geasa surfaces-tui state)
               #:use-module
               (geasa foundation-config plugin)
               #:use-module
               (geasa foundation-log plugin)
               #:use-module
               (geasa providers-model common)
               #:use-module
               (geasa providers-model openai)
               #:use-module
               (geasa runtimes-context plugin)
               #:use-module
               (geasa runtimes-prompt plugin)
               #:use-module
               (geasa runtimes-react plugin)
               #:use-module
               (geasa capabilities-code-nav plugin)
               #:use-module
               (geasa capabilities-skills plugin)
               #:use-module
               (geasa surfaces-tui editor)
               #:use-module
               (geasa surfaces-tui transcript)
               #:use-module
               (geasa surfaces-tui widgets)
               #:use-module
               (geasa surfaces-tui overlay)
               #:use-module
               (geasa surfaces-tui commands)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 threads)
               #:use-module
               (srfi srfi-9)
               #:export
               (<tui-state> make-tui-state
                            tui-state?
                            ;; Backend & curses bindings
                            tui-nref
                            tui-stdscr
                            set-tui-stdscr!
                            tui-colors-enabled?
                            set-tui-colors-enabled?!
                            tui-nodelay-proc
                            tui-steering-err-val
                            tui-ctrl-up-key
                            set-tui-ctrl-up-key!
                            tui-ctrl-down-key
                            set-tui-ctrl-down-key!
                            ;; Config & kernel handles
                            tui-cfg
                            set-tui-cfg!
                            tui-initial-api-key
                            tui-ui-theme
                            set-tui-ui-theme!
                            tui-cur-endpoint
                            set-tui-cur-endpoint!
                            tui-cur-model
                            set-tui-cur-model!
                            tui-cur-api-key
                            set-tui-cur-api-key!
                            tui-cur-journal-path
                            tui-reg
                            tui-jrn
                            tui-mgr
                            tui-mcp-mgr
                            set-tui-mcp-mgr!
                            tui-policy
                            tui-compactor
                            tui-rollback-mgr
                            tui-verifier
                            tui-composer
                            tui-client
                            set-tui-client!
                            tui-ctx-idx
                            tui-skill-reg
                            tui-llm-history
                            set-tui-llm-history!
                            tui-context-limit-discovered?
                            set-tui-context-limit-discovered?!
                            tui-repl-mod
                            ;; Banner & transcript
                            tui-banner-tips
                            tui-banner-frame
                            set-tui-banner-frame!
                            tui-banner-last-time
                            set-tui-banner-last-time!
                            tui-banner-anim-done?
                            set-tui-banner-anim-done?!
                            tui-messages
                            set-tui-messages!
                            tui-session-file
                            set-tui-session-file!
                            ;; Token accounting
                            tui-session-prompt-tokens
                            set-tui-session-prompt-tokens!
                            tui-session-completion-tokens
                            set-tui-session-completion-tokens!
                            tui-session-estimated-tokens
                            set-tui-session-estimated-tokens!
                            tui-session-turns
                            set-tui-session-turns!
                            tui-turn-usage-seen?
                            set-tui-turn-usage-seen?!
                            tui-turn-pre-estimate
                            set-tui-turn-pre-estimate!
                            ;; Editor, scroll & overlay
                            tui-input-buffer
                            set-tui-input-buffer!
                            tui-cursor-pos
                            set-tui-cursor-pos!
                            tui-input-history
                            tui-history-draft
                            set-tui-history-draft!
                            tui-history-draft-pos
                            set-tui-history-draft-pos!
                            tui-scroll-offset
                            set-tui-scroll-offset!
                            tui-max-scroll-seen
                            set-tui-max-scroll-seen!
                            tui-status-text
                            set-tui-status-text!
                            tui-selector-index
                            set-tui-selector-index!
                            tui-active-overlay
                            set-tui-active-overlay!
                            tui-msgsel-index
                            set-tui-msgsel-index!
                            tui-prompt-mode
                            set-tui-prompt-mode!
                            tui-active-keymap
                            set-tui-active-keymap!
                            tui-history-search
                            set-tui-history-search!
                            tui-workspace-files-cache
                            set-tui-workspace-files-cache!
                            tui-at-selector-index
                            set-tui-at-selector-index!
                            tui-active-todo-list
                            set-tui-active-todo-list!
                            ;; Render & UI server caches
                            tui-last-render-error-sig
                            set-tui-last-render-error-sig!
                            tui-role-pair-cache
                            set-tui-role-pair-cache!
                            tui-next-pair-id
                            set-tui-next-pair-id!
                            tui-row-cache
                            tui-asm-messages
                            set-tui-asm-messages!
                            tui-asm-width
                            set-tui-asm-width!
                            tui-asm-lines
                            set-tui-asm-lines!
                            tui-asm-counts
                            set-tui-asm-counts!
                            tui-asm-total
                            set-tui-asm-total!
                            tui-asm-msg-count
                            set-tui-asm-msg-count!
                            tui-click-targets
                            tui-row-texts
                            tui-ui-server
                            tui-ui-client
                            tui-last-tab-title-sig
                            set-tui-last-tab-title-sig!
                            tui-subagent-bus-sub-id
                            set-tui-subagent-bus-sub-id!
                            ;; Streaming & steering
                            tui-react-stream-content-chunks
                            set-tui-react-stream-content-chunks!
                            tui-react-stream-thoughts-chunks
                            set-tui-react-stream-thoughts-chunks!
                            tui-react-stream-last-render
                            set-tui-react-stream-last-render!
                            tui-last-spin-time
                            set-tui-last-spin-time!
                            tui-steering-queue
                            tui-steering-buffer
                            set-tui-steering-buffer!
                            tui-interrupt-requested?
                            set-tui-interrupt-requested?!
                            tui-steering-mutex
                            tui-force-stop-requested?
                            set-tui-force-stop-requested?!
                            tui-urgent-steer-requested?
                            set-tui-urgent-steer-requested?!
                            tui-mid-run-pending-commands
                            set-tui-mid-run-pending-commands!
                            tui-current-run-id
                            set-tui-current-run-id!
                            tui-steering-selector-index
                            set-tui-steering-selector-index!
                            tui-render-mutex
                            tui-render-dirty?
                            set-tui-render-dirty?!
                            tui-agent-thread
                            set-tui-agent-thread!
                            tui-ctx-cache-tokens
                            set-tui-ctx-cache-tokens!
                            tui-ctx-cache-key
                            set-tui-ctx-cache-key!
                            tui-jrn-count-cache
                            set-tui-jrn-count-cache!
                            tui-jrn-count-time
                            set-tui-jrn-count-time!
                            tui-jrn-count-refreshing?
                            set-tui-jrn-count-refreshing?!
                            tui-jrn-count-mutex
                            ;; Core state helpers
                            tui-agent-running?
                            tui-active-menu
                            tui-set-active-menu!
                            tui-clear-overlay!
                            tui-add-msg!
                            tui-add-sys-msg!
                            tui-add-err-msg!
                            tui-add-cmd-msg!
                            tui-add-scm-msg!
                            tui-add-user-msg!
                            tui-record-session-usage!
                            tui-toggle-selected-collapse!
                            tui-record-history!
                            tui-try-save-config!
                            tui-sync-context-limit!
                            tui-refresh-client!
                            tui-prefill-input!
                            tui-make-system-turn
                            tui-make-startup-notice-msg
                            tui-advance-startup-banner!
                            tui-reset-history-browse!
                            tui-clamp-selector-index!
                            tui-apply-edit!
                            tui-insert-burst!))

(define-record-type <tui-state>
                    (%make-tui-state nref
                                     stdscr
                                     colors-enabled?
                                     nodelay-proc
                                     steering-err-val
                                     ctrl-up-key
                                     ctrl-down-key
                                     cfg
                                     initial-api-key
                                     ui-theme
                                     cur-endpoint
                                     cur-model
                                     cur-api-key
                                     cur-journal-path
                                     reg
                                     jrn
                                     mgr
                                     mcp-mgr
                                     policy
                                     compactor
                                     rollback-mgr
                                     verifier
                                     composer
                                     client
                                     ctx-idx
                                     skill-reg
                                     llm-history
                                     context-limit-discovered?
                                     repl-mod
                                     banner-tips
                                     banner-frame
                                     banner-last-time
                                     banner-anim-done?
                                     messages
                                     session-file
                                     session-prompt-tokens
                                     session-completion-tokens
                                     session-estimated-tokens
                                     session-turns
                                     turn-usage-seen?
                                     turn-pre-estimate
                                     input-buffer
                                     cursor-pos
                                     input-history
                                     history-draft
                                     history-draft-pos
                                     scroll-offset
                                     max-scroll-seen
                                     status-text
                                     selector-index
                                     active-overlay
                                     msgsel-index
                                     prompt-mode
                                     active-keymap
                                     history-search
                                     workspace-files-cache
                                     at-selector-index
                                     active-todo-list
                                     last-render-error-sig
                                     role-pair-cache
                                     next-pair-id
                                     row-cache
                                     asm-messages
                                     asm-width
                                     asm-lines
                                     asm-counts
                                     asm-total
                                     asm-msg-count
                                     click-targets
                                     row-texts
                                     ui-server
                                     ui-client
                                     last-tab-title-sig
                                     subagent-bus-sub-id
                                     react-stream-content-chunks
                                     react-stream-thoughts-chunks
                                     react-stream-last-render
                                     last-spin-time
                                     steering-queue
                                     steering-buffer
                                     interrupt-requested?
                                     steering-mutex
                                     force-stop-requested?
                                     urgent-steer-requested?
                                     mid-run-pending-commands
                                     current-run-id
                                     steering-selector-index
                                     render-mutex
                                     render-dirty?
                                     agent-thread
                                     ctx-cache-tokens
                                     ctx-cache-key
                                     jrn-count-cache
                                     jrn-count-time
                                     jrn-count-refreshing?
                                     jrn-count-mutex)
                    tui-state?
                    (nref tui-nref)
                    (stdscr tui-stdscr set-tui-stdscr!)
                    (colors-enabled? tui-colors-enabled? set-tui-colors-enabled?!)
                    (nodelay-proc tui-nodelay-proc)
                    (steering-err-val tui-steering-err-val)
                    (ctrl-up-key tui-ctrl-up-key set-tui-ctrl-up-key!)
                    (ctrl-down-key tui-ctrl-down-key set-tui-ctrl-down-key!)
                    (cfg tui-cfg set-tui-cfg!)
                    (initial-api-key tui-initial-api-key)
                    (ui-theme tui-ui-theme set-tui-ui-theme!)
                    (cur-endpoint tui-cur-endpoint set-tui-cur-endpoint!)
                    (cur-model tui-cur-model set-tui-cur-model!)
                    (cur-api-key tui-cur-api-key set-tui-cur-api-key!)
                    (cur-journal-path tui-cur-journal-path)
                    (reg tui-reg)
                    (jrn tui-jrn)
                    (mgr tui-mgr)
                    (mcp-mgr tui-mcp-mgr set-tui-mcp-mgr!)
                    (policy tui-policy)
                    (compactor tui-compactor)
                    (rollback-mgr tui-rollback-mgr)
                    (verifier tui-verifier)
                    (composer tui-composer)
                    (client tui-client set-tui-client!)
                    (ctx-idx tui-ctx-idx)
                    (skill-reg tui-skill-reg)
                    (llm-history tui-llm-history set-tui-llm-history!)
                    (context-limit-discovered?
                      tui-context-limit-discovered?
                      set-tui-context-limit-discovered?!)
                    (repl-mod tui-repl-mod)
                    (banner-tips tui-banner-tips)
                    (banner-frame tui-banner-frame set-tui-banner-frame!)
                    (banner-last-time tui-banner-last-time set-tui-banner-last-time!)
                    (banner-anim-done? tui-banner-anim-done? set-tui-banner-anim-done?!)
                    (messages tui-messages set-tui-messages!)
                    (session-file tui-session-file set-tui-session-file!)
                    (session-prompt-tokens tui-session-prompt-tokens set-tui-session-prompt-tokens!)
                    (session-completion-tokens
                      tui-session-completion-tokens
                      set-tui-session-completion-tokens!)
                    (session-estimated-tokens
                      tui-session-estimated-tokens
                      set-tui-session-estimated-tokens!)
                    (session-turns tui-session-turns set-tui-session-turns!)
                    (turn-usage-seen? tui-turn-usage-seen? set-tui-turn-usage-seen?!)
                    (turn-pre-estimate tui-turn-pre-estimate set-tui-turn-pre-estimate!)
                    (input-buffer tui-input-buffer set-tui-input-buffer!)
                    (cursor-pos tui-cursor-pos set-tui-cursor-pos!)
                    (input-history tui-input-history)
                    (history-draft tui-history-draft set-tui-history-draft!)
                    (history-draft-pos tui-history-draft-pos set-tui-history-draft-pos!)
                    (scroll-offset tui-scroll-offset set-tui-scroll-offset!)
                    (max-scroll-seen tui-max-scroll-seen set-tui-max-scroll-seen!)
                    (status-text tui-status-text set-tui-status-text!)
                    (selector-index tui-selector-index set-tui-selector-index!)
                    (active-overlay tui-active-overlay set-tui-active-overlay!)
                    (msgsel-index tui-msgsel-index set-tui-msgsel-index!)
                    (prompt-mode tui-prompt-mode set-tui-prompt-mode!)
                    (active-keymap tui-active-keymap set-tui-active-keymap!)
                    (history-search tui-history-search set-tui-history-search!)
                    (workspace-files-cache tui-workspace-files-cache set-tui-workspace-files-cache!)
                    (at-selector-index tui-at-selector-index set-tui-at-selector-index!)
                    (active-todo-list tui-active-todo-list set-tui-active-todo-list!)
                    (last-render-error-sig tui-last-render-error-sig set-tui-last-render-error-sig!)
                    (role-pair-cache tui-role-pair-cache set-tui-role-pair-cache!)
                    (next-pair-id tui-next-pair-id set-tui-next-pair-id!)
                    (row-cache tui-row-cache)
                    (asm-messages tui-asm-messages set-tui-asm-messages!)
                    (asm-width tui-asm-width set-tui-asm-width!)
                    (asm-lines tui-asm-lines set-tui-asm-lines!)
                    (asm-counts tui-asm-counts set-tui-asm-counts!)
                    (asm-total tui-asm-total set-tui-asm-total!)
                    (asm-msg-count tui-asm-msg-count set-tui-asm-msg-count!)
                    (click-targets tui-click-targets)
                    (row-texts tui-row-texts)
                    (ui-server tui-ui-server)
                    (ui-client tui-ui-client)
                    (last-tab-title-sig tui-last-tab-title-sig set-tui-last-tab-title-sig!)
                    (subagent-bus-sub-id tui-subagent-bus-sub-id set-tui-subagent-bus-sub-id!)
                    (react-stream-content-chunks
                      tui-react-stream-content-chunks
                      set-tui-react-stream-content-chunks!)
                    (react-stream-thoughts-chunks
                      tui-react-stream-thoughts-chunks
                      set-tui-react-stream-thoughts-chunks!)
                    (react-stream-last-render
                      tui-react-stream-last-render
                      set-tui-react-stream-last-render!)
                    (last-spin-time tui-last-spin-time set-tui-last-spin-time!)
                    (steering-queue tui-steering-queue)
                    (steering-buffer tui-steering-buffer set-tui-steering-buffer!)
                    (interrupt-requested? tui-interrupt-requested? set-tui-interrupt-requested?!)
                    (steering-mutex tui-steering-mutex)
                    (force-stop-requested? tui-force-stop-requested? set-tui-force-stop-requested?!)
                    (urgent-steer-requested?
                      tui-urgent-steer-requested?
                      set-tui-urgent-steer-requested?!)
                    (mid-run-pending-commands
                      tui-mid-run-pending-commands
                      set-tui-mid-run-pending-commands!)
                    (current-run-id tui-current-run-id set-tui-current-run-id!)
                    (steering-selector-index
                      tui-steering-selector-index
                      set-tui-steering-selector-index!)
                    (render-mutex tui-render-mutex)
                    (render-dirty? tui-render-dirty? set-tui-render-dirty?!)
                    (agent-thread tui-agent-thread set-tui-agent-thread!)
                    (ctx-cache-tokens tui-ctx-cache-tokens set-tui-ctx-cache-tokens!)
                    (ctx-cache-key tui-ctx-cache-key set-tui-ctx-cache-key!)
                    (jrn-count-cache tui-jrn-count-cache set-tui-jrn-count-cache!)
                    (jrn-count-time tui-jrn-count-time set-tui-jrn-count-time!)
                    (jrn-count-refreshing? tui-jrn-count-refreshing? set-tui-jrn-count-refreshing?!)
                    (jrn-count-mutex tui-jrn-count-mutex))

(define*
  (make-tui-state #:key
                  nref
                  nodelay-proc
                  steering-err-val
                  cfg
                  (initial-api-key #f)
                  ui-theme
                  cur-endpoint
                  cur-model
                  cur-api-key
                  cur-journal-path
                  reg
                  jrn
                  mgr
                  mcp-mgr
                  policy
                  compactor
                  rollback-mgr
                  verifier
                  composer
                  client
                  ctx-idx
                  skill-reg
                  repl-mod
                  banner-tips
                  session-file
                  input-history
                  ui-server
                  ui-client)
  "Construct an initialized <tui-state> record."
  (%make-tui-state nref
                   #f
                   #f
                   nodelay-proc
                   steering-err-val
                   0
                   0
                   cfg
                   initial-api-key
                   ui-theme
                   cur-endpoint
                   cur-model
                   cur-api-key
                   cur-journal-path
                   reg
                   jrn
                   mgr
                   mcp-mgr
                   policy
                   compactor
                   rollback-mgr
                   verifier
                   composer
                   client
                   ctx-idx
                   skill-reg
                   '()
                   #f
                   repl-mod
                   banner-tips
                   0
                   (get-internal-real-time)
                   (not nodelay-proc)
                   '()
                   session-file
                   0
                   0
                   0
                   0
                   #f
                   0
                   ""
                   0
                   input-history
                   ""
                   0
                   0
                   0
                   "READY"
                   0
                   #f
                   #f
                   'chat
                   (make-default-keymap)
                   (make-history-search)
                   #f
                   0
                   (make-todo-list #:title "Tasks")
                   #f
                   '()
                   1
                   (make-row-cache)
                   #f
                   #f
                   '()
                   '()
                   0
                   0
                   (make-hash-table 61)
                   (make-hash-table 61)
                   ui-server
                   ui-client
                   #f
                   #f
                   '()
                   '()
                   0
                   0
                   (make-steering-queue)
                   ""
                   #f
                   (make-mutex)
                   #f
                   #f
                   '()
                   0
                   0
                   (make-mutex 'recursive)
                   #f
                   #f
                   #f
                   #f
                   -1
                   0
                   #f
                   (make-mutex)))

(define (tui-agent-running? st)
        "Return #t when the background agent thread in ST is active."
        (let ((t (tui-agent-thread st))) (and (thread? t) (not (thread-exited? t)))))

(define (tui-active-menu st)
        "Return the active <menu> if ST's active overlay is a menu overlay, or #f."
        (menu-overlay-menu (tui-active-overlay st)))

(define (tui-set-active-menu! st m)
        "Set ST's active overlay to wrap <menu> M, or clear it when M is #f."
        (set-tui-active-overlay! st (and m (make-menu-overlay m))))

(define (tui-clear-overlay! st)
        "Dismiss any active overlay on ST."
        (set-tui-active-overlay! st #f))

(define (tui-add-msg! st role content)
        "Append a message with ROLE and CONTENT to ST's transcript."
        (with-mutex (tui-render-mutex st)
                    (set-tui-messages! st
                                       (append (tui-messages st) (list `((role . ,role) (content . ,content)))))))

(define (tui-add-sys-msg! st content) (tui-add-msg! st "system" content))

(define (tui-add-err-msg! st content)
        (log-error!
          (if (string? content)
            content
            (format #f "~s" content))
          "tui")
        (tui-add-msg! st "error" content))

(define (tui-add-cmd-msg! st content) (tui-add-msg! st "shell-cmd" content))

(define (tui-add-scm-msg! st content) (tui-add-msg! st "scheme-in" content))

(define (tui-add-user-msg! st content) (tui-add-msg! st "user" content))

(define (tui-record-session-usage! st usage)
        "Accumulate one model call's USAGE alist into ST's session counters."
        (let
            ((p (assoc-ref usage 'prompt)) (c (assoc-ref usage 'completion)))
          (when (and (number? p) (number? c))
            (set-tui-session-prompt-tokens! st (+ (tui-session-prompt-tokens st) p))
            (set-tui-session-completion-tokens! st (+ (tui-session-completion-tokens st) c))
            (set-tui-turn-usage-seen?! st #t))))

(define (tui-toggle-selected-collapse! st)
        "Toggle collapse of the currently selected message card in ST."
        (let*
            ((msgs (tui-messages st))
             (idx (min (tui-msgsel-index st) (- (length msgs) 1)))
             (msg (list-ref msgs idx)))
          (when (message-collapsible? msg)
            (set-tui-messages! st (replace-nth msgs idx (message-toggle-collapsed msg))))))

(define (tui-record-history! st input mode)
        "Record INPUT under MODE in ST's shared input history and journal file."
        (let
            ((trimmed
               (string-trim-both
                 (if (string? input)
                   input
                   ""))))
          (when
              (history-add (tui-input-history st) trimmed)
            (append-history-file-entry! (default-history-file-path) trimmed mode))))

(define (tui-try-save-config! st)
        "Best-effort save of ST's configuration."
        (catch #t
          (lambda ()
            (save-local-config! (tui-cfg st)))
          (lambda _
            #f)))

(define (tui-sync-context-limit! st)
        "Update compactor max-tokens from explicit config or live server metadata."
        (let*
            ((cfg (tui-cfg st))
             (configured
               (badb-config-model-context-limit cfg (tui-cur-endpoint st) (tui-cur-model st)))
             (limit
               (or configured
                   (catch #t
                     (lambda ()
                       (fetch-model-context-limit (tui-client st)))
                     (lambda _
                       #f)))))
          (set-tui-context-limit-discovered?! st (and (integer? limit) (> limit 4096)))
          (set-context-compactor-max-tokens!
            (tui-compactor st)
            (if (tui-context-limit-discovered? st)
              limit
              16384))))

(define (tui-refresh-client! st)
        "Rebuild ST's model client, update repl-mod, and sync context limit."
        (let*
            ((cfg (tui-cfg st)) (ep (tui-cur-endpoint st))
             (cl
               (make-model-client #:endpoint
                                  ep
                                  #:api-key
                                  (tui-cur-api-key st)
                                  #:model
                                  (tui-cur-model st)
                                  #:allow-missing-thinking?
                                  (badb-config-allow-missing-thinking? cfg ep))))
          (set-tui-client! st cl)
          (module-define! (tui-repl-mod st) 'client cl)
          (tui-sync-context-limit! st)))

(define (tui-prefill-input! st text)
        "Place TEXT in ST's input buffer, close any overlay, and reset mode to 'chat."
        (tui-clear-overlay! st)
        (set-tui-prompt-mode! st 'chat)
        (set-tui-input-buffer! st text)
        (set-tui-cursor-pos! st (string-length text)))

(define (tui-make-system-turn st)
        "Build the system prompt turn for ST's llm-history."
        `((role . "system")
          (content .
                   ,(assemble-system-prompt (tui-composer st)
                                            #:workspace-dir
                                            (getcwd)
                                            #:tools
                                            (collect-available-tools (tui-reg st) (tui-mcp-mgr st))
                                            #:skills
                                            (skill-catalog-summary (tui-skill-reg st))
                                            #:memories
                                            (catch #t
                                              (lambda ()
                                                (memory-read "all"))
                                              (lambda _
                                                #f))
                                            #:static-only?
                                            #f))))

(define (tui-make-startup-notice-msg st frame)
        "Build the startup banner notice message for FRAME."
        (let
            ((banner-rows
               (format-startup-banner 108
                                      #:frame
                                      frame
                                      #:model
                                      (tui-cur-model st)
                                      #:workspace
                                      (getcwd)
                                      #:tips
                                      (tui-banner-tips st)))
             (banner-text
               (format-startup-banner-text 108
                                           #:frame
                                           frame
                                           #:model
                                           (tui-cur-model st)
                                           #:workspace
                                           (getcwd)
                                           #:tips
                                           (tui-banner-tips st))))
          `((role . "notice") (hide-label? . #t)
            (startup-banner? . #t)
            (styled-rows . ,banner-rows)
            (content . ,banner-text))))

(define (tui-advance-startup-banner! st)
        "Advance ST's startup banner animation by one frame if due. Returns #t if updated."
        (let ((msgs (tui-messages st)))
          (cond
            ((tui-banner-anim-done? st) #f)
            ((or (null? msgs) (not (assoc-ref (car msgs) 'startup-banner?)))
              (set-tui-banner-anim-done?! st #t)
              #f)
            ((>= (tui-banner-frame st) (- (crow-banner-frame-count) 1))
              (set-tui-banner-anim-done?! st #t)
              #f)
            (else
              (let*
                  ((now (get-internal-real-time))
                   (step (quotient internal-time-units-per-second 5)))
                (if (>= (- now (tui-banner-last-time st)) step)
                  (let ((next-frame (+ (tui-banner-frame st) 1)))
                    (set-tui-banner-frame! st next-frame)
                    (set-tui-banner-last-time! st now)
                    (set-tui-messages! st
                                       (cons (tui-make-startup-notice-msg st next-frame) (cdr msgs)))
                    (when
                        (>= next-frame (- (crow-banner-frame-count) 1))
                      (set-tui-banner-anim-done?! st #t))
                    #t)
                  #f))))))

(define (tui-reset-history-browse! st)
        "Reset ST's input-history browsing cursor."
        (history-reset-nav (tui-input-history st))
        (set-tui-history-draft! st "")
        (set-tui-history-draft-pos! st 0))

(define (tui-clamp-selector-index! st)
        "Clamp ST's slash and @-mention selector indices to current match counts."
        (when (selector-active? (tui-input-buffer st) (tui-prompt-mode st))
          (let ((matches (deduplicate-slash-commands (matching-slash-commands (tui-input-buffer st)))))
            (set-tui-selector-index!
              st (if (null? matches)
                   0
                   (max 0 (min (tui-selector-index st) (- (length matches) 1)))))))
        (when (at-selector-active? (tui-input-buffer st) (tui-cursor-pos st) (tui-prompt-mode st))
          (unless (tui-workspace-files-cache st)
            (set-tui-workspace-files-cache! st (collect-workspace-files)))
          (let ((cands (matching-at-candidates (tui-input-buffer st)
                                               #:cursor-pos (tui-cursor-pos st)
                                               #:candidates (tui-workspace-files-cache st)
                                               #:limit 25)))
            (set-tui-at-selector-index!
              st (if (null? cands)
                   0
                   (max 0 (min (tui-at-selector-index st) (- (length cands) 1))))))))

(define (tui-apply-edit! st new-buf new-pos)
        "Apply NEW-BUF and NEW-POS to ST's editor state."
        (set-tui-input-buffer! st new-buf)
        (set-tui-cursor-pos! st new-pos))

(define (tui-insert-burst! st burst)
        "Insert a pasted list of integer codepoints BURST into ST's input buffer."
        (let ((KEY_ENTER (and (tui-nref st) ((tui-nref st) 'KEY_ENTER))))
          (for-each
            (lambda (id)
              (let ((text (if (or (eqv? id 10) (eqv? id 13) (eqv? id KEY_ENTER))
                            (string #\newline)
                            (string (integer->char id)))))
                (call-with-values
                  (lambda () (insert-at (tui-input-buffer st) (tui-cursor-pos st) text))
                  (lambda (nb np) (tui-apply-edit! st nb np)))))
            burst)
          (tui-clamp-selector-index! st)))