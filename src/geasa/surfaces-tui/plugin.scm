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
               #:use-module
               (geasa providers-model common)
               #:use-module
               (geasa providers-model openai)
               #:use-module
               (badb kernel registry)
               #:use-module
               (badb kernel transaction)
               #:use-module
               (badb kernel journal)
               #:use-module
               (geasa foundation-config plugin)
               #:use-module
               (geasa capabilities-mcp plugin)
               #:use-module
               (geasa capabilities-history-tools plugin)
               #:use-module
               (geasa policies-guardrail plugin)
               #:use-module
               (geasa runtimes-context plugin)
               #:use-module
               (geasa runtimes-prompt plugin)
               #:use-module
               (geasa runtimes-rollback plugin)
               #:use-module
               (geasa capabilities-context-index plugin)
               #:use-module
               (geasa capabilities-skills plugin)
               #:use-module
               (geasa runtimes-geis-loader plugin)
               #:use-module
               (geasa runtimes-workflows plugin)
               #:use-module
               (geasa runtimes-verify plugin)
               #:use-module
               (geasa surfaces-theme plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (geasa surfaces-tui editor)
               #:use-module
               (geasa surfaces-tui session)
               #:use-module
               (geasa surfaces-tui transcript)
               #:use-module
               (geasa surfaces-tui markdown)
               #:use-module
               (geasa surfaces-tui widgets)
               #:use-module
               (geasa surfaces-tui protocol)
               #:use-module
               (geasa surfaces-tui daemon)
               #:use-module
               (geasa surfaces-tui menus)
               #:use-module
               (geasa surfaces-tui tools)
               #:use-module
               (geasa surfaces-tui commands)
               #:use-module
               (geasa surfaces-tui steering)
               #:use-module
               (geasa surfaces-tui state)
               #:use-module
               (geasa surfaces-tui renderer)
               #:use-module
               (geasa surfaces-tui events)
               #:use-module
               (geasa agents-lifecycle plugin)
               #:use-module
               (geasa agents-coordination plugin)
               #:use-module
               (geasa agents-subagent plugin)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 optargs)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (ice-9 threads)
               #:export
               (run-tui collect-enter-burst
                        nc-shift-held?
                        format-response-images-notice
                        render-ncvisual-overlay!
                        show-geis-bindings-uplot!
                        make-tui-semantic-summarizer
                        format-geis-help-text
                        tui-theme-attr-constant
                        tui-theme-color->backend-value)
               #:re-export
               (ui-client-send-event ui-client-connected?
                                     ui-client-notifications
                                     ui-message-type
                                     <ui-daemon>
                                     make-ui-daemon
                                     ui-daemon?
                                     ui-daemon-server
                                     ui-daemon-transport
                                     ui-daemon-host
                                     ui-daemon-port
                                     ui-daemon-socket-path
                                     ui-daemon-status
                                     ui-daemon-endpoint
                                     ui-daemon-client-count
                                     ui-daemon-start!
                                     ui-daemon-stop!
                                     ui-daemon-wait
                                     <ui-remote-connection>
                                     ui-remote-connection?
                                     ui-remote-connection-client
                                     ui-remote-connection-connected?
                                     ui-remote-connection-wait
                                     ui-client-connect-unix!
                                     ui-client-connect-tcp!
                                     ui-client-connect-ports!
                                     ui-client-connect-ssh!
                                     ui-remote-disconnect!
                                     parse-ui-listen-spec
                                     wire-ui-server-handlers!
                                     run-ui-server!
                                     run-remote-tui-client
                                     %mid-run-safe-commands
                                     %mid-run-slash-command-entries
                                     mid-run-safe-command?
                                     parse-steer-argument
                                     dispatch-mid-run-input
                                     matching-mid-run-slash-commands
                                     push-urgent-steering!
                                     force-stop-agent!
                                     format-steering-keymap-line
                                     steering-bar-layout
                                     format-mid-run-subagents-summary
                                     span-rows->text
                                     handle-todo-command!
                                     format-response-widgets-notice
                                     format-conversation-transcript
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
                                     prompt-mode-on-char
                                     prompt-mode-on-backspace
                                     prompt-mode-on-escape
                                     prompt-mode-submit-text
                                     format-slash-completion
                                     fuzzy-score
                                     fuzzy-match-history
                                     <history-search>
                                     history-search?
                                     make-history-search
                                     history-search-active?
                                     history-search-open
                                     history-search-handle-key
                                     history-search-matches
                                     history-search-selected-index
                                     extract-at-token
                                     at-selector-active?
                                     collect-workspace-files
                                     matching-at-candidates
                                     apply-at-completion
                                     <keymap>
                                     keymap?
                                     make-keymap
                                     make-default-keymap
                                     keymap-bind!
                                     keymap-unbind!
                                     keymap-lookup
                                     keymap-bindings
                                     keymap->menu-items
                                     key-event->spec
                                     resolve-user-shell
                                     resolve-external-editor
                                     open-in-external-editor
                                     read-via-external-editor
                                     session-turns->transcript-messages
                                     session-file-metadata
                                     recent-session-summaries
                                     repl-help
                                     repl-inspect
                                     repl-geasa
                                     repl-state
                                     %menu-max-visible
                                     make-menu
                                     menu?
                                     menu-title
                                     menu-items
                                     menu-selected-index
                                     menu-count
                                     menu-move
                                     menu-trigger-highlight!
                                     menu-push
                                     menu-pop
                                     menu-depth
                                     menu-breadcrumbs
                                     menu-breadcrumb-title
                                     menu-selected-item
                                     menu-visible-slice
                                     menu-item-label
                                     menu-item-description
                                     menu-item-action
                                     provider-menu-items
                                     synthesizer-config-menu-items
                                     synthesizer-provider-menu-items
                                     synthesizer-model-menu-items
                                     apply-synthesis-provider-config!
                                     session-resurrection-menu-items
                                     make-theme-preview-menu
                                     %command-categories
                                     command-category-menu-items
                                     command-items-for-category
                                     make-commands-root-menu
                                     open-command-category-menu
                                     %command-aliases
                                     resolve-command-alias
                                     deduplicate-slash-commands
                                     execute-context-subcommand!
                                     format-config-summary
                                     format-provider-presets
                                     format-guardrail-status
                                     sparkline-plot
                                     format-geis-bindings-graph
                                     get-prompt-indicator
                                     format-header-segments
                                     format-rule-line
                                     format-keymap-line
                                     steering-key-handle
                                     steering-pending-line
                                     agent-run-active?
                                     spinner-active-status?
                                     format-osc-tab-title
                                     reset-osc-tab-title
                                     segment-line
                                     block-row-segments
                                     message-role-icon
                                     format-card-header
                                     format-card-divider
                                     card-gutter-glyph
                                     render-markdown
                                     render-markdown-lines
                                     segment-input-line
                                     highlight-scheme-line
                                     highlight-shell-line
                                     format-osc8-link
                                     extract-clickable-links
                                     find-link-at-column
                                     nerd-fonts-enabled?
                                     nerd-icon
                                     detect-image-protocol
                                     kitty-graphics-supported?
                                     render-image-ncvisual
                                     <widget>
                                     make-widget
                                     widget?
                                     box-widget
                                     rule-widget
                                     pill-widget
                                     scroll-view-widget
                                     format-input-box-frame
                                     format-bottom-status-bar
                                     %crow-banner-frames
                                     format-startup-banner
                                     format-startup-banner-text
                                     format-git-diff-view
                                     <todo-list>
                                     make-todo-list
                                     todo-add
                                     todo-toggle
                                     todo-remove
                                     todo-clear-done
                                     todo-counts
                                     format-todo-widget
                                     render-scheme-response-widget
                                     extract-response-widgets
                                     <ui-server>
                                     <ui-client>
                                     make-ui-server
                                     make-ui-client
                                     ui-client-subagent-events
                                     ui-server-route-subagent-event!
                                     pump-ui-pair!
                                     current-harness-id
                                     parent-harness-id
                                     subagent-harness?
                                     register-subagent-link!
                                     subagent-hierarchy-tree
                                     format-subagent-hierarchy
                                     format-subagent-overview
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
                                     row-cache-invalidate!
                                     row-cache-clear!
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
(define
  (collect-enter-burst ch input-pending? drain-input-burst!)
  "Return (cons ch burst) when Enter is part of a multiline paste
(INPUT-PENDING? is true and DRAIN-INPUT-BURST! returns at least one
payload codepoint), or #f when Enter should submit."
  (and (input-pending?)
       (let ((burst (drain-input-burst!))) (and (pair? burst) (cons ch burst)))))

(define*
  (nc-shift-held? #:optional
                  (ncinput
                    (let
                        ((m (false-if-exception (resolve-module '(notcurses compat curses) #f))))
                      (and m (false-if-exception (module-ref m '%ncinput)))))
                  (get-mods
                    (let
                        ((m (false-if-exception (resolve-module '(notcurses structs) #f))))
                      (and m (false-if-exception (module-ref m 'ncinput-modifiers)))))
                  (mod-shift
                    (let
                        ((m (false-if-exception (resolve-module '(notcurses constants) #f))))
                      (and m (false-if-exception (module-ref m 'NCKEY_MOD_SHIFT))))))
  "Return #t when NCINPUT reports the Shift modifier bit MOD-SHIFT."
  (and ncinput
       get-mods
       mod-shift
       (not (zero? (logand (get-mods ncinput) mod-shift)))))

;;;
;;; Git Diff, OSC Title & Theme Helpers
;;;
(define* (format-git-diff-output #:optional (arg "") (width 78))
         "Run git diff (optionally with safe path/ref tokens from ARG) and format
the output with WP1 format-git-diff-view."
         (let* ((extra-args
                  (filter (lambda (s) (not (string-null? s)))
                          (string-split (string-trim-both (or arg "")) #\space)))
                (out
                  (catch #t
                    (lambda ()
                      (let* ((p (apply open-pipe* OPEN_READ "git" "diff" extra-args))
                             (txt (read-string p))
                             (_st (close-pipe p)))
                        (or txt "")))
                    (lambda _ ""))))
           (if (string-null? (string-trim-both out))
             "No unstaged git diff changes."
             (span-rows->text (format-git-diff-view out width #:title "git diff")))))

(define (tui-theme-attr-constant attr nref)
        "Resolve theme attribute symbol ATTR using backend NREF."
        (case attr
          ((bold) (nref 'A_BOLD))
          ((dim) (nref 'A_DIM))
          ((reverse) (nref 'A_REVERSE))
          (else #f)))

(define (tui-theme-color->backend-value color nref)
        "Resolve theme COLOR (hex #rrggbb or curses symbol) via backend NREF."
        (if (string? color)
          (string->number (substring color 1) 16)
          (nref (color-symbol->ncurses-constant color))))

(define (format-geis-help-text)
        "Return help text for the /geis slash command."
        (string-append
          "Geis (Contract Gate & Dynamic Synthesis):\n"
          "  /geis add <description>            - Prompt agent to draft spec, lock contract, synthesize module, and hot-attach\n"
          "  /geis model [name]                 - Inspect or set default model for geis contract synthesis\n"
          "  /geis list                         - List all locked feature contracts (geasa)\n"
          "  /geis bindings                     - Show shifting geis bindings & journal plot (native ncuplot + graph)\n"
          "  /geis status [feature-id]          - Check status/hash of locked contract(s)\n"
          "  /geis verify <feature-id>          - Replay the LOCKED copy (.badb/) in a disposable kernel subprocess; if stale, re-lock via ./scripts/verify-geasa, never copy into .badb/\n"
          "  /geis synthesize <feature-id> ...  - Run Sema plugin synthesis for locked feature\n\n"
          "Example:\n"
          "  /geis add retrieval for the numpy docs\n"))

(define (make-tui-semantic-summarizer client)
        "Build a semantic context-compaction summarizer closure backed by CLIENT."
        (lambda (older-msgs)
          (let* ((rendered
                   (let loop ((rest older-msgs) (acc '()) (total 0))
                             (if (or (null? rest) (>= total 24000))
                               (string-join (reverse acc) "\n")
                               (let* ((m (car rest))
                                      (line (format #f "~a: ~a"
                                                    (or (assoc-ref m 'role) "?")
                                                    (or (assoc-ref m 'content) "")))
                                      (len (string-length line)))
                                 (loop (cdr rest) (cons line acc) (+ total len 1))))))
                 (truncate-to
                   (lambda (s n)
                     (if (> (string-length s) n)
                       (string-append (substring s 0 n) "\n[...verbatim fallback truncated...]")
                       s))))
            (catch #t
              (lambda ()
                (let* ((sys-msg
                         '((role . "system")
                           (content . "You are a context-compaction engine. Produce a state block with EXACTLY these sections: Pinned Constraints; Verified State / File Map; Rejected Approaches; Exact Identifiers. Keep identifiers, file paths, hashes and error codes verbatim. Never claim work completed without evidence.")))
                       (req-msg
                         `((role . "user")
                           (content . ,(format #f "Summarize the following ~a conversation turns into the schema state block:\n\n~a"
                                               (length older-msgs) rendered))))
                       (resp (model-complete client (list sys-msg req-msg) #:max-tokens 1024)))
                  (if (and (string? resp) (not (string-null? (string-trim-both resp))))
                    (format #f "[Context Summary of preceding turns]:\n~a" resp)
                    (truncate-to (format #f "[Context Summary of preceding turns]:\n~a" rendered) 3000))))
              (lambda _
                (truncate-to (format #f "[Context Summary of preceding turns]:\n~a" rendered) 3000))))))

(define* (format-response-images-notice text #:optional (maxx 76))
         "Extract existing local image paths from Markdown ![alt](path) in TEXT
and return a notice string with placeholders and /image preview hints, or #f."
         (let* ((paths (filter file-exists? (extract-markdown-image-paths text))))
           (if (null? paths)
             #f
             (string-join
               (map (lambda (p)
                      (string-append (format-image-placeholder p #:width (max 20 (- maxx 18)))
                                     " — /image " p))
                    paths)
               "\n"))))

(define*
  (render-ncvisual-overlay! stdscr
                            path
                            getch-fn
                            #:key
                            (max-rows 20)
                            (max-cols 60))
  "Display image PATH in a centered notcurses child plane using ncvisual,
wait for a key via GETCH-FN, and clean up. Returns info alist on success or #f."
  (and stdscr
       (string? path)
       (file-exists? path)
       (catch #t
         (lambda ()
           (let*
               ((plane-mod (false-if-exception (resolve-module '(notcurses plane) #f)))
                (core-mod (false-if-exception (resolve-module '(notcurses core) #f)))
                (structs-mod (false-if-exception (resolve-module '(notcurses structs) #f))))
             (and plane-mod
                  core-mod
                  structs-mod
                  (let*
                      ((nc ((module-ref plane-mod 'ncplane_notcurses) stdscr))
                       (make-opts (module-ref structs-mod 'make-ncplane-options))
                       (->ptr (module-ref structs-mod '->pointer))
                       (ncplane-create (module-ref plane-mod 'ncplane_create))
                       (ncplane-destroy (module-ref plane-mod 'ncplane_destroy))
                       (nc-render (module-ref core-mod 'notcurses_render))
                       (opts (make-opts #:y 2 #:x 4 #:rows max-rows #:cols max-cols))
                       (child (ncplane-create stdscr (->ptr opts))))
                    (and child
                         (dynamic-wind
                           (lambda ()
                             #t)
                           (lambda ()
                             (let
                                 ((info
                                    (render-image-ncvisual nc
                                                           child
                                                           path
                                                           #:y
                                                           0
                                                           #:x
                                                           0
                                                           #:max-rows
                                                           max-rows
                                                           #:max-cols
                                                           max-cols)))
                               (when info
                                 (nc-render nc)
                                 (when (procedure? getch-fn) (getch-fn)))
                               info))
                           (lambda ()
                             (false-if-exception (ncplane-destroy child)))))))))
         (lambda _
           #f))))

(define*
  (show-geis-bindings-uplot! stdscr jrn getch-fn #:key (rows 12) (cols 56))
  "Render a native notcurses ncuplot overlay of journal geis-binding activity
when running in a live notcurses session. Returns #t if displayed, #f otherwise."
  (and stdscr
       jrn
       (catch #t
         (lambda ()
           (let*
               ((entries (journal-entries jrn))
                (plane-mod (false-if-exception (resolve-module '(notcurses plane) #f)))
                (core-mod (false-if-exception (resolve-module '(notcurses core) #f)))
                (structs-mod (false-if-exception (resolve-module '(notcurses structs) #f)))
                (widgets-mod (false-if-exception (resolve-module '(notcurses widgets) #f))))
             (and (pair? entries)
                  plane-mod
                  core-mod
                  structs-mod
                  widgets-mod
                  (let*
                      ((nc ((module-ref plane-mod 'ncplane_notcurses) stdscr))
                       (make-popts (module-ref structs-mod 'make-ncplane-options))
                       (make-uopts (module-ref structs-mod 'make-ncplot-options))
                       (->ptr (module-ref structs-mod '->pointer))
                       (ncplane-create (module-ref plane-mod 'ncplane_create))
                       (ncplane-destroy (module-ref plane-mod 'ncplane_destroy))
                       (ncuplot-create (module-ref widgets-mod 'ncuplot_create))
                       (ncuplot-set (module-ref widgets-mod 'ncuplot_set_sample))
                       (ncuplot-destroy (module-ref widgets-mod 'ncuplot_destroy))
                       (nc-render (module-ref core-mod 'notcurses_render))
                       (n (min 16 (length entries)))
                       (recent (take (reverse entries) n))
                       (max-y (max 1 (length recent)))
                       (child
                         (ncplane-create stdscr (->ptr (make-popts #:y 3 #:x 4 #:rows rows #:cols cols)))))
                    (and child
                         (let ((plot #f))
                           (dynamic-wind
                             (lambda ()
                               #t)
                             (lambda ()
                               (set! plot (ncuplot-create child (->ptr (make-uopts)) 0 max-y))
                               (let loop
                                 ((i 0) (rest recent))
                                 (unless (null? rest)
                                   (let*
                                       ((e (car rest))
                                        (kind (or (assoc-ref e 'kind) 'event))
                                        (w
                                          (cond
                                            ((eq? kind 'commit) 4)
                                            ((eq? kind 'checkpoint) 3)
                                            ((eq? kind 'rollback) 5)
                                            (else 2))))
                                     (ncuplot-set plot i (+ w (modulo i 3)))
                                     (loop (+ i 1) (cdr rest)))))
                               (nc-render nc)
                               (when (procedure? getch-fn) (getch-fn))
                               #t)
                             (lambda ()
                               (when plot (false-if-exception (ncuplot-destroy plot)))
                               (false-if-exception (ncplane-destroy child))))))))))
         (lambda _
           #f))))

(define (startup-auto-detect-model! st)
        (let*
            ((cfg (tui-cfg st))
             (cur-model (tui-cur-model st))
             (config-model-was-generic? (generic-model-name? (badb-config-model-name cfg))))
          (call-with-values
            (lambda ()
              (if (generic-model-name? cur-model)
                (catch #t
                  (lambda ()
                    (auto-detect-model (tui-client st)))
                  (lambda (key . args)
                    (values #f (format #f "Detection error (~a): ~s" key args))))
                (values #f #f)))
            (lambda (detected-model detection-error)
              (call-with-values
                (lambda ()
                  (detect-model-decision cur-model detected-model detection-error))
                (lambda (new-model kind)
                  (case kind
                    ((adopted) (set-tui-cur-model! st new-model)
                               (badb-config-set-model-name! cfg new-model)
                               (tui-refresh-client! st)
                               (when config-model-was-generic? (tui-try-save-config! st))
                               (tui-add-msg! st "notice" (format #f "model auto-detected: ~a" new-model)))
                    ((failed)
                      (tui-add-msg! st
                                    "notice"
                                    (format #f "model detection failed: ~a" detection-error)))
                    ((unavailable)
                      (tui-add-msg! st "notice" "model detection found no models on the endpoint"))
                    (else #f))))))))

(define (shutdown-tui! st)
        (let*
            ((nref (tui-nref st)) (endwin (nref 'endwin)) (t (tui-agent-thread st)))
          (kill-active-subprocess!)
          (catch #t
            (lambda ()
              (harness-event-unsubscribe! (tui-subagent-bus-sub-id st)))
            (lambda _
              #f))
          (tui-safe-write-osc-title! (reset-osc-tab-title))
          (when (and t (not (thread-exited? t)))
            (catch #t
              (lambda ()
                (cancel-thread t))
              (lambda _
                #f)))
          (catch #t
            (lambda ()
              (mcp-manager-stop-all! (tui-mcp-mgr st)))
            (lambda _
              #f))
          (tui-safe-curs-set! st 1)
          (catch #t
            (lambda ()
              (endwin))
            (lambda _
              #f))))

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
          (false-if-exception (resolve-interface '(notcurses compat curses)))
          #t)
        (lambda _
          #f))
    (format (current-error-port)
            "Error: (notcurses compat curses) notcurses backend is not available.~%~
               Please run badb within the Guix dev environment:~%~
                 ./scripts/devenv -- ./scripts/badb~%")
    (exit 1))
  (parameterize
    ((current-warning-port (%make-void-port "w")))
    (let*
        ((ncurses-mod (resolve-module '(notcurses compat curses)))
         (nref
           (lambda (s)
             (module-ref ncurses-mod s)))
         (initscr (nref 'initscr))
         (cbreak! (nref 'cbreak!))
         (noecho! (nref 'noecho!))
         (keypad! (nref 'keypad!))
         (has-colors? (nref 'has-colors?))
         (start-color! (nref 'start-color!))
         (key-defined (nref 'key-defined))
         (define-key (nref 'define-key))
         (drain-mouse-events (nref 'drain-mouse-events))
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
         (cfg (or config (load-config)))
         (ui-theme
           (let ((configured (badb-config-ui-theme cfg)))
             (or (and (string? configured) (lookup-theme configured)) (default-theme))))
         (cur-endpoint
           (or endpoint (getenv "BADB_MODEL_ENDPOINT") (badb-config-model-endpoint cfg)))
         (cur-model (or model (getenv "BADB_MODEL_NAME") (badb-config-model-name cfg)))
         (cur-api-key
           (or api-key
               (getenv "BADB_API_KEY")
               (getenv "OPENAI_API_KEY")
               (badb-config-model-api-key cfg)))
         (cur-journal-path
           (or journal-path (getenv "BADB_JOURNAL_PATH") (badb-config-journal-path cfg)))
         (reg (make-registry))
         (jrn (make-journal cur-journal-path))
         (mgr (make-transaction-manager reg #:journal jrn))
         (mcp-mgr (make-mcp-manager cfg))
         (policy (make-guardrail-policy #:level 'cautious))
         (compactor
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
         (rollback-mgr
           (make-rollback-manager #:workspace-dir (getcwd) #:registry reg #:journal jrn))
         (verifier (make-verifier #:check-syntax? #t #:run-tests? #f))
         (composer (make-prompt-composer))
         (client
           (make-model-client #:endpoint
                              cur-endpoint
                              #:api-key
                              cur-api-key
                              #:model
                              cur-model
                              #:allow-missing-thinking?
                              (badb-config-allow-missing-thinking? cfg cur-endpoint)))
         (ctx-idx (make-bm25-index))
         (skill-reg (make-skill-registry))
         (banner-tips
           (list "! shell · ( scheme · / commands · /help"
                 "ctrl+r history · @ files · ctrl+x $EDITOR · /todo · /diff"
                 (if (file-exists? ".badb/geasa")
                   "geasa active · /geis list · /geis bindings"
                   "Tip: run /geis onboard to detect toolchain & synthesize geasa")))
         (ui-server
           (make-ui-server #:initial-state
                           (list (cons 'model cur-model) (cons 'status "READY"))))
         (ui-client (make-ui-client #:id 'local-tui))
         (repl-mod
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
         (st
           (make-tui-state #:nref
                           nref
                           #:nodelay-proc
                           nodelay-proc
                           #:steering-err-val
                           steering-err-val
                           #:cfg
                           cfg
                           #:initial-api-key
                           api-key
                           #:ui-theme
                           ui-theme
                           #:cur-endpoint
                           cur-endpoint
                           #:cur-model
                           cur-model
                           #:cur-api-key
                           cur-api-key
                           #:cur-journal-path
                           cur-journal-path
                           #:reg
                           reg
                           #:jrn
                           jrn
                           #:mgr
                           mgr
                           #:mcp-mgr
                           mcp-mgr
                           #:policy
                           policy
                           #:compactor
                           compactor
                           #:rollback-mgr
                           rollback-mgr
                           #:verifier
                           verifier
                           #:composer
                           composer
                           #:client
                           client
                           #:ctx-idx
                           ctx-idx
                           #:skill-reg
                           skill-reg
                           #:repl-mod
                           repl-mod
                           #:banner-tips
                           banner-tips
                           #:session-file
                           (session-file-path)
                           #:input-history
                           (make-history (load-history-file (default-history-file-path)))
                           #:ui-server
                           ui-server
                           #:ui-client
                           ui-client))
         (hooks
           `((format-geis-help-text . ,format-geis-help-text)
             (format-git-diff-output . ,format-git-diff-output)
             (render-ncvisual-overlay! . ,render-ncvisual-overlay!)
             (show-geis-bindings-uplot! . ,show-geis-bindings-uplot!)
             (spec-tool-status . ,spec-tool-status)
             (spec-tool-verify . ,spec-tool-verify))))
      (catch #t
        (lambda ()
          (discover-skills! skill-reg)
          (discover-and-mount-project-geasa! reg #:workspace (getcwd))
          (mount-bundle-workflows! reg #:workspace (getcwd)))
        (lambda _
          #f))
      (register-builtin-tui-tools! reg
                                   rollback-mgr
                                   verifier
                                   mcp-mgr
                                   (lambda ()
                                     (tui-llm-history st))
                                   #:ctx-idx
                                   ctx-idx
                                   #:skill-reg
                                   skill-reg
                                   #:client
                                   (tui-client st)
                                   #:config
                                   cfg
                                   #:policy
                                   policy)
      (auto-start-mcp-servers! cfg mcp-mgr)
      (set-context-compactor-strategy! compactor
                                       (strategy-cascading #:keep-recent-turns
                                                           6
                                                           #:max-tool-chars
                                                           1200
                                                           #:pin-first-user
                                                           #t
                                                           #:summarize-fn
                                                           (make-tui-semantic-summarizer (tui-client st))))
      (tui-sync-context-limit! st)
      (set-tui-messages! st (list (tui-make-startup-notice-msg st 0)))
      (set-tui-llm-history! st (list (tui-make-system-turn st)))
      (set-tui-subagent-bus-sub-id! st
                                    (harness-event-subscribe!
                                      (lambda (ev)
                                        (let
                                            ((src (harness-event-source-id ev))
                                             (tgt (or (harness-event-target-id ev) (current-harness-id)))
                                             (kind (harness-event-kind ev))
                                             (payload (harness-event-payload ev)))
                                          (catch #t
                                            (lambda ()
                                              (ui-server-route-subagent-event! ui-server src tgt kind payload))
                                            (lambda _
                                              #f))
                                          (set-tui-render-dirty?! st #t)))
                                      #:target-id
                                      (current-harness-id)))
      (startup-auto-detect-model! st)
      (let ((stdscr (initscr)))
        (set-tui-stdscr! st stdscr)
        (cbreak!)
        (noecho!)
        (keypad! stdscr #t)
        (set-tui-ctrl-up-key! st
                              (resolve-modified-key
                                (string #\esc #\[ #\1 #\; #\5 #\A)
                                key-defined
                                define-key
                                KEY_FIND))
        (set-tui-ctrl-down-key! st
                                (resolve-modified-key
                                  (string #\esc #\[ #\1 #\; #\5 #\B)
                                  key-defined
                                  define-key
                                  KEY_OPEN))
        (tui-safe-curs-set! st 1)
        (set-tui-colors-enabled?! st (has-colors?))
        (when (tui-colors-enabled? st) (start-color!))
        (dynamic-wind
          (lambda ()
            #t)
          (lambda ()
            (tui-run-event-loop! st
                                 input-pending?
                                 drain-input-burst!
                                 drain-mouse-events
                                 hooks))
          (lambda ()
            (shutdown-tui! st)))))))