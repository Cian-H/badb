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
               (badb kernel geis-gate)
               #:use-module
               (badb geis bundle)
               #:use-module
               (badb kernel digest)
               #:use-module
               (geasa foundation-config plugin)
               #:use-module
               (geasa capabilities-mcp plugin)
               #:use-module
               (geasa capabilities-macro plugin)
               #:use-module
               (geasa capabilities-history-tools plugin)
               #:use-module
               (geasa runtimes-react plugin)
               #:use-module
               (geasa runtimes-loop-guard plugin)
               #:use-module
               (geasa providers-systemone plugin)
               #:use-module
               (geasa policies-systemone-gate plugin)
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
               (geasa capabilities-code-nav plugin)
               #:use-module
               (geasa runtimes-geis-loader plugin)
               #:use-module
               (geasa runtimes-workflows plugin)
               #:use-module
               (geasa runtimes-verify plugin)
               #:use-module
               (geasa surfaces-theme plugin)
               #:use-module
               (geasa foundation-log plugin)
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
               (geasa surfaces-tui menus)
               #:use-module
               (geasa surfaces-tui tools)
               #:use-module
               (geasa surfaces-tui commands)
               #:use-module
               (geasa agents-lifecycle plugin)
               #:use-module
               (geasa agents-coordination plugin)
               #:use-module
               (geasa agents-subagent plugin)
               #:use-module
               (json)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 optargs)
               #:use-module
               (ice-9 regex)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:use-module
               (srfi srfi-11)
               #:use-module
               (srfi srfi-19)
               #:use-module
               (ice-9 threads)
               #:export
               (run-tui collect-enter-burst
                        nc-shift-held?
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
                        handle-todo-command!
                        format-response-widgets-notice
                        format-response-images-notice
                        render-ncvisual-overlay!
                        show-geis-bindings-uplot!
                        make-tui-semantic-summarizer
                        format-geis-help-text
                        tui-theme-attr-constant
                        tui-theme-color->backend-value)
               #:re-export
               (format-conversation-transcript wrap-line
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
;; DEV-DEBT [complexity]: run-tui is the largest form in the repo (9209 AST nodes,
;; cyclomatic ~447 per ast_complexity_metrics). Covered by the 582-assertion bundle
;; spec, but the prime candidate for decomposition into smaller units.
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
;;; Mid-Run Steering, Safe Slash Commands & Force-Stop Helpers
;;;
(define %steering-op-mutex (make-mutex))

(define %mid-run-safe-commands
        '("context" "usage" "theme" "help" "?" "subagents" "stop" "steer" "interrupt"))

(define %mid-run-slash-command-entries
        '(("context" . "View conversation token stats and metrics")
          ("usage" . "Show per-session token usage report")
          ("theme" . "Inspect or switch the active UI theme")
          ("help" . "Show help manual & shortcuts")
          ("subagents" . "Inspect active subagents and hierarchy")
          ("stop" . "Force-stop the running agent immediately")
          ("steer" . "Queue or urgently push steering (! / --urgent)")
          ("interrupt" . "Halt the running agent at its next boundary")))

(define (mid-run-safe-command? cmd)
        "Return #t when CMD (with or without leading '/') is safe to invoke while
%agent-thread is running."
        (and (string? cmd)
             (let* ((trimmed (string-trim-both cmd))
                    (bare
                      (if (string-prefix? "/" trimmed)
                        (substring trimmed 1)
                        trimmed))
                    (space-idx (string-index bare #\space))
                    (name
                      (string-downcase
                        (if space-idx
                          (substring bare 0 space-idx)
                          bare))))
               (and (not (string-null? name))
                    (member name %mid-run-safe-commands)
                    #t))))

(define (parse-steer-argument arg)
        "Parse /steer ARG for an urgency prefix ('--urgent ' or '! ').
Return (values urgent? cleaned-text)."
        (let ((trimmed (string-trim-both (if (string? arg) arg ""))))
          (cond
            ((string-prefix? "--urgent " trimmed)
              (values #t (string-trim-both (substring trimmed 9))))
            ((string=? "--urgent" trimmed)
              (values #t ""))
            ((string-prefix? "! " trimmed)
              (values #t (string-trim-both (substring trimmed 2))))
            ((and (> (string-length trimmed) 1) (string-prefix? "!" trimmed))
              (values #t (string-trim-both (substring trimmed 1))))
            (else (values #f trimmed)))))

(define (dispatch-mid-run-input raw-input)
        "Classify RAW-INPUT typed into the steering bar while the agent runs.
Returns one of:
  '(noop)
  '(safe-slash CMD ARG)
  '(blocked-slash CMD ARG)
  '(steer . TEXT)"
        (let ((trimmed (string-trim-both (if (string? raw-input) raw-input ""))))
          (if (string-null? trimmed)
            '(noop)
            (let ((classified (classify-input trimmed)))
              (if (and (pair? classified) (eq? (car classified) 'slash))
                (let ((cmd (cadr classified)) (arg (caddr classified)))
                  (if (mid-run-safe-command? cmd)
                    (list 'safe-slash (string-downcase cmd) arg)
                    (list 'blocked-slash cmd arg)))
                (cons 'steer trimmed))))))

(define (matching-mid-run-slash-commands buffer)
        "Return matching %mid-run-slash-command-entries for slash BUFFER."
        (let* ((trimmed (if (string? buffer) buffer ""))
               (prefix
                 (if (string-prefix? "/" trimmed)
                   (substring trimmed 1)
                   trimmed))
               (q (string-downcase (string-trim-both prefix))))
          (if (string-null? q)
            %mid-run-slash-command-entries
            (filter
              (lambda (entry)
                (string-contains (string-downcase (car entry)) q))
              %mid-run-slash-command-entries))))

(define* (push-urgent-steering! queue text
                                #:key
                                (on-urgent #f)
                                (kill-subprocess! #f))
         "Immediately push TEXT to the front of STEERING-QUEUE ahead of any
already queued steering entries. Optionally invokes KILL-SUBPROCESS! and
(ON-URGENT clean-text). Returns #t if pushed, #f otherwise."
         (let ((clean (and (string? text) (string-trim-both text))))
           (if (or (not (steering-queue? queue)) (not clean) (string-null? clean))
             #f
             (begin
               (with-mutex %steering-op-mutex
                           (let ((existing (steering-drain! queue)))
                             (steering-push! queue clean)
                             (for-each
                               (lambda (entry)
                                 (steering-push! queue entry))
                               existing)))
               (when (procedure? kill-subprocess!)
                 (catch #t (lambda () (kill-subprocess!)) (lambda _ #f)))
               (when (procedure? on-urgent)
                 (catch #t (lambda () (on-urgent clean)) (lambda _ #f)))
               #t))))

(define* (force-stop-agent! thread
                            #:key
                            (steering-queue #f)
                            (kill-subprocess! kill-active-subprocess!)
                            (on-stop #f))
         "Immediately force-stop a running agent THREAD by killing any active
subprocess, pushing 'interrupt onto STEERING-QUEUE, and calling cancel-thread.
Returns an alist: ((stopped? . BOOL) (thread-cancelled? . BOOL) (subprocess-killed? . BOOL))."
         (let ((subproc-killed? #f) (thread-cancelled? #f))
           (when (procedure? kill-subprocess!)
             (let ((had-pid?
                     (catch #t
                       (lambda ()
                         (let ((pid (active-subprocess-pid)))
                           (and (number? pid) (> pid 0))))
                       (lambda _ #f))))
               (catch #t (lambda () (kill-subprocess!)) (lambda _ #f))
               (set! subproc-killed? (and had-pid? #t))))
           (when (steering-queue? steering-queue)
             (with-mutex %steering-op-mutex
                         (steering-push! steering-queue 'interrupt)))
           (when (and (thread? thread) (not (thread-exited? thread)))
             (catch #t
               (lambda ()
                 (cancel-thread thread)
                 (set! thread-cancelled? #t))
               (lambda _ #f)))
           (when (procedure? on-stop)
             (catch #t (lambda () (on-stop)) (lambda _ #f)))
           `((stopped? . ,(or thread-cancelled? subproc-killed?))
             (thread-cancelled? . ,thread-cancelled?)
             (subprocess-killed? . ,subproc-killed?))))

(define* (format-steering-keymap-line #:optional (slash-mode? #f))
         "Return the dim bottom keymap hint for the redesigned mid-run steering bar."
         (if slash-mode?
           "enter:run cmd · tab:complete · esc:clear · ^c:force-stop"
           "enter:queue · ^u:urgent · /:cmd · ^i:interrupt · ^c/esc:force-stop"))

(define* (steering-bar-layout buffer queued-count maxx
                              #:key
                              (interrupt-requested? #f)
                              (urgent-pending? #f)
                              (status-text #f))
         "Compute the redesigned mid-run steering UI bar layout for BUFFER,
QUEUED-COUNT, and terminal width MAXX."
         (let* ((safe-buf (if (string? buffer) buffer ""))
                (safe-q (if (and (integer? queued-count) (> queued-count 0)) queued-count 0))
                (safe-w (max 12 (if (integer? maxx) maxx 80)))
                (slash-mode? (string-prefix? "/" safe-buf))
                (badge
                  (cond
                    (interrupt-requested? "[INTERRUPT]")
                    (urgent-pending? "[URGENT]")
                    (slash-mode? "[CMD]")
                    (else "[STEER]")))
                (badge-role
                  (cond
                    (interrupt-requested? 'error)
                    (urgent-pending? 'warning)
                    (else 'accent)))
                (queue-summary
                  (and (> safe-q 0) (format #f "+~a queued" safe-q)))
                (status-part
                  (and (string? status-text)
                       (not (string-null? (string-trim-both status-text)))
                       (string-trim-both status-text)))
                (meta-parts
                  (filter identity (list queue-summary status-part)))
                (meta-str
                  (if (null? meta-parts)
                    ""
                    (string-append " " (string-join meta-parts " · "))))
                (raw-header (string-append badge meta-str))
                (header-line
                  (if (> (string-length raw-header) safe-w)
                    (substring raw-header 0 safe-w)
                    raw-header))
                (avail-in (max 1 (- safe-w 3)))
                (placeholder? (string-null? safe-buf))
                (placeholder-text
                  "type steering, or /context /usage /theme /help /subagents /stop...")
                (input-text
                  (if placeholder?
                    (if (> (string-length placeholder-text) avail-in)
                      (substring placeholder-text 0 avail-in)
                      placeholder-text)
                    (let ((len (string-length safe-buf)))
                      (if (> len avail-in)
                        (substring safe-buf (- len avail-in) len)
                        safe-buf))))
                (cursor-col
                  (if placeholder?
                    0
                    (string-length input-text))))
           `((mode . ,(if slash-mode? 'slash 'steer))
             (badge . ,badge)
             (badge-role . ,badge-role)
             (queue-summary . ,queue-summary)
             (meta-line . ,meta-str)
             (header-line . ,header-line)
             (input-text . ,input-text)
             (placeholder? . ,placeholder?)
             (cursor-col . ,cursor-col)
             (hint . ,(format-steering-keymap-line slash-mode?)))))

(define (format-mid-run-subagents-summary reg)
        "Format an overview of active/available subagents using WP5 hierarchy &
event-bus overview, falling back to registered subagent tools in REG."
        (let* ((links (all-subagent-links))
               (events (harness-event-poll #:limit 5))
               (overview (format-subagent-overview #:max-width 76)))
          (if (and (null? links) (null? events) reg)
            (let* ((sub-tools
                     '("investigate" "investigate_parallel" "subagent_transaction" "remote_subagent"))
                   (registered
                     (filter (lambda (name) (registry-lookup reg name)) sub-tools)))
              (if (null? registered)
                overview
                (string-append overview "\nRegistered tools: " (string-join registered ", "))))
            overview)))

(define (span-rows->text rows)
        "Convert a list of WP1 styled span rows ((text role bold? dim?) ...) into plain text."
        (string-join
          (map (lambda (row)
                 (string-concatenate
                   (map (lambda (sp) (if (pair? sp) (car sp) "")) row)))
               (or rows '()))
          "\n"))

(define* (handle-todo-command! todo-list arg #:optional (width 76))
         "Apply /todo ARG ('add <text>', 'toggle <id>', 'rm <id>', 'clear', or 'list')
to TODO-LIST. Returns (values updated-todo-list formatted-text)."
         (let* ((trimmed (string-trim-both (if (string? arg) arg "")))
                (space-pos (string-index trimmed #\space))
                (sub (if space-pos (string-downcase (substring trimmed 0 space-pos)) (string-downcase trimmed)))
                (rest (if space-pos (string-trim-both (substring trimmed (+ space-pos 1))) "")))
           (cond
             ((or (string-null? trimmed) (string=? sub "list") (string=? sub "show"))
               (values todo-list (span-rows->text (format-todo-widget todo-list width))))
             ((string=? sub "add")
               (if (string-null? rest)
                 (values todo-list "Usage: /todo add <task description>")
                 (let ((updated (todo-add todo-list rest #:source 'user)))
                   (values updated (span-rows->text (format-todo-widget updated width))))))
             ((or (string=? sub "toggle") (string=? sub "done"))
               (let ((id (string->number rest)))
                 (if (integer? id)
                   (let ((updated (todo-toggle todo-list id)))
                     (values updated (span-rows->text (format-todo-widget updated width))))
                   (values todo-list "Usage: /todo toggle <id>"))))
             ((or (string=? sub "rm") (string=? sub "remove"))
               (let ((id (string->number rest)))
                 (if (integer? id)
                   (let ((updated (todo-remove todo-list id)))
                     (values updated (span-rows->text (format-todo-widget updated width))))
                   (values todo-list "Usage: /todo rm <id>"))))
             ((string=? sub "clear")
               (let ((updated (todo-clear-done todo-list)))
                 (values updated (span-rows->text (format-todo-widget updated width)))))
             (else
               (let ((updated (todo-add todo-list trimmed #:source 'user)))
                 (values updated (span-rows->text (format-todo-widget updated width))))))))

(define* (format-response-widgets-notice text #:optional (maxx 76))
         "Extract fenced scheme-widget / widget blocks from TEXT, render them safely
via render-scheme-response-widget, and return a formatted string or #f."
         (let ((blocks (if (string? text) (extract-response-widgets text) '())))
           (if (null? blocks)
             #f
             (let ((rendered
                     (filter-map
                       (lambda (expr)
                         (let ((res (render-scheme-response-widget expr (max 20 maxx))))
                           (if (assoc-ref res 'ok?)
                             (string-join (or (assoc-ref res 'lines) '()) "\n")
                             (let ((err (assoc-ref res 'error)))
                               (and err (format #f "[widget error: ~a]" err))))))
                       blocks)))
               (if (null? rendered)
                 #f
                 (string-join rendered "\n\n"))))))

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

(define (safe-write-osc-title! seq)
        "Write OSC title sequence SEQ to the notcurses tty control port when available."
        (when (and (string? seq) (not (string-null? seq)))
          (catch #t
            (lambda ()
              (let* ((term-mod (false-if-exception (resolve-module '(notcurses terminal) #f)))
                     (port-fn (and term-mod (module-ref term-mod 'tty-control-port #f)))
                     (port (and (procedure? port-fn) (port-fn))))
                (when (and (output-port? port) (not (port-closed? port)))
                  (display seq port)
                  (force-output port))))
            (lambda _ #f))))

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
          "  /geis verify <feature-id>          - Replay locked contract in a disposable kernel subprocess\n"
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

(define* (render-ncvisual-overlay! stdscr path getch-fn
                                   #:key (max-rows 20) (max-cols 60))
         "Display image PATH in a centered notcurses child plane using ncvisual,
wait for a key via GETCH-FN, and clean up. Returns info alist on success or #f."
         (and stdscr (string? path) (file-exists? path)
              (catch #t
                (lambda ()
                  (let* ((plane-mod (false-if-exception (resolve-module '(notcurses plane) #f)))
                         (core-mod (false-if-exception (resolve-module '(notcurses core) #f)))
                         (structs-mod (false-if-exception (resolve-module '(notcurses structs) #f))))
                    (if (not (and plane-mod core-mod structs-mod))
                      #f
                      (let* ((nc ((module-ref plane-mod 'ncplane_notcurses) stdscr))
                             (make-opts (module-ref structs-mod 'make-ncplane-options))
                             (->ptr (module-ref structs-mod '->pointer))
                             (ncplane-create (module-ref plane-mod 'ncplane_create))
                             (ncplane-destroy (module-ref plane-mod 'ncplane_destroy))
                             (nc-render (module-ref core-mod 'notcurses_render))
                             (opts (make-opts #:y 2 #:x 4 #:rows max-rows #:cols max-cols))
                             (child (ncplane-create stdscr (->ptr opts))))
                        (if (not child)
                          #f
                          (dynamic-wind
                            (lambda () #t)
                            (lambda ()
                              (let ((info (render-image-ncvisual nc child path
                                                                 #:y 0 #:x 0
                                                                 #:max-rows max-rows
                                                                 #:max-cols max-cols)))
                                (when info
                                  (nc-render nc)
                                  (when (procedure? getch-fn) (getch-fn)))
                                info))
                            (lambda ()
                              (false-if-exception (ncplane-destroy child)))))))))
                (lambda _ #f))))

(define* (show-geis-bindings-uplot! stdscr jrn getch-fn
                                    #:key (rows 12) (cols 56))
         "Render a native notcurses ncuplot overlay of journal geis-binding activity
when running in a live notcurses session. Returns #t if displayed, #f otherwise."
         (and stdscr jrn
              (catch #t
                (lambda ()
                  (let* ((entries (journal-entries jrn))
                         (plane-mod (false-if-exception (resolve-module '(notcurses plane) #f)))
                         (core-mod (false-if-exception (resolve-module '(notcurses core) #f)))
                         (structs-mod (false-if-exception (resolve-module '(notcurses structs) #f)))
                         (widgets-mod (false-if-exception (resolve-module '(notcurses widgets) #f))))
                    (if (not (and (pair? entries) plane-mod core-mod structs-mod widgets-mod))
                      #f
                      (let* ((nc ((module-ref plane-mod 'ncplane_notcurses) stdscr))
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
                             (child (ncplane-create stdscr (->ptr (make-popts #:y 3 #:x 4 #:rows rows #:cols cols)))))
                        (if (not child)
                          #f
                          (let ((plot #f))
                            (dynamic-wind
                              (lambda () #t)
                              (lambda ()
                                (set! plot (ncuplot-create child (->ptr (make-uopts)) 0 max-y))
                                (let loop ((i 0) (rest recent))
                                          (unless (null? rest)
                                            (let* ((e (car rest))
                                                   (kind (or (assoc-ref e 'kind) 'event))
                                                   (w (cond ((eq? kind 'commit) 4)
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
                (lambda _ #f))))

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
         (ncinput-obj (false-if-exception (nref '%ncinput)))
         (ncinput-modifiers-proc
           (let
               ((m (false-if-exception (resolve-module '(notcurses structs) #f))))
             (and m (false-if-exception (module-ref m 'ncinput-modifiers)))))
         (nckey-mod-shift-val
           (let
               ((m (false-if-exception (resolve-module '(notcurses constants) #f))))
             (and m (false-if-exception (module-ref m 'NCKEY_MOD_SHIFT)))))
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
        (lambda _
          #f))
      ;; Register Built-in Autonomous Agent Tools & Auto-start MCP servers
      (define llm-history '())
      (register-builtin-tui-tools! reg
                                   rollback-mgr
                                   verifier
                                   mcp-mgr
                                   (lambda ()
                                     llm-history)
                                   #:ctx-idx
                                   ctx-idx
                                   #:skill-reg
                                   skill-reg
                                   #:client
                                   client
                                   #:config
                                   cfg
                                   #:policy
                                   policy)
      (auto-start-mcp-servers! cfg mcp-mgr)
      (define semantic-summarize-fn (make-tui-semantic-summarizer client))
      (set-context-compactor-strategy! compactor
                                       (strategy-cascading #:keep-recent-turns
                                                           6
                                                           #:max-tool-chars
                                                           1200
                                                           #:pin-first-user
                                                           #t
                                                           #:summarize-fn
                                                           semantic-summarize-fn))
      (define context-limit-discovered? #f)
      (define (sync-context-limit!)
              "Update compactor max-tokens from explicit user config or live server metadata.
Only skips server probing when the user explicitly configured context-limits."
              (let*
                  ((configured (badb-config-model-context-limit cfg cur-endpoint cur-model))
                   (limit
                     (or configured
                         (catch #t
                           (lambda ()
                             (fetch-model-context-limit client))
                           (lambda _
                             #f)))))
                (set! context-limit-discovered? (and (integer? limit) (> limit 4096)))
                (set-context-compactor-max-tokens! compactor
                                                   (if context-limit-discovered?
                                                     limit
                                                     16384))))
      (sync-context-limit!)
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
                                                  #:workspace-dir
                                                  (getcwd)
                                                  #:tools
                                                  (collect-available-tools reg mcp-mgr)
                                                  #:skills
                                                  (skill-catalog-summary skill-reg)
                                                  #:memories
                                                  (catch #t
                                                    (lambda ()
                                                      (memory-read "all"))
                                                    (lambda _
                                                      #f))
                                                  #:static-only?
                                                  #f))))
      ;; The system prompt stays in llm-history for the model but is not
      ;; displayed as a transcript card.
      (define banner-tips
              (list "! shell · ( scheme · / commands · /help"
                    "ctrl+r history · @ files · ctrl+x $EDITOR · /todo · /diff"
                    (if (file-exists? ".badb/geasa")
                      "geasa active · /geis list · /geis bindings"
                      "Tip: run /geis onboard to detect toolchain & synthesize geasa")))
      (define (make-startup-notice-msg frame)
              (let
                  ((banner-rows
                     (format-startup-banner 108
                                            #:frame
                                            frame
                                            #:model
                                            cur-model
                                            #:workspace
                                            (getcwd)
                                            #:tips
                                            banner-tips))
                   (banner-text
                     (format-startup-banner-text 108
                                                 #:frame
                                                 frame
                                                 #:model
                                                 cur-model
                                                 #:workspace
                                                 (getcwd)
                                                 #:tips
                                                 banner-tips)))
                `((role . "notice") (hide-label? . #t)
                  (startup-banner? . #t)
                  (styled-rows . ,banner-rows)
                  (content . ,banner-text))))
      (define banner-frame 0)
      (define banner-last-time (get-internal-real-time))
      (define banner-anim-done? (not nodelay-proc))
      (define (advance-startup-banner!)
              (cond
                (banner-anim-done? #f)
                ((or (null? messages) (not (assoc-ref (car messages) 'startup-banner?)))
                  (set! banner-anim-done? #t)
                  #f)
                ((>= banner-frame (- (crow-banner-frame-count) 1))
                  (set! banner-anim-done? #t)
                  #f)
                (else
                  (let*
                      ((now (get-internal-real-time))
                       (step (quotient internal-time-units-per-second 5)))
                    (if (>= (- now banner-last-time) step)
                      (begin (set! banner-frame (+ banner-frame 1))
                             (set! banner-last-time now)
                             (set! messages (cons (make-startup-notice-msg banner-frame) (cdr messages)))
                             (when
                                 (>= banner-frame (- (crow-banner-frame-count) 1))
                               (set! banner-anim-done? #t))
                             #t)
                      #f)))))
      (define messages (list (make-startup-notice-msg 0)))
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
      (define react-stream-content-chunks '())
      (define react-stream-thoughts-chunks '())
      (define react-stream-last-render 0)
      (define last-spin-time 0)
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
      (define %row-texts (make-hash-table 61))
      (define active-keymap (make-default-keymap))
      (define history-search (make-history-search))
      (define %workspace-files-cache #f)
      (define at-selector-index 0)
      (define active-todo-list (make-todo-list #:title "Tasks"))
      (define ui-server
              (make-ui-server #:initial-state
                              (list (cons 'model cur-model) (cons 'status "READY"))))
      (define ui-client (make-ui-client #:id 'local-tui))
      (define last-tab-title-sig #f)
      (define %subagent-bus-sub-id
              (harness-event-subscribe!
                (lambda (ev)
                  (let*
                      ((src (harness-event-source-id ev))
                       (tgt (or (harness-event-target-id ev) (current-harness-id)))
                       (kind (harness-event-kind ev))
                       (payload (harness-event-payload ev)))
                    (catch #t
                      (lambda ()
                        (ui-server-route-subagent-event! ui-server src tgt kind payload))
                      (lambda _
                        #f))
                    (set! render-dirty? #t)))
                #:target-id
                (current-harness-id)))
      ;; Agent steering state: the queue is shared with every react session
      ;; and persists across runs (and /clear, which only resets the
      ;; transcript) so /steer and /interrupt can pre-queue entries between
      ;; runs — react-run! drains before its first LLM call too. The buffer
      ;; holds text typed mid-run; interrupt-requested? drives the pending
      ;; pill until a run consumes the queued 'interrupt.
      (define steering-queue (make-steering-queue))
      (define steering-buffer "")
      (define interrupt-requested? #f)
      (define %steering-mutex (make-mutex))
      (define %force-stop-requested? #f)
      (define %urgent-steer-requested? #f)
      (define %mid-run-pending-commands '())
      (define current-run-id 0)
      (define steering-selector-index 0)
      (define %render-mutex (make-mutex 'recursive))
      (define render-dirty? #f)
      (define %agent-thread #f)
      (define (agent-running?)
              (and (thread? %agent-thread) (not (thread-exited? %agent-thread))))
      (define (request-render!)
              (if (agent-running?)
                (set! render-dirty? #t)
                (render!)))
      ;; Message appending helpers
      (define (add-msg! role content)
              (with-mutex %render-mutex
                          (set! messages (append messages (list `((role . ,role) (content . ,content)))))))
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
      ;; Poll mid-run steering/control state from the ReAct loop hooks
      ;; (on-step, on-tool-call, on-tool-result, on-delta). Drains any queued
      ;; mid-run safe commands, and returns 'stop or 'urgent-steer when an
      ;; immediate force-stop or urgent steering preemption was requested.
      (define (poll-steering-input!)
              (let
                  ((pending-cmds
                     (with-mutex %steering-mutex
                                 (let ((cmds (reverse %mid-run-pending-commands)))
                                   (set! %mid-run-pending-commands '())
                                   cmds))))
                (for-each
                  (lambda (pair)
                    (catch #t
                      (lambda ()
                        (handle-command! pair))
                      (lambda _
                        #f)))
                  pending-cmds)
                (cond
                  (%force-stop-requested? 'stop)
                  (%urgent-steer-requested? 'urgent-steer)
                  (else #f))))
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
              (module-define! repl-mod 'client client)
              (sync-context-limit!))
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
              (tui-theme-attr-constant attr nref))
      (define (role-attrs role)
              (let ((a (caddr (theme-ref ui-theme role))))
                (cond
                  ((memq a '(bold dim reverse)) (list a))
                  ((list? a) a)
                  (else '()))))
      (define (theme-color->backend-value color)
              (tui-theme-color->backend-value color nref))
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
      ;; Render pre-computed (text role bold? dim?) segments from WP2 markdown/syntax
      ;; highlighters or segment-input-line, padding to INNER-W cleanly.
      (define
        (draw-segment-list y start-x segs inner-w)
        (let loop
          ((rest segs) (cur-x start-x))
          (if
              (or (null? rest) (>= cur-x (+ start-x inner-w)))
            (let ((rem (- (+ start-x inner-w) cur-x)))
              (when (> rem 0) (safe-addstr stdscr y cur-x (make-string rem #\space) rem)))
            (let*
                ((seg (car rest)) (str (car seg))
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
              (loop (cdr rest) (+ cur-x len))))))
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
                  ((theme-role (cadr entry)) (text (caddr entry))
                   (has-precomputed? (and (pair? (cdddr entry)) (list? (cadddr entry)))))
                (draw-gutter! y theme-role selected?)
                (hashq-set! %row-texts y text)
                (if has-precomputed?
                  (draw-segment-list y 2 (cadddr entry) content-width)
                  (draw-styled-interior y 2 text content-width theme-role))))
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
              (with-mutex %render-mutex
                          (catch #t
                            (lambda ()
                              (let*
                                  ((maxy (getmaxy stdscr)) (maxx (getmaxx stdscr))
                                   (running? (agent-running?))
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
                                   (box-style
                                     (if (< maxy 14)
                                       'tint
                                       'lines))
                                   (box-overhead
                                     (cond
                                       (running? 1)
                                       ((eq? box-style 'tint) input-rows)
                                       (else (+ 2 input-rows))))
                                   (box-top
                                     (if running?
                                       (- maxy 3)
                                       (- maxy 2 box-overhead)))
                                   (prompt-top
                                     (if running?
                                       (- maxy 3)
                                       (+ box-top
                                          (if (eq? box-style 'tint)
                                            0
                                            1))))
                                   (view-top 2)
                                   (view-bottom (max (+ view-top 1) (- box-top 1)))
                                   (view-height (max 1 (- view-bottom view-top)))
                                   (content-width (max 10 (- maxx 3)))
                                   (popup-anchor-y
                                     (if running?
                                       prompt-top
                                       box-top)))
                                (set! %click-targets (make-hash-table 61))
                                (set! %row-texts (make-hash-table 61))
                                ;; Update Ghostty/terminal tab title spinner when status changes
                                (let* ((tab-sig (cons status-text cur-model)))
                                  (unless (equal? tab-sig last-tab-title-sig)
                                    (set! last-tab-title-sig tab-sig)
                                    (safe-write-osc-title! (format-osc-tab-title status-text #:model cur-model))))
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
                                  ;; Rebuild row-count metadata only when the messages list or content width changed.
                                  ;; Formatted rows come from row-cache; unchanged messages are never re-formatted.
                                  (unless asm-valid?
                                    (when (< msg-count asm-msg-count) (row-cache-prune! row-cache messages))
                                    (let
                                        ((counts
                                           (map
                                             (lambda (msg)
                                               (length (row-cache-rows row-cache msg content-width)))
                                             messages)))
                                      (set! asm-counts counts)
                                      (set! asm-total (apply + 0 counts))
                                      (set! asm-msg-count msg-count)
                                      (set! asm-messages messages)
                                      (set! asm-width content-width)))
                                  (let*
                                      ((total-lines asm-total) (max-scroll (max 0 (- total-lines view-height))))
                                    ;; Clamp scroll after collapse state changed line counts
                                    (set! scroll-offset (max 0 (min scroll-offset max-scroll)))
                                    (set! max-scroll-seen max-scroll)
                                    ;; Keep the selected message inside the visible window
                                    (when
                                        (and (integer? sel-idx) (>= sel-idx 0) (< sel-idx (length asm-counts)))
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
                                        ((safe-scroll (max 0 (min scroll-offset max-scroll)))
                                         (start-line (max 0 (- total-lines view-height safe-scroll)))
                                         (end-line (+ start-line (min view-height (max 0 (- total-lines start-line)))))
                                         (visible-lines
                                           (let loop
                                             ((msgs messages) (counts asm-counts) (idx 0) (pos 0) (acc '()))
                                             (if (or (null? msgs) (>= pos end-line))
                                               (reverse acc)
                                               (let*
                                                   ((msg (car msgs)) (cnt (car counts)) (next-pos (+ pos cnt)))
                                                 (if (<= next-pos start-line)
                                                   (loop (cdr msgs) (cdr counts) (+ idx 1) next-pos acc)
                                                   (let*
                                                       ((skip (max 0 (- start-line pos)))
                                                        (take-count (min (- cnt skip) (- end-line (+ pos skip))))
                                                        (rows (row-cache-rows row-cache msg content-width))
                                                        (sliced (take (drop rows skip) take-count))
                                                        (tagged
                                                          (map
                                                            (lambda (r)
                                                              (cons idx r))
                                                            sliced)))
                                                     (loop (cdr msgs) (cdr counts) (+ idx 1) next-pos (append (reverse tagged) acc)))))))))
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
                                ;; Steering & Prompt Input Area:
                                ;; When %agent-thread is active, render the redesigned
                                ;; steering bar (mode badge, queue count, active input
                                ;; buffer or placeholder, and mid-run slash palette).
                                ;; Otherwise render the standard idle prompt and any
                                ;; pre-queued steering pill.
                                (let ((running? (agent-running?)))
                                  (if running?
                                    (let*
                                        ((bar
                                           (steering-bar-layout steering-buffer
                                                                (steering-count steering-queue)
                                                                maxx
                                                                #:interrupt-requested?
                                                                interrupt-requested?
                                                                #:urgent-pending?
                                                                %urgent-steer-requested?
                                                                #:status-text
                                                                status-text))
                                         (badge (assoc-ref bar 'badge))
                                         (badge-role (assoc-ref bar 'badge-role))
                                         (meta-line (assoc-ref bar 'meta-line))
                                         (in-text (assoc-ref bar 'input-text))
                                         (placeholder? (assoc-ref bar 'placeholder?))
                                         (slash-mode? (eq? (assoc-ref bar 'mode) 'slash))
                                         (glyph
                                           (if slash-mode?
                                             "/"
                                             (streaming-spinner-glyph))))
                                      (draw-role! badge-role #t)
                                      (attr-on! stdscr A_BOLD)
                                      (safe-addstr stdscr view-bottom 0 badge maxx)
                                      (attr-off! stdscr A_BOLD)
                                      (draw-role! badge-role #f)
                                      (let ((rem-x (string-length badge)))
                                        (when (and (string? meta-line) (< rem-x maxx))
                                          (draw-role! 'dim #t)
                                          (safe-addstr stdscr view-bottom rem-x meta-line (- maxx rem-x))
                                          (draw-role! 'dim #f)))
                                      (draw-role! 'accent #t)
                                      (attr-on! stdscr A_BOLD)
                                      (safe-addstr stdscr prompt-top 0 glyph 1)
                                      (attr-off! stdscr A_BOLD)
                                      (draw-role! 'accent #f)
                                      (draw-role!
                                        (if placeholder?
                                          'dim
                                          'default)
                                        #t)
                                      (safe-addstr stdscr prompt-top 2 in-text avail-width)
                                      (draw-role!
                                        (if placeholder?
                                          'dim
                                          'default)
                                        #f)
                                      (draw-role! 'dim #t)
                                      (safe-addstr stdscr (- maxy 2) 0 (assoc-ref bar 'hint) maxx)
                                      (draw-role! 'dim #f)
                                      (when (selector-active? steering-buffer)
                                        (let*
                                            ((matches (matching-mid-run-slash-commands steering-buffer))
                                             (box-w (min 62 (max 20 (- maxx 4)))))
                                          (unless (null? matches)
                                            (call-with-values
                                              (lambda ()
                                                (selector-visible-slice matches
                                                                        (min %selector-max-visible (max 1 (- maxy 5)))
                                                                        steering-selector-index))
                                              (lambda
                                                  (vis-items offset sel-pos above-count below-count)
                                                (let*
                                                    ((vis-count (length vis-items))
                                                     (box-h (+ vis-count 2))
                                                     (start-y (- popup-anchor-y 1 box-h))
                                                     (rows
                                                       (map
                                                         (lambda (item)
                                                           (cons (format #f "/~a" (car item)) (cdr item)))
                                                         vis-items)))
                                                  (when (>= start-y 2)
                                                    (draw-popup! "mid-run commands"
                                                                 rows
                                                                 sel-pos
                                                                 box-w
                                                                 start-y
                                                                 2
                                                                 above-count
                                                                 below-count))))))))
                                      (if active-menu
                                        #f
                                        (if placeholder?
                                          (begin (move stdscr prompt-top 2) (safe-curs-set! 0))
                                          (begin
                                            (move stdscr prompt-top (+ 2 (assoc-ref bar 'cursor-col)))
                                            (safe-curs-set! 1)
                                            (cursor-shape 'bar)))))
                                    (begin
                                      (let
                                          ((pending
                                             (steering-pending-line steering-buffer
                                                                    (steering-count steering-queue)
                                                                    interrupt-requested?)))
                                        (when pending
                                          (draw-role! 'dim #t)
                                          (safe-addstr stdscr view-bottom 0 pending maxx)
                                          (draw-role! 'dim #f)))
                                      ;; 3. Prompt Input Block with WP1 demarcation frame & WP2 live input highlighting
                                      (call-with-values
                                        (lambda ()
                                          (get-prompt-indicator input-buffer status-text prompt-mode))
                                        (lambda (glyph theme-role bold?)
                                          (when
                                              (or (string=? status-text "STREAMING...") (agent-run-active? status-text))
                                            (set! glyph (streaming-spinner-glyph)))
                                          (let*
                                              ((frame
                                                 (format-input-box-frame input-lines
                                                                         maxx
                                                                         #:prompt-glyph
                                                                         glyph
                                                                         #:prompt-role
                                                                         theme-role
                                                                         #:prompt-mode
                                                                         prompt-mode
                                                                         #:style
                                                                         box-style
                                                                         #:focused?
                                                                         (not active-menu)
                                                                         #:cursor-row
                                                                         input-cur-row
                                                                         #:cursor-col
                                                                         input-cur-col))
                                               (text-orig-y (assoc-ref frame 'text-origin-y))
                                               (text-orig-x (assoc-ref frame 'text-origin-x))
                                               (hl-mode
                                                 (if (selector-active? input-buffer prompt-mode)
                                                   'slash
                                                   prompt-mode)))
                                            (for-each
                                              (lambda (row-entry)
                                                (let*
                                                    ((dy (car row-entry)) (y (+ box-top dy))
                                                     (is-input-row? (and (>= dy text-orig-y) (< (- dy text-orig-y) input-rows))))
                                                  (for-each
                                                    (lambda (seg)
                                                      (let
                                                          ((sx (car seg)) (stext (cadr seg))
                                                           (srole (caddr seg))
                                                           (sbold? (cadddr seg))
                                                           (sdim? (car (cddddr seg))))
                                                        (draw-role! srole #t)
                                                        (when sbold? (attr-on! stdscr A_BOLD))
                                                        (when sdim? (attr-on! stdscr A_DIM))
                                                        (safe-addstr stdscr y sx stext (max 0 (- maxx sx)))
                                                        (when sdim? (attr-off! stdscr A_DIM))
                                                        (when sbold? (attr-off! stdscr A_BOLD))
                                                        (draw-role! srole #f)))
                                                    (cdr row-entry))
                                                  (when is-input-row?
                                                    (let
                                                        ((line-txt (list-ref input-lines (- dy text-orig-y))))
                                                      (unless (string-null? line-txt)
                                                        (draw-segment-list y
                                                                           text-orig-x
                                                                           (segment-input-line line-txt #:prompt-mode hl-mode)
                                                                           (max 1 (- maxx text-orig-x))))))))
                                              (assoc-ref frame 'rows)))))
                                      ;; 4. Keymap hint line at (- maxy 2)
                                      (let
                                          ((hint
                                             (format-keymap-line
                                               (cond
                                                 (active-menu 'menu)
                                                 ((history-search-active? history-search) 'selector)
                                                 ((selector-active? input-buffer prompt-mode) 'selector)
                                                 ((and (agent-run-active? status-text) nodelay-proc) 'steering)
                                                 (else prompt-mode)))))
                                        (draw-role! 'dim #t)
                                        (safe-addstr stdscr (- maxy 2) 0 hint maxx)
                                        (draw-role! 'dim #f))
                                      ;; 5. Floating Slash Command Selector Popup Overlay
                                      (when
                                          (and
                                            (not (history-search-active? history-search))
                                            (selector-active? input-buffer prompt-mode))
                                        (let*
                                            ((matches (deduplicate-slash-commands (matching-slash-commands input-buffer)))
                                             (box-w (min 62 (max 20 (- maxx 4)))))
                                          (unless (null? matches)
                                            (call-with-values
                                              (lambda ()
                                                (selector-visible-slice matches
                                                                        (min %selector-max-visible (max 1 (- maxy 6)))
                                                                        selector-index))
                                              (lambda
                                                  (vis-items offset sel-pos above-count below-count)
                                                (let*
                                                    ((vis-count (length vis-items))
                                                     (box-h (+ vis-count 2))
                                                     (start-y (- popup-anchor-y 1 box-h))
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
                                      ;; 5b. Floating Ctrl+R Fuzzy History Search Popup
                                      (when (history-search-active? history-search)
                                        (let*
                                            ((matches (history-search-matches history-search))
                                             (box-w (min 72 (max 24 (- maxx 4))))
                                             (q (history-search-query history-search))
                                             (title
                                               (if (string-null? q)
                                                 "history search (ctrl+r)"
                                                 (format #f "history: ~a" q))))
                                          (if (null? matches)
                                            (let ((start-y (- popup-anchor-y 1 3)))
                                              (when (>= start-y 2)
                                                (draw-popup! title
                                                             '(("(no matching history)" . "type to refine, esc to cancel"))
                                                             0
                                                             box-w
                                                             start-y
                                                             2
                                                             0
                                                             0)))
                                            (call-with-values
                                              (lambda ()
                                                (selector-visible-slice matches
                                                                        (min %selector-max-visible (max 1 (- maxy 6)))
                                                                        (history-search-selected-index history-search)))
                                              (lambda
                                                  (vis-items offset sel-pos above-count below-count)
                                                (let*
                                                    ((vis-count (length vis-items))
                                                     (box-h (+ vis-count 2))
                                                     (start-y (- popup-anchor-y 1 box-h))
                                                     (rows
                                                       (map
                                                         (lambda (s)
                                                           (cons s ""))
                                                         vis-items)))
                                                  (when (>= start-y 2)
                                                    (draw-popup! title rows sel-pos box-w start-y 2 above-count below-count))))))))
                                      ;; 5c. Floating @ Mention Completion Popup
                                      (when
                                          (and
                                            (not (history-search-active? history-search))
                                            (not (selector-active? input-buffer prompt-mode))
                                            (at-selector-active? input-buffer cursor-pos prompt-mode))
                                        (unless %workspace-files-cache
                                          (set! %workspace-files-cache (collect-workspace-files)))
                                        (let*
                                            ((cands
                                               (matching-at-candidates input-buffer
                                                                       #:cursor-pos
                                                                       cursor-pos
                                                                       #:candidates
                                                                       %workspace-files-cache
                                                                       #:limit
                                                                       25))
                                             (box-w (min 62 (max 20 (- maxx 4)))))
                                          (unless (null? cands)
                                            (call-with-values
                                              (lambda ()
                                                (selector-visible-slice cands
                                                                        (min %selector-max-visible (max 1 (- maxy 6)))
                                                                        at-selector-index))
                                              (lambda
                                                  (vis-items offset sel-pos above-count below-count)
                                                (let*
                                                    ((vis-count (length vis-items))
                                                     (box-h (+ vis-count 2))
                                                     (start-y (- popup-anchor-y 1 box-h))
                                                     (rows
                                                       (map
                                                         (lambda (it)
                                                           (cons (format #f "@~a" (car it)) (cdr it)))
                                                         vis-items)))
                                                  (when (>= start-y 2)
                                                    (draw-popup! "@ mentions" rows sel-pos box-w start-y 2 above-count below-count)))))))))))
                                ;; 4b. WP1 Bottom Status Bar at (- maxy 1)
                                (let*
                                    ((todo-pair (todo-counts active-todo-list))
                                     (todo-sum
                                       (and (> (cdr todo-pair) 0)
                                            (format #f "✓ ~a/~a" (car todo-pair) (cdr todo-pair))))
                                     (sub-cnt
                                       (length
                                         (filter
                                           (lambda (l)
                                             (eq? (subagent-link-status l) 'running))
                                           (all-subagent-links))))
                                     (used-tok (or %ctx-cache-tokens (estimate-conversation-tokens llm-history)))
                                     (lim-tok (context-compactor-effective-ceiling compactor)))
                                  (for-each
                                    (lambda (seg)
                                      (let
                                          ((x (car seg)) (txt (cadr seg))
                                           (role (caddr seg))
                                           (b? (cadddr seg))
                                           (d? (car (cddddr seg))))
                                        (draw-role! role #t)
                                        (when b? (attr-on! stdscr A_BOLD))
                                        (when d? (attr-on! stdscr A_DIM))
                                        (safe-addstr stdscr (- maxy 1) x txt (max 0 (- maxx x)))
                                        (when d? (attr-off! stdscr A_DIM))
                                        (when b? (attr-off! stdscr A_BOLD))
                                        (draw-role! role #f)))
                                    (format-bottom-status-bar maxx
                                                              #:mode
                                                              (if running?
                                                                'steering
                                                                prompt-mode)
                                                              #:model
                                                              (or cur-model "")
                                                              #:status
                                                              status-text
                                                              #:cwd
                                                              (getcwd)
                                                              #:tokens-used
                                                              (if (number? used-tok)
                                                                used-tok
                                                                0)
                                                              #:tokens-limit
                                                              (if (number? lim-tok)
                                                                lim-tok
                                                                0)
                                                              #:subagent-count
                                                              sub-cnt
                                                              #:todo-summary
                                                              todo-sum)))
                                ;; 6. Floating Menu Popup Overlay (breadcrumb title from WP4)
                                (when active-menu
                                  (let ((m-count (menu-count active-menu)))
                                    (when (> m-count 0)
                                      (call-with-values
                                        (lambda ()
                                          (menu-visible-slice active-menu (min %menu-max-visible (max 1 (- maxy 6)))))
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
                                               (start-y (- popup-anchor-y 1 box-h))
                                               (rows
                                                 (map
                                                   (lambda (it)
                                                     (cons (menu-item-label it) (menu-item-description it)))
                                                   vis-items)))
                                            (when (>= start-y 2)
                                              (draw-popup! (menu-breadcrumb-title active-menu)
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
                                (unless running?
                                  (move stdscr (+ prompt-top input-cur-row) (+ 2 input-cur-col))
                                  (cond
                                    ((agent-run-active? status-text) (safe-curs-set! 0))
                                    ((integer? msgsel-index) (safe-curs-set! 1) (cursor-shape 'block))
                                    ((memq prompt-mode '(shell scheme))
                                      (safe-curs-set! 1)
                                      (cursor-shape 'underline))
                                    (else (safe-curs-set! 1) (cursor-shape 'bar))))
                                (refresh stdscr)))
                            (lambda (key . args)
                              ;; Never swallow render failures silently: surface each new
                              ;; error once in the transcript so a broken draw path is
                              ;; visible instead of freezing the UI with no diagnostics.
                              (let ((sig (format #f "~s ~s" key args)))
                                (unless (equal? sig last-render-error-sig)
                                  (set! last-render-error-sig sig)
                                  (catch #t
                                    (lambda ()
                                      (add-err-msg! (format #f "render error: ~a ~s" key args)))
                                    (lambda _
                                      #f))))
                              #f))))
      ;; Live streaming display: one flagged assistant card grows in the
      ;; transcript as deltas arrive ('streaming flag); later steps replace
      ;; exactly that placeholder, commit it permanently, or remove it.
      (define (react-stream-placeholder-index)
              (list-index
                (lambda (m)
                  (assoc-ref m 'streaming))
                messages))
      (define (update-react-stream-message! delta type)
              (with-mutex %render-mutex
                          (cond
                            ((eq? type 'thought)
                              (when (string? delta)
                                (set! react-stream-thoughts-chunks (cons delta react-stream-thoughts-chunks)))
                              (set! status-text "THINKING..."))
                            ((eq? type 'content)
                              (when (string? delta)
                                (set! react-stream-content-chunks (cons delta react-stream-content-chunks)))
                              (set! status-text "STREAMING..."))
                            ((eq? type 'usage) (record-session-usage! delta))
                            ((eq? type 'tool-call)
                              (let
                                  ((name (and (pair? delta) (assoc-ref delta 'name))))
                                (if (non-empty-string? name)
                                  (set! status-text (string-append "TOOL: " name "..."))
                                  (set! status-text "TOOL CALL..."))))
                            (else #f))
                          (let
                              ((react-stream-content
                                 (string-concatenate (reverse react-stream-content-chunks)))
                               (react-stream-thoughts
                                 (string-concatenate (reverse react-stream-thoughts-chunks))))
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
                                      (set! messages (append messages (list updated)))))))))))
      (define (commit-react-stream-message!)
              "Keep the streamed assistant card permanently but drop its
placeholder flag, so the next streamed call starts a fresh card."
              (with-mutex %render-mutex
                          (set! messages
                                (map
                                  (lambda (m)
                                    (if (assoc-ref m 'streaming)
                                      (filter
                                        (lambda (p)
                                          (not (eq? (car p) 'streaming)))
                                        m)
                                      m))
                                  messages))))
      (define (remove-react-stream-message!)
              (with-mutex %render-mutex
                          (set! messages
                                (filter
                                  (lambda (m)
                                    (not (assoc-ref m 'streaming)))
                                  messages))))
      (define (reset-react-stream-state!)
              (set! react-stream-content-chunks '())
              (set! react-stream-thoughts-chunks '()))
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
              (let ((now (get-internal-real-time)))
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
              (let ((now (get-internal-real-time)))
                (when
                    (>= (- now react-stream-last-render)
                        (quotient internal-time-units-per-second 15))
                  (set! react-stream-last-render now)
                  (request-render!))))
      ;; Execute streaming query against LLM with separated thoughts
      (define (execute-chat-stream! user-prompt)
              (let ((my-run-id current-run-id))
                (set! scroll-offset 0)
                ;; Session accounting: a new turn starts, and until an
                ;; endpoint usage block arrives the growth of the
                ;; conversation provides the fallback estimate.
                (set! session-turns (+ session-turns 1))
                (set! turn-usage-seen? #f)
                (set! turn-pre-estimate (estimate-conversation-tokens llm-history))
                (dynamic-wind
                  (lambda ()
                    #t)
                  (lambda ()
                    (catch #t
                      (lambda ()
                        (set! llm-history
                              (append llm-history (list `((role . "user") (content . ,user-prompt)))))
                        (begin
                          (when
                              (and
                                (not (badb-config-model-context-limit cfg cur-endpoint cur-model))
                                (or (not context-limit-discovered?) (compaction-needed? compactor llm-history)))
                            (sync-context-limit!))
                          (when (compaction-needed? compactor llm-history)
                            (set! llm-history (compact-messages compactor llm-history))))
                        (let run-turn
                          ()
                          (let
                              ((tools (collect-available-tools reg mcp-mgr)))
                            (if (pair? tools)
                              ;; Autonomous ReAct Tool-Calling Loop
                              (catch #t
                                (lambda ()
                                  (let*
                                      ((max-react-iters (badb-config-react-max-iterations cfg))
                                       (loop-guard (make-loop-guard))
                                       (gate-options
                                         (systemone-gate-config->options cfg
                                                                         #:warn
                                                                         (lambda (msg)
                                                                           (add-sys-msg! msg))))
                                       (gate-client
                                         (and
                                           (systemone-gate-options-enabled? gate-options)
                                           (make-systemone-client #:endpoint
                                                                  (or
                                                                    (systemone-gate-options-endpoint gate-options)
                                                                    "https://api.typesafe.ai/v1/systemone")
                                                                  #:api-key
                                                                  (or (systemone-gate-options-api-key gate-options) (getenv "TYPESAFE_API_KEY"))
                                                                  #:model
                                                                  (systemone-gate-options-model gate-options))))
                                       (gate
                                         (and gate-client
                                              (make-systemone-gate #:options
                                                                   gate-options
                                                                   #:check
                                                                   (lambda (state)
                                                                     (systemone-gate-check gate-client state))
                                                                   #:journal
                                                                   jrn
                                                                   #:model
                                                                   (badb-config-model-name cfg)
                                                                   #:active-geasa
                                                                   (map
                                                                     (lambda (entry)
                                                                       (ensure-string (assoc-ref entry (quote id))))
                                                                     (registry-list reg))
                                                                   #:on-verdict
                                                                   (lambda (action verdict)
                                                                     (set! status-text (format #f "SYSTEMONE: ~a" action))
                                                                     (render!)))))
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
                                                             (append (make-loop-guard-hooks loop-guard)
                                                                     (if gate
                                                                       (make-systemone-gate-hooks gate)
                                                                       '())
                                                                     (list
                                                                       (cons 'on-step
                                                                             (lambda (iter phase)
                                                                               (when (eq? phase 'starting)
                                                                                 (commit-react-stream-message!)
                                                                                 (reset-react-stream-state!))
                                                                               (set! status-text
                                                                                     (cond
                                                                                       ((and (pair? phase) (eq? (car phase) 'rate-limited))
                                                                                         (format #f
                                                                                                 "RATE LIMITED (~as)..."
                                                                                                 (inexact->exact (ceiling (or (cdr phase) 1)))))
                                                                                       (max-react-iters (format #f "REACT [~a/~a]" iter max-react-iters))
                                                                                       (else (format #f "REACT [~a]" iter))))
                                                                               ;; Crash-safe persistence: sync the transcript
                                                                               ;; from the live session and rewrite the session
                                                                               ;; file after every executed step so /resume
                                                                               ;; recovers mid-run state.
                                                                               (when
                                                                                   (memq phase '(tools-executed loop-detected))
                                                                                 (let ((sess (current-react-session)))
                                                                                   (when (react-session? sess)
                                                                                     (set! llm-history (react-session-messages sess))
                                                                                     (persist-session-file! session-file llm-history))))
                                                                               (poll-steering-input!)
                                                                               (request-render!)))
                                                                       (cons 'on-tool-call
                                                                             (lambda (tc)
                                                                               (let
                                                                                   ((t-name (prop-ref tc 'name)) (t-args (prop-ref tc 'arguments)))
                                                                                 (set! status-text (format #f "TOOL: ~a..." t-name))
                                                                                 (add-msg! "tool-call" (format #f "Invoking tool: ~a (~s)" t-name t-args))
                                                                                 (poll-steering-input!)
                                                                                 (request-render!))))
                                                                       (cons 'on-tool-result
                                                                             (lambda (tc res err?)
                                                                               (let*
                                                                                   ((t-name (prop-ref tc 'name))
                                                                                    (blocked? (tool-result-blocked-payload? res))
                                                                                    (res-str
                                                                                      (if (string? res)
                                                                                        res
                                                                                        (safe-json-string res)))
                                                                                    (preview
                                                                                      (if (> (string-length res-str) 140)
                                                                                        (string-append (substring res-str 0 140) "...")
                                                                                        res-str)))
                                                                                 (add-msg! "tool-result"
                                                                                           (cond
                                                                                             (blocked? (format #f "Tool ~a blocked (duplicate call): ~a" t-name preview))
                                                                                             (err? (format #f "Tool ~a failed: ~a" t-name preview))
                                                                                             (else (format #f "Tool ~a returned: ~a" t-name preview))))
                                                                                 (poll-steering-input!)
                                                                                 (request-render!))))
                                                                       (cons 'on-delta
                                                                             (lambda (delta type)
                                                                               (let ((sig (poll-steering-input!)))
                                                                                 (if (memq sig '(stop urgent-steer))
                                                                                   'stop
                                                                                   (begin (update-react-stream-message! delta type) (throttled-stream-render!) #t)))))))
                                                             #:compactor
                                                             compactor
                                                             #:loop-detector
                                                             (make-loop-detector)
                                                             ;; The per-step user-role recitation was repeatedly
                                                             ;; mistaken for untrusted tool-output instructions.
                                                             #:recitation?
                                                             #f)))
                                    (call-with-values
                                      (lambda ()
                                        (react-run! session))
                                      (lambda (verdict final-content sess)
                                        (set! llm-history (react-session-messages sess))
                                        (cond
                                          ((and %urgent-steer-requested?
                                                (not %force-stop-requested?)
                                                (not (steering-empty? steering-queue)))
                                            (commit-react-stream-message!)
                                            (reset-react-stream-state!)
                                            (set! %urgent-steer-requested? #f)
                                            (run-turn))
                                          ((eq? verdict 'completed)
                                            (remove-react-stream-message!)
                                            (call-with-values
                                              (lambda ()
                                                (split-thoughts-and-content (or final-content "")))
                                              (lambda (th ct)
                                                (with-mutex %render-mutex
                                                            (set! messages
                                                                  (append messages
                                                                          (list
                                                                            (default-collapse-message
                                                                              `((role . "assistant") (thoughts . ,th) (content . ,ct)))))))
                                                (let
                                                    ((widget-txt (format-response-widgets-notice ct 76))
                                                     (img-txt (format-response-images-notice ct 76)))
                                                  (when widget-txt (add-msg! "notice" widget-txt))
                                                  (when img-txt (add-msg! "notice" img-txt))))))
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
                                     (with-mutex %render-mutex
                                                 (set! messages
                                                       (append messages
                                                               (list
                                                                 (default-collapse-message
                                                                   `((role . "assistant") (thoughts . #f) (content . "")))))))
                                     (render!)
                                     ;; Direct-streaming accumulators live OUTSIDE the catch so a
                                     ;; mid-stream failure can still preserve the partial output.
                                     (let ((raw-api-thoughts "") (raw-api-content ""))
                                       (catch #t
                                         (lambda ()
                                           (let ((target-cell (last-pair messages)))
                                             (model-stream client
                                                           llm-history
                                                           (lambda (delta . type-args)
                                                             (let ((sig (poll-steering-input!)))
                                                               (if (memq sig '(stop urgent-steer))
                                                                 'stop
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
                                                                         (throttled-stream-render!)
                                                                         #t)))))))
                                                           #:max-tokens
                                                           (or (badb-config-model-max-tokens cfg) 4096))
                                             (set! llm-history
                                                   (append llm-history (list `((role . "assistant") (content . ,raw-api-content)))))
                                             (when
                                                 (and %urgent-steer-requested?
                                                      (not %force-stop-requested?)
                                                      (not (steering-empty? steering-queue)))
                                               (let
                                                   ((entries (with-mutex %steering-mutex (steering-drain! steering-queue))))
                                                 (for-each
                                                   (lambda (e)
                                                     (when (string? e)
                                                       (set! llm-history (append llm-history (list `((role . "user") (content . ,e)))))))
                                                   entries))
                                               (set! %urgent-steer-requested? #f)
                                               (run-turn))))
                                         (lambda (key . args)
                                           ;; Preserve any partially streamed output in
                                           ;; llm-history (with its thinking trace) so the
                                           ;; model sees its own partial answer when the
                                           ;; next turn recovers from the stream failure.
                                           (when
                                               (and (string? raw-api-content)
                                                    (not (string-null? (string-trim-both raw-api-content))))
                                             (set! llm-history
                                                   (append llm-history
                                                           (list
                                                             (append
                                                               `((role . "assistant")
                                                                 ,@(if (non-empty-string? raw-api-thoughts)
                                                                     `((reasoning_content . ,raw-api-thoughts))
                                                                     '()))
                                                               `((content . ,raw-api-content)))))))
                                           (add-err-msg! (format #f "Stream error (~a): ~s" key args))))))))))
                      (lambda (key . args)
                        (add-err-msg! (format #f "Execution error (~a): ~s" key args)))))
                  (lambda ()
                    (when (= my-run-id current-run-id)
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
                      (set! steering-selector-index 0)
                      (set! interrupt-requested? #f)
                      (set! %urgent-steer-requested? #f)
                      (set! %force-stop-requested? #f)
                      (set! status-text "READY")
                      (set! render-dirty? #t)
                      (render!))))))
      ;; Masked password prompt modal for safe sudo execution (never echoes or logs)
      (define (prompt-password-modal prompt-str)
              (let
                  ((pw "") (maxy (getmaxy stdscr)) (maxx (getmaxx stdscr)))
                (when nodelay-proc (nodelay-proc stdscr #f))
                (let pw-loop
                  ()
                  (let*
                      ((y (max 2 (- maxy 3)))
                       (masked (make-string (string-length pw) #\*))
                       (line (format #f " [SUDO] ~a: ~a" (or prompt-str "Password") masked)))
                    (draw-role! 'warning #t)
                    (safe-addstr stdscr y 0 (make-string maxx #\space) maxx)
                    (safe-addstr stdscr y 0 line maxx)
                    (draw-role! 'warning #f)
                    (move stdscr y (min (- maxx 1) (string-length line)))
                    (refresh stdscr)
                    (let ((ch (key->code (getch stdscr))))
                      (cond
                        ((or (eqv? ch 27) (eqv? ch 3) (eqv? ch 4)) #f)
                        ((or (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13)) pw)
                        ((or (eqv? ch KEY_BACKSPACE) (eqv? ch 8) (eqv? ch 127))
                          (when (> (string-length pw) 0)
                            (set! pw (substring pw 0 (- (string-length pw) 1))))
                          (pw-loop))
                        ((and (integer? ch) (>= ch 32) (<= ch 126))
                          (set! pw (string-append pw (string (integer->char ch))))
                          (pw-loop))
                        (else (pw-loop))))))))
      (define (execute-shell! cmd)
              (set! status-text "EXEC...")
              (render!)
              (if (string-null? cmd)
                (add-sys-msg! "Usage: !<command> (e.g. !git status, !ls -la)")
                (call-with-values
                  (lambda ()
                    (execute-shell-command cmd #:password-provider prompt-password-modal))
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
                (string-case
                  (if (string-null? arg)
                    ""
                    subcmd)
                  (""
                   (set! active-menu
                         (make-menu "Configuration"
                                    (config-menu-items-for-state cfg cur-model cur-endpoint policy))))
                  ("show"
                   (add-sys-msg!
                     (format-config-summary cfg
                                            cur-model
                                            cur-endpoint
                                            cur-journal-path
                                            #:policy
                                            policy)))
                  ("reload"
                   (catch #t
                     (lambda ()
                       (set! cfg (load-config))
                       (set! cur-model (badb-config-model-name cfg))
                       (set! cur-endpoint (badb-config-model-endpoint cfg))
                       (set! cur-api-key
                             (or api-key
                                 (getenv "BADB_API_KEY")
                                 (getenv "OPENAI_API_KEY")
                                 (badb-config-model-api-key cfg)))
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
                                    ("model" (set! cur-model val)
                                     (badb-config-set-model-name! cfg val)
                                     (refresh-client!)
                                     (try-save-config!)
                                     (add-sys-msg! (format #f "Model set to '~a' and saved." val)))
                                    ("endpoint" (set-endpoint-url! val "Endpoint set to '~a' and saved."))
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
                  (add-sys-msg! (format-guardrail-status policy))
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
      (define (set-endpoint-url! raw-url success-fmt)
              "Validate RAW-URL as an HTTP(S) URI, apply it to cur-endpoint and cfg,
refresh client, save config, and log SUCCESS-FMT. Rejects non-URI values
and hints /provider <name> when RAW-URL matches a configured preset."
              (let ((trimmed (string-trim-both raw-url)))
                (if (valid-endpoint-uri? trimmed)
                  (let
                      ((endpoint-changed? (not (equal? trimmed cur-endpoint)))
                       (matched-prov (badb-config-provider-for-endpoint cfg trimmed)))
                    (set! cur-endpoint trimmed)
                    (badb-config-set-model-endpoint! cfg trimmed)
                    (when endpoint-changed?
                      (let
                          ((new-key (and matched-prov (get-prop matched-prov 'api-key #f))))
                        (set! cur-api-key new-key)
                        (badb-config-set-model-api-key! cfg new-key)))
                    (refresh-client!)
                    (try-save-config!)
                    (add-sys-msg! (format #f success-fmt cur-endpoint)))
                  (if
                      (find
                        (lambda (p)
                          (and (pair? p) (string=? (ensure-string (car p)) trimmed)))
                        (badb-config-providers cfg))
                    (add-err-msg!
                      (format #f
                              "Invalid endpoint URL '~a' (expected http://... or https://...). '~a' is a provider preset — use '/provider ~a' to switch to it."
                              trimmed
                              trimmed
                              trimmed))
                    (add-err-msg!
                      (format #f
                              "Invalid endpoint URL '~a' (expected http://... or https://...)."
                              trimmed))))))
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
                  (add-sys-msg!
                    (format-provider-presets (badb-config-providers cfg) cur-endpoint))
                  (switch-provider-preset! trimmed))))
      (define (handle-models-cmd!)
              "Show the models available on the active endpoint as a system
message, or a clear error notice when the listing fails."
              (call-with-values
                (lambda ()
                  (fetch-live-models client))
                (lambda (ids err)
                  (if ids
                    (begin (sync-context-limit!)
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
                                              "\n"))))
                    (add-err-msg! (format #f "Failed to list models: ~a" err))))))
      (define (apply-live-theme! name)
              (let ((th (lookup-theme name)))
                (when th
                  (set! ui-theme th)
                  #t)))
      (define*
        (open-theme-picker! #:optional (parent-menu #f))
        (let
            ((cur-name (or (badb-config-ui-theme cfg) (theme-name ui-theme))))
          (set! active-menu
                (make-theme-preview-menu cur-name apply-live-theme! #:parent parent-menu))))
      (define (apply-menu-selection! item)
              "Apply the action data carried by the selected menu ITEM, closing
any open menu first; submenu actions push onto the parent breadcrumb stack."
              (let
                  ((prev-menu active-menu) (action (menu-item-action item)))
                (define*
                  (push-sub! m-or-title #:optional (items '()))
                  (set! active-menu
                        (cond
                          ((menu? m-or-title)
                            (if prev-menu
                              (menu-push prev-menu m-or-title)
                              m-or-title))
                          (prev-menu (menu-push prev-menu m-or-title items))
                          (else (make-menu m-or-title items)))))
                (set! active-menu #f)
                (cond
                  ((symbol? action)
                    (case action
                      ((open-config-menu)
                        (push-sub! "Configuration"
                                   (config-menu-items-for-state cfg cur-model cur-endpoint policy)))
                      ((open-model-menu)
                        (push-sub! "Model"
                                   (build-live-or-recorded-model-menu-items client cfg cur-model)))
                      ((open-endpoint-menu)
                        (push-sub! "Endpoint" (endpoint-menu-items-for-config cfg)))
                      ((open-provider-menu)
                        (push-sub! "Provider" (provider-menu-items cfg cur-endpoint)))
                      ((open-synthesizer-menu)
                        (push-sub! "Synthesiser" (synthesizer-config-menu-items cfg)))
                      ((open-synth-provider-menu)
                        (push-sub! "Synthesis Provider" (synthesizer-provider-menu-items cfg)))
                      ((open-synth-model-menu)
                        (push-sub! "Synthesis Model" (synthesizer-model-menu-items cfg)))
                      ((open-theme-menu) (open-theme-picker! prev-menu))
                      ((open-guardrail-menu) (push-sub! "Guardrail" (guardrail-menu-items)))
                      ((open-keymap-menu)
                        (push-sub! "Keybindings" (keymap->menu-items active-keymap)))
                      ((open-resume-menu) (handle-resume-cmd! ""))
                      ((prefill-model) (prefill-input! "/model "))
                      ((prefill-endpoint) (prefill-input! "/endpoint "))
                      ((prefill-temperature) (prefill-input! "/config set temperature "))
                      ((prefill-synth-model) (prefill-input! "/geis model "))
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
                  ((and (pair? action) (eq? (car action) 'set-synth-provider))
                    (let ((prov (cdr action)))
                      (apply-synthesis-provider-config! cfg prov)
                      (try-save-config!)
                      (add-sys-msg!
                        (if prov
                          (format #f "Synthesis provider set to '~a' (saved to config)." prov)
                          "Synthesis provider reset to inherit chat provider (saved to config)."))))
                  ((and (pair? action) (eq? (car action) 'set-synth-model))
                    (let ((m (cdr action)))
                      (badb-config-set-synthesis-model! cfg m)
                      (try-save-config!)
                      (add-sys-msg! (format #f "Geis synthesis model set to: ~a (saved to config)" m))))
                  ((and (pair? action) (eq? (car action) 'set-theme))
                    (let ((name (cdr action)))
                      (badb-config-set-ui-theme! cfg name)
                      (apply-live-theme! name)
                      (try-save-config!)
                      (add-sys-msg! (format #f "UI theme set to '~a' and saved." name))))
                  ((and (pair? action) (eq? (car action) 'set-guardrail))
                    (handle-guardrail-cmd! (symbol->string (cdr action))))
                  ((and (pair? action) (eq? (car action) 'open-command-category))
                    (set! active-menu
                          (open-command-category-menu
                            (or prev-menu (make-commands-root-menu))
                            (cdr action))))
                  ((and (pair? action) (eq? (car action) 'run-command))
                    (handle-command! (cons (cdr action) "")))
                  ((and (pair? action) (eq? (car action) 'prefill-command))
                    (prefill-input! (cdr action)))
                  ((and (pair? action) (eq? (car action) 'keymap-bind))
                    (add-sys-msg!
                      (format #f
                              "Keybinding ~a in ~a mode (customize via keymap-bind!)."
                              (cdr action)
                              (car action))))
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
                               (set! messages (session-turns->transcript-messages loaded))
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
      ;; without one, open the live-preview theme picker menu.
      (define (handle-theme-cmd! arg)
              (let ((name (string-trim-both arg)))
                (cond
                  ((string-null? name) (open-theme-picker!))
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
              (let
                  ((button (car ev)) (y (cadr ev)) (x (and (pair? (cddr ev)) (caddr ev))))
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
                        ((and (selector-active? input-buffer prompt-mode)
                              (pair? target)
                              (eq? (car target) 'palette-item))
                          (set! selector-index (cdr target))
                          #t)
                        ((integer? target)
                          (let*
                              ((row-txt (hashq-ref %row-texts y))
                               (link
                                 (and (string? row-txt)
                                      (integer? x)
                                      (>= x 2)
                                      (find-link-at-column row-txt (- x 2)))))
                            (if link
                              (let
                                  ((kind (assoc-ref link 'kind))
                                   (dest (assoc-ref link 'target))
                                   (line-num (assoc-ref link 'line)))
                                (cond
                                  ((and (eq? kind 'file) (string? dest) (file-exists? dest))
                                    (open-in-external-editor dest
                                                             #:line
                                                             line-num
                                                             #:suspend-tty!
                                                             endwin
                                                             #:resume-tty!
                                                             refresh))
                                  ((and (eq? kind 'url) (string? dest))
                                    (catch #t
                                      (lambda ()
                                        (let ((p (open-pipe* OPEN_READ "xdg-open" dest))) (close-pipe p)))
                                      (lambda _
                                        #f))))
                                (set! msgsel-index target)
                                #t)
                              (begin (set! msgsel-index target) #t))))
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
      (define (checkpoint-conversation-path checkpoint)
              (format #f
                      "~a/~a.conversation.jsonl"
                      (rollback-manager-storage-dir rollback-mgr)
                      (checkpoint-id checkpoint)))
      (define
        (save-checkpoint-conversation! checkpoint)
        (persist-session-file! (checkpoint-conversation-path checkpoint) llm-history))
      (define
        (restore-checkpoint-conversation! checkpoint-id)
        (let
            ((checkpoint (get-checkpoint rollback-mgr checkpoint-id)))
          (and checkpoint
               (let*
                   ((path (checkpoint-conversation-path checkpoint))
                    (history (load-session-file path)))
                 (and (file-exists? path)
                      (pair? history)
                      (equal? (assoc-ref (car history) 'role) "system")
                      history)))))
      (define (handle-replay-cmd! arg)
              (let*
                  ((checkpoint-id (string-trim-both arg))
                   (history
                     (and (not (string-null? checkpoint-id))
                          (restore-checkpoint-conversation! checkpoint-id))))
                (if (not history)
                  (add-err-msg!
                    "Replay needs a saved checkpoint ID. Use /checkpoint first; /journal lists checkpoint IDs.")
                  (let
                      ((result (rollback-to-checkpoint! rollback-mgr checkpoint-id)))
                    (if
                        (equal? (assoc-ref result 'status) "rolled_back")
                      (begin (set! llm-history history)
                             (persist-session-file! session-file llm-history)
                             (set! %ctx-cache-key #f)
                             (set! messages '())
                             (add-sys-msg!
                               (format #f
                                       "Replayed checkpoint ~a (~a)."
                                       checkpoint-id
                                       (assoc-ref result 'description))))
                      (add-err-msg! (format #f "Replay failed: ~a" (assoc-ref result 'error))))))))
      (define (handle-fork-cmd! arg)
              (let ((requested (string-trim-both arg)))
                (if (string-null? requested)
                  (let
                      ((fork (create-checkpoint! rollback-mgr "Conversation fork")))
                    (if (save-checkpoint-conversation! fork)
                      (add-sys-msg!
                        (format #f "Fork point saved as checkpoint ~a." (checkpoint-id fork)))
                      (add-err-msg!
                        "Fork point created, but its conversation snapshot could not be saved.")))
                  (let
                      ((restored (restore-checkpoint-conversation! requested)))
                    (if (not restored)
                      (add-err-msg!
                        "Fork checkpoint has no saved conversation. Use /checkpoint to create replayable checkpoints.")
                      (let
                          ((result (rollback-to-checkpoint! rollback-mgr requested)))
                        (if
                            (not (equal? (assoc-ref result 'status) "rolled_back"))
                          (add-err-msg! (format #f "Fork failed: ~a" (assoc-ref result 'error)))
                          (begin (set! llm-history restored)
                                 (set! %ctx-cache-key #f)
                                 (set! messages '())
                                 (let
                                     ((fork (create-checkpoint! rollback-mgr (format #f "Fork of ~a" requested))))
                                   (if (save-checkpoint-conversation! fork)
                                     (begin
                                       (persist-session-file! session-file llm-history)
                                       (add-sys-msg!
                                         (format #f "Forked checkpoint ~a as ~a." requested (checkpoint-id fork))))
                                     (add-err-msg!
                                       "Fork checkpoint created, but its conversation snapshot could not be saved.")))))))))))
      (define (handle-checkpoint-cmd! arg)
              (let*
                  ((desc
                     (if (string-null? (string-trim-both arg))
                       "User manual checkpoint"
                       (string-trim-both arg)))
                   (chkpt (create-checkpoint! rollback-mgr desc)))
                (unless (save-checkpoint-conversation! chkpt)
                  (add-err-msg!
                    "Checkpoint created, but the conversation snapshot could not be saved."))
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
      ;; Handle conversation context metrics & unified ctx subcommands
      (define (handle-context-cmd! arg)
              (unless (agent-running?) (sync-context-limit!))
              (execute-context-subcommand! compactor
                                           llm-history
                                           messages
                                           ctx-idx
                                           arg
                                           add-sys-msg!
                                           add-err-msg!
                                           #:jrn
                                           jrn
                                           #:reg
                                           reg))
      ;; Handle per-session token usage report
      (define (handle-usage-cmd!)
              (unless (agent-running?) (sync-context-limit!))
              (add-sys-msg!
                (format-usage-report compactor
                                     llm-history
                                     session-prompt-tokens
                                     session-completion-tokens
                                     session-estimated-tokens
                                     session-turns
                                     #:ctx-idx
                                     ctx-idx)))
      ;; Handle session resume: interactive resurrection picker over recent session logs
      (define (handle-resume-cmd! _arg)
              (let
                  ((items (session-resurrection-menu-items (default-sessions-dir))))
                (if (null? items)
                  (add-sys-msg!
                    "No saved sessions found. A session file is written after every chat turn.")
                  (set! active-menu (make-menu "Resume Session" items)))))
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
                    (add-sys-msg! (format-geis-help-text)))
                  ((string=? sub "bindings")
                    (show-geis-bindings-uplot! stdscr jrn (lambda () (getch stdscr)))
                    (add-sys-msg! (format-geis-bindings-graph jrn reg)))
                  ((string=? sub "add")
                    (if (string-null? rest-arg)
                      (add-sys-msg!
                        "Usage: /geis add <feature description>\nExample: /geis add retrieval for the numpy docs")
                      (let
                          ((user-line (format #f "/geis add ~a" rest-arg)))
                        (record-history! user-line 'chat)
                        (add-user-msg! user-line)
                        (set! status-text "SYNTHESIZING...")
                        (set! current-run-id (+ current-run-id 1))
                        (set! %force-stop-requested? #f)
                        (set! %urgent-steer-requested? #f)
                        (render!)
                        (set! %agent-thread
                              (call-with-new-thread
                                (lambda ()
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
                                      (render!)))))))))
                  ((string=? sub "model")
                    (if (string-null? rest-arg)
                      (let*
                          ((synth-model (badb-config-synthesis-model cfg #f)))
                        (add-sys-msg!
                          (if synth-model
                            (format #f
                                    "Geis synthesis model: ~a (configured in .badb/config.scm)\nUsage: /geis model [NAME]"
                                    synth-model)
                            (format #f
                                    "Geis synthesis model: ~a (default)\nUsage: /geis model [NAME]"
                                    (badb-config-synthesis-model cfg)))))
                      (let ((new-model (string-trim-both rest-arg)))
                        (badb-config-set-synthesis-model! cfg new-model)
                        (try-save-config!)
                        (add-sys-msg!
                          (format #f "Geis synthesis model set to: ~a (saved to config)" new-model)))))
                  ((string=? sub "list") (add-sys-msg! (spec-tool-status)))
                  ((string=? sub "bindings") (add-sys-msg! (format-geis-bindings-graph jrn reg)))
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
                  ((string=? sub "bootstrap") (execute-geis-bootstrap! add-sys-msg! add-err-msg!))
                  ((string=? sub "onboard") (execute-geis-onboard! reg add-sys-msg! add-err-msg!))
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
      (define (trigger-urgent-steering! text)
              (let
                  ((clean
                     (string-trim-both
                       (if (string? text)
                         text
                         ""))))
                (if (string-null? clean)
                  (add-sys-msg! "Usage: /steer ! <message> (or Ctrl+U with steering text)")
                  (begin
                    (with-mutex %steering-mutex (push-urgent-steering! steering-queue clean))
                    (when (agent-running?)
                      (set! %urgent-steer-requested? #t)
                      (kill-active-subprocess!))
                    (add-msg! "steer"
                              (if (agent-running?)
                                (format #f "Urgent steering pushed immediately: ~a" clean)
                                (format #f "Urgent steering placed at front of queue: ~a" clean)))
                    (request-render!)))))
      (define (force-stop-run!)
              (let ((t %agent-thread))
                (set! current-run-id (+ current-run-id 1))
                (set! %force-stop-requested? #t)
                (set! %urgent-steer-requested? #f)
                (set! %agent-thread #f)
                (force-stop-agent! t
                                   #:steering-queue
                                   steering-queue
                                   #:kill-subprocess!
                                   kill-active-subprocess!)
                ;; Strip residual 'interrupt markers so the next prompt is not aborted
                (with-mutex %steering-mutex
                            (let ((entries (steering-drain! steering-queue)))
                              (for-each
                                (lambda (e)
                                  (unless (eq? e 'interrupt) (steering-push! steering-queue e)))
                                entries)))
                (commit-react-stream-message!)
                (reset-react-stream-state!)
                (set! steering-buffer "")
                (set! steering-selector-index 0)
                (set! interrupt-requested? #f)
                (set! status-text "READY")
                (add-msg! "steer" "Agent run force-stopped immediately.")
                (persist-session-file! session-file llm-history)
                (render!)))
      (define (execute-mid-run-safe-command! cmd arg)
              (cond
                ((string=? cmd "stop") (force-stop-run!))
                ((string=? cmd "interrupt")
                  (kill-active-subprocess!)
                  (with-mutex %steering-mutex (steering-push! steering-queue 'interrupt))
                  (set! interrupt-requested? #t)
                  (set! status-text "INTERRUPT REQUESTED")
                  (add-msg! "steer"
                            "Interrupt requested — halting at the next iteration boundary."))
                ((string=? cmd "steer")
                  (call-with-values
                    (lambda ()
                      (parse-steer-argument arg))
                    (lambda (urgent? text)
                      (if (string-null? text)
                        (add-sys-msg! "Usage: /steer [--urgent|!] <message>")
                        (if urgent?
                          (trigger-urgent-steering! text)
                          (begin
                            (with-mutex %steering-mutex (steering-push! steering-queue text))
                            (add-msg! "steer" (format #f "Steering queued for the running agent: ~a" text))))))))
                (else (handle-command! (cons cmd arg)))))
      (define (handle-command! cmd-pair)
              (let
                  ((cmd (resolve-command-alias (car cmd-pair))) (arg (cdr cmd-pair)))
                (cond
                  ((string=? cmd "help") (add-sys-msg! %tui-help-message))
                  ((or (string=? cmd "commands") (string=? cmd "menu"))
                    (set! active-menu (make-commands-root-menu)))
                  ((string=? cmd "keys")
                    (set! active-menu (make-menu "Keybindings" (keymap->menu-items active-keymap))))
                  ((string=? cmd "todo")
                    (call-with-values
                      (lambda ()
                        (handle-todo-command! active-todo-list arg))
                      (lambda (updated txt)
                        (set! active-todo-list updated)
                        (add-sys-msg! txt))))
                  ((string=? cmd "diff") (add-sys-msg! (format-git-diff-output arg)))
                  ((string=? cmd "edit")
                    (let*
                        ((parts (split-words arg))
                         (path (and (pair? parts) (car parts)))
                         (line-num (and (pair? parts) (pair? (cdr parts)) (string->number (cadr parts)))))
                      (if (not path)
                        (add-sys-msg! "Usage: /edit <file> [line]")
                        (call-with-values
                          (lambda ()
                            (open-in-external-editor path
                                                     #:line
                                                     line-num
                                                     #:suspend-tty!
                                                     endwin
                                                     #:resume-tty!
                                                     refresh))
                          (lambda (ok? res)
                            (unless ok? (add-err-msg! (format #f "Editor error: ~a" res))))))))
                  ((string=? cmd "guardrail") (handle-guardrail-cmd! arg))
                  ((string=? cmd "compact") (handle-compact-cmd! arg))
                  ((string=? cmd "resume") (handle-resume-cmd! arg))
                  ((string=? cmd "checkpoint") (handle-checkpoint-cmd! arg))
                  ((string=? cmd "rollback") (handle-rollback-cmd! arg))
                  ((string=? cmd "fork") (handle-fork-cmd! arg))
                  ((string=? cmd "replay") (handle-replay-cmd! arg))
                  ((string=? cmd "rules") (handle-rules-cmd! arg))
                  ((string=? cmd "geis") (handle-geis-cmd! arg))
                  ((string=? cmd "context") (handle-context-cmd! arg))
                  ((string=? cmd "skills")
                    (execute-skills-subcommand! skill-reg arg add-sys-msg! add-err-msg!))
                  ((string=? cmd "memories")
                    (execute-memories-subcommand! arg add-sys-msg! add-err-msg!))
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
                      (set-endpoint-url! arg "Endpoint updated to: ~a (saved to config)")))
                  ((string=? cmd "provider") (handle-provider-cmd! arg))
                  ((string=? cmd "mcp") (handle-mcp-cmd! arg))
                  ((string=? cmd "config") (handle-config-cmd! arg))
                  ((string=? cmd "theme") (handle-theme-cmd! arg))
                  ((string=? cmd "sh")
                    (if (string-null? arg)
                      (add-sys-msg! "Usage: /sh <command> (or !<command>)")
                      (begin (add-cmd-msg! arg) (execute-shell! arg))))
                  ((string=? cmd "eval")
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
                  ((string=? cmd "subagents")
                    (add-sys-msg! (format-mid-run-subagents-summary reg)))
                  ((string=? cmd "stop")
                    (if (agent-running?)
                      (force-stop-run!)
                      (add-sys-msg! "No agent run is currently active.")))
                  ((string=? cmd "steer")
                    (call-with-values
                      (lambda ()
                        (parse-steer-argument arg))
                      (lambda (urgent? text)
                        (if (string-null? text)
                          (add-sys-msg!
                            "Usage: /steer [--urgent|!] <message> (queued or pushed urgently)")
                          (if urgent?
                            (trigger-urgent-steering! text)
                            (begin
                              (with-mutex %steering-mutex (steering-push! steering-queue text))
                              (add-msg! "steer"
                                        (if
                                            (or (agent-running?) (agent-run-active? status-text))
                                          (format #f "Steering queued for the running agent: ~a" text)
                                          (format #f "Steering queued for the next agent run: ~a" text)))))))))
                  ((string=? cmd "interrupt")
                    (kill-active-subprocess!)
                    (with-mutex %steering-mutex (steering-push! steering-queue 'interrupt))
                    (set! interrupt-requested? #t)
                    (add-msg! "steer"
                              (if
                                  (or (agent-running?) (agent-run-active? status-text))
                                "Interrupt requested — the agent halts at the next iteration boundary."
                                "Interrupt queued — the next agent run halts before its first LLM call.")))
                  ((string=? cmd "clear") (set! messages '())
                                          (set! llm-history (list (make-system-turn)))
                                          (set! scroll-offset 0)
                                          (set! msgsel-index #f))
                  ((string=? cmd "dump") (handle-dump-cmd! arg))
                  ((string=? cmd "image")
                    (let ((p (string-trim-both arg)))
                      (cond
                        ((string-null? p)
                          (add-sys-msg! "Usage: /image <path>"))
                        ((not (file-exists? p))
                          (add-err-msg! (format #f "Image file not found: ~a" p)))
                        (else
                          (let ((info (render-ncvisual-overlay! stdscr p (lambda () (getch stdscr)))))
                            (if info
                              (add-sys-msg!
                                (format #f "Rendered image ~a (~a, ~ax~a cells)"
                                        p
                                        (or (assoc-ref info 'blitter) 'ncvisual)
                                        (or (assoc-ref info 'cols) "?")
                                        (or (assoc-ref info 'rows) "?")))
                              (add-sys-msg!
                                (format-image-placeholder p))))))))
                  ((string=? cmd "quit") 'exit)
                  (else
                    (add-err-msg! (format #f "Unknown command: /~a (type /help for commands)" cmd))))))
      ;; Cleanup handler
      (define (safe-curs-set! v)
              (catch #t
                (lambda ()
                  (curs-set v))
                (lambda _
                  #f)))
      (define (shutdown!) (kill-active-subprocess!)
              (catch #t
                (lambda ()
                  (harness-event-unsubscribe! %subagent-bus-sub-id))
                (lambda _
                  #f))
              (safe-write-osc-title! (reset-osc-tab-title))
              (when
                  (and %agent-thread (not (thread-exited? %agent-thread)))
                (catch #t
                  (lambda ()
                    (cancel-thread %agent-thread))
                  (lambda _
                    #f)))
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
                  (when (selector-active? input-buffer prompt-mode)
                    (let
                        ((matches (deduplicate-slash-commands (matching-slash-commands input-buffer))))
                      (set! selector-index
                            (if (null? matches)
                              0
                              (max 0 (min selector-index (- (length matches) 1)))))))
                  (when
                      (at-selector-active? input-buffer cursor-pos prompt-mode)
                    (unless %workspace-files-cache
                      (set! %workspace-files-cache (collect-workspace-files)))
                    (let
                        ((cands
                           (matching-at-candidates input-buffer
                                                   #:cursor-pos
                                                   cursor-pos
                                                   #:candidates
                                                   %workspace-files-cache
                                                   #:limit
                                                   25)))
                      (set! at-selector-index
                            (if (null? cands)
                              0
                              (max 0 (min at-selector-index (- (length cands) 1))))))))
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
                           (when
                               (or needs-render? mouse-dirty? render-dirty?)
                             (set! render-dirty? #f)
                             (render!))
                           (when
                               (and nodelay-proc (or (agent-running?) (not banner-anim-done?)))
                             (nodelay-proc stdscr #t))
                           (let ((ch (key->code (getch stdscr))))
                             (if (agent-running?)
                               (cond
                                 ((and active-menu (not (eqv? ch 3)))
                                   (cond
                                     ((or (not ch) (eqv? ch -1)) (usleep 20000) (loop #f))
                                     ((eqv? ch KEY_UP)
                                       (set! active-menu (menu-move active-menu -1))
                                       (render!)
                                       (loop #f))
                                     ((eqv? ch KEY_DOWN)
                                       (set! active-menu (menu-move active-menu 1))
                                       (render!)
                                       (loop #f))
                                     ((eqv? ch 27) (set! active-menu #f) (render!) (loop #f))
                                     ((or (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13))
                                       (let ((item (menu-selected-item active-menu)))
                                         (when item (apply-menu-selection! item)))
                                       (render!)
                                       (loop #f))
                                     (else (loop #f))))
                                 ;; Ctrl+C: immediate force-stop
                                 ((eqv? ch 3) (set! active-menu #f) (force-stop-run!) (loop #f))
                                 ;; Ctrl+U: urgent steering push (front of queue + stream preemption)
                                 ((eqv? ch 21)
                                   (let
                                       ((trimmed (string-trim-both steering-buffer)))
                                     (set! steering-buffer "")
                                     (set! steering-selector-index 0)
                                     (unless (string-null? trimmed)
                                       (let ((disp (dispatch-mid-run-input trimmed)))
                                         (case (car disp)
                                           ((safe-slash) (execute-mid-run-safe-command! (cadr disp) (caddr disp)))
                                           ((blocked-slash)
                                             (add-sys-msg!
                                               (format #f
                                                       "Command /~a cannot run while the agent is active (safe mid-run: /context, /usage, /theme, /help, /subagents, /stop, /steer)."
                                                       (cadr disp))))
                                           ((steer) (trigger-urgent-steering! (cdr disp)))
                                           (else #f)))))
                                   (render!)
                                   (loop #f))
                                 ;; Tab / Ctrl+I: autocomplete mid-run slash command if typing '/',
                                 ;; otherwise request graceful iteration-boundary interrupt.
                                 ((eqv? ch 9)
                                   (if (selector-active? steering-buffer)
                                     (let*
                                         ((matches (matching-mid-run-slash-commands steering-buffer))
                                          (safe-idx
                                            (if (null? matches)
                                              0
                                              (max 0 (min steering-selector-index (- (length matches) 1)))))
                                          (chosen (and (pair? matches) (car (list-ref matches safe-idx)))))
                                       (when chosen
                                         (set! steering-buffer
                                               (if (string=? chosen "steer")
                                                 "/steer "
                                                 (string-append "/" chosen)))
                                         (set! steering-selector-index 0)))
                                     (begin (kill-active-subprocess!)
                                            (with-mutex %steering-mutex (steering-push! steering-queue 'interrupt))
                                            (set! interrupt-requested? #t)
                                            (set! status-text "INTERRUPT REQUESTED")
                                            (add-msg! "steer"
                                                      "Interrupt requested — halting at the next iteration boundary.")))
                                   (render!)
                                   (loop #f))
                                 ((or (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13))
                                   (let
                                       ((raw
                                          (if (selector-active? steering-buffer)
                                            (let
                                                ((matches (matching-mid-run-slash-commands steering-buffer)))
                                              (if (null? matches)
                                                (string-trim-both steering-buffer)
                                                (let*
                                                    ((safe-idx (max 0 (min steering-selector-index (- (length matches) 1))))
                                                     (chosen (car (list-ref matches safe-idx))))
                                                  (if (string=? chosen "steer")
                                                    ""
                                                    (string-append "/" chosen)))))
                                            (string-trim-both steering-buffer))))
                                     (if
                                         (and (selector-active? steering-buffer)
                                              (string-null? raw)
                                              (pair? (matching-mid-run-slash-commands steering-buffer)))
                                       (begin (set! steering-buffer "/steer ") (set! steering-selector-index 0))
                                       (begin (set! steering-buffer "")
                                              (set! steering-selector-index 0)
                                              (let ((disp (dispatch-mid-run-input raw)))
                                                (case (car disp)
                                                  ((safe-slash) (execute-mid-run-safe-command! (cadr disp) (caddr disp)))
                                                  ((blocked-slash)
                                                    (add-sys-msg!
                                                      (format #f
                                                              "Command /~a cannot run while the agent is active (safe mid-run: /context, /usage, /theme, /help, /subagents, /stop, /steer)."
                                                              (cadr disp))))
                                                  ((steer)
                                                    (with-mutex %steering-mutex (steering-push! steering-queue (cdr disp)))
                                                    (add-msg! "steer" (format #f "Steering queued: ~a" (cdr disp))))
                                                  (else #f))))))
                                   (render!)
                                   (loop #f))
                                 ((or (eqv? ch KEY_BACKSPACE) (eqv? ch 8) (eqv? ch 127))
                                   (when (> (string-length steering-buffer) 0)
                                     (set! steering-buffer
                                           (substring steering-buffer 0 (- (string-length steering-buffer) 1))))
                                   (set! steering-selector-index 0)
                                   (render!)
                                   (loop #f))
                                 ;; Esc: clear non-empty steering buffer, or force-stop if empty
                                 ((eqv? ch 27)
                                   (if (string-null? steering-buffer)
                                     (force-stop-run!)
                                     (begin (set! steering-buffer "") (set! steering-selector-index 0) (render!)))
                                   (loop #f))
                                 ((eqv? ch KEY_PPAGE)
                                   (set! scroll-offset (min max-scroll-seen (+ scroll-offset 10)))
                                   (render!)
                                   (loop #f))
                                 ((eqv? ch KEY_NPAGE)
                                   (set! scroll-offset (max 0 (- scroll-offset 10)))
                                   (render!)
                                   (loop #f))
                                 ((eqv? ch KEY_UP)
                                   (if (selector-active? steering-buffer)
                                     (let
                                         ((matches (matching-mid-run-slash-commands steering-buffer)))
                                       (unless (null? matches)
                                         (let*
                                             ((cnt (length matches)) (cur (max 0 (min steering-selector-index (- cnt 1)))))
                                           (set! steering-selector-index
                                                 (if (<= cur 0)
                                                   (- cnt 1)
                                                   (- cur 1))))))
                                     (set! scroll-offset (min max-scroll-seen (+ scroll-offset 1))))
                                   (render!)
                                   (loop #f))
                                 ((eqv? ch KEY_DOWN)
                                   (if (selector-active? steering-buffer)
                                     (let
                                         ((matches (matching-mid-run-slash-commands steering-buffer)))
                                       (unless (null? matches)
                                         (let*
                                             ((cnt (length matches)) (cur (max 0 (min steering-selector-index (- cnt 1)))))
                                           (set! steering-selector-index (remainder (+ cur 1) cnt)))))
                                     (set! scroll-offset (max 0 (- scroll-offset 1))))
                                   (render!)
                                   (loop #f))
                                 ((eqv? ch KEY_RESIZE) (render!) (loop #f))
                                 ((and (integer? ch) (>= ch 32) (<= ch 126))
                                   (set! steering-buffer
                                         (string-append steering-buffer (string (integer->char ch))))
                                   (set! steering-selector-index 0)
                                   (render!)
                                   (loop #f))
                                 (else (usleep 20000)
                                       (let*
                                           ((now (get-internal-real-time))
                                            (spin-due?
                                              (and
                                                (or (agent-run-active? status-text) (string=? status-text "STREAMING..."))
                                                (>= (- now last-spin-time) (quotient internal-time-units-per-second 12)))))
                                         (when spin-due? (set! last-spin-time now))
                                         (loop spin-due?))))
                               (begin
                                 (when (and nodelay-proc banner-anim-done?) (nodelay-proc stdscr #f))
                                 (unless
                                     (or (not ch) (eqv? ch -1) (eqv? ch KEY_UP) (eqv? ch KEY_DOWN))
                                   (history-reset-nav input-history))
                                 (cond
                                   ;; Active menu popup: takes priority over the selector, input
                                   ;; and scroll keys. Up/Down move the selection, Enter applies
                                   ;; it, Esc closes; other keys are swallowed while the menu is
                                   ;; open, except Ctrl+C / Ctrl+D which still exit below.
                                   ((or (not ch) (eqv? ch -1))
                                     (if banner-anim-done?
                                       (loop #t)
                                       (begin (usleep 25000) (loop (advance-startup-banner!)))))
                                   ((and active-menu (not (or (eqv? ch 3) (eqv? ch 4))))
                                     (cond
                                       ((eqv? ch KEY_UP) (set! active-menu (menu-move active-menu -1)))
                                       ((eqv? ch KEY_DOWN) (set! active-menu (menu-move active-menu 1)))
                                       ((or (eqv? ch 27)
                                            (eqv? ch KEY_LEFT)
                                            (eqv? ch KEY_BACKSPACE)
                                            (eqv? ch 127)
                                            (eqv? ch 8))
                                         (set! active-menu (menu-pop active-menu)))
                                       ((or (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13))
                                         (let ((item (menu-selected-item active-menu)))
                                           (when item (apply-menu-selection! item))))
                                       (else #f))
                                     (loop))
                                   ;; Ctrl+R fuzzy history search popup
                                   ((and (history-search-active? history-search) (not (or (eqv? ch 3) (eqv? ch 4))))
                                     (call-with-values
                                       (lambda ()
                                         (history-search-handle-key history-search input-history ch))
                                       (lambda (action new-st res-buf res-pos)
                                         (set! history-search new-st)
                                         (case action
                                           ((accept cancel) (set! input-buffer res-buf)
                                                            (set! cursor-pos res-pos)
                                                            (clamp-selector-index!)))))
                                     (loop))
                                   ;; Ctrl+R (18): open fuzzy history finder
                                   ((eqv? ch 18)
                                     (set! history-search
                                           (history-search-open input-history input-buffer cursor-pos))
                                     (loop))
                                   ;; Ctrl+X (24): compose/edit input in external $EDITOR
                                   ((eqv? ch 24)
                                     (call-with-values
                                       (lambda ()
                                         (read-via-external-editor #:initial-text
                                                                   input-buffer
                                                                   #:suspend-tty!
                                                                   endwin
                                                                   #:resume-tty!
                                                                   refresh))
                                       (lambda (ok? res)
                                         (if ok?
                                           (begin
                                             (set! input-buffer (string-trim-right res #\newline))
                                             (set! cursor-pos (string-length input-buffer))
                                             (clamp-selector-index!))
                                           (add-err-msg! (format #f "External editor error: ~a" res)))))
                                     (loop))
                                   ;; Shift+Enter: insert a literal newline without submitting.
                                   ;; Enter key: process buffer, execute modal command, or
                                   ;; complete selector. Under cbreak the tty's ICRNL maps the
                                   ;; Enter key to 10, so 10 must submit (Ctrl+N is select-next)
                                   ((or (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13))
                                     (let
                                         ((burst (collect-enter-burst ch input-pending? drain-input-burst!)))
                                       (if burst
                                         ;; Enter with input still queued is a pasted newline,
                                         ;; not a submit: collect the burst and insert it as
                                         ;; multiline text.
                                         (begin (insert-burst! burst) (loop))
                                         (if (selector-active? input-buffer prompt-mode)
                                           (let
                                               ((matches (deduplicate-slash-commands (matching-slash-commands input-buffer))))
                                             (if (null? matches)
                                               (let
                                                   ((trimmed (string-trim-both input-buffer))
                                                    (was-slash? (eq? prompt-mode 'slash)))
                                                 (set! input-buffer "")
                                                 (set! cursor-pos 0)
                                                 (set! selector-index 0)
                                                 (when was-slash? (set! prompt-mode 'chat))
                                                 (if (string-null? trimmed)
                                                   (loop)
                                                   (let*
                                                       ((canonical
                                                          (if was-slash?
                                                            (string-append "/" trimmed)
                                                            trimmed))
                                                        (action (classify-input canonical))
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
                                                   ((member chosen '("provider" "sh" "eval" "steer" "geis" "edit" "todo" "diff"))
                                                     (set! input-buffer (format-slash-completion chosen prompt-mode #t))
                                                     (set! cursor-pos (string-length input-buffer))
                                                     (set! selector-index 0)
                                                     (loop))
                                                   (else (set! input-buffer "")
                                                         (set! cursor-pos 0)
                                                         (set! selector-index 0)
                                                         (when (eq? prompt-mode 'slash) (set! prompt-mode 'chat))
                                                         (let ((res (handle-command! (cons chosen "")))) (unless (eq? res 'exit) (loop))))))))
                                           (let
                                               ((trimmed (string-trim-both input-buffer)) (cur-mode prompt-mode))
                                             (set! input-buffer "")
                                             (set! cursor-pos 0)
                                             (set! selector-index 0)
                                             (when (eq? cur-mode 'slash) (set! prompt-mode 'chat))
                                             (cond
                                               ((string-null? trimmed) (loop))
                                               ((eq? cur-mode 'shell)
                                                 (record-history! trimmed 'shell)
                                                 (add-cmd-msg! trimmed)
                                                 (execute-shell! trimmed)
                                                 (loop))
                                               ((eq? cur-mode 'scheme)
                                                 (record-history! trimmed 'scheme)
                                                 (add-scm-msg! trimmed)
                                                 (execute-scheme! trimmed)
                                                 (loop))
                                               (else
                                                 (let*
                                                     ((canonical (prompt-mode-submit-text cur-mode trimmed))
                                                      (action (classify-input canonical)))
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
                                                       (let*
                                                           ((user-text (cdr action))
                                                            ;; Expand @@, @file:PATH, and @SHORTCUT
                                                            ;; directives before the turn reaches
                                                            ;; ReAct (see capabilities-macro). The
                                                            ;; expansion never raises, so failures
                                                            ;; surface as @error: markers.
                                                            (delivered (expand-macros user-text)))
                                                         (record-history! delivered 'chat)
                                                         (add-user-msg! delivered)
                                                         (set! current-run-id (+ current-run-id 1))
                                                         (set! %force-stop-requested? #f)
                                                         (set! %urgent-steer-requested? #f)
                                                         (set! %agent-thread
                                                               (call-with-new-thread
                                                                 (lambda ()
                                                                   (catch #t
                                                                     (lambda ()
                                                                       (execute-chat-stream! delivered)
                                                                       ;; The ReAct loop ran for a while:
                                                                       ;; ring the terminal bell so an
                                                                       ;; away-from-keyboard user knows badb
                                                                       ;; is waiting for them again.
                                                                       (beep))
                                                                     (lambda (key . args)
                                                                       (add-err-msg! (format #f "Chat error (~a): ~s" key args))
                                                                       (set! status-text "READY")
                                                                       (render!))))))
                                                         (loop))))))))))))
                                   ;; Tab key: autocomplete selected slash command or @ mention
                                   ((eqv? ch 9)
                                     (cond
                                       ((selector-active? input-buffer prompt-mode)
                                         (let*
                                             ((matches (deduplicate-slash-commands (matching-slash-commands input-buffer)))
                                              (chosen (selected-command-name matches)))
                                           (when chosen
                                             (let
                                                 ((needs-arg?
                                                    (and
                                                      (member chosen
                                                              '("model" "endpoint"
                                                                "provider"
                                                                "mcp"
                                                                "config"
                                                                "sh"
                                                                "eval"
                                                                "steer"
                                                                "edit"
                                                                "todo"
                                                                "diff"))
                                                      #t)))
                                               (set! input-buffer (format-slash-completion chosen prompt-mode needs-arg?))
                                               (set! cursor-pos (string-length input-buffer))
                                               (set! selector-index 0)))))
                                       ((at-selector-active? input-buffer cursor-pos prompt-mode)
                                         (unless %workspace-files-cache
                                           (set! %workspace-files-cache (collect-workspace-files)))
                                         (let
                                             ((cands
                                                (matching-at-candidates input-buffer
                                                                        #:cursor-pos
                                                                        cursor-pos
                                                                        #:candidates
                                                                        %workspace-files-cache
                                                                        #:limit
                                                                        25)))
                                           (when (pair? cands)
                                             (let*
                                                 ((safe-i (max 0 (min at-selector-index (- (length cands) 1))))
                                                  (chosen (list-ref cands safe-i)))
                                               (call-with-values
                                                 (lambda ()
                                                   (apply-at-completion input-buffer cursor-pos chosen))
                                                 (lambda (nb np)
                                                   (set! input-buffer nb)
                                                   (set! cursor-pos np)
                                                   (set! at-selector-index 0))))))))
                                     (loop))
                                   ;; Escape key: close selector palette, return to normal chat
                                   ;; mode, or clear the message selection on an empty buffer
                                   ((eqv? ch 27)
                                     (cond
                                       ((or (selector-active? input-buffer prompt-mode) (not (eq? prompt-mode 'chat)))
                                         (call-with-values
                                           (lambda ()
                                             (prompt-mode-on-escape prompt-mode input-buffer cursor-pos))
                                           (lambda (nm nb np)
                                             (set! prompt-mode nm)
                                             (set! input-buffer nb)
                                             (set! cursor-pos np)
                                             (set! selector-index 0))))
                                       ((and (integer? msgsel-index) (string-null? input-buffer))
                                         (set! msgsel-index #f)))
                                     (loop))
                                   ;; Up Arrow: selector navigation, @ mention navigation, or history recall.
                                   ((eqv? ch KEY_UP)
                                     (cond
                                       ((selector-active? input-buffer prompt-mode)
                                         (let
                                             ((matches (deduplicate-slash-commands (matching-slash-commands input-buffer))))
                                           (unless (null? matches)
                                             (let*
                                                 ((count (length matches)) (cur (max 0 (min selector-index (- count 1)))))
                                               (set! selector-index
                                                     (if (<= cur 0)
                                                       (- count 1)
                                                       (- cur 1)))))))
                                       ((at-selector-active? input-buffer cursor-pos prompt-mode)
                                         (let
                                             ((cands
                                                (matching-at-candidates input-buffer
                                                                        #:cursor-pos
                                                                        cursor-pos
                                                                        #:candidates
                                                                        %workspace-files-cache
                                                                        #:limit
                                                                        25)))
                                           (unless (null? cands)
                                             (let*
                                                 ((cnt (length cands)) (cur (max 0 (min at-selector-index (- cnt 1)))))
                                               (set! at-selector-index
                                                     (if (<= cur 0)
                                                       (- cnt 1)
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
                                   ;; Down Arrow: selector navigation, @ mention navigation, or history recall.
                                   ((eqv? ch KEY_DOWN)
                                     (cond
                                       ((selector-active? input-buffer prompt-mode)
                                         (let
                                             ((matches (deduplicate-slash-commands (matching-slash-commands input-buffer))))
                                           (unless (null? matches)
                                             (let*
                                                 ((count (length matches)) (cur (max 0 (min selector-index (- count 1)))))
                                               (set! selector-index (remainder (+ cur 1) count))))))
                                       ((at-selector-active? input-buffer cursor-pos prompt-mode)
                                         (let
                                             ((cands
                                                (matching-at-candidates input-buffer
                                                                        #:cursor-pos
                                                                        cursor-pos
                                                                        #:candidates
                                                                        %workspace-files-cache
                                                                        #:limit
                                                                        25)))
                                           (unless (null? cands)
                                             (let*
                                                 ((cnt (length cands)) (cur (max 0 (min at-selector-index (- cnt 1)))))
                                               (set! at-selector-index (remainder (+ cur 1) cnt))))))
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
                                   ;; Backspace: delete character or return to chat mode if empty
                                   ((or (eqv? ch KEY_BACKSPACE) (eqv? ch 127) (eqv? ch 8))
                                     (call-with-values
                                       (lambda ()
                                         (prompt-mode-on-backspace prompt-mode input-buffer cursor-pos))
                                       (lambda (nm nb np)
                                         (set! prompt-mode nm)
                                         (set! input-buffer nb)
                                         (set! cursor-pos np)
                                         (clamp-selector-index!)))
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
                                   ;; Ctrl+O (15): non-conflicting collapse toggle on selected message
                                   ((eqv? ch 15)
                                     (when
                                         (and (integer? msgsel-index)
                                              (pair? messages)
                                              (< msgsel-index (length messages)))
                                       (toggle-selected-collapse!))
                                     (loop))
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
                                           (call-with-values
                                             (lambda ()
                                               (prompt-mode-on-char prompt-mode input-buffer cursor-pos c))
                                             (lambda (nm nb np _absorbed?)
                                               (set! prompt-mode nm)
                                               (set! input-buffer nb)
                                               (set! cursor-pos np)
                                               (clamp-selector-index!)
                                               (loop)))))))
                                   (else (loop))))))))
          (loop))
        (lambda ()
          (shutdown!))))))