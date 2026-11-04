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

(define-module (geasa surfaces-tui plugin) #:use-module
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
               (geasa capabilities-history-tools plugin)
               #:use-module
               (geasa runtimes-react plugin)
               #:use-module
               (geasa policies-guardrail plugin)
               #:use-module
               (geasa runtimes-context plugin)
               #:use-module
               (geasa runtimes-prompt plugin)
               #:use-module
               (geasa runtimes-rollback plugin)
               #:use-module
               (geasa runtimes-verify plugin)
               #:use-module
               (geasa surfaces-theme plugin)
               #:use-module
               (geasa foundation-log plugin)
               #:use-module
               (geasa foundation-util plugin)
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
               #:export
               (run-tui format-conversation-transcript
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
                        <menu>)
               #:re-export
               (jsonl-line->turn truncate-string))

;;;
;;; Text Formatting & Word Wrapping Helpers
;;;
(define (wrap-line line max-width)
  "Wrap a single LINE into a list of strings with length <= MAX-WIDTH,
breaking on whitespace when possible."
  (let ((line-str (cond
                   ((string? line) line)
                   ((not line) "")
                   (else (format #f "~a" line)))))
    (if (or (<= (string-length line-str) max-width) (<= max-width 0))
        (list line-str)
        (let ((words (string-split line-str #\space)))
          (let loop ((words words) (current "") (acc '()))
            (if (null? words)
                (reverse (if (string-null? current) acc (cons current acc)))
                (let* ((w (car words))
                       (candidate (if (string-null? current)
                                      w
                                      (string-append current " " w))))
                  (cond
                   ((<= (string-length candidate) max-width)
                    (loop (cdr words) candidate acc))
                   ((string-null? current)
                    ;; Single word longer than max-width: hard-break it
                    (loop (cons (substring w max-width) (cdr words))
                          ""
                          (cons (substring w 0 max-width) acc)))
                   (else
                    (loop words "" (cons current acc)))))))))))

(define (wrap-text text max-width)
  "Wrap multi-line TEXT into lines that each fit within MAX-WIDTH."
  (let* ((safe-str (cond
                    ((string? text) text)
                    ((not text) "")
                    (else (format #f "~a" text))))
         (raw-lines (string-split safe-str #\newline)))
    (append-map (lambda (line) (wrap-line (if (string? line) line "") max-width)) raw-lines)))

;;;
;;; Slash Commands Registry & Parsing
;;;
(define %slash-commands
        '(("help" . "Show help manual & shortcuts")
          ("model" . "Switch or inspect active LLM model")
          ("models" . "List models available on the endpoint")
          ("endpoint" . "Switch or inspect endpoint URL")
          ("provider" . "Switch or inspect model provider preset (e.g. lmstudio)")
          ("mcp" . "Manage MCP servers (list, start, stop, add, remove, tools)")
          ("config" . "Inspect, reload, or save badb configuration")
          ("theme" . "Inspect or switch the active UI theme")
          ("guardrail" . "Inspect or set guardrail security policy level")
          ("compact" . "Compact conversation history context")
          ("resume" . "Resume a previous chat session from disk")
          ("checkpoint" . "Create a workspace and registry checkpoint")
          ("rollback" . "Revert to latest or specified checkpoint")
          ("rules" . "Inspect discovered project rules & workspace tree")
          ("geis" . "Contract-gated dynamic harness synthesis (/geis add <desc>, list, verify, status)")
          ("context" . "View conversation token stats and metrics")
          ("usage" . "Show per-session token usage report")
          ("features" . "List kernel registered features")
          ("journal" . "View checkpoint provenance audit log")
          ("test" . "Run bootstrap verification test suite")
          ("sh" . "Execute shell command (!<cmd>)")
          ("eval" . "Evaluate Scheme code in REPL (,<expr>)")
          ("steer" . "Queue steering text for the running agent")
          ("interrupt" . "Halt the running agent at its next boundary")
          ("clear" . "Clear chat viewport history")
          ("quit" . "Exit badb microharness")
          ("exit" . "Exit badb microharness")
          ("dump" . "Export the conversation to a text file [path]")))

(define (selector-active? input-buffer)
  "Return #t if the slash command selector palette should be visible."
  (and (string-prefix? "/" input-buffer)
       (not (string-contains input-buffer " "))))

(define (matching-slash-commands input-buffer)
  "Return list of (%slash-commands) items matching the current query in INPUT-BUFFER."
  (let* ((prefix (if (string-prefix? "/" input-buffer)
                     (substring input-buffer 1)
                     input-buffer))
         (clean-q (string-downcase (string-trim-both prefix))))
    (if (string-null? clean-q)
        %slash-commands
        (filter (lambda (item)
                  (string-contains (string-downcase (car item)) clean-q))
                %slash-commands))))

(define %selector-max-visible 7)

(define* (selector-visible-slice matches #:optional (max-visible %selector-max-visible) (sel-index 0))
  "Return (values visible-items offset sel-pos above-count below-count):
the window of at most MAX-VISIBLE items of MATCHES positioned so SEL-INDEX
stays visible inside the slice. Returns offset, relative sel-pos within the slice,
and above/below scroll counts."
  (let* ((items (if (list? matches) matches '()))
         (count (length items))
         (cap (max 1 (or max-visible %selector-max-visible)))
         (sel (min (max 0 sel-index) (max 0 (- count 1)))))
    (if (<= count cap)
        (values items 0 sel 0 0)
        (let* ((start (min (max 0 (- (+ sel 1) cap)) (- count cap)))
               (vis-items (take (drop items start) cap))
               (offset start)
               (sel-pos (- sel offset))
               (above-count offset)
               (below-count (- count offset cap)))
          (values vis-items offset sel-pos above-count below-count)))))

(define (parse-command input)
  "Parse user INPUT into (command . arg-string) or #f if not a slash command."
  (let ((trimmed (string-trim-both input)))
    (if (string-prefix? "/" trimmed)
        (let* ((without-slash (substring trimmed 1))
               (space-pos (string-index without-slash #\space)))
          (if space-pos
              (cons (substring without-slash 0 space-pos)
                    (string-trim-both (substring without-slash (+ space-pos 1))))
              (cons without-slash "")))
        #f)))

;;;
;;; Model Thoughts & Reasoning Separation Helper
;;;
(define (split-thoughts-and-content text)
  "Separate thought blocks (<think>...</think> or <thought>...</thought>)
from response content in TEXT. Handles unclosed tags from live streaming.
Returns (values thoughts content)."
  (let loop ((remaining text)
             (thoughts-acc '())
             (content-acc '()))
    (let* ((think-start (string-contains remaining "<think>"))
           (thought-start (string-contains remaining "<thought>"))
           (start-tag (cond
                       ((and think-start thought-start)
                        (if (< think-start thought-start)
                            (cons think-start "<think>")
                            (cons thought-start "<thought>")))
                       (think-start (cons think-start "<think>"))
                       (thought-start (cons thought-start "<thought>"))
                       (else #f))))
      (if start-tag
          (let* ((start-pos (car start-tag))
                 (tag-str (cdr start-tag))
                 (tag-len (string-length tag-str))
                 (end-tag-str (if (string=? tag-str "<think>") "</think>" "</thought>"))
                 (prefix-content (substring remaining 0 start-pos))
                 (after-tag (substring remaining (+ start-pos tag-len)))
                 (end-pos (string-contains after-tag end-tag-str)))
            (if end-pos
                (let ((thought-body (substring after-tag 0 end-pos))
                      (rest-after (substring after-tag (+ end-pos (string-length end-tag-str)))))
                  (loop rest-after
                        (cons (string-trim-both thought-body) thoughts-acc)
                        (if (string-null? (string-trim-both prefix-content))
                            content-acc
                            (cons (string-trim-both prefix-content) content-acc))))
                (let ((thought-body (string-trim-both after-tag)))
                  (let* ((all-thoughts (reverse (cons thought-body thoughts-acc)))
                         (all-content (if (string-null? (string-trim-both prefix-content))
                                          (reverse content-acc)
                                          (reverse (cons (string-trim-both prefix-content) content-acc))))
                         (final-th (string-trim-both (string-join all-thoughts "\n\n")))
                         (final-ct (string-trim-both (string-join all-content "\n"))))
                    (values (if (string-null? final-th) #f final-th)
                            final-ct)))))
          (let* ((all-c (if (string-null? (string-trim-both remaining))
                            content-acc
                            (cons (string-trim-both remaining) content-acc)))
                 (final-content (string-trim-both (string-join (reverse all-c) "\n")))
                 (final-thoughts (string-trim-both (string-join (reverse thoughts-acc) "\n\n"))))
            (values (if (string-null? final-thoughts) #f final-thoughts)
                    final-content))))))

;;;
;;; Theme Color Mapping
;;;
(define (color-symbol->ncurses-constant color)
  "Map a theme COLOR symbol to the name of the matching color constant
symbol (resolved against the (geasa surfaces-tui nc) backend module at runtime
by the renderer). The neutral 'default maps to white."
  (case color
    ((black) 'COLOR_BLACK)
    ((red) 'COLOR_RED)
    ((green) 'COLOR_GREEN)
    ((yellow) 'COLOR_YELLOW)
    ((blue) 'COLOR_BLUE)
    ((magenta) 'COLOR_MAGENTA)
    ((cyan) 'COLOR_CYAN)
    ((white) 'COLOR_WHITE)
    (else 'COLOR_WHITE)))

;;;
;;; Morphing Prompt Indicator
;;;
(define %spinner-frames (vector "⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧"))
(define (streaming-spinner-glyph)
  "Return the current braille spinner frame, advanced by wall-clock
time so the prompt indicator animates on every streaming redraw."
  (let* ((ms (quotient (get-internal-run-time)
                       (quotient internal-time-units-per-second 1000)))
         (i (modulo (quotient ms 80) (vector-length %spinner-frames))))
    (vector-ref %spinner-frames i)))

(define* (get-prompt-indicator input-buffer status-text #:optional (prompt-mode 'chat))
  "Return (values glyph-char theme-role bold?) for the prompt indicator
based on current input mode, execution status, and active prompt mode."
  (cond
   ((string=? status-text "STREAMING...")
    (values "⟳" 'status #t))     ; Status-role bold spinner
   ((string=? status-text "EXEC...")
    (values "◆" 'shell #t))      ; Shell-role bold diamond
   ((string-prefix? "TOOL:" status-text)
    (values "◆" 'tool-call #t))  ; Tool-role bold diamond
   ((string-prefix? "REACT" status-text)
    (values "◆" 'tool-call #t))  ; Tool-role bold diamond
   ((string=? status-text "EVAL...")
    (values "λ" 'scheme #t))     ; Scheme-role bold lambda
   ((string=? status-text "TESTING...")
    (values "◆" 'warning #t))   ; Warning-role bold diamond
   ((eq? prompt-mode 'shell)
    (values "$" 'shell #t))     ; Shell-role bold shell mode
   ((eq? prompt-mode 'scheme)
    (values "λ" 'scheme #t))    ; Scheme-role bold scheme mode
   ((string-prefix? "!" input-buffer)
    (values "$" 'shell #t))     ; Shell-role bold shell prefix
   ((string-prefix? "(" input-buffer)
    (values "λ" 'scheme #t))    ; Scheme-role bold lambda prefix
   (else
    (values "❯" 'accent #t))))   ; Accent-role bold chat

;;;
;;; Slim Header, Rule & Keymap Layout Formatters
;;;
(define (format-rule-line maxx)
  "Return a thin horizontal rule line filling MAXX columns for the header
underline."
  (make-string (max 0 maxx) #\─))

(define (status->theme-role status-text)
  "Map a STATUS-TEXT word to the theme role it should carry in the
header: idle is success-green, active work is accent-cyan, and anything
else falls back to the neutral status role."
  (cond
    ((string=? status-text "READY") 'success)
    ((or (string-prefix? "EXEC" status-text)
         (string-prefix? "EVAL" status-text)
         (string-prefix? "STREAMING" status-text)
         (string-prefix? "TESTING" status-text)
         (string-prefix? "REACT" status-text)
         (string-prefix? "TOOL" status-text))
      'accent)
    (else 'status)))

(define* (format-header-segments maxx status-text model-name feat-count jrn-count
                                 #:optional (mcp-info #f) (ctx-info #f))
  "Build the slim header line as positioned segments of the form
((x text role bold?) ...) for a terminal MAXX columns wide: the badb
wordmark leads in the accent role with the model name dim behind it, and
the status word plus nonzero feature / checkpoint / mcp counts sit flush
right. Right-side items drop out, least important first, whenever the two
sides would collide; the model name truncates last."
  (let* ((wordmark "badb")
         (model (cond
                 ((and (string? model-name) (not (string-null? model-name))) model-name)
                 (else "")))
         (right-items
           (filter-map
             (lambda (item) (and (pair? item) item))
             (list (and (string? status-text) (not (string-null? status-text))
                        (cons (status->theme-role status-text) status-text))
                   (and (string? ctx-info) (not (string-null? ctx-info))
                        (cons 'dim ctx-info))
                   (and (> feat-count 0) (cons 'dim (format #f "~a feats" feat-count)))
                   (and (> jrn-count 0) (cons 'dim (format #f "~a chkpts" jrn-count)))
                   (and (string? mcp-info) (not (string-null? mcp-info))
                        (cons 'dim (format #f "mcp ~a" mcp-info))))))
         (right-width
           (lambda (items)
             (if (null? items)
                 0
                 (+ (- (length items) 1)
                    (fold (lambda (i acc) (+ acc (string-length (cdr i)))) 0 items)))))
         (build
           (lambda (model-fit items)
             (let* ((left-segs
                      (if (string-null? model-fit)
                        (list (list 0 wordmark 'accent #t))
                        (list (list 0 wordmark 'accent #t)
                              (list 5 model-fit 'dim #f))))
                    (rw (right-width items))
                    (right-start (max 0 (- maxx rw))))
               (append
                left-segs
                (let rloop ((rest items) (x right-start))
                  (if (null? rest)
                      '()
                      (cons (list x (cdar rest) (caar rest) #f)
                            (rloop (cdr rest) (+ x (string-length (cdar rest)) 1))))))))))
    (let fit ((items right-items))
      (let* ((rw (right-width items))
             (fits-full?
               (<= (+ 4 1 (string-length model) 2 rw) (- maxx 1)))
             (model-room (max 0 (- maxx 1 2 rw 4 1))))
        (cond
          (fits-full? (build model items))
          ;; The model name truncates to keep the current right items
          ((>= model-room 1) (build (substring model 0 model-room) items))
          ;; No room for a model: drop the least important right item
          ((pair? items) (fit (drop-right items 1)))
          (else (build "" '())))))))

(define (format-keymap-line mode)
  "Return the dim bottom keymap hint line describing the keys active in
MODE (a symbol: chat, shell, scheme, selector, menu or steering)."
  (case mode
    ((shell) "enter:run · backspace:back to chat")
    ((scheme) "enter:eval · backspace:back to chat")
    ((selector) "↑↓:choose · tab:complete · enter:run · esc:dismiss")
    ((menu) "↑↓:move · enter:apply · esc:close")
    ((steering) "type:steer · enter:send · ^i:interrupt · bs:edit · esc:clear")
    (else "! shell · ( scheme · / cmds · ↑↓ history · PgUp/PgDn scroll · ^↑/^↓ select · ^e collapse")))

;;;
;;; Agent Steering Helpers
;;;
;; Pure keyboard-to-intent mapping and presentation formatters for mid-run
;; steering: while the autonomous ReAct loop executes, keypresses polled
;; from the terminal accumulate in a steering buffer and are pushed onto
;; the session's steering queue (injected as user messages at the next
;; iteration boundary, or halting the loop with 'interrupt). Keeping these
;; helpers pure makes the curses wiring trivial and testable headless.
(define (steering-key-handle buffer ch)
  "Map a keypress CH to a steering action and the resulting buffer.
Return (values new-buffer action) with actions:
  'append     printable character appended to BUFFER
  'backspace  last character deleted (Ctrl+H / backspace / DEL)
  'queue      BUFFER submitted to the steering queue (Enter); a queue
              action is only produced for non-blank content
  'interrupt  interrupt requested (Ctrl+I / Tab); the buffer is discarded
  'clear      buffer discarded (Escape)
  'none       key ignored (other control chars, curses key codes)
CH is either a character (printable input) or an integer control code or
curses key constant, matching what getch delivers under keypad mode."
  (cond
   ;; Enter (newline, carriage return, or their integer codes)
   ((or (eqv? ch #\newline) (eqv? ch #\return) (eqv? ch 10) (eqv? ch 13))
    (if (string-null? (string-trim-both buffer))
        (values buffer 'none)
        (values "" 'queue)))
   ;; Ctrl+I (Tab): request an interrupt, discarding the pending buffer
   ((or (eqv? ch #\tab) (eqv? ch 9))
    (values "" 'interrupt))
   ;; Escape: discard the pending buffer
   ((eqv? ch 27)
    (values "" 'clear))
   ;; Backspace / Ctrl+H / DEL: delete the last character
   ((or (eqv? ch #\backspace) (eqv? ch 8) (eqv? ch 127))
    (if (string-null? buffer)
        (values buffer 'none)
        (values (substring buffer 0 (- (string-length buffer) 1)) 'backspace)))
   ;; Printable characters append
   ((and (char? ch) (>= (char->integer ch) 32))
    (values (string-append buffer (string ch)) 'append))
   ;; Integer codes in the printable range (some terminals deliver these)
   ((and (integer? ch) (>= ch 32) (<= ch 126))
    (values (string-append buffer (string (integer->char ch))) 'append))
   ;; Everything else (control chars, arrow and function key codes)
   (else (values buffer 'none))))

(define* (steering-pending-line buffer queued-count
                                #:optional (interrupt-queued? #f))
  "Compose the dim pending-steering display line near the prompt: the
in-progress BUFFER being typed mid-run, the QUEUED-COUNT of steering
entries queued but not yet consumed by the loop, and the INTERRUPT-QUEUED?
pill shown while an interrupt is pending. Return #f when nothing is
pending and no line should be drawn."
  (cond
   (interrupt-queued? "steering: interrupt requested")
   ((and (not (string-null? buffer)) (> queued-count 0))
    (format #f "steering: ~a (+~a queued)" buffer queued-count))
   ((not (string-null? buffer))
    (format #f "steering: ~a" buffer))
   ((> queued-count 0)
    (format #f "steering: ~a queued" queued-count))
   (else #f)))

(define (agent-run-active? status-text)
  "Return #t when STATUS-TEXT indicates the autonomous ReAct loop is
currently executing (a REACT iteration or TOOL status)."
  (and (string? status-text)
       (or (string-prefix? "REACT" status-text)
           (string-prefix? "TOOL:" status-text))
       #t))

;;;
;;; Shell & REPL Execution Helpers
;;;
(define (classify-input input)
  "Classify trimmed user INPUT into an action tag and payload:
   '(slash cmd . arg)
   '(shell . cmd-string)
   '(scheme . code-string)
   '(chat . text)
   #f for empty or whitespace-only input"
  (let ((trimmed (string-trim-both input)))
    (cond
     ((string-null? trimmed)
      #f)
     ((string-prefix? "!" trimmed)
      (cons 'shell (string-trim-both (substring trimmed 1))))


     ((string-prefix? "(" trimmed)
      (cons 'scheme trimmed))
     ((string-prefix? "/" trimmed)
      (let* ((without-slash (substring trimmed 1))
             (space-pos (string-index without-slash #\space)))
        (if space-pos
            (list 'slash
                  (substring without-slash 0 space-pos)
                  (string-trim-both (substring without-slash (+ space-pos 1))))
            (list 'slash without-slash ""))))
     (else
      (cons 'chat trimmed)))))

(define (normalize-scheme-expr str)
  "Normalize user Scheme input string, ensuring s-expressions without outer
parentheses (e.g. '+ 1 2' or '+ 1 2)') are wrapped for evaluation."
  (let ((trimmed (string-trim-both str)))
    (cond
     ((string-null? trimmed) trimmed)
     ;; Already properly wrapped
     ((string-prefix? "(" trimmed) trimmed)
     ;; Literal string or quote
     ((or (string-prefix? "\"" trimmed) (string-prefix? "'" trimmed)) trimmed)
     ;; Ends with ')' but missing '(': e.g. "+ 1 2)"
     ((and (string-suffix? ")" trimmed) (not (string-prefix? "(" trimmed)))
      (string-append "(" trimmed))
     ;; Has spaces and no outer parens: e.g. "+ 1 2"
     ((string-contains trimmed " ")
      (string-append "(" trimmed ")"))
     ;; Standalone identifier or literal number: e.g. "reg", "42"
     (else trimmed))))

(define* (make-repl-mod reg jrn mgr client #:optional (cfg #f) (mcp-mgr #f)
                        (policy #f) (compactor #f) (rollback-mgr #f) (verifier #f) (composer #f))
  "Create an isolated user module pre-populated with kernel state bindings
and core badb modules for interactive evaluation."
  (let ((mod (make-fresh-user-module)))
    (module-use! mod (resolve-interface '(guile)))
    (module-use! mod (resolve-interface '(geasa providers-model plugin)))
    (module-use! mod (resolve-interface '(badb registry)))
    (module-use! mod (resolve-interface '(badb transaction)))
    (module-use! mod (resolve-interface '(badb journal)))
    (module-use! mod (resolve-interface '(geasa foundation-config plugin)))
    (catch #t (lambda () (module-use! mod (resolve-interface '(geasa capabilities-mcp plugin)))) (lambda _ #f))
    (catch #t (lambda () (module-use! mod (resolve-interface '(geasa policies-guardrail plugin)))) (lambda _ #f))
    (catch #t (lambda () (module-use! mod (resolve-interface '(geasa runtimes-context plugin)))) (lambda _ #f))
    (catch #t (lambda () (module-use! mod (resolve-interface '(geasa runtimes-prompt plugin)))) (lambda _ #f))
    (catch #t (lambda () (module-use! mod (resolve-interface '(geasa runtimes-rollback plugin)))) (lambda _ #f))
    (catch #t (lambda () (module-use! mod (resolve-interface '(geasa runtimes-verify plugin)))) (lambda _ #f))
    (catch #t (lambda () (module-use! mod (resolve-interface '(geasa runtimes-react plugin)))) (lambda _ #f))
    (module-use! mod (resolve-interface '(ice-9 format)))
    (module-use! mod (resolve-interface '(ice-9 rdelim)))
    (module-use! mod (resolve-interface '(ice-9 pretty-print)))
    ;; (ice-9 session) carries the REPL introspection toolkit (apropos,
    ;; module-inspect helpers) so the scheme prompt can explore the
    ;; harness beyond plain eval.
    (catch #t
      (lambda () (module-use! mod (resolve-interface '(ice-9 session))))
      (lambda _ #f))
    (module-use! mod (resolve-interface '(srfi srfi-1)))
    (module-define! mod 'reg reg)
    (module-define! mod 'jrn jrn)
    (module-define! mod 'mgr mgr)
    (module-define! mod 'client client)
    (when cfg (module-define! mod 'cfg cfg))
    (when mcp-mgr (module-define! mod 'mcp-mgr mcp-mgr))
    (when policy (module-define! mod 'policy policy))
    (when compactor (module-define! mod 'compactor compactor))
    (when rollback-mgr (module-define! mod 'rollback-mgr rollback-mgr))
    (when verifier (module-define! mod 'verifier verifier))
    (when composer (module-define! mod 'composer composer))
    mod))

(define (eval-scheme-code code-str repl-mod)
  "Evaluate CODE-STR in REPL-MOD and return (values result-string error-or-#f)."
  (let ((normalized (normalize-scheme-expr code-str)))
    (catch #t
      (lambda ()
        (let* ((p (open-input-string normalized))
               (output-port (open-output-string)))
          (let loop ()
            (let ((expr (read p)))
              (unless (eof-object? expr)
                (let ((printed
                       (with-output-to-string
                         (lambda ()
                           (call-with-values (lambda () (eval expr repl-mod))
                             (lambda vals
                               (for-each (lambda (v)
                                           (format #t "=> ~s\n" v))
                                         vals)))))))
                  (display printed output-port)
                  (loop)))))
          (let ((res (string-trim-right (get-output-string output-port))))
            (values (if (string-null? res) "=> #<unspecified>" res) #f))))
      (lambda (key . args)
        (values #f (format #f "Scheme error (~a): ~s" key args))))))

(define (%exec-log-path)
  "Exec log destination: BADB_EXEC_LOG when set (empty string disables
logging), else .badb/exec-log.jsonl."
  (let ((env (getenv "BADB_EXEC_LOG")))
    (cond
      ((and (string? env) (string-null? env)) #f)
      ((string? env) env)
      (else ".badb/exec-log.jsonl"))))

(define (%append-exec-log! cmd)
  "Append a byte-exact record of CMD to the exec log just before spawn,
best effort: never throws, silently skips when the log is disabled or
unwritable.  The cmd field is the exact string handed to the shell —
the transcript's post-decode alist views are re-escaped renders, so
this file is the ground truth when diagnosing quoting-layer corruption
(a lost paren, a stripped escape, a truncated quote).
Format: one JSON object per line: {ts, us, len, cmd}."
  (catch #t
    (lambda ()
      (let ((path (%exec-log-path)))
        (when path
          (let ((dir (dirname path)))
            (unless (file-exists? dir) (ensure-directory! dir)))
          (let* ((tv (gettimeofday))
                 (port (open-file path "a")))
            (format port "{\"ts\":~a,\"us\":~a,\"len\":~a,\"cmd\":~a}\n"
                    (car tv) (cdr tv) (string-length cmd) (scm->json-string cmd))
            (close-port port)))))
    (lambda _ #f)))

(define (execute-shell-command cmd)
  "Execute shell CMD and return (values output-string exit-code
error-or-#f).  Records the byte-exact command to the exec log
(.badb/exec-log.jsonl, BADB_EXEC_LOG override; empty disables) just
before spawn."
  (%append-exec-log! cmd)
  (catch #t
    (lambda ()
      (let* ((pipe (open-pipe (string-append cmd " 2>&1") OPEN_READ))
             (raw-out (read-string pipe))
             (status (close-pipe pipe))
             (exit-val (if (status:exit-val status) (status:exit-val status) 0))
             (clean-out (if (or (eof-object? raw-out) (string-null? raw-out))
                            ""
                            (string-trim-right raw-out))))
        (values clean-out exit-val #f)))
    (lambda (key . args)
      (values #f 1 (format #f "Shell error (~a): ~s" key args)))))

;;;
;;; Visual Hierarchy & Token Highlight Engine
;;;
;; Canonical status pills with (tag theme-role bold? dim?)
(define %status-tags
  (list (list "[RUNNING]" 'success #t #f)
        (list "[OK]" 'success #t #f)
        (list "[ALLOW]" 'success #t #f)
        (list "[ACTIVE]" 'success #t #f)
        (list "[PASSED]" 'success #t #f)
        (list "[READY]" 'success #t #f)
        (list "[STOPPED]" 'warning #t #f)
        (list "[WARN]" 'warning #t #f)
        (list "[WARNING]" 'warning #t #f)
        (list "[ASK]" 'warning #t #f)
        (list "[PENDING]" 'warning #t #f)
        (list "[CAUTIOUS]" 'warning #t #f)
        (list "[STRICT]" 'warning #t #f)
        (list "[ERROR]" 'error #t #f)
        (list "[DENY]" 'error #t #f)
        (list "[FAILED]" 'error #t #f)
        (list "[PARANOID]" 'error #t #f)
        (list "[PERMISSIVE]" 'notice #t #f)
        (list "[DISABLED]" 'dim #t #f)))

;; Regex for key label at the start of a clause: e.g. "Model: ", "System Guardrail: "
(define %key-label-rx (make-regexp "^[ \t]*([A-Za-z0-9_-]+( [A-Za-z0-9_-]+){0,2}):[ \t]+"))

(define (segment-line line)
  "Segment LINE into styled chunks of the form ((chunk-str theme-role bold? dim?) ...),
mapping status pills, list bullets and key labels to their semantic theme
roles and plain text to the neutral 'default role."
  (let loop ((pos 0) (acc '()))
    (if (>= pos (string-length line))
        (reverse acc)
        (let* ((rem (substring line pos))
               (tag-match
                (find (lambda (tag-entry)
                        (string-prefix? (car tag-entry) rem))
                      %status-tags)))
          (cond
           (tag-match
            (let* ((tag-str (car tag-match))
                   (theme-role (cadr tag-match))
                   (bold? (caddr tag-match))
                   (dim? (cadddr tag-match)))
              (loop (+ pos (string-length tag-str))
                    (cons (list tag-str theme-role bold? dim?) acc))))
           ((or (string-prefix? "• " rem) (string-prefix? "❯ " rem))
            (let ((bullet-str (substring rem 0 2)))
              (loop (+ pos 2)
                    (cons (list bullet-str 'accent #t #f) acc))))
           ((regexp-exec %key-label-rx rem)
            => (lambda (m)
                 (let* ((full-match (match:substring m 0))
                        (match-len (string-length full-match)))
                   (loop (+ pos match-len)
                         (cons (list full-match 'accent #f #f) acc)))))
           (else
            (let find-next ((end (+ pos 1)))
              (if (>= end (string-length line))
                  (reverse (cons (list (substring line pos) 'default #f #f) acc))
                  (let ((sub-rem (substring line end)))
                    (if (or (any (lambda (t) (string-prefix? (car t) sub-rem)) %status-tags)
                            (string-prefix? "• " sub-rem)
                            (string-prefix? "❯ " sub-rem)
                            (regexp-exec %key-label-rx sub-rem))
                        (loop end
                              (cons (list (substring line pos end) 'default #f #f) acc))
                        (find-next (+ end 1))))))))))))

;;;
;;; Input Buffer Cursor Editing Helpers
;;;
(define (clamp-cursor pos buffer)
  "Clamp cursor position POS into the valid range 0..(string-length BUFFER)."
  (max 0 (min pos (string-length buffer))))

(define (insert-at buffer pos str)
  "Insert STR into BUFFER at cursor position POS.
Return (values new-buffer new-pos) with the cursor advanced past STR."
  (let* ((p (clamp-cursor pos buffer))
         (new-buffer (string-append (substring buffer 0 p)
                                    str
                                    (substring buffer p))))
    (values new-buffer (+ p (string-length str)))))

(define (backspace-at buffer pos)
  "Delete the character before cursor position POS in BUFFER.
A no-op when POS is 0. Return (values new-buffer new-pos)."
  (let ((p (clamp-cursor pos buffer)))
    (if (<= p 0)
        (values buffer 0)
        (values (string-append (substring buffer 0 (- p 1))
                               (substring buffer p))
                (- p 1)))))

(define (delete-at buffer pos)
  "Delete the character at cursor position POS in BUFFER (KEY_DC semantics).
A no-op when POS is at the end. Return (values new-buffer new-pos)."
  (let* ((p (clamp-cursor pos buffer))
         (len (string-length buffer)))
    (if (>= p len)
        (values buffer p)
        (values (string-append (substring buffer 0 p)
                               (substring buffer (+ p 1)))
                p))))

(define (key->code ch)
  "Normalize a keypress CH from the backend's getch to an integer code:
chars become their codepoint, integers pass through unchanged, anything
else (e.g. #f from a failed/non-blocking read) stays as-is. The backend
may return control keys as chars while function keys arrive as
integers — so all key dispatch must compare against the normalized code."
  (cond
   ((not ch) ch)
   ((char? ch) (char->integer ch))
   (else ch)))

(define (resolve-modified-key seq probe bind fallback)
  "Return the integer code getch delivers when the terminal sends the
escape sequence SEQ, for modifier combinations like Ctrl+Up whose codes
terminfo assigns dynamically or not at all. PROBE returns the code
already bound to SEQ or #f; when nothing is bound, BIND associates SEQ
with FALLBACK. Returns 0 - a code no keypress produces - when the key
cannot be resolved, so callers can dispatch safely."
  (or (probe seq)
      (and (bind seq fallback) fallback)
      0))

(define (input-visible-window buffer cursor-pos avail-width)
  "Compute the horizontally scrolled slice of BUFFER that fits a window
AVAIL-WIDTH columns wide such that CURSOR-POS is always visible.
Return (values visible-substring cursor-x-in-window). When the buffer fits
entirely no scrolling occurs; otherwise the window follows the cursor and
prefers to keep at least one column of right context in view."
  (let* ((len (string-length buffer))
         (pos (clamp-cursor cursor-pos buffer))
         (w (max 1 avail-width)))
    (cond
     ((<= len w)
      (values buffer pos))
     ((= w 1)
      (let ((start (min pos len)))
        (values (substring buffer start (min len (+ start 1))) 0)))
     ((<= pos (- w 2))
      (values (substring buffer 0 w) pos))
     ((>= pos len)
      (let ((start (- len w)))
        (values (substring buffer start len) w)))
     (else
      (let ((start (- pos (- w 2))))
        (values (substring buffer start (min len (+ start w)))
                (- pos start)))))))

;; Maximum rows the wrapped input may occupy before it scrolls.
(define %max-input-rows 4)

(define* (input-wrapped-rows buffer cursor-pos avail-width max-rows)
  "Lay out BUFFER for multi-line wrapped display AVAIL-WIDTH columns wide,
keeping at most MAX-ROWS lines and scrolling so the line holding the
editing cursor stays visible. Return (values visible-lines cursor-row
cursor-col): VISIBLE-LINES is the list of rendered lines, CURSOR-ROW the
0-based index of the cursor's line within VISIBLE-LINES, CURSOR-COL the
column of the cursor within that line."
  (let* ((w (max 1 avail-width))
         (mr (max 1 max-rows))
         (lines (wrap-text buffer w))
         (prefix (substring buffer 0 (clamp-cursor cursor-pos buffer)))
         (prefix-lines (wrap-text prefix w))
         (cur-row (max 0 (- (length prefix-lines) 1)))
         (cur-col (string-length (car (last-pair prefix-lines))))
         (total (length lines))
         (scrolled (if (<= total mr)
                     0
                     (max 0
                          (min (max 0 (- cur-row (- mr 1)))
                               (- total mr)))))
         (visible (take (drop lines scrolled)
                        (min mr (- total scrolled)))))
    (values visible
            (max 0 (- cur-row scrolled))
            (min cur-col w))))

;;;
;;; Input History Recall & Session Persistence
;;;
(define %history-max-entries 500)

(define-record-type <input-history>
  (%make-input-history entries nav-index)
  input-history?
  (entries history-entries set-history-entries!)
  (nav-index history-recall-position set-history-recall-position!))

(define (history-cap-entries entries)
  "Return ENTRIES truncated to the newest %history-max-entries items,
dropping the oldest entries when over the cap."
  (let ((len (length entries)))
    (if (> len %history-max-entries)
        (take-right entries %history-max-entries)
        entries)))

(define* (make-history #:optional (initial-entries '()))
  "Create a fresh input history seeded with INITIAL-ENTRIES (oldest first),
capped at %history-max-entries, with the recall position at the newest
boundary (draft state)."
  (%make-input-history (history-cap-entries initial-entries) #f))

(define (history-length hist)
  "Return the number of recorded entries in HIST."
  (length (history-entries hist)))

(define (history-add hist entry)
  "Append ENTRY (trimmed) to HIST, skipping empty input and entries
identical to the most recent one. Cap the list at %history-max-entries
(oldest dropped) and reset the recall position to the newest boundary.
Return #t when the entry was added, #f when it was skipped."
  (let* ((raw (if (string? entry) entry ""))
         (trimmed (string-trim-both raw))
         (entries (history-entries hist)))
    (cond
     ((string-null? trimmed)
      (set-history-recall-position! hist #f)
      #f)
     ((and (pair? entries) (string=? (last entries) trimmed))
      (set-history-recall-position! hist #f)
      #f)
     (else
      (set-history-entries! hist
                            (history-cap-entries (append entries (list trimmed))))
      (set-history-recall-position! hist #f)
      #t))))

(define (history-recall hist delta)
  "Advance the recall position of HIST by DELTA entries (-1 = older,
+1 = newer) and return the entry at the new position. Return #f when the
position moves past the newest entry, meaning the caller should restore
the in-progress draft input. The position clamps at the oldest entry."
  (let* ((entries (history-entries hist))
         (len (length entries)))
    (if (= len 0)
        #f
        (let ((target (+ (or (history-recall-position hist) len) delta)))
          (cond
           ((>= target len)
            (set-history-recall-position! hist #f)
            #f)
           ((< target 0)
            (set-history-recall-position! hist 0)
            (car entries))
           (else
            (set-history-recall-position! hist target)
            (list-ref entries target)))))))

(define (history-reset-nav hist)
  "Reset the recall position of HIST to the newest boundary (draft state)."
  (set-history-recall-position! hist #f))

(define (history-entry->jsonl-line input mode ts)
  "Serialize a history record (INPUT string, MODE tag, TS unix seconds)
into a single JSON object line for the persistent history journal."
  (scm->json-string
   `(("input" . ,(if (string? input) input (format #f "~a" (or input ""))))
     ("mode" . ,(cond
                 ((string? mode) mode)
                 ((symbol? mode) (symbol->string mode))
                 (else "chat")))
     ("ts" . ,(if (number? ts) ts 0)))))

(define (jsonl-line->history-entry line)
  "Parse a single JSONL LINE into a history entry alist of the form
((\"input\" . STRING) (\"mode\" . STRING) (\"ts\" . NUMBER)). Return #f for
blank or malformed lines and for entries without a string input."
  (let ((parsed (safe-json-parse line)))
    (if (and (list? parsed) (string? (assoc-ref parsed "input")))
        `(("input" . ,(assoc-ref parsed "input"))
          ("mode" . ,(let ((m (assoc-ref parsed "mode")))
                       (cond
                        ((string? m) m)
                        ((symbol? m) (symbol->string m))
                        (else "chat"))))
          ("ts" . ,(let ((t (assoc-ref parsed "ts")))
                     (if (number? t) t 0))))
        #f)))

(define (default-history-file-path)
  "Return the default persistent input history path (.badb/history.jsonl
under the current workspace directory)."
  ".badb/history.jsonl")

(define (load-history-file path)
  "Load persisted input history from the JSONL file at PATH, returning the
list of input strings oldest-first, capped at %history-max-entries with
consecutive duplicates collapsed. An absent or unreadable file yields an
empty history."
  (catch #t
    (lambda ()
      (if (and (string? path) (file-exists? path))
          (let* ((raw (catch #t
                        (lambda () (call-with-input-file path read-string))
                        (lambda _ "")))
                 (entries (filter-map jsonl-line->history-entry
                                      (string-split raw #\newline))))
            (history-cap-entries
             (let loop ((rest (map (lambda (e) (assoc-ref e "input")) entries))
                        (acc '()))
               (cond
                ((null? rest) (reverse acc))
                ((and (pair? acc) (string=? (car acc) (car rest)))
                 (loop (cdr rest) acc))
                (else
                 (loop (cdr rest) (cons (car rest) acc)))))))
          '()))
    (lambda _ '())))

(define (append-history-file-entry! path input mode)
  "Append a single history record for INPUT and MODE to the JSONL file at
PATH, creating the enclosing directory when missing. Return #t on success
and #f on any filesystem failure."
  (catch #t
    (lambda ()
      (ensure-parent-directory! path)
      (let ((port (open-file path "a")))
        (display (history-entry->jsonl-line input mode
                                            (time-second (current-time)))
                 port)
        (newline port)
        (close-port port)
        #t))
    (lambda _ #f)))

;;;
;;; Chat Session Persistence (/resume)
;;;
(define %session-filename-max-length 40)

(define (default-sessions-dir)
  "Return the standard directory for persisted chat sessions
(~/.local/share/badb/sessions, honoring XDG_DATA_HOME)."
  (let ((xdg (getenv "XDG_DATA_HOME"))
        (home (getenv "HOME")))
    (cond
     ((and xdg (not (string-null? xdg)))
      (string-append xdg "/badb/sessions"))
     ((and home (not (string-null? home)))
      (string-append home "/.local/share/badb/sessions"))
     (else
      "/tmp/badb/sessions"))))

(define (session-file-path)
  "Return the JSONL path for this TUI process's session log: a
<timestamp>-<pid> name under (default-sessions-dir)."
  (string-append
   (default-sessions-dir) "/"
   (date->string (current-date 0) "~Y~m~dT~H~M~SZ")
   "-"
   (number->string (getpid))
   ".jsonl"))

(define (turn->jsonl-line msg)
  "Serialize one conversation turn MSG (an alist with role and content)
into a single JSON object line {\"role\":...,\"content\":...}."
  (scm->json-string
   `(("role" . ,(ensure-string (or (prop-ref msg 'role) "user")))
     ("content" . ,(ensure-string (or (prop-ref msg 'content) ""))))))

(define (persist-session-file! path history)
  "Write the whole conversation HISTORY to the session JSONL file at PATH,
one JSON turn object per line (the system prompt first, as-is), creating
the enclosing directory when missing. Return #t on success and #f on any
filesystem failure."
  (catch #t
    (lambda ()
      (ensure-parent-directory! path)
      (call-with-output-file path
        (lambda (port)
          (for-each
           (lambda (msg)
             (display (turn->jsonl-line msg) port)
             (newline port))
           history)))
      #t)
    (lambda _ #f)))

(define (load-session-file path)
  "Load the conversation turns persisted in the session JSONL file at PATH,
oldest first, skipping blank or malformed lines. An absent or unreadable
file yields the empty list."
  (catch #t
    (lambda ()
      (if (and (string? path) (file-exists? path))
          (let ((raw (catch #t
                       (lambda () (call-with-input-file path read-string))
                       (lambda _ ""))))
            (filter-map jsonl-line->turn (string-split raw #\newline)))
          '()))
    (lambda _ '())))

(define (recent-session-files dir)
  "Return up to 7 session JSONL file paths in DIR, most recently modified
first. A missing or unreadable directory yields the empty list."
  (catch #t
    (lambda ()
      (let* ((names
              (catch 'system-error
                (lambda ()
                  (let ((stream (opendir dir)))
                    (let loop ((accum '()))
                      (let ((entry (readdir stream)))
                        (if (eof-object? entry)
                            (begin (closedir stream) (reverse accum))
                            (loop (cons entry accum)))))))
                (lambda _ '())))
             (paths
              (filter-map
               (lambda (name)
                 (and (string-suffix? ".jsonl" name)
                      (let ((path (string-append dir "/" name)))
                        (and (file-exists? path) path))))
               names)))
        (take
         (sort paths
               (lambda (a b)
                 (> (stat:mtime (stat a))
                    (stat:mtime (stat b)))))
         (min 7 (length paths)))))
    (lambda _ '())))

(define (session-mtime-string path)
  "Format the modification time of PATH as a local timestamp for the
resume menu."
  (catch #t
    (lambda ()
      (date->string
       (time-utc->date (make-time time-utc 0 (stat:mtime (stat path))))
       "~Y-~m-~d ~H:~M"))
    (lambda _ "unknown")))

;;;
;;; Message Collapse State & Block Formatting
;;;
;; Message alists are immutable data: collapsing a message produces a new
;; alist carrying a 'collapsed? flag, it never mutates the original.
(define (replace-nth lst idx val)
  "Return a copy of LST with the element at position IDX replaced by VAL."
  (append (take lst idx) (list val) (drop lst (+ idx 1))))

(define (message-collapsed? msg)
  "Return #t when MSG carries a truthy 'collapsed? flag. An absent flag
means the message is expanded."
  (and (assoc-ref msg 'collapsed?) #t))

(define (message-with-collapsed msg collapsed?)
  "Return a new message alist with MSG's 'collapsed? flag set to COLLAPSED?.
A false value removes the flag entirely, so an expanded message carries no
'collapsed? key at all."
  (if collapsed?
      (prop-set msg 'collapsed? #t)
      (alist-remove 'collapsed? msg)))

(define (make-collapsed-message msg)
  "Return a new message alist with MSG's 'collapsed? flag set to #t."
  (message-with-collapsed msg #t))

(define (message-toggle-collapsed msg)
  "Return a new message alist with MSG's collapse state flipped."
  (message-with-collapsed msg (not (message-collapsed? msg))))

(define (default-collapsed-role? role)
  "Return #t when a card tagged with ROLE collapses by default: reasoning
('thought' / 'thinking') cards do, every other role stays expanded."
  (let ((r (cond
            ((string? role) role)
            ((symbol? role) (symbol->string role))
            (else (format #f "~a" (or role ""))))))
    (or (string=? r "thought") (string=? r "thinking"))))

(define (message-collapsible? msg)
  "Return #t when MSG has renderable content or thoughts and can therefore
be collapsed into a summary row."
  (or (non-empty-string? (assoc-ref msg 'content))
      (non-empty-string? (assoc-ref msg 'thoughts))))

(define (default-collapse-message msg)
  "Apply the default collapse policy to MSG: assistant reasoning collapses
to a summary row by default. An explicit 'collapsed? flag (set by the user)
is always preserved."
  (cond
   ((assoc 'collapsed? msg) msg)
   ((and (non-empty-string? (assoc-ref msg 'thoughts))
         (default-collapsed-role? "thought"))
    (make-collapsed-message msg))
   (else msg)))

(define %collapsed-preview-chars 40)

(define (thoughts-preview thoughts)
  "Return a single-line preview of THOUGHTS trimmed to roughly
%collapsed-preview-chars characters for a collapsed summary row."
  (let* ((first-line (car (string-split (string-trim-both thoughts) #\newline)))
         (clean (string-trim-both
                 (string-map (lambda (c) (if (char=? c #\tab) #\space c))
                             first-line))))
    (if (> (string-length clean) %collapsed-preview-chars)
        (substring clean 0 %collapsed-preview-chars)
        clean)))

(define (format-collapsed-summary-line label hidden-lines)
  "Compose the dim summary text for a collapsed block: the role LABEL, the
HIDDEN-LINES row count it hides, and the ctrl+e expand hint."
  (format #f "~a (~a line~a) — ctrl+e to expand"
          label hidden-lines (if (= hidden-lines 1) "" "s")))

;; Role label text shown on the header line of each flat message block.
(define (message-role-label role)
  "Return the lowercase role label text for a message ROLE string."
  (cond
   ((string=? role "user") "you")
   ((string=? role "assistant") "assistant")
   ((string=? role "thought") "thinking")
   ((string=? role "shell-cmd") "shell")
   ((string=? role "shell-out") "shell output")
   ((string=? role "scheme-in") "scheme")
   ((string=? role "scheme-out") "scheme output")
   ((or (string=? role "tool-call") (string=? role "tool")) "tool")
   ((string=? role "tool-result") "tool result")
   ((string=? role "error") "error")
   ((string=? role "notice") "notice")
   ((string=? role "steer") "steering")
   (else "system")))

(define (message-role->theme-role role)
  "Map a message ROLE string to its semantic theme color role symbol."
  (cond
   ((string=? role "user") 'user)
   ((string=? role "assistant") 'assistant)
   ((string=? role "thought") 'thought)
   ((string=? role "system") 'system)
   ((string=? role "notice") 'notice)
   ((string=? role "error") 'error)
   ((or (string=? role "shell-cmd") (string=? role "shell-out")) 'shell)
   ((or (string=? role "scheme-in") (string=? role "scheme-out")) 'scheme)
   ((or (string=? role "tool-call") (string=? role "tool")) 'tool-call)
   ((string=? role "tool-result") 'tool-result)
   ((string=? role "steer") 'accent)
   (else 'system)))

;; Content-derived label for system messages, replacing the old emoji badges.
(define (system-message-label content)
  "Pick the lowercase label text for a system message from CONTENT."
  (cond
   ((or (string-contains content "MCP Server")
        (string-prefix? "Active Model:" content))
    "mcp servers")
   ((string-contains content "Configuration") "configuration")
   ((string-contains content "Guardrail") "guardrails")
   ((string-contains content "Context Statistics") "context")
   ((string-contains content "Features & Architecture") "features")
   ((string-contains content "Help & Commands") "help")
   ((string-contains content "Test Suite") "tests")
   ((string-contains content "ReAct") "react")
   ((or (string-contains content "Journal")
        (string-contains content "Checkpoint")
        (string-contains content "checkpoint"))
    "journal")
   (else "system")))

;; Helper to wrap lines into a flat block descriptor
(define (make-block-entry label theme-role lines)
  "Turn wrapped LINES into flat block rows: one block-label header row
carrying LABEL and THEME-ROLE, then one block-body row per line. Callers
append the single blank spacer row between messages."
  (if (null? lines)
      '()
      (cons (list "block-label" label theme-role)
            (map (lambda (l) (list "block-body" theme-role l)) lines))))

(define (message-block-label role content)
  "Return the label text for the block header of a message with ROLE and
CONTENT: system messages derive their label from the content, every other
role from the role itself."
  (if (string=? role "system")
      (system-message-label content)
      (message-role-label role)))

;; Format a single message entry into flat block structure rows
(define (format-message-entry msg content-width)
  "Format MSG into flat block rows for a viewport CONTENT-WIDTH wide: a
role label line, body lines, and one trailing blank spacer row. Collapsed
messages (truthy 'collapsed? flag and collapsible content) render a single
block-summary line instead of the full block, except assistant messages
whose thoughts collapse to a summary line while their content block stays
visible."
  (let*
      ((role-val (assoc-ref msg 'role))
       (role
         (cond
           ((string? role-val) role-val)
           ((symbol? role-val) (symbol->string role-val))
           (else "system")))
       (raw-content (or (assoc-ref msg 'content) ""))
       (content
         (if (string? raw-content)
           raw-content
           (format #f "~a" raw-content)))
       (raw-thoughts (assoc-ref msg 'thoughts))
       (thoughts
         (and raw-thoughts
              (if (string? raw-thoughts)
                raw-thoughts
                (format #f "~a" raw-thoughts)))))
    (cond
      ((and (message-collapsed? msg) (message-collapsible? msg))
        (format-collapsed-message-rows msg role content thoughts content-width))
      ((string=? role "assistant")
        (let*
            ((th-block
               (if (non-empty-string? thoughts)
                 (make-block-entry "thinking"
                                   'thought
                                   (wrap-text (string-trim-both thoughts) content-width))
                 '()))
             (ct-block
               (if (non-empty-string? content)
                 (make-block-entry "assistant"
                                   'assistant
                                   (wrap-text content content-width))
                 '())))
          (append
            (if (and (null? th-block) (null? ct-block))
              (make-block-entry "assistant"
                                'assistant
                                (wrap-text
                                  (if (string-null? content)
                                    "(empty response)"
                                    content)
                                  content-width))
              (append th-block ct-block))
            (list (list "spacer" "")))))
      (else
        (let*
            ((theme-role (message-role->theme-role role))
             (label (message-block-label role content))
             (text (if (string=? role "user") (string-trim-both content) content)))
          (append
            (make-block-entry label theme-role (wrap-text text content-width))
            (list (list "spacer" ""))))))))

;; Rows for a collapsed message: one block-summary line (plus the visible
;; assistant content block when only the reasoning collapses).
(define (format-collapsed-message-rows msg role content thoughts content-width)
  "Return the block rows for collapsed MSG with pre-normalized ROLE, CONTENT
and THOUGHTS fields, followed by the trailing spacer row. The summary
reports the number of hidden block rows and the ctrl+e expand hint."
  (cond
    ((and (string=? role "assistant") (non-empty-string? thoughts))
      (let*
          ((th-block
             (make-block-entry "thinking"
                               'thought
                               (wrap-text (string-trim-both thoughts) content-width)))
           (hidden (max 0 (- (length th-block) 1)))
           (summary-row
             (list "block-summary" 'thought
                   (format-collapsed-summary-line "thinking" hidden)))
           (ct-block
             (if (non-empty-string? content)
               (make-block-entry "assistant"
                                 'assistant
                                 (wrap-text content content-width))
               '())))
        (append (list summary-row) ct-block (list (list "spacer" "")))))
    (else
      (let*
          ((theme-role (message-role->theme-role role))
           (label (message-block-label role content))
           (block (make-block-entry label theme-role (wrap-text content content-width)))
           (hidden (length block)))
        (if (null? block)
          (list (list "spacer" ""))
          (append
            (list (list "block-summary" theme-role
                        (format-collapsed-summary-line label hidden)))
            (list (list "spacer" ""))))))))

;;;
;;; Formatted-Row Cache (incremental rendering)
;;;
(define-record-type <row-cache>
                    (%make-row-cache table width)
                    row-cache?
                    (table row-cache-table set-row-cache-table!)
                    (width row-cache-width set-row-cache-width!))

(define (make-row-cache)
  "Create an empty formatted-row cache for the message renderer."
  (%make-row-cache (make-hash-table 63) #f))

(define (row-cache-rows cache msg content-width)
  "Return the formatted block rows of MSG at CONTENT-WIDTH, memoized in
CACHE per message object (identity-keyed): rendering an unchanged
transcript costs O(new messages) instead of re-wrapping the whole
history every frame. Streaming updates and collapse toggles replace
message objects, so they naturally miss and re-format; a CONTENT-WIDTH
change (terminal resize) clears the cache wholesale. Evicted messages
are dropped with ROW-CACHE-PRUNE!."
  (unless (eq? (row-cache-width cache) content-width)
    (set-row-cache-width! cache content-width)
    (hash-clear! (row-cache-table cache)))
  (or (hashq-ref (row-cache-table cache) msg)
      (let ((rows (format-message-entry msg content-width)))
        (hashq-set! (row-cache-table cache) msg rows)
        rows)))

(define (row-cache-prune! cache messages)
  "Drop cache entries whose message objects are no longer in MESSAGES.
The react streaming placeholder is replaced with a fresh object every
delta, so without pruning a long streaming run grows the cache
without bound; pruning keeps it proportional to the live transcript."
  (let ((live (make-hash-table 31)))
    (for-each (lambda (m) (hashq-set! live m #t)) messages)
    (let ((kept (make-hash-table 63)))
      (hash-for-each (lambda (k v)
                       (when (hashq-ref live k)
                         (hashq-set! kept k v)))
                     (row-cache-table cache))
      (set-row-cache-table! cache kept))))

;;;
;;; Interactive Menu Model
;;;
;; Pure data model for the interactive menu pop-ups (the /config, /model and
;; /endpoint flows). A menu is a plain record of a title, an item list, and
;; a selected index; items carry their actions as data — a symbol or a small
;; (tag . value) pair — so the model stays pure and testable without any
;; curses or handler wiring.
(define %menu-max-visible 12)

(define-record-type <menu>
  (%make-menu title items selected-index)
  menu?
  (title menu-title)
  (items menu-items)
  (selected-index menu-selected-index))

(define (normalize-menu-item item)
  "Coerce raw ITEM into canonical ((label . description) . action) form.
Accepts a head that is a (label . description) pair or a bare label, with
the action data carried in the tail."
  (let* ((head (if (pair? item) (car item) item))
         (label (if (pair? head) (car head) head))
         (desc (if (pair? head) (cdr head) #f))
         (action (if (pair? item) (cdr item) #f)))
    (cons (cons (ensure-string label) (ensure-string (or desc ""))) action)))

(define* (make-menu title items #:key (selected-index 0))
  "Create a menu titled TITLE from ITEMS, each of the form
((label . description) . action), with SELECTED-INDEX preselected. The
index is clamped into the valid range and the title coerced to a string."
  (let* ((norm-items (map normalize-menu-item (if (list? items) items '())))
         (count (length norm-items))
         (idx (min (max 0 selected-index) (max 0 (- count 1)))))
    (%make-menu (ensure-string (or title "Menu")) norm-items idx)))

(define (menu-count menu)
  "Return the number of items in MENU."
  (length (menu-items menu)))

(define (menu-move menu delta)
  "Return a new MENU with the selection moved by DELTA positions, wrapping
around at both ends. Empty menus are returned unchanged."
  (let ((count (menu-count menu)))
    (if (= count 0)
        menu
        (let ((m (remainder (+ (menu-selected-index menu) delta) count)))
          (%make-menu (menu-title menu)
                      (menu-items menu)
                      (if (< m 0) (+ m count) m))))))

(define (menu-selected-item menu)
  "Return the currently selected item of MENU, or #f when it is empty."
  (let ((count (menu-count menu)))
    (if (= count 0)
        #f
        (list-ref (menu-items menu)
                  (min (max 0 (menu-selected-index menu)) (- count 1))))))

(define (menu-visible-slice menu max-visible)
  "Return (values visible-items offset): the window of at most MAX-VISIBLE
items of MENU positioned so the selected item stays on screen. OFFSET is
the index of the first returned item within the full item list, for scroll
indicators. Menus that fit return every item with offset 0."
  (let* ((items (menu-items menu))
         (count (length items))
         (cap (max 1 max-visible))
         (sel (min (max 0 (menu-selected-index menu)) (max 0 (- count 1)))))
    (if (<= count cap)
        (values items 0)
        (let ((start (min (max 0 (- (+ sel 1) cap)) (- count cap))))
          (values (take (drop items start) cap) start)))))

(define (menu-item-label item)
  "Return the label string of menu ITEM."
  (if (and (pair? item) (pair? (car item))) (ensure-string (caar item)) ""))

(define (menu-item-description item)
  "Return the description string of menu ITEM."
  (if (and (pair? item) (pair? (car item))) (ensure-string (cdar item)) ""))

(define (menu-item-action item)
  "Return the action data carried by menu ITEM (a symbol, a small pair, or
other plain data); #f when the item carries none."
  (if (pair? item) (cdr item) #f))

;; Short descriptions for the built-in themes shown in the Theme menu.
(define %theme-descriptions
  '(("badb-dark" . "Cyan & green on black (default)")
    ("midnight" . "Deep blue night palette")
    ("nord" . "Nord-inspired cool blues")
    ("solarized-dark" . "Solarized dark warm tones")
    ("paper" . "Light paper-like scheme")
    ("mono" . "Monochrome, no colors")
    ("tokyo-night" . "Blue-heavy modern night theme")
    ("tokyo-night-storm" . "Tokyo Night, deeper and muted")
    ("tokyo-night-day" . "Tokyo Night light variant")
    ("catppuccin-mocha" . "Pastel dark standard")
    ("catppuccin-latte" . "Pastel light counterpart")
    ("gruvbox-dark" . "Warm retro groove")
    ("gruvbox-light" . "Daytime groove")
    ("dracula" . "The famous purple-dark theme")
    ("kanagawa" . "Japanese wave palette")
    ("rose-pine" . "Soft sunset pastels")
    ("everforest-dark" . "Calm forest greens")
    ("monokai" . "Classic editor dark")
    ("onedark" . "Atom editor classic")))

;; Guardrail policy levels with short descriptions for the Guardrail menu.
(define %guardrail-levels
  '((disabled . "No guardrail restrictions")
    (permissive . "Ask only on high-risk commands")
    (cautious . "Deny destructive, ask mutations (default)")
    (strict . "Deny dangerous, ask for everything")
    (paranoid . "Deny all state mutations")))

;;;
;;; Model Auto-Detection & Live Model Menu Helpers
;;;
(define (detect-model-decision cur-model detected-model error)
  "Pure adopt/detect decision policy for startup model detection.
Return (values new-model notice-kind):
  'skipped      CUR-MODEL is a specific model: keep it, skip detection.
  'adopted      DETECTED-MODEL becomes the new model.
  'failed       detection errored (ERROR describes the failure).
  'unavailable  detection succeeded but found no model.
NEW-MODEL is #f for the failed and unavailable outcomes."
  (cond
   ((not (generic-model-name? cur-model)) (values cur-model 'skipped))
   (error (values #f 'failed))
   (detected-model (values detected-model 'adopted))
   (else (values #f 'unavailable))))

(define (build-model-menu-items model-ids active-model)
  "Pure construction of live model menu items: one item per MODEL-ID in
order, each carrying a (set-model . id) action, with ACTIVE-MODEL marked in
its description, plus the trailing 'Set model...' manual entry item."
  (append
   (map
    (lambda (id)
      (cons (cons id
                  (if (and (string? active-model)
                           (string=? id active-model))
                      "active"
                      "available"))
            (cons 'set-model id)))
    (if (list? model-ids) model-ids '()))
   (list (cons (cons "Set model..." "Type a model name manually")
               'prefill-model))))

;;;
;;; Main TUI Controller
;;;
(define (format-conversation-transcript messages)
        "Render MESSAGES (the TUI conversation alist with role/content/thoughts
entries) as a plain-text transcript: one block per message with a role
header, an optional thinking section, and the content. Pure - no I/O."
        (let
            ((blocks
               (map
                 (lambda (msg)
                   (let*
                       ((role
                          (let ((r (assoc-ref msg 'role)))
                            (cond
                              ((string? r) r)
                              ((symbol? r) (symbol->string r))
                              (else "system"))))
                        (content
                          (let ((c (assoc-ref msg 'content)))
                            (cond
                              ((string? c) c)
                              ((not c) "")
                              (else (format #f "~a" c)))))
                        (thoughts
                          (let ((t (assoc-ref msg 'thoughts))) (and (string? t) (not (string-null? t)) t))))
                     (string-append "=== "
                                    (string-upcase role)
                                    " ===\n"
                                    (if thoughts
                                      (string-append "[thinking]\n" thoughts "\n\n")
                                      "")
                                    (if (string-null? content)
                                      ""
                                      (string-append content "\n"))
                                    "\n")))
                 messages)))
          (string-append ";; badb conversation transcript\n" (string-join blocks ""))))

;;;
;;; Kernel-Owned Spec Gate Tools (roadmap Phase 2/3 dogfood)
;;;
(define (spec-tool-format-failure failure)
        "Render one parsed spec failure record (a string-keyed alist from
the diagnostics JSON) as a single diagnostic line."
        (format #f
                "- ~a [kind: ~a] seed: ~a inputs: ~s exception: ~a\n"
                (or (assoc-ref failure "name") "?")
                (or (assoc-ref failure "kind") "?")
                (or (assoc-ref failure "seed") "?")
                (or (assoc-ref failure "inputs") '?)
                (or (assoc-ref failure "exception") "?")))

(define (spec-tool-lock spec-path feature-id)
        "Kernel tool 'spec.lock': lock the authored spec at SPEC-PATH for
FEATURE-ID into the kernel-owned locked spec store and return the
locked copy path plus its SHA-256 spec-hash. Locking is how a
contract becomes binding: the kernel re-reads the LOCKED copy at
commit time, never the agent's working copy. The gate itself
constrains the copy to (spec-gate-root)/specs/."
        (catch #t
          (lambda ()
            (cond
              ((not
                 (and (string? spec-path)
                      (not (string-null? (string-trim-both spec-path)))
                      (string? feature-id)
                      (not (string-null? (string-trim-both feature-id)))))
                "Error: spec.lock requires non-empty string arguments spec-path and feature-id")
              ((not (file-exists? spec-path))
                (format #f "Error: spec file '~a' does not exist" spec-path))
              ((not
                 (catch #t
                   (lambda ()
                     (call-with-input-file spec-path
                                           (lambda (_port)
                                             #t)))
                   (lambda _
                     #f)))
                (format #f "Error: spec file '~a' is not readable" spec-path))
              (else
                (let*
                    ((hash (lock-spec spec-path feature-id)) (locked (locked-spec-path feature-id)))
                  (format #f
                          "Locked spec for feature '~a'.\nLocked copy: ~a\nSHA-256 spec-hash: ~a\nThe kernel re-reads the LOCKED copy at commit time; locking is how this contract became binding."
                          feature-id
                          locked
                          hash)))))
          (lambda (key . args)
            (format #f "Error locking spec (~a): ~s" key args))))

;; DEV-DEBT [ux-gap]: verifies only the locked FLAT spec (.badb/specs/<fid>.scm);
;; 'badb geis verify <bundle-fid>' never falls back to verify-locked-geis-bundle and
;; can replay a stale flat lock. Use 'geis verify --all' for bundle contracts.
(define (spec-tool-verify feature-id)
        "Kernel tool 'spec.verify': replay the LOCKED spec copy for
FEATURE-ID in a disposable kernel subprocess and return the verdict,
spec-hash, summary counts, and failure diagnostics. The verdict comes
only from kernel subprocess replay of the locked copy, never from
agent self-report, and the gate fails closed on any spawn, timeout,
or parse failure."
        (if (and (string? feature-id) (not (string-null? (string-trim-both feature-id))))
          (let*
              ((result (verify-locked-spec feature-id))
               (verdict (ensure-string (assoc-ref result 'verdict)))
               (failures (assoc-ref result 'failures)))
            (apply string-append
                   (append
                     (list
                       (format #f
                               "Kernel replay verdict: ~a for feature '~a'.\n"
                               (string-upcase verdict)
                               feature-id)
                       (format #f "spec-hash: ~a\n" (or (assoc-ref result 'spec-hash) "none"))
                       (format #f
                               "summary: total=~a passed=~a failed=~a\n"
                               (or (assoc-ref result 'summary-total) "?")
                               (or (assoc-ref result 'summary-passed) "?")
                               (or (assoc-ref result 'summary-failed) "?"))
                       (format #f "seed: ~a\n" (ensure-string (assoc-ref result 'seed)))
                       (format #f
                               "exit-status: ~a signal: ~a timed-out: ~a\n"
                               (or (assoc-ref result 'exit-status) "none")
                               (or (assoc-ref result 'signal) "none")
                               (assoc-ref result 'timed-out?))
                       (format #f "reason: ~a\n" (ensure-string (assoc-ref result 'reason)))
                       "Verdicts come only from kernel subprocess replay of the LOCKED copy; the kernel re-reads it at commit time.\n")
                     (if (or (not failures) (null? failures))
                       (list "failures: none\n")
                       (cons "failures:\n" (map spec-tool-format-failure failures))))))
          "Error: spec.verify requires a non-empty string feature-id"))

(define (spec-tool-list-locked-specs)
        "Return the sorted locked spec file names in the kernel locked-spec
store directory, or the empty list when the store does not exist yet."
        (let ((dir (spec-gate-dir)))
          (if (file-exists? dir)
            (let ((d (opendir dir)))
              (let loop
                ((names '()))
                (let
                    ((entry
                       (catch #t
                         (lambda ()
                           (readdir d))
                         (lambda _
                           #f))))
                  (cond
                    ((or (not entry) (eof-object? entry)) (closedir d) (sort names string<?))
                    ((string-suffix? ".scm" entry) (loop (cons entry names)))
                    (else (loop names))))))
            '())))

(define (spec-tool-status . maybe-feature-id)
        "Kernel tool 'spec.status': list the locked specs in the kernel
locked-spec store with their recomputed SHA-256 hashes and, when a
FEATURE-ID is supplied, report whether that feature has a locked
spec. The kernel re-reads the LOCKED copies at commit time; locking
is how a contract becomes binding."
        (let*
            ((raw-fid (and (pair? maybe-feature-id) (car maybe-feature-id)))
             (feature-id
               (and (string? raw-fid) (not (string-null? (string-trim-both raw-fid))) raw-fid))
             (dir (spec-gate-dir))
             (names (spec-tool-list-locked-specs)))
          (apply string-append
                 (append
                   (list (format #f "Locked spec store: ~a\n" dir)
                         (if (null? names)
                           "No locked specs yet.\n"
                           (format #f
                                   "~a locked spec~a:\n"
                                   (length names)
                                   (if (= (length names) 1)
                                     ""
                                     "s"))))
                   (map
                     (lambda (name)
                       (let*
                           ((path (string-append dir "/" name))
                            (hash
                              (catch #t
                                (lambda ()
                                  (sha256-hex (call-with-input-file path read-string)))
                                (lambda _
                                  "unreadable"))))
                         (format #f
                                 "  ~a  ~a  (~a)\n"
                                 (substring name 0 (- (string-length name) 4))
                                 hash
                                 path)))
                     names)
                   (if feature-id
                     (let ((locked (locked-spec-path feature-id)))
                       (list
                         (if (file-exists? locked)
                           (format #f "Locked spec for feature '~a': present at ~a\n" feature-id locked)
                           (format #f
                                   "Locked spec for feature '~a': none (no locked copy at ~a)\n"
                                   feature-id
                                   locked))))
                     '())))))

(define (register-spec-tools! reg)
        "Attach the kernel-owned spec gate tools to the kernel feature
registry REG: spec.lock, spec.verify, and spec.status. The
descriptions tell the agent that the kernel re-reads the LOCKED copy
at commit time, that locking is how a contract becomes binding, and
that verdicts come only from kernel subprocess replay."
        (registry-attach! reg
                          "spec.lock"
                          (lambda (spec-path feature-id)
                            (spec-tool-lock spec-path feature-id))
                          '((description .
                             "Lock the authored spec at spec-path for feature-id into the kernel-owned locked spec store; returns the locked copy path and its SHA-256 spec-hash. Locking is how a contract becomes binding: the kernel re-reads the LOCKED copy at commit time, never the agent's working copy.")
                            (parameters .
                             ((type . "object")
                              (properties .
                               ((spec-path .
                                 ((type . "string") (description . "Path to the authored spec file to lock")))
                                (feature-id .
                                 ((type . "string") (description . "Feature identifier the spec gates")))))
                              (required . #("spec-path" "feature-id"))))))
        (registry-attach! reg
                          "spec.verify"
                          (lambda (feature-id)
                            (spec-tool-verify feature-id))
                          '((description .
                             "Ask the kernel to replay the LOCKED spec copy for feature-id in a disposable subprocess; returns the verdict, spec-hash, summary counts, and failure diagnostics. Verdicts come only from kernel subprocess replay of the locked copy, never from agent self-report; the gate fails closed.")
                            (parameters .
                             ((type . "object")
                              (properties .
                               ((feature-id .
                                 ((type . "string")
                                  (description . "Feature identifier whose locked spec to replay")))))
                              (required . #("feature-id"))))))
        (registry-attach! reg
                          "spec.status"
                          (lambda args
                            (apply spec-tool-status args))
                          '((description .
                             "List the locked specs in the kernel locked-spec store with recomputed SHA-256 hashes; with optional feature-id, report whether that feature has a locked spec. The kernel re-reads the LOCKED copies at commit time.")
                            (parameters .
                             ((type . "object")
                              (properties .
                               ((feature-id .
                                 ((type . "string")
                                  (description . "Optional feature identifier to check for a locked spec")))))
                              (required . #())))))
        (registry-attach! reg
                          "spec.synthesize"
                          (lambda* (feature-id #:optional target-path spec-path)
                            (catch #t
                              (lambda ()
                                (let* ((synth-mod (or (false-if-exception (resolve-module '(geasa runtimes-synthesis plugin)))
                                                       (resolve-module '(geasa runtimes-synthesis plugin))))
                                       (resolve-role (module-ref synth-mod 'resolve-synthesis-role))
                                       (synth-fn (module-ref synth-mod 'run-synthesis-workflow))
                                       ;; The synthesis agent role (model + persona
                                       ;; from the persona geasa) resolved from the
                                       ;; project config at call time.
                                       (role (catch #t
                                               (lambda () (resolve-role (load-config)))
                                               (lambda _ #f)))
                                       (res (synth-fn feature-id
                                                      #:target-path target-path
                                                      #:spec-path spec-path
                                                      #:model (and role (assoc-ref role 'model))
                                                      #:persona (and role (assoc-ref role 'persona-text))
                                                      #:env (if role
                                                                ((module-ref synth-mod 'synthesis-env-alist) role)
                                                                '())
                                                      #:llm-endpoint
                                                      (if role
                                                          (assoc-ref
                                                           ((module-ref synth-mod 'synthesis-llm-args) role)
                                                           'llm-endpoint)
                                                          #f)
                                                      #:llm-api-key
                                                      (if role
                                                          (assoc-ref
                                                           ((module-ref synth-mod 'synthesis-llm-args) role)
                                                           'llm-api-key)
                                                          #f)))
                                       (status (assoc-ref res 'status))
                                       (wf-result (assoc-ref res 'result)))
                                  (let* ((target (or target-path
                                                      ((module-ref synth-mod 'default-synthesis-target) feature-id)))
                                         (install-report
                                           (if (string=? (or status "") "success")
                                               ((module-ref synth-mod 'install-synthesized-bundle!)
                                                feature-id target)
                                               #f)))
                                    (format #f "Sema plugin synthesis ~a for feature '~a'.\n~a~a"
                                            status
                                            feature-id
                                            (if wf-result
                                                (scm->json-string wf-result)
                                                (or (assoc-ref res 'stdout) ""))
                                            (if install-report
                                                (format #f "\nInstalled geis for project: ~a verdict: ~a"
                                                        (or (assoc-ref install-report 'feature-id) feature-id)
                                                        (or (assoc-ref install-report 'verify-verdict)
                                                            (assoc-ref install-report 'install-status)
                                                            "?"))
                                                "")))))
                              (lambda (k . a)
                                (format #f "Error during synthesis (~a): ~s" k a))))
                          '((description .
                             "Run the Sema plugin synthesis workflow for feature-id against its locked contract specification; iteratively synthesizes the Guile module from counterexamples and triggers kernel commit + hot-attach.")
                            (parameters .
                             ((type . "object")
                              (properties .
                               ((feature-id .
                                 ((type . "string")
                                  (description . "Feature identifier to synthesize")))
                                (target-path .
                                 ((type . "string")
                                  (description . "Optional path to target Guile module implementation file")))
                                (spec-path .
                                 ((type . "string")
                                  (description . "Optional path to contract specification file")))))
                              (required . #("feature-id"))))))
        reg)

(define (build-geis-add-prompt desc)
  "Construct a structured prompt instructing the agent to author a contract
specification, lock it via spec.lock, synthesize it via spec.synthesize, and
verify hot-attachment."
  (format #f
"Please implement and dynamically attach a new feature to the badb harness: ~a

Follow the contract-gated dynamic synthesis workflow:
1. Design and write a declarative contract specification in Scheme (SRFI-64) defining the feature requirements, exported procedures, invariants, and test cases. Save this to a file under 'specs/' (e.g. 'specs/<feature-id>.spec.scm').
2. Lock the specification into the kernel-owned spec store using the 'spec.lock' tool with feature-id and spec-path. (Locking makes the contract binding; the kernel re-reads the locked copy at commit time).
3. Synthesize the Guile module implementation using the 'spec.synthesize' tool for your feature-id (which iteratively runs the Sema plugin-synthesis workflow with counterexample feedback until all contracts pass, then commits and hot-attaches the module).
4. Verify the newly attached feature is listed in the kernel registry (e.g. via 'spec.status' or kernel inspection) and summarize what capabilities were attached."
          desc))

;;;
;;; TUI Kernel Tool Registration, Menu Builders & Subcommand Formatters
;;;
(define (exec-shell-tool-impl cmd-or-arg)
  "Execute a shell command passed as a string or tool-call argument alist."
  (let ((cmd (cond
              ((string? cmd-or-arg) cmd-or-arg)
              ((and (pair? cmd-or-arg) (pair? (car cmd-or-arg)))
               (or (assoc-ref cmd-or-arg "command")
                   (assoc-ref cmd-or-arg 'command)
                   (assoc-ref cmd-or-arg "cmd")
                   (assoc-ref cmd-or-arg 'cmd)
                   (cdar cmd-or-arg)))
              (else (object->string cmd-or-arg)))))
    (call-with-values
      (lambda () (execute-shell-command cmd))
      (lambda (out exit-val err)
        (if err
            (format #f "Error: ~a" err)
            (format #f "[exit ~a]\n~a" exit-val out))))))

(define (register-builtin-tui-tools! reg rollback-mgr verifier mcp-mgr get-llm-history)
  "Register built-in autonomous agent tools, MCP kernel tools, and spec gate
tools on REG."
  (registry-attach! reg
                    "read_file"
                    (lambda (path)
                      (catch #t
                        (lambda ()
                          (if (file-exists? path)
                              (call-with-input-file path read-string)
                              (format #f "Error: file '~a' does not exist" path)))
                        (lambda (_k . a)
                          (format #f "Error reading file: ~s" a))))
                    '((description . "Read the complete contents of a file at PATH")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((path . ((type . "string") (description . "Path to the file to read")))))
                        (required . #("path"))))))
  (registry-attach! reg
                    "write_file"
                    (lambda (path content)
                      (catch #t
                        (lambda ()
                          (create-checkpoint! rollback-mgr
                                              (format #f "pre-write: ~a" path)
                                              #:files (list path))
                          (let ((v-res (verify-action verifier #:code content #:file path)))
                            (if (assoc-ref v-res 'passed?)
                                (begin
                                  (call-with-output-file path
                                    (lambda (p) (display content p)))
                                  (format #f
                                          "Successfully wrote ~a characters to '~a'"
                                          (string-length content)
                                          path))
                                (begin
                                  (rollback-latest! rollback-mgr)
                                  (format #f "Write aborted: verification failed: ~s"
                                          (assoc-ref v-res 'failures))))))
                        (lambda (_k . a)
                          (format #f "Error writing file: ~s" a))))
                    '((description . "Write CONTENT to a file at PATH")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((path . ((type . "string") (description . "File path to write")))
                          (content . ((type . "string") (description . "Content to write")))))
                        (required . #("path" "content"))))))
  (registry-attach! reg
                    "execute_command"
                    exec-shell-tool-impl
                    '((description . "Execute a shell command and return output and exit status")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((cmd . ((type . "string") (description . "Shell command to run")))))
                        (required . #("cmd"))))))
  (registry-attach! reg
                    "bash"
                    exec-shell-tool-impl
                    '((description . "Execute a bash shell command and return stdout/stderr and exit code")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((command . ((type . "string") (description . "Bash command line")))))
                        (required . #("command"))))))
  (registry-attach! reg
                    "sh"
                    exec-shell-tool-impl
                    '((description . "Execute a POSIX shell command and return output and exit code")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((command . ((type . "string") (description . "Shell command line")))))
                        (required . #("command"))))))
  (registry-attach! reg
                    "checkpoint_create"
                    (lambda (description)
                      (let ((c (create-checkpoint! rollback-mgr (or description "agent-checkpoint"))))
                        (format #f "Created checkpoint: ~a (~a) covering ~a workspace file(s) and the registry. Files outside the workspace directory are NOT captured."
                                (checkpoint-id c)
                                (checkpoint-description c)
                                (length (checkpoint-files c)))))
                    '((description . "Create a rollback checkpoint for current workspace files and registry. Only files under the workspace directory are captured; paths outside it are silently left out of the checkpoint.")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((description . ((type . "string") (description . "Checkpoint description")))))))))
  (registry-attach! reg
                    "checkpoint_rollback"
                    (lambda* (#:optional id)
                      (let ((res (if (and id (not (string-null? id)))
                                     (rollback-to-checkpoint! rollback-mgr id)
                                     (rollback-latest! rollback-mgr))))
                        (format #f "Rollback: ~a (restored ~a file(s), removed ~a file(s)). Only files captured by the checkpoint are restored; files outside the workspace were never captured."
                                (assoc-ref res 'status)
                                (length (or (assoc-ref res 'restored-files) '()))
                                (length (or (assoc-ref res 'removed-files) '())))))
                    '((description . "Rollback to specified or latest checkpoint. Only files captured by the checkpoint (workspace files at creation time) are restored.")
                      (parameters .
                       ((type . "object")
                        (properties . ((id . ((type . "string") (description . "Checkpoint ID")))))))))
  (registry-attach! reg
                    "run_tests"
                    (lambda* (#:optional (cmd "./scripts/test"))
                      (let ((res (verify-test-suite #:command (or cmd "./scripts/test"))))
                        (format #f "Tests: ~a (exit ~a)\n~a"
                                (if (assoc-ref res 'passed?) "PASS" "FAIL")
                                (assoc-ref res 'exit-code)
                                (assoc-ref res 'output))))
                    '((description . "Run project test suite and return verdict")
                      (parameters .
                       ((type . "object")
                        (properties . ((cmd . ((type . "string") (description . "Test command")))))))))
  (registry-attach! reg
                    "history/summarize"
                    (lambda ()
                      (let ((summary (summarize-turns (get-llm-history)))
                            (hist-file (getenv "BADB_HISTORY_FILE")))
                        (if (and hist-file (not (string-null? hist-file)))
                            (let ((file-summary (summarize-history-file hist-file)))
                              (if file-summary
                                  (string-append summary "\n\n--- " hist-file " ---\n" file-summary)
                                  summary))
                            summary)))
                    '((description . "Summarize the current conversation: turn counts, token estimate, recent topics")
                      (parameters . ((type . "object") (properties . ()) (required . #())))))
  ;; Register MCP Kernel Features
  (registry-attach! reg
                    "mcp.list"
                    (lambda () (mcp-manager-list-servers mcp-mgr))
                    '((description . "List all configured MCP servers and their status")))
  (registry-attach! reg
                    "mcp.start"
                    (lambda (name) (mcp-manager-start-server! mcp-mgr name))
                    '((description . "Start a configured MCP server by name")))
  (registry-attach! reg
                    "mcp.stop"
                    (lambda (name) (mcp-manager-stop-server! mcp-mgr name))
                    '((description . "Stop a running MCP server by name")))
  (registry-attach! reg
                    "mcp.add"
                    (lambda (name cmd args)
                      (mcp-manager-add-server! mcp-mgr name cmd args #:auto-start #t))
                    '((description . "Add a new MCP server specification and persist to config")))
  (registry-attach! reg
                    "mcp.remove"
                    (lambda (name) (mcp-manager-remove-server! mcp-mgr name))
                    '((description . "Remove an MCP server from configuration and stop it")))
  (registry-attach! reg
                    "mcp.tools"
                    (lambda* (#:optional server-name) (mcp-manager-list-tools mcp-mgr server-name))
                    '((description . "List available tools discovered from running MCP servers")))
  (registry-attach! reg
                    "mcp.call"
                    (lambda (name-or-server tool-or-args . maybe-args)
                      (if (pair? maybe-args)
                          (mcp-manager-call-tool mcp-mgr name-or-server tool-or-args (car maybe-args))
                          (mcp-manager-call-tool mcp-mgr name-or-server tool-or-args)))
                    '((description . "Call a tool on an MCP server")))
  ;; Register kernel-owned spec gate tools (Phase 2 dogfood)
  (register-spec-tools! reg))

(define (auto-start-mcp-servers! cfg mcp-mgr)
  "Auto-start configured MCP servers that specify auto-start: #t."
  (catch #t
    (lambda ()
      (for-each
        (lambda (entry)
          (let* ((name (if (pair? entry)
                           (if (pair? (car entry))
                               (assoc-ref entry 'name)
                               (car entry))
                           #f))
                 (spec (if (pair? entry)
                           (if (pair? (car entry))
                               entry
                               (cdr entry))
                           '()))
                 (auto? (or (assoc-ref spec 'auto-start) (assoc-ref spec 'auto-start?))))
            (when (and name auto?)
              (catch #t
                (lambda () (mcp-manager-start-server! mcp-mgr name))
                (lambda _ #f)))))
        (badb-config-mcp-servers cfg)))
    (lambda _ #f)))

;; DEV-DEBT [complexity]: cyclomatic ~41 / nesting 18 (ast_complexity_metrics).
(define (execute-mcp-subcommand! mcp-mgr arg add-sys-msg! add-err-msg!)
  "Dispatch /mcp subcommands (list, start, stop, restart, add, remove, tools)."
  (let* ((tokens (split-words arg))
         (subcmd (if (null? tokens) "list" (car tokens)))
         (rest (if (null? tokens) '() (cdr tokens))))
    (cond
     ((or (string=? subcmd "list") (string=? subcmd "ls") (string-null? arg))
      (let ((servers (mcp-manager-list-servers mcp-mgr)))
        (if (null? servers)
            (add-sys-msg!
             "No MCP servers configured.\nUse /mcp add <name> <cmd> [args...] to register one.")
            (let ((desc (map (lambda (s)
                               (let* ((name (assoc-ref s 'name))
                                      (cmd (assoc-ref s 'command))
                                      (s-args (or (assoc-ref s 'args) '()))
                                      (running? (assoc-ref s 'running?))
                                      (pid (assoc-ref s 'pid))
                                      (tools (assoc-ref s 'tools))
                                      (t-count (if (list? tools) (length tools) 0))
                                      (auto? (assoc-ref s 'auto-start))
                                      (auto-str (if auto? " (auto-start)" ""))
                                      (status (if running?
                                                  (format #f "[RUNNING pid:~a tools:~a]" pid t-count)
                                                  "[STOPPED]")))
                                 (format #f "• ~a ~a~a\n  command: ~a ~a"
                                         name status auto-str cmd (string-join s-args " "))))
                             servers)))
              (add-sys-msg! (string-append "Configured MCP Servers:\n" (string-join desc "\n")))))))
     ((string=? subcmd "start")
      (let ((name (and (pair? rest) (car rest))))
        (if (or (not name) (string=? name "all"))
            (let* ((servers (mcp-manager-list-servers mcp-mgr))
                   (results (map (lambda (s)
                                   (let ((s-name (assoc-ref s 'name))
                                         (running? (assoc-ref s 'running?)))
                                     (if running?
                                         (format #f "• ~a: already running" s-name)
                                         (call-with-values
                                           (lambda () (mcp-manager-start-server! mcp-mgr s-name))
                                           (lambda (_client tools err)
                                             (if err
                                                 (format #f "• ~a: error (~a)" s-name err)
                                                 (format #f "• ~a: started (~a tools)" s-name (length tools))))))))
                                 servers)))
              (add-sys-msg! (string-append "Starting MCP servers:\n" (string-join results "\n"))))
            (call-with-values
              (lambda () (mcp-manager-start-server! mcp-mgr name))
              (lambda (_client tools err)
                (if err
                    (add-err-msg! err)
                    (add-sys-msg!
                     (format #f "Started MCP server '~a' (~a tools available)." name (length tools)))))))))
     ((string=? subcmd "stop")
      (let ((name (and (pair? rest) (car rest))))
        (if (or (not name) (string=? name "all"))
            (let ((servers (mcp-manager-list-servers mcp-mgr)))
              (mcp-manager-stop-all! mcp-mgr)
              (add-sys-msg! (format #f "Stopped all MCP servers (~a total)." (length servers))))
            (if (mcp-manager-stop-server! mcp-mgr name)
                (add-sys-msg! (format #f "Stopped MCP server '~a'." name))
                (add-err-msg! (format #f "MCP server '~a' was not running." name))))))
     ((string=? subcmd "restart")
      (let ((name (and (pair? rest) (car rest))))
        (if (or (not name) (string=? name "all"))
            (begin
              (mcp-manager-stop-all! mcp-mgr)
              (let* ((servers (mcp-manager-list-servers mcp-mgr))
                     (results (map (lambda (s)
                                     (let ((s-name (assoc-ref s 'name)))
                                       (call-with-values
                                         (lambda () (mcp-manager-start-server! mcp-mgr s-name))
                                         (lambda (_client tools err)
                                           (if err
                                               (format #f "• ~a: error (~a)" s-name err)
                                               (format #f "• ~a: restarted (~a tools)" s-name (length tools)))))))
                                   servers)))
                (add-sys-msg! (string-append "Restarted MCP servers:\n" (string-join results "\n")))))
            (begin
              (mcp-manager-stop-server! mcp-mgr name)
              (call-with-values
                (lambda () (mcp-manager-start-server! mcp-mgr name))
                (lambda (_client tools err)
                  (if err
                      (add-err-msg! (format #f "Failed to restart '~a': ~a" name err))
                      (add-sys-msg!
                       (format #f "Restarted MCP server '~a' (~a tools available)."
                               name (length tools))))))))))
     ((string=? subcmd "add")
      (if (< (length rest) 2)
          (add-err-msg! "Usage: /mcp add <name> <command> [args...]")
          (let ((name (car rest)) (cmd (cadr rest)) (cmd-args (cddr rest)))
            (mcp-manager-add-server! mcp-mgr name cmd cmd-args #:auto-start #t)
            (add-sys-msg!
             (format #f "Added MCP server '~a' (~a ~a).\nSaved to ~a."
                     name cmd (string-join cmd-args " ") (default-local-config-path))))))
     ((or (string=? subcmd "remove") (string=? subcmd "rm"))
      (if (null? rest)
          (add-err-msg! "Usage: /mcp remove <name>")
          (let ((name (car rest)))
            (if (mcp-manager-remove-server! mcp-mgr name)
                (add-sys-msg! (format #f "Removed MCP server '~a'. Configuration updated." name))
                (add-err-msg! (format #f "MCP server '~a' not found." name))))))
     ((string=? subcmd "tools")
      (let* ((target-server (and (pair? rest) (car rest)))
             (tools (mcp-manager-list-tools mcp-mgr target-server)))
        (if (null? tools)
            (add-sys-msg!
             (if target-server
                 (format #f "No tools available from server '~a' (is it running?)." target-server)
                 "No MCP tools available. Make sure servers are running with /mcp start <name>."))
            (let ((desc (map (lambda (t)
                               (format #f "• ~a (~a): ~a"
                                       (assoc-ref t 'name)
                                       (assoc-ref t 'server)
                                       (or (assoc-ref t 'description) "no description")))
                             tools)))
              (add-sys-msg! (string-append "Discovered MCP Tools:\n" (string-join desc "\n")))))))
     (else
      (add-err-msg!
       (format #f "Unknown /mcp subcommand '~a'. Valid: list, start, stop, restart, add, remove, tools"
               subcmd))))))

(define (config-recorded-models cfg)
  "Collect the model names recorded in CFG: the models of every provider
preset, deduplicated in declaration order."
  (delete-duplicates
   (append-map
    (lambda (p)
      (let ((raw (get-prop (cdr p) 'models #f)))
        (cond
         ((string? raw) (list raw))
         ((list? raw)
          (filter-map
           (lambda (m)
             (cond
              ((string? m) m)
              ((and (pair? m) (string? (car m))) (car m))
              ((and (pair? m) (string? (cdr m))) (cdr m))
              (else #f)))
           raw))
         (else '()))))
    (badb-config-providers cfg))
   equal?))

(define (fetch-live-models client)
  "Query CLIENT's /models listing. Returns (values ids error): IDS is the
non-empty list of available model id strings or #f, ERROR the failure
description or #f. Never throws."
  (catch #t
    (lambda ()
      (call-with-values
        (lambda () (list-models client))
        (lambda (ids err)
          (if (or err (null? ids))
              (values #f (or err "no models listed on endpoint"))
              (values ids #f)))))
    (lambda (key . args)
      (values #f (format #f "model list request failed (~a): ~s" key args)))))

(define (build-live-or-recorded-model-menu-items client cfg cur-model)
  "Build the model menu items: live models from CLIENT when available,
otherwise CUR-MODEL plus models recorded in CFG provider presets, plus
the trailing manual entry item."
  (call-with-values
    (lambda () (fetch-live-models client))
    (lambda (live-ids _live-err)
      (if live-ids
          (build-model-menu-items live-ids cur-model)
          (let ((known (delete-duplicates (cons cur-model (config-recorded-models cfg)) equal?)))
            (append
             (map (lambda (m)
                    (cons (cons m (if (string=? m cur-model)
                                      "current model"
                                      "recorded in config"))
                          (cons 'set-model m)))
                  known)
             (list (cons (cons "Set model..." "Type a model name manually") 'prefill-model))))))))

(define (endpoint-menu-items-for-config cfg)
  "Build the endpoint menu items: configured provider presets in CFG plus
a manual entry item."
  (append
   (map (lambda (p)
          (let ((name (ensure-string (car p))))
            (cons (cons name (or (get-prop (cdr p) 'endpoint #f) "provider preset"))
                  (cons 'switch-provider name))))
        (badb-config-providers cfg))
   (list (cons (cons "Set endpoint..." "Type an endpoint URL manually")
               'prefill-endpoint))))

(define (theme-menu-items)
  "Build the theme menu items from the built-in theme list."
  (map (lambda (name)
         (cons (cons name (or (assoc-ref %theme-descriptions name) "Built-in color theme"))
               (cons 'set-theme name)))
       (all-theme-names)))

(define (guardrail-menu-items)
  "Build the guardrail level menu items."
  (map (lambda (entry)
         (cons (cons (symbol->string (car entry)) (cdr entry))
               (cons 'set-guardrail (car entry))))
       %guardrail-levels))

(define (config-menu-items-for-state cfg cur-model cur-endpoint policy)
  "Build the top-level config menu items with live state woven into descriptions."
  (list
   (cons (cons "Model" (format #f "Switch active model (~a)" cur-model))
         'open-model-menu)
   (cons (cons "Endpoint" (format #f "Switch endpoint (~a)" cur-endpoint))
         'open-endpoint-menu)
   (cons (cons "Temperature"
               (format #f "Set sampling temperature (~a)" (badb-config-model-temperature cfg)))
         'prefill-temperature)
   (cons (cons "Theme"
               (format #f "Choose UI color theme (~a)" (or (badb-config-ui-theme cfg) "default")))
         'open-theme-menu)
   (cons (cons "Guardrail"
               (format #f "Set policy level (~a)" (guardrail-policy-level policy)))
         'open-guardrail-menu)
   (cons (cons "Reload" "Reload configuration from disk") 'config-reload)
   (cons (cons "Save" "Save configuration to disk") 'config-save)))

(define (format-config-summary cfg cur-model cur-endpoint cur-journal-path)
  "Format the /config show status report."
  (let* ((global-path (default-global-config-path))
         (local-path (default-local-config-path))
         (global? (file-exists? global-path))
         (local? (file-exists? local-path))
         (mcp-count (length (badb-config-mcp-servers cfg))))
    (format #f
            "badb Configuration:\n~
            • Model: ~a\n~
            • Endpoint: ~a\n~
            • Temperature: ~a\n~
            • Journal Path: ~a\n~
            • Global Config: ~a (~a)\n~
            • Local Config:  ~a (~a)\n~
            • MCP Servers Configured: ~a"
            cur-model
            cur-endpoint
            (badb-config-model-temperature cfg)
            cur-journal-path
            global-path
            (if global? "active" "not found")
            local-path
            (if local? "active" "not found")
            mcp-count)))

(define (format-provider-presets provs)
  "Format the list of configured provider presets for /provider."
  (if (null? provs)
      "No provider presets configured.\nDefine presets under (options (providers ...)) in .badb/config.scm."
      (string-append "Configured Provider Presets:\n"
                     (string-join
                      (map (lambda (p)
                             (format #f "• ~a -> ~a"
                                     (car p)
                                     (or (get-prop (cdr p) 'endpoint #f) "(no endpoint)")))
                           provs)
                      "\n")
                     "\nUsage: /provider <name> to switch the active preset.")))

(define (format-rules-and-tree workspace-dir)
  "Format discovered project rules and workspace file tree for /rules."
  (let* ((rules (discover-project-rules workspace-dir))
         (tree (build-workspace-file-tree workspace-dir #:max-depth 2 #:max-files 20))
         (rule-desc (if (null? rules)
                        "No project rule files found."
                        (string-join
                         (map (lambda (r) (format #f "• ~a" (assoc-ref r 'path))) rules)
                         "\n"))))
    (format #f "Discovered Project Rules:\n~a\n\nWorkspace Tree:\n~a" rule-desc tree)))

(define (format-context-metrics compactor llm-history messages)
  "Format conversation context metrics for /context."
  (let* ((tokens (estimate-conversation-tokens llm-history))
         (ceiling (- (context-compactor-max-tokens compactor)
                     (context-compactor-reserve-tokens compactor)))
         (needed? (compaction-needed? compactor llm-history)))
    (format #f
            "Conversation Context Metrics:\n• LLM Message Count: ~a\n• UI Cards Count: ~a\n• Estimated Tokens: ~a / ~a ceiling\n• Status: ~a"
            (length llm-history)
            (length messages)
            tokens
            ceiling
            (if needed? "COMPACTION REQUIRED" "NORMAL"))))

(define (format-usage-report compactor llm-history prompt-tokens completion-tokens estimated-tokens turns)
  "Format per-session token usage report for /usage."
  (let* ((used (estimate-conversation-tokens llm-history))
         (ceiling (context-compactor-effective-ceiling compactor))
         (k (lambda (n) (format #f "~,1fk" (/ n 1000.0))))
         (est-suffix (if (> estimated-tokens 0)
                         (format #f " (+~a estimated)" estimated-tokens)
                         "")))
    (format #f
            "Session token usage:\n• Prompt tokens: ~a~a\n• Completion tokens: ~a\n• Turns: ~a\n• Context: ~a/~a tokens (estimated)"
            prompt-tokens
            est-suffix
            completion-tokens
            turns
            (if (number? used) (k used) "?")
            (if (> ceiling 0) (k ceiling) "?"))))

(define (format-features-list reg)
  "Format active registry features for /features."
  (let ((feats (registry-list reg)))
    (if (null? feats)
        "No features currently registered in kernel."
        (string-append
         "Registered Features:\n"
         (string-join
          (map (lambda (f)
                 (format #f "• ~a (~a)" (assoc-ref f 'id) (assoc-ref f 'metadata)))
               feats)
          "\n")))))

(define (format-journal-recent jrn)
  "Format recent checkpoint provenance entries for /journal."
  (let ((entries (journal-entries jrn)))
    (if (null? entries)
        "Journal is empty (0 checkpoints on disk)."
        (let* ((recent (take-right entries (min 8 (length entries))))
               (formatted (map (lambda (e)
                                 (format #f "  #~a: [~a] ~a -> ~a (~a)"
                                         (assoc-ref e "iteration")
                                         (assoc-ref e "checkpoint")
                                         (assoc-ref e "feature-id")
                                         (assoc-ref e "status")
                                         (assoc-ref e "timestamp")))
                               recent)))
          (string-append "Recent Checkpoints:\n" (string-join formatted "\n"))))))

(define %tui-help-message
  (string-append "Available Commands:\n"
                 "  /help             Display this help message\n"
                 "  /model [NAME]     Switch LLM model; no argument opens a menu\n"
                 "  /models           List models available on the endpoint\n"
                 "  /endpoint [URL]   Switch endpoint URL; no argument opens a menu\n"
                 "  /provider [NAME]  Switch or inspect provider preset\n"
                 "  /mcp [CMD]        Manage MCP servers (list, start, stop, add, remove, tools)\n"
                 "  /config [CMD]     Inspect, reload, save, set configuration; no argument opens a menu\n"
                 "  /guardrail [LVL]  Set policy level (disabled, permissive, cautious, strict, paranoid)\n"
                 "  /compact          Compact conversation history\n"
                 "  /resume           Resume a previous chat session from disk\n"
                 "  /checkpoint [DSC] Create a rollback checkpoint snapshot\n"
                 "  /rollback [ID]    Revert to latest or specified checkpoint\n"
                 "  /rules            View discovered project rules & workspace tree\n"
                 "  /geis [CMD]       Contract-gated dynamic synthesis (/geis add <desc>, list, verify, status)\n"
                 "  /context          View conversation token metrics\n"
                 "  /usage            Show per-session token usage report\n"
                 "  /steer [TEXT]     Queue steering text for the running or next agent run\n"
                 "  /interrupt        Halt the agent at its next iteration boundary\n"
                 "  /features         List active registry features\n"
                 "  /journal          View recent checkpoint provenance\n"
                 "  /test             Run the bootstrap test suite\n"
                 "  /clear            Clear chat history\n"
                 "  /quit, /exit      Exit TUI\n"
                 "  /dump [PATH]      Export the conversation to a text file\n"
                 "  !<command>        Execute shell command\n"
                 "  ,<expr> or (..)   Evaluate Scheme code\n"
                 "  ctrl+up / ctrl+down   Select a message block\n"
                 "  ctrl+e            Collapse or expand the selected message\n"
                 "  while agent runs  type to steer it: enter sends, ctrl+i interrupts\n"
                 "  menus             up/down move, enter apply, esc close (/config, /model, /endpoint)\n"
                 "  /sh <cmd>         Alias for shell command\n"
                 "  /eval <expr>      Alias for Scheme eval"))

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
          (or
            (false-if-exception (resolve-interface '(geasa surfaces-tui nc)))
            (resolve-interface '(geasa surfaces-tui nc)))
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
            (make-context-compactor #:max-tokens 16384 #:reserve-tokens 2048))
    (define rollback-mgr
            (make-rollback-manager #:workspace-dir (getcwd) #:registry reg #:journal jrn))
    (define verifier (make-verifier #:check-syntax? #t #:run-tests? #f))
    (define composer (make-prompt-composer))
    ;; Register Built-in Autonomous Agent Tools & Auto-start MCP servers
    (define llm-history '())
    (register-builtin-tui-tools! reg
                                 rollback-mgr
                                 verifier
                                 mcp-mgr
                                 (lambda ()
                                   llm-history))
    (auto-start-mcp-servers! cfg mcp-mgr)
    (define client
            (make-model-client #:endpoint
                               cur-endpoint
                               #:api-key
                               cur-api-key
                               #:model
                               cur-model
                               #:allow-missing-thinking?
                               (badb-config-allow-missing-thinking? cfg cur-endpoint)))
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
                       ,(default-system-prompt #:workspace-dir (getcwd)
                                               #:tools
                                               (collect-available-tools reg mcp-mgr)))))
    ;; The system prompt stays in llm-history for the model but is not
    ;; displayed as a transcript card.
    (define messages
            (list
              `((role . "notice")
                (content .
                         ,(string-append "badb microharness\n"
                                         "• ! shell · ( scheme · / commands · /help for keys")))))
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
                                              (length (registry-list reg))
                                              (journal-count jrn)
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
                  (row-cache-prune! row-cache messages)
                  (let*
                      ((msg-count (length messages))
                       (sel-idx (and (integer? msgsel-index) (< msgsel-index msg-count) msgsel-index))
                       (indexed-rows
                         (map
                           (lambda (idx msg)
                             (map
                               (lambda (entry)
                                 (cons idx entry))
                               (row-cache-rows row-cache msg content-width)))
                           (iota msg-count)
                           messages))
                       (formatted-lines (apply append indexed-rows))
                       (total-lines (length formatted-lines))
                       (max-scroll (max 0 (- total-lines view-height))))
                    ;; Clamp scroll after collapse state changed line counts
                    (set! scroll-offset (min scroll-offset max-scroll))
                    ;; Keep the selected message inside the visible window
                    (when sel-idx
                      (let*
                          ((row-counts (map length indexed-rows))
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
                           (take (drop formatted-lines start-line)
                                 (min view-height (- total-lines start-line)))))
                      (let loop
                        ((lines visible-lines) (y view-top))
                        (unless (or (null? lines) (>= y view-bottom))
                          (let ((tagged (car lines)))
                            (hashq-set! %click-targets y (car tagged))
                            (draw-block-line y
                                             (cdr tagged)
                                             content-width
                                             (and sel-idx (= (car tagged) sel-idx))))
                          (loop (cdr lines) (+ y 1))))))
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
                      (set! messages
                            (append (take messages idx) (list updated) (drop messages (+ idx 1))))
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
    (define (context-gauge-info)
            (let ((key (length llm-history)))
              ;; The estimate only changes when history grows; cache it.
              (unless (eq? key %ctx-cache-key)
                (set! %ctx-cache-key key)
                (set! %ctx-cache-tokens (estimate-conversation-tokens llm-history)))
              (let*
                  ((used %ctx-cache-tokens)
                   (ceiling (context-compactor-effective-ceiling compactor))
                   (k
                     (lambda (n)
                       (format #f "~,1fk" (/ n 1000.0)))))
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
                                                             (throttled-stream-render!)))))))
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
              (cond
                ((string-null? arg)
                  (set! active-menu
                        (make-menu "Configuration"
                                   (config-menu-items-for-state cfg cur-model cur-endpoint policy))))
                ((string=? subcmd "show")
                  (add-sys-msg!
                    (format-config-summary cfg cur-model cur-endpoint cur-journal-path)))
                ((string=? subcmd "reload")
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
                ((string=? subcmd "save")
                  (catch #t
                    (lambda ()
                      (save-local-config! cfg)
                      (add-sys-msg!
                        (format #f "Configuration saved to ~a" (default-local-config-path))))
                    (lambda (k . a)
                      (add-err-msg! (format #f "Failed to save config: ~a ~s" k a)))))
                ((string=? subcmd "set")
                  (if (< (length rest) 2)
                    (add-err-msg! "Usage: /config set <model|endpoint|temperature> <value>")
                    (let ((prop (car rest)) (val (cadr rest)))
                      (cond
                        ((string=? prop "model") (set! cur-model val)
                                                 (badb-config-set-model-name! cfg val)
                                                 (refresh-client!)
                                                 (try-save-config!)
                                                 (add-sys-msg! (format #f "Model set to '~a' and saved." val)))
                        ((string=? prop "endpoint")
                          (set! cur-endpoint val)
                          (badb-config-set-model-endpoint! cfg val)
                          (refresh-client!)
                          (try-save-config!)
                          (add-sys-msg! (format #f "Endpoint set to '~a' and saved." val)))
                        ((string=? prop "temperature")
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
                ((= button 4) (set! scroll-offset (+ scroll-offset 3)))
                ((= button 5) (set! scroll-offset (max 0 (- scroll-offset 3))))
                ((= button 1)
                  (let ((target (hashq-ref %click-targets y)))
                    (cond
                      ((and active-menu (pair? target) (eq? (car target) 'menu-item))
                        (set! active-menu
                              (menu-move active-menu (- (cdr target) (menu-selected-index active-menu))))
                        (let ((item (menu-selected-item active-menu)))
                          (when item (apply-menu-selection! item))))
                      ((and (selector-active? input-buffer)
                            (pair? target)
                            (eq? (car target) 'palette-item))
                        (set! selector-index (cdr target)))
                      ((integer? target) (set! msgsel-index target))
                      (else (set! msgsel-index #f))))))))
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
                                   session-turns)))
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
                ((string=? sub "synthesize")
                  (if (string-null? rest-arg)
                    (add-sys-msg! "Usage: /geis synthesize <feature-id> [target-path] [spec-path]")
                    (let*
                        ((parts (string-split rest-arg #\space))
                         (parts
                           (filter
                             (lambda (s)
                               (not (string-null? s)))
                             parts))
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
                                   (or
                                     (false-if-exception (resolve-module '(geasa runtimes-synthesis plugin)))
                                     (resolve-module '(geasa runtimes-synthesis plugin)))
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
                        (lambda (new-buffer new-pos)
                          (set! input-buffer new-buffer)
                          (set! cursor-pos new-pos)))))
                  ids)
                (clamp-selector-index!))
        ;; The input loop as an internal define: needs-render? defaults
        ;; to #t, so every plain (loop) call site keeps rendering. A
        ;; named let would give the recursion a fixed arity and break
        ;; them all.
        (define loop
                (lambda* (#:optional (needs-render? #t))
                         ;; Drain mouse events BEFORE rendering so a click's effects
                         ;; draw in this same iteration (the click map is rebuilt by
                         ;; every render; handlers act on the displayed frame).
                         (for-each handle-mouse-event! (drain-mouse-events))
                         ;; Rendering is skipped only for no-op scroll keys in a key
                         ;; burst: a held or pasted run of scroll keys must not trigger
                         ;; a full-transcript render per key.
                         (when needs-render? (render!))
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
                                     (lambda (new-buffer new-pos)
                                       (set! input-buffer new-buffer)
                                       (set! cursor-pos new-pos)))
                                   (clamp-selector-index!))
                                 ((not (eq? prompt-mode 'chat)) (set! prompt-mode 'chat)))
                               (loop))
                             ;; Page Up / Page Down: half-viewport jumps
                             ((eqv? ch KEY_PPAGE)
                               (set! scroll-offset
                                     (+ scroll-offset (max 1 (quotient (- (getmaxy stdscr) 6) 2))))
                               (loop))
                             ((eqv? ch KEY_NPAGE)
                               (set! scroll-offset
                                     (max 0 (- scroll-offset (max 1 (quotient (- (getmaxy stdscr) 6) 2)))))
                               (loop))
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
                                 (lambda (new-buffer new-pos)
                                   (set! input-buffer new-buffer)
                                   (set! cursor-pos new-pos)))
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
                                           (lambda (new-buffer new-pos)
                                             (set! input-buffer new-buffer)
                                             (set! cursor-pos new-pos)))
                                         (clamp-selector-index!)
                                         (loop)))))))
                             (else (loop))))))
        (loop))
      (lambda ()
        (shutdown!)))))