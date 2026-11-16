;;; (geasa surfaces-tui transcript) — Message Rendering, Styling & Layout Formatters
;;;
;;; Extracted from surfaces-tui plugin.scm. Provides message styling, thought/content
;;; splitting, status indicators, collapsible block formatting, row cache, and key mapping.

(define-module (geasa surfaces-tui transcript)
               #:use-module (geasa surfaces-tui editor)
               #:use-module (geasa surfaces-tui markdown)
               #:use-module (geasa foundation-util plugin)
               #:use-module (ice-9 format)
               #:use-module (ice-9 regex)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:re-export (process-fenced-code-blocks)
               #:export (split-thoughts-and-content
                          color-symbol->ncurses-constant
                          %spinner-frames
                          streaming-spinner-glyph
                          get-prompt-indicator
                          format-rule-line
                          status->theme-role
                          format-header-segments
                          format-keymap-line
                          steering-key-handle
                          steering-pending-line
                          agent-run-active?
                          spinner-active-status?
                          format-osc-tab-title
                          reset-osc-tab-title
                          %status-tags
                          segment-line
                          block-row-segments
                          replace-nth
                          message-collapsed?
                          message-with-collapsed
                          make-collapsed-message
                          message-toggle-collapsed
                          default-collapsed-role?
                          message-collapsible?
                          default-collapse-message
                          %collapsed-preview-chars
                          thoughts-preview
                          format-collapsed-summary-line
                          message-role-label
                          message-role-icon
                          message-role->theme-role
                          system-message-label
                          make-block-entry
                          format-card-header
                          format-card-divider
                          card-gutter-glyph
                          message-block-label
                          format-message-entry
                          format-collapsed-message-rows
                          <row-cache>
                          row-cache?
                          make-row-cache
                          row-cache-rows
                          row-cache-invalidate!
                          row-cache-clear!
                          row-cache-prune!
                          key->code
                          resolve-modified-key))

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
        (let*
            ((ms
               (quotient (get-internal-real-time)
                         (quotient internal-time-units-per-second 1000)))
             (i (modulo (quotient ms 80) (vector-length %spinner-frames))))
          (vector-ref %spinner-frames i)))

(define* (get-prompt-indicator input-buffer status-text #:optional (prompt-mode 'chat))
         "Return (values glyph-char theme-role bold?) for the prompt indicator
based on current input mode, execution status, and active prompt mode."
         (let ((st (or status-text "")))
           (cond
             ((string=? st "STREAMING...")
               (values "⟳" 'status #t))
             ((string=? st "EXEC...")
               (values "◆" 'shell #t))
             ((or (string-prefix? "TOOL" st)
                  (string-prefix? "CALLING" st)
                  (string-prefix? "REACT" st)
                  (string-prefix? "THINKING" st)
                  (string-prefix? "SYSTEMONE" st)
                  (string-prefix? "RATE LIMITED" st)
                  (string-prefix? "WORKING" st))
               (values "◆" 'tool-call #t))
             ((string=? st "EVAL...")
               (values "λ" 'scheme #t))
             ((string=? st "TESTING...")
               (values "◆" 'warning #t))
             ((eq? prompt-mode 'shell)
               (values "$" 'shell #t))
             ((eq? prompt-mode 'scheme)
               (values "λ" 'scheme #t))
             ((eq? prompt-mode 'slash)
               (values "/" 'accent #t))
             ((string-prefix? "!" input-buffer)
               (values "$" 'shell #t))
             ((string-prefix? "(" input-buffer)
               (values "λ" 'scheme #t))
             (else
               (values "❯" 'accent #t)))))

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
          ((not (string? status-text)) 'status)
          ((string=? status-text "READY") 'success)
          ((or (string-prefix? "EXEC" status-text)
               (string-prefix? "EVAL" status-text)
               (string-prefix? "STREAMING" status-text)
               (string-prefix? "TESTING" status-text)
               (string-prefix? "REACT" status-text)
               (string-prefix? "TOOL" status-text)
               (string-prefix? "CALLING" status-text)
               (string-prefix? "THINKING" status-text)
               (string-prefix? "SYSTEMONE" status-text)
               (string-prefix? "RATE LIMITED" status-text)
               (string-prefix? "WORKING" status-text)
               (string-prefix? "SYNTHESIZING" status-text))
            'accent)
          (else 'status)))

(define* (format-header-segments maxx status-text model-name feat-count jrn-count
                                 #:optional (mcp-info #f) (ctx-info #f))
         "Build the slim header line as positioned segments of the form
((x text role bold?) ...) for a terminal MAXX columns wide."
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
                        ((>= model-room 1) (build (substring model 0 model-room) items))
                        ((pair? items) (fit (drop-right items 1)))
                        (else (build "" '())))))))

(define (format-keymap-line mode)
        "Return the dim bottom keymap hint line describing the keys active in
MODE (a symbol: chat, shell, scheme, selector, menu or steering)."
        (case mode
          ((shell) "enter:run · backspace:back to chat")
          ((scheme) "enter:eval · backspace:back to chat")
          ((slash) "tab:complete · enter:run · backspace:back to chat · esc:cancel")
          ((selector) "↑↓:choose · tab:complete · enter:run · esc:dismiss")
          ((menu) "↑↓:move · enter:apply · esc:close")
          ((steering) "type:steer · enter:send · ^i:interrupt · bs:edit · esc:clear")
          (else "! shell · ( scheme · / cmds · ↑↓ history · PgUp/PgDn scroll · ^↑/^↓ select · ^e collapse")))

;;;
;;; Agent Steering Helpers
;;;
(define (steering-key-handle buffer ch)
        "Map a keypress CH to a steering action and the resulting buffer.
Return (values new-buffer action)."
        (cond
          ((or (eqv? ch #\newline) (eqv? ch #\return) (eqv? ch 10) (eqv? ch 13))
            (if (string-null? (string-trim-both buffer))
              (values buffer 'none)
              (values "" 'queue)))
          ((or (eqv? ch #\tab) (eqv? ch 9))
            (values "" 'interrupt))
          ((eqv? ch 27)
            (values "" 'clear))
          ((or (eqv? ch #\backspace) (eqv? ch 8) (eqv? ch 127))
            (if (string-null? buffer)
              (values buffer 'none)
              (values (substring buffer 0 (- (string-length buffer) 1)) 'backspace)))
          ((and (char? ch) (>= (char->integer ch) 32))
            (values (string-append buffer (string ch)) 'append))
          ((and (integer? ch) (>= ch 32) (<= ch 126))
            (values (string-append buffer (string (integer->char ch))) 'append))
          (else (values buffer 'none))))

(define* (steering-pending-line buffer queued-count
                                #:optional (interrupt-queued? #f))
         "Compose the dim pending-steering display line near the prompt."
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
        "Return #t when STATUS-TEXT indicates the autonomous ReAct loop or a
long-running tool invocation (REACT, TOOL:, TOOL CALL, CALLING, THINKING,
SYSTEMONE, RATE LIMITED, WORKING) is currently executing."
        (and (string? status-text)
             (or (string-prefix? "REACT" status-text)
                 (string-prefix? "TOOL" status-text)
                 (string-prefix? "CALLING" status-text)
                 (string-prefix? "THINKING" status-text)
                 (string-prefix? "SYSTEMONE" status-text)
                 (string-prefix? "RATE LIMITED" status-text)
                 (string-prefix? "WORKING" status-text))
             #t))

(define (spinner-active-status? status-text)
        "Return #t when STATUS-TEXT should animate both the prompt spinner and
the terminal tab title spinner."
        (and (string? status-text)
             (or (agent-run-active? status-text)
                 (string-prefix? "STREAMING" status-text)
                 (string-prefix? "EXEC" status-text)
                 (string-prefix? "EVAL" status-text)
                 (string-prefix? "TESTING" status-text)
                 (string-prefix? "SYNTHESIZING" status-text))
             #t))

(define* (format-osc-tab-title status-text
                               #:key (label "badb")
                               (spinner-glyph #f)
                               (osc-code 2)
                               (model #f))
         "Format an OSC 0 or OSC 2 terminal title sequence (supported by Ghostty,
Kitty, WezTerm, Alacritty, xterm) reflecting STATUS-TEXT and an animated
spinner glyph while working."
         (let* ((code (if (eqv? osc-code 0) 0 2))
                (safe-label (let ((s (sanitize-osc-text (or label "badb"))))
                              (if (string-null? s) "badb" s)))
                (safe-status (sanitize-osc-text (or status-text "")))
                (safe-model (and (string? model)
                                 (not (string-null? model))
                                 (sanitize-osc-text model)))
                (active? (spinner-active-status? status-text))
                (glyph (and active?
                            (sanitize-osc-text (or spinner-glyph (streaming-spinner-glyph)))))
                (title-body
                  (cond
                    (active?
                      (string-append glyph " " safe-label " — " safe-status
                                     (if safe-model (string-append " · " safe-model) "")))
                    (safe-model
                      (string-append safe-label " · " safe-model))
                    (else safe-label))))
           (format #f "\x1b]~a;~a\x07" code title-body)))

(define* (reset-osc-tab-title #:key (label "badb") (osc-code 2))
         "Return an OSC 0/2 sequence restoring the idle terminal tab title."
         (format-osc-tab-title "READY" #:label label #:osc-code osc-code))

;;;
;;; Visual Hierarchy & Token Highlight Engine
;;;
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

(define %key-label-rx (make-regexp "^[ \t]*([A-Za-z0-9_-]+( [A-Za-z0-9_-]+){0,2}):[ \t]+"))

(define (segment-line line)
        "Segment LINE into styled chunks of the form ((chunk-str theme-role bold? dim?) ...)."
        (let loop
          ((pos 0) (acc '()))
          (if (>= pos (string-length line))
            (reverse acc)
            (let*
                ((rem (substring line pos))
                 (tag-match
                   (find
                     (lambda (tag-entry)
                       (string-prefix? (car tag-entry) rem))
                     %status-tags)))
              (cond
                (tag-match
                  (let*
                      ((tag-str (car tag-match))
                       (theme-role (cadr tag-match))
                       (bold? (caddr tag-match))
                       (dim? (cadddr tag-match)))
                    (loop (+ pos (string-length tag-str))
                          (cons (list tag-str theme-role bold? dim?) acc))))
                ((or (string-prefix? "• " rem) (string-prefix? "❯ " rem))
                  (let ((bullet-str (substring rem 0 2)))
                    (loop (+ pos 2) (cons (list bullet-str 'accent #t #f) acc))))
                ((regexp-exec %key-label-rx rem) =>
                                                 (lambda (m)
                                                   (let*
                                                       ((full-match (match:substring m 0)) (match-len (string-length full-match)))
                                                     (loop (+ pos match-len) (cons (list full-match 'accent #f #f) acc)))))
                ((string-prefix? "[CODE:" rem)
                  (let*
                      ((end-pos (string-contains rem "[/CODE]"))
                       (code-end
                         (if end-pos
                           (+ pos end-pos 7)
                           #f)))
                    (if code-end
                      (let*
                          ((full-match (substring line pos code-end))
                           (colon-pos (string-index full-match #\:))
                           (role-start
                             (if colon-pos
                               (+ colon-pos 1)
                               6))
                           (bracket-pos (string-index full-match #\]))
                           (role
                             (if (and bracket-pos (> bracket-pos role-start))
                               (string->symbol (substring full-match role-start bracket-pos))
                               'accent))
                           (content-start bracket-pos)
                           (content-end-pos (string-contains full-match "[/CODE]"))
                           (content (substring full-match (+ content-start 1) content-end-pos)))
                        (loop code-end
                              (if (string-null? content)
                                acc
                                (cons (list content role #f #f) acc))))
                      (loop (string-length line)
                            (cons (list (substring line pos) 'default #f #f) acc)))))
                ((string-prefix? "`" rem)
                  (let*
                      ((end-pos (string-index rem #\` 1))
                       (code-end
                         (if end-pos
                           (+ pos end-pos 1)
                           #f)))
                    (if code-end
                      (loop code-end (cons (list (substring line pos code-end) 'accent #f #f) acc))
                      (loop (string-length line)
                            (cons (list (substring line pos) 'default #f #f) acc)))))
                (else
                  (let find-next
                    ((end (+ pos 1)))
                    (if (>= end (string-length line))
                      (reverse (cons (list (substring line pos) 'default #f #f) acc))
                      (let ((sub-rem (substring line end)))
                        (if
                            (or
                              (any
                                (lambda (t)
                                  (string-prefix? (car t) sub-rem))
                                %status-tags)
                              (string-prefix? "• " sub-rem)
                              (string-prefix? "❯ " sub-rem)
                              (regexp-exec %key-label-rx sub-rem)
                              (string-prefix? "`" sub-rem)
                              (string-prefix? "[CODE:" sub-rem)
                              (string-prefix? "[/CODE]" sub-rem))
                          (loop end (cons (list (substring line pos end) 'default #f #f) acc))
                          (find-next (+ end 1))))))))))))

(define (block-row-segments row)
        "Return pre-computed styled segments for a block-body ROW if attached,
otherwise fall back to (segment-line (caddr row))."
        (cond
          ((and (pair? row)
                (string=? (car row) "block-body")
                (>= (length row) 4)
                (list? (cadddr row)))
            (cadddr row))
          ((and (pair? row) (>= (length row) 3) (string? (caddr row)))
            (segment-line (caddr row)))
          (else '())))

;;;
;;; Key normalizers
;;;
(define (key->code ch)
        "Normalize a keypress CH from the backend's getch to an integer code."
        (cond
          ((not ch) ch)
          ((char? ch) (char->integer ch))
          (else ch)))

(define (resolve-modified-key seq probe bind fallback)
        "Return the integer code getch delivers when the terminal sends the
escape sequence SEQ."
        (or (probe seq)
            (and (bind seq fallback) fallback)
            0))

;;;
;;; Message Collapse State & Block Formatting
;;;
(define (replace-nth lst idx val)
        "Return a copy of LST with the element at position IDX replaced by VAL."
        (let loop ((rest lst) (i 0) (acc '()))
                  (cond
                    ((null? rest) (reverse acc))
                    ((= i idx) (append-reverse acc (cons val (cdr rest))))
                    (else (loop (cdr rest) (+ i 1) (cons (car rest) acc))))))

(define (message-collapsed? msg)
        "Return #t when MSG carries a truthy 'collapsed? flag."
        (and (pair? msg) (prop-ref msg 'collapsed?) #t))

(define (message-with-collapsed msg collapsed?)
        "Return a new message alist with MSG's 'collapsed? flag set to COLLAPSED?."
        (if collapsed?
          (prop-set msg 'collapsed? #t)
          (alist-remove 'collapsed? msg)))

(define (make-collapsed-message msg)
        "Return a new message alist with MSG's 'collapsed? flag set to #t."
        (message-with-collapsed msg #t))

(define (message-toggle-collapsed msg)
        "Return a new message alist with MSG's collapse state flipped.
Stores an explicit #f when expanding so default-collapse-message preserves
user expansion and row-cache sees a distinct alist cell."
        (prop-set msg 'collapsed? (not (message-collapsed? msg))))

(define (default-collapsed-role? role)
        "Return #t when a card tagged with ROLE collapses by default."
        (let ((r (cond
                   ((string? role) role)
                   ((symbol? role) (symbol->string role))
                   (else (format #f "~a" (or role ""))))))
          (or (string=? r "thought") (string=? r "thinking"))))

(define (extract-msg-thoughts msg)
        (let ((t (or (prop-ref msg 'thoughts)
                     (prop-ref msg 'reasoning_content))))
          (and t
               (let ((s (if (string? t) t (format #f "~a" t))))
                 (and (non-empty-string? s) s)))))

(define (message-collapsible? msg)
        "Return #t when MSG has renderable content or thoughts and can therefore
be collapsed into a summary row."
        (or (non-empty-string? (prop-ref msg 'content))
            (and (extract-msg-thoughts msg) #t)))

(define (default-collapse-message msg)
        "Apply the default collapse policy to MSG: assistant reasoning and
thought cards collapse to a summary row by default unless explicitly set."
        (cond
          ((or (assoc 'collapsed? msg) (assoc "collapsed?" msg)) msg)
          ((or (extract-msg-thoughts msg)
               (default-collapsed-role? (prop-ref msg 'role "")))
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
        "Compose the dim summary text for a collapsed block."
        (format #f "~a (~a line~a) — ctrl+e to expand"
                label hidden-lines (if (= hidden-lines 1) "" "s")))

(define (message-role-label role)
        "Return the lowercase role label text for a message ROLE string."
        (cond
          ((string=? role "user") "you")
          ((string=? role "assistant") "assistant")
          ((or (string=? role "thought") (string=? role "thinking")) "thinking")
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

(define* (message-role-icon role #:optional (nerd? (nerd-fonts-enabled?)))
         "Return the Nerd Font or Unicode icon glyph for message ROLE."
         (nerd-icon
           (cond
             ((string=? role "user") 'user)
             ((string=? role "assistant") 'assistant)
             ((or (string=? role "thought") (string=? role "thinking")) 'thinking)
             ((or (string=? role "shell-cmd") (string=? role "shell-out")) 'shell)
             ((or (string=? role "scheme-in") (string=? role "scheme-out")) 'scheme)
             ((or (string=? role "tool-call") (string=? role "tool")) 'tool)
             ((string=? role "tool-result") 'tool-result)
             ((string=? role "error") 'error)
             ((string=? role "notice") 'notice)
             ((string=? role "steer") 'steer)
             (else 'system))
           nerd?))

(define (message-role->theme-role role)
        "Map a message ROLE string to its semantic theme color role symbol."
        (cond
          ((string=? role "user") 'user)
          ((string=? role "assistant") 'assistant)
          ((or (string=? role "thought") (string=? role "thinking")) 'thought)
          ((string=? role "system") 'system)
          ((string=? role "notice") 'notice)
          ((string=? role "error") 'error)
          ((or (string=? role "shell-cmd") (string=? role "shell-out")) 'shell)
          ((or (string=? role "scheme-in") (string=? role "scheme-out")) 'scheme)
          ((or (string=? role "tool-call") (string=? role "tool")) 'tool-call)
          ((string=? role "tool-result") 'tool-result)
          ((string=? role "steer") 'accent)
          (else 'system)))

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

(define (make-block-entry label theme-role lines)
        "Turn wrapped LINES into flat block rows: one block-label header row
carrying LABEL and THEME-ROLE, then one block-body row per line."
        (if (null? lines)
          '()
          (cons (list "block-label" label theme-role)
                (map (lambda (l) (list "block-body" theme-role l)) lines))))

(define (trivial-default-segments? segs)
        (and (pair? segs)
             (null? (cdr segs))
             (let ((s (car segs)))
               (and (eq? (cadr s) 'default)
                    (not (caddr s))
                    (not (cadddr s))))))

(define*
  (make-styled-block-entry label theme-role styled-rows #:key (hide-label? #f))
  "Turn STYLED-ROWS ((line-text . segs) ...) into block rows, attaching
pre-computed segments as the 4th element when non-trivial."
  (if (null? styled-rows)
    '()
    (let
        ((body
           (map
             (lambda (sr)
               (let ((txt (car sr)) (segs (cdr sr)))
                 (if (trivial-default-segments? segs)
                   (list "block-body" theme-role txt)
                   (list "block-body" theme-role txt segs))))
             styled-rows)))
      (if hide-label?
        body
        (cons (list "block-label" label theme-role) body)))))

(define* (format-card-header label width
                             #:key (icon #f)
                             (collapsed? #f)
                             (nerd-fonts? (nerd-fonts-enabled?)))
         "Format a demarcated message card header line with optional icon and trailing rule."
         (let* ((safe-w (max 8 width))
                (chevron (if collapsed?
                           (string-append (nerd-icon 'collapsed nerd-fonts?) " ")
                           ""))
                (lead-icon (if (and (string? icon) (not (string-null? icon)))
                             (string-append icon " ")
                             ""))
                (prefix (string-append "─ " chevron lead-icon (or label "system") " "))
                (plen (string-length prefix)))
           (if (>= plen safe-w)
             (substring prefix 0 safe-w)
             (string-append prefix (make-string (- safe-w plen) #\─)))))

(define* (format-card-divider width #:optional (style 'thin))
         "Format a subtle horizontal divider between message cards."
         (let ((ch (case style
                     ((dashed) #\┄)
                     ((double) #\═)
                     (else #\─))))
           (make-string (max 0 width) ch)))

(define (card-gutter-glyph selected?)
        "Return the 2-column left gutter bar for a message card."
        (if selected? "██" "▏ "))

(define (message-block-label role content)
        "Return the label text for the block header of a message with ROLE and CONTENT."
        (if (string=? role "system")
          (system-message-label content)
          (message-role-label role)))

(define (format-assistant-content-block content content-width streaming?)
        (if streaming?
          (make-block-entry "assistant" 'assistant (wrap-text content content-width))
          (make-styled-block-entry "assistant"
                                   'assistant
                                   (render-markdown content #:width content-width))))

(define (format-highlighted-role-block label theme-role text content-width highlighter)
        (let* ((wrapped (wrap-text text content-width))
               (styled (map (lambda (ln) (cons ln (highlighter ln))) wrapped)))
          (make-styled-block-entry label theme-role styled)))

(define (format-message-entry msg content-width)
        "Format MSG into flat block rows for a viewport CONTENT-WIDTH wide."
        (let*
            ((role-val (prop-ref msg 'role))
             (role
               (cond
                 ((string? role-val) role-val)
                 ((symbol? role-val) (symbol->string role-val))
                 (else "system")))
             (raw-content (or (prop-ref msg 'content) ""))
             (content
               (if (string? raw-content)
                 raw-content
                 (format #f "~a" raw-content)))
             (thoughts (extract-msg-thoughts msg))
             (is-streaming? (prop-ref msg 'streaming))
             (styled-rows (prop-ref msg 'styled-rows))
             (hide-label? (prop-ref msg 'hide-label?)))
          (cond
            ((and (message-collapsed? msg) (message-collapsible? msg))
              (format-collapsed-message-rows msg
                                             role
                                             content
                                             thoughts
                                             content-width
                                             is-streaming?))
            ((pair? styled-rows)
              (let*
                  ((theme-role (message-role->theme-role role))
                   (label (message-block-label role content))
                   (row-pairs
                     (map
                       (lambda (spans)
                         (let*
                             ((clipped
                                (let loop
                                  ((rest spans) (rem (max 0 content-width)) (acc '()))
                                  (if (or (null? rest) (<= rem 0))
                                    (reverse acc)
                                    (let*
                                        ((sp (car rest)) (s (car sp))
                                         (slen (string-length s))
                                         (take-len (min slen rem))
                                         (txt
                                           (if (< take-len slen)
                                             (substring s 0 take-len)
                                             s)))
                                      (loop (cdr rest) (- rem take-len)
                                            (if (zero? take-len)
                                              acc
                                              (cons (cons txt (cdr sp)) acc)))))))
                              (plain (string-concatenate (map car clipped))))
                           (cons plain clipped)))
                       styled-rows)))
                (append
                  (make-styled-block-entry label theme-role row-pairs #:hide-label? hide-label?)
                  (list (list "spacer" "")))))
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
                       (format-assistant-content-block content content-width is-streaming?)
                       '())))
                (if
                    (and is-streaming? (null? th-block) (null? ct-block))
                  '()
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
                    (list (list "spacer" ""))))))
            ((string=? role "shell-cmd")
              (append
                (format-highlighted-role-block
                  (message-block-label role content)
                  'shell
                  content
                  content-width
                  highlight-shell-line)
                (list (list "spacer" ""))))
            ((or (string=? role "scheme-in") (string=? role "scheme-out"))
              (append
                (format-highlighted-role-block
                  (message-block-label role content)
                  'scheme
                  content
                  content-width
                  highlight-scheme-line)
                (list (list "spacer" ""))))
            (else
              (let*
                  ((theme-role (message-role->theme-role role))
                   (label (message-block-label role content))
                   (text
                     (if (string=? role "user")
                       (string-trim-both content)
                       content)))
                (append
                  (make-block-entry label theme-role (wrap-text text content-width))
                  (list (list "spacer" ""))))))))

(define* (format-collapsed-message-rows msg role content thoughts content-width #:optional (streaming? #f))
         "Return the block rows for collapsed MSG with pre-normalized ROLE, CONTENT
and THOUGHTS fields, followed by the trailing spacer row."
         (cond
           ((and (string=? role "assistant") (non-empty-string? thoughts))
             (let* ((th-block
                      (make-block-entry "thinking"
                                        'thought
                                        (wrap-text (string-trim-both thoughts) content-width)))
                    (hidden (max 0 (- (length th-block) 1)))
                    (summary-row
                      (list "block-summary" 'thought
                            (format-collapsed-summary-line "thinking" hidden)))
                    (ct-block
                      (if (non-empty-string? content)
                        (format-assistant-content-block content content-width streaming?)
                        '())))
               (append (list summary-row) ct-block (list (list "spacer" "")))))
           (else
             (let* ((theme-role (message-role->theme-role role))
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

(define (row-cache-invalidate! cache msg)
        "Evict MSG from CACHE so its next render re-formats its block rows."
        (hashq-remove! (row-cache-table cache) msg))

(define (row-cache-clear! cache)
        "Clear all cached message rows in CACHE."
        (hash-clear! (row-cache-table cache)))

(define (row-cache-entry-matches? entry msg)
        (and (vector? entry)
             (= (vector-length entry) 6)
             (eq? (vector-ref entry 0) (message-collapsed? msg))
             (equal? (vector-ref entry 1) (prop-ref msg 'role))
             (equal? (vector-ref entry 2) (prop-ref msg 'thoughts))
             (equal? (vector-ref entry 3) (prop-ref msg 'reasoning_content))
             (equal? (vector-ref entry 4) (prop-ref msg 'content))))

(define (row-cache-rows cache msg content-width)
        "Return the formatted block rows of MSG at CONTENT-WIDTH, memoized in
CACHE per finalized message object and collapse/payload state. In-flight
streaming messages are formatted directly without caching."
        (unless
            (eq? (row-cache-width cache) content-width)
          (set-row-cache-width! cache content-width)
          (hash-clear! (row-cache-table cache)))
        (if (and (pair? msg) (prop-ref msg 'streaming))
          (format-message-entry msg content-width)
          (let ((cached (hashq-ref (row-cache-table cache) msg)))
            (if (row-cache-entry-matches? cached msg)
              (vector-ref cached 5)
              (let ((rows (format-message-entry msg content-width)))
                (hashq-set! (row-cache-table cache)
                            msg
                            (vector (message-collapsed? msg)
                                    (prop-ref msg 'role)
                                    (prop-ref msg 'thoughts)
                                    (prop-ref msg 'reasoning_content)
                                    (prop-ref msg 'content)
                                    rows))
                rows)))))

(define (row-cache-prune! cache messages)
        "Drop cache entries whose message objects are no longer in MESSAGES."
        (let ((live (make-hash-table 31)))
          (for-each (lambda (m) (hashq-set! live m #t)) messages)
          (let ((kept (make-hash-table 63)))
            (hash-for-each (lambda (k v)
                             (when (hashq-ref live k)
                               (hashq-set! kept k v)))
                           (row-cache-table cache))
            (set-row-cache-table! cache kept))))
