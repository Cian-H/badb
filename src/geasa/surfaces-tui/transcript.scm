;;; (geasa surfaces-tui transcript) — Message Rendering, Styling & Layout Formatters
;;;
;;; Extracted from surfaces-tui plugin.scm. Provides message styling, thought/content
;;; splitting, status indicators, collapsible block formatting, row cache, and key mapping.

(define-module (geasa surfaces-tui transcript)
  #:use-module (geasa surfaces-tui editor)
  #:use-module (geasa foundation-util plugin)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:autoload (syntax-highlight) (highlight highlights->sxml)
  #:autoload (syntax-highlight scheme) (lex-scheme)
  #:autoload (syntax-highlight c) (lex-c)
  #:autoload (syntax-highlight lisp) (lex-lisp)
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
            %status-tags
            segment-line
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
            message-role->theme-role
            system-message-label
            make-block-entry
            message-block-label
            format-message-entry
            format-collapsed-message-rows
            <row-cache>
            row-cache?
            make-row-cache
            row-cache-rows
            row-cache-prune!
            key->code
            resolve-modified-key))

;; Syntax highlighting helpers.
(define (get-lexer lang)
  (cond ((string=? lang "scheme") lex-scheme)
        ((string=? lang "c") lex-c)
        ((string=? lang "lisp") lex-lisp)
        (else #f)))

(define (syntax-tag->theme-role tag)
  (cond ((memq tag '(comment)) 'dim)
        ((memq tag '(string)) 'accent)
        ((memq tag '(special keyword)) 'scheme)
        (else 'default)))

(define (highlights->segments highlights)
  (let loop ((hls highlights) (result '()) (current-str "") (current-role 'default))
    (if (null? hls)
        (if (string-null? current-str)
            (reverse result)
            (reverse (cons (list current-str current-role #f #f) result)))
        (let* ((h (car hls))
               (text (cond ((string? h) h)
                           ((and (pair? h) (string? (last h))) (last h))
                           (else "")))
               (tag (if (pair? h) (car h) #f))
               (role (if tag (syntax-tag->theme-role tag) 'default)))
          (if (eq? current-role role)
              (loop (cdr hls) result (string-append current-str text) role)
              (loop (cdr hls)
                    (if (string-null? current-str)
                        result
                        (cons (list current-str current-role #f #f) result))
                    text
                    role))))))

(define (highlight-text content lang)
  (catch #t
    (lambda ()
      (let* ((lexer (get-lexer lang))
             (hls (if lexer (highlight lexer content) #f)))
        (if hls
            (highlights->segments hls)
            (list (list content 'default #f #f)))))
    (lambda (key . args)
      (list (list content 'default #f #f)))))

(define (process-fenced-code-blocks text)
  "Process fenced code blocks in TEXT: colorize scheme/c/lisp code
between ``` fences via highlight-text and mark segments with [CODE:...]
roles for segment-line rendering; degrade to raw TEXT on any error."
  (catch #t
    (lambda ()
      (let* ((lines (string-split text #\newline))
             (in-code-block? #f)
             (lang "")
             (result '()))
        (let loop ((lines lines) (in-code-block? in-code-block?) (lang lang) (result result))
          (if (null? lines)
              (string-join (reverse result) "\n")
              (let* ((line (car lines))
                     (trimmed (string-trim-both line)))
                (cond
                 ((and (not in-code-block?) (string-prefix? "```" trimmed))
                  (let* ((lang-part (substring trimmed 3))
                         (lang-end (let loop-lang ((i 0))
                                     (if (or (>= i (string-length lang-part))
                                             (memq (string-ref lang-part i) '(#\space #\tab #\newline)))
                                         i
                                         (loop-lang (+ i 1)))))
                         (block-lang (if lang-end (substring lang-part 0 lang-end) "")))
                    (loop (cdr lines) #t block-lang
                          (cons (format #f "```~a" block-lang) result))))
                 ((and in-code-block? (string-prefix? "```" trimmed))
                  (loop (cdr lines) #f ""
                        (cons "```" result)))
                 (in-code-block?
                  (let* ((segments (highlight-text line lang))
                         (formatted-line
                          (string-join
                           (map (lambda (seg)
                                  (let ((seg-text (car seg))
                                        (role (cadr seg)))
                                    (format #f "[CODE:~a]~a[/CODE]" role seg-text)))
                                segments)
                           "")))
                    (loop (cdr lines) in-code-block? lang
                          (cons formatted-line result))))
                 (else
                  (loop (cdr lines) in-code-block? lang
                        (cons line result)))))))))
    (lambda (key . args)
      text)))

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
    (values "⟳" 'status #t))
   ((string=? status-text "EXEC...")
    (values "◆" 'shell #t))
   ((string-prefix? "TOOL:" status-text)
    (values "◆" 'tool-call #t))
   ((string-prefix? "REACT" status-text)
    (values "◆" 'tool-call #t))
   ((string=? status-text "EVAL...")
    (values "λ" 'scheme #t))
   ((string=? status-text "TESTING...")
    (values "◆" 'warning #t))
   ((eq? prompt-mode 'shell)
    (values "$" 'shell #t))
   ((eq? prompt-mode 'scheme)
    (values "λ" 'scheme #t))
   ((string-prefix? "!" input-buffer)
    (values "$" 'shell #t))
   ((string-prefix? "(" input-buffer)
    (values "λ" 'scheme #t))
   (else
    (values "❯" 'accent #t))))

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
  "Return #t when STATUS-TEXT indicates the autonomous ReAct loop is
currently executing."
  (and (string? status-text)
       (or (string-prefix? "REACT" status-text)
           (string-prefix? "TOOL:" status-text))
       #t))

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
           ((string-prefix? "[CODE:" rem)
            (let* ((end-pos (string-index rem "[/CODE]"))
                   (code-end (if end-pos (+ pos end-pos 7) #f)))
              (if code-end
                  (let* ((full-match (substring line pos code-end))
                         (colon-pos (string-index full-match ":"))
                         (role-start (if colon-pos (+ colon-pos 1) 6))
                         (bracket-pos (string-index full-match "]"))
                         (role (if (and bracket-pos (> bracket-pos role-start))
                                   (substring full-match role-start bracket-pos)
                                   'accent))
                         (content-start bracket-pos)
                         (content-end-pos (string-index full-match "[/CODE]"))
                         (content (substring full-match (+ content-start 1) content-end-pos)))
                    (let* ((opening-fence (substring full-match 0 (+ bracket-pos 1)))
                           (code-content content)
                           (closing-fence "[/CODE]"))
                      (loop code-end
                            (cons (list opening-fence 'dim #f #f)
                                  (cons (list code-content role #f #f)
                                        (cons (list closing-fence 'dim #f #f) acc)))))))
                  (loop (string-length line)
                        (cons (list (substring line pos) 'default #f #f) acc))))
           ((string-prefix? "`" rem)
            (let* ((end-pos (string-index rem "`" 1))
                   (code-end (if end-pos (+ pos end-pos 1) #f)))
              (if code-end
                  (loop code-end
                        (cons (list (substring line pos code-end) 'accent #f #f) acc))
                  (loop (string-length line)
                        (cons (list (substring line pos) 'default #f #f) acc)))))
           (else
            (let find-next ((end (+ pos 1)))
              (if (>= end (string-length line))
                  (reverse (cons (list (substring line pos) 'default #f #f) acc))
                  (let ((sub-rem (substring line end)))
                    (if (or (any (lambda (t) (string-prefix? (car t) sub-rem)) %status-tags)
                            (string-prefix? "• " sub-rem)
                            (string-prefix? "❯ " sub-rem)
                            (regexp-exec %key-label-rx sub-rem)
                            (string-prefix? "`" sub-rem)
                            (string-prefix? "[CODE:" sub-rem)
                            (string-prefix? "[/CODE]" sub-rem))
                        (loop end
                              (cons (list (substring line pos end) 'default #f #f) acc))
                        (find-next (+ end 1))))))))))))

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
  (append (take lst idx) (list val) (drop lst (+ idx 1))))

(define (message-collapsed? msg)
  "Return #t when MSG carries a truthy 'collapsed? flag."
  (and (assoc-ref msg 'collapsed?) #t))

(define (message-with-collapsed msg collapsed?)
  "Return a new message alist with MSG's 'collapsed? flag set to COLLAPSED?."
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
  "Return #t when a card tagged with ROLE collapses by default."
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
to a summary row by default."
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
  "Compose the dim summary text for a collapsed block."
  (format #f "~a (~a line~a) — ctrl+e to expand"
          label hidden-lines (if (= hidden-lines 1) "" "s")))

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

(define (message-block-label role content)
  "Return the label text for the block header of a message with ROLE and CONTENT."
  (if (string=? role "system")
      (system-message-label content)
      (message-role-label role)))

(define (format-message-entry msg content-width)
  "Format MSG into flat block rows for a viewport CONTENT-WIDTH wide."
  (let* ((role-val (assoc-ref msg 'role))
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
                   (format #f "~a" raw-thoughts))))
         ;; Process fenced code blocks in content and thoughts
         (content (process-fenced-code-blocks content))
         (thoughts (and thoughts (process-fenced-code-blocks thoughts))))
    (cond
     ((and (message-collapsed? msg) (message-collapsible? msg))
      (format-collapsed-message-rows msg role content thoughts content-width))
     ((string=? role "assistant")
      (let* ((th-block
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
      (let* ((theme-role (message-role->theme-role role))
             (label (message-block-label role content))
             (text (if (string=? role "user") (string-trim-both content) content)))
        (append
         (make-block-entry label theme-role (wrap-text text content-width))
         (list (list "spacer" ""))))))))

(define (format-collapsed-message-rows msg role content thoughts content-width)
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
                (make-block-entry "assistant"
                                  'assistant
                                  (wrap-text content content-width))
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

(define (row-cache-rows cache msg content-width)
  "Return the formatted block rows of MSG at CONTENT-WIDTH, memoized in
CACHE per message object."
  (unless (eq? (row-cache-width cache) content-width)
    (set-row-cache-width! cache content-width)
    (hash-clear! (row-cache-table cache)))
  (or (hashq-ref (row-cache-table cache) msg)
      (let ((rows (format-message-entry msg content-width)))
        (hashq-set! (row-cache-table cache) msg rows)
        rows)))

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
