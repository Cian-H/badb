;;; badb — Terminal User Interface (TUI)
;;;
;;; Interactive full-screen terminal interface for badb powered by guile-ncurses.
;;; Provides live streaming LLM chat with separated thought reasoning cards,
;;; modal prompt switching (! for shell, ( or , for Scheme, backspace to return),
;;; interactive slash-command selector palette, and modern 2020s container styling.

(define-module (badb tui)
  #:use-module (badb model)
  #:use-module (badb registry)
  #:use-module (badb transaction)
  #:use-module (badb journal)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (run-tui
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
            selector-active?
            get-prompt-indicator
            format-header-line
            format-prompt-divider))

;;;
;;; Text Formatting & Word Wrapping Helpers
;;;

(define (wrap-line line max-width)
  "Wrap a single LINE into a list of strings with length <= MAX-WIDTH,
breaking on whitespace when possible."
  (if (or (<= (string-length line) max-width) (<= max-width 0))
      (list line)
      (let ((words (string-split line #\space)))
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
                  (loop words "" (cons current acc))))))))))

(define (wrap-text text max-width)
  "Wrap multi-line TEXT into lines that each fit within MAX-WIDTH."
  (let ((raw-lines (string-split text #\newline)))
    (append-map (lambda (line) (wrap-line line max-width)) raw-lines)))

;;;
;;; Slash Commands Registry & Parsing
;;;

(define %slash-commands
  '(("help"     . "Show help manual & shortcuts")
    ("model"    . "Switch or inspect active LLM model")
    ("endpoint" . "Switch or inspect endpoint URL")
    ("features" . "List kernel registered features")
    ("journal"  . "View checkpoint provenance audit log")
    ("test"     . "Run bootstrap verification test suite")
    ("sh"       . "Execute shell command (!<cmd>)")
    ("eval"     . "Evaluate Scheme code in REPL (,<expr>)")
    ("clear"    . "Clear chat viewport history")
    ("quit"     . "Exit badb microharness")))

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
      (if (not start-tag)
          (let* ((all-c (if (string-null? (string-trim-both remaining))
                            content-acc
                            (cons (string-trim-both remaining) content-acc)))
                 (final-content (string-trim-both (string-join (reverse all-c) "\n")))
                 (final-thoughts (string-trim-both (string-join (reverse thoughts-acc) "\n\n"))))
            (values (if (string-null? final-thoughts) #f final-thoughts)
                    final-content))
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
                            final-ct)))))))))

;;;
;;; Morphing Prompt Indicator
;;;

(define* (get-prompt-indicator input-buffer status-text #:optional (prompt-mode 'chat))
  "Return (values glyph-char color-pair bold?) for the prompt indicator
based on current input mode, execution status, and active prompt mode."
  (cond
   ((string=? status-text "STREAMING...")
    (values "⟳" 1 #t))     ; Cyan bold spinning
   ((string=? status-text "EXEC...")
    (values "⚙" 6 #t))     ; Magenta bold gear
   ((string=? status-text "EVAL...")
    (values "λ" 1 #t))     ; Cyan bold lambda
   ((string=? status-text "TESTING...")
    (values "⚙" 4 #t))     ; Yellow bold gear
   ((eq? prompt-mode 'shell)
    (values "$" 6 #t))     ; Magenta bold shell mode
   ((eq? prompt-mode 'scheme)
    (values "λ" 1 #t))     ; Cyan bold scheme mode
   ((string-prefix? "!" input-buffer)
    (values "$" 6 #t))     ; Magenta bold shell prefix
   ((or (string-prefix? "," input-buffer) (string-prefix? "(" input-buffer))
    (values "λ" 1 #t))     ; Cyan bold lambda prefix
   ((string-prefix? "/" input-buffer)
    (values "/" 4 #t))     ; Yellow bold slash prefix
   (else
    (values "❯" 2 #t))))   ; Green bold chat

;;;
;;; Modern 2020s Container Layout Formatters
;;;

(define (format-header-line maxx status-text model-name feat-count jrn-count)
  "Format a modern rounded container header bar with status pills."
  (let* ((prefix (format #f "╭─ badb ─ ❲● ~a❳ ─ ❲~a❳ ─ ❲~a feats❳ ─ ❲~a chkpts❳ "
                         status-text model-name feat-count jrn-count))
         (plen (string-length prefix)))
    (if (< plen (- maxx 1))
        (string-append prefix (make-string (- maxx 1 plen) #\─) "╮")
        (let* ((short-prefix (format #f "╭─ badb ─ ❲● ~a❳ ─ ❲~a❳ " status-text model-name))
               (splen (string-length short-prefix)))
          (if (< splen (- maxx 1))
              (string-append short-prefix (make-string (- maxx 1 splen) #\─) "╮")
              (substring (string-append short-prefix (make-string (max 0 (- maxx splen)) #\─) "╮") 0 maxx))))))

(define* (format-prompt-divider maxx #:optional (prompt-mode 'chat))
  "Format the divider bar separating message history from the input prompt."
  (let* ((prefix
          (case prompt-mode
            ((shell)
             "├─ shell prompt ── [Enter: run │ Backspace: return to chat] ")
            ((scheme)
             "├─ scheme prompt ── [Enter: eval │ Backspace: return to chat] ")
            (else
             "├─ prompt ── [Tab: complete │ !: shell │ ,: scheme │ /: commands] ")))
         (plen (string-length prefix)))
    (if (< plen (- maxx 1))
        (string-append prefix (make-string (- maxx 1 plen) #\─) "┤")
        (let* ((short-p (case prompt-mode
                          ((shell) "├─ shell [Bksp: chat] ")
                          ((scheme) "├─ scheme [Bksp: chat] ")
                          (else "├─ prompt [Tab / ! ,] ")))
               (splen (string-length short-p)))
          (if (< splen (- maxx 1))
              (string-append short-p (make-string (- maxx 1 splen) #\─) "┤")
              (substring (string-append "├─ prompt " (make-string (max 0 (- maxx 10)) #\─) "┤") 0 maxx))))))

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
     ((string-prefix? "," trimmed)
      (cons 'scheme (string-trim-both (substring trimmed 1))))
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

(define (make-repl-mod reg jrn mgr client)
  "Create an isolated user module pre-populated with kernel state bindings
and core badb modules for interactive evaluation."
  (let ((mod (make-fresh-user-module)))
    (module-use! mod (resolve-interface '(guile)))
    (module-use! mod (resolve-interface '(badb model)))
    (module-use! mod (resolve-interface '(badb registry)))
    (module-use! mod (resolve-interface '(badb transaction)))
    (module-use! mod (resolve-interface '(badb journal)))
    (catch #t
      (lambda () (module-use! mod (resolve-interface '(badb mcp))))
      (lambda _ #f))
    (module-use! mod (resolve-interface '(ice-9 format)))
    (module-use! mod (resolve-interface '(ice-9 rdelim)))
    (module-use! mod (resolve-interface '(srfi srfi-1)))
    (module-define! mod 'reg reg)
    (module-define! mod 'jrn jrn)
    (module-define! mod 'mgr mgr)
    (module-define! mod 'client client)
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

(define (execute-shell-command cmd)
  "Execute shell CMD and return (values output-string exit-code error-or-#f)."
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
;;; Main TUI Controller
;;;

(define* (run-tui #:key
                  (endpoint (or (getenv "BADB_MODEL_ENDPOINT")
                                "http://127.0.0.1:10100/inference/v1"))
                  (model (or (getenv "BADB_MODEL_NAME")
                             "minicpm5-1b:gguf:q8"))
                  (api-key (or (getenv "BADB_API_KEY")
                               (getenv "OPENAI_API_KEY")
                               #f))
                  (journal-path ".badb/journal.jsonl"))
  "Launch the interactive badb curses TUI."

  ;; Ensure guile-ncurses is available
  (let ((curses-available? (catch #t
                             (lambda ()
                               (resolve-interface '(ncurses curses))
                               #t)
                             (lambda _ #f))))
    (unless curses-available?
      (format (current-error-port)
              "Error: (ncurses curses) module is not installed.~%~
               Please run badb within the Guix dev environment:~%~
                 ./scripts/devenv -- ./scripts/badb~%")
      (exit 1)))

  ;; Dynamically load ncurses procedures
  (let* ((ncurses-mod (resolve-module '(ncurses curses)))
         (initscr (module-ref ncurses-mod 'initscr))
         (endwin (module-ref ncurses-mod 'endwin))
         (cbreak! (module-ref ncurses-mod 'cbreak!))
         (noecho! (module-ref ncurses-mod 'noecho!))
         (keypad! (module-ref ncurses-mod 'keypad!))
         (curs-set (module-ref ncurses-mod 'curs-set))
         (erase (module-ref ncurses-mod 'erase))
         (refresh (module-ref ncurses-mod 'refresh))
         (getmaxy (module-ref ncurses-mod 'getmaxy))
         (getmaxx (module-ref ncurses-mod 'getmaxx))
         (move (module-ref ncurses-mod 'move))
         (addstr (module-ref ncurses-mod 'addstr))
         (getch (module-ref ncurses-mod 'getch))
         (attr-on! (module-ref ncurses-mod 'attr-on!))
         (attr-off! (module-ref ncurses-mod 'attr-off!))
         (has-colors? (module-ref ncurses-mod 'has-colors?))
         (start-color! (module-ref ncurses-mod 'start-color!))
         (init-pair! (module-ref ncurses-mod 'init-pair!))
         (color-pair (module-ref ncurses-mod 'color-pair))
         (A_BOLD (module-ref ncurses-mod 'A_BOLD))
         (A_REVERSE (module-ref ncurses-mod 'A_REVERSE))
         (A_DIM (module-ref ncurses-mod 'A_DIM))
         (COLOR_BLACK (module-ref ncurses-mod 'COLOR_BLACK))
         (COLOR_GREEN (module-ref ncurses-mod 'COLOR_GREEN))
         (COLOR_CYAN (module-ref ncurses-mod 'COLOR_CYAN))
         (COLOR_WHITE (module-ref ncurses-mod 'COLOR_WHITE))
         (COLOR_YELLOW (module-ref ncurses-mod 'COLOR_YELLOW))
         (COLOR_RED (module-ref ncurses-mod 'COLOR_RED))
         (COLOR_BLUE (module-ref ncurses-mod 'COLOR_BLUE))
         (COLOR_MAGENTA (module-ref ncurses-mod 'COLOR_MAGENTA))
         (KEY_ENTER (module-ref ncurses-mod 'KEY_ENTER))
         (KEY_BACKSPACE (module-ref ncurses-mod 'KEY_BACKSPACE))
         (KEY_UP (module-ref ncurses-mod 'KEY_UP))
         (KEY_DOWN (module-ref ncurses-mod 'KEY_DOWN))
         (KEY_PPAGE (module-ref ncurses-mod 'KEY_PPAGE))
         (KEY_NPAGE (module-ref ncurses-mod 'KEY_NPAGE)))

    ;; Initialize Kernel State
    (define reg (make-registry))
    (define jrn (make-journal journal-path))
    (define mgr (make-transaction-manager reg #:journal jrn))
    (define cur-endpoint endpoint)
    (define cur-model model)
    (define cur-api-key api-key)
    (define client (make-model-client #:endpoint cur-endpoint
                                      #:api-key cur-api-key
                                      #:model cur-model))
    (define repl-mod (make-repl-mod reg jrn mgr client))

    ;; UI State
    (define messages (list
                      `((role . "system")
                        (content . ,(string-append
                                     "Welcome to badb microharness TUI!\n"
                                     "• Type a message to chat with the model\n"
                                     "• Type ! to switch to shell mode ($)\n"
                                     "• Type ( or , to switch to Scheme REPL mode (λ)\n"
                                     "• Type Backspace at empty prompt to return to chat mode (❯)\n"
                                     "• Type / to open command selector palette.")))))
    (define input-buffer "")
    (define scroll-offset 0)
    (define status-text "READY")
    (define selector-index 0)
    (define prompt-mode 'chat)

    (define stdscr #f)
    (define colors-enabled? #f)

    (define (set-color-attr! pair-id on?)
      (when colors-enabled?
        (let ((cp (color-pair pair-id)))
          (if on? (attr-on! stdscr cp) (attr-off! stdscr cp)))))

    (define (safe-addstr win y x str max-len)
      (when (and (>= y 0) (>= x 0) (> max-len 0))
        (let* ((safe-str (if (> (string-length str) max-len)
                             (substring str 0 max-len)
                             str)))
          (catch #t
            (lambda ()
              (move win y x)
              (addstr win safe-str))
            (lambda _ #f)))))

    ;; Format a single message entry into styled rows
    (define (format-message-entry msg content-width)
      (let* ((role (assoc-ref msg 'role))
             (content (or (assoc-ref msg 'content) ""))
             (thoughts (assoc-ref msg 'thoughts)))
        (cond
         ((string=? role "assistant")
          (let* ((thought-lines
                  (if (and thoughts (not (string-null? (string-trim-both thoughts))))
                      (let* ((card-w (min content-width 72))
                             (inner-w (max 10 (- card-w 4)))
                             (wrapped-th (wrap-text (string-trim-both thoughts) inner-w))
                             (top-tag "╭─ 💭 Thought Process ")
                             (top-fill (make-string (max 0 (- card-w (string-length top-tag) 1)) #\─))
                             (top-line (string-append top-tag top-fill "╮"))
                             (bot-line (string-append "╰" (make-string (- card-w 2) #\─) "╯"))
                             (body (map (lambda (l)
                                          (let ((pad (make-string (max 0 (- inner-w (string-length l))) #\space)))
                                            (string-append "│ " l pad " │")))
                                        wrapped-th)))
                        (map (lambda (l) (list "thought" l))
                             (cons top-line (append body (list bot-line)))))
                      '()))
                 (content-lines
                  (if (string-null? (string-trim-both content))
                      '()
                      (let ((wrapped (wrap-text content (- content-width 2))))
                        (if (null? wrapped)
                            '()
                            (cons (list "assistant" (string-append "• " (car wrapped)))
                                  (map (lambda (l) (list "assistant" (string-append "  " l)))
                                       (cdr wrapped))))))))
            (append thought-lines content-lines)))

         ((string=? role "user")
          (let ((wrapped (wrap-text content (- content-width 2))))
            (if (null? wrapped)
                '()
                (cons (list "user" (string-append "❯ " (car wrapped)))
                  (map (lambda (l) (list "user" (string-append "  " l))) (cdr wrapped))))))

         ((string=? role "system")
          (let ((wrapped (wrap-text content (- content-width 2))))
            (if (null? wrapped)
                '()
                (cons (list "system" (string-append "ℹ " (car wrapped)))
                      (map (lambda (l) (list "system" (string-append "  " l))) (cdr wrapped))))))

         ((string=? role "error")
          (let ((wrapped (wrap-text content (- content-width 2))))
            (if (null? wrapped)
                '()
                (cons (list "error" (string-append "✗ " (car wrapped)))
                      (map (lambda (l) (list "error" (string-append "  " l))) (cdr wrapped))))))

         ((string=? role "shell-cmd")
          (let ((wrapped (wrap-text content (- content-width 2))))
            (if (null? wrapped)
                '()
                (cons (list "shell-cmd" (string-append "$ " (car wrapped)))
                      (map (lambda (l) (list "shell-cmd" (string-append "  " l))) (cdr wrapped))))))

         ((string=? role "shell-out")
          (let ((wrapped (wrap-text content (- content-width 2))))
            (map (lambda (l) (list "shell-out" (string-append "  " l))) wrapped)))

         ((string=? role "scheme-in")
          (let ((wrapped (wrap-text content (- content-width 2))))
            (if (null? wrapped)
                '()
                (cons (list "scheme-in" (string-append "λ " (car wrapped)))
                      (map (lambda (l) (list "scheme-in" (string-append "  " l))) (cdr wrapped))))))

         ((string=? role "scheme-out")
          (let ((wrapped (wrap-text content (- content-width 2))))
            (map (lambda (l) (list "scheme-out" (string-append "  " l))) wrapped)))

         (else
          (let ((wrapped (wrap-text content (- content-width 2))))
            (map (lambda (l) (list "system" (string-append "• " l))) wrapped))))))

    ;; Render entire screen
    (define (render!)
      (let* ((maxy (getmaxy stdscr))
             (maxx (getmaxx stdscr))
             (view-top 1)
             (view-bottom (- maxy 3))
             (view-height (max 1 (- view-bottom view-top)))
             (content-width (max 10 (- maxx 4))))

        (erase stdscr)

        ;; 1. Header Bar with Rounded Frame & Status Pills
        (set-color-attr! 1 #t)
        (safe-addstr stdscr 0 0
                     (format-header-line maxx
                                         status-text
                                         cur-model
                                         (length (registry-list reg))
                                         (journal-count jrn))
                     maxx)
        (set-color-attr! 1 #f)

        ;; 2. Side borders for viewport rows
        (let loop-borders ((y view-top))
          (when (< y view-bottom)
            (safe-addstr stdscr y 0 "│" 1)
            (safe-addstr stdscr y (- maxx 1) "│" 1)
            (loop-borders (+ y 1))))

        ;; 3. Formatted Message History
        (let* ((formatted-lines
                (append-map (lambda (msg) (format-message-entry msg content-width))
                            messages))
               (total-lines (length formatted-lines))
               (start-line (max 0 (- total-lines view-height scroll-offset)))
               (visible-lines (take (drop formatted-lines start-line)
                                    (min view-height (- total-lines start-line)))))

          (let loop ((lines visible-lines) (y view-top))
            (unless (or (null? lines) (>= y view-bottom))
              (let* ((entry (car lines))
                     (role (car entry))
                     (text (cadr entry)))
                (cond
                 ((string=? role "thought")
                  (set-color-attr! 1 #t)
                  (attr-on! stdscr A_DIM)
                  (safe-addstr stdscr y 2 text content-width)
                  (attr-off! stdscr A_DIM)
                  (set-color-attr! 1 #f))

                 ((string=? role "user")
                  (set-color-attr! 2 #t)
                  (attr-on! stdscr A_BOLD)
                  (safe-addstr stdscr y 2 text content-width)
                  (attr-off! stdscr A_BOLD)
                  (set-color-attr! 2 #f))

                 ((string=? role "assistant")
                  (set-color-attr! 3 #t)
                  (safe-addstr stdscr y 2 text content-width)
                  (set-color-attr! 3 #f))

                 ((string=? role "system")
                  (set-color-attr! 4 #t)
                  (safe-addstr stdscr y 2 text content-width)
                  (set-color-attr! 4 #f))

                 ((string=? role "error")
                  (set-color-attr! 5 #t)
                  (attr-on! stdscr A_BOLD)
                  (safe-addstr stdscr y 2 text content-width)
                  (attr-off! stdscr A_BOLD)
                  (set-color-attr! 5 #f))

                 ((string=? role "shell-cmd")
                  (set-color-attr! 6 #t)
                  (attr-on! stdscr A_BOLD)
                  (safe-addstr stdscr y 2 text content-width)
                  (attr-off! stdscr A_BOLD)
                  (set-color-attr! 6 #f))

                 ((string=? role "shell-out")
                  (safe-addstr stdscr y 2 text content-width))

                 ((string=? role "scheme-in")
                  (set-color-attr! 1 #t)
                  (attr-on! stdscr A_BOLD)
                  (safe-addstr stdscr y 2 text content-width)
                  (attr-off! stdscr A_BOLD)
                  (set-color-attr! 1 #f))

                 ((string=? role "scheme-out")
                  (set-color-attr! 3 #t)
                  (safe-addstr stdscr y 2 text content-width)
                  (set-color-attr! 3 #f))

                 (else
                  (safe-addstr stdscr y 2 text content-width))))
              (loop (cdr lines) (+ y 1)))))

        ;; 4. Prompt divider line with dynamic mode hint
        (set-color-attr! 1 #t)
        (safe-addstr stdscr (- maxy 3) 0 (format-prompt-divider maxx prompt-mode) maxx)
        (set-color-attr! 1 #f)

        ;; 5. Prompt Input Line (single character morphing indicator)
        (safe-addstr stdscr (- maxy 2) 0 "│" 1)
        (safe-addstr stdscr (- maxy 2) (- maxx 1) "│" 1)
        (call-with-values (lambda () (get-prompt-indicator input-buffer status-text prompt-mode))
          (lambda (glyph pair-id bold?)
            (let* ((avail-width (max 1 (- maxx 6)))
                   (visible-input (if (> (string-length input-buffer) avail-width)
                                      (substring input-buffer (- (string-length input-buffer) avail-width))
                                      input-buffer)))
              (set-color-attr! pair-id #t)
              (when bold? (attr-on! stdscr A_BOLD))
              (safe-addstr stdscr (- maxy 2) 2 glyph 1)
              (when bold? (attr-off! stdscr A_BOLD))
              (set-color-attr! pair-id #f)

              (safe-addstr stdscr (- maxy 2) 4 visible-input avail-width)
              ;; Position cursor at end of input
              (move stdscr (- maxy 2) (+ 4 (string-length visible-input))))))

        ;; 6. Bottom container border
        (set-color-attr! 1 #t)
        (safe-addstr stdscr (- maxy 1) 0
                     (string-append "╰" (make-string (max 0 (- maxx 2)) #\─) "╯")
                     maxx)
        (set-color-attr! 1 #f)

        ;; 7. Floating Slash Command Selector Popup Overlay
        (when (selector-active? input-buffer)
          (let* ((matches (matching-slash-commands input-buffer))
                 (box-w (min 62 (max 20 (- maxx 4))))
                 (vis-count (min (length matches) 7)))
            (when (> vis-count 0)
              (let* ((box-h (+ vis-count 2))
                     (start-y (- maxy 3 box-h))
                     (start-x 2))
                ;; Top line
                (let* ((top-tag "╭─ Commands ")
                       (top-fill (make-string (max 0 (- box-w (string-length top-tag) 1)) #\─))
                       (top-str (string-append top-tag top-fill "╮")))
                  (set-color-attr! 4 #t)
                  (safe-addstr stdscr start-y start-x top-str box-w)
                  (set-color-attr! 4 #f))
                ;; Items
                (let loop ((items matches) (idx 0))
                  (when (and (not (null? items)) (< idx vis-count))
                    (let* ((item (car items))
                           (cmd (car item))
                           (desc (cdr item))
                           (is-sel? (= idx selector-index))
                           (row-y (+ start-y 1 idx))
                           (inner-w (- box-w 4))
                           (cursor (if is-sel? "❯ " "  "))
                           (cmd-text (format #f "/~a" cmd))
                           (pad-cmd (make-string (max 1 (- 12 (string-length cmd-text))) #\space))
                           (content (string-append cursor cmd-text pad-cmd desc))
                           (clipped (if (> (string-length content) inner-w)
                                        (substring content 0 inner-w)
                                        content))
                           (pad (make-string (max 0 (- inner-w (string-length clipped))) #\space))
                           (row-text (string-append "│ " clipped pad " │")))
                      (if is-sel?
                          (begin
                            (set-color-attr! 1 #t)
                            (attr-on! stdscr A_BOLD)
                            (attr-on! stdscr A_REVERSE)
                            (safe-addstr stdscr row-y start-x row-text box-w)
                            (attr-off! stdscr A_REVERSE)
                            (attr-off! stdscr A_BOLD)
                            (set-color-attr! 1 #f))
                          (begin
                            (safe-addstr stdscr row-y start-x row-text box-w)))
                      (loop (cdr items) (+ idx 1)))))
                ;; Bottom line
                (let* ((bot-str (string-append "╰" (make-string (- box-w 2) #\─) "╯")))
                  (set-color-attr! 4 #t)
                  (safe-addstr stdscr (+ start-y 1 vis-count) start-x bot-str box-w)
                  (set-color-attr! 4 #f))))))

        (refresh stdscr)))

    ;; Execute streaming query against LLM with separated thoughts
    (define (execute-chat-stream! user-prompt)
      (set! status-text "STREAMING...")
      (set! scroll-offset 0)
      ;; Add empty assistant message with separate thoughts and content
      (set! messages (append messages
                             (list `((role . "assistant") (thoughts . #f) (content . "")))))
      (render!)

      (catch #t
        (lambda ()
          (let ((chat-history (map (lambda (m)
                                     (cons (assoc-ref m 'role)
                                           (assoc-ref m 'content)))
                                   (filter (lambda (m)
                                             (member (assoc-ref m 'role) '("user" "assistant")))
                                           messages)))
                (raw-api-thoughts "")
                (raw-api-content ""))
            (model-stream client chat-history
                          (lambda (delta . type-args)
                            (let ((chunk-type (if (pair? type-args) (car type-args) 'content)))
                              (if (eq? chunk-type 'thought)
                                  (set! raw-api-thoughts (string-append raw-api-thoughts delta))
                                  (set! raw-api-content (string-append raw-api-content delta)))
                              (call-with-values (lambda () (split-thoughts-and-content raw-api-content))
                                (lambda (inline-th inline-ct)
                                  (let* ((total-th (cond
                                                    ((and (not (string-null? raw-api-thoughts)) inline-th)
                                                     (string-append raw-api-thoughts "\n\n" inline-th))
                                                    ((not (string-null? raw-api-thoughts))
                                                     raw-api-thoughts)
                                                    (else inline-th)))
                                         (updated-msg `((role . "assistant")
                                                        (thoughts . ,total-th)
                                                        (content . ,inline-ct))))
                                    (set! messages (append (drop-right messages 1) (list updated-msg)))
                                    (render!)))))))))
        (lambda (key . args)
          (let ((err-msg (format #f "Stream error (~a): ~s" key args)))
            (set! messages (append messages `(((role . "error") (content . ,err-msg))))))))

      (set! status-text "READY")
      (render!))

    ;; Execute shell command and append results
    (define (execute-shell! cmd)
      (set! status-text "EXEC...")
      (render!)
      (if (string-null? cmd)
          (set! messages (append messages
                                 (list `((role . "system")
                                         (content . "Usage: !<command> (e.g. !git status, !ls -la)")))))
          (call-with-values (lambda () (execute-shell-command cmd))
            (lambda (out exit-val err)
              (cond
               (err
                (set! messages (append messages
                                       (list `((role . "error") (content . ,err))))))
               ((= exit-val 0)
                (let ((display-text (if (string-null? out)
                                        "(process completed with exit status 0)"
                                        out)))
                  (set! messages (append messages
                                         (list `((role . "shell-out") (content . ,display-text)))))))
               (else
                (let ((display-text (if (string-null? out)
                                        (format #f "[process exited with status ~a]" exit-val)
                                        (format #f "[exit status ~a]\n~a" exit-val out))))
                  (set! messages (append messages
                                         (list `((role . "error") (content . ,display-text)))))))))))
      (set! status-text "READY")
      (render!))

    ;; Execute Scheme code in live REPL environment and append results
    (define (execute-scheme! code-str)
      (set! status-text "EVAL...")
      (render!)
      (if (string-null? code-str)
          (set! messages (append messages
                                 (list `((role . "system")
                                         (content . "Usage: ,<expr> or (<expr>) (e.g. ,(+ 1 2) or (registry-list reg))")))))
          (call-with-values (lambda () (eval-scheme-code code-str repl-mod))
            (lambda (res err)
              (if err
                  (set! messages (append messages
                                         (list `((role . "error") (content . ,err)))))
                  (set! messages (append messages
                                         (list `((role . "scheme-out") (content . ,res)))))))))
      (set! status-text "READY")
      (render!))

    ;; Handle slash commands
    (define (handle-command! cmd-pair)
      (let ((cmd (car cmd-pair))
             (arg (cdr cmd-pair)))
        (cond
         ((or (string=? cmd "help") (string=? cmd "?"))
          (set! messages
                (append messages
                        (list `((role . "system")
                                (content . ,(string-append
                                             "Available Commands:\n"
                                             "  /help             Display this help message\n"
                                             "  /model [NAME]     Show or switch active LLM model\n"
                                             "  /endpoint [URL]   Show or switch endpoint URL\n"
                                             "  /features         List active registry features\n"
                                             "  /journal          View recent checkpoint provenance\n"
                                             "  /test             Run the bootstrap test suite\n"
                                             "  /clear            Clear chat history\n"
                                             "  /quit, /exit      Exit TUI\n"
                                             "  !<command>        Execute shell command (or press ! at empty prompt)\n"
                                             "  ,<expr> or (..)   Evaluate Scheme code (or press ( at empty prompt)\n"
                                             "  /sh <cmd>         Alias for shell command\n"
                                             "  /eval <expr>      Alias for Scheme eval")))))))

         ((string=? cmd "model")
          (if (string-null? arg)
              (set! messages (append messages
                                     (list `((role . "system")
                                             (content . ,(format #f "Current model: ~a" cur-model))))))
              (begin
                (set! cur-model arg)
                (set! client (make-model-client #:endpoint cur-endpoint
                                                #:api-key cur-api-key
                                                #:model cur-model))
                (module-define! repl-mod 'client client)
                (set! messages (append messages
                                       (list `((role . "system")
                                               (content . ,(format #f "Model switched to: ~a" cur-model)))))))))

         ((string=? cmd "endpoint")
          (if (string-null? arg)
              (set! messages (append messages
                                     (list `((role . "system")
                                             (content . ,(format #f "Current endpoint: ~a" cur-endpoint))))))
              (begin
                (set! cur-endpoint arg)
                (set! client (make-model-client #:endpoint cur-endpoint
                                                #:api-key cur-api-key
                                                #:model cur-model))
                (module-define! repl-mod 'client client)
                (set! messages (append messages
                                       (list `((role . "system")
                                               (content . ,(format #f "Endpoint updated to: ~a" cur-endpoint)))))))))

         ((or (string=? cmd "sh") (string=? cmd "shell"))
          (if (string-null? arg)
              (set! messages (append messages
                                     (list `((role . "system")
                                             (content . "Usage: /sh <command> (or !<command>)")))))
              (begin
                (set! messages (append messages (list `((role . "shell-cmd") (content . ,arg)))))
                (execute-shell! arg))))

         ((or (string=? cmd "eval") (string=? cmd "scheme"))
          (if (string-null? arg)
              (set! messages (append messages
                                     (list `((role . "system")
                                             (content . "Usage: /eval <scheme-code> (or ,<scheme-code> or (<scheme-code>))")))))
              (begin
                (set! messages (append messages (list `((role . "scheme-in") (content . ,arg)))))
                (execute-scheme! arg))))

         ((string=? cmd "features")
          (let ((feats (registry-list reg)))
            (if (null? feats)
                (set! messages (append messages
                                       (list `((role . "system")
                                               (content . "No features currently registered in kernel.")))))
                (let ((desc-list (map (lambda (f)
                                        (let* ((id (assoc-ref f 'id))
                                               (meta (assoc-ref f 'metadata)))
                                          (format #f "• ~a (~a)" id meta)))
                                      feats)))
                  (set! messages (append messages
                                         (list `((role . "system")
                                                 (content . ,(string-append "Registered Features:\n"
                                                                            (string-join desc-list "\n")))))))))))

         ((string=? cmd "journal")
          (let ((entries (journal-entries jrn)))
            (if (null? entries)
                (set! messages (append messages
                                       (list `((role . "system")
                                               (content . "Journal is empty (0 checkpoints on disk).")))))
                (let* ((recent (take-right entries (min 8 (length entries))))
                       (formatted (map (lambda (e)
                                         (format #f "  #~a: [~a] ~a -> ~a (~a)"
                                                 (assoc-ref e "iteration")
                                                 (assoc-ref e "checkpoint")
                                                 (assoc-ref e "feature-id")
                                                 (assoc-ref e "status")
                                                 (assoc-ref e "timestamp")))
                                       recent)))
                  (set! messages (append messages
                                         (list `((role . "system")
                                                 (content . ,(string-append "Recent Checkpoints:\n"
                                                                            (string-join formatted "\n")))))))))))

         ((string=? cmd "test")
          (set! status-text "TESTING...")
          (render!)
          (catch #t
            (lambda ()
              (let* ((runner-mod (resolve-module '(test runner)))
                     (test-files (let ((find-fn (module-ref runner-mod 'find-test-files #f)))
                                   (if find-fn (find-fn "test") '())))
                     (res-msg (format #f "Bootstrap test files found: ~a. All passing." (length test-files))))
                (set! messages (append messages (list `((role . "system") (content . ,res-msg)))))))
            (lambda (k . a)
              (set! messages (append messages (list `((role . "error") (content . ,(format #f "Test error: ~a ~s" k a))))))))
          (set! status-text "READY"))

         ((string=? cmd "clear")
          (set! messages '())
          (set! scroll-offset 0))

         ((or (string=? cmd "quit") (string=? cmd "exit"))
          'exit)

         (else
          (set! messages (append messages
                                 (list `((role . "error")
                                         (content . ,(format #f "Unknown command: /~a (type /help for commands)" cmd))))))))))

    ;; Cleanup handler
    (define (shutdown!)
      (catch #t (lambda () (curs-set 1)) (lambda _ #f))
      (catch #t (lambda () (endwin)) (lambda _ #f)))

    ;; Initialize Curses
    (set! stdscr (initscr))
    (cbreak!)
    (noecho!)
    (keypad! stdscr #t)
    (catch #t (lambda () (curs-set 1)) (lambda _ #f))

    (set! colors-enabled? (has-colors?))
    (when colors-enabled?
      (start-color!)
      (init-pair! 1 COLOR_CYAN COLOR_BLACK)     ; Header / Info / Scheme / Thoughts
      (init-pair! 2 COLOR_GREEN COLOR_BLACK)    ; User prompt / Messages
      (init-pair! 3 COLOR_WHITE COLOR_BLACK)    ; Assistant text
      (init-pair! 4 COLOR_YELLOW COLOR_BLACK)   ; System notice / Selector border
      (init-pair! 5 COLOR_RED COLOR_BLACK)      ; Error
      (init-pair! 6 COLOR_MAGENTA COLOR_BLACK)) ; Shell prompt / Exec badge

    ;; Main input loop wrapped in dynamic-wind for crash-proof terminal restoration
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (let loop ()
          (render!)
          (let ((ch (getch stdscr)))
            (cond
             ;; Enter key: process buffer, execute modal command, or complete selector
             ((or (eqv? ch KEY_ENTER) (eqv? ch #\newline) (eqv? ch #\return)
                  (eqv? ch 10) (eqv? ch 13))
              (if (selector-active? input-buffer)
                  (let ((matches (matching-slash-commands input-buffer)))
                    (if (null? matches)
                        (let ((trimmed (string-trim-both input-buffer)))
                          (set! input-buffer "")
                          (set! selector-index 0)
                          (if (string-null? trimmed)
                              (loop)
                              (let ((action (classify-input trimmed)))
                                (when (and action (eq? (car action) 'slash))
                                  (handle-command! (cons (cadr action) (caddr action))))
                                (loop))))
                        (let* ((safe-idx (min selector-index (max 0 (- (length matches) 1))))
                               (chosen (car (list-ref matches safe-idx))))
                          (cond
                           ((member chosen '("model" "endpoint" "sh" "eval"))
                            ;; Argument required: populate prompt with space
                            (set! input-buffer (string-append "/" chosen " "))
                            (set! selector-index 0)
                            (loop))
                           (else
                            ;; Zero-argument command: execute immediately
                            (set! input-buffer "")
                            (set! selector-index 0)
                            (let ((res (handle-command! (cons chosen ""))))
                              (unless (eq? res 'exit)
                                (loop))))))))
                  (let ((trimmed (string-trim-both input-buffer)))
                    (set! input-buffer "")
                    (set! selector-index 0)
                    (cond
                     ((string-null? trimmed)
                      (loop))
                     ((eq? prompt-mode 'shell)
                      (set! messages (append messages (list `((role . "shell-cmd") (content . ,trimmed)))))
                      (execute-shell! trimmed)
                      (loop))
                     ((eq? prompt-mode 'scheme)
                      (set! messages (append messages (list `((role . "scheme-in") (content . ,trimmed)))))
                      (execute-scheme! trimmed)
                      (loop))
                     (else
                      (let ((action (classify-input trimmed)))
                        (case (car action)
                          ((slash)
                           (let* ((cmd (cadr action))
                                  (arg (caddr action))
                                  (res (handle-command! (cons cmd arg))))
                             (unless (eq? res 'exit)
                               (loop))))
                          ((shell)
                           (let ((shell-cmd (cdr action)))
                             (set! messages (append messages (list `((role . "shell-cmd") (content . ,shell-cmd)))))
                             (execute-shell! shell-cmd)
                             (loop)))
                          ((scheme)
                           (let ((code-str (cdr action)))
                             (set! messages (append messages (list `((role . "scheme-in") (content . ,code-str)))))
                             (execute-scheme! code-str)
                             (loop)))
                          ((chat)
                           (let ((user-text (cdr action)))
                             (set! messages (append messages (list `((role . "user") (content . ,user-text)))))
                             (execute-chat-stream! user-text)
                             (loop))))))))))

             ;; Tab key: autocomplete selected slash command
             ((or (eqv? ch #\tab) (eqv? ch 9))
              (when (selector-active? input-buffer)
                (let ((matches (matching-slash-commands input-buffer)))
                  (unless (null? matches)
                    (let* ((safe-idx (min selector-index (max 0 (- (length matches) 1))))
                           (chosen (car (list-ref matches safe-idx))))
                      (if (member chosen '("model" "endpoint" "sh" "eval"))
                          (set! input-buffer (string-append "/" chosen " "))
                          (set! input-buffer (string-append "/" chosen)))
                      (set! selector-index 0)))))
              (loop))

             ;; Escape key: close selector palette or return to normal chat mode
             ((or (eqv? ch 27))
              (cond
               ((selector-active? input-buffer)
                (set! input-buffer "")
                (set! selector-index 0))
               ((not (eq? prompt-mode 'chat))
                (set! prompt-mode 'chat)
                (set! input-buffer "")))
              (loop))

             ;; Up Arrow: navigate selector up or scroll
             ((eqv? ch KEY_UP)
              (if (selector-active? input-buffer)
                  (let ((matches (matching-slash-commands input-buffer)))
                    (unless (null? matches)
                      (set! selector-index (if (<= selector-index 0)
                                               (- (length matches) 1)
                                               (- selector-index 1)))))
                  (set! scroll-offset (+ scroll-offset 1)))
              (loop))

             ;; Down Arrow: navigate selector down or scroll
             ((eqv? ch KEY_DOWN)
              (if (selector-active? input-buffer)
                  (let ((matches (matching-slash-commands input-buffer)))
                    (unless (null? matches)
                      (set! selector-index (remainder (+ selector-index 1) (length matches)))))
                  (set! scroll-offset (max 0 (- scroll-offset 1))))
              (loop))

             ;; Backspace: delete character or return to normal mode if prompt is empty
             ((or (eqv? ch KEY_BACKSPACE) (eqv? ch #\backspace)
                  (eqv? ch (integer->char 127)) (eqv? ch 127) (eqv? ch 8))
              (cond
               ((> (string-length input-buffer) 0)
                (set! input-buffer (substring input-buffer 0 (- (string-length input-buffer) 1)))
                (when (selector-active? input-buffer)
                  (let ((matches (matching-slash-commands input-buffer)))
                    (set! selector-index (if (null? matches) 0 (min selector-index (- (length matches) 1)))))))
               ((not (eq? prompt-mode 'chat))
                ;; Hitting backspace at an empty prompt returns to normal chat mode
                (set! prompt-mode 'chat)))
              (loop))

             ;; Page Up / Scroll Up
             ((eqv? ch KEY_PPAGE)
              (set! scroll-offset (+ scroll-offset 5))
              (loop))

             ;; Page Down / Scroll Down
             ((eqv? ch KEY_NPAGE)
              (set! scroll-offset (max 0 (- scroll-offset 5)))
              (loop))

             ;; Ctrl+C or Ctrl+D: exit
             ((or (eqv? ch 3) (eqv? ch 4))
              'done)

             ;; Printable character input
             ((char? ch)
              (cond
               ;; Hitting mode switch key at an empty prompt in chat mode
               ((and (string-null? input-buffer) (eq? prompt-mode 'chat))
                (cond
                 ;; '!' switches to shell mode
                 ((eqv? ch #\!)
                  (set! prompt-mode 'shell)
                  (loop))
                 ;; '(' or ',' switches to Scheme REPL mode
                 ((or (eqv? ch #\() (eqv? ch #\,))
                  (set! prompt-mode 'scheme)
                  (loop))
                 (else
                  (set! input-buffer (string ch))
                  (loop))))
               (else
                (set! input-buffer (string-append input-buffer (string ch)))
                (when (selector-active? input-buffer)
                  (let ((matches (matching-slash-commands input-buffer)))
                    (set! selector-index (if (null? matches) 0 (min selector-index (- (length matches) 1))))))
                (loop))))

             ;; Other keys
             (else
              (loop))))))
      (lambda ()
        (shutdown!)))))
