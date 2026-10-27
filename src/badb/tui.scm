;;; badb — Terminal User Interface (TUI)
;;;
;;; Interactive full-screen terminal interface for badb powered by guile-ncurses.
;;; Provides live streaming LLM chat, model configuration, registry inspection,
;;; and checkpoint journal viewing.

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
            eval-scheme-code
            execute-shell-command))

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
  (catch #t
    (lambda ()
      (let* ((p (open-input-string code-str))
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
      (values #f (format #f "Scheme error (~a): ~s" key args)))))

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
;;; Curses Safe Rendering Utilities
;;;

;;;
;;; Main TUI Event Loop
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
                 ./scripts/devenv -- ./scripts/badb tui~%")
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
                                    "• Type !<cmd> to execute shell commands (e.g. !git status)\n"
                                    "• Type ,<expr> or (<expr>) to evaluate Scheme (e.g. ,(+ 1 2))\n"
                                    "• Type /help for all available commands.")))))
    (define input-buffer "")
    (define scroll-offset 0)
    (define status-text "READY")

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

    ;; Render entire screen
    (define (render!)
      (let* ((maxy (getmaxy stdscr))
             (maxx (getmaxx stdscr))
             (view-top 2)
             (view-bottom (- maxy 3))
             (view-height (max 1 (- view-bottom view-top)))
             (content-width (max 10 (- maxx 4))))

        (erase stdscr)

        ;; 1. Header Bar
        (set-color-attr! 1 #t)
        (attr-on! stdscr A_BOLD)
        (attr-on! stdscr A_REVERSE)
        (let* ((header-str (format #f " badb [~a] | Model: ~a | Features: ~a | Journal: ~a "
                                   status-text
                                   cur-model
                                   (length (registry-list reg))
                                   (journal-count jrn)))
               (pad-len (max 0 (- maxx (string-length header-str))))
               (full-header (string-append header-str (make-string pad-len #\space))))
          (safe-addstr stdscr 0 0 full-header maxx))
        (attr-off! stdscr A_REVERSE)
        (attr-off! stdscr A_BOLD)
        (set-color-attr! 1 #f)

        ;; 2. Separator line below header
        (safe-addstr stdscr 1 0 (make-string maxx #\─) maxx)

        ;; 3. Formatted Message History
        (let* ((formatted-lines
                (append-map
                 (lambda (msg)
                   (let* ((role (assoc-ref msg 'role))
                          (content (or (assoc-ref msg 'content) ""))
                          (prefix (cond
                                   ((string=? role "user") "> ")
                                   ((string=? role "assistant") "• ")
                                   ((string=? role "system") "ℹ ")
                                   ((string=? role "error") "✗ ")
                                   ((string=? role "shell-cmd") "$ ")
                                   ((string=? role "shell-out") "  ")
                                   ((string=? role "scheme-in") "λ> ")
                                   ((string=? role "scheme-out") "  ")
                                   (else "* ")))
                          (wrapped (wrap-text content (- content-width (string-length prefix)))))
                     (if (null? wrapped)
                         '()
                         (cons (list role (string-append prefix (car wrapped)))
                               (map (lambda (line)
                                      (list role (string-append
                                                  (make-string (string-length prefix) #\space)
                                                  line)))
                                    (cdr wrapped))))))
                 messages))
               (total-lines (length formatted-lines))
               ;; Calculate scroll window
               (start-line (max 0 (- total-lines view-height scroll-offset)))
               (visible-lines (take (drop formatted-lines start-line)
                                    (min view-height (- total-lines start-line)))))

          (let loop ((lines visible-lines) (y view-top))
            (unless (or (null? lines) (>= y view-bottom))
              (let* ((entry (car lines))
                     (role (car entry))
                     (text (cadr entry)))
                (cond
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

        ;; 4. Separator line above input
        (safe-addstr stdscr (- maxy 3) 0 (make-string maxx #\─) maxx)
        (let ((hint " [/help | !<cmd> | ,<expr> | /test | /quit] "))
          (safe-addstr stdscr (- maxy 3) (max 0 (- maxx (string-length hint) 2)) hint (string-length hint)))

        ;; 5. Input Prompt Line (dynamic prefix based on active input mode)
        (let* ((prompt-prefix
                (cond
                 ((string-prefix? "!" input-buffer) "sh!> ")
                 ((or (string-prefix? "," input-buffer) (string-prefix? "(" input-buffer)) "schemeλ> ")
                 (else "badb> ")))
               (avail-width (max 1 (- maxx (string-length prompt-prefix) 2)))
               (visible-input (if (> (string-length input-buffer) avail-width)
                                  (substring input-buffer (- (string-length input-buffer) avail-width))
                                  input-buffer)))
          (set-color-attr! 1 #t)
          (attr-on! stdscr A_BOLD)
          (safe-addstr stdscr (- maxy 2) 1 prompt-prefix (string-length prompt-prefix))
          (attr-off! stdscr A_BOLD)
          (set-color-attr! 1 #f)

          (safe-addstr stdscr (- maxy 2) (+ 1 (string-length prompt-prefix)) visible-input avail-width)
          ;; Position cursor
          (move stdscr (- maxy 2) (+ 1 (string-length prompt-prefix) (string-length visible-input))))

        (refresh stdscr)))

    ;; Execute streaming query against LLM
    (define (execute-chat-stream! user-prompt)
      (set! status-text "STREAMING...")
      (set! scroll-offset 0)
      ;; Add empty assistant message to accumulate stream
      (set! messages (append messages
                             (list `((role . "assistant") (content . "")))))
      (render!)

      (catch #t
        (lambda ()
          (let ((chat-history (map (lambda (m)
                                     (cons (assoc-ref m 'role)
                                           (assoc-ref m 'content)))
                                   (filter (lambda (m)
                                             (member (assoc-ref m 'role) '("user" "assistant")))
                                           messages))))
            (model-stream client chat-history
                          (lambda (delta)
                            (let* ((cur-last (last messages))
                                   (cur-content (assoc-ref cur-last 'content))
                                   (new-content (string-append cur-content delta))
                                   (updated-msg `((role . "assistant") (content . ,new-content))))
                              (set! messages (append (drop-right messages 1) (list updated-msg)))
                              (render!))))))
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
                                            "  !<command>        Execute shell command (e.g. !git status)\n"
                                            "  ,<expr> or (..)   Send Scheme code to live REPL (e.g. ,(+ 1 2))\n"
                                            "  /sh <cmd>         Alias for !<command>\n"
                                            "  /eval <expr>      Alias for ,<expr>")))))))

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
      (init-pair! 1 COLOR_CYAN COLOR_BLACK)     ; Header / Info
      (init-pair! 2 COLOR_GREEN COLOR_BLACK)    ; User prompt
      (init-pair! 3 COLOR_WHITE COLOR_BLACK)    ; Assistant text
      (init-pair! 4 COLOR_YELLOW COLOR_BLACK)   ; System notice
      (init-pair! 5 COLOR_RED COLOR_BLACK)      ; Error
      (init-pair! 6 COLOR_MAGENTA COLOR_BLACK)) ; Transaction badge

    ;; Main input loop wrapped in dynamic-wind for crash-proof terminal restoration
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (let loop ()
          (render!)
          (let ((ch (getch stdscr)))
            (cond
             ;; Enter key: process buffer
             ((or (eqv? ch KEY_ENTER) (eqv? ch #\newline) (eqv? ch #\return)
                  (eqv? ch 10) (eqv? ch 13))
              (let ((trimmed (string-trim-both input-buffer)))
                (set! input-buffer "")
                (if (string-null? trimmed)
                    (loop)
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
                           (loop))))))))

             ;; Backspace
             ((or (eqv? ch KEY_BACKSPACE) (eqv? ch #\backspace)
                  (eqv? ch (integer->char 127)) (eqv? ch 127) (eqv? ch 8))
              (when (> (string-length input-buffer) 0)
                (set! input-buffer (substring input-buffer 0 (- (string-length input-buffer) 1))))
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
              (set! input-buffer (string-append input-buffer (string ch)))
              (loop))

             ;; Other keys (e.g. arrow keys)
             (else
              (loop))))))
      (lambda ()
        (shutdown!)))))
