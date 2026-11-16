;;; test/test-ui-wp3.scm — SRFI-64 tests for WP3 (editor.scm & session.scm)
;;;
;;; Covers:
;;; - ui-wp3.1: '/' slash prompt mode absorbing first char & backspace/escape
;;; - ui-wp3.2: Ctrl+R fuzzy history search & @ workspace mention completion
;;; - ui-wp3.3: <keymap> table, Ctrl+I vs Tab resolution, rebindable actions, menu items
;;; - ui-wp3.4: $SHELL passthrough, safe sudo prompting & secret redaction, $EDITOR jump
;;; - ui-wp3.5: session-turns->transcript-messages, session metadata, REPL introspection

(define-module (test test-ui-wp3)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-64)
               #:use-module (ice-9 ftw)
               #:use-module (badb kernel registry)
               #:use-module (badb kernel journal)
               #:use-module (badb kernel transaction)
               #:use-module (geasa foundation-config plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa capabilities-mcp plugin)
               #:use-module (geasa policies-guardrail plugin)
               #:use-module (geasa providers-model common)
               #:use-module (geasa surfaces-tui editor)
               #:use-module (geasa surfaces-tui session))

(define %standalone?
        (and (pair? (command-line))
             (string-suffix? "test-ui-wp3.scm" (car (command-line)))))

(define %wp3-runner
        (if %standalone?
          (let ((r (test-runner-simple)))
            (test-runner-current r)
            r)
          (test-runner-current)))

(define (rm-rf path)
        (when (file-exists? path)
          (if (file-is-directory? path)
            (begin
              (for-each (lambda (file)
                          (unless (member file '("." ".."))
                            (rm-rf (string-append path "/" file))))
                        (or (scandir path) '()))
              (rmdir path))
            (delete-file path))))

(test-begin "ui-wp3")

;;;
;;; ui-wp3.1: Custom / Slash Prompt Mode
;;;
(test-group "wp3.1-slash-prompt-mode"
            ;; Typing / on empty chat enters 'slash mode absorbing the leading '/'
            (call-with-values (lambda () (prompt-mode-on-char 'chat "" 0 #\/))
                              (lambda (mode buf pos absorbed?)
                                (test-equal "slash char enters 'slash mode" 'slash mode)
                                (test-equal "slash char absorbed leaving empty buffer" "" buf)
                                (test-equal "cursor stays at 0" 0 pos)
                                (test-assert "absorbed? is #t" absorbed?)))

            ;; Typing ! on empty chat enters 'shell mode absorbing '!'
            (call-with-values (lambda () (prompt-mode-on-char 'chat "" 0 #\!))
                              (lambda (mode buf pos absorbed?)
                                (test-equal "bang enters 'shell mode" 'shell mode)
                                (test-equal "bang absorbed" "" buf)
                                (test-equal "cursor at 0" 0 pos)
                                (test-assert "absorbed? is #t" absorbed?)))

            ;; Typing ( on empty chat enters 'scheme mode keeping '('
            (call-with-values (lambda () (prompt-mode-on-char 'chat "" 0 #\())
                              (lambda (mode buf pos absorbed?)
                                (test-equal "paren enters 'scheme mode" 'scheme mode)
                                (test-equal "paren kept in buffer" "(" buf)
                                (test-equal "cursor at 1" 1 pos)
                                (test-assert "absorbed? is #f" (not absorbed?))))

            ;; Typing / mid-sentence stays in 'chat
            (call-with-values (lambda () (prompt-mode-on-char 'chat "path" 4 #\/))
                              (lambda (mode buf pos absorbed?)
                                (test-equal "mid-text slash stays in 'chat" 'chat mode)
                                (test-equal "mid-text slash inserted" "path/" buf)
                                (test-equal "cursor advances to 5" 5 pos)
                                (test-assert "not absorbed" (not absorbed?))))

            ;; Backspace on empty 'slash returns to 'chat; on non-empty deletes char
            (call-with-values (lambda () (prompt-mode-on-backspace 'slash "m" 1))
                              (lambda (mode buf pos)
                                (test-equal "non-empty slash stays 'slash" 'slash mode)
                                (test-equal "deletes character" "" buf)
                                (test-equal "cursor returns to 0" 0 pos)))
            (call-with-values (lambda () (prompt-mode-on-backspace 'slash "" 0))
                              (lambda (mode buf pos)
                                (test-equal "empty slash backspace returns to 'chat" 'chat mode)
                                (test-equal "buffer remains empty" "" buf)
                                (test-equal "cursor at 0" 0 pos)))

            ;; Escape exits 'slash mode cleanly
            (call-with-values (lambda () (prompt-mode-on-escape 'slash "mod" 3))
                              (lambda (mode buf pos)
                                (test-equal "escape exits 'slash to 'chat" 'chat mode)
                                (test-equal "escape clears buffer" "" buf)
                                (test-equal "cursor resets to 0" 0 pos)))

            ;; selector-active?, parse-command, matching-slash-commands, submit text
            (test-assert "selector active in 'slash mode with empty buffer"
                         (selector-active? "" 'slash))
            (test-assert "selector active in 'slash mode with partial command"
                         (selector-active? "mod" 'slash))
            (test-assert "selector inactive in 'slash mode after space"
                         (not (selector-active? "model " 'slash)))
            (test-assert "backward compat: selector active on '/mod' in 'chat"
                         (selector-active? "/mod"))
            (test-equal "parse-command in 'slash mode without leading slash"
                        '("model" . "llama3")
                        (parse-command "model llama3" 'slash))
            (test-equal "prompt-mode-submit-text prepends / in 'slash mode"
                        "/help"
                        (prompt-mode-submit-text 'slash "help"))
            (test-equal "format-slash-completion omits / in 'slash mode"
                        "model "
                        (format-slash-completion "model" 'slash #t)))

;;;
;;; ui-wp3.2: Fuzzy History Search (Ctrl+R) & @ Mention Completion
;;;
(test-group "wp3.2-fuzzy-history-and-at-completion"
            ;; fuzzy-score ranking
            (test-assert "exact match beats substring match"
                         (> (fuzzy-score "git status" "git status")
                            (fuzzy-score "git status" "run git status --short")))
            (test-assert "non-matching returns #f"
                         (not (fuzzy-score "xyz" "hello world")))

            ;; fuzzy-match-history deduplicates and respects recency/score
            (let ((hist (make-history '("git status" "ls -la" "git diff" "git status" "help me"))))
              (let ((all (fuzzy-match-history hist "")))
                (test-equal "empty query returns unique entries newest-first"
                            '("help me" "git status" "git diff" "ls -la")
                            all))
              (let ((gits (fuzzy-match-history hist "gdiff")))
                (test-assert "fuzzy subsequence 'gdiff' matches 'git diff'"
                             (and (pair? gits) (string=? (car gits) "git diff")))))

            ;; <history-search> interactive state transitions
            (let* ((hist (make-history '("alpha" "beta test" "gamma test")))
                   (s0 (history-search-open hist "draft" 5)))
              (test-assert "opened search is active" (history-search-active? s0))
              (test-equal "opened search captures saved buffer" "draft" (history-search-saved-buffer s0))
              (let ((s1 (history-search-update s0 hist "test")))
                (test-equal "filtered matches count is 2" 2 (length (history-search-matches s1)))
                (test-equal "top match is newest 'gamma test'" "gamma test" (history-search-selected s1))
                (let ((s2 (history-search-move s1 1)))
                  (test-equal "moving down selects 'beta test'" "beta test" (history-search-selected s2))
                  (call-with-values (lambda () (history-search-accept s2))
                                    (lambda (buf pos closed)
                                      (test-equal "accept returns selected entry" "beta test" buf)
                                      (test-equal "accept places cursor at end" 9 pos)
                                      (test-assert "accept closes search" (not (history-search-active? closed))))))
                (call-with-values (lambda () (history-search-cancel s1))
                                  (lambda (buf pos closed)
                                    (test-equal "cancel restores saved buffer" "draft" buf)
                                    (test-equal "cancel restores saved pos" 5 pos)
                                    (test-assert "cancel closes search" (not (history-search-active? closed)))))))

            ;; Key handler for Ctrl+R
            (let* ((hist (make-history '("one" "two" "three")))
                   (s0 (history-search-open hist "prev" 4)))
              (call-with-values (lambda () (history-search-handle-key s0 hist #\t))
                                (lambda (act s1 _b _p)
                                  (test-equal "typing updates query" 'update act)
                                  (test-equal "query is 't'" "t" (history-search-query s1))
                                  (call-with-values (lambda () (history-search-handle-key s1 hist 10))
                                                    (lambda (act2 s2 buf pos)
                                                      (test-equal "enter accepts" 'accept act2)
                                                      (test-equal "accepted newest 'three'" "three" buf))))))

            ;; @ mention extraction & completion
            (test-assert "@ active at start" (at-selector-active? "@"))
            (test-assert "@ active after whitespace" (at-selector-active? "read @src/"))
            (test-assert "@ inactive inside email address" (not (at-selector-active? "user@example.com")))
            (test-assert "@ inactive on escaped @@" (not (at-selector-active? "@@literal")))
            (test-assert "@ inactive in 'slash mode" (not (at-selector-active? "@foo" 4 'slash)))

            (let ((matches (matching-at-candidates
                             "inspect @ed"
                             #:candidates '("src/geasa/surfaces-tui/editor.scm"
                                            "src/geasa/surfaces-tui/session.scm"
                                            "readme.md")
                             #:shortcuts '(("todo" . "Summarize todos")))))
              (test-assert "matching-at-candidates finds editor.scm"
                           (equal? (car matches) '("src/geasa/surfaces-tui/editor.scm" . "file")))
              (call-with-values
                (lambda () (apply-at-completion "inspect @ed" 11 (car matches)))
                (lambda (new-buf new-pos)
                  (test-equal "file completion expands to @file:PATH "
                              "inspect @file:src/geasa/surfaces-tui/editor.scm "
                              new-buf)
                  (test-equal "cursor moves after completed token"
                              (string-length new-buf)
                              new-pos))))

            ;; Shortcut completion expands as @NAME
            (let ((s-matches (matching-at-candidates
                               "run @to"
                               #:candidates '("readme.md")
                               #:shortcuts '(("todo" . "Summarize todos")))))
              (test-equal "shortcut candidate matched" '("todo" . "shortcut") (car s-matches))
              (call-with-values
                (lambda () (apply-at-completion "run @to" 7 (car s-matches)))
                (lambda (new-buf _pos)
                  (test-equal "shortcut expands to @NAME " "run @todo " new-buf))))

            ;; Workspace file collector on temp dir
            (let ((dir (mkdtemp "/tmp/badb-wp3-files-XXXXXX")))
              (mkdir (string-append dir "/.git"))
              (string->file (string-append dir "/.git/HEAD") "ref: main")
              (string->file (string-append dir "/src/a.scm") "(define a 1)")
              (let ((files (collect-workspace-files dir 20)))
                (test-equal "collect-workspace-files skips .git and finds src/a.scm"
                            '("src/a.scm")
                            files))
              (rm-rf dir)))

;;;
;;; ui-wp3.3: Configurable <keymap> & Ctrl+I / Tab Resolution
;;;
(test-group "wp3.3-configurable-keymap"
            (let ((km (make-default-keymap)))
              (test-assert "make-default-keymap returns <keymap>" (keymap? km))
              ;; Tab vs Ctrl+I collision resolved:
              (test-equal "Tab maps to 'complete globally"
                          'complete
                          (keymap-lookup km 'tab))
              (test-equal "Tab in steering mode is NOT 'interrupt"
                          'complete
                          (keymap-lookup km 'tab 'steering))
              (test-equal "Ctrl+C in steering mode maps to 'interrupt"
                          'interrupt
                          (keymap-lookup km 'ctrl-c 'steering))
              (test-equal "Esc in steering mode maps to 'interrupt"
                          'interrupt
                          (keymap-lookup km 'esc 'steering))
              (test-equal "Ctrl+R maps to 'fuzzy-history"
                          'fuzzy-history
                          (keymap-lookup km 'ctrl-r))
              (test-equal "Ctrl+O maps to 'toggle-collapse"
                          'toggle-collapse
                          (keymap-lookup km 'ctrl-o))
              (test-equal "Ctrl+E in msgsel maps to 'toggle-collapse"
                          'toggle-collapse
                          (keymap-lookup km 'ctrl-e 'msgsel))
              (test-equal "Ctrl+E in chat maps to 'line-end"
                          'line-end
                          (keymap-lookup km 'ctrl-e 'chat))

              ;; key-event->spec resolves byte 9 vs Kitty Ctrl+I
              (test-equal "raw byte 9 maps to 'tab" 'tab (key-event->spec 9))
              (test-equal "Kitty CSI-u Ctrl+I maps to 'ctrl-i"
                          'ctrl-i
                          (key-event->spec 105 #:kitty-ctrl? #t))

              ;; Rebinding & unbinding
              (keymap-bind! km "Ctrl+K" 'interrupt #:context 'steering #:description "Custom interrupt")
              (test-equal "rebound Ctrl+K maps to 'interrupt in steering"
                          'interrupt
                          (keymap-lookup km 'ctrl-k 'steering))
              (keymap-unbind! km 'ctrl-k #:context 'steering)
              (test-equal "unbound Ctrl+K returns #f"
                          #f
                          (keymap-lookup km 'ctrl-k 'steering))

              ;; Menu items export
              (let ((items (keymap->menu-items km 'steering)))
                (test-assert "keymap->menu-items produces non-empty list" (pair? items))
                (test-assert "menu items carry label/desc and keymap-bind action"
                             (let ((first (car items)))
                               (and (pair? (car first))
                                    (string? (caar first))
                                    (pair? (cdr first))
                                    (eq? (cadr first) 'keymap-bind)))))))

;;;
;;; ui-wp3.4: $SHELL Passthrough, Safe sudo Prompting & $EDITOR Jump
;;;
(test-group "wp3.4-shell-sudo-editor"
            ;; Shell kind and argv construction for nu, fish, zsh, bash
            (test-equal "shell-kind nu" 'nu (shell-kind "/usr/bin/nu"))
            (test-equal "shell-kind fish" 'fish (shell-kind "/usr/bin/fish"))
            (test-equal "shell-kind zsh" 'zsh (shell-kind "/bin/zsh"))
            (test-equal "shell-kind bash" 'bash (shell-kind "/bin/bash"))
            (test-equal "build-shell-command-argv uses -c"
                        '("/usr/bin/fish" "-c" "echo hi")
                        (build-shell-command-argv "/usr/bin/fish" "echo hi"))

            ;; Execution via $SHELL (test with sh/bash)
            (let ((log-path "/tmp/badb-wp3-execlog.jsonl"))
              (dynamic-wind
                (lambda ()
                  (setenv "BADB_EXEC_LOG" log-path)
                  (catch #t (lambda () (delete-file log-path)) (lambda _ #f)))
                (lambda ()
                  (call-with-values
                    (lambda () (execute-shell-command "echo wp3-shell-ok" #:shell "/bin/sh"))
                    (lambda (out code err)
                      (test-assert "shell exec succeeds" (not err))
                      (test-equal "exit 0" 0 code)
                      (test-equal "stdout captured" "wp3-shell-ok" out)))
                  ;; Secret redaction in exec log
                  (call-with-values
                    (lambda ()
                      (execute-shell-command "API_KEY=supersecret123 echo safe" #:shell "/bin/sh"))
                    (lambda (out code err)
                      (test-equal "command still runs" 0 code)
                      (test-equal "output ok" "safe" out)))
                  (let ((log-content (file->string log-path)))
                    (test-assert "exec log does NOT contain raw API_KEY secret"
                                 (not (string-contains log-content "supersecret123")))
                    (test-assert "exec log contains [REDACTED]"
                                 (string-contains log-content "[REDACTED]"))))
                (lambda ()
                  (unsetenv "BADB_EXEC_LOG")
                  (catch #t (lambda () (delete-file log-path)) (lambda _ #f)))))

            ;; sudo detection, injection, and hermetic password prompt flow (no secret logged)
            (test-assert "command-invokes-sudo? detects sudo"
                         (command-invokes-sudo? "sudo ls /root"))
            (test-assert "command-invokes-sudo? ignores echo sudoers"
                         (not (command-invokes-sudo? "echo sudoers")))
            (test-assert "sudo-password-prompt? detects terminal required"
                         (sudo-password-prompt? "sudo: a terminal is required to read the password"))
            (test-equal "inject-sudo-stdin-flag adds -S -p ''"
                        "sudo -S -p '' apt update"
                        (inject-sudo-stdin-flag "sudo apt update"))
            (test-equal "redact-shell-secrets redacts piped sudo password"
                        "echo [REDACTED] | sudo -S id"
                        (redact-shell-secrets "echo my-secret-pw | sudo -S id"))

            ;; Hermetic sudo prompt test via #:exec-fn
            (let ((calls '())
                  (log-path "/tmp/badb-wp3-sudo-log.jsonl"))
              (dynamic-wind
                (lambda ()
                  (setenv "BADB_EXEC_LOG" log-path)
                  (catch #t (lambda () (delete-file log-path)) (lambda _ #f)))
                (lambda ()
                  (call-with-values
                    (lambda ()
                      (execute-shell-command
                        "sudo id"
                        #:shell "/bin/sh"
                        #:password-provider (lambda (_prompt) "s3cr3t-pw!")
                        #:exec-fn (lambda (argv stdin-data)
                                    (set! calls (cons (list argv stdin-data) calls))
                                    (if (not stdin-data)
                                      (values "sudo: a terminal is required to read the password" 1 #f)
                                      (values (string-append "uid=0(root) leaked:" stdin-data) 0 #f)))))
                    (lambda (out code err)
                      (test-equal "sudo retry succeeds with exit 0" 0 code)
                      (test-assert "no error" (not err))
                      (test-assert "secret scrubbed from output"
                                   (not (string-contains out "s3cr3t-pw!")))
                      (test-assert "secret fed via stdin on second call"
                                   (and (= (length calls) 2)
                                        (string=? (cadar calls) "s3cr3t-pw!\n")))))
                  (let ((log-content (or (file->string log-path) "")))
                    (test-assert "password never written to exec log"
                                 (not (string-contains log-content "s3cr3t-pw!")))))
                (lambda ()
                  (unsetenv "BADB_EXEC_LOG")
                  (catch #t (lambda () (delete-file log-path)) (lambda _ #f)))))

            ;; $EDITOR command construction and open-in-external-editor with TTY hooks
            (test-equal "build-editor-command vim +42 file"
                        '("nvim" "+42" "src/a.scm")
                        (build-editor-command "nvim" "src/a.scm" 42))
            (test-equal "build-editor-command helix file:line"
                        '("hx" "src/a.scm:42")
                        (build-editor-command "hx" "src/a.scm" 42))
            (test-equal "build-editor-command vscode --goto"
                        '("code" "--wait" "--goto" "src/a.scm:42:5")
                        (build-editor-command "code --wait" "src/a.scm" 42 5))

            (let ((events '()))
              (call-with-values
                (lambda ()
                  (open-in-external-editor
                    "src/a.scm"
                    #:line 17
                    #:editor "vim"
                    #:suspend-tty! (lambda () (set! events (cons 'suspend events)))
                    #:resume-tty! (lambda () (set! events (cons 'resume events)))
                    #:runner (lambda (argv)
                               (set! events (cons (cons 'run argv) events))
                               0)))
                (lambda (ok? rc)
                  (test-assert "open-in-external-editor succeeds" ok?)
                  (test-equal "exit code 0" 0 rc)
                  (test-equal "suspend -> run -> resume ordering"
                              '(suspend (run "vim" "+17" "src/a.scm") resume)
                              (reverse events))))))

;;;
;;; ui-wp3.5: Session Resume Transcript Reconstruction & REPL Introspection
;;;
(test-group "wp3.5-session-resume-and-repl"
            ;; session-turns->transcript-messages
            (let* ((turns
                     `(((role . "system") (content . "System prompt"))
                       ((role . "user") (content . "Fix editor.scm"))
                       ((role . "assistant")
                        (reasoning_content . "Let me inspect editor.scm")
                        (content . "Checking now.")
                        (tool_calls . #(((id . "tc1")
                                         (type . "function")
                                         (function . ((name . "read_file")
                                                      (arguments . "{\"path\":\"editor.scm\"}")))))))
                       ((role . "tool")
                        (name . "read_file")
                        (tool_call_id . "tc1")
                        (content . "(define-module ...)"))))
                   (cards (session-turns->transcript-messages turns)))
              (test-equal "skips system prompt and reconstructs 4 cards" 4 (length cards))
              (test-equal "first card is user" "user" (assoc-ref (car cards) 'role))
              (test-equal "second card is assistant" "assistant" (assoc-ref (cadr cards) 'role))
              (test-equal "assistant thoughts preserved"
                          "Let me inspect editor.scm"
                          (assoc-ref (cadr cards) 'thoughts))
              (test-assert "assistant thoughts collapsed by default"
                           (assoc-ref (cadr cards) 'collapsed?))
              (test-equal "third card is tool-call" "tool-call" (assoc-ref (caddr cards) 'role))
              (test-assert "tool-call card names read_file"
                           (string-contains (assoc-ref (caddr cards) 'content) "read_file"))
              (test-equal "fourth card is tool-result" "tool-result" (assoc-ref (cadddr cards) 'role))
              (test-assert "tool-result card includes preview"
                           (string-contains (assoc-ref (cadddr cards) 'content) "read_file")))

            ;; session-file-metadata & recent-session-summaries
            (let ((dir (mkdtemp "/tmp/badb-wp3-sess-XXXXXX")))
              (let ((s1 (string-append dir "/s1.jsonl")))
                (persist-session-file!
                  s1
                  `(((role . "system") (content . "sys"))
                    ((role . "user") (content . "Refactor keybindings in editor"))
                    ((role . "assistant") (content . "Done."))))
                (let* ((meta (session-file-metadata s1))
                       (all (recent-session-summaries dir 5)))
                  (test-equal "turn-count excludes system" 2 (assoc-ref meta 'turn-count))
                  (test-equal "user-turn-count is 1" 1 (assoc-ref meta 'user-turn-count))
                  (test-assert "first-prompt captured"
                               (string-contains (assoc-ref meta 'first-prompt) "Refactor keybindings"))
                  (test-assert "summary includes turn count"
                               (string-contains (assoc-ref meta 'summary) "2 turns"))
                  (test-equal "recent-session-summaries returns 1 summary" 1 (length all))))
              (rm-rf dir))

            ;; REPL introspection helpers inside make-repl-mod
            (let* ((reg (make-registry))
                   (jrn-path "/tmp/badb-wp3-repl-jrn.jsonl")
                   (_clean (catch #t (lambda () (delete-file jrn-path)) (lambda _ #f)))
                   (jrn (make-journal jrn-path))
                   (mgr (make-transaction-manager reg #:journal jrn))
                   (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "dummy"))
                   (cfg (make-badb-config #:model-name "wp3-model"))
                   (mcp-mgr (make-mcp-manager cfg))
                   (policy (make-guardrail-policy #:level 'cautious))
                   (repl-mod (make-repl-mod reg jrn mgr client cfg mcp-mgr policy)))
              (registry-attach! reg 'sample-feat (lambda () 'ok))
              (call-with-values (lambda () (eval-scheme-code "(repl-help)" repl-mod))
                                (lambda (res err)
                                  (test-assert "repl-help succeeds" (not err))
                                  (test-assert "repl-help mentions repl-state"
                                               (string-contains res "repl-state"))))
              (call-with-values (lambda () (eval-scheme-code "(repl-inspect reg)" repl-mod))
                                (lambda (res err)
                                  (test-assert "repl-inspect reg succeeds" (not err))
                                  (test-assert "repl-inspect shows sample-feat"
                                               (string-contains res "sample-feat"))))
              (call-with-values (lambda () (eval-scheme-code "(assoc-ref (repl-geasa) 'registered)" repl-mod))
                                (lambda (res err)
                                  (test-assert "repl-geasa succeeds" (not err))
                                  (test-assert "repl-geasa lists sample-feat"
                                               (string-contains res "sample-feat"))))
              (call-with-values (lambda () (eval-scheme-code "(assoc-ref (repl-state) 'model)" repl-mod))
                                (lambda (res err)
                                  (test-assert "repl-state succeeds" (not err))
                                  (test-equal "repl-state reports model" "=> \"wp3-model\"" res)))
              (catch #t (lambda () (delete-file jrn-path)) (lambda _ #f))))

(test-end "ui-wp3")

(when %standalone?
  (when (> (test-runner-fail-count %wp3-runner) 0)
    (exit 1)))
