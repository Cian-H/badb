;;; test-tui.scm — Tests for (badb tui)
;;;
;;; Verifies text wrapping, command parsing, and TUI helper functions.

(define-module (test test-tui)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (badb model)
  #:use-module (badb registry)
  #:use-module (badb transaction)
  #:use-module (badb journal)
  #:use-module (badb config)
  #:use-module (badb mcp)
  #:use-module (badb tui))

(test-begin "badb-tui")

;;;
;;; 1. Word Wrapping (wrap-line and wrap-text)
;;;
(test-group "tui-wrapping"
  (let ((short-line "hello world"))
    (test-equal "short line is not wrapped" '("hello world") (wrap-line short-line 20)))

  (let* ((text "This is a sentence that should wrap nicely at word boundaries.")
         (wrapped (wrap-line text 20)))
    (test-assert "wrapped produces multiple lines" (> (length wrapped) 1))
    (test-assert "every wrapped line is within max width"
      (every (lambda (l) (<= (string-length l) 20)) wrapped))
    (test-equal "reconstructed string preserves all words" text (string-join wrapped " ")))

  ;; Edge cases: single word exceeding max width
  (let* ((long-word "supercalifragilisticexpialidocious")
         (wrapped (wrap-line long-word 10)))
    (test-assert "long word is broken into chunks" (> (length wrapped) 1))
    (test-assert "all chunks <= max width" (every (lambda (l) (<= (string-length l) 10)) wrapped))
    (test-equal "concatenated chunks equal original word" long-word (apply string-append wrapped)))

  ;; Multi-line text wrapping
  (let* ((multi "Line one\nLine two is longer and needs to wrap\nLine three")
         (wrapped (wrap-text multi 15)))
    (test-assert "multi-line wrapped produces list of lines" (> (length wrapped) 3))
    (test-assert "all lines are <= 15 chars" (every (lambda (l) (<= (string-length l) 15)) wrapped))))

;;;
;;; 2. Command Parsing (parse-command)
;;;
(test-group "tui-command-parser"
            ;; Basic commands without arguments
            (test-equal "parse /help" '("help" . "") (parse-command "/help"))
            (test-equal "parse /quit" '("quit" . "") (parse-command "/quit"))
            (test-equal "parse /features" '("features" . "") (parse-command "/features"))
            ;; Commands with arguments
            (test-equal "parse /model with argument"
                        '("model" . "minicpm5-1b:gguf:q8")
                        (parse-command "/model minicpm5-1b:gguf:q8"))
            (test-equal "parse /endpoint with URL"
                        '("endpoint" . "http://127.0.0.1:10100/inference/v1")
                        (parse-command "/endpoint http://127.0.0.1:10100/inference/v1"))
            (test-equal "parse /provider with argument"
                        '("provider" . "lmstudio")
                        (parse-command "/provider lmstudio"))
            ;; Whitespace trimming
            (test-equal
              "parse with leading and trailing whitespace"
              '("test" . "")
              (parse-command "   /test   "))
            (test-equal "parse argument with extra spaces"
                        '("model" . "llama-3")
                        (parse-command "  /model   llama-3  "))
            ;; Non-command inputs
            (test-assert "regular chat prompt returns #f"
                         (not (parse-command "Why GNU Guile?")))
            (test-assert "empty string returns #f" (not (parse-command "")))
            (test-assert
              "string with slash in middle returns #f"
              (not (parse-command "and/or question"))))

;;;
;;; 3. Input Classification (classify-input)
;;;
(test-group "tui-input-classifier"
            ;; Shell commands with '!'
            (test-equal "classify !ls" '(shell . "ls -la") (classify-input "!ls -la"))
            (test-equal "classify ! with space"
                        '(shell . "git status")
                        (classify-input "! git status"))
            (test-equal "classify ! with trailing spaces"
                        '(shell . "make test")
                        (classify-input "  ! make test   "))
            (test-equal "classify bare !" '(shell . "") (classify-input "!"))
            ;; Scheme expressions with ',' or '('
            (test-equal "classify ,(+ 1 2)"
                        '(scheme . "(+ 1 2)")
                        (classify-input ",(+ 1 2)"))
            (test-equal "classify , (+ 1 2) with space"
                        '(scheme . "(+ 1 2)")
                        (classify-input ", (+ 1 2)"))
            (test-equal "classify bare ," '(scheme . "") (classify-input ","))
            (test-equal "classify direct s-expression"
                        '(scheme . "(define x 42)")
                        (classify-input "(define x 42)"))
            (test-equal
              "classify s-expression with leading whitespace"
              '(scheme . "(* x 2)")
              (classify-input "   (* x 2)  "))
            ;; Slash commands
            (test-equal "classify /help" '(slash "help" "") (classify-input "/help"))
            (test-equal "classify /model arg"
                        '(slash "model" "llama3")
                        (classify-input "/model llama3"))
            (test-equal "classify /provider arg"
                        '(slash "provider" "lmstudio")
                        (classify-input "/provider lmstudio"))
            ;; Regular chat inputs
            (test-equal "classify chat message"
                        '(chat . "Why GNU Guile?")
                        (classify-input "Why GNU Guile?"))
            (test-assert "classify empty string is #f" (not (classify-input "")))
            (test-assert "classify whitespace is #f" (not (classify-input "    "))))

;;;
;;; 4. Scheme REPL Evaluation (eval-scheme-code & make-repl-mod)
;;;
(test-group "tui-scheme-eval"
  (let* ((reg (make-registry))
         (jrn (make-journal "/tmp/badb-test-tui-jrn.jsonl"))
         (mgr (make-transaction-manager reg #:journal jrn))
         (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "dummy"))
         (cfg (make-badb-config #:model-name "test-model"))
         (mcp-mgr (make-mcp-manager cfg))
         (repl-mod (make-repl-mod reg jrn mgr client cfg mcp-mgr)))

    ;; Simple arithmetic
    (call-with-values (lambda () (eval-scheme-code "(+ 10 25)" repl-mod))
      (lambda (res err)
        (test-assert "eval arithmetic succeeds" (not err))
        (test-equal "eval arithmetic result" "=> 35" res)))

    ;; Definitions and state preservation across evaluations
    (call-with-values (lambda () (eval-scheme-code "(define (double n) (* n 2))" repl-mod))
      (lambda (res err)
        (test-assert "eval define succeeds" (not err))))

    (call-with-values (lambda () (eval-scheme-code "(double 21)" repl-mod))
      (lambda (res err)
        (test-assert "eval function call succeeds" (not err))
        (test-equal "eval function call result" "=> 42" res)))

    ;; Accessing kernel handles
    (call-with-values (lambda () (eval-scheme-code "(registry? reg)" repl-mod))
      (lambda (res err)
        (test-assert "reg handle accessible" (not err))
        (test-equal "reg handle is registry" "=> #t" res)))

    (call-with-values (lambda () (eval-scheme-code "(journal? jrn)" repl-mod))
      (lambda (res err)
        (test-assert "jrn handle accessible" (not err))
        (test-equal "jrn handle is journal" "=> #t" res)))

    (call-with-values (lambda () (eval-scheme-code "(badb-config? cfg)" repl-mod))
      (lambda (res err)
        (test-assert "cfg handle accessible" (not err))
        (test-equal "cfg handle is config" "=> #t" res)))

    (call-with-values (lambda () (eval-scheme-code "(mcp-manager? mcp-mgr)" repl-mod))
      (lambda (res err)
        (test-assert "mcp-mgr handle accessible" (not err))
        (test-equal "mcp-mgr handle is manager" "=> #t" res)))

    ;; Error handling: numerical overflow / division by zero
    (call-with-values (lambda () (eval-scheme-code "(/ 1 0)" repl-mod))
      (lambda (res err)
        (test-assert "division by zero returns error string" (string? err))
        (test-assert "error mentions numerical-overflow" (string-contains err "numerical-overflow"))))

    ;; Error handling: syntax error
    (call-with-values (lambda () (eval-scheme-code "(+ 1" repl-mod))
      (lambda (res err)
        (test-assert "incomplete syntax returns error" (string? err))
        (test-assert "error mentions read-error" (string-contains err "read-error"))))))

;;;
;;; 5. Shell Execution (execute-shell-command)
;;;
(test-group "tui-shell-exec"
  ;; Successful echo
  (call-with-values (lambda () (execute-shell-command "echo 'hello badb shell'"))
    (lambda (out exit-val err)
      (test-assert "shell exec succeeds" (not err))
      (test-equal "exit code is 0" 0 exit-val)
      (test-equal "stdout captured" "hello badb shell" out)))

  ;; Non-zero exit code
  (call-with-values (lambda () (execute-shell-command "sh -c 'echo oops >&2; exit 42'"))
    (lambda (out exit-val err)
      (test-assert "shell exec captures exit" (not err))
      (test-equal "exit code is 42" 42 exit-val)
      (test-assert "stderr captured" (string-contains out "oops")))))

;;;
;;; 6. Model Thoughts and Reasoning Separation (split-thoughts-and-content)
;;;
(test-group "tui-thoughts-separation"
  ;; Closed <think> tag
  (call-with-values (lambda () (split-thoughts-and-content "<think>Let me reason carefully.</think>The answer is 42."))
    (lambda (th ct)
      (test-equal "extracts closed <think> tag thoughts" "Let me reason carefully." th)
      (test-equal "extracts closed <think> tag content" "The answer is 42." ct)))

  ;; Closed <thought> tag
  (call-with-values (lambda () (split-thoughts-and-content "<thought>Pondering options...</thought>Option A is best."))
    (lambda (th ct)
      (test-equal "extracts closed <thought> tag thoughts" "Pondering options..." th)
      (test-equal "extracts closed <thought> tag content" "Option A is best." ct)))

  ;; Streaming in-flight unclosed <think> tag
  (call-with-values (lambda () (split-thoughts-and-content "<think>Currently reasoning in real-time..."))
    (lambda (th ct)
      (test-equal "extracts in-flight streaming thoughts" "Currently reasoning in real-time..." th)
      (test-equal "content is empty during in-flight thinking" "" ct)))

  ;; Multiple thought blocks
  (call-with-values (lambda () (split-thoughts-and-content "<think>Part 1</think>Content 1\n<think>Part 2</think>Content 2"))
    (lambda (th ct)
      (test-equal "combines multiple thought tags" "Part 1\n\nPart 2" th)
      (test-equal "combines content around multiple thought tags" "Content 1\nContent 2" ct)))

  ;; Text with no thought tags
  (call-with-values (lambda () (split-thoughts-and-content "Pure answer with no internal thoughts."))
    (lambda (th ct)
      (test-assert "thoughts are #f when no tags present" (not th))
      (test-equal "content is unchanged" "Pure answer with no internal thoughts." ct)))

  ;; Empty text
  (call-with-values (lambda () (split-thoughts-and-content ""))
    (lambda (th ct)
      (test-assert "thoughts are #f on empty input" (not th))
      (test-equal "content is empty on empty input" "" ct))))

;;;
;;; 7. Slash Command Palette & Selector Filtering
;;;
(test-group "tui-slash-selector"
            ;; selector-active?
            (test-assert
              "selector is active when buffer starts with /"
              (selector-active? "/"))
            (test-assert
              "selector is active when query is being typed"
              (selector-active? "/mod"))
            (test-assert
              "selector is inactive once a space is typed (argument mode)"
              (not (selector-active? "/model ")))
            (test-assert
              "selector is inactive for normal chat"
              (not (selector-active? "hello /help")))
            (test-assert
              "selector is inactive for shell command"
              (not (selector-active? "!ls")))
            ;; %slash-commands registry contents
            (test-assert "slash commands list contains help" (assoc "help" %slash-commands))
            (test-assert "slash commands list contains model"
                         (assoc "model" %slash-commands))
            (test-assert
              "slash commands list contains endpoint"
              (assoc "endpoint" %slash-commands))
            (test-assert
              "slash commands list contains provider"
              (assoc "provider" %slash-commands))
            (test-assert "slash commands list contains mcp" (assoc "mcp" %slash-commands))
            (test-assert
              "slash commands list contains config"
              (assoc "config" %slash-commands))
            (test-assert
              "slash commands list contains guardrail"
              (assoc "guardrail" %slash-commands))
            (test-assert
              "slash commands list contains compact"
              (assoc "compact" %slash-commands))
            (test-assert
              "slash commands list contains checkpoint"
              (assoc "checkpoint" %slash-commands))
            (test-assert
              "slash commands list contains rollback"
              (assoc "rollback" %slash-commands))
            (test-assert "slash commands list contains rules"
                         (assoc "rules" %slash-commands))
            (test-assert
              "slash commands list contains context"
              (assoc "context" %slash-commands))
            (test-assert
              "slash commands list contains features"
              (assoc "features" %slash-commands))
            (test-assert
              "slash commands list contains journal"
              (assoc "journal" %slash-commands))
            (test-assert "slash commands list contains test" (assoc "test" %slash-commands))
            (test-assert "slash commands list contains sh" (assoc "sh" %slash-commands))
            (test-assert "slash commands list contains eval" (assoc "eval" %slash-commands))
            (test-assert "slash commands list contains clear"
                         (assoc "clear" %slash-commands))
            (test-assert "slash commands list contains quit" (assoc "quit" %slash-commands))
            ;; matching-slash-commands filtering
            (let
                ((all-matches (matching-slash-commands "/")))
              (test-equal "bare / returns all slash commands"
                          (length %slash-commands)
                          (length all-matches)))
            (let ((m-matches (matching-slash-commands "/m")))
              (test-assert "matches commands containing m"
                           (member "model" (map car m-matches))))
            (let
                ((exact-matches (matching-slash-commands "/jour")))
              (test-equal "matches journal command" '("journal") (map car exact-matches)))
            (let
                ((no-matches (matching-slash-commands "/xyznonexistent")))
              (test-assert "empty list for non-matching query" (null? no-matches))))

;;;
;;; 8. Morphing Single-Character Prompt Indicators (get-prompt-indicator)
;;;
(test-group "tui-prompt-indicator"
  ;; Idle prompt: single-character ❯
  (call-with-values (lambda () (get-prompt-indicator "" "READY"))
    (lambda (glyph role bold?)
      (test-equal "idle prompt is single char ❯" "❯" glyph)
      (test-equal "idle prompt uses the accent role" 'accent role)
      (test-assert "idle prompt is bold" bold?)))

  ;; Shell mode: single-character $
  (call-with-values (lambda () (get-prompt-indicator "!git status" "READY"))
    (lambda (glyph role bold?)
      (test-equal "shell prompt is single char $" "$" glyph)
      (test-equal "shell prompt uses the shell role" 'shell role)
      (test-assert "shell prompt is bold" bold?)))

  ;; Scheme mode: single-character λ
  (call-with-values (lambda () (get-prompt-indicator ",(+ 1 2)" "READY"))
    (lambda (glyph role bold?)
      (test-equal "scheme prompt is single char λ" "λ" glyph)
      (test-equal "scheme prompt uses the scheme role" 'scheme role)
      (test-assert "scheme prompt is bold" bold?)))

  (call-with-values (lambda () (get-prompt-indicator "(define x 42)" "READY"))
    (lambda (glyph role bold?)
      (test-equal "s-expr prompt is single char λ" "λ" glyph)
      (test-equal "s-expr prompt uses the scheme role" 'scheme role)))

  ;; Slash command mode: single-character /
  (call-with-values (lambda () (get-prompt-indicator "/model" "READY"))
    (lambda (glyph role bold?)
      (test-equal "slash prompt is single char /" "/" glyph)
      (test-equal "slash prompt uses the accent role" 'accent role)))

  ;; Task-based morphing indicators
  (call-with-values (lambda () (get-prompt-indicator "" "STREAMING..."))
    (lambda (glyph role bold?)
      (test-equal "streaming prompt is single char ⟳" "⟳" glyph)
      (test-equal "streaming prompt uses the status role" 'status role)))

  (call-with-values (lambda () (get-prompt-indicator "" "EXEC..."))
    (lambda (glyph role bold?)
      (test-equal "executing prompt is single char ◆" "◆" glyph)
      (test-equal "executing prompt uses the shell role" 'shell role)))

  (call-with-values (lambda () (get-prompt-indicator "" "TOOL: read_file..."))
    (lambda (glyph role bold?)
      (test-equal "tool prompt is single char ◆" "◆" glyph)
      (test-equal "tool prompt uses the tool-call role" 'tool-call role)))

  (call-with-values (lambda () (get-prompt-indicator "" "REACT [1/8]"))
    (lambda (glyph role bold?)
      (test-equal "react prompt is single char ◆" "◆" glyph)
      (test-equal "react prompt uses the tool-call role" 'tool-call role)))

  (call-with-values (lambda () (get-prompt-indicator "" "EVAL..."))
    (lambda (glyph role bold?)
      (test-equal "evaluating prompt is single char λ" "λ" glyph)
      (test-equal "evaluating prompt uses the scheme role" 'scheme role)))

  (call-with-values (lambda () (get-prompt-indicator "" "TESTING..."))
    (lambda (glyph role bold?)
      (test-equal "testing prompt is single char ◆" "◆" glyph)
      (test-equal "testing prompt uses the warning role" 'warning role))))

;;;
;;; 9. Slim Header, Rule & Keymap Layout Formatters
;;;
(test-group "tui-header-and-rule-formatters"
  ;; Header segments
  (let ((segs (format-header-segments 80 "READY" "minicpm5-1b:gguf:q8" 3 5)))
    (test-equal "header starts with the accent wordmark at column 0"
                '(0 "badb" accent #t)
                (car segs))
    (test-assert "header carries the model name in dim after the wordmark"
                 (find (lambda (s) (equal? s (list 5 "minicpm5-1b:gguf:q8" 'dim #f)))
                       segs))
    (test-assert "header carries the status word in the status role"
                 (find (lambda (s)
                         (and (string=? (cadr s) "READY") (eq? (caddr s) 'status)))
                       segs))
    (test-assert "header carries the feature count when nonzero"
                 (find (lambda (s) (string-contains (cadr s) "3 feats")) segs))
    (test-assert "header carries the checkpoint count when nonzero"
                 (find (lambda (s) (string-contains (cadr s) "5 chkpts")) segs))
    (test-assert "rightmost header segment fits the terminal width"
                 (let ((last-seg (car (reverse segs))))
                   (<= (+ (car last-seg) (string-length (cadr last-seg))) 80))))

  (let ((segs (format-header-segments 100 "READY" "minicpm5-1b:gguf:q8" 3 5 "1/2")))
    (test-assert "header carries the mcp count when provided"
                 (find (lambda (s) (string-contains (cadr s) "mcp 1/2")) segs))
    (test-assert "wide header still fits the terminal width"
                 (let ((last-seg (car (reverse segs))))
                   (<= (+ (car last-seg) (string-length (cadr last-seg))) 100))))

  ;; Zero counts drop out entirely
  (let ((segs (format-header-segments 80 "READY" "minicpm" 0 0)))
    (test-assert "zero counts are omitted from the header"
                 (not (find (lambda (s)
                              (or (string-contains (cadr s) "feats")
                                  (string-contains (cadr s) "chkpts")))
                            segs))))

  ;; Narrow terminals drop the least important right items first
  (let ((segs (format-header-segments 28 "READY" "minicpm5-1b:gguf:q8" 3 5 "1/2")))
    (test-equal "narrow header keeps the wordmark segment"
                "badb"
                (cadr (car segs)))
    (test-assert "narrow header keeps the status segment"
                 (find (lambda (s) (string=? (cadr s) "READY")) segs))
    (test-assert "narrow header drops the mcp count"
                 (not (find (lambda (s) (string-contains (cadr s) "mcp")) segs))))

  ;; Thin horizontal rule under the header
  (let ((rule (format-rule-line 80)))
    (test-equal "rule line fills the terminal width" 80 (string-length rule))
    (test-equal "rule line ends with the rule char" #\─ (string-ref rule 79))
    (test-equal "rule line consists of rule chars only"
                (make-string 80 #\─)
                rule)))

(test-group "tui-keymap-hints"
  (test-assert "chat keymap lists the modal prompt keys"
               (string-contains (format-keymap-line 'chat) "!:shell"))
  (test-assert "chat keymap lists the message selection keys"
               (string-contains (format-keymap-line 'chat) "^p/^n:select"))
  (test-assert "chat keymap lists the collapse key"
               (string-contains (format-keymap-line 'chat) "^e:collapse"))
  (test-assert "shell keymap documents the run key"
               (string-contains (format-keymap-line 'shell) "enter:run"))
  (test-assert "shell keymap documents the return key"
               (string-contains (format-keymap-line 'shell) "backspace:back to chat"))
  (test-assert "scheme keymap documents the eval key"
               (string-contains (format-keymap-line 'scheme) "enter:eval"))
  (test-assert "selector keymap documents the palette keys"
               (string-contains (format-keymap-line 'selector) "up/down:choose"))
  (test-assert "menu keymap documents the menu keys"
               (string-contains (format-keymap-line 'menu) "enter:apply"))
  (test-assert "unknown modes fall back to the chat keymap"
               (string=? (format-keymap-line 'anything-else)
                         (format-keymap-line 'chat))))

;;;
;;; 10. Modal Prompt Switching & Mode Hints
;;;
(test-group "tui-modal-prompt-switching"
  ;; Mode-based indicators
  (call-with-values (lambda () (get-prompt-indicator "" "READY" 'shell))
    (lambda (glyph role bold?)
      (test-equal "shell mode indicator is $" "$" glyph)
      (test-equal "shell mode uses the shell role" 'shell role)
      (test-assert "shell mode indicator is bold" bold?)))

  (call-with-values (lambda () (get-prompt-indicator "" "READY" 'scheme))
    (lambda (glyph role bold?)
      (test-equal "scheme mode indicator is λ" "λ" glyph)
      (test-equal "scheme mode uses the scheme role" 'scheme role)
      (test-assert "scheme mode indicator is bold" bold?)))

  (call-with-values (lambda () (get-prompt-indicator "" "READY" 'chat))
    (lambda (glyph role bold?)
      (test-equal "chat mode indicator is ❯" "❯" glyph)
      (test-equal "chat mode uses the accent role" 'accent role)
      (test-assert "chat mode indicator is bold" bold?)))

  ;; Mode-based keymap hints
  (test-assert "shell keymap names the shell mode"
               (string-contains (format-keymap-line 'shell) "enter:run"))
  (test-assert "shell keymap documents the backspace return"
               (string-contains (format-keymap-line 'shell) "backspace:back to chat"))

  (test-assert "scheme keymap names the scheme mode"
               (string-contains (format-keymap-line 'scheme) "enter:eval"))
  (test-assert "scheme keymap documents the backspace return"
               (string-contains (format-keymap-line 'scheme) "backspace:back to chat")))

;;;
;;; 11. Scheme Expression Normalization (normalize-scheme-expr)
;;;
(test-group "tui-scheme-normalization"
  (test-equal "preserves balanced s-expressions" "(+ 1 2)" (normalize-scheme-expr "(+ 1 2)"))

  (test-equal "auto-wraps multiple tokens without parens" "(+ 1 2)" (normalize-scheme-expr "+ 1 2"))

  (test-equal "auto-wraps trailing paren missing opening paren" "(+ 1 2)" (normalize-scheme-expr "+ 1 2)"))

  (test-equal "preserves standalone identifier" "reg" (normalize-scheme-expr "reg"))

  (test-equal "preserves standalone literal number" "42" (normalize-scheme-expr "42"))

  (test-equal "preserves complex definition"
    "(define (double x) (* x 2))" (normalize-scheme-expr "(define (double x) (* x 2))")))

;;;
;;; 12. Visual Hierarchy & Token Segmentation (segment-line)
;;;
(test-group "tui-token-segmentation"
  ;; Plain text without special tokens
  (let ((segs (segment-line "Simple text with no tokens")))
    (test-equal "plain text is single chunk" 1 (length segs))
    (test-equal "plain text chunk uses the default role" 'default (cadr (car segs)))
    (test-assert "plain text chunk is not bold" (not (caddr (car segs)))))

  ;; Status tags
  (let ((segs-running (segment-line "[RUNNING]")))
    (test-equal "running status text" "[RUNNING]" (caar segs-running))
    (test-equal "running status uses the success role" 'success (cadar segs-running))
    (test-assert "running status is bold" (caddar segs-running)))

  (let ((segs-stopped (segment-line "[STOPPED]")))
    (test-equal "stopped status text" "[STOPPED]" (caar segs-stopped))
    (test-equal "stopped status uses the warning role" 'warning (cadar segs-stopped))
    (test-assert "stopped status is bold" (caddar segs-stopped)))

  (let ((segs-deny (segment-line "[DENY]")))
    (test-equal "deny status text" "[DENY]" (caar segs-deny))
    (test-equal "deny status uses the error role" 'error (cadar segs-deny))
    (test-assert "deny status is bold" (caddar segs-deny)))

  ;; Bullets
  (let ((segs-bullet (segment-line "• list item")))
    (test-equal "bullet glyph text" "• " (caar segs-bullet))
    (test-equal "bullet glyph uses the accent role" 'accent (cadar segs-bullet))
    (test-assert "bullet glyph is bold" (caddar segs-bullet))
    (test-equal "bullet content uses the default role" 'default (cadr (cadr segs-bullet))))

  ;; Property keys
  (let ((segs-key (segment-line "Model: moonshotai/kimi-k2.6")))
    (test-equal "property key text" "Model: " (caar segs-key))
    (test-equal "property key uses the accent role" 'accent (cadar segs-key))
    (test-equal "property value uses the default role" 'default (cadr (cadr segs-key))))

  ;; Composite MCP line: "  • filesystem: [RUNNING] (14 tools)"
  (let ((segs-mcp (segment-line "  • filesystem: [RUNNING] (14 tools)")))
    (test-assert "composite line has multiple styled segments" (>= (length segs-mcp) 5))
    ;; Verify presence of green [RUNNING] segment
    (let ((running-seg (find (lambda (s) (string=? (car s) "[RUNNING]")) segs-mcp)))
      (test-assert "running tag found in segments" running-seg)
      (test-equal "running tag uses the success role" 'success (cadr running-seg))
      (test-assert "running tag is bold" (caddr running-seg)))
    ;; Verify presence of cyan filesystem: key
    (let ((key-seg (find (lambda (s) (string=? (car s) "filesystem: ")) segs-mcp)))
      (test-assert "filesystem key found in segments" key-seg)
      (test-equal "filesystem key uses the accent role" 'accent (cadr key-seg))))

  ;; URL preservation (not broken by key label regex)
  (let ((segs-url (segment-line "Endpoint: https://openrouter.ai/api/v1")))
    (test-equal "endpoint key is matched" "Endpoint: " (caar segs-url))
    (test-equal "url is preserved intact as default role"
      "https://openrouter.ai/api/v1" (car (cadr segs-url)))
    (test-equal "url uses the default role" 'default (cadr (cadr segs-url)))))

;;;
;;; 13. Input Buffer Cursor Editing Helpers
;;;
(test-group "tui-input-cursor"
  ;; insert-at: start / middle / end
  (call-with-values (lambda () (insert-at "world" 0 "hello "))
    (lambda (buf pos)
      (test-equal "insert at start" "hello world" buf)
      (test-equal "insert at start moves cursor past str" 6 pos)))

  (call-with-values (lambda () (insert-at "ad" 1 "bc"))
    (lambda (buf pos)
      (test-equal "insert in middle" "abcd" buf)
      (test-equal "insert in middle moves cursor past str" 3 pos)))

  (call-with-values (lambda () (insert-at "hello" 5 " world"))
    (lambda (buf pos)
      (test-equal "insert at end" "hello world" buf)
      (test-equal "insert at end moves cursor past str" 11 pos)))

  (call-with-values (lambda () (insert-at "abc" 99 "X"))
    (lambda (buf pos)
      (test-equal "insert beyond end clamps to end" "abcX" buf)
      (test-equal "insert beyond end cursor at new end" 4 pos)))

  (call-with-values (lambda () (insert-at "abc" -3 "X"))
    (lambda (buf pos)
      (test-equal "insert before start clamps to 0" "Xabc" buf)
      (test-equal "insert before start cursor after str" 1 pos)))

  ;; backspace-at: no-op at 0, delete before cursor
  (call-with-values (lambda () (backspace-at "hello" 0))
    (lambda (buf pos)
      (test-equal "backspace at 0 is a no-op" "hello" buf)
      (test-equal "backspace at 0 keeps cursor at 0" 0 pos)))

  (call-with-values (lambda () (backspace-at "hello" 2))
    (lambda (buf pos)
      (test-equal "backspace mid-buffer deletes char before cursor" "hllo" buf)
      (test-equal "backspace mid-buffer moves cursor left" 1 pos)))

  (call-with-values (lambda () (backspace-at "hello" 5))
    (lambda (buf pos)
      (test-equal "backspace at end deletes last char" "hell" buf)
      (test-equal "backspace at end moves cursor left" 4 pos)))

  ;; delete-at: KEY_DC semantics, no-op at end
  (call-with-values (lambda () (delete-at "hello" 5))
    (lambda (buf pos)
      (test-equal "delete at end is a no-op" "hello" buf)
      (test-equal "delete at end keeps cursor" 5 pos)))

  (call-with-values (lambda () (delete-at "hello" 1))
    (lambda (buf pos)
      (test-equal "delete mid-buffer removes char at cursor" "hllo" buf)
      (test-equal "delete mid-buffer keeps cursor" 1 pos)))

  (call-with-values (lambda () (delete-at "hello" 0))
    (lambda (buf pos)
      (test-equal "delete at start removes first char" "ello" buf)
      (test-equal "delete at start keeps cursor at 0" 0 pos)))

  ;; clamp-cursor
  (test-equal "clamp negative pos to 0" 0 (clamp-cursor -5 "abc"))
  (test-equal "clamp pos beyond end to length" 3 (clamp-cursor 99 "abc"))
  (test-equal "clamp keeps valid position" 2 (clamp-cursor 2 "abc"))
  (test-equal "clamp on empty buffer is 0" 0 (clamp-cursor 3 ""))

  ;; input-visible-window: buffer fits entirely
  (call-with-values (lambda () (input-visible-window "abc" 2 10))
    (lambda (vis cx)
      (test-equal "short buffer shown whole" "abc" vis)
      (test-equal "short buffer cursor keeps absolute offset" 2 cx)))

  ;; cursor at start of long buffer: no scroll, cursor at x 0
  (call-with-values (lambda () (input-visible-window "0123456789" 0 5))
    (lambda (vis cx)
      (test-equal "cursor at start shows window head" "01234" vis)
      (test-equal "cursor at start x is 0" 0 cx)))

  ;; cursor past right edge: window scrolls to keep cursor visible with right context
  (call-with-values (lambda () (input-visible-window "0123456789" 7 5))
    (lambda (vis cx)
      (test-equal "cursor past right edge scrolls window" "45678" vis)
      (test-equal "cursor past right edge keeps cursor in window" 3 cx)
      (test-assert "cursor past right edge shows right context"
        (and (>= cx 0) (< cx 4)))))

  ;; cursor at very end of long buffer: tail window, cursor after last char
  (call-with-values (lambda () (input-visible-window "0123456789" 10 5))
    (lambda (vis cx)
      (test-equal "cursor at end shows tail window" "56789" vis)
      (test-equal "cursor at end x equals window width" 5 cx)))

  ;; tiny avail-width: single column window
  (call-with-values (lambda () (input-visible-window "abcdef" 2 1))
    (lambda (vis cx)
      (test-equal "tiny width shows char at cursor" "c" vis)
      (test-equal "tiny width cursor x is 0" 0 cx)))

  (call-with-values (lambda () (input-visible-window "abcdef" 6 1))
    (lambda (vis cx)
      (test-equal "tiny width at end shows empty window" "" vis)
      (test-equal "tiny width at end cursor x is 0" 0 cx)))

  ;; cursor position is always inside the window
  (test-assert "cursor always visible in window for sweep of positions"
    (let loop ((pos 0) (ok? #t))
      (if (> pos 12)
          ok?
          (call-with-values
            (lambda () (input-visible-window "0123456789AB" pos 5))
            (lambda (vis cx)
              (loop (+ pos 1)
                    (and ok?
                         (<= 0 cx 5)
                         (or (= cx 5)
                             (string-prefix? (substring vis cx (min (string-length vis) (+ cx 1)))
                                             (substring "0123456789AB" pos)))))))))))

;;;
;;; 14. Input History Recall & Session Persistence
;;;
(test-group "tui-input-history"
  ;; Construction & accessors
  (let ((h (make-history)))
    (test-assert "fresh history object" (input-history? h))
    (test-equal "fresh history is empty" '() (history-entries h))
    (test-equal "fresh history length is 0" 0 (history-length h)))

  (let ((h (make-history '("alpha" "beta"))))
    (test-equal "seeded entries are oldest first"
                '("alpha" "beta")
                (history-entries h))
    (test-equal "seeded history length is 2" 2 (history-length h)))

  ;; history-add: appending, trimming, and dedupe
  (let ((h (make-history)))
    (test-equal "history-add returns #t on append" #t (history-add h "first"))
    (test-equal "history-add returns #t on second append" #t (history-add h "second"))
    (test-equal "history-add appends in order"
                '("first" "second")
                (history-entries h))
    (test-equal "history-add skips identical most recent entry"
                #f
                (history-add h "second"))
    (test-equal "dedupe keeps length unchanged" 2 (history-length h))
    (test-equal "history-add trims surrounding whitespace"
                #t
                (history-add h "  padded  "))
    (test-equal "trimmed entry compared against most recent"
                #f
                (history-add h "padded"))
    (test-equal "history-add skips empty input" #f (history-add h ""))
    (test-equal "history-add skips whitespace-only input" #f (history-add h "   "))
    (test-equal "skips leave length unchanged" 3 (history-length h))
    (test-equal "re-adding an older value is allowed"
                #t
                (history-add h "first")))

  ;; Cap behavior: oldest entries dropped beyond the 500 cap
  (let ((h (make-history)))
    (let loop ((i 0))
      (when (< i (+ %history-max-entries 5))
        (history-add h (format #f "cmd-~a" i))
        (loop (+ i 1))))
    (test-equal "history is capped at the maximum"
                %history-max-entries
                (history-length h))
    (test-equal "oldest entries are dropped on cap"
                "cmd-5"
                (car (history-entries h)))
    (test-equal "newest entry is preserved on cap"
                (format #f "cmd-~a" (+ %history-max-entries 4))
                (last (history-entries h))))

  ;; Recall ordering: Up walks older, Down walks newer
  (let ((h (make-history '("one" "two" "three"))))
    (test-equal "first Up recalls the newest entry" "three" (history-recall h -1))
    (test-equal "recall position is the newest index" 2 (history-recall-position h))
    (test-equal "second Up recalls the older entry" "two" (history-recall h -1))
    (test-equal "third Up recalls the oldest entry" "one" (history-recall h -1))
    (test-equal "Up at the oldest entry stays at the oldest"
                "one"
                (history-recall h -1))
    (test-equal "Down recalls the newer entry" "two" (history-recall h 1))
    (test-equal "Down again recalls the newest entry" "three" (history-recall h 1))
    (test-equal "Down past the newest returns #f (restore draft)"
                #f
                (history-recall h 1))
    (test-equal "Down at the draft boundary still returns #f"
                #f
                (history-recall h 1))
    (test-equal "recall position resets to draft at newest boundary"
                #f
                (history-recall-position h))
    (test-equal "Up from draft again recalls the newest"
                "three"
                (history-recall h -1)))

  ;; Empty history never recalls
  (let ((h (make-history)))
    (test-equal "Up on an empty history returns #f" #f (history-recall h -1))
    (test-equal "Down on an empty history returns #f" #f (history-recall h 1)))

  ;; Reset-on-edit semantics
  (let ((h (make-history '("one" "two" "three"))))
    (history-recall h -1)
    (history-recall h -1)
    (test-equal "recall position is mid-history before reset"
                1
                (history-recall-position h))
    (history-reset-nav h)
    (test-equal "history-reset-nav returns to the draft boundary"
                #f
                (history-recall-position h))
    (test-equal "Up after reset starts from the newest entry"
                "three"
                (history-recall h -1))
    (history-reset-nav h)
    (test-equal "Down after reset stays at draft (no stale restore)"
                #f
                (history-recall h 1))))

;;;
;;; 15. Input History JSONL Persistence
;;;
(test-group "tui-input-history-jsonl"
  ;; Serialize / parse round-trip
  (let* ((line (history-entry->jsonl-line "ls -la" "shell" 1700000000))
         (entry (jsonl-line->history-entry line)))
    (test-assert "serialized line is a string" (string? line))
    (test-equal "round-trip preserves input" "ls -la" (assoc-ref entry "input"))
    (test-equal "round-trip preserves mode" "shell" (assoc-ref entry "mode"))
    (test-equal "round-trip preserves timestamp" 1700000000 (assoc-ref entry "ts")))

  ;; Special characters survive the round-trip on a single line
  (let* ((tricky "say \"hi\"\nnewline & <tag>")
         (line (history-entry->jsonl-line tricky "chat" 1))
         (entry (jsonl-line->history-entry line)))
    (test-equal "serialized line contains no raw newline"
                 #f
                 (string-index line #\newline))
    (test-equal "round-trip preserves escaped special characters"
                tricky
                (assoc-ref entry "input")))

  ;; Defensive parsing
  (test-equal "parse blank line returns #f" #f (jsonl-line->history-entry ""))
  (test-equal "parse whitespace line returns #f" #f (jsonl-line->history-entry "   "))
  (test-equal "parse garbage returns #f" #f (jsonl-line->history-entry "not json {"))
  (test-equal "parse entry without input returns #f"
              #f
              (jsonl-line->history-entry "{\"mode\":\"chat\"}"))
  (let ((minimal (jsonl-line->history-entry "{\"input\":\"hi\"}")))
    (test-equal "entry missing mode defaults to chat" "chat" (assoc-ref minimal "mode"))
    (test-equal "entry missing ts defaults to 0" 0 (assoc-ref minimal "ts")))
  (test-equal "serialize symbol mode as string"
              "scheme"
              (assoc-ref (jsonl-line->history-entry
                          (history-entry->jsonl-line "(+ 1 2)" 'scheme 42))
                         "mode"))
  (test-equal "serialize non-string input defensively"
              "42"
              (assoc-ref (jsonl-line->history-entry
                          (history-entry->jsonl-line 42 "chat" 0))
                         "input")))

;;;
;;; 16. Input History File Load & Append
;;;
(test-group "tui-input-history-persistence"
  (let ((path "/tmp/badb-test-tui-history.jsonl"))
    (catch #t (lambda () (delete-file path)) (lambda _ #f))
    (test-equal "append writes and returns #t"
                #t
                (append-history-file-entry! path "alpha" "chat"))
    (test-equal "append is idempotent on success"
                #t
                (append-history-file-entry! path "beta" "shell"))
    (test-equal "load returns inputs oldest first"
                '("alpha" "beta")
                (load-history-file path))
    (test-equal "load missing file yields empty history"
                '()
                (load-history-file "/tmp/badb-test-tui-no-such-history.jsonl"))
    (catch #t (lambda () (delete-file path)) (lambda _ #f)))

  ;; Load respects the 500-entry cap
  (let ((path "/tmp/badb-test-tui-history-cap.jsonl"))
    (catch #t (lambda () (delete-file path)) (lambda _ #f))
    (call-with-output-file path
      (lambda (port)
        (let loop ((i 0))
          (when (< i (+ %history-max-entries 2))
            (display (history-entry->jsonl-line (format #f "cmd-~a" i) "chat" i) port)
            (newline port)
            (loop (+ i 1))))))
    (let ((loaded (load-history-file path)))
      (test-equal "load respects the entry cap"
                  %history-max-entries
                  (length loaded))
      (test-equal "load drops the oldest entries beyond the cap"
                  "cmd-2"
                  (car loaded)))
    (catch #t (lambda () (delete-file path)) (lambda _ #f))))

;;;
;;; 17. Message Collapse State Helpers
;;;
(test-group "tui-message-collapse"
  ;; Flag presence & lookup
  (test-assert "absent collapsed flag means not collapsed"
               (not (message-collapsed? `((role . "user") (content . "hi")))))
  (test-assert "truthy collapsed flag means collapsed"
               (message-collapsed? `((role . "user") (content . "hi") (collapsed? . #t))))

  ;; make-collapsed-message: pure constructor
  (let ((orig `((role . "assistant") (thoughts . "abc") (content . "def"))))
    (test-assert "make-collapsed-message sets the flag"
                 (message-collapsed? (make-collapsed-message orig)))
    (test-assert "make-collapsed-message leaves the original untouched"
                 (not (message-collapsed? orig))))

  ;; message-with-collapsed: pure update / removal
  (let* ((orig `((role . "user") (content . "hi")))
         (collapsed (message-with-collapsed orig #t))
         (restored (message-with-collapsed collapsed #f)))
    (test-assert "message-with-collapsed #t collapses" (message-collapsed? collapsed))
    (test-assert "message-with-collapsed #f expands" (not (message-collapsed? restored)))
    (test-equal "flag removal round-trips to the original alist" orig restored))

  ;; Toggle round-trip preserving payload fields
  (let* ((m `((role . "assistant") (thoughts . "deep thoughts") (content . "final answer")))
         (once (message-toggle-collapsed m))
         (twice (message-toggle-collapsed once)))
    (test-assert "toggle collapses an expanded message" (message-collapsed? once))
    (test-assert "toggle expands a collapsed message" (not (message-collapsed? twice)))
    (test-equal "round-trip preserves role" "assistant" (assoc-ref twice 'role))
    (test-equal "round-trip preserves thoughts" "deep thoughts" (assoc-ref twice 'thoughts))
    (test-equal "round-trip preserves content" "final answer" (assoc-ref twice 'content)))

  ;; default-collapsed-role? per role
  (test-assert "thought cards collapse by default" (default-collapsed-role? "thought"))
  (test-assert "thinking cards collapse by default" (default-collapsed-role? "thinking"))
  (test-assert "symbol thought collapses by default" (default-collapsed-role? 'thought))
  (for-each
    (lambda (r)
      (test-assert (format #f "role '~a' stays expanded by default" r)
                   (not (default-collapsed-role? r))))
    '("user" "assistant" "system" "notice" "error" "shell-cmd" "shell-out"
      "scheme-in" "scheme-out" "tool-call" "tool-result"))

  ;; message-collapsible?
  (test-assert "user message with content is collapsible"
               (message-collapsible? `((role . "user") (content . "hi"))))
  (test-assert "assistant message with thoughts is collapsible"
               (message-collapsible? `((role . "assistant") (thoughts . "hmm") (content . ""))))
  (test-assert "empty message is not collapsible"
               (not (message-collapsible? `((role . "user") (content . "")))))
  (test-assert "whitespace-only message is not collapsible"
               (not (message-collapsible? `((role . "notice") (content . "   ")))))

  ;; default-collapse-message policy
  (test-assert "assistant with thoughts collapses by default"
               (message-collapsed?
                 (default-collapse-message
                   `((role . "assistant") (thoughts . "hmm") (content . "c")))))
  (test-assert "assistant without thoughts stays expanded"
               (not (message-collapsed?
                      (default-collapse-message
                        `((role . "assistant") (thoughts . #f) (content . "c"))))))
  (test-assert "user messages never collapse by default"
               (not (message-collapsed?
                      (default-collapse-message `((role . "user") (content . "hi"))))))
  (test-assert "explicit user collapse is preserved"
               (message-collapsed?
                 (default-collapse-message
                   `((role . "user") (content . "hi") (collapsed? . #t)))))
  (test-assert "explicit user expansion is preserved"
               (not (message-collapsed?
                       (default-collapse-message
                         `((role . "assistant") (thoughts . "hmm") (content . "c")
                           (collapsed? . #f))))))

  ;; Collapsed summary lines
  (let ((s (format-collapsed-summary-line "thinking" 12)))
    (test-assert "summary line carries the block label"
                 (string-contains s "thinking"))
    (test-assert "summary line mentions the toggle key"
                 (string-contains s "ctrl+e"))
    (test-assert "summary line mentions the hidden line count"
                 (string-contains s "12")))
  (test-equal "single hidden line reads singular"
              "you (1 line) — ctrl+e to expand"
              (format-collapsed-summary-line "you" 1))
  (test-equal "multiple hidden lines read plural"
              "you (3 lines) — ctrl+e to expand"
              (format-collapsed-summary-line "you" 3))

  ;; Thought previews

  ;; Thought previews
  (test-equal "preview keeps short thoughts intact"
              "short thought"
              (thoughts-preview "short thought"))
  (test-equal "preview truncates at 40 characters"
              40
              (string-length (thoughts-preview (make-string 90 #\a))))
  (test-equal "preview uses only the first line"
              "first line"
              (thoughts-preview "first line\nsecond line")))

;;;
;;; 18. Collapse-Aware Block Rendering (format-message-entry)
;;;
(test-group "tui-collapse-rendering"
  ;; Collapsed non-assistant message: exactly one content row
  (let* ((msg `((role . "user")
               (content . "a longer user message that wraps across a couple of lines")))
         (rows (format-message-entry (make-collapsed-message msg) 40))
         (content-rows (filter (lambda (e) (not (string=? (car e) "spacer"))) rows)))
    (test-equal "collapsed message renders exactly one content row"
                1
                (length content-rows))
    (test-equal "collapsed row is a block-summary row"
                "block-summary"
                (car (car content-rows)))
    (test-equal "collapsed summary row carries the user theme role"
                'user
                (cadr (car content-rows)))
    (test-assert "collapsed summary line documents the expand key"
                 (string-contains (caddr (car content-rows)) "ctrl+e"))
    (test-assert "collapsed summary line carries the role label"
                 (string-contains (caddr (car content-rows)) "you")))

  ;; Expanded message renders the full flat block
  (let* ((msg `((role . "user") (content . "hello from the user")))
         (rows (format-message-entry msg 40))
         (label-row (find (lambda (e) (string=? (car e) "block-label")) rows)))
    (test-assert "expanded message renders a block-label row"
                 label-row)
    (test-assert "expanded message renders a block-body row"
                 (any (lambda (e) (string=? (car e) "block-body")) rows))
    (test-equal "label row carries the lowercase role label"
                "you"
                (cadr label-row))
    (test-equal "label row carries the user theme role"
                'user
                (caddr label-row))
    (test-equal "expanded message ends with a single spacer row"
                "spacer"
                (car (last rows))))

  ;; Collapsed assistant reasoning: one summary line, content block visible
  (let* ((msg `((role . "assistant")
                (thoughts . "internal reasoning about the task")
                (content . "final answer")))
         (rows (format-message-entry (make-collapsed-message msg) 60))
         (summaries (filter (lambda (e) (string=? (car e) "block-summary")) rows))
         (bodies (filter (lambda (e) (string=? (car e) "block-body")) rows)))
    (test-equal "collapsed thoughts render one summary row"
                1
                (length summaries))
    (test-assert "summary row reports the thinking label"
                 (string-contains (caddr (car summaries)) "thinking"))
    (test-assert "summary row documents the expand key"
                 (string-contains (caddr (car summaries)) "ctrl+e"))
    (test-equal "assistant content body stays visible"
                "final answer"
                (caddr (find (lambda (e)
                               (and (string=? (car e) "block-body")
                                    (eq? (cadr e) 'assistant)))
                             bodies)))
    (test-assert "no thought body rows while collapsed"
                 (not (any (lambda (e)
                             (and (string=? (car e) "block-body")
                                  (eq? (cadr e) 'thought)))
                           rows))))

  ;; Expanded assistant reasoning: thought block plus content block
  (let* ((msg `((role . "assistant") (thoughts . "internal reasoning") (content . "answer")))
         (rows (format-message-entry msg 60)))
    (test-assert "expanded thoughts render thought body rows"
                 (any (lambda (e)
                        (and (string=? (car e) "block-body")
                             (eq? (cadr e) 'thought)))
                      rows))
    (test-assert "expanded assistant renders content body rows"
                 (any (lambda (e)
                        (and (string=? (car e) "block-body")
                             (eq? (cadr e) 'assistant)))
                      rows))
    (test-assert "expanded reasoning renders no summary rows"
                 (not (any (lambda (e) (string=? (car e) "block-summary")) rows)))
    (test-equal "assistant message ends with exactly one spacer row"
                1
                (length (filter (lambda (e) (string=? (car e) "spacer")) rows))))

  ;; Collapsing shrinks the rendered row count
  (let* ((msg `((role . "user")
               (content . "line one of a fairly long message that will wrap around the narrow column limit")))
         (expanded-count (length (format-message-entry msg 40)))
         (collapsed-count (length (format-message-entry (make-collapsed-message msg) 40))))
    (test-assert "collapsed renders fewer rows than expanded"
                 (< collapsed-count expanded-count)))

  ;; Non-collapsible message ignores the collapse flag
  (let ((rows (format-message-entry `((role . "user") (content . "") (collapsed? . #t)) 40)))
    (test-assert "empty message ignores the collapse flag"
                 (any (lambda (e) (string=? (car e) "block-label")) rows))))

;;;
;;; 19. Role Labels, Theme Mapping & Color Constants
;;;
(test-group "tui-role-labels-and-theme-mapping"
  ;; Role label text
  (test-equal "user role label" "you" (message-role-label "user"))
  (test-equal "assistant role label" "assistant" (message-role-label "assistant"))
  (test-equal "thought role label" "thinking" (message-role-label "thought"))
  (test-equal "shell-cmd role label" "shell" (message-role-label "shell-cmd"))
  (test-equal "shell-out role label" "shell output" (message-role-label "shell-out"))
  (test-equal "scheme-in role label" "scheme" (message-role-label "scheme-in"))
  (test-equal "scheme-out role label" "scheme output" (message-role-label "scheme-out"))
  (test-equal "tool-call role label" "tool" (message-role-label "tool-call"))
  (test-equal "tool role label" "tool" (message-role-label "tool"))
  (test-equal "tool-result role label" "tool result" (message-role-label "tool-result"))
  (test-equal "error role label" "error" (message-role-label "error"))
  (test-equal "notice role label" "notice" (message-role-label "notice"))
  (test-equal "unknown roles fall back to system"
              "system"
              (message-role-label "mystery"))

  ;; Theme role mapping
  (test-equal "user maps to the user theme role" 'user (message-role->theme-role "user"))
  (test-equal "assistant maps to the assistant theme role"
              'assistant
              (message-role->theme-role "assistant"))
  (test-equal "thought maps to the thought theme role" 'thought (message-role->theme-role "thought"))
  (test-equal "shell roles map to the shell theme role"
              'shell
              (message-role->theme-role "shell-out"))
  (test-equal "scheme roles map to the scheme theme role"
              'scheme
              (message-role->theme-role "scheme-in"))
  (test-equal "tool calls map to the tool-call theme role"
              'tool-call
              (message-role->theme-role "tool"))
  (test-equal "tool results map to the tool-result theme role"
              'tool-result
              (message-role->theme-role "tool-result"))
  (test-equal "notice maps to the notice theme role" 'notice (message-role->theme-role "notice"))
  (test-equal "unknown roles map to the system theme role"
              'system
              (message-role->theme-role "mystery"))

  ;; System message labels derive from content
  (test-equal "mcp listing content"
              "mcp servers"
              (system-message-label "Configured MCP Servers:\n- filesystem"))
  (test-equal "configuration content"
              "configuration"
              (system-message-label "badb Configuration:\nModel: x"))
  (test-equal "guardrail content"
              "guardrails"
              (system-message-label "Guardrail Policy Level: cautious"))
  (test-equal "help content"
              "help"
              (system-message-label "Help & Commands"))
  (test-equal "checkpoint content"
              "journal"
              (system-message-label "Created checkpoint 'abc' (desc)."))
  (test-equal "generic system content"
              "system"
              (system-message-label "something unmatched"))

  ;; Color symbol -> ncurses constant mapping
  (test-equal "cyan maps to COLOR_CYAN" 'COLOR_CYAN (color-symbol->ncurses-constant 'cyan))
  (test-equal "black maps to COLOR_BLACK" 'COLOR_BLACK (color-symbol->ncurses-constant 'black))
  (test-equal "white maps to COLOR_WHITE" 'COLOR_WHITE (color-symbol->ncurses-constant 'white))
  (test-equal "magenta maps to COLOR_MAGENTA"
              'COLOR_MAGENTA
              (color-symbol->ncurses-constant 'magenta))
  (test-equal "default maps to COLOR_WHITE"
              'COLOR_WHITE
              (color-symbol->ncurses-constant 'default))
  (test-equal "invalid colors map to COLOR_WHITE"
              'COLOR_WHITE
              (color-symbol->ncurses-constant 'sparkly)))

;;;
;;; 20. Interactive Menu Model (make-menu & friends)
;;;
(test-group "tui-menu-model"
  ;; Construction & accessors
  (let* ((items (list (cons (cons "Model" "Switch model") 'open-model)
                      (cons (cons "Endpoint" "Switch endpoint") 'open-endpoint)
                      (cons (cons "Theme" "Choose theme") 'open-theme)))
         (m (make-menu "Configuration" items)))
    (test-assert "make-menu returns a menu" (menu? m))
    (test-equal "menu title is stored" "Configuration" (menu-title m))
    (test-equal "menu count is 3" 3 (menu-count m))
    (test-equal "default selected index is 0" 0 (menu-selected-index m))
    (test-equal "selected item is the first item" (car items) (menu-selected-item m))
    (test-equal "menu items are preserved" 3 (length (menu-items m))))

  ;; Item accessors: label, description, action data
  (let ((item (cons (cons "Theme" "Choose UI theme") (cons 'set-theme "nord"))))
    (test-equal "menu-item-label returns the label" "Theme" (menu-item-label item))
    (test-equal "menu-item-description returns the description"
                "Choose UI theme"
                (menu-item-description item))
    (test-equal "menu-item-action returns the action data"
                '(set-theme . "nord")
                (menu-item-action item)))

  ;; Bare-string items are normalized with an empty description
  (let ((m (make-menu "T" (list "plain"))))
    (test-equal "bare string item normalized to a label"
                "plain"
                (menu-item-label (menu-selected-item m)))
    (test-equal "bare string item has empty description"
                ""
                (menu-item-description (menu-selected-item m))))

  ;; #:selected-index keyword & clamping
  (let ((m (make-menu "T"
                      (list (cons (cons "a" "1") 'x)
                            (cons (cons "b" "2") 'y)
                            (cons (cons "c" "3") "z"))
                      #:selected-index 1)))
    (test-equal "selected index keyword is honored" 1 (menu-selected-index m))
    (test-equal "selected item follows the index"
                "b"
                (menu-item-label (menu-selected-item m))))
  (let ((m (make-menu "T" (list (cons (cons "a" "1") 'x)) #:selected-index 42)))
    (test-equal "selected index clamps to the last item" 0 (menu-selected-index m)))
  (let ((m (make-menu "T" '())))
    (test-equal "empty menu count is 0" 0 (menu-count m))
    (test-equal "empty menu selected index stays 0" 0 (menu-selected-index m))
    (test-assert "empty menu has no selected item" (not (menu-selected-item m))))

  ;; menu-move: advancing, retreating, and wrap-around
  (let* ((items (map (lambda (i) (cons (cons (format #f "i~a" i) "") i)) (iota 3)))
         (m (make-menu "T" items)))
    (test-assert "menu-move returns a fresh menu" (menu? (menu-move m 1)))
    (test-equal "menu-move preserves the item list"
                (menu-items m)
                (menu-items (menu-move m 1)))
    (test-equal "move down advances the selection" 1 (menu-selected-index (menu-move m 1)))
    (test-equal "move up retreats the selection"
                1
                (menu-selected-index
                  (menu-move (make-menu "T" items #:selected-index 2) -1)))
    (test-equal "move down past the end wraps to the top"
                0
                (menu-selected-index (menu-move (menu-move (menu-move m 1) 1) 1)))
    (test-equal "move up at the top wraps to the bottom"
                2
                (menu-selected-index (menu-move m -1)))
    (test-equal "large deltas wrap modulo the count"
                1
                (menu-selected-index (menu-move m 7))))

  ;; menu-move on an empty menu is a no-op
  (let ((m (make-menu "T" '())))
    (test-equal "moving an empty menu changes nothing" m (menu-move m 1)))

  ;; menu-visible-slice: short menus return everything with offset 0
  (let ((m (make-menu "T" (list (cons (cons "a" "1") 'x)
                                (cons (cons "b" "2") 'y)))))
    (call-with-values (lambda () (menu-visible-slice m 5))
      (lambda (vis offset)
        (test-equal "short menu returns all items" 2 (length vis))
        (test-equal "short menu offset is 0" 0 offset))))

  ;; menu-visible-slice: long menus window around the selection
  (let* ((items (map (lambda (i) (cons (cons (format #f "item ~a" i) "") i)) (iota 20)))
         (m (make-menu "Long" items)))
    ;; selection at the top: window anchored at the start
    (call-with-values (lambda () (menu-visible-slice m 5))
      (lambda (vis offset)
        (test-equal "window caps at max-visible items" 5 (length vis))
        (test-equal "window anchored at the top initially" 0 offset)
        (test-equal "first visible item is the first item"
                    "item 0"
                    (menu-item-label (car vis)))))
    ;; selection near the bottom: window scrolls to keep it visible
    (let ((m9 (make-menu "Long" items #:selected-index 9)))
      (call-with-values (lambda () (menu-visible-slice m9 5))
        (lambda (vis offset)
          (test-equal "window scrolls to follow the selection" 5 offset)
          (test-equal "selected item stays visible at the window bottom"
                      "item 9"
                      (menu-item-label (car (reverse vis)))))))
    ;; selection at the very end: window clamps to the list end
    (let ((m19 (make-menu "Long" items #:selected-index 19)))
      (call-with-values (lambda () (menu-visible-slice m19 5))
        (lambda (vis offset)
          (test-equal "window ends at the last item" 15 offset)
          (test-equal "last item is visible"
                      "item 19"
                      (menu-item-label (car (reverse vis)))))))
    ;; cap larger than the menu returns everything
    (call-with-values (lambda () (menu-visible-slice m 50))
      (lambda (vis offset)
        (test-equal "cap beyond count returns all items" 20 (length vis))
        (test-equal "offset is 0 when everything fits" 0 offset)))))

(test-group "tui-model-detection-decision"
  ;; A specific current model always skips detection
  (call-with-values
      (lambda () (detect-model-decision "gpt-4o-mini" #f #f))
    (lambda (new-model kind)
      (test-equal "specific model is kept" "gpt-4o-mini" new-model)
      (test-equal "specific model skips detection" 'skipped kind)))
  (call-with-values
      (lambda () (detect-model-decision "gpt-4o-mini" "qwen3-1.7b" #f))
    (lambda (new-model kind)
      (test-equal "specific model ignores a detected model" "gpt-4o-mini" new-model)
      (test-equal "specific model skips even on success" 'skipped kind)))
  (call-with-values
      (lambda () (detect-model-decision "gpt-4o-mini" #f "endpoint down"))
    (lambda (new-model kind)
      (test-equal "specific model never reports failure" 'skipped kind)))

  ;; Generic current model adopts the detected one
  (call-with-values
      (lambda () (detect-model-decision #f "qwen3-1.7b" #f))
    (lambda (new-model kind)
      (test-equal "generic #f model adopts the detected model" "qwen3-1.7b" new-model)
      (test-equal "adoption is reported" 'adopted kind)))
  (call-with-values
      (lambda () (detect-model-decision "" "qwen3-1.7b" #f))
    (lambda (new-model kind)
      (test-equal "empty model adopts the detected model" "qwen3-1.7b" new-model)
      (test-equal "empty model adoption is reported" 'adopted kind)))
  (call-with-values
      (lambda () (detect-model-decision "default" "qwen3-1.7b" #f))
    (lambda (new-model kind)
      (test-equal "'default' model adopts the detected model" "qwen3-1.7b" new-model)
      (test-equal "'default' model adoption is reported" 'adopted kind)))

  ;; Generic current model with a detection failure
  (call-with-values
      (lambda () (detect-model-decision "default" #f "connect failed"))
    (lambda (new-model kind)
      (test-equal "failed detection yields no model" #f new-model)
      (test-equal "failure is reported" 'failed kind)))

  ;; Generic current model, no error, nothing detected
  (call-with-values
      (lambda () (detect-model-decision #f #f #f))
    (lambda (new-model kind)
      (test-equal "no detected model yields #f" #f new-model)
      (test-equal "unavailable outcome is reported" 'unavailable kind))))

(test-group "tui-model-menu-items"
  (let* ((ids '("qwen3-1.7b" "llama-3.2-3b-instruct" "gpt-4o-mini"))
         (items (build-model-menu-items ids "llama-3.2-3b-instruct")))
    (test-equal "one item per id plus the manual entry" 4 (length items))
    (test-equal "ids keep their order"
                ids
                (map menu-item-label (take items 3)))
    (test-equal "every id item carries a set-model action"
                (map (lambda (id) (cons 'set-model id)) ids)
                (map menu-item-action (take items 3)))
    (test-equal "active model is marked in its description"
                "active"
                (menu-item-description (cadr items)))
    (test-equal "other models describe availability"
                "available"
                (menu-item-description (car items)))
    (test-equal "trailing item is the manual entry"
                "Set model..."
                (menu-item-label (car (take-right items 1))))
    (test-equal "manual entry carries the prefill action"
                'prefill-model
                (menu-item-action (car (take-right items 1)))))

  ;; Active model absent from the list: nothing is marked active
  (let ((items (build-model-menu-items '("qwen3-1.7b" "gpt-4o-mini")
                                       "claude-4-opus")))
    (test-assert "no item is marked active when the model is absent"
      (not (member "active" (map menu-item-description items)))))

  ;; Generic active model marks nothing
  (let ((items (build-model-menu-items '("qwen3-1.7b") "default")))
    (test-equal "generic active model marks nothing"
                '("available")
                (map menu-item-description (take items 1))))

  ;; Empty id list: only the manual entry survives
  (let ((items (build-model-menu-items '() #f)))
    (test-equal "empty id list yields a single item" 1 (length items))
    (test-equal "empty id list leaves only the manual entry"
                "Set model..."
                (menu-item-label (car items))))

  ;; The items plug straight into the pure menu model
  (let ((m (make-menu "Model"
                      (build-model-menu-items '("qwen3-1.7b" "gpt-4o-mini")
                                              "gpt-4o-mini"))))
    (test-equal "items build a valid menu" 3 (menu-count m))
    (test-equal "menu labels come from the ids"
                "qwen3-1.7b"
                (menu-item-label (menu-selected-item m)))))

;;;
;;; Agent Steering: Pure Keyboard & Presentation Helpers
;;;
(test-group "tui-agent-steering"
  ;; steering-key-handle: printable characters append
  (call-with-values (lambda () (steering-key-handle "go nor" #\t))
    (lambda (new-buffer action)
      (test-equal "printable character appends" "go nort" new-buffer)
      (test-equal "append action reported" 'append action)))

  (call-with-values (lambda () (steering-key-handle "" #\a))
    (lambda (new-buffer action)
      (test-equal "append works on empty buffer" "a" new-buffer)
      (test-equal "append action reported" 'append action)))

  ;; Integer codes in the printable range also append
  (call-with-values (lambda () (steering-key-handle "x" 98))
    (lambda (new-buffer action)
      (test-equal "integer printable code appends" "xb" new-buffer)
      (test-equal "append action reported" 'append action)))

  ;; Backspace / Ctrl+H / DEL delete the last character
  (call-with-values (lambda () (steering-key-handle "abc" 127))
    (lambda (new-buffer action)
      (test-equal "DEL deletes the last character" "ab" new-buffer)
      (test-equal "backspace action reported" 'backspace action)))

  (call-with-values (lambda () (steering-key-handle "abc" 8))
    (lambda (new-buffer action)
      (test-equal "Ctrl+H deletes the last character" "ab" new-buffer)
      (test-equal "backspace action reported" 'backspace action)))

  (call-with-values (lambda () (steering-key-handle "" 127))
    (lambda (new-buffer action)
      (test-equal "backspace on empty buffer is a no-op" "" new-buffer)
      (test-equal "empty backspace reports none" 'none action)))

  ;; Enter queues non-blank buffers and clears them
  (call-with-values (lambda () (steering-key-handle "go north" #\newline))
    (lambda (new-buffer action)
      (test-equal "enter clears the buffer" "" new-buffer)
      (test-equal "enter on non-empty buffer queues" 'queue action)))

  (call-with-values (lambda () (steering-key-handle "go north" 10))
    (lambda (new-buffer action)
      (test-equal "integer newline code queues" 'queue action)))

  (call-with-values (lambda () (steering-key-handle "ok" 13))
    (lambda (new-buffer action)
      (test-equal "carriage return code queues" 'queue action)))

  (call-with-values (lambda () (steering-key-handle "   " #\newline))
    (lambda (new-buffer action)
      (test-equal "blank buffer survives a blank submit" "   " new-buffer)
      (test-equal "enter on blank buffer reports none" 'none action)))

  ;; Ctrl+I (Tab) requests an interrupt and discards the buffer
  (call-with-values (lambda () (steering-key-handle "partial" 9))
    (lambda (new-buffer action)
      (test-equal "ctrl+i discards the pending buffer" "" new-buffer)
      (test-equal "ctrl+i reports interrupt" 'interrupt action)))

  (call-with-values (lambda () (steering-key-handle "" #\tab))
    (lambda (new-buffer action)
      (test-equal "tab character also reports interrupt" 'interrupt action)))

  ;; Escape clears the buffer
  (call-with-values (lambda () (steering-key-handle "abc" 27))
    (lambda (new-buffer action)
      (test-equal "escape clears the buffer" "" new-buffer)
      (test-equal "escape reports clear" 'clear action)))

  ;; Other control chars and curses key codes are ignored
  (call-with-values (lambda () (steering-key-handle "abc" 3))
    (lambda (new-buffer action)
      (test-equal "unmapped control char leaves the buffer" "abc" new-buffer)
      (test-equal "unmapped control char reports none" 'none action)))

  (call-with-values (lambda () (steering-key-handle "abc" 259))
    (lambda (new-buffer action)
      (test-equal "curses key code leaves the buffer" "abc" new-buffer)
      (test-equal "curses key code reports none" 'none action)))

  ;; steering-pending-line: nothing pending yields #f
  (test-equal "no buffer and no queue yields #f"
              #f
              (steering-pending-line "" 0))

  ;; In-progress buffer display
  (test-equal "buffer alone renders the steering line"
              "steering: slow down"
              (steering-pending-line "slow down" 0))

  ;; Queued-but-unconsumed count display
  (test-equal "queue count alone renders the queued line"
              "steering: 2 queued"
              (steering-pending-line "" 2))

  ;; Buffer plus queued entries
  (test-equal "buffer and queue render combined"
              "steering: hi (+1 queued)"
              (steering-pending-line "hi" 1))

  ;; Interrupt pill takes precedence
  (test-equal "interrupt request renders the pill"
              "steering: interrupt requested"
              (steering-pending-line "" 0 #t))

  (test-equal "interrupt request beats a typed buffer"
              "steering: interrupt requested"
              (steering-pending-line "typing" 1 #t))

  ;; agent-run-active?: status text classification
  (test-assert "REACT status marks an active run"
               (agent-run-active? "REACT [3/15]"))
  (test-assert "TOOL status marks an active run"
               (agent-run-active? "TOOL: read_file..."))
  (test-assert "READY is not an active run"
               (not (agent-run-active? "READY")))
  (test-assert "STREAMING is not an agent run"
               (not (agent-run-active? "STREAMING...")))
  (test-assert "non-string status is not an active run"
               (not (agent-run-active? #f)))

  ;; Steering keymap hint shown while the agent runs
  (test-assert "steering keymap documents the send key"
               (string-contains (format-keymap-line 'steering) "enter:send"))
  (test-assert "steering keymap documents the interrupt key"
               (string-contains (format-keymap-line 'steering) "^i:interrupt"))
  (test-assert "steering keymap documents the escape key"
               (string-contains (format-keymap-line 'steering) "esc:clear"))

  ;; /steer and /interrupt are registered in the slash command palette
  (test-assert "slash commands list contains steer"
               (assoc "steer" %slash-commands))
  (test-assert "slash commands list contains interrupt"
               (assoc "interrupt" %slash-commands)))

;;;
;;; Key normalization (key->code)
;;;
(test-group "tui-key-normalization"
  ;; guile-ncurses getch returns chars for ordinary/control keys and
  ;; integers for function-key constants; dispatch must normalize.
  (test-equal "escape char becomes 27" 27 (key->code #\esc))
  (test-equal "ctrl-e char becomes 5" 5 (key->code #\x05))
  (test-equal "ctrl-p char becomes 16" 16 (key->code #\x10))
  (test-equal "letter char becomes codepoint" 97 (key->code #\a))
  (test-equal "newline char becomes 10" 10 (key->code #\newline))
  (test-equal "return char becomes 13" 13 (key->code #\return))
  (test-equal "tab char becomes 9" 9 (key->code #\tab))
  (test-equal "delete char becomes 127" 127 (key->code #\del))
  (test-equal "integer key constants pass through" 259 (key->code 259))
  (test-equal "KEY_ENTER integer passes through" 343 (key->code 343))
  (test-eq "small integers pass through unchanged" 13 (key->code 13))
  (test-assert "#f stays #f (non-blocking read miss)" (not (key->code #f)))
  (test-assert "chars and their codes are NOT eqv? in Guile"
               (not (eqv? #\esc 27))))

(test-group "tui-conversation-transcript"
            (let
                ((msgs
                   '(((role . "user") (content . "hello"))
                     ((role . "assistant") (thoughts . "hmm") (content . "hi there"))
                     ((role . "error") (content . "boom")))))
              (let
                  ((txt (format-conversation-transcript msgs)))
                (test-assert "transcript has header"
                             (string-prefix? ";; badb conversation transcript" txt))
                (test-assert "user block present" (string-contains txt "=== USER ==="))
                (test-assert "assistant block present"
                             (string-contains txt "=== ASSISTANT ==="))
                (test-assert "thinking section present" (string-contains txt "[thinking]"))
                (test-assert "thinking content recorded" (string-contains txt "hmm"))
                (test-assert "content recorded" (string-contains txt "hi there"))
                (test-assert "error block present" (string-contains txt "=== ERROR ==="))
                (test-assert "error content recorded" (string-contains txt "boom"))))
            (test-assert
              "empty transcript is just the header"
              (string=? ";; badb conversation transcript\n"
                        (format-conversation-transcript '())))
            (test-assert "non-string content is stringified"
                         (string-contains
                           (format-conversation-transcript '(((role . "system") (content . 42))))
                           "42"))
            (test-assert "symbol roles become string headers"
                         (string-contains
                           (format-conversation-transcript '(((role . user) (content . "x"))))
                           "=== USER ==="))
            (test-assert "slash commands list contains dump" (assoc "dump" %slash-commands))
            (test-assert "slash commands list contains exit" (assoc "exit" %slash-commands)))

;;;
;;; Wrapped input layout (input-wrapped-rows)
;;;
(test-group "tui-wrapped-input"
  (call-with-values
    (lambda () (input-wrapped-rows "hello world" 11 5 4))
    (lambda (lines row col)
      (test-equal "wraps to two lines" '("hello" "world") lines)
      (test-equal "cursor row at end" 1 row)
      (test-equal "cursor col at end" 5 col)))
  (call-with-values
    (lambda () (input-wrapped-rows "hello world" 5 5 4))
    (lambda (lines row col)
      (test-equal "same layout" '("hello" "world") lines)
      (test-equal "cursor on first line" 0 row)
      (test-equal "cursor col on first line" 5 col)))
  (call-with-values
    (lambda () (input-wrapped-rows "" 0 20 4))
    (lambda (lines row col)
      (test-equal "empty buffer is one empty line" '("") lines)
      (test-equal "empty buffer cursor row 0" 0 row)
      (test-equal "empty buffer cursor col 0" 0 col)))
  (call-with-values
    (lambda () (input-wrapped-rows (make-string 95 #\a) 95 10 3))
    (lambda (lines row col)
      (test-equal "long input scrolled to last rows" 3 (length lines))
      (test-equal "scrolled window shows the tail"
        "aaaaa" (list-ref lines 2))
      (test-equal "cursor row within window" 2 row)
      (test-equal "cursor col at tail end" 5 col)))
  (call-with-values
    (lambda () (input-wrapped-rows (make-string 95 #\a) 0 10 3))
    (lambda (lines row col)
      (test-equal "cursor at start keeps the first rows"
        "aaaaaaaaaa" (car lines))
      (test-equal "cursor row stays visible" 0 row)
      (test-equal "cursor col at start" 0 col))))

(test-end "badb-tui")
