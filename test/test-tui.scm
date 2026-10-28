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
    (lambda (glyph pair bold?)
      (test-equal "idle prompt is single char ❯" "❯" glyph)
      (test-equal "idle prompt color is green pair 2" 2 pair)
      (test-assert "idle prompt is bold" bold?)))

  ;; Shell mode: single-character $
  (call-with-values (lambda () (get-prompt-indicator "!git status" "READY"))
    (lambda (glyph pair bold?)
      (test-equal "shell prompt is single char $" "$" glyph)
      (test-equal "shell prompt color is magenta pair 6" 6 pair)
      (test-assert "shell prompt is bold" bold?)))

  ;; Scheme mode: single-character λ
  (call-with-values (lambda () (get-prompt-indicator ",(+ 1 2)" "READY"))
    (lambda (glyph pair bold?)
      (test-equal "scheme prompt is single char λ" "λ" glyph)
      (test-equal "scheme prompt color is cyan pair 1" 1 pair)
      (test-assert "scheme prompt is bold" bold?)))

  (call-with-values (lambda () (get-prompt-indicator "(define x 42)" "READY"))
    (lambda (glyph pair bold?)
      (test-equal "s-expr prompt is single char λ" "λ" glyph)
      (test-equal "s-expr prompt color is cyan pair 1" 1 pair)))

  ;; Slash command mode: single-character /
  (call-with-values (lambda () (get-prompt-indicator "/model" "READY"))
    (lambda (glyph pair bold?)
      (test-equal "slash prompt is single char /" "/" glyph)
      (test-equal "slash prompt color is yellow pair 4" 4 pair)))

  ;; Task-based morphing indicators
  (call-with-values (lambda () (get-prompt-indicator "" "STREAMING..."))
    (lambda (glyph pair bold?)
      (test-equal "streaming prompt is single char ⟳" "⟳" glyph)
      (test-equal "streaming prompt color is cyan pair 1" 1 pair)))

  (call-with-values (lambda () (get-prompt-indicator "" "EXEC..."))
    (lambda (glyph pair bold?)
      (test-equal "executing prompt is single char ⚙" "⚙" glyph)
      (test-equal "executing prompt color is magenta pair 6" 6 pair)))

  (call-with-values (lambda () (get-prompt-indicator "" "TOOL: read_file..."))
    (lambda (glyph pair bold?)
      (test-equal "tool prompt is single char ⚙" "⚙" glyph)
      (test-equal "tool prompt color is magenta pair 6" 6 pair)))

  (call-with-values (lambda () (get-prompt-indicator "" "REACT [1/8]"))
    (lambda (glyph pair bold?)
      (test-equal "react prompt is single char ⚙" "⚙" glyph)
      (test-equal "react prompt color is yellow pair 4" 4 pair)))

  (call-with-values (lambda () (get-prompt-indicator "" "EVAL..."))
    (lambda (glyph pair bold?)
      (test-equal "evaluating prompt is single char λ" "λ" glyph)
      (test-equal "evaluating prompt color is cyan pair 1" 1 pair)))

  (call-with-values (lambda () (get-prompt-indicator "" "TESTING..."))
    (lambda (glyph pair bold?)
      (test-equal "testing prompt is single char ⚙" "⚙" glyph)
      (test-equal "testing prompt color is yellow pair 4" 4 pair))))

;;;
;;; 9. Modern 2020s Container Frame Formatters
;;;
(test-group "tui-modern-frame-formatters"
  ;; Header Line
  (let ((header (format-header-line 80 "READY" "minicpm5-1b:gguf:q8" 3 5)))
    (test-assert "header starts with top-left rounded corner" (string-prefix? "╭─ badb ─" header))
    (test-assert "header ends with top-right rounded corner" (string-suffix? "╮" header))
    (test-assert "header contains status pill" (string-contains header "❲● READY❳"))
    (test-assert "header contains model pill" (string-contains header "❲minicpm5-1b:gguf:q8❳"))
    (test-assert "header contains features pill" (string-contains header "❲3 feats❳"))
    (test-assert "header contains journal pill" (string-contains header "❲5 chkpts❳"))
    (test-equal "header length matches terminal width"
      80 (string-length header)))

  (let ((header-mcp (format-header-line 100 "READY" "minicpm5-1b:gguf:q8" 3 5 "1/2")))
    (test-assert "header contains mcp status pill when provided" (string-contains header-mcp "❲mcp: 1/2❳"))
    (test-equal "header with mcp matches terminal width"
      100 (string-length header-mcp)))

  ;; Prompt Divider
  (let ((divider (format-prompt-divider 80)))
    (test-assert "divider starts with left tee branch" (string-prefix? "├─ prompt ──" divider))
    (test-assert "divider ends with right tee branch" (string-suffix? "┤" divider))
    (test-assert "divider contains shortcut hint" (string-contains divider "[Tab: complete"))
    (test-equal "divider length matches terminal width"
      80 (string-length divider))))

;;;
;;; 10. Modal Prompt Switching & Dynamic Hints
;;;
(test-group "tui-modal-prompt-switching"
  ;; Mode-based indicators
  (call-with-values (lambda () (get-prompt-indicator "" "READY" 'shell))
    (lambda (glyph pair bold?)
      (test-equal "shell mode indicator is $" "$" glyph)
      (test-equal "shell mode color is magenta pair 6" 6 pair)
      (test-assert "shell mode indicator is bold" bold?)))

  (call-with-values (lambda () (get-prompt-indicator "" "READY" 'scheme))
    (lambda (glyph pair bold?)
      (test-equal "scheme mode indicator is λ" "λ" glyph)
      (test-equal "scheme mode color is cyan pair 1" 1 pair)
      (test-assert "scheme mode indicator is bold" bold?)))

  (call-with-values (lambda () (get-prompt-indicator "" "READY" 'chat))
    (lambda (glyph pair bold?)
      (test-equal "chat mode indicator is ❯" "❯" glyph)
      (test-equal "chat mode color is green pair 2" 2 pair)
      (test-assert "chat mode indicator is bold" bold?)))

  ;; Mode-based divider hints
  (let ((shell-div (format-prompt-divider 80 'shell)))
    (test-assert "shell divider contains shell prompt tag" (string-contains shell-div "shell prompt"))
    (test-assert "shell divider contains backspace hint"
      (string-contains shell-div "Backspace: return to chat")))

  (let ((scheme-div (format-prompt-divider 80 'scheme)))
    (test-assert "scheme divider contains scheme prompt tag" (string-contains scheme-div "scheme prompt"))
    (test-assert "scheme divider contains backspace hint"
      (string-contains scheme-div "Backspace: return to chat"))))

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
    (test-equal "plain text chunk has white pair 3" 3 (cadr (car segs)))
    (test-assert "plain text chunk is not bold" (not (caddr (car segs)))))

  ;; Status tags
  (let ((segs-running (segment-line "[RUNNING]")))
    (test-equal "running status text" "[RUNNING]" (caar segs-running))
    (test-equal "running status has green pair 2" 2 (cadar segs-running))
    (test-assert "running status is bold" (caddar segs-running)))

  (let ((segs-stopped (segment-line "[STOPPED]")))
    (test-equal "stopped status text" "[STOPPED]" (caar segs-stopped))
    (test-equal "stopped status has yellow pair 4" 4 (cadar segs-stopped))
    (test-assert "stopped status is bold" (caddar segs-stopped)))

  (let ((segs-deny (segment-line "[DENY]")))
    (test-equal "deny status text" "[DENY]" (caar segs-deny))
    (test-equal "deny status has red pair 5" 5 (cadar segs-deny))
    (test-assert "deny status is bold" (caddar segs-deny)))

  ;; Bullets
  (let ((segs-bullet (segment-line "• list item")))
    (test-equal "bullet glyph text" "• " (caar segs-bullet))
    (test-equal "bullet glyph has cyan pair 1" 1 (cadar segs-bullet))
    (test-assert "bullet glyph is bold" (caddar segs-bullet))
    (test-equal "bullet content has white pair 3" 3 (cadr (cadr segs-bullet))))

  ;; Property keys
  (let ((segs-key (segment-line "Model: moonshotai/kimi-k2.6")))
    (test-equal "property key text" "Model: " (caar segs-key))
    (test-equal "property key has cyan pair 1" 1 (cadar segs-key))
    (test-equal "property value has white pair 3" 3 (cadr (cadr segs-key))))

  ;; Composite MCP line: "  • filesystem: [RUNNING] (14 tools)"
  (let ((segs-mcp (segment-line "  • filesystem: [RUNNING] (14 tools)")))
    (test-assert "composite line has multiple styled segments" (>= (length segs-mcp) 5))
    ;; Verify presence of green [RUNNING] segment
    (let ((running-seg (find (lambda (s) (string=? (car s) "[RUNNING]")) segs-mcp)))
      (test-assert "running tag found in segments" running-seg)
      (test-equal "running tag has green pair 2" 2 (cadr running-seg))
      (test-assert "running tag is bold" (caddr running-seg)))
    ;; Verify presence of cyan filesystem: key
    (let ((key-seg (find (lambda (s) (string=? (car s) "filesystem: ")) segs-mcp)))
      (test-assert "filesystem key found in segments" key-seg)
      (test-equal "filesystem key has cyan pair 1" 1 (cadr key-seg))))

  ;; URL preservation (not broken by key label regex)
  (let ((segs-url (segment-line "Endpoint: https://openrouter.ai/api/v1")))
    (test-equal "endpoint key is matched" "Endpoint: " (caar segs-url))
    (test-equal "url is preserved intact as white pair 3"
      "https://openrouter.ai/api/v1" (car (cadr segs-url)))
    (test-equal "url has white pair 3" 3 (cadr (cadr segs-url)))))

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

  ;; Summary suffixes
  (let ((s (collapsed-summary-suffix
             `((role . "assistant")
               (thoughts . "I need to think about this problem")
               (content . "x"))
             5)))
    (test-assert "thought suffix previews the reasoning text"
                 (string-contains s "I need to think"))
    (test-assert "thought suffix mentions the toggle key"
                 (string-contains s "ctrl+e"))
    (test-assert "thought suffix mentions the hidden line count"
                 (string-contains s "5")))
  (let ((s (collapsed-summary-suffix `((role . "user") (content . "hi")) 3)))
    (test-assert "generic suffix uses the collapse hint"
                 (string-contains s "(collapsed - ctrl+e to expand)"))
    (test-assert "generic suffix mentions the hidden line count"
                 (string-contains s "3 lines hidden")))

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
;;; 18. Collapse-Aware Card Rendering (format-message-entry)
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
    (test-equal "collapsed row is a card-summary row"
                "card-summary"
                (car (car content-rows)))
    (test-equal "collapsed row keeps the expanded badge"
                "👤 USER"
                (cadr (car content-rows)))
    (test-assert "collapsed suffix documents the expand key"
                 (string-contains (cadddr (car content-rows)) "ctrl+e")))

  ;; Expanded message renders the full card
  (let* ((msg `((role . "user") (content . "hello from the user")))
         (rows (format-message-entry msg 40)))
    (test-assert "expanded message renders a card-top row"
                 (any (lambda (e) (string=? (car e) "card-top")) rows))
    (test-assert "expanded message renders a card-body row"
                 (any (lambda (e) (string=? (car e) "card-body")) rows))
    (test-assert "expanded message renders a card-bot row"
                 (any (lambda (e) (string=? (car e) "card-bot")) rows))
    (test-equal "expanded message ends with a spacer row"
                "spacer"
                (car (last rows))))

  ;; Collapsed assistant reasoning: one summary row, content card visible
  (let* ((msg `((role . "assistant")
                (thoughts . "internal reasoning about the task")
                (content . "final answer")))
         (rows (format-message-entry (make-collapsed-message msg) 60))
         (summaries (filter (lambda (e) (string=? (car e) "card-summary")) rows))
         (bodies (filter (lambda (e) (string=? (car e) "card-body")) rows)))
    (test-equal "collapsed thoughts render one summary row"
                1
                (length summaries))
    (test-equal "summary row carries the thought badge"
                "💭 THOUGHT PROCESS"
                (cadr (car summaries)))
    (test-assert "summary row previews the reasoning text"
                 (string-contains (cadddr (car summaries)) "internal reasoning"))
    (test-equal "assistant content body stays visible"
                "final answer"
                (caddr (car bodies)))
    (test-assert "no thought body rows while collapsed"
                 (not (any (lambda (e)
                             (and (string=? (car e) "card-body")
                                  (string=? (cadr e) "thought")))
                           rows))))

  ;; Expanded assistant reasoning: full thought card plus content card
  (let* ((msg `((role . "assistant") (thoughts . "internal reasoning") (content . "answer")))
         (rows (format-message-entry msg 60)))
    (test-assert "expanded thoughts render thought body rows"
                 (any (lambda (e)
                        (and (string=? (car e) "card-body")
                             (string=? (cadr e) "thought")))
                      rows))
    (test-assert "expanded assistant renders content body rows"
                 (any (lambda (e)
                        (and (string=? (car e) "card-body")
                             (string=? (cadr e) "assistant")))
                      rows))
    (test-assert "expanded reasoning renders no summary rows"
                 (not (any (lambda (e) (string=? (car e) "card-summary")) rows))))

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
                 (any (lambda (e) (string=? (car e) "card-top")) rows))))

;;;
;;; 19. Interactive Menu Model (make-menu & friends)
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

(test-end "badb-tui")
