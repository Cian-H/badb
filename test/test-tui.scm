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
    '("model" . "minicpm5-1b:gguf:q8") (parse-command "/model minicpm5-1b:gguf:q8"))

  (test-equal "parse /endpoint with URL"
    '("endpoint" . "http://127.0.0.1:10100/inference/v1")
    (parse-command "/endpoint http://127.0.0.1:10100/inference/v1"))

  ;; Whitespace trimming
  (test-equal "parse with leading and trailing whitespace" '("test" . "") (parse-command "   /test   "))

  (test-equal "parse argument with extra spaces"
    '("model" . "llama-3") (parse-command "  /model   llama-3  "))

  ;; Non-command inputs
  (test-assert "regular chat prompt returns #f" (not (parse-command "Why GNU Guile?")))

  (test-assert "empty string returns #f" (not (parse-command "")))

  (test-assert "string with slash in middle returns #f" (not (parse-command "and/or question"))))

;;;
;;; 3. Input Classification (classify-input)
;;;

(test-group "tui-input-classifier"
  ;; Shell commands with '!'
  (test-equal "classify !ls" '(shell . "ls -la") (classify-input "!ls -la"))

  (test-equal "classify ! with space" '(shell . "git status") (classify-input "! git status"))

  (test-equal "classify ! with trailing spaces" '(shell . "make test") (classify-input "  ! make test   "))

  (test-equal "classify bare !" '(shell . "") (classify-input "!"))

  ;; Scheme expressions with ',' or '('
  (test-equal "classify ,(+ 1 2)" '(scheme . "(+ 1 2)") (classify-input ",(+ 1 2)"))

  (test-equal "classify , (+ 1 2) with space" '(scheme . "(+ 1 2)") (classify-input ", (+ 1 2)"))

  (test-equal "classify bare ," '(scheme . "") (classify-input ","))

  (test-equal "classify direct s-expression" '(scheme . "(define x 42)") (classify-input "(define x 42)"))

  (test-equal "classify s-expression with leading whitespace"
    '(scheme . "(* x 2)") (classify-input "   (* x 2)  "))

  ;; Slash commands
  (test-equal "classify /help" '(slash "help" "") (classify-input "/help"))

  (test-equal "classify /model arg" '(slash "model" "llama3") (classify-input "/model llama3"))

  ;; Regular chat inputs
  (test-equal "classify chat message" '(chat . "Why GNU Guile?") (classify-input "Why GNU Guile?"))

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
  (test-assert "selector is active when buffer starts with /" (selector-active? "/"))
  (test-assert "selector is active when query is being typed" (selector-active? "/mod"))
  (test-assert "selector is inactive once a space is typed (argument mode)" (not (selector-active? "/model ")))
  (test-assert "selector is inactive for normal chat" (not (selector-active? "hello /help")))
  (test-assert "selector is inactive for shell command" (not (selector-active? "!ls")))

  ;; %slash-commands registry contents
  (test-assert "slash commands list contains help" (assoc "help" %slash-commands))
  (test-assert "slash commands list contains model" (assoc "model" %slash-commands))
  (test-assert "slash commands list contains endpoint" (assoc "endpoint" %slash-commands))
  (test-assert "slash commands list contains mcp" (assoc "mcp" %slash-commands))
  (test-assert "slash commands list contains config" (assoc "config" %slash-commands))
  (test-assert "slash commands list contains guardrail" (assoc "guardrail" %slash-commands))
  (test-assert "slash commands list contains compact" (assoc "compact" %slash-commands))
  (test-assert "slash commands list contains checkpoint" (assoc "checkpoint" %slash-commands))
  (test-assert "slash commands list contains rollback" (assoc "rollback" %slash-commands))
  (test-assert "slash commands list contains rules" (assoc "rules" %slash-commands))
  (test-assert "slash commands list contains context" (assoc "context" %slash-commands))
  (test-assert "slash commands list contains features" (assoc "features" %slash-commands))
  (test-assert "slash commands list contains journal" (assoc "journal" %slash-commands))
  (test-assert "slash commands list contains test" (assoc "test" %slash-commands))
  (test-assert "slash commands list contains sh" (assoc "sh" %slash-commands))
  (test-assert "slash commands list contains eval" (assoc "eval" %slash-commands))
  (test-assert "slash commands list contains clear" (assoc "clear" %slash-commands))
  (test-assert "slash commands list contains quit" (assoc "quit" %slash-commands))

  ;; matching-slash-commands filtering
  (let ((all-matches (matching-slash-commands "/")))
    (test-equal "bare / returns all slash commands" (length %slash-commands) (length all-matches)))

  (let ((m-matches (matching-slash-commands "/m")))
    (test-assert "matches commands containing m" (member "model" (map car m-matches))))

  (let ((exact-matches (matching-slash-commands "/jour")))
    (test-equal "matches journal command" '("journal") (map car exact-matches)))

  (let ((no-matches (matching-slash-commands "/xyznonexistent")))
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

(test-end "badb-tui")
