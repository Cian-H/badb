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
  #:use-module (badb tui))

(test-begin "badb-tui")

;;;
;;; 1. Word Wrapping (wrap-line and wrap-text)
;;;

(test-group "tui-wrapping"
  (let ((short-line "hello world"))
    (test-equal "short line is not wrapped"
      '("hello world")
      (wrap-line short-line 20)))

  (let* ((text "This is a sentence that should wrap nicely at word boundaries.")
         (wrapped (wrap-line text 20)))
    (test-assert "wrapped produces multiple lines"
      (> (length wrapped) 1))
    (test-assert "every wrapped line is within max width"
      (every (lambda (l) (<= (string-length l) 20)) wrapped))
    (test-equal "reconstructed string preserves all words"
      text
      (string-join wrapped " ")))

  ;; Edge cases: single word exceeding max width
  (let* ((long-word "supercalifragilisticexpialidocious")
         (wrapped (wrap-line long-word 10)))
    (test-assert "long word is broken into chunks"
      (> (length wrapped) 1))
    (test-assert "all chunks <= max width"
      (every (lambda (l) (<= (string-length l) 10)) wrapped))
    (test-equal "concatenated chunks equal original word"
      long-word
      (apply string-append wrapped)))

  ;; Multi-line text wrapping
  (let* ((multi "Line one\nLine two is longer and needs to wrap\nLine three")
         (wrapped (wrap-text multi 15)))
    (test-assert "multi-line wrapped produces list of lines"
      (> (length wrapped) 3))
    (test-assert "all lines are <= 15 chars"
      (every (lambda (l) (<= (string-length l) 15)) wrapped))))

;;;
;;; 2. Command Parsing (parse-command)
;;;

(test-group "tui-command-parser"
  ;; Basic commands without arguments
  (test-equal "parse /help"
    '("help" . "")
    (parse-command "/help"))

  (test-equal "parse /quit"
    '("quit" . "")
    (parse-command "/quit"))

  (test-equal "parse /features"
    '("features" . "")
    (parse-command "/features"))

  ;; Commands with arguments
  (test-equal "parse /model with argument"
    '("model" . "minicpm5-1b:gguf:q8")
    (parse-command "/model minicpm5-1b:gguf:q8"))

  (test-equal "parse /endpoint with URL"
    '("endpoint" . "http://127.0.0.1:10100/inference/v1")
    (parse-command "/endpoint http://127.0.0.1:10100/inference/v1"))

  ;; Whitespace trimming
  (test-equal "parse with leading and trailing whitespace"
    '("test" . "")
    (parse-command "   /test   "))

  (test-equal "parse argument with extra spaces"
    '("model" . "llama-3")
    (parse-command "  /model   llama-3  "))

  ;; Non-command inputs
  (test-assert "regular chat prompt returns #f"
    (not (parse-command "Why GNU Guile?")))

  (test-assert "empty string returns #f"
    (not (parse-command "")))

  (test-assert "string with slash in middle returns #f"
    (not (parse-command "and/or question"))))

;;;
;;; 3. Input Classification (classify-input)
;;;

(test-group "tui-input-classifier"
  ;; Shell commands with '!'
  (test-equal "classify !ls"
    '(shell . "ls -la")
    (classify-input "!ls -la"))

  (test-equal "classify ! with space"
    '(shell . "git status")
    (classify-input "! git status"))

  (test-equal "classify ! with trailing spaces"
    '(shell . "make test")
    (classify-input "  ! make test   "))

  (test-equal "classify bare !"
    '(shell . "")
    (classify-input "!"))

  ;; Scheme expressions with ',' or '('
  (test-equal "classify ,(+ 1 2)"
    '(scheme . "(+ 1 2)")
    (classify-input ",(+ 1 2)"))

  (test-equal "classify , (+ 1 2) with space"
    '(scheme . "(+ 1 2)")
    (classify-input ", (+ 1 2)"))

  (test-equal "classify bare ,"
    '(scheme . "")
    (classify-input ","))

  (test-equal "classify direct s-expression"
    '(scheme . "(define x 42)")
    (classify-input "(define x 42)"))

  (test-equal "classify s-expression with leading whitespace"
    '(scheme . "(* x 2)")
    (classify-input "   (* x 2)  "))

  ;; Slash commands
  (test-equal "classify /help"
    '(slash "help" "")
    (classify-input "/help"))

  (test-equal "classify /model arg"
    '(slash "model" "llama3")
    (classify-input "/model llama3"))

  ;; Regular chat inputs
  (test-equal "classify chat message"
    '(chat . "Why GNU Guile?")
    (classify-input "Why GNU Guile?"))

  (test-assert "classify empty string is #f"
    (not (classify-input "")))

  (test-assert "classify whitespace is #f"
    (not (classify-input "    "))))

;;;
;;; 4. Scheme REPL Evaluation (eval-scheme-code & make-repl-mod)
;;;

(test-group "tui-scheme-eval"
  (let* ((reg (make-registry))
         (jrn (make-journal "/tmp/badb-test-tui-jrn.jsonl"))
         (mgr (make-transaction-manager reg #:journal jrn))
         (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "dummy"))
         (repl-mod (make-repl-mod reg jrn mgr client)))

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

    ;; Error handling: numerical overflow / division by zero
    (call-with-values (lambda () (eval-scheme-code "(/ 1 0)" repl-mod))
      (lambda (res err)
        (test-assert "division by zero returns error string" (string? err))
        (test-assert "error mentions numerical-overflow"
          (string-contains err "numerical-overflow"))))

    ;; Error handling: syntax error
    (call-with-values (lambda () (eval-scheme-code "(+ 1" repl-mod))
      (lambda (res err)
        (test-assert "incomplete syntax returns error" (string? err))
        (test-assert "error mentions read-error"
          (string-contains err "read-error"))))))

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

(test-end "badb-tui")
