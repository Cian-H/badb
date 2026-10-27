;;; test-tui.scm — Tests for (badb tui)
;;;
;;; Verifies text wrapping, command parsing, and TUI helper functions.

(define-module (test test-tui)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
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

(test-end "badb-tui")
