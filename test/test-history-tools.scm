;;; test-history-tools.scm — Tests for Conversation History Summarization (badb history-tools)
;;;
;;; Verifies summarize-turns against deterministic fixtures (exact turn
;;; counts, role counts, token estimate, and topic sketch assertions),
;;; summarize-history-file against a temp JSONL session file (including a
;;; malformed line to skip), and the edge cases: empty turn list, empty
;;; file, and missing file.

(define-module (test test-history-tools)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (badb history-tools))

(test-begin "badb-history-tools")

;;;
;;; 1. Deterministic fixture for summarize-turns
;;;
(define %fixture-turns
  (list '((role . "system") (content . "You are badb, a terminal agent."))
        '((role . "user") (content . "The alpha compiler rewrites beta gamma delta"))
        '((role . "assistant") (content . "Understood, examining the compiler."))
        '((role . "user") (content . "The alpha module fails when gamma loads"))
        '((role . "assistant") (content . "Reproduced the module failure."))
        '((role . "user") (content . "Fix the alpha gamma bug now please"))))

(define %fixture-summary-lines
  (string-split (summarize-turns %fixture-turns) #\newline))

(test-group "history-summarize-turns-fixture"
  (test-equal "six turns in the fixture" 6 (length %fixture-turns))
  (test-equal "summary has eight lines" 8 (length %fixture-summary-lines))
  (test-equal "total turns line"
    "Total turns: 6"
    (list-ref %fixture-summary-lines 0))
  (test-equal "token estimate line matches context heuristic"
    "Estimated tokens: 92"
    (list-ref %fixture-summary-lines 1))
  (test-equal "role counts sorted alphabetically"
    "Turns by role: assistant=2, system=1, user=3"
    (list-ref %fixture-summary-lines 2))
  (test-equal "recent user messages section newest first"
    "Recent user messages:"
    (list-ref %fixture-summary-lines 3))
  (test-equal "most recent user message numbered 1"
    "  1. Fix the alpha gamma bug now please"
    (list-ref %fixture-summary-lines 4))
  (test-equal "older user message numbered 3"
    "  3. The alpha compiler rewrites beta gamma delta"
    (list-ref %fixture-summary-lines 6))
  (test-equal "topic sketch: counts desc then alphabetical ties"
    "Topics: alpha gamma beta bug compiler delta fails fix"
    (string-join
      (filter (lambda (line) (string-prefix? "Topics: " line))
              %fixture-summary-lines)
      "")))

;;;
;;; 2. summarize-turns edge cases
;;;
(test-group "history-summarize-turns-edges"
  (test-equal "empty turn list summarizes to zero counts"
    "Total turns: 0
Estimated tokens: 2
Turns by role: (none)
Recent user messages:
  (none)
Topics: (none)"
    (summarize-turns '()))
  (test-equal "vector input is accepted"
    "Total turns: 1
Estimated tokens: 9
Turns by role: user=1
Recent user messages:
  1. hello
Topics: hello"
    (summarize-turns (list->vector '(((role . "user") (content . "hello"))))))
  (test-equal "long user message truncated to ~80 chars"
    (string-append "  1. " (make-string 77 #\z) "...")
    (list-ref
      (string-split
        (summarize-turns
          (list `((role . "user") (content . ,(string-append (make-string 80 #\z) " tail")))))
        #\newline)
      4))
  (let ((summary
          (summarize-turns
            (map (lambda (i) `((role . "user") (content . ,(format #f "message ~a" i))))
                 (iota 7)))))
    (test-equal "only the five most recent user messages appear"
      5
      (length
        (filter (lambda (line) (string-prefix? "  " line))
                (string-split summary #\newline))))
    (test-assert "most recent of seven listed first"
      (string-contains summary "  1. message 6"))
    (test-assert "oldest of seven is dropped"
      (not (string-contains summary "message 0")))))

;;;
;;; 3. summarize-history-file against a temp JSONL file
;;;
(define %temp-jsonl-path
  (string-append "/tmp/badb-test-history-tools-" (number->string (getpid)) ".jsonl"))

(define (write-temp-jsonl! lines)
  (call-with-output-file %temp-jsonl-path
    (lambda (port)
      (for-each (lambda (line) (display line port) (newline port)) lines))))

(test-group "history-summarize-history-file"
  (let ((summary
          (begin
            (write-temp-jsonl!
              (list "{\"role\":\"user\",\"content\":\"First question about streams\"}"
                    "not valid json at all"
                    "{\"role\":\"assistant\"}"
                    "{\"content\":\"missing valid role\",\"role\":123}"
                    "{\"role\":\"assistant\",\"content\":\"Answering the streams question\"}"))
            (summarize-history-file %temp-jsonl-path))))
    (test-assert "malformed lines skipped, file summarized" summary)
    (test-equal "two well-formed turns counted"
      "Total turns: 2"
      (list-ref (string-split summary #\newline) 0))
    (test-equal "role counts reflect only parsed turns"
      "Turns by role: assistant=1, user=1"
      (list-ref (string-split summary #\newline) 2))
    (test-assert "topic sketch drawn from the user message"
      (string-contains summary "Topics: first question streams")))
  (test-equal "empty file summarizes to zero turns"
    "Total turns: 0"
    (list-ref
      (string-split
        (begin
          (write-temp-jsonl! '())
          (summarize-history-file %temp-jsonl-path))
        #\newline)
      0))
  (test-equal "missing file yields #f"
    #f
    (summarize-history-file "/tmp/badb-no-such-history-file.jsonl"))
  (delete-file %temp-jsonl-path))

(test-end "badb-history-tools")
