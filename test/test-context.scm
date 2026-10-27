;;; test-context.scm — Tests for Pluggable Context Compaction & Token Management (badb context)
;;;
;;; Verifies token estimators, tool pruning, sliding window recency, semantic
;;; summarization, journal offloading, composable pipelines, and compaction triggers.

(define-module (test test-context)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (badb journal)
  #:use-module (badb context))

(test-begin "badb-context")

;;;
;;; 1. Token Estimators
;;;

(test-group "context-token-estimators"
  (test-equal "empty string has 0 tokens" 0 (char-heuristic-estimator ""))
  (test-assert "16-char string estimated as ~4 tokens"
    (= 4 (char-heuristic-estimator "1234567890123456")))

  (test-equal "word estimator handles spaces"
    (inexact->exact (round (* 4 1.3)))
    (word-heuristic-estimator "one two three four"))

  (let* ((msg '((role . "user") (content . "Hello world from Dublin!")))
         (tokens (estimate-message-tokens msg)))
    (test-assert "message tokens > 5" (> tokens 5)))

  (let* ((convo (list '((role . "system") (content . "You are a helpful coding assistant."))
                      '((role . "user") (content . "Help me fix a bug."))
                      '((role . "assistant") (content . "Sure, show me the code."))))
         (total (estimate-conversation-tokens convo)))
    (test-assert "conversation tokens calculation" (> total 20))))

;;;
;;; 2. Strategy: Prune Tool Results
;;;

(test-group "context-prune-tool-results"
  (let* ((verbose-output (make-string 500 #\x))
         (messages (list
                    '((role . "system") (content . "System prompt"))
                    '((role . "user") (content . "Run git diff"))
                    `((role . "tool") (name . "git_diff") (content . ,verbose-output))
                    '((role . "assistant") (content . "Here is my reasoning."))
                    '((role . "user") (content . "Now check tests"))
                    '((role . "tool") (name . "run_tests") (content . "PASS"))
                    '((role . "assistant") (content . "All passing."))))
         (pruner (strategy-prune-tool-results #:keep-recent-turns 1 #:max-tool-chars 100))
         (compacted (pruner messages 1000 char-heuristic-estimator)))

    (test-equal "message count preserved after tool pruning"
      (length messages)
      (length compacted))

    ;; Old tool output should be pruned
    (let ((old-tool (caddr compacted)))
      (test-assert "old tool content was replaced with pruned notice"
        (string-contains (assoc-ref old-tool 'content) "[Pruned tool output")))

    ;; Recent tool output should NOT be pruned
    (let ((recent-tool (list-ref compacted 5)))
      (test-equal "recent tool content preserved intact"
        "PASS"
        (assoc-ref recent-tool 'content)))))

;;;
;;; 3. Strategy: Sliding Window with System Pinning
;;;

(test-group "context-sliding-window"
  (let* ((sys-msg '((role . "system") (content . "Always be concise.")))
         (m1 '((role . "user") (content . "Message 1")))
         (m2 '((role . "assistant") (content . "Response 1")))
         (m3 '((role . "user") (content . "Message 2")))
         (m4 '((role . "assistant") (content . "Response 2")))
         (messages (list sys-msg m1 m2 m3 m4))
         (window-fn (strategy-sliding-window #:keep-system-prompt #t))
         ;; Constrain budget so only system + last 2 messages fit
         (sys-cost (estimate-message-tokens sys-msg))
         (m3-cost (estimate-message-tokens m3))
         (m4-cost (estimate-message-tokens m4))
         (compacted (window-fn messages (+ sys-cost m3-cost m4-cost 5) char-heuristic-estimator)))

    (test-equal "system prompt is pinned at head"
      "Always be concise."
      (assoc-ref (car compacted) 'content))

    (test-equal "most recent message is preserved"
      "Response 2"
      (assoc-ref (last compacted) 'content))

    (test-assert "older message 1 was dropped"
      (not (any (lambda (m) (equal? (assoc-ref m 'content) "Message 1")) compacted)))))

;;;
;;; 4. Strategy: Summarize Older Turns
;;;

(test-group "context-summarize-older-turns"
  (let* ((messages (list
                    '((role . "system") (content . "Base prompt"))
                    '((role . "user") (content . "Turn 1"))
                    '((role . "assistant") (content . "Answer 1"))
                    '((role . "user") (content . "Turn 2"))
                    '((role . "assistant") (content . "Answer 2"))
                    '((role . "user") (content . "Turn 3"))
                    '((role . "assistant") (content . "Answer 3"))))
         (summarizer (lambda (older-msgs)
                       (format #f "Discussed ~a turns of code." (length older-msgs))))
         (strategy (strategy-summarize-older-turns summarizer #:keep-recent-turns 1))
         (compacted (strategy messages 1000 char-heuristic-estimator)))

    (test-equal "first message remains system"
      "Base prompt"
      (assoc-ref (car compacted) 'content))

    (let ((summary-msg (cadr compacted)))
      (test-assert "summary message injected"
        (string-contains (assoc-ref summary-msg 'content) "[Context Summary of preceding turns]"))
      (test-assert "summary content matches"
        (string-contains (assoc-ref summary-msg 'content) "Discussed 4 turns of code.")))

    (test-equal "most recent turn preserved"
      "Answer 3"
      (assoc-ref (last compacted) 'content))))

;;;
;;; 5. Strategy: Journal Checkpoint Offloading
;;;

(test-group "context-journal-offload"
  (let* ((jrn-path "/tmp/badb-test-context-jrn.jsonl")
         (_ (when (file-exists? jrn-path) (delete-file jrn-path)))
         (jrn (make-journal jrn-path))
         (messages (list
                    '((role . "user") (content . "Turn 1"))
                    '((role . "assistant") (content . "Turn 1 reply"))
                    '((role . "user") (content . "Turn 2"))
                    '((role . "assistant") (content . "Turn 2 reply"))
                    '((role . "user") (content . "Turn 3"))
                    '((role . "assistant") (content . "Turn 3 reply"))))
         (strategy (strategy-journal-offload jrn #:keep-recent-turns 1))
         (compacted (strategy messages 1000 char-heuristic-estimator)))

    (test-equal "journal recorded archive checkpoint"
      1
      (journal-count jrn))

    (let ((entry (car (journal-entries jrn))))
      (test-equal "archived message count is 4"
        4
        (assoc-ref entry "message-count")))

    (let ((first-msg (car compacted)))
      (test-assert "first message points to journal checkpoint"
        (string-contains (assoc-ref first-msg 'content) "archived to journal checkpoint")))))

;;;
;;; 6. Compactor Controller & Pipeline
;;;

(test-group "context-compactor-controller"
  (let* ((compactor (make-context-compactor #:max-tokens 100
                                           #:reserve-tokens 20
                                           #:strategy (strategy-sliding-window)))
         (small-messages (list '((role . "user") (content . "Short"))))
         (large-messages (map (lambda (i) `((role . "user") (content . ,(format #f "Message line number ~a in conversation" i))))
                              (iota 25))))

    (test-assert "compaction not needed for small conversation"
      (not (compaction-needed? compactor small-messages)))

    (test-assert "compaction needed for large conversation"
      (compaction-needed? compactor large-messages))

    (let ((compacted (compact-messages compactor large-messages)))
      (test-assert "compacted messages token count within ceiling"
        (<= (estimate-conversation-tokens compacted) 80)))))

(test-end "badb-context")
