;;; test-context.scm — Tests for Pluggable Context Compaction & Token Management (geasa runtimes-context plugin)
;;;
;;; Verifies token estimators, tool pruning, sliding window recency, semantic
;;; summarization, journal offloading, composable pipelines, and compaction triggers.

(define-module (test test-context)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (srfi srfi-1)
               #:use-module (badb kernel journal)
               #:use-module (geasa runtimes-context plugin))

(spec-begin "runtimes-context")

;;;
;;; 1. Token Estimators
;;;
(test-group "context-token-estimators"
            (test-equal "empty string has 0 tokens" 0 (char-heuristic-estimator ""))
            (test-assert "16-char string estimated as ~4 tokens" (= 4 (char-heuristic-estimator "1234567890123456")))

            (test-equal "word estimator handles spaces"
                        (inexact->exact (round (* 4 1.3))) (word-heuristic-estimator "one two three four"))

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

              (test-equal "message count preserved after tool pruning" (length messages) (length compacted))

              ;; Old tool output should be pruned
              (let ((old-tool (caddr compacted)))
                (test-assert "old tool content was replaced with pruned notice"
                             (string-contains (assoc-ref old-tool 'content) "[Pruned tool output")))

              ;; Recent tool output should NOT be pruned
              (let ((recent-tool (list-ref compacted 5)))
                (test-equal "recent tool content preserved intact" "PASS" (assoc-ref recent-tool 'content)))))

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

              (test-equal "system prompt is pinned at head" "Always be concise." (assoc-ref (car compacted) 'content))

              (test-equal "most recent message is preserved" "Response 2" (assoc-ref (last compacted) 'content))

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

              (test-equal "first message remains system" "Base prompt" (assoc-ref (car compacted) 'content))

              (let ((summary-msg (cadr compacted)))
                (test-assert "summary message injected"
                             (string-contains (assoc-ref summary-msg 'content) "[Context Summary of preceding turns]"))
                (test-assert "summary content matches"
                             (string-contains (assoc-ref summary-msg 'content) "Discussed 4 turns of code.")))

              (test-equal "most recent turn preserved" "Answer 3" (assoc-ref (last compacted) 'content))))

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

              (test-equal "journal recorded archive checkpoint" 1 (journal-count jrn))

              (let ((entry (car (journal-entries jrn))))
                (test-equal "archived message count is 4" 4 (assoc-ref entry "message-count")))

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

              (test-assert "compaction needed for large conversation" (compaction-needed? compactor large-messages))

              (let ((compacted (compact-messages compactor large-messages)))
                (test-assert "compacted messages token count within ceiling"
                             (<= (estimate-conversation-tokens compacted) 80)))))

(test-group "compaction-retains-user-query"
            ;; Regression: a tool-heavy conversation whose newest turns are all
            ;; assistant/tool messages used to evict the single user query from the
            ;; sliding window, producing transcripts with no user message that the
            ;; endpoint rejects ("No user query found in messages").
            (let* ((user-msg '((role . "user") (content . "please inspect the whole repository carefully")))
                   (tool-chatter
                     (append
                       (list '((role . "system") (content . "You are the badb kernel agent."))
                             user-msg)
                       (append-map
                         (lambda (i)
                           (list
                             `((role . "assistant")
                               (tool_calls . #(((id . ,(format #f "c~a" i))
                                                (type . "function")
                                                (function . ((name . "inspect")
                                                             (arguments . "{}")))))))
                             `((role . "tool") (tool_call_id . ,(format #f "c~a" i))
                               (name . "inspect")
                               (content . ,(format #f "inspection result number ~a with a verbose summary of the repository state" i)))))
                         (iota 30))))
                   (compactor (make-context-compactor #:max-tokens 120 #:reserve-tokens 20))
                   (compacted (compact-messages compactor tool-chatter)))
              (test-assert "compaction triggered"
                           (< (length compacted) (length tool-chatter)))
              (test-assert "a user message survives compaction"
                           (latest-user-message compacted))
              (test-assert "the retained user message is the original query"
                           (equal? (assoc-ref (latest-user-message compacted) 'content)
                                   (assoc-ref user-msg 'content)))
              (test-assert "system prompt stays pinned at position 0"
                           (equal? (assoc-ref (car compacted) 'role) "system"))
              (test-equal "latest-user-message finds newest of several"
                          "second question"
                          (assoc-ref (latest-user-message
                                       (list '((role . "user") (content . "first question"))
                                             '((role . "assistant") (content . "answer"))
                                             '((role . "user") (content . "second question"))
                                             '((role . "assistant") (content . "answer 2"))))
                                     'content))
              (test-assert "latest-user-message returns #f without user turns"
                           (not (latest-user-message
                                  (list '((role . "assistant") (content . "hi"))
                                        '((role . "tool") (content . "result"))))))))

(test-group
  "context-sliding-window-pin-first-user"
  ;; Regression (dogfood round 1): once steering messages arrive, the
  ;; sliding window could evict the ORIGINAL task message, leaving the
  ;; agent without its assignment. The observed failure mode was the
  ;; agent endlessly re-reading its task file.
  (let*
      ((sys '((role . "system") (content . "You are the badb kernel agent.")))
       (task `((role . "user") (content . ,(make-string 200 #\t))))
       (chatter
         (append-map
           (lambda (i)
             (list
               `((role . "assistant")
                 (content .
                          ,(format #f
                                   "verbose working turn ~a with filler text padding its token cost well past the window budget"
                                   i)))
               `((role . "tool")
                 (tool_call_id . ,(format #f "c~a" i))
                 (name . "inspect")
                 (content . ,(make-string 90 #\x)))))
           (iota 8)))
       (steer '((role . "user") (content . "steer now")))
       (messages (append (list sys task) chatter (list steer)))
       (budget 120)
       (pin-fn (strategy-sliding-window #:pin-first-user #t))
       (plain-fn (strategy-sliding-window))
       (pinned (pin-fn messages budget char-heuristic-estimator))
       (plain (plain-fn messages budget char-heuristic-estimator)))
    (test-assert
      "pinned window retains the original task message"
      (any
        (lambda (m)
          (equal? (assoc-ref m 'content) (make-string 200 #\t)))
        pinned))
    (test-assert
      "pinned window retains the newest user message too"
      (equal? "steer now" (assoc-ref (latest-user-message pinned) 'content)))
    (test-assert
      "plain window drops the original task (old behavior)"
      (not
        (any
          (lambda (m)
            (equal? (assoc-ref m 'content) (make-string 200 #\t)))
          plain)))
    (test-assert "cascading strategy threads the pin"
                 (let*
                     ((casc
                        (make-context-compactor #:max-tokens
                                                140
                                                #:reserve-tokens
                                                20
                                                #:strategy
                                                (strategy-cascading #:pin-first-user #t))))
                   (any
                     (lambda (m)
                       (equal? (assoc-ref m 'content) (make-string 200 #\t)))
                     (compact-messages casc messages))))))

;;;
;;; 7. Watermark Compaction Trigger
;;;
(test-group "context-watermark-trigger"
            (let* ((comp75 (make-context-compactor #:max-tokens 100
                                                   #:reserve-tokens 20
                                                   #:watermark 0.75))
                   (comp100 (make-context-compactor #:max-tokens 100
                                                    #:reserve-tokens 20
                                                    #:watermark 1.0))
                   ;; Create messages that estimate to ~70 tokens (between 61 and 80)
                   (messages (list
                               '((role . "system") (content . "System"))
                               '((role . "user") (content . "A user message with some content here"))
                               '((role . "assistant") (content . "Assistant response with more text"))
                               '((role . "user") (content . "Another user query"))
                               '((role . "assistant") (content . "Another assistant response")))))

              (test-equal "effective trigger at 0.75 watermark with ceiling 80" 60
                          (context-compactor-effective-trigger comp75))

              (test-assert "compaction NOT needed at watermark 1.0 for ~70 tokens"
                           (not (compaction-needed? comp100 messages)))

              (test-assert "compaction IS needed at watermark 0.75 for ~70 tokens"
                           (compaction-needed? comp75 messages))))

;;;
;;; 8. Summary-of-Summaries Protection
;;;
(test-group "context-summary-exclusion"
            (let* ((summarize-calls '())
                   (summarizer (lambda (older-msgs)
                                 (set! summarize-calls (cons older-msgs summarize-calls))
                                 (format #f "Summarized ~a messages" (length older-msgs))))
                   (strategy (strategy-summarize-older-turns summarizer #:keep-recent-turns 1))
                   (prior-summary '((role . "system")
                                    (content . "[Context Summary of preceding turns]: Previous discussion about X")))
                   (messages (list
                               '((role . "system") (content . "Base prompt"))
                               '((role . "user") (content . "First turn"))
                               '((role . "assistant") (content . "First answer"))
                               prior-summary
                               '((role . "user") (content . "Second turn"))
                               '((role . "assistant") (content . "Second answer"))
                               '((role . "user") (content . "Third turn"))
                               '((role . "assistant") (content . "Third answer"))))
                   (compacted (strategy messages 1000 char-heuristic-estimator)))

              (test-assert "prior summary was NOT passed to summarize-fn"
                           (not (any (lambda (call)
                                       (any (lambda (msg)
                                              (let ((content (or (assoc-ref msg 'content) (assoc-ref msg "content") "")))
                                                (string-prefix? "[Context Summary of preceding turns]" content)))
                                            call))
                                     summarize-calls)))

              (test-assert "compacted output contains exactly one summary message"
                           (= 1 (count (lambda (msg)
                                         (let ((content (or (assoc-ref msg 'content) (assoc-ref msg "content") "")))
                                           (string-prefix? "[Context Summary of preceding turns]" content)))
                                       compacted)))

              (test-equal "system message stays at index 0"
                          "Base prompt" (assoc-ref (car compacted) 'content))))

(test-group "tool-token-estimate"
            (test-equal "empty string has 0 tokens" 0 (tool-token-estimate ""))
            (test-equal "100 chars has ~26 tokens" 26 (tool-token-estimate (make-string 100 #\x)))
            (test-assert "tool schema object estimates non-zero tokens"
                         (> (tool-token-estimate (quote ((name . "read_file") (description . "Read file contents")))) 5)))

(test-group "strategy-prune-reasoning"
            (let* ((msgs (list
                           (quote ((role . "system") (content . "sys")))
                           (quote ((role . "user") (content . "q1")))
                           (quote ((role . "assistant") (reasoning_content . "think 1") (content . "a1")))
                           (quote ((role . "user") (content . "q2")))
                           (quote ((role . "assistant") (reasoning_content . "think 2") (content . "a2")))
                           (quote ((role . "user") (content . "q3")))
                           (quote ((role . "assistant") (reasoning_content . "think 3") (content . "a3")))))
                   (pruned-k1 (strategy-prune-reasoning msgs 1))
                   (pruned-k2 (strategy-prune-reasoning msgs 2))
                   (pruned-k0 (strategy-prune-reasoning msgs 0)))

              ;; k=1: only turn 3 retains reasoning
              (test-equal "msgs count unchanged" 7 (length pruned-k1))
              (test-assert "turn 1 reasoning stripped"
                           (not (assoc-ref (list-ref pruned-k1 2) (quote reasoning_content))))
              (test-assert "turn 2 reasoning stripped"
                           (not (assoc-ref (list-ref pruned-k1 4) (quote reasoning_content))))
              (test-equal "turn 3 reasoning preserved"
                          "think 3" (assoc-ref (list-ref pruned-k1 6) (quote reasoning_content)))

              ;; k=2: turn 2 and 3 retain reasoning, turn 1 stripped
              (test-assert "k=2: turn 1 stripped"
                           (not (assoc-ref (list-ref pruned-k2 2) (quote reasoning_content))))
              (test-equal "k=2: turn 2 preserved"
                          "think 2" (assoc-ref (list-ref pruned-k2 4) (quote reasoning_content)))
              (test-equal "k=2: turn 3 preserved"
                          "think 3" (assoc-ref (list-ref pruned-k2 6) (quote reasoning_content)))

              ;; k=0: all reasoning stripped
              (test-assert "k=0: turn 1 stripped"
                           (not (assoc-ref (list-ref pruned-k0 2) (quote reasoning_content))))
              (test-assert "k=0: turn 2 stripped"
                           (not (assoc-ref (list-ref pruned-k0 4) (quote reasoning_content))))
              (test-assert "k=0: turn 3 stripped"
                           (not (assoc-ref (list-ref pruned-k0 6) (quote reasoning_content))))))

(test-group
  "compaction-preserves-tool-boundaries"
  (let*
      ((msgs
         (list '((role . "system") (content . "sys"))
               '((role . "user") (content . "q1"))
               '((role . "assistant") (content . "a1"))
               '((role . "assistant") (content . "")
                 (tool_calls .
                  #(((id . "c1") (type . "function")
                     (function . ((name . "read_file") (arguments . "{}")))))))
               '((role . "tool") (tool_call_id . "c1") (name . "read_file") (content . "r1"))))
       (strat
         (strategy-summarize-older-turns
           (lambda (_)
             "summary")
           #:keep-recent-turns
           1))
       (out (strat msgs 1000 char-heuristic-estimator)))
    ;; keep-recent-turns 1 -> raw split would land on the tool message;
    ;; boundary alignment must keep the assistant+tool pair together.
    (test-assert
      "no orphaned tool message at start of recent turns"
      (not
        (any
          (lambda (m)
            (and (equal? (assoc-ref m 'role) "tool")
                 (not
                   (any
                     (lambda (a)
                       (and (equal? (assoc-ref a 'role) "assistant") (assoc-ref a 'tool_calls)))
                     out))))
          out)))))

(spec-end)
(spec-exit)
