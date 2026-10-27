;;; badb — Pluggable Context Compaction & Token Management
;;;
;;; Heavily customizable context window governance and compaction engine.
;;; Supports multiple pluggable token estimation algorithms and compaction
;;; strategies: surgical tool-result pruning, semantic summarization,
;;; sliding-window recency, journal-checkpoint offloading, and composable pipelines.

(define-module (badb context) #:use-module
               (badb model)
               #:use-module
               (badb journal)
               #:use-module
               (json)
               #:use-module
               (ice-9 format)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:export
               (<context-compactor> make-context-compactor
                                    context-compactor?
                                    context-compactor-max-tokens
                                    context-compactor-reserve-tokens
                                    context-compactor-estimator
                                    context-compactor-strategy
                                    context-compactor-pinned-roles
                                    context-compactor-effective-ceiling
                                    set-context-compactor-max-tokens!
                                    set-context-compactor-reserve-tokens!
                                    set-context-compactor-estimator!
                                    set-context-compactor-strategy!
                                    ;; Token Estimators
                                    char-heuristic-estimator
                                    word-heuristic-estimator
                                    estimate-message-tokens
                                    estimate-conversation-tokens
                                    ;; Pluggable Compaction Strategies
                                    strategy-prune-tool-results
                                    strategy-sliding-window
                                    strategy-summarize-older-turns
                                    strategy-journal-offload
                                    strategy-pipeline
                                    strategy-cascading
                                    strategy-cascading-pipeline
                                    ;; Compaction Application
                                    compact-messages
                                    compaction-needed?))

;;;
;;; Token Estimators
;;;
(define (non-empty-string? str)
        "Return #t if STR is a non-empty string."
        (and (string? str) (not (string-null? str))))

(define (message-role msg)
        "Extract normalized role string from MSG."
        (let
            ((role (or (assoc-ref msg "role") (assoc-ref msg 'role))))
          (cond
            ((symbol? role) (symbol->string role))
            ((string? role) role)
            (else ""))))

(define (message-content msg)
        "Extract content field from MSG alist."
        (or (assoc-ref msg "content") (assoc-ref msg 'content) ""))

(define (content->string content)
        "Convert content to a string representation for token estimation."
        (cond
          ((string? content) content)
          ((pair? content) (scm->json-string content))
          (else "")))

(define (tool-calls->string tool-calls)
        "Convert tool-calls structure to a string representation."
        (cond
          ((vector? tool-calls) (scm->json-string (vector->list tool-calls)))
          ((pair? tool-calls) (scm->json-string tool-calls))
          (else "")))

(define (char-heuristic-estimator text)
        "Heuristic estimation assuming ~4 characters per token (OpenAI/Llama average)."
        (if (non-empty-string? text)
          (max 1 (quotient (+ (string-length text) 3) 4))
          0))

(define (word-heuristic-estimator text)
        "Heuristic estimation assuming ~1.3 tokens per whitespace-separated word."
        (if (non-empty-string? text)
          (let*
              ((words (filter non-empty-string? (string-split text #\space)))
               (wcount (length words)))
            (max 1 (inexact->exact (round (* wcount 1.3)))))
          0))

(define*
  (estimate-message-tokens msg #:optional (estimator char-heuristic-estimator))
  "Estimate token count of a single message alist."
  (let*
      ((role-str (message-role msg))
       (content-str (content->string (message-content msg)))
       (tool-calls (or (assoc-ref msg "tool_calls") (assoc-ref msg 'tool_calls) #f))
       (tc-str (tool-calls->string tool-calls)))
    (+ 4
       ;; message framing overhead
       (estimator role-str)
       (estimator content-str)
       (estimator tc-str))))

(define* (estimate-conversation-tokens messages #:optional (estimator char-heuristic-estimator))
  "Calculate aggregate token count for a list or vector of messages."
  (let ((msg-list (if (vector? messages) (vector->list messages) messages)))
    (fold (lambda (msg acc) (+ acc (estimate-message-tokens msg estimator)))
          2 ;; conversation priming tokens
          msg-list)))

;;;
;;; Compaction Strategies
;;;
(define (system-message? msg)
        "Return #t if MSG has role 'system'."
        (and (pair? msg) (string=? (message-role msg) "system")))

(define (partition-system-prompt messages keep?)
        "Split MESSAGES into (cons sys-msg-or-#f pool)."
        (if
            (and keep? (pair? messages) (system-message? (car messages)))
          (cons (car messages) (cdr messages))
          (cons #f messages)))

(define (attach-system sys-msg msgs)
        "Prepend SYS-MSG to MSGS if present."
        (if sys-msg
          (cons sys-msg msgs)
          msgs))

(define
  (prune-single-tool-output msg max-chars placeholder-generator)
  "Prune MSG if it is a tool output exceeding MAX-CHARS."
  (let ((content (message-content msg)))
    (if
        (and (string=? (message-role msg) "tool")
             (string? content)
             (> (string-length content) max-chars))
      (let*
          ((lines (string-split content #\newline))
           (stub
             (if (procedure? placeholder-generator)
               (placeholder-generator msg)
               (format #f
                       "[Pruned tool output (~a lines, ~a chars) — completed]"
                       (length lines)
                       (string-length content)))))
        (map
          (lambda (pair)
            (if
                (or (eq? (car pair) 'content) (equal? (car pair) "content"))
              (cons (car pair) stub)
              pair))
          msg))
      msg)))

(define
  (journal-record-offload! jrn chkpt-id to-offload)
  "Record archived messages to journal JRN."
  (when (and jrn (journal? jrn))
    (catch #t
      (lambda ()
        (journal-append! jrn
                         `((feature-id . "context")
                           (checkpoint . ,chkpt-id)
                           (status . "archived")
                           (message-count . ,(length to-offload))
                           (messages-json . ,(scm->json-string (list->vector to-offload))))))
      (lambda _
        #f))))

(define*
  (strategy-prune-tool-results #:key
                               (keep-recent-turns 2)
                               (max-tool-chars 300)
                               (placeholder-generator #f))
  "Compaction strategy that prunes verbose tool outputs from turns older than
KEEP-RECENT-TURNS while preserving model reasoning and tool call status."
  (lambda (messages _target-tokens _estimator)
    (let*
        ((total-len (length messages))
         (threshold-idx (max 0 (- total-len (* keep-recent-turns 2)))))
      (let loop
        ((msgs messages) (idx 0) (acc '()))
        (if (null? msgs)
          (reverse acc)
          (let*
              ((msg (car msgs))
               (processed
                 (if (< idx threshold-idx)
                   (prune-single-tool-output msg max-tool-chars placeholder-generator)
                   msg)))
            (loop (cdr msgs)
                  (+ idx 1)
                  (cons processed acc))))))))

(define*
  (strategy-sliding-window #:key (keep-system-prompt #t))
  "Compaction strategy that keeps the newest turns fitting within target tokens,
optionally pinning the system prompt at position 0."
  (lambda (messages target-tokens estimator)
    (if (null? messages)
      '()
      (let*
          ((partition (partition-system-prompt messages keep-system-prompt))
           (sys-msg (car partition))
           (pool (cdr partition))
           (sys-tokens
             (if sys-msg
               (estimate-message-tokens sys-msg estimator)
               0))
           (avail-tokens (max 0 (- target-tokens sys-tokens))))
        (let loop
          ((rest (reverse pool)) (acc '()) (cur-tokens 0))
          (if (null? rest)
            (attach-system sys-msg acc)
            (let*
                ((msg (car rest)) (cost (estimate-message-tokens msg estimator)))
              (if
                  (and (> cur-tokens 0) (> (+ cur-tokens cost) avail-tokens))
                (attach-system sys-msg acc)
                (loop (cdr rest)
                      (cons msg acc)
                      (+ cur-tokens cost))))))))))

(define*
  (strategy-summarize-older-turns summarize-fn #:key (keep-recent-turns 3))
  "Compaction strategy that summarizes older conversation turns using SUMMARIZE-FN.
SUMMARIZE-FN receives a list of older messages and returns a summary string."
  (lambda (messages _target-tokens _estimator)
    (let*
        ((partition (partition-system-prompt messages #t))
         (sys-msg (car partition))
         (pool (cdr partition))
         (recent-count (* keep-recent-turns 2))
         (split-point (max 0 (- (length pool) recent-count))))
      (if (<= split-point 0)
        messages
        (let*
            ((older-msgs (take pool split-point))
             (recent-msgs (drop pool split-point))
             (summary-text (summarize-fn older-msgs))
             (summary-msg `((role . "system")
                            (content .
                                     ,(format #f
                                              "[Context Summary of preceding turns]:
~a"
                                              summary-text)))))
          (attach-system sys-msg (cons summary-msg recent-msgs)))))))

(define*
  (strategy-journal-offload jrn #:key (keep-recent-turns 2))
  "Compaction strategy that logs pruned conversation blocks to JRN and replaces
them with audit trail checkpoint pointers."
  (lambda (messages _target-tokens _estimator)
    (let*
        ((total (length messages))
         (threshold (max 0 (- total (* keep-recent-turns 2)))))
      (if (<= threshold 0)
        messages
        (let*
            ((to-offload (take messages threshold))
             (kept (drop messages threshold))
             (chkpt-id (format #f "context-archive-~a" (current-time))))
          (journal-record-offload! jrn chkpt-id to-offload)
          (let
              ((ref-msg `((role . "system")
                          (content .
                                   ,(format #f
                                            "[Prior ~a turns archived to journal checkpoint '~a']"
                                            (length to-offload)
                                            chkpt-id)))))
            (cons ref-msg kept)))))))

(define (strategy-pipeline . strategies)
  "Combine multiple compaction strategies in sequence."
  (lambda (messages target-tokens estimator)
    (fold (lambda (strat msgs)
            (if (<= (estimate-conversation-tokens msgs estimator) target-tokens)
                msgs
                (strat msgs target-tokens estimator)))
          messages
          strategies)))

(define*
  (strategy-cascading #:key
                      (summarize-fn #f)
                      (journal #f)
                      (keep-recent-turns 2)
                      (max-tool-chars 300)
                      (keep-system-prompt #t)
                      (placeholder-generator #f))
  "Cascading compaction pipeline: pruning -> summarization -> journal-offload -> sliding-window.
Executes each configured strategy in order until the token ceiling is satisfied."
  (let
      ((stages
         (filter procedure?
                 (list
                   (strategy-prune-tool-results #:keep-recent-turns
                                                keep-recent-turns
                                                #:max-tool-chars
                                                max-tool-chars
                                                #:placeholder-generator
                                                placeholder-generator)
                   (and summarize-fn
                        (strategy-summarize-older-turns summarize-fn
                                                        #:keep-recent-turns
                                                        keep-recent-turns))
                   (and journal
                        (strategy-journal-offload journal #:keep-recent-turns keep-recent-turns))
                   (strategy-sliding-window #:keep-system-prompt keep-system-prompt)))))
    (apply strategy-pipeline stages)))

(define strategy-cascading-pipeline strategy-cascading)

;;;
;;; Context Compactor Record
;;;
(define-record-type <context-compactor>
  (%make-context-compactor max-tokens reserve-tokens estimator strategy pinned-roles)
  context-compactor?
  (max-tokens context-compactor-max-tokens set-context-compactor-max-tokens!)
  (reserve-tokens context-compactor-reserve-tokens set-context-compactor-reserve-tokens!)
  (estimator context-compactor-estimator set-context-compactor-estimator!)
  (strategy context-compactor-strategy set-context-compactor-strategy!)
  (pinned-roles context-compactor-pinned-roles))

(define*
  (make-context-compactor #:key
                          (max-tokens 8192)
                          (reserve-tokens 1024)
                          (estimator char-heuristic-estimator)
                          (strategy (strategy-cascading))
                          (pinned-roles '("system")))
  "Construct a new <context-compactor> instance."
  (%make-context-compactor max-tokens
                           reserve-tokens
                           estimator
                           strategy
                           pinned-roles))

;;;
;;; Compaction Application
;;;
(define
  (context-compactor-effective-ceiling compactor)
  "Calculate effective token ceiling (max-tokens minus reserve-tokens)."
  (max 1
       (- (context-compactor-max-tokens compactor)
          (context-compactor-reserve-tokens compactor))))

(define (compaction-needed? compactor messages)
        "Return #t if MESSAGES exceed the effective token ceiling of COMPACTOR."
        (let
            ((ceiling (context-compactor-effective-ceiling compactor))
             (estimator (context-compactor-estimator compactor)))
          (> (estimate-conversation-tokens messages estimator) ceiling)))

(define*
  (compact-messages compactor messages #:key (force #f))
  "Apply the configured strategy to MESSAGES if token count exceeds ceiling or FORCE is #t."
  (if
      (or force (compaction-needed? compactor messages))
    (let
        ((ceiling (context-compactor-effective-ceiling compactor))
         (estimator (context-compactor-estimator compactor))
         (strategy (context-compactor-strategy compactor)))
      (strategy messages ceiling estimator))
    messages))