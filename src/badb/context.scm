;;; badb — Pluggable Context Compaction & Token Management
;;;
;;; Heavily customizable context window governance and compaction engine.
;;; Supports multiple pluggable token estimation algorithms and compaction
;;; strategies: surgical tool-result pruning, semantic summarization,
;;; sliding-window recency, journal-checkpoint offloading, and composable pipelines.

(define-module (badb context)
  #:use-module (badb model)
  #:use-module (badb journal)
  #:use-module (json)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<context-compactor>
            make-context-compactor
            context-compactor?
            context-compactor-max-tokens
            context-compactor-reserve-tokens
            context-compactor-estimator
            context-compactor-strategy
            context-compactor-pinned-roles
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

            ;; Compaction Application
            compact-messages
            compaction-needed?))

;;;
;;; Token Estimators
;;;

(define (char-heuristic-estimator text)
  "Heuristic estimation assuming ~4 characters per token (OpenAI/Llama average)."
  (if (or (not (string? text)) (string-null? text))
      0
      (max 1 (quotient (+ (string-length text) 3) 4))))

(define (word-heuristic-estimator text)
  "Heuristic estimation assuming ~1.3 tokens per whitespace-separated word."
  (if (or (not (string? text)) (string-null? text))
      0
      (let* ((words (filter (lambda (w) (not (string-null? w)))
                            (string-split text #\space)))
             (wcount (length words)))
        (max 1 (inexact->exact (round (* wcount 1.3)))))))

(define* (estimate-message-tokens msg #:optional (estimator char-heuristic-estimator))
  "Estimate token count of a single message alist."
  (let* ((role (or (assoc-ref msg "role") (assoc-ref msg 'role) ""))
         (content (or (assoc-ref msg "content") (assoc-ref msg 'content) ""))
         (tool-calls (or (assoc-ref msg "tool_calls") (assoc-ref msg 'tool_calls) #f))
         (role-str (cond
                    ((symbol? role) (symbol->string role))
                    ((string? role) role)
                    (else "")))
         (content-str (cond
                       ((string? content) content)
                       ((pair? content) (scm->json-string content))
                       (else "")))
         (tc-str (cond
                  ((vector? tool-calls) (scm->json-string (vector->list tool-calls)))
                  ((pair? tool-calls) (scm->json-string tool-calls))
                  (else ""))))
    (+ 4 ;; message framing overhead
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

(define* (strategy-prune-tool-results #:key
                                     (keep-recent-turns 2)
                                     (max-tool-chars 300)
                                     (placeholder-generator #f))
  "Compaction strategy that prunes verbose tool outputs from turns older than
KEEP-RECENT-TURNS while preserving model reasoning and tool call status."
  (lambda (messages target-tokens estimator)
    (let* ((total-len (length messages))
           (threshold-idx (max 0 (- total-len (* keep-recent-turns 2)))))
      (let loop ((msgs messages)
                 (idx 0)
                 (acc '()))
        (if (null? msgs)
            (reverse acc)
            (let* ((msg (car msgs))
                   (role (or (assoc-ref msg "role") (assoc-ref msg 'role)))
                   (role-str (cond
                              ((symbol? role) (symbol->string role))
                              ((string? role) role)
                              (else "")))
                   (content (or (assoc-ref msg "content") (assoc-ref msg 'content) "")))
              (if (and (< idx threshold-idx)
                       (string=? role-str "tool")
                       (string? content)
                       (> (string-length content) max-tool-chars))
                  (let* ((lines (string-split content #\newline))
                         (stub (if (procedure? placeholder-generator)
                                   (placeholder-generator msg)
                                   (format #f "[Pruned tool output (~a lines, ~a chars) — completed]"
                                           (length lines) (string-length content))))
                         (updated (map (lambda (pair)
                                         (if (or (eq? (car pair) 'content) (equal? (car pair) "content"))
                                             (cons (car pair) stub)
                                             pair))
                                       msg)))
                    (loop (cdr msgs) (+ idx 1) (cons updated acc)))
                  (loop (cdr msgs) (+ idx 1) (cons msg acc)))))))))

(define* (strategy-sliding-window #:key (keep-system-prompt #t))
  "Compaction strategy that keeps the newest turns fitting within target tokens,
optionally pinning the system prompt at position 0."
  (lambda (messages target-tokens estimator)
    (if (null? messages)
        '()
        (let* ((has-system? (and keep-system-prompt
                                 (let ((r (or (assoc-ref (car messages) "role")
                                              (assoc-ref (car messages) 'role))))
                                   (and r (string=? (if (symbol? r) (symbol->string r) r) "system")))))
               (sys-msg (and has-system? (car messages)))
               (pool (if has-system? (cdr messages) messages))
               (sys-tokens (if has-system? (estimate-message-tokens sys-msg estimator) 0))
               (avail-tokens (max 0 (- target-tokens sys-tokens))))
          (let loop ((rest (reverse pool))
                     (acc '())
                     (cur-tokens 0))
            (if (null? rest)
                (if has-system? (cons sys-msg acc) acc)
                (let* ((msg (car rest))
                       (cost (estimate-message-tokens msg estimator)))
                  (if (and (> cur-tokens 0) (> (+ cur-tokens cost) avail-tokens))
                      ;; Exceeded token budget: terminate window
                      (if has-system? (cons sys-msg acc) acc)
                      (loop (cdr rest) (cons msg acc) (+ cur-tokens cost))))))))))

(define* (strategy-summarize-older-turns summarize-fn
                                       #:key
                                       (keep-recent-turns 3))
  "Compaction strategy that summarizes older conversation turns using SUMMARIZE-FN.
SUMMARIZE-FN receives a list of older messages and returns a summary string."
  (lambda (messages target-tokens estimator)
    (let* ((total-len (length messages))
           (has-system? (and (pair? messages)
                             (let ((r (or (assoc-ref (car messages) "role")
                                          (assoc-ref (car messages) 'role))))
                               (and r (string=? (if (symbol? r) (symbol->string r) r) "system")))))
           (sys-msg (and has-system? (car messages)))
           (pool (if has-system? (cdr messages) messages))
           (recent-count (* keep-recent-turns 2))
           (split-point (max 0 (- (length pool) recent-count))))
      (if (<= split-point 0)
          messages
          (let* ((older-msgs (take pool split-point))
                 (recent-msgs (drop pool split-point))
                 (summary-text (summarize-fn older-msgs))
                 (summary-msg `((role . "system")
                                (content . ,(format #f "[Context Summary of preceding turns]:\n~a" summary-text)))))
            (if has-system?
                (cons sys-msg (cons summary-msg recent-msgs))
                (cons summary-msg recent-msgs)))))))

(define* (strategy-journal-offload jrn #:key (keep-recent-turns 2))
  "Compaction strategy that logs pruned conversation blocks to JRN and replaces
them with audit trail checkpoint pointers."
  (lambda (messages target-tokens estimator)
    (let* ((total (length messages))
           (threshold (max 0 (- total (* keep-recent-turns 2)))))
      (if (<= threshold 0)
          messages
          (let* ((to-offload (take messages threshold))
                 (kept (drop messages threshold))
                 (chkpt-id (format #f "context-archive-~a" (current-time))))
            (when (and jrn (journal? jrn))
              (catch #t
                (lambda ()
                  (journal-append! jrn
                                   `((feature-id . "context")
                                     (checkpoint . ,chkpt-id)
                                     (status . "archived")
                                     (message-count . ,(length to-offload))
                                     (messages-json . ,(scm->json-string (list->vector to-offload))))))
                (lambda _ #f)))
            (let ((ref-msg `((role . "system")
                             (content . ,(format #f "[Prior ~a turns archived to journal checkpoint '~a']"
                                                 (length to-offload) chkpt-id)))))
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

(define* (make-context-compactor #:key
                                 (max-tokens 8192)
                                 (reserve-tokens 1024)
                                 (estimator char-heuristic-estimator)
                                 (strategy (strategy-pipeline
                                            (strategy-prune-tool-results)
                                            (strategy-sliding-window)))
                                 (pinned-roles '("system")))
  "Construct a new <context-compactor> instance."
  (%make-context-compactor max-tokens reserve-tokens estimator strategy pinned-roles))

;;;
;;; Compaction Application
;;;

(define (compaction-needed? compactor messages)
  "Return #t if MESSAGES exceed the effective token ceiling of COMPACTOR."
  (let* ((max-t (context-compactor-max-tokens compactor))
         (res-t (context-compactor-reserve-tokens compactor))
         (ceiling (max 1 (- max-t res-t)))
         (estimator (context-compactor-estimator compactor))
         (current (estimate-conversation-tokens messages estimator)))
    (> current ceiling)))

(define* (compact-messages compactor messages #:key (force #f))
  "Apply the configured strategy to MESSAGES if token count exceeds ceiling or FORCE is #t."
  (let* ((max-t (context-compactor-max-tokens compactor))
         (res-t (context-compactor-reserve-tokens compactor))
         (ceiling (max 1 (- max-t res-t)))
         (estimator (context-compactor-estimator compactor))
         (strategy (context-compactor-strategy compactor)))
    (if (or force (compaction-needed? compactor messages))
        (strategy messages ceiling estimator)
        messages)))
