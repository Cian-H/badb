;;; (geasa surfaces-tui agent-loop) — Streaming ReAct Loop & Mid-Run Steering
;;;
;;; Manages background LLM streaming (both autonomous ReAct tool-calling and
;;; direct streaming), live transcript placeholder updates, throttled repaints,
;;; urgent steering preemption, and force-stop on <tui-state>.

(define-module (geasa surfaces-tui agent-loop)
               #:use-module
               (geasa foundation-config plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (badb kernel registry)
               #:use-module
               (geasa providers-model common)
               #:use-module
               (geasa providers-model openai)
               #:use-module
               (geasa providers-systemone plugin)
               #:use-module
               (geasa policies-guardrail plugin)
               #:use-module
               (geasa policies-systemone-gate plugin)
               #:use-module
               (geasa runtimes-context plugin)
               #:use-module
               (geasa runtimes-loop-guard plugin)
               #:use-module
               (geasa runtimes-react plugin)
               #:use-module
               (geasa capabilities-context-index plugin)
               #:use-module
               (geasa surfaces-tui session)
               #:use-module
               (geasa surfaces-tui transcript)
               #:use-module
               (geasa surfaces-tui markdown)
               #:use-module
               (geasa surfaces-tui widgets)
               #:use-module
               (geasa surfaces-tui commands)
               #:use-module
               (geasa surfaces-tui steering)
               #:use-module
               (geasa surfaces-tui tools)
               #:use-module
               (geasa surfaces-tui state)
               #:use-module
               (geasa surfaces-tui renderer)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 threads)
               #:use-module
               (srfi srfi-1)
               #:export
               (tui-format-response-images-notice
                 tui-poll-steering-input!
                 tui-update-react-stream-message!
                 tui-commit-react-stream-message!
                 tui-remove-react-stream-message!
                 tui-reset-react-stream-state!
                 tui-throttled-stream-render!
                 tui-trigger-urgent-steering!
                 tui-force-stop-run!
                 tui-execute-mid-run-safe-command!
                 tui-execute-chat-stream!))

(define*
  (tui-format-response-images-notice text #:optional (maxx 76))
  "Extract existing local image paths from Markdown ![alt](path) in TEXT
and return a notice string with placeholders and /image preview hints, or #f."
  (let
      ((paths (filter file-exists? (extract-markdown-image-paths text))))
    (if (null? paths)
      #f
      (string-join
        (map
          (lambda (p)
            (string-append
              (format-image-placeholder p #:width (max 20 (- maxx 18)))
              " — /image "
              p))
          paths)
        "\n"))))

(define (tui-poll-steering-input! st on-command!)
        "Drain queued mid-run safe commands via ON-COMMAND! and return 'stop,
'urgent-steer, or #f."
        (let
            ((pending-cmds
               (with-mutex (tui-steering-mutex st)
                           (let
                               ((cmds (reverse (tui-mid-run-pending-commands st))))
                             (set-tui-mid-run-pending-commands! st '())
                             cmds))))
          (for-each
            (lambda (pair)
              (catch #t
                (lambda ()
                  (on-command! pair))
                (lambda _
                  #f)))
            pending-cmds)
          (cond
            ((tui-force-stop-requested? st) 'stop)
            ((tui-urgent-steer-requested? st) 'urgent-steer)
            (else #f))))

(define (react-stream-placeholder-index st)
        (list-index
          (lambda (m)
            (assoc-ref m 'streaming))
          (tui-messages st)))

(define
  (tui-update-react-stream-message! st delta type)
  "Update the in-flight streaming assistant card on ST with DELTA of TYPE."
  (with-mutex (tui-render-mutex st)
              (cond
                ((eq? type 'thought)
                  (when (string? delta)
                    (set-tui-react-stream-thoughts-chunks! st
                                                           (cons delta (tui-react-stream-thoughts-chunks st))))
                  (set-tui-status-text! st "THINKING..."))
                ((eq? type 'content)
                  (when (string? delta)
                    (set-tui-react-stream-content-chunks! st
                                                          (cons delta (tui-react-stream-content-chunks st))))
                  (set-tui-status-text! st "STREAMING..."))
                ((eq? type 'usage) (tui-record-session-usage! st delta))
                ((eq? type 'tool-call)
                  (let
                      ((name (and (pair? delta) (assoc-ref delta 'name))))
                    (set-tui-status-text! st
                                          (if (non-empty-string? name)
                                            (string-append "TOOL: " name "...")
                                            "TOOL CALL..."))))
                (else #f))
              (let
                  ((react-stream-content
                     (string-concatenate (reverse (tui-react-stream-content-chunks st))))
                   (react-stream-thoughts
                     (string-concatenate (reverse (tui-react-stream-thoughts-chunks st)))))
                (call-with-values
                  (lambda ()
                    (split-thoughts-and-content react-stream-content))
                  (lambda (inline-th inline-ct)
                    (let*
                        ((total-th
                           (cond
                             ((and (non-empty-string? react-stream-thoughts) inline-th)
                               (string-append react-stream-thoughts "\n\n" inline-th))
                             ((non-empty-string? react-stream-thoughts) react-stream-thoughts)
                             (else inline-th)))
                         (updated
                           (cons (cons 'streaming #t)
                                 (default-collapse-message
                                   `((role . "assistant") (thoughts . ,total-th) (content . ,inline-ct)))))
                         (idx (react-stream-placeholder-index st)))
                      (if idx
                        (set-tui-messages! st (replace-nth (tui-messages st) idx updated))
                        (set-tui-messages! st (append (tui-messages st) (list updated))))))))))

(define (tui-commit-react-stream-message! st)
        "Drop the 'streaming flag from the streamed assistant card so it stays permanent."
        (with-mutex (tui-render-mutex st)
                    (set-tui-messages! st
                                       (map
                                         (lambda (m)
                                           (if (assoc-ref m 'streaming)
                                             (filter
                                               (lambda (p)
                                                 (not (eq? (car p) 'streaming)))
                                               m)
                                             m))
                                         (tui-messages st)))))

(define (tui-remove-react-stream-message! st)
        "Remove any in-flight 'streaming placeholder card from ST's transcript."
        (with-mutex (tui-render-mutex st)
                    (set-tui-messages! st
                                       (filter
                                         (lambda (m)
                                           (not (assoc-ref m 'streaming)))
                                         (tui-messages st)))))

(define (tui-reset-react-stream-state! st)
        "Clear ST's accumulated stream chunk buffers."
        (set-tui-react-stream-content-chunks! st '())
        (set-tui-react-stream-thoughts-chunks! st '()))

(define (tui-throttled-stream-render! st)
        "Rate-limit streaming repaints to ~15fps on ST."
        (let ((now (get-internal-real-time)))
          (when
              (>= (- now (tui-react-stream-last-render st))
                  (quotient internal-time-units-per-second 15))
            (set-tui-react-stream-last-render! st now)
            (tui-request-render! st))))

(define (tui-trigger-urgent-steering! st text)
        "Push TEXT to the front of ST's steering queue and preempt any running stream."
        (let
            ((clean
               (string-trim-both
                 (if (string? text)
                   text
                   ""))))
          (if (string-null? clean)
            (tui-add-sys-msg! st "Usage: /steer ! <message> (or Ctrl+U with steering text)")
            (begin
              (with-mutex (tui-steering-mutex st)
                          (push-urgent-steering! (tui-steering-queue st) clean))
              (when (tui-agent-running? st)
                (set-tui-urgent-steer-requested?! st #t)
                (kill-active-subprocess!))
              (tui-add-msg! st
                            "steer"
                            (if (tui-agent-running? st)
                              (format #f "Urgent steering pushed immediately: ~a" clean)
                              (format #f "Urgent steering placed at front of queue: ~a" clean)))
              (tui-request-render! st)))))

(define (tui-force-stop-run! st)
        "Immediately abort ST's active agent thread and restore READY state."
        (let ((t (tui-agent-thread st)))
          (set-tui-current-run-id! st (+ (tui-current-run-id st) 1))
          (set-tui-force-stop-requested?! st #t)
          (set-tui-urgent-steer-requested?! st #f)
          (set-tui-agent-thread! st #f)
          (force-stop-agent! t
                             #:steering-queue
                             (tui-steering-queue st)
                             #:kill-subprocess!
                             kill-active-subprocess!)
          (with-mutex (tui-steering-mutex st)
                      (let
                          ((entries (steering-drain! (tui-steering-queue st))))
                        (for-each
                          (lambda (e)
                            (unless (eq? e 'interrupt) (steering-push! (tui-steering-queue st) e)))
                          entries)))
          (tui-commit-react-stream-message! st)
          (tui-reset-react-stream-state! st)
          (set-tui-steering-buffer! st "")
          (set-tui-steering-selector-index! st 0)
          (set-tui-interrupt-requested?! st #f)
          (set-tui-status-text! st "READY")
          (tui-add-msg! st "steer" "Agent run force-stopped immediately.")
          (persist-session-file! (tui-session-file st) (tui-llm-history st))
          (tui-render! st)))

(define
  (tui-execute-mid-run-safe-command! st cmd arg on-command!)
  "Execute safe mid-run slash command CMD with ARG on ST."
  (cond
    ((string=? cmd "stop") (tui-force-stop-run! st))
    ((string=? cmd "interrupt")
      (kill-active-subprocess!)
      (with-mutex (tui-steering-mutex st)
                  (steering-push! (tui-steering-queue st) 'interrupt))
      (set-tui-interrupt-requested?! st #t)
      (set-tui-status-text! st "INTERRUPT REQUESTED")
      (tui-add-msg! st
                    "steer"
                    "Interrupt requested — halting at the next iteration boundary."))
    ((string=? cmd "steer")
      (call-with-values
        (lambda ()
          (parse-steer-argument arg))
        (lambda (urgent? text)
          (if (string-null? text)
            (tui-add-sys-msg! st "Usage: /steer [--urgent|!] <message>")
            (if urgent?
              (tui-trigger-urgent-steering! st text)
              (begin
                (with-mutex (tui-steering-mutex st)
                            (steering-push! (tui-steering-queue st) text))
                (tui-add-msg! st
                              "steer"
                              (format #f "Steering queued for the running agent: ~a" text))))))))
    (else (on-command! (cons cmd arg)))))

(define (run-react-turn! st on-command!)
        (let*
            ((cfg (tui-cfg st)) (reg (tui-reg st))
             (jrn (tui-jrn st))
             (max-react-iters (badb-config-react-max-iterations cfg))
             (loop-guard (make-loop-guard))
             (gate-options
               (systemone-gate-config->options cfg
                                               #:warn
                                               (lambda (msg)
                                                 (tui-add-sys-msg! st msg))))
             (gate-client
               (and
                 (systemone-gate-options-enabled? gate-options)
                 (make-systemone-client #:endpoint
                                        (or
                                          (systemone-gate-options-endpoint gate-options)
                                          "https://api.typesafe.ai/v1/systemone")
                                        #:api-key
                                        (or (systemone-gate-options-api-key gate-options) (getenv "TYPESAFE_API_KEY"))
                                        #:model
                                        (systemone-gate-options-model gate-options))))
             (gate
               (and gate-client
                    (make-systemone-gate #:options
                                         gate-options
                                         #:check
                                         (lambda (s)
                                           (systemone-gate-check gate-client s))
                                         #:journal
                                         jrn
                                         #:model
                                         (badb-config-model-name cfg)
                                         #:active-geasa
                                         (map
                                           (lambda (entry)
                                             (ensure-string (assoc-ref entry 'id)))
                                           (registry-list reg))
                                         #:on-verdict
                                         (lambda (action _verdict)
                                           (set-tui-status-text! st (format #f "SYSTEMONE: ~a" action))
                                           (tui-render! st)))))
             (session
               (make-react-session #:client
                                   (tui-client st)
                                   #:messages
                                   (tui-llm-history st)
                                   #:registry
                                   reg
                                   #:mcp-manager
                                   (tui-mcp-mgr st)
                                   #:journal
                                   jrn
                                   #:transaction-manager
                                   (tui-mgr st)
                                   #:max-iterations
                                   max-react-iters
                                   #:steering-queue
                                   (tui-steering-queue st)
                                   #:max-tokens
                                   (or (badb-config-model-max-tokens cfg) 4096)
                                   #:deferred-tools?
                                   #t
                                   #:offload-hook
                                   (make-ctx-offload-hook (tui-ctx-idx st))
                                   #:guardrail
                                   (lambda (t-name t-args)
                                     (evaluate-guardrail (tui-policy st) t-name t-args))
                                   #:hooks
                                   (append (make-loop-guard-hooks loop-guard)
                                           (if gate
                                             (make-systemone-gate-hooks gate)
                                             '())
                                           (list
                                             (cons 'on-step
                                                   (lambda (iter phase)
                                                     (when (eq? phase 'starting)
                                                       (tui-commit-react-stream-message! st)
                                                       (tui-reset-react-stream-state! st))
                                                     (set-tui-status-text! st
                                                                           (cond
                                                                             ((and (pair? phase) (eq? (car phase) 'rate-limited))
                                                                               (format #f
                                                                                       "RATE LIMITED (~as)..."
                                                                                       (inexact->exact (ceiling (or (cdr phase) 1)))))
                                                                             (max-react-iters (format #f "REACT [~a/~a]" iter max-react-iters))
                                                                             (else (format #f "REACT [~a]" iter))))
                                                     (when
                                                         (memq phase '(tools-executed loop-detected))
                                                       (let ((sess (current-react-session)))
                                                         (when (react-session? sess)
                                                           (set-tui-llm-history! st (react-session-messages sess))
                                                           (persist-session-file! (tui-session-file st) (tui-llm-history st)))))
                                                     (tui-poll-steering-input! st on-command!)
                                                     (tui-request-render! st)))
                                             (cons 'on-tool-call
                                                   (lambda (tc)
                                                     (let
                                                         ((t-name (prop-ref tc 'name)) (t-args (prop-ref tc 'arguments)))
                                                       (set-tui-status-text! st (format #f "TOOL: ~a..." t-name))
                                                       (tui-add-msg! st "tool-call" (format #f "Invoking tool: ~a (~s)" t-name t-args))
                                                       (tui-poll-steering-input! st on-command!)
                                                       (tui-request-render! st))))
                                             (cons 'on-tool-result
                                                   (lambda (tc res err?)
                                                     (let*
                                                         ((t-name (prop-ref tc 'name))
                                                          (blocked? (tool-result-blocked-payload? res))
                                                          (res-str
                                                            (if (string? res)
                                                              res
                                                              (safe-json-string res)))
                                                          (preview
                                                            (if (> (string-length res-str) 140)
                                                              (string-append (substring res-str 0 140) "...")
                                                              res-str)))
                                                       (tui-add-msg! st
                                                                     "tool-result"
                                                                     (cond
                                                                       (blocked? (format #f "Tool ~a blocked (duplicate call): ~a" t-name preview))
                                                                       (err? (format #f "Tool ~a failed: ~a" t-name preview))
                                                                       (else (format #f "Tool ~a returned: ~a" t-name preview))))
                                                       (tui-poll-steering-input! st on-command!)
                                                       (tui-request-render! st))))
                                             (cons 'on-delta
                                                   (lambda (delta type)
                                                     (let
                                                         ((sig (tui-poll-steering-input! st on-command!)))
                                                       (if (memq sig '(stop urgent-steer))
                                                         'stop
                                                         (begin
                                                           (tui-update-react-stream-message! st delta type)
                                                           (tui-throttled-stream-render! st)
                                                           #t)))))))
                                   #:compactor
                                   (tui-compactor st)
                                   #:loop-detector
                                   (make-loop-detector)
                                   #:recitation?
                                   #f)))
          (call-with-values
            (lambda ()
              (react-run! session))
            (lambda (verdict final-content sess)
              (set-tui-llm-history! st (react-session-messages sess))
              (cond
                ((and (tui-urgent-steer-requested? st)
                      (not (tui-force-stop-requested? st))
                      (not (steering-empty? (tui-steering-queue st))))
                  (tui-commit-react-stream-message! st)
                  (tui-reset-react-stream-state! st)
                  (set-tui-urgent-steer-requested?! st #f)
                  'urgent-continue)
                ((eq? verdict 'completed)
                  (tui-remove-react-stream-message! st)
                  (call-with-values
                    (lambda ()
                      (split-thoughts-and-content (or final-content "")))
                    (lambda (th ct)
                      (with-mutex (tui-render-mutex st)
                                  (set-tui-messages! st
                                                     (append (tui-messages st)
                                                             (list
                                                               (default-collapse-message
                                                                 `((role . "assistant") (thoughts . ,th) (content . ,ct)))))))
                      (let
                          ((widget-txt (format-response-widgets-notice ct 76))
                           (img-txt (tui-format-response-images-notice ct 76)))
                        (when widget-txt (tui-add-msg! st "notice" widget-txt))
                        (when img-txt (tui-add-msg! st "notice" img-txt)))))
                  'done)
                ((eq? verdict 'max-iterations-reached)
                  (tui-commit-react-stream-message! st)
                  (tui-add-sys-msg! st
                                    (format #f
                                            "ReAct iteration ceiling reached (~a iterations). Type 'Continue' or follow up to proceed."
                                            max-react-iters))
                  'done)
                ((eq? verdict 'interrupted)
                  (tui-commit-react-stream-message! st)
                  (tui-add-sys-msg! st
                                    "Agent run interrupted — the loop halted at the current iteration boundary. Type a message to continue.")
                  'done)
                ((eq? verdict 'loop-detected)
                  (tui-commit-react-stream-message! st)
                  (tui-add-sys-msg! st
                                    "Loop detected: the identical tool call was repeated too many times, so the agent run halted. Check the loop-detector warning in the transcript, then steer the agent with a follow-up message.")
                  'done)
                ((eq? verdict 'error)
                  (tui-commit-react-stream-message! st)
                  (tui-add-err-msg! st (format #f "ReAct error: ~a" final-content))
                  'done)
                (else 'done))))))

(define (run-direct-stream-turn! st on-command!)
        (set-tui-status-text! st "STREAMING...")
        (with-mutex (tui-render-mutex st)
                    (set-tui-messages! st
                                       (append (tui-messages st)
                                               (list
                                                 (default-collapse-message
                                                   `((role . "assistant") (thoughts . #f) (content . "")))))))
        (tui-render! st)
        (let
            ((raw-api-thoughts "") (raw-api-content "") (again? #f))
          (catch #t
            (lambda ()
              (let
                  ((target-cell (last-pair (tui-messages st))))
                (model-stream (tui-client st)
                              (tui-llm-history st)
                              (lambda (delta . type-args)
                                (let
                                    ((sig (tui-poll-steering-input! st on-command!)))
                                  (if (memq sig '(stop urgent-steer))
                                    'stop
                                    (let
                                        ((chunk-type
                                           (if (pair? type-args)
                                             (car type-args)
                                             'content)))
                                      (cond
                                        ((eq? chunk-type 'complete-json)
                                          (tui-record-session-usage! st (extract-usage delta))
                                          (call-with-values
                                            (lambda ()
                                              (determine-step-content delta))
                                            (lambda (ct rc)
                                              (set! raw-api-content (string-append raw-api-content ct))
                                              (set! raw-api-thoughts (string-append raw-api-thoughts (or rc ""))))))
                                        ((eq? chunk-type 'thought)
                                          (set! raw-api-thoughts (string-append raw-api-thoughts delta)))
                                        (else (set! raw-api-content (string-append raw-api-content delta))))
                                      (call-with-values
                                        (lambda ()
                                          (split-thoughts-and-content raw-api-content))
                                        (lambda (inline-th inline-ct)
                                          (let*
                                              ((total-th
                                                 (cond
                                                   ((and (not (string-null? raw-api-thoughts)) inline-th)
                                                     (string-append raw-api-thoughts "\n\n" inline-th))
                                                   ((not (string-null? raw-api-thoughts)) raw-api-thoughts)
                                                   (else inline-th)))
                                               (updated-msg
                                                 (default-collapse-message
                                                   `((role . "assistant") (thoughts . ,total-th) (content . ,inline-ct)))))
                                            (if (pair? target-cell)
                                              (set-car! target-cell updated-msg)
                                              (set-tui-messages! st
                                                                 (append (drop-right (tui-messages st) 1) (list updated-msg))))
                                            (tui-throttled-stream-render! st)
                                            #t)))))))
                              #:max-tokens
                              (or (badb-config-model-max-tokens (tui-cfg st)) 4096))
                (set-tui-llm-history! st
                                      (append (tui-llm-history st)
                                              `(((role . "assistant") (content . ,raw-api-content)))))
                (when
                    (and (tui-urgent-steer-requested? st)
                         (not (tui-force-stop-requested? st))
                         (not (steering-empty? (tui-steering-queue st))))
                  (let
                      ((entries
                         (with-mutex (tui-steering-mutex st) (steering-drain! (tui-steering-queue st)))))
                    (for-each
                      (lambda (e)
                        (when (string? e)
                          (set-tui-llm-history! st
                                                (append (tui-llm-history st) `(((role . "user") (content . ,e)))))))
                      entries))
                  (set-tui-urgent-steer-requested?! st #f)
                  (set! again? #t))))
            (lambda (key . args)
              (when
                  (and (string? raw-api-content)
                       (not (string-null? (string-trim-both raw-api-content))))
                (set-tui-llm-history! st
                                      (append (tui-llm-history st)
                                              (list
                                                (append
                                                  `((role . "assistant")
                                                    ,@(if (non-empty-string? raw-api-thoughts)
                                                        `((reasoning_content . ,raw-api-thoughts))
                                                        '()))
                                                  `((content . ,raw-api-content)))))))
              (tui-add-err-msg! st (format #f "Stream error (~a): ~s" key args))))
          (if again?
            'urgent-continue
            'done)))

(define
  (tui-execute-chat-stream! st user-prompt on-command!)
  "Execute USER-PROMPT against the ReAct loop or direct streaming on ST."
  (let ((my-run-id (tui-current-run-id st)))
    (set-tui-scroll-offset! st 0)
    (set-tui-session-turns! st (+ (tui-session-turns st) 1))
    (set-tui-turn-usage-seen?! st #f)
    (set-tui-turn-pre-estimate! st
                                (estimate-conversation-tokens (tui-llm-history st)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (catch #t
          (lambda ()
            (set-tui-llm-history! st
                                  (append (tui-llm-history st) `(((role . "user") (content . ,user-prompt)))))
            (when
                (and
                  (not
                    (badb-config-model-context-limit (tui-cfg st)
                                                     (tui-cur-endpoint st)
                                                     (tui-cur-model st)))
                  (or (not (tui-context-limit-discovered? st))
                      (compaction-needed? (tui-compactor st) (tui-llm-history st))))
              (tui-sync-context-limit! st))
            (when
                (compaction-needed? (tui-compactor st) (tui-llm-history st))
              (set-tui-llm-history! st
                                    (compact-messages (tui-compactor st) (tui-llm-history st))))
            (let run-turn
              ()
              (let
                  ((tools (collect-available-tools (tui-reg st) (tui-mcp-mgr st))))
                (if (pair? tools)
                  (let
                      ((res
                         (catch #t
                           (lambda ()
                             (run-react-turn! st on-command!))
                           (lambda (key . args)
                             (tui-add-err-msg! st (format #f "Agent loop failure: ~a ~s" key args))
                             'done))))
                    (when (eq? res 'urgent-continue) (run-turn)))
                  (when
                      (eq? (run-direct-stream-turn! st on-command!) 'urgent-continue)
                    (run-turn))))))
          (lambda (key . args)
            (tui-add-err-msg! st (format #f "Execution error (~a): ~s" key args)))))
      (lambda ()
        (when (= my-run-id (tui-current-run-id st))
          (unless (tui-turn-usage-seen? st)
            (let
                ((post (estimate-conversation-tokens (tui-llm-history st))))
              (when (number? post)
                (set-tui-session-estimated-tokens! st
                                                   (+ (tui-session-estimated-tokens st)
                                                      (max 0 (- post (or (tui-turn-pre-estimate st) 0))))))))
          (persist-session-file! (tui-session-file st) (tui-llm-history st))
          (set-tui-steering-buffer! st "")
          (set-tui-steering-selector-index! st 0)
          (set-tui-interrupt-requested?! st #f)
          (set-tui-urgent-steer-requested?! st #f)
          (set-tui-force-stop-requested?! st #f)
          (set-tui-status-text! st "READY")
          (set-tui-render-dirty?! st #t)
          (tui-render! st))))))