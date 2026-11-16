;;; (geasa surfaces-tui steering) — Mid-Run Steering & Subagent Summary Helpers
;;;
;;; Provides mid-run safe-command classification, /steer argument parsing,
;;; urgent steering queue preemption, force-stop orchestration, steering bar
;;; layout, and subagent hierarchy formatting.

(define-module (geasa surfaces-tui steering)
               #:use-module (badb kernel registry)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa runtimes-react plugin)
               #:use-module (geasa agents-lifecycle plugin)
               #:use-module (geasa agents-coordination plugin)
               #:use-module (geasa agents-subagent plugin)
               #:use-module (geasa surfaces-tui session)
               #:use-module (ice-9 format)
               #:use-module (ice-9 threads)
               #:use-module (srfi srfi-1)
               #:export (%mid-run-safe-commands
                          %mid-run-slash-command-entries
                          mid-run-safe-command?
                          parse-steer-argument
                          dispatch-mid-run-input
                          matching-mid-run-slash-commands
                          push-urgent-steering!
                          force-stop-agent!
                          format-steering-keymap-line
                          steering-bar-layout
                          format-mid-run-subagents-summary))

(define %steering-op-mutex (make-mutex))

(define %mid-run-safe-commands
        '("context" "usage" "theme" "help" "?" "subagents" "stop" "steer" "interrupt"))

(define %mid-run-slash-command-entries
        '(("context" . "View conversation token stats and metrics")
          ("usage" . "Show per-session token usage report")
          ("theme" . "Inspect or switch the active UI theme")
          ("help" . "Show help manual & shortcuts")
          ("subagents" . "Inspect active subagents and hierarchy")
          ("stop" . "Force-stop the running agent immediately")
          ("steer" . "Queue or urgently push steering (! / --urgent)")
          ("interrupt" . "Halt the running agent at its next boundary")))

(define (mid-run-safe-command? cmd)
        "Return #t when CMD (with or without leading '/') is safe to invoke while
%agent-thread is running."
        (and (string? cmd)
             (let* ((trimmed (string-trim-both cmd))
                    (bare (if (string-prefix? "/" trimmed) (substring trimmed 1) trimmed))
                    (space-idx (string-index bare #\space))
                    (name (string-downcase (if space-idx (substring bare 0 space-idx) bare))))
               (and (not (string-null? name))
                    (member name %mid-run-safe-commands)
                    #t))))

(define (parse-steer-argument arg)
        "Parse /steer ARG for an urgency prefix ('--urgent ' or '! ').
Return (values urgent? cleaned-text)."
        (let ((trimmed (string-trim-both (if (string? arg) arg ""))))
          (cond
            ((string-prefix? "--urgent " trimmed)
              (values #t (string-trim-both (substring trimmed 9))))
            ((string=? "--urgent" trimmed)
              (values #t ""))
            ((string-prefix? "! " trimmed)
              (values #t (string-trim-both (substring trimmed 2))))
            ((and (> (string-length trimmed) 1) (string-prefix? "!" trimmed))
              (values #t (string-trim-both (substring trimmed 1))))
            (else (values #f trimmed)))))

(define (dispatch-mid-run-input raw-input)
        "Classify RAW-INPUT typed into the steering bar while the agent runs.
Returns one of:
  '(noop)
  '(safe-slash CMD ARG)
  '(blocked-slash CMD ARG)
  '(steer . TEXT)"
        (let ((trimmed (string-trim-both (if (string? raw-input) raw-input ""))))
          (if (string-null? trimmed)
            '(noop)
            (let ((classified (classify-input trimmed)))
              (if (and (pair? classified) (eq? (car classified) 'slash))
                (let ((cmd (cadr classified)) (arg (caddr classified)))
                  (if (mid-run-safe-command? cmd)
                    (list 'safe-slash (string-downcase cmd) arg)
                    (list 'blocked-slash cmd arg)))
                (cons 'steer trimmed))))))

(define (matching-mid-run-slash-commands buffer)
        "Return matching %mid-run-slash-command-entries for slash BUFFER."
        (let* ((trimmed (if (string? buffer) buffer ""))
               (prefix (if (string-prefix? "/" trimmed) (substring trimmed 1) trimmed))
               (q (string-downcase (string-trim-both prefix))))
          (if (string-null? q)
            %mid-run-slash-command-entries
            (filter
              (lambda (entry) (string-contains (string-downcase (car entry)) q))
              %mid-run-slash-command-entries))))

(define* (push-urgent-steering! queue text
                                #:key
                                (on-urgent #f)
                                (kill-subprocess! #f))
         "Immediately push TEXT to the front of STEERING-QUEUE ahead of any
already queued steering entries. Optionally invokes KILL-SUBPROCESS! and
(ON-URGENT clean-text). Returns #t if pushed, #f otherwise."
         (let ((clean (and (string? text) (string-trim-both text))))
           (if (or (not (steering-queue? queue)) (not clean) (string-null? clean))
             #f
             (begin
               (with-mutex %steering-op-mutex
                           (let ((existing (steering-drain! queue)))
                             (steering-push! queue clean)
                             (for-each (lambda (entry) (steering-push! queue entry)) existing)))
               (when (procedure? kill-subprocess!)
                 (catch #t (lambda () (kill-subprocess!)) (lambda _ #f)))
               (when (procedure? on-urgent)
                 (catch #t (lambda () (on-urgent clean)) (lambda _ #f)))
               #t))))

(define* (force-stop-agent! thread
                            #:key
                            (steering-queue #f)
                            (kill-subprocess! kill-active-subprocess!)
                            (on-stop #f))
         "Immediately force-stop a running agent THREAD by killing any active
subprocess, pushing 'interrupt onto STEERING-QUEUE, and calling cancel-thread.
Returns an alist: ((stopped? . BOOL) (thread-cancelled? . BOOL) (subprocess-killed? . BOOL))."
         (let ((subproc-killed? #f) (thread-cancelled? #f))
           (when (procedure? kill-subprocess!)
             (let ((had-pid?
                     (catch #t
                       (lambda ()
                         (let ((pid (active-subprocess-pid)))
                           (and (number? pid) (> pid 0))))
                       (lambda _ #f))))
               (catch #t (lambda () (kill-subprocess!)) (lambda _ #f))
               (set! subproc-killed? (and had-pid? #t))))
           (when (steering-queue? steering-queue)
             (with-mutex %steering-op-mutex
                         (steering-push! steering-queue 'interrupt)))
           (when (and (thread? thread) (not (thread-exited? thread)))
             (catch #t
               (lambda ()
                 (cancel-thread thread)
                 (set! thread-cancelled? #t))
               (lambda _ #f)))
           (when (procedure? on-stop)
             (catch #t (lambda () (on-stop)) (lambda _ #f)))
           `((stopped? . ,(or thread-cancelled? subproc-killed?))
             (thread-cancelled? . ,thread-cancelled?)
             (subprocess-killed? . ,subproc-killed?))))

(define* (format-steering-keymap-line #:optional (slash-mode? #f))
         "Return the dim bottom keymap hint for the redesigned mid-run steering bar."
         (if slash-mode?
           "enter:run cmd · tab:complete · esc:clear · ^c:force-stop"
           "enter:queue · ^u:urgent · /:cmd · ^i:interrupt · ^c/esc:force-stop"))

(define* (steering-bar-layout buffer queued-count maxx
                              #:key
                              (interrupt-requested? #f)
                              (urgent-pending? #f)
                              (status-text #f))
         "Compute the redesigned mid-run steering UI bar layout for BUFFER,
QUEUED-COUNT, and terminal width MAXX."
         (let* ((safe-buf (if (string? buffer) buffer ""))
                (safe-q (if (and (integer? queued-count) (> queued-count 0)) queued-count 0))
                (safe-w (max 12 (if (integer? maxx) maxx 80)))
                (slash-mode? (string-prefix? "/" safe-buf))
                (badge (cond
                         (interrupt-requested? "[INTERRUPT]")
                         (urgent-pending? "[URGENT]")
                         (slash-mode? "[CMD]")
                         (else "[STEER]")))
                (badge-role (cond
                              (interrupt-requested? 'error)
                              (urgent-pending? 'warning)
                              (else 'accent)))
                (queue-summary (and (> safe-q 0) (format #f "+~a queued" safe-q)))
                (status-part (and (string? status-text)
                                  (not (string-null? (string-trim-both status-text)))
                                  (string-trim-both status-text)))
                (meta-parts (filter identity (list queue-summary status-part)))
                (meta-str (if (null? meta-parts)
                            ""
                            (string-append " " (string-join meta-parts " · "))))
                (raw-header (string-append badge meta-str))
                (header-line (if (> (string-length raw-header) safe-w)
                               (substring raw-header 0 safe-w)
                               raw-header))
                (avail-in (max 1 (- safe-w 3)))
                (placeholder? (string-null? safe-buf))
                (placeholder-text
                  "type steering, or /context /usage /theme /help /subagents /stop...")
                (input-text (if placeholder?
                              (if (> (string-length placeholder-text) avail-in)
                                (substring placeholder-text 0 avail-in)
                                placeholder-text)
                              (let ((len (string-length safe-buf)))
                                (if (> len avail-in)
                                  (substring safe-buf (- len avail-in) len)
                                  safe-buf))))
                (cursor-col (if placeholder? 0 (string-length input-text))))
           `((mode . ,(if slash-mode? 'slash 'steer))
             (badge . ,badge)
             (badge-role . ,badge-role)
             (queue-summary . ,queue-summary)
             (meta-line . ,meta-str)
             (header-line . ,header-line)
             (input-text . ,input-text)
             (placeholder? . ,placeholder?)
             (cursor-col . ,cursor-col)
             (hint . ,(format-steering-keymap-line slash-mode?)))))

(define (format-mid-run-subagents-summary reg)
        "Format an overview of active/available subagents using WP5 hierarchy &
event-bus overview, falling back to registered subagent tools in REG."
        (let* ((links (all-subagent-links))
               (events (harness-event-poll #:limit 5))
               (overview (format-subagent-overview #:max-width 76)))
          (if (and (null? links) (null? events) reg)
            (let* ((sub-tools '("investigate" "investigate_parallel" "subagent_transaction" "remote_subagent"))
                   (registered (filter (lambda (name) (registry-lookup reg name)) sub-tools)))
              (if (null? registered)
                overview
                (string-append overview "\nRegistered tools: " (string-join registered ", "))))
            overview)))
