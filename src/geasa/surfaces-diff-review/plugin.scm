;;; (geasa surfaces-diff-review plugin) — Guardrail Approval & /diff Review Modal

(define-module (geasa surfaces-diff-review plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (make-guardrail-approval-overlay
                          make-diff-review-overlay
                          register-diff-review-tui!
                          diff-review-state?
                          diff-review-files
                          diff-review-selected-file
                          diff-review-status-message
                          guardrail-approval-state?
                          guardrail-approval-decision))

;;;
;;; Guardrail Approval Overlay
;;;
(define-record-type <guardrail-approval-state>
                    (%make-guardrail-approval-state risk title command preview detail decision)
                    guardrail-approval-state?
                    (risk guardrail-approval-risk)
                    (title guardrail-approval-title)
                    (command guardrail-approval-command)
                    (preview guardrail-approval-preview)
                    (detail guardrail-approval-detail)
                    (decision guardrail-approval-decision set-guardrail-approval-decision!))

(define (risk->role risk)
        (case risk
          ((high critical error) 'error)
          ((medium warn warning) 'warn)
          (else 'ok)))

(define* (make-guardrail-approval-overlay
           #:key
           (risk 'medium)
           (title "Guardrail Approval Required")
           (command "")
           (preview "")
           (detail "")
           (on-decision #f))
         "Create a guardrail approval modal <overlay> with [a] Allow once, [A] Session allow, [d] Deny."
         (let ((st (%make-guardrail-approval-state risk title command preview detail #f)))
           (define (decide! ov choice key-ctx)
                   (set-guardrail-approval-decision! st choice)
                   (when (procedure? on-decision)
                     (on-decision choice))
                   (let ((on-close! (assq-ref key-ctx 'on-close!)))
                     (when (procedure? on-close!) (on-close!)))
                   'close)
           (make-modal-overlay
             #:title title
             #:state st
             #:width 0.72
             #:height 0.55
             #:show-detail? #f
             #:body-renderer
             (lambda (ov state rect)
               (let* ((badge (status-badge-widget (risk->role (guardrail-approval-risk state))
                                                  (symbol->string (guardrail-approval-risk state))))
                      (cmd-line (text-widget (format #f "Command: ~a" (guardrail-approval-command state)) #:bold? #t))
                      (prev-text (guardrail-approval-preview state))
                      (prev-lines (if (string-null? prev-text)
                                    '()
                                    (list (rule-widget)
                                          (text-widget prev-text #:dim? #f)))))
                 (vstack-widget
                   (append (list (hstack-widget (list (text-widget "Risk: ") badge))
                                 cmd-line)
                           prev-lines))))
             #:detail-renderer
             (lambda (ov state rect)
               (let ((d (guardrail-approval-detail state)))
                 (text-widget (if (string-null? d) "No additional detail." d))))
             #:key-bindings
             `((#\a . ,(lambda (ov st key-ctx) (decide! ov 'allow-once key-ctx)))
               (#\A . ,(lambda (ov st key-ctx) (decide! ov 'allow-session key-ctx)))
               (#\d . ,(lambda (ov st key-ctx) (decide! ov 'deny key-ctx)))
               (enter . ,(lambda (ov st key-ctx)
                           (set-modal-show-detail?! ov (not (modal-show-detail? ov)))
                           'handled)))
             #:footer-hints '(("a" . "Allow once")
                              ("A" . "Session allow")
                              ("d" . "Deny")
                              ("Enter" . "Detail")
                              ("Esc" . "Close")))))

;;;
;;; /diff Workspace Review Overlay
;;;
(define-record-type <diff-review-state>
                    (%make-diff-review-state files list diff-fn on-revert status-message confirm-revert?)
                    diff-review-state?
                    (files diff-review-files set-diff-review-files!)
                    (list diff-review-list set-diff-review-list!)
                    (diff-fn diff-review-diff-fn)
                    (on-revert diff-review-on-revert)
                    (status-message diff-review-status-message set-diff-review-status-message!)
                    (confirm-revert? diff-review-confirm-revert? set-diff-review-confirm-revert?!))

(define (diff-review-selected-file st)
        (list-selected-item (diff-review-list st)))

(define (entry-path entry)
        (cond
          ((string? entry) entry)
          ((pair? entry) (or (assq-ref entry 'path) (car entry)))
          (else (format #f "~a" entry))))

(define (entry-status entry)
        (if (pair? entry)
          (or (assq-ref entry 'status) "M")
          "M"))

(define* (make-diff-review-overlay
           #:key
           (files '())
           (diff-fn (lambda (_entry) ""))
           (on-revert #f))
         "Create a /diff workspace review modal <overlay> with file list, unified diff preview, and revert."
         (let* ((slist (make-selectable-list files #:max-visible 12))
                (st (%make-diff-review-state files slist diff-fn on-revert "" #f)))
           (make-modal-overlay
             #:title "Workspace Diff Review (/diff)"
             #:state st
             #:width 0.85
             #:height 0.80
             #:show-detail? #t
             #:body-renderer
             (lambda (ov state rect)
               (let ((msg (diff-review-status-message state))
                     (lw (max 20 (quotient (rect-width rect) 2))))
                 (vstack-widget
                   (append
                     (if (string-null? msg)
                       '()
                       (list (text-widget msg #:role 'warn #:bold? #t)))
                     (list
                       (selectable-list-widget
                         (diff-review-list state)
                         #:item-renderer
                         (lambda (entry selected?)
                           (let* ((p (entry-path entry))
                                  (s (entry-status entry))
                                  (role (cond
                                          ((string-contains s "D") 'error)
                                          ((or (string-contains s "A")
                                               (string-contains s "?")) 'ok)
                                          ((string-contains s "M") 'warn)
                                          ((string-contains s "R") 'info)
                                          (else 'accent))))
                             (hstack-widget
                               (list
                                 (text-widget (format #f "[~a] " s)
                                              #:role role
                                              #:reverse? selected?
                                              #:bold? #t)
                                 (text-widget p
                                              #:role (if selected? role #f)
                                              #:reverse? selected?
                                              #:bold? selected?)))))))))))
             #:detail-renderer
             (lambda (ov state rect)
               (let ((sel (diff-review-selected-file state)))
                 (if sel
                   (let ((diff-text ((diff-review-diff-fn state) sel)))
                     (if (and diff-text (not (string-null? diff-text)))
                       (let ((lines (string-split diff-text #\newline)))
                         (vstack-widget
                           (map (lambda (ln)
                                  (cond
                                    ((or (string-prefix? "diff " ln)
                                         (string-prefix? "+++" ln)
                                         (string-prefix? "---" ln)
                                         (string-prefix? "index " ln))
                                      (text-widget ln #:role 'accent #:bold? #t))
                                    ((string-prefix? "@@" ln)
                                      (text-widget ln #:role 'info))
                                    ((string-prefix? "+" ln)
                                      (text-widget ln #:role 'ok))
                                    ((string-prefix? "-" ln)
                                      (text-widget ln #:role 'error))
                                    (else
                                      (text-widget ln #:role 'dim))))
                                lines)
                           #:gap 0))
                       (text-widget "No diff hunks." #:dim? #t)))
                   (text-widget "No file selected." #:dim? #t))))
             #:key-bindings
             `((#\j . ,(lambda (ov st _ctx)
                         (set-diff-review-confirm-revert?! st #f)
                         (set-diff-review-list! st (list-move (diff-review-list st) 1))
                         'handled))
               (#\k . ,(lambda (ov st _ctx)
                         (set-diff-review-confirm-revert?! st #f)
                         (set-diff-review-list! st (list-move (diff-review-list st) -1))
                         'handled))
               (down . ,(lambda (ov st _ctx)
                          (set-diff-review-confirm-revert?! st #f)
                          (set-diff-review-list! st (list-move (diff-review-list st) 1))
                          'handled))
               (up . ,(lambda (ov st _ctx)
                        (set-diff-review-confirm-revert?! st #f)
                        (set-diff-review-list! st (list-move (diff-review-list st) -1))
                        'handled))
               (enter . ,(lambda (ov st _ctx)
                           (set-modal-show-detail?! ov (not (modal-show-detail? ov)))
                           'handled))
               (#\r . ,(lambda (ov st _ctx)
                         (let ((sel (diff-review-selected-file st)))
                           (when sel
                             (if (diff-review-confirm-revert? st)
                               (begin
                                 (set-diff-review-confirm-revert?! st #f)
                                 (when (procedure? (diff-review-on-revert st))
                                   ((diff-review-on-revert st) sel))
                                 (set-diff-review-status-message!
                                   st
                                   (format #f "Reverted ~a" (entry-path sel))))
                               (begin
                                 (set-diff-review-confirm-revert?! st #t)
                                 (set-diff-review-status-message!
                                   st
                                   (format #f "Press r again to revert ~a" (entry-path sel))))))
                           'handled)))
               (#\q . ,(lambda (_ov _st _ctx) 'close)))
             #:footer-hints '(("j/k" . "Navigate")
                              ("Enter" . "Diff")
                              ("r" . "Revert")
                              ("q/Esc" . "Close")))))

(define* (register-diff-review-tui!
           #:key
           (register-category! (lambda (_p _d) #f))
           (register-command! (lambda (_n _d _a . _r) #f))
           (open-overlay! (lambda (_ov) #f))
           (files-provider (lambda () '()))
           (diff-fn (lambda (_entry) ""))
           (on-revert #f))
         "Register /diff command and Workspace category in TUI."
         (register-category! '("Workspace") "Workspace inspection and diff review")
         (register-command!
           "diff"
           "Review modified workspace files and diff hunks"
           (lambda _args
             (open-overlay!
               (make-diff-review-overlay
                 #:files (files-provider)
                 #:diff-fn diff-fn
                 #:on-revert on-revert)))))
