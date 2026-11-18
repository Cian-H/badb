;;; (geasa surfaces-agent-monitor plugin) — /agents Subagent Mission Control Modal

(define-module (geasa surfaces-agent-monitor plugin)
               #:use-module (geasa agents-coordination plugin)
               #:use-module (geasa agents-subagent plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa surfaces-notify plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (agent-monitor-state?
                          agent-monitor-entries
                          agent-monitor-selected-entry
                          agent-monitor-status-message
                          set-agent-monitor-status-message!
                          agent-monitor-spinner-frame
                          agent-monitor-tick!
                          agent-monitor-refresh!
                          make-agent-monitor-overlay
                          register-agent-monitor-tui!))

;;;
;;; State & Defaults
;;;
(define %default-notify-info! notify-info!)
(define %default-notify-warn! notify-warn!)

(define-record-type <agent-monitor-state>
                    (%make-agent-monitor-state tab
                                               all-entries
                                               entries
                                               list
                                               filter-query
                                               filtered-list
                                               status-message
                                               spinner-frame
                                               list-agents-fn
                                               get-events-fn
                                               steer-agent-fn
                                               stop-agent-fn
                                               notify-info-fn
                                               notify-warn-fn)
                    agent-monitor-state?
                    (tab agent-monitor-tab set-agent-monitor-tab!)
                    (all-entries agent-monitor-all-entries set-agent-monitor-all-entries!)
                    (entries agent-monitor-entries set-agent-monitor-entries!)
                    (list agent-monitor-list set-agent-monitor-list!)
                    (filter-query agent-monitor-filter-query set-agent-monitor-filter-query!)
                    (filtered-list agent-monitor-filtered-list set-agent-monitor-filtered-list!)
                    (status-message agent-monitor-status-message set-agent-monitor-status-message!)
                    (spinner-frame agent-monitor-spinner-frame set-agent-monitor-spinner-frame!)
                    (list-agents-fn agent-monitor-list-agents-fn)
                    (get-events-fn agent-monitor-get-events-fn)
                    (steer-agent-fn agent-monitor-steer-agent-fn)
                    (stop-agent-fn agent-monitor-stop-agent-fn)
                    (notify-info-fn agent-monitor-notify-info-fn)
                    (notify-warn-fn agent-monitor-notify-warn-fn))

(define (entry-id entry)
        (ensure-string (or (assq-ref entry 'id) "")))

(define (entry-role entry)
        (ensure-string (or (assq-ref entry 'role) "agent")))

(define (entry-status entry)
        (or (assq-ref entry 'status) 'done))

(define (running-status? status)
        (and (memq status '(thinking tool running)) #t))

(define (entry-filter-key entry)
        (string-append (entry-id entry) " " (entry-role entry)))

(define (default-list-agents)
        (let ((tree (catch #t
                      (lambda () (subagent-hierarchy-tree (current-harness-id)))
                      (lambda _ '()))))
          (let flatten ((nodes (if (list? tree) tree '())) (depth 0) (acc '()))
                       (if (null? nodes)
                         (reverse acc)
                         (let* ((node (car nodes))
                                (id (if (pair? node) (ensure-string (car node)) (ensure-string node)))
                                (children (if (and (pair? node) (list? (cdr node))) (cdr node) '()))
                                (entry `((id . ,id)
                                         (role . "subagent")
                                         (depth . ,depth)
                                         (status . done)
                                         (iteration . 0)
                                         (events . ()))))
                           (flatten (cdr nodes)
                                    depth
                                    (append (reverse (flatten children (+ depth 1) '()))
                                            (cons entry acc))))))))

(define (filter-by-tab entries tab)
        (case tab
          ((active) (filter (lambda (e) (running-status? (entry-status e))) entries))
          ((completed) (filter (lambda (e) (not (running-status? (entry-status e)))) entries))
          (else entries)))

(define (agent-monitor-effective-list ov st)
        (let ((q (if (and ov (modal-overlay? ov)) (modal-filter-query ov) "")))
          (if (equal? q (agent-monitor-filter-query st))
            (agent-monitor-filtered-list st)
            (let ((fl (filter-selectable-list (agent-monitor-list st) q entry-filter-key)))
              (set-agent-monitor-filter-query! st q)
              (set-agent-monitor-filtered-list! st fl)
              fl))))

(define (agent-monitor-selected-entry st)
        (list-selected-item (or (agent-monitor-filtered-list st) (agent-monitor-list st))))

(define (agent-monitor-tick! st)
        "Advance spinner animation frame for running subagents."
        (when (agent-monitor-state? st)
          (set-agent-monitor-spinner-frame! st (+ (agent-monitor-spinner-frame st) 1))))

(define (agent-monitor-refresh! st)
        (let* ((list-fn (agent-monitor-list-agents-fn st))
               (raw (catch #t (lambda () (list-fn)) (lambda _ '())))
               (all (if (list? raw) raw '()))
               (tab-entries (filter-by-tab all (agent-monitor-tab st)))
               (old-idx (selectable-list-selected-index (agent-monitor-list st)))
               (new-idx (if (null? tab-entries) 0 (min old-idx (- (length tab-entries) 1))))
               (sl (make-selectable-list tab-entries #:selected-index new-idx #:max-visible 12)))
          (set-agent-monitor-all-entries! st all)
          (set-agent-monitor-entries! st tab-entries)
          (set-agent-monitor-list! st sl)
          (set-agent-monitor-filtered-list!
            st
            (filter-selectable-list sl (agent-monitor-filter-query st) entry-filter-key))
          st))

;;;
;;; Rendering Helpers
;;;
(define (format-agent-label entry)
        (let* ((depth (max 0 (or (assq-ref entry 'depth) 0)))
               (id (entry-id entry))
               (indent (if (> depth 0)
                         (string-append (make-string (* 2 (- depth 1)) #\space) "└─ ")
                         "")))
          (string-append indent id)))

(define (render-agent-status status frame)
        (case status
          ((thinking)
            (hstack-widget
              (list (spinner-widget frame #:role 'accent)
                    (status-badge-widget 'info "thinking"))
              #:gap 1))
          ((tool)
            (hstack-widget
              (list (spinner-widget frame #:role 'accent)
                    (status-badge-widget 'info "tool"))
              #:gap 1))
          ((running)
            (hstack-widget
              (list (spinner-widget frame #:role 'accent)
                    (status-badge-widget 'info "running"))
              #:gap 1))
          ((done) (status-badge-widget 'ok "done"))
          ((error) (status-badge-widget 'error "error"))
          (else (status-badge-widget 'muted (symbol->string status)))))

(define (format-iter entry)
        (let ((iter (or (assq-ref entry 'iteration) 0))
              (max-it (assq-ref entry 'max-iterations)))
          (if max-it
            (format #f "~a/~a" iter max-it)
            (format #f "~a" iter))))

(define (entry-row-cells frame entry)
        (list (format-agent-label entry)
              (entry-role entry)
              (render-agent-status (entry-status entry) frame)
              (format-iter entry)))

(define (render-monitor-body ov st _rect)
        (let* ((sl (agent-monitor-effective-list ov st))
               (frame (agent-monitor-spinner-frame st))
               (cols '(((header . "Agent") (width . 22))
                       ((header . "Role") (width . 16))
                       ((header . "Status") (width . 16))
                       ((header . "Iter") (width . 8))))
               (entries (selectable-list-items sl))
               (rows (map (lambda (e) (entry-row-cells frame e)) entries))
               (tbl (table-widget cols rows
                                  #:selected-index (selectable-list-selected-index sl)
                                  #:scroll-offset (selectable-list-scroll-offset sl)
                                  #:max-visible (selectable-list-max-visible sl)
                                  #:gap 1
                                  #:empty-text "No subagents in this view."))
               (status-w (and (agent-monitor-status-message st)
                              (text-widget (agent-monitor-status-message st) #:role 'warn))))
          (vstack-widget (filter identity (list tbl status-w)) #:gap 1)))

(define (event-kind-role kind)
        (case kind
          ((thought) 'accent)
          ((tool) 'info)
          ((error) 'error)
          (else 'dim)))

(define (render-monitor-detail ov st _rect)
        (let ((entry (list-selected-item (agent-monitor-effective-list ov st))))
          (if (not entry)
            (text-widget "No agent selected." #:dim? #t)
            (let* ((id (entry-id entry))
                   (role (entry-role entry))
                   (status (entry-status entry))
                   (iter (format-iter entry))
                   (ev-fn (agent-monitor-get-events-fn st))
                   (raw-events (if (procedure? ev-fn)
                                 (catch #t (lambda () (ev-fn id)) (lambda _ '()))
                                 (or (assq-ref entry 'events) '())))
                   (events (if (list? raw-events)
                             (let ((n (length raw-events)))
                               (if (> n 8) (drop raw-events (- n 8)) raw-events))
                             '()))
                   (hdr (text-widget (format #f "Agent: ~a (~a)  Status: ~a  Iter: ~a"
                                             id role status iter)
                                     #:bold? #t))
                   (ev-ws (if (null? events)
                            (list (text-widget "No ReAct events recorded." #:dim? #t))
                            (map (lambda (ev)
                                   (let* ((k (or (assq-ref ev 'kind) 'info))
                                          (txt (ensure-string (or (assq-ref ev 'text) ""))))
                                     (text-widget (format #f "  [~a] ~a" k txt)
                                                  #:role (event-kind-role k)
                                                  #:wrap? #t)))
                                 events))))
              (vstack-widget (cons hdr ev-ws) #:gap 0)))))

;;;
;;; Key & Dialog Actions
;;;
(define (push-child-modal! key-ctx dlg)
        (let ((push-fn (assq-ref key-ctx 'push-modal!))
              (stack (assq-ref key-ctx 'overlay-stack)))
          (cond
            ((procedure? push-fn) (push-fn dlg) #t)
            ((overlay-stack? stack) (overlay-stack-push-modal! stack dlg) #t)
            (else #f))))

(define (handle-monitor-action ov st ch key-ctx)
        (let* ((_ (agent-monitor-effective-list ov st))
               (entry (agent-monitor-selected-entry st)))
          (cond
            ((eq? ch 'down)
              (set-agent-monitor-filtered-list!
                st
                (list-move (agent-monitor-effective-list ov st) 1))
              'handled)
            ((eq? ch 'up)
              (set-agent-monitor-filtered-list!
                st
                (list-move (agent-monitor-effective-list ov st) -1))
              'handled)
            ((eqv? ch #\s)
              (when entry
                (let* ((id (entry-id entry))
                       (steer-fn (agent-monitor-steer-agent-fn st))
                       (info-fn (agent-monitor-notify-info-fn st))
                       (dlg (make-input-dialog
                              #:title (format #f "Steer ~a" id)
                              #:prompt "Steering instruction:"
                              #:placeholder "Focus on..."
                              #:validator (lambda (raw)
                                            (if (string-null? (string-trim-both raw))
                                              "Instruction cannot be empty"
                                              #f))
                              #:on-submit
                              (lambda (raw)
                                (let ((msg (string-trim-both raw)))
                                  (catch #t
                                    (lambda ()
                                      (when (procedure? steer-fn)
                                        (steer-fn id msg))
                                      (when (procedure? info-fn)
                                        (info-fn (format #f "Steered ~a" id) #:title "Agents" #:source 'agents))
                                      (agent-monitor-refresh! st)
                                      (set-agent-monitor-status-message! st (format #f "Steered ~a" id)))
                                    (lambda (k . _args)
                                      (set-agent-monitor-status-message!
                                        st
                                        (format #f "Steer failed: ~a" k)))))))))
                  (push-child-modal! key-ctx dlg)))
              'handled)
            ((eqv? ch #\k)
              (when entry
                (let* ((id (entry-id entry))
                       (stop-fn (agent-monitor-stop-agent-fn st))
                       (warn-fn (agent-monitor-notify-warn-fn st))
                       (dlg (make-confirm-dialog
                              #:title "Force-Stop Agent"
                              #:message (format #f "Force-stop subagent ~a?" id)
                              #:confirm-label "Force-Stop"
                              #:cancel-label "Cancel"
                              #:danger? #t
                              #:on-confirm
                              (lambda ()
                                (catch #t
                                  (lambda ()
                                    (when (procedure? stop-fn)
                                      (stop-fn id))
                                    (when (procedure? warn-fn)
                                      (warn-fn (format #f "Force-stopped ~a" id) #:title "Agents" #:source 'agents))
                                    (agent-monitor-refresh! st)
                                    (set-agent-monitor-status-message! st (format #f "Stopped ~a" id)))
                                  (lambda (k . _args)
                                    (set-agent-monitor-status-message!
                                      st
                                      (format #f "Stop failed: ~a" k))))))))
                  (push-child-modal! key-ctx dlg)))
              'handled)
            ((or (eqv? ch #\newline) (eqv? ch #\return) (eq? ch 'enter))
              (set-modal-show-detail?! ov (not (modal-show-detail? ov)))
              'handled)
            ((or (eqv? ch #\q) (eqv? ch #\esc) (eq? ch 'esc) (eqv? ch #\escape))
              'close)
            (else 'pass))))

;;;
;;; Modal Constructor & TUI Registration
;;;
(define* (make-agent-monitor-overlay #:key
                                     (active-tab 'all)
                                     (list-agents #f)
                                     (get-events #f)
                                     (steer-agent! #f)
                                     (stop-agent! #f)
                                     (notify-info! #f)
                                     (notify-warn! #f))
         "Create the /agents subagent mission control modal <overlay>."
         (let* ((init-sl (make-selectable-list '() #:max-visible 12))
                (st (%make-agent-monitor-state
                      active-tab
                      '()
                      '()
                      init-sl
                      ""
                      init-sl
                      #f
                      0
                      (or list-agents default-list-agents)
                      get-events
                      steer-agent!
                      stop-agent!
                      (or notify-info! %default-notify-info!)
                      (or notify-warn! %default-notify-warn!)))
                (_ (agent-monitor-refresh! st))
                (hints '(("↑/↓" . "nav")
                         ("Tab" . "view")
                         ("/" . "filter")
                         ("Enter" . "tail")
                         ("s" . "steer")
                         ("k" . "force-stop")
                         ("q" . "close"))))
           (make-modal-overlay
             #:title "Agent Monitor"
             #:tabs '((all . "All") (active . "Active") (completed . "Completed"))
             #:active-tab active-tab
             #:state st
             #:width 0.85
             #:height 0.8
             #:filterable? #t
             #:filter-key-fn entry-filter-key
             #:body-renderer render-monitor-body
             #:detail-renderer render-monitor-detail
             #:show-detail? #f
             #:footer-hints hints
             #:key-bindings
             (map (lambda (k)
                    (cons k (lambda (ov s ctx) (handle-monitor-action ov s k ctx))))
                  (list 'down 'up #\s #\k #\newline #\return 'enter #\q 'esc #\esc #\escape))
             #:on-tab-change
             (lambda (_ov s new-tab)
               (set-agent-monitor-tab! s new-tab)
               (set-agent-monitor-status-message! s #f)
               (agent-monitor-refresh! s)))))

(define* (register-agent-monitor-tui! #:key
                                      register-category!
                                      register-command!
                                      open-overlay!
                                      (list-agents #f)
                                      (get-events #f)
                                      (steer-agent! #f)
                                      (stop-agent! #f))
         "Register the Agents category and /agents slash command using the provided TUI callbacks."
         (when (procedure? register-category!)
           (register-category! '("Agents") "Monitor and steer active subagents"))
         (when (procedure? register-command!)
           (register-command!
             "agents"
             "Open Agent Monitor modal"
             (lambda _
               (when (procedure? open-overlay!)
                 (open-overlay!
                   (make-agent-monitor-overlay
                     #:list-agents list-agents
                     #:get-events get-events
                     #:steer-agent! steer-agent!
                     #:stop-agent! stop-agent!))))
             #:path '("Agents"))))
