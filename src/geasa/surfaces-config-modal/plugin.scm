;;; (geasa surfaces-config-modal plugin) — /config Tabbed Workbench

(define-module (geasa surfaces-config-modal plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (geasa surfaces-notify plugin)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (make-config-modal-overlay
                          register-config-modal-tui!
                          config-modal-state?
                          config-modal-scope
                          config-modal-lists
                          config-modal-selected-item
                          config-modal-status-message))

(define-record-type <config-modal-state>
                    (%make-config-modal-state scope lists on-highlight on-save status-message)
                    config-modal-state?
                    (scope config-modal-scope set-config-modal-scope!)
                    (lists config-modal-lists set-config-modal-lists!)
                    (on-highlight config-modal-on-highlight)
                    (on-save config-modal-on-save)
                    (status-message config-modal-status-message set-config-modal-status-message!))

(define (active-tab-list ov)
        (let* ((st (modal-overlay-state ov))
               (tab (modal-active-tab ov)))
          (assq-ref (config-modal-lists st) tab)))

(define (set-active-tab-list! ov new-list)
        (let* ((st (modal-overlay-state ov))
               (tab (modal-active-tab ov)))
          (set-config-modal-lists!
            st
            (acons tab new-list (alist-delete tab (config-modal-lists st) eq?)))))

(define (config-modal-selected-item ov)
        (let ((slist (active-tab-list ov)))
          (and slist (list-selected-item slist))))

(define (entry-label entry)
        (or (assq-ref entry 'label)
            (assq-ref entry 'key)
            "option"))

(define (entry-bool? entry)
        (or (eq? (assq-ref entry 'type) 'boolean)
            (boolean? (assq-ref entry 'value))))

(define (move-selection! ov delta)
        (let ((slist (active-tab-list ov))
              (st (modal-overlay-state ov)))
          (when slist
            (let ((moved (list-move slist delta)))
              (set-active-tab-list! ov moved)
              (when (and (eq? (modal-active-tab ov) 'theme)
                         (procedure? (config-modal-on-highlight st)))
                (let ((sel (list-selected-item moved)))
                  (when sel
                    ((config-modal-on-highlight st) sel))))))))

(define (save-config! ov)
        (let* ((st (modal-overlay-state ov))
               (scope (config-modal-scope st))
               (snapshot (map (lambda (pair)
                                (cons (car pair) (selectable-list-items (cdr pair))))
                              (config-modal-lists st))))
          (when (procedure? (config-modal-on-save st))
            (catch #t
              (lambda () ((config-modal-on-save st) scope snapshot))
              (lambda _ #f)))
          (catch #t
            (lambda () (notify-info! (format #f "Saved config (~a)" scope) #:title "Config"))
            (lambda _ #f))
          (set-config-modal-status-message! st (format #f "Saved (~a)" scope))))

(define (toggle-selected-option! ov)
        (let ((slist (active-tab-list ov))
              (sel (config-modal-selected-item ov))
              (tab (modal-active-tab ov))
              (st (modal-overlay-state ov)))
          (when (and slist sel)
            (let ((k (assq-ref sel 'key))
                  (choices (assq-ref sel 'choices)))
              (cond
                ((assq-ref sel 'action) =>
                                        (lambda (act)
                                          (when (procedure? (config-modal-on-highlight st))
                                            (catch #t
                                              (lambda () ((config-modal-on-highlight st) `((tab . action) (action . ,act) ,@sel)))
                                              (lambda _ #f)))))
                ((entry-bool? sel)
                  (let* ((new-val (not (assq-ref sel 'value)))
                         (updated (map (lambda (it)
                                         (if (equal? (assq-ref it 'key) k)
                                           (acons 'value new-val (alist-delete 'value it))
                                           it))
                                       (selectable-list-items slist))))
                    (set-active-tab-list!
                      ov
                      (make-selectable-list
                        updated
                        #:selected-index (selectable-list-selected-index slist)
                        #:max-visible 12))
                    (set-config-modal-status-message!
                      st (format #f "~a: ~a" (entry-label sel) new-val))))
                ((and (list? choices) (pair? choices))
                  (let* ((cur (assq-ref sel 'value))
                         (tail (member cur choices))
                         (nxt (if (and tail (pair? (cdr tail))) (cadr tail) (car choices)))
                         (updated (map (lambda (it)
                                         (if (equal? (assq-ref it 'key) k)
                                           (acons 'value nxt (alist-delete 'value it))
                                           it))
                                       (selectable-list-items slist))))
                    (set-active-tab-list!
                      ov
                      (make-selectable-list
                        updated
                        #:selected-index (selectable-list-selected-index slist)
                        #:max-visible 12))
                    (set-config-modal-status-message!
                      st (format #f "~a: ~a" (entry-label sel) nxt))))
                ((eq? tab 'providers)
                  (let ((updated (map (lambda (it)
                                        (let ((active? (equal? (assq-ref it 'key) k))
                                              (base (or (assq-ref it 'summary) (assq-ref it 'value) "")))
                                          (acons 'active? active?
                                                 (acons 'value
                                                        (if active?
                                                          (if (string-prefix? "★ " base) base (string-append "★ " base))
                                                          (if (string-prefix? "★ " base) (substring base 2) base))
                                                        (alist-delete 'active? (alist-delete 'value it))))))
                                      (selectable-list-items slist))))
                    (set-active-tab-list!
                      ov
                      (make-selectable-list
                        updated
                        #:selected-index (selectable-list-selected-index slist)
                        #:max-visible 12))
                    (when (procedure? (config-modal-on-highlight st))
                      (catch #t (lambda () ((config-modal-on-highlight st) (acons 'tab 'providers sel))) (lambda _ #f)))
                    (set-config-modal-status-message!
                      st (format #f "Active provider: ~a" (entry-label sel)))))
                ((eq? tab 'theme)
                  (let ((updated (map (lambda (it)
                                        (acons 'value
                                               (if (equal? (assq-ref it 'key) k) "active" "")
                                               (alist-delete 'value it)))
                                      (selectable-list-items slist))))
                    (set-active-tab-list!
                      ov
                      (make-selectable-list
                        updated
                        #:selected-index (selectable-list-selected-index slist)
                        #:max-visible 12))
                    (when (procedure? (config-modal-on-highlight st))
                      ((config-modal-on-highlight st) sel))
                    (set-config-modal-status-message!
                      st (format #f "Theme: ~a" (entry-label sel))))))))))

(define* (make-config-modal-overlay
           #:key
           (active-tab 'providers)
           (scope 'user)
           (providers '())
           (guardrails '())
           (themes '())
           (synthesizer '())
           (on-highlight #f)
           (on-save #f))
         "Create a /config tabbed workbench modal <overlay>."
         (let* ((lists `((providers . ,(make-selectable-list providers #:max-visible 12))
                         (guardrails . ,(make-selectable-list guardrails #:max-visible 12))
                         (theme . ,(make-selectable-list themes #:max-visible 12))
                         (synthesizer . ,(make-selectable-list synthesizer #:max-visible 12))))
                (st (%make-config-modal-state scope lists on-highlight on-save "")))
           (make-modal-overlay
             #:title "Configuration (/config)"
             #:tabs '((providers . "Providers & Models")
                      (guardrails . "Guardrails")
                      (theme . "Theme")
                      (synthesizer . "Synthesizer"))
             #:active-tab active-tab
             #:state st
             #:width 0.85
             #:height 0.80
             #:show-detail? #t
             #:body-renderer
             (lambda (ov state rect)
               (let ((slist (active-tab-list ov))
                     (msg (config-modal-status-message state))
                     (sc (config-modal-scope state)))
                 (vstack-widget
                   (append
                     (list (text-widget (format #f "Scope: ~a" sc) #:dim? #t))
                     (if (string-null? msg)
                       '()
                       (list (text-widget msg #:role 'ok #:bold? #t)))
                     (if slist
                       (list
                         (selectable-list-widget
                           slist
                           #:item-renderer
                           (lambda (entry selected?)
                             (let ((lbl (entry-label entry))
                                   (val (assq-ref entry 'value)))
                               (if (entry-bool? entry)
                                 (hstack-widget
                                   (list
                                     (toggle-widget (and val #t))
                                     (text-widget (format #f " ~a" lbl)
                                                  #:reverse? selected?
                                                  #:bold? selected?)))
                                 (text-widget
                                   (format #f "~a: ~a" lbl (or val ""))
                                   #:reverse? selected?
                                   #:bold? selected?))))))
                       '())))))
             #:detail-renderer
             (lambda (ov _state _rect)
               (let ((sel (config-modal-selected-item ov)))
                 (if sel
                   (let ((desc (or (assq-ref sel 'description) "")))
                     (text-widget (if (string-null? desc)
                                    (format #f "~a = ~a" (entry-label sel) (assq-ref sel 'value))
                                    desc)))
                   (text-widget "No option selected." #:dim? #t))))
             #:key-bindings
             `((#\j . ,(lambda (ov _st _ctx) (move-selection! ov 1) 'handled))
               (#\k . ,(lambda (ov _st _ctx) (move-selection! ov -1) 'handled))
               (down . ,(lambda (ov _st _ctx) (move-selection! ov 1) 'handled))
               (up . ,(lambda (ov _st _ctx) (move-selection! ov -1) 'handled))
               (#\space . ,(lambda (ov _st _ctx)
                             (toggle-selected-option! ov)
                             'handled))
               (#\w . ,(lambda (_ov st _ctx)
                         (let ((next (if (eq? (config-modal-scope st) 'user) 'project 'user)))
                           (set-config-modal-scope! st next)
                           (set-config-modal-status-message! st (format #f "Scope: ~a" next)))
                         'handled))
               (#\s . ,(lambda (ov _st _ctx) (save-config! ov) 'handled))
               (enter . ,(lambda (ov _st _ctx)
                           (toggle-selected-option! ov)
                           'handled))
               (#\q . ,(lambda (_ov _st _ctx) 'close)))
             #:footer-hints '(("Tab" . "Switch tab")
                              ("j/k" . "Navigate")
                              ("Space/Enter" . "Activate/Toggle")
                              ("w" . "Scope")
                              ("s" . "Save")
                              ("q/Esc" . "Close")))))

(define* (register-config-modal-tui!
           #:key
           (register-category! (lambda (_p _d) #f))
           (register-command! (lambda (_n _d _a . _r) #f))
           (open-overlay! (lambda (_ov) #f))
           (providers-fn (lambda () '()))
           (guardrails-fn (lambda () '()))
           (themes-fn (lambda () '()))
           (synthesizer-fn (lambda () '()))
           (on-highlight #f)
           (on-save #f))
         "Register /config command and Settings category in TUI."
         (register-category! '("Settings") "Configuration and preferences")
         (register-command!
           "config"
           "Open the /config tabbed workbench"
           (lambda _args
             (open-overlay!
               (make-config-modal-overlay
                 #:providers (providers-fn)
                 #:guardrails (guardrails-fn)
                 #:themes (themes-fn)
                 #:synthesizer (synthesizer-fn)
                 #:on-highlight on-highlight
                 #:on-save on-save)))))
