;;; (geasa surfaces-knowledge-browser plugin) — /skills & /memories Tabbed Browser

(define-module (geasa surfaces-knowledge-browser plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (make-knowledge-browser-overlay
                          register-knowledge-browser-tui!
                          knowledge-browser-state?
                          knowledge-browser-skills
                          knowledge-browser-memories
                          knowledge-browser-selected-item
                          knowledge-browser-status-message))

(define-record-type <knowledge-browser-state>
                    (%make-knowledge-browser-state
                      skills memories skills-list memories-list
                      on-toggle-skill on-delete-memory status-message confirm-delete?)
                    knowledge-browser-state?
                    (skills knowledge-browser-skills set-knowledge-browser-skills!)
                    (memories knowledge-browser-memories set-knowledge-browser-memories!)
                    (skills-list kb-skills-list set-kb-skills-list!)
                    (memories-list kb-memories-list set-kb-memories-list!)
                    (on-toggle-skill kb-on-toggle-skill)
                    (on-delete-memory kb-on-delete-memory)
                    (status-message knowledge-browser-status-message set-knowledge-browser-status-message!)
                    (confirm-delete? kb-confirm-delete? set-kb-confirm-delete?!))

(define (knowledge-browser-selected-item ov)
        (let ((st (modal-overlay-state ov)))
          (if (eq? (modal-active-tab ov) 'memories)
            (list-selected-item (kb-memories-list st))
            (list-selected-item (kb-skills-list st)))))

(define (item-name item)
        (or (assq-ref item 'name)
            (assq-ref item 'id)
            "unnamed"))

(define (item-content item)
        (or (assq-ref item 'content)
            (assq-ref item 'preview)
            ""))

(define (%kb-render-body ov state _rect)
        (let ((msg (knowledge-browser-status-message state))
              (tab (modal-active-tab ov)))
          (vstack-widget
            (append
              (if (string-null? msg)
                '()
                (list (text-widget msg #:role 'warn #:bold? #t)))
              (list
                (if (eq? tab 'memories)
                  (selectable-list-widget
                    (kb-memories-list state)
                    #:item-renderer
                    (lambda (item selected?)
                      (let ((scope (or (assq-ref item 'scope) "project")))
                        (text-widget
                          (format #f "[~a] ~a" scope (item-name item))
                          #:reverse? selected?
                          #:bold? selected?))))
                  (selectable-list-widget
                    (kb-skills-list state)
                    #:item-renderer
                    (lambda (item selected?)
                      (let ((enabled? (assq-ref item 'enabled?)))
                        (hstack-widget
                          (list
                            (toggle-widget enabled?)
                            (text-widget
                              (format #f " ~a" (item-name item))
                              #:reverse? selected?
                              #:bold? selected?))))))))))))

(define (%kb-render-detail ov _state _rect)
        (let ((sel (knowledge-browser-selected-item ov)))
          (if sel
            (let ((txt (item-content sel)))
              (text-widget (if (string-null? txt) "No preview available." txt)))
            (text-widget "Nothing selected." #:dim? #t))))

(define (%kb-move-selection! delta)
        (lambda (ov st _ctx)
          (set-kb-confirm-delete?! st #f)
          (if (eq? (modal-active-tab ov) 'memories)
            (set-kb-memories-list! st (list-move (kb-memories-list st) delta))
            (set-kb-skills-list! st (list-move (kb-skills-list st) delta)))
          'handled))

(define (%kb-toggle-skill! ov st _ctx)
        (when (eq? (modal-active-tab ov) 'skills)
          (let ((sel (list-selected-item (kb-skills-list st))))
            (when sel
              (let* ((new-val (not (assq-ref sel 'enabled?)))
                     (updated (map (lambda (it)
                                     (if (equal? (item-name it) (item-name sel))
                                       (acons 'enabled? new-val (alist-delete 'enabled? it))
                                       it))
                                   (knowledge-browser-skills st))))
                (set-knowledge-browser-skills! st updated)
                (set-kb-skills-list!
                  st
                  (make-selectable-list
                    updated
                    #:selected-index (selectable-list-selected-index (kb-skills-list st))
                    #:max-visible 12))
                (when (procedure? (kb-on-toggle-skill st))
                  ((kb-on-toggle-skill st) sel new-val))
                (set-knowledge-browser-status-message!
                  st
                  (format #f "~a ~a"
                          (if new-val "Enabled" "Disabled")
                          (item-name sel)))))))
        'handled)

(define (%kb-delete-memory! ov st _ctx)
        (when (eq? (modal-active-tab ov) 'memories)
          (let ((sel (list-selected-item (kb-memories-list st))))
            (when sel
              (if (kb-confirm-delete? st)
                (let ((remaining (remove (lambda (it)
                                           (equal? (item-name it) (item-name sel)))
                                         (knowledge-browser-memories st))))
                  (set-kb-confirm-delete?! st #f)
                  (set-knowledge-browser-memories! st remaining)
                  (set-kb-memories-list!
                    st
                    (make-selectable-list remaining #:max-visible 12))
                  (when (procedure? (kb-on-delete-memory st))
                    ((kb-on-delete-memory st) sel))
                  (set-knowledge-browser-status-message!
                    st
                    (format #f "Deleted ~a" (item-name sel))))
                (begin
                  (set-kb-confirm-delete?! st #t)
                  (set-knowledge-browser-status-message!
                    st
                    (format #f "Press d again to delete ~a" (item-name sel))))))))
        'handled)

(define* (make-knowledge-browser-overlay
           #:key
           (active-tab 'skills)
           (skills '())
           (memories '())
           (on-toggle-skill #f)
           (on-delete-memory #f))
         "Create a /skills & /memories tabbed browser modal <overlay>."
         (let* ((slist (make-selectable-list skills #:max-visible 12))
                (mlist (make-selectable-list memories #:max-visible 12))
                (st (%make-knowledge-browser-state
                      skills memories slist mlist on-toggle-skill on-delete-memory "" #f)))
           (make-modal-overlay
             #:title "Knowledge Browser"
             #:tabs '((skills . "Skills") (memories . "Memories"))
             #:active-tab active-tab
             #:state st
             #:width 0.85
             #:height 0.80
             #:show-detail? #t
             #:body-renderer %kb-render-body
             #:detail-renderer %kb-render-detail
             #:key-bindings
             `((#\j . ,(%kb-move-selection! 1))
               (#\k . ,(%kb-move-selection! -1))
               (down . ,(%kb-move-selection! 1))
               (up . ,(%kb-move-selection! -1))
               (enter . ,(lambda (ov _st _ctx)
                           (set-modal-show-detail?! ov (not (modal-show-detail? ov)))
                           'handled))
               (#\space . ,%kb-toggle-skill!)
               (#\d . ,%kb-delete-memory!)
               (#\q . ,(lambda (_ov _st _ctx) 'close)))
             #:footer-hints '(("Tab" . "Switch tab")
                              ("j/k" . "Navigate")
                              ("Space" . "Toggle")
                              ("d" . "Delete")
                              ("Enter" . "Preview")
                              ("q/Esc" . "Close")))))

(define* (register-knowledge-browser-tui!
           #:key
           (register-category! (lambda (_p _d) #f))
           (register-command! (lambda (_n _d _a . _r) #f))
           (open-overlay! (lambda (_ov) #f))
           (skills-provider (lambda () '()))
           (memories-provider (lambda () '()))
           (on-toggle-skill #f)
           (on-delete-memory #f))
         "Register /skills and /memories commands in TUI."
         (register-category! '("Knowledge") "Skills and memories browser")
         (register-command!
           "skills"
           "Browse and toggle installed skills"
           (lambda _args
             (open-overlay!
               (make-knowledge-browser-overlay
                 #:active-tab 'skills
                 #:skills (skills-provider)
                 #:memories (memories-provider)
                 #:on-toggle-skill on-toggle-skill
                 #:on-delete-memory on-delete-memory))))
         (register-command!
           "memories"
           "Browse and manage saved memories"
           (lambda _args
             (open-overlay!
               (make-knowledge-browser-overlay
                 #:active-tab 'memories
                 #:skills (skills-provider)
                 #:memories (memories-provider)
                 #:on-toggle-skill on-toggle-skill
                 #:on-delete-memory on-delete-memory)))))
