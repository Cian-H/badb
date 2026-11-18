;;; (geasa surfaces-geis-manager plugin) — lazy.nvim-style /geasa modal + extension API

(define-module
  (geasa surfaces-geis-manager plugin) #:use-module
  (geasa surfaces-overlay plugin)
  #:use-module
  (geasa surfaces-widgets plugin)
  #:use-module
  (geasa runtimes-geis-pkg plugin)
  #:use-module
  (badb geis bundle)
  #:use-module
  (ice-9 format)
  #:use-module
  (srfi srfi-1)
  #:use-module
  (srfi srfi-9)
  #:export
  (register-geis-manager-column!
    unregister-geis-manager-column!
    list-geis-manager-columns
    register-geis-manager-action!
    unregister-geis-manager-action!
    list-geis-manager-actions
    register-geis-manager-banner!
    unregister-geis-manager-banner!
    list-geis-manager-banners
    clear-geis-manager-extensions!
    current-geis-manager-workspace
    geis-manager-state?
    geis-manager-scope
    geis-manager-workspace
    geis-manager-entries
    geis-manager-selected-entry
    geis-manager-status-message
    set-geis-manager-status-message!
    geis-manager-refresh!
    make-geis-manager-overlay
    register-geis-manager-tui!))

;;;
;;; Extension Registries
;;;
(define %geis-manager-columns '())
(define %geis-manager-actions '())
(define %geis-manager-banners '())

(define* (register-geis-manager-column! id #:key (header "") (width 12) render)
         "Register an extension column RENDER (scope entry) -> <widget>."
         (set! %geis-manager-columns
               (append (alist-delete id %geis-manager-columns equal?)
                       (list (cons id `((header . ,header)
                                        (width . ,width)
                                        (render . ,render)))))))

(define (unregister-geis-manager-column! id)
        (set! %geis-manager-columns (alist-delete id %geis-manager-columns equal?)))

(define (list-geis-manager-columns)
        %geis-manager-columns)

(define* (register-geis-manager-action! key #:key (label "") handler)
         "Register an extension action KEY with HANDLER (scope entry modal-ctx)."
         (set! %geis-manager-actions
               (append (alist-delete key %geis-manager-actions equal?)
                       (list (cons key `((label . ,label)
                                         (handler . ,handler)))))))

(define (unregister-geis-manager-action! key)
        (set! %geis-manager-actions (alist-delete key %geis-manager-actions equal?)))

(define (list-geis-manager-actions)
        %geis-manager-actions)

(define* (register-geis-manager-banner! id #:key render)
         "Register an extension banner RENDER (scope) -> <widget> or #f."
         (set! %geis-manager-banners
               (append (alist-delete id %geis-manager-banners equal?)
                       (list (cons id `((render . ,render)))))))

(define (unregister-geis-manager-banner! id)
        (set! %geis-manager-banners (alist-delete id %geis-manager-banners equal?)))

(define (list-geis-manager-banners)
        %geis-manager-banners)

(define (clear-geis-manager-extensions!)
        (set! %geis-manager-columns '())
        (set! %geis-manager-actions '())
        (set! %geis-manager-banners '()))

;;;
;;; Manager State
;;;
(define current-geis-manager-workspace (make-parameter #f))

(define-record-type <geis-manager-state>
                    (%make-geis-manager-state scope workspace entries list status-message filter-query filtered-list)
                    geis-manager-state?
                    (scope geis-manager-scope set-geis-manager-scope!)
                    (workspace geis-manager-workspace)
                    (entries geis-manager-entries set-geis-manager-entries!)
                    (list geis-manager-list set-geis-manager-list!)
                    (status-message geis-manager-status-message set-geis-manager-status-message!)
                    (filter-query geis-manager-filter-query set-geis-manager-filter-query!)
                    (filtered-list geis-manager-filtered-list set-geis-manager-filtered-list!))

(define (geis-manager-effective-list ov st)
        (let ((q (if (and ov (modal-overlay? ov)) (modal-filter-query ov) "")))
          (if (equal? q (geis-manager-filter-query st))
            (geis-manager-filtered-list st)
            (let ((fl (filter-selectable-list (geis-manager-list st) q entry-fid)))
              (set-geis-manager-filter-query! st q)
              (set-geis-manager-filtered-list! st fl)
              fl))))

(define (geis-manager-selected-entry st)
        (list-selected-item (or (geis-manager-filtered-list st) (geis-manager-list st))))

(define (geis-manager-refresh! st)
        (let*
            ((scope (geis-manager-scope st))
             (ws (geis-manager-workspace st))
             (raw-entries
               (catch #t
                 (lambda ()
                   (geis-pkg-list scope ws))
                 (lambda _
                   '())))
             (entries
               (sort raw-entries
                     (lambda (a b)
                       (string<? (entry-fid a) (entry-fid b)))))
             (old-idx (selectable-list-selected-index (geis-manager-list st)))
             (new-idx
               (if (null? entries)
                 0
                 (min old-idx (- (length entries) 1))))
             (sl (make-selectable-list entries #:selected-index new-idx #:max-visible 12)))
          (set-geis-manager-entries! st entries)
          (set-geis-manager-list! st sl)
          (set-geis-manager-filtered-list!
            st
            (filter-selectable-list sl (geis-manager-filter-query st) entry-fid))
          st))

;;;
;;; Rendering Helpers
;;;
(define (entry-fid entry)
        (car entry))

(define (entry-field entry key default)
        (let ((pair (assq key (cdr entry))))
          (if pair (cdr pair) default)))

(define (entry-row-cells scope entry selected?)
        (let*
            ((fid (entry-fid entry))
             (ver (or (entry-field entry 'version #f) "0.0.0.0"))
             (pinned? (entry-field entry 'pinned? #f))
             (enabled? (entry-field entry 'enabled? #t))
             (core? (and (member fid %core-protected-geasa) #t))
             (toggle (toggle-widget enabled? #:locked? core?))
             (pin-str
               (if pinned?
                 "📌"
                 ""))
             (base-cells (list toggle fid ver pin-str))
             (ext-cells
               (filter-map
                 (lambda (col)
                   (let ((render-fn (assq-ref (cdr col) 'render)))
                     (and (procedure? render-fn)
                          (catch #t
                            (lambda ()
                              (render-fn scope entry))
                            (lambda _
                              "")))))
                 %geis-manager-columns)))
          (append base-cells ext-cells)))

(define (render-geis-body ov st rect)
        (let*
            ((scope (geis-manager-scope st))
             (ws (geis-manager-workspace st))
             (sl (geis-manager-effective-list ov st)))
          (parameterize
            ((current-geis-manager-workspace ws))
            (let*
                ((banners
                   (filter-map
                     (lambda (b)
                       (let ((render-fn (assq-ref (cdr b) 'render)))
                         (and (procedure? render-fn)
                              (catch #t
                                (lambda ()
                                  (render-fn scope))
                                (lambda _
                                  #f)))))
                     %geis-manager-banners))
                 (base-cols
                   '(((header . "Active") (width . 13))
                     ((header . "Geis") (width . 28))
                     ((header . "Version") (width . 14))
                     ((header . "Pin") (width . 4))))
                 (ext-cols
                   (map
                     (lambda (col)
                       `((header . ,(or (assq-ref (cdr col) 'header) ""))
                         (width . ,(or (assq-ref (cdr col) 'width) 14))))
                     %geis-manager-columns))
                 (all-cols (append base-cols ext-cols))
                 (entries (selectable-list-items sl))
                 (sel-idx (selectable-list-selected-index sl))
                 (scroll-off (selectable-list-scroll-offset sl))
                 (max-vis (selectable-list-max-visible sl))
                 (rows
                   (map
                     (lambda (e)
                       (entry-row-cells scope e #f))
                     entries))
                 (table-w
                   (table-widget all-cols
                                 rows
                                 #:selected-index
                                 sel-idx
                                 #:scroll-offset
                                 scroll-off
                                 #:max-visible
                                 max-vis
                                 #:gap
                                 1
                                 #:empty-text
                                 "No geasa installed in this scope."))
                 (status-w
                   (and (geis-manager-status-message st)
                        (text-widget (geis-manager-status-message st) #:role 'warn))))
              (vstack-widget
                (filter identity (append banners (list table-w status-w)))
                #:gap
                1)))))

(define (render-geis-detail ov st rect)
        (let ((entry (list-selected-item (geis-manager-effective-list ov st))))
          (if (not entry)
            (text-widget "No geis selected." #:dim? #t)
            (let* ((fid (entry-fid entry))
                   (ver (or (entry-field entry 'version #f) "unknown"))
                   (src (entry-field entry 'source #f))
                   (sha (or (entry-field entry 'bundle-sha256 #f) "none"))
                   (commit (or (entry-field entry 'commit #f) "local"))
                   (pinned? (entry-field entry 'pinned? #f))
                   (enabled? (entry-field entry 'enabled? #t))
                   (core? (and (member fid %core-protected-geasa) #t)))
              (vstack-widget
                (list
                  (text-widget (format #f "Feature: ~a~a" fid (if core? " (core)" "")) #:bold? #t)
                  (text-widget (format #f "Version: ~a  Pinned: ~a  Enabled: ~a" ver pinned? enabled?))
                  (text-widget (format #f "Commit:  ~a" commit))
                  (text-widget (format #f "SHA256:  ~a" sha))
                  (text-widget (format #f "Source:  ~s" src) #:wrap? #t))
                #:gap 0)))))

;;;
;;; Actions & Key Handling
;;;
(define (parse-add-spec spec)
        (let* ((hash-idx (string-index spec #\#)))
          (if (not hash-idx)
            (values spec "" "main")
            (let* ((url (substring spec 0 hash-idx))
                   (rest (substring spec (+ hash-idx 1)))
                   (at-idx (string-index rest #\@)))
              (if at-idx
                (values url
                        (substring rest 0 at-idx)
                        (substring rest (+ at-idx 1)))
                (values url rest "main"))))))

(define (push-child-modal! key-ctx dlg)
        (let ((push-fn (assq-ref key-ctx 'push-modal!))
              (stack (assq-ref key-ctx 'overlay-stack)))
          (cond
            ((procedure? push-fn) (push-fn dlg) #t)
            ((overlay-stack? stack) (overlay-stack-push-modal! stack dlg) #t)
            (else #f))))

(define (handle-geis-action ov st ch key-ctx)
        (let*
            ((_ (geis-manager-effective-list ov st))
             (scope (geis-manager-scope st))
             (ws (geis-manager-workspace st))
             (entry (geis-manager-selected-entry st)))
          (cond
            ((or (eqv? ch #\j) (eq? ch 'down))
              (set-geis-manager-filtered-list!
                st
                (list-move (geis-manager-effective-list ov st) 1))
              'handled)
            ((or (eqv? ch #\k) (eq? ch 'up))
              (set-geis-manager-filtered-list!
                st
                (list-move (geis-manager-effective-list ov st) -1))
              'handled)
            ((or (eqv? ch #\space) (eqv? ch #\t))
              (when entry
                (let
                    ((fid (entry-fid entry)) (enabled? (entry-field entry 'enabled? #t)))
                  (if
                      (and enabled? (member fid %core-protected-geasa))
                    (set-geis-manager-status-message! st "Core geis cannot be disabled")
                    (catch #t
                      (lambda ()
                        (if enabled?
                          (geis-pkg-disable! fid #:scope scope #:workspace ws)
                          (geis-pkg-enable! fid #:scope scope #:workspace ws))
                        (geis-manager-refresh! st)
                        (set-geis-manager-status-message! st
                                                          (format #f
                                                                  "~a ~a"
                                                                  (if enabled?
                                                                    "Disabled"
                                                                    "Enabled")
                                                                  fid)))
                      (lambda (k . _args)
                        (set-geis-manager-status-message! st (format #f "Toggle failed: ~a" k)))))))
              'handled)
            ((eqv? ch #\p)
              (when entry
                (let
                    ((fid (entry-fid entry)) (pinned? (entry-field entry 'pinned? #f)))
                  (catch #t
                    (lambda ()
                      (geis-pkg-pin! fid #:scope scope #:workspace ws #:pinned? (not pinned?))
                      (geis-manager-refresh! st)
                      (set-geis-manager-status-message! st
                                                        (format #f
                                                                "~a ~a"
                                                                (if pinned?
                                                                  "Unpinned"
                                                                  "Pinned")
                                                                fid)))
                    (lambda (k . _args)
                      (set-geis-manager-status-message! st (format #f "Pin failed: ~a" k))))))
              'handled)
            ((eqv? ch #\r)
              (when entry
                (let ((fid (entry-fid entry)))
                  (catch #t
                    (lambda ()
                      (geis-pkg-rollback! fid #:scope scope #:workspace ws)
                      (geis-manager-refresh! st)
                      (set-geis-manager-status-message! st (format #f "Rolled back ~a" fid)))
                    (lambda (k . _args)
                      (set-geis-manager-status-message! st (format #f "Rollback failed: ~a" k))))))
              'handled)
            ((eqv? ch #\v)
              (when entry
                (let*
                    ((fid (entry-fid entry))
                     (bdir (string-append (geis-pkg-store-dir scope ws) "/" fid)))
                  (catch #t
                    (lambda ()
                      (let ((errors (geis-manifest-valid? bdir)))
                        (set-geis-manager-status-message! st
                                                          (format #f
                                                                  "Verified ~a: ~a"
                                                                  fid
                                                                  (if (null? errors)
                                                                    "OK"
                                                                    "FAILED")))))
                    (lambda (k . _args)
                      (set-geis-manager-status-message! st (format #f "Verify failed: ~a" k))))))
              'handled)
            ((eqv? ch #\a)
              (let ((dlg (make-input-dialog
                           #:title "Add Remote Geis"
                           #:prompt "Spec (url#subpath[@ref]):"
                           #:placeholder "https://github.com/..."
                           #:validator (lambda (raw)
                                         (if (string-null? (string-trim-both raw))
                                           "Spec cannot be empty"
                                           #f))
                           #:on-submit
                           (lambda (raw)
                             (let ((spec (string-trim-both raw)))
                               (call-with-values
                                 (lambda () (parse-add-spec spec))
                                 (lambda (url subpath ref)
                                   (catch #t
                                     (lambda ()
                                       (geis-pkg-install! url subpath
                                                          #:ref ref
                                                          #:scope scope
                                                          #:workspace ws)
                                       (geis-manager-refresh! st)
                                       (set-geis-manager-status-message! st (format #f "Installed ~a" spec)))
                                     (lambda (k . args)
                                       (set-geis-manager-status-message!
                                         st
                                         (format #f "Install failed: ~a ~s" k args)))))))))))
                (push-child-modal! key-ctx dlg))
              'handled)
            ((eqv? ch #\d)
              (when entry
                (let ((fid (entry-fid entry)))
                  (if (member fid %core-protected-geasa)
                    (set-geis-manager-status-message! st "Core geis cannot be removed")
                    (let ((dlg (make-confirm-dialog
                                 #:title "Remove Geis"
                                 #:message (format #f "Remove ~a from ~a scope?" fid scope)
                                 #:confirm-label "Remove"
                                 #:cancel-label "Cancel"
                                 #:danger? #t
                                 #:on-confirm
                                 (lambda ()
                                   (catch #t
                                     (lambda ()
                                       (geis-pkg-remove! fid #:scope scope #:workspace ws)
                                       (geis-manager-refresh! st)
                                       (set-geis-manager-status-message! st (format #f "Removed ~a" fid)))
                                     (lambda (k . _args)
                                       (set-geis-manager-status-message! st (format #f "Remove failed: ~a" k))))))))
                      (push-child-modal! key-ctx dlg)))))
              'handled)
            ((or (eqv? ch #\newline) (eqv? ch #\return) (eq? ch 'enter))
              (set-modal-show-detail?! ov (not (modal-show-detail? ov)))
              'handled)
            ((or (eqv? ch #\q) (eqv? ch #\esc) (eq? ch 'esc) (eqv? ch #\escape)) 'close)
            (else
              (let ((ext (assoc ch %geis-manager-actions)))
                (if ext
                  (let ((handler (assq-ref (cdr ext) 'handler)))
                    (when (procedure? handler)
                      (handler scope entry `((overlay . ,ov) (state . ,st))))
                    'handled)
                  'pass))))))

;;;
;;; Modal Constructor & TUI Registration
;;;
(define*
  (make-geis-manager-overlay #:key (scope 'user) (workspace (getcwd)))
  "Create the lazy.nvim-style /geasa modal <overlay>."
  (let*
      ((init-sl (make-selectable-list '() #:max-visible 12))
       (st
         (%make-geis-manager-state scope
                                   workspace
                                   '()
                                   init-sl
                                   #f
                                   ""
                                   init-sl))
       (_ (geis-manager-refresh! st))
       (base-hints
         '(("j/k" . "nav") ("Tab" . "scope")
           ("/" . "filter")
           ("Space" . "toggle")
           ("p" . "pin")
           ("r" . "rollback")
           ("v" . "verify")
           ("a" . "add")
           ("d" . "remove")
           ("Enter" . "inspect")
           ("q" . "close")))
       (ext-hints
         (map
           (lambda (act)
             (cons
               (if (char? (car act))
                 (string (car act))
                 (format #f "~a" (car act)))
               (or (assq-ref (cdr act) 'label) "")))
           %geis-manager-actions)))
    (make-modal-overlay #:title
                        "Geis Manager"
                        #:tabs
                        '((user . "User") (project . "Project"))
                        #:active-tab
                        scope
                        #:state
                        st
                        #:width
                        0.85
                        #:height
                        0.8
                        #:filterable?
                        #t
                        #:filter-key-fn
                        entry-fid
                        #:body-renderer
                        render-geis-body
                        #:detail-renderer
                        render-geis-detail
                        #:show-detail?
                        #f
                        #:footer-hints
                        (append base-hints ext-hints)
                        #:key-bindings
                        (append
                          (map
                            (lambda (k)
                              (cons k
                                    (lambda (ov s ctx)
                                      (handle-geis-action ov s k ctx))))
                            (list #\j
                                  'down
                                  #\k
                                  'up
                                  #\space
                                  #\t
                                  #\p
                                  #\r
                                  #\v
                                  #\a
                                  #\d
                                  #\newline
                                  #\return
                                  'enter
                                  #\q
                                  'esc
                                  #\esc
                                  #\escape))
                          (map
                            (lambda (act)
                              (let ((k (car act)))
                                (cons k
                                      (lambda (ov s ctx)
                                        (handle-geis-action ov s k ctx)))))
                            %geis-manager-actions))
                        #:on-tab-change
                        (lambda (_ov s new-tab)
                          (set-geis-manager-scope! s new-tab)
                          (set-geis-manager-status-message! s #f)
                          (geis-manager-refresh! s)))))

(define* (register-geis-manager-tui! #:key register-category! register-command! open-overlay!)
         "Register the Geasa category and /geasa slash command using the provided TUI callbacks."
         (when (procedure? register-category!)
           (register-category! '("Geasa") "Manage installed geasa bundles"))
         (when (procedure? register-command!)
           (register-command!
             "geasa"
             "Open Geasa Manager modal"
             (lambda _
               (when (procedure? open-overlay!)
                 (open-overlay! (make-geis-manager-overlay))))
             #:path '("Geasa"))))
