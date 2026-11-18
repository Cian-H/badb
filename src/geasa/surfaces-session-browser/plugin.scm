;;; (geasa surfaces-session-browser plugin) — /sessions & /journal Time-Travel Modal
;;;
;;; Provides make-session-browser-overlay and register-session-browser-tui!
;;; with Sessions | Checkpoints tabs, / fuzzy filtering, Enter detail preview
;;; (recent turns / file changes + journal provenance bindings), and
;;; confirm/input dialogs for fork, delete, rollback, and checkpoint creation.

(define-module (geasa surfaces-session-browser plugin)
               #:use-module ((badb kernel journal)
                             #:select ((journal-entries . read-journal-entries)
                                       journal?
                                       journal-entry?
                                       journal-entry-ref
                                       journal-entry-timestamp
                                       journal-entry-iteration
                                       journal-entry-feature-id
                                       journal-entry-commit
                                       journal-entry-test-verdict
                                       journal-entry-author
                                       journal-entry-details
                                       journal-entry-ast-diff-hash
                                       journal-entry-spec-hash))
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa runtimes-rollback plugin)
               #:use-module (geasa surfaces-notify plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (ice-9 format)
               #:use-module (pfds hamts)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (<session-browser-state>
                          session-browser-state?
                          session-browser-sessions
                          session-browser-checkpoints
                          session-browser-journal-entries
                          session-browser-status-message
                          make-session-browser-overlay
                          register-session-browser-tui!))

;;;
;;; State Record & Data Helpers
;;;
(define-record-type <session-browser-state>
                    (%make-session-browser-state sessions-list
                                                 checkpoints-list
                                                 journal-entries
                                                 sessions-provider
                                                 checkpoints-provider
                                                 journal-provider
                                                 rollback-manager
                                                 on-resume
                                                 on-fork
                                                 on-delete
                                                 on-rollback
                                                 on-create-checkpoint
                                                 filter-query
                                                 filtered-sessions
                                                 filtered-checkpoints
                                                 status-message)
                    session-browser-state?
                    (sessions-list sb-sessions-list set-sb-sessions-list!)
                    (checkpoints-list sb-checkpoints-list set-sb-checkpoints-list!)
                    (journal-entries session-browser-journal-entries set-sb-journal-entries!)
                    (sessions-provider sb-sessions-provider)
                    (checkpoints-provider sb-checkpoints-provider)
                    (journal-provider sb-journal-provider)
                    (rollback-manager sb-rollback-manager)
                    (on-resume sb-on-resume)
                    (on-fork sb-on-fork)
                    (on-delete sb-on-delete)
                    (on-rollback sb-on-rollback)
                    (on-create-checkpoint sb-on-create-checkpoint)
                    (filter-query sb-filter-query set-sb-filter-query!)
                    (filtered-sessions sb-filtered-sessions set-sb-filtered-sessions!)
                    (filtered-checkpoints sb-filtered-checkpoints set-sb-filtered-checkpoints!)
                    (status-message session-browser-status-message set-sb-status-message!))

(define (session-browser-sessions st)
        (selectable-list-items (sb-sessions-list st)))

(define (session-browser-checkpoints st)
        (selectable-list-items (sb-checkpoints-list st)))

(define (session-id s)
        (ensure-string (or (assq-ref s 'id) "")))

(define (session-title s)
        (ensure-string (or (assq-ref s 'title) (session-id s))))

(define (session-turns s)
        (or (assq-ref s 'turns)
            (let ((rt (assq-ref s 'recent-turns)))
              (if (list? rt) (length rt) 0))))

(define (session-timestamp s)
        (ensure-string (or (assq-ref s 'timestamp) "")))

(define (session-search-key s)
        (string-append (session-id s) " " (session-title s)))

(define (cp-id cp)
        (cond
          ((checkpoint? cp) (ensure-string (checkpoint-id cp)))
          ((pair? cp) (ensure-string (or (assq-ref cp 'id) "")))
          (else "")))

(define (cp-desc cp)
        (cond
          ((checkpoint? cp) (ensure-string (checkpoint-description cp)))
          ((pair? cp) (ensure-string (or (assq-ref cp 'description) "")))
          (else "")))

(define (cp-timestamp cp)
        (cond
          ((checkpoint? cp) (format #f "~a" (checkpoint-timestamp cp)))
          ((pair? cp) (ensure-string (or (assq-ref cp 'timestamp) "")))
          (else "")))

(define (cp-files cp)
        (let ((raw (cond
                     ((checkpoint? cp) (checkpoint-files cp))
                     ((pair? cp) (assq-ref cp 'files))
                     (else '()))))
          (cond
            ((hamt? raw) (hamt-fold (lambda (k v acc) (cons (cons k v) acc)) '() raw))
            ((list? raw) raw)
            (else '()))))

(define (cp-search-key cp)
        (string-append (cp-id cp) " " (cp-desc cp)))

(define (jentry-field je key)
        (cond
          ((journal-entry? je)
            (case key
              ((timestamp) (journal-entry-timestamp je))
              ((iteration) (journal-entry-iteration je))
              ((feature-id) (journal-entry-feature-id je))
              ((commit) (journal-entry-commit je))
              ((test-verdict) (journal-entry-test-verdict je))
              ((author) (journal-entry-author je))
              ((details) (journal-entry-details je))
              ((ast-diff-hash) (journal-entry-ast-diff-hash je))
              ((spec-hash) (journal-entry-spec-hash je))
              (else #f)))
          ((pair? je)
            (or (journal-entry-ref je key)
                (journal-entry-ref je (symbol->string key))))
          (else #f)))

(define (find-provenance-for-cp cp jentries)
        (let ((direct (and (pair? cp) (assq-ref cp 'journal-entry))))
          (or direct
              (let ((cid (cp-id cp)))
                (find (lambda (je)
                        (let ((cmt (jentry-field je 'commit))
                              (det (jentry-field je 'details)))
                          (or (equal? cmt cid)
                              (and (string? det) (string-contains det cid)))))
                      jentries)))))

(define %cp-prov-cache (make-weak-key-hash-table 61))

(define (build-cp-prov-table cps jentries)
        (let ((tbl (make-hash-table)))
          (for-each
            (lambda (cp)
              (let ((cid (cp-id cp)))
                (when (and (string? cid) (not (string-null? cid)))
                  (hash-set! tbl cid (find-provenance-for-cp cp jentries)))))
            cps)
          tbl))

(define (cached-provenance-for-cp st cp jentries)
        (let ((tbl (hashq-ref %cp-prov-cache st))
              (cid (cp-id cp)))
          (if tbl
            (hash-ref tbl cid #f)
            (find-provenance-for-cp cp jentries))))

;;;
;;; Refresh & Filter Helpers
;;;
(define (sb-refresh! st)
        (let* ((sess (if (procedure? (sb-sessions-provider st))
                       (or ((sb-sessions-provider st)) '())
                       '()))
               (cps (cond
                      ((procedure? (sb-checkpoints-provider st))
                        (or ((sb-checkpoints-provider st)) '()))
                      ((rollback-manager? (sb-rollback-manager st))
                        (list-checkpoints (sb-rollback-manager st)))
                      (else '())))
               (jents (if (procedure? (sb-journal-provider st))
                        (or ((sb-journal-provider st)) '())
                        '()))
               (s-idx (selectable-list-selected-index (sb-sessions-list st)))
               (c-idx (selectable-list-selected-index (sb-checkpoints-list st))))
          (set-sb-sessions-list! st (make-selectable-list sess #:selected-index s-idx #:max-visible 10))
          (set-sb-checkpoints-list! st (make-selectable-list cps #:selected-index c-idx #:max-visible 10))
          (set-sb-journal-entries! st jents)
          (hashq-set! %cp-prov-cache st (build-cp-prov-table cps jents))
          (set-sb-filter-query! st #f)
          (set-sb-filtered-sessions! st #f)
          (set-sb-filtered-checkpoints! st #f)
          st))

(define (sb-effective-list ov st tab)
        (let ((q (if (and ov (modal-overlay? ov)) (modal-filter-query ov) "")))
          (unless (equal? q (sb-filter-query st))
            (set-sb-filter-query! st q)
            (if (string-null? q)
              (begin
                (set-sb-filtered-sessions! st #f)
                (set-sb-filtered-checkpoints! st #f))
              (begin
                (set-sb-filtered-sessions!
                  st (filter-selectable-list (sb-sessions-list st) q session-search-key))
                (set-sb-filtered-checkpoints!
                  st (filter-selectable-list (sb-checkpoints-list st) q cp-search-key)))))
          (if (eq? tab 'checkpoints)
            (or (sb-filtered-checkpoints st) (sb-checkpoints-list st))
            (or (sb-filtered-sessions st) (sb-sessions-list st)))))

(define (sb-set-effective-list! st tab new-slist)
        (if (eq? tab 'checkpoints)
          (if (sb-filtered-checkpoints st)
            (set-sb-filtered-checkpoints! st new-slist)
            (set-sb-checkpoints-list! st new-slist))
          (if (sb-filtered-sessions st)
            (set-sb-filtered-sessions! st new-slist)
            (set-sb-sessions-list! st new-slist))))

;;;
;;; Renderers
;;;
(define (render-session-row s _selected? width)
        (let* ((id (session-id s))
               (ttl (session-title s))
               (turns (format #f "~at" (session-turns s)))
               (ts (session-timestamp s))
               (title-w (max 10 (- width 34))))
          (hstack-widget
            (list (text-widget (pad-str id 14) #:role 'accent #:bold? #t)
                  (text-widget (pad-str ttl title-w))
                  (text-widget (pad-str turns 5) #:role 'info)
                  (text-widget (pad-str ts 12) #:role 'dim #:dim? #t))
            #:gap 1)))

(define (render-checkpoint-row cp prov _selected? width)
        (let* ((cid (cp-id cp))
               (desc (cp-desc cp))
               (verdict (and prov (jentry-field prov 'test-verdict)))
               (badge (if verdict
                        (status-badge-widget
                          (if (string-ci=? (ensure-string verdict) "pass") 'ok 'error)
                          (ensure-string verdict))
                        (text-widget "cp" #:role 'dim)))
               (desc-w (max 10 (- width 32))))
          (hstack-widget
            (list (text-widget (pad-str cid 16) #:role 'accent #:bold? #t)
                  (text-widget (pad-str desc desc-w))
                  badge)
            #:gap 1)))

(define (render-sb-body ov st rect)
        (let* ((tab (modal-active-tab ov))
               (w (rect-width rect))
               (slist (sb-effective-list ov st tab))
               (jents (session-browser-journal-entries st))
               (list-w
                 (if (null? (selectable-list-items slist))
                   (text-widget
                     (if (eq? tab 'checkpoints)
                       "  No checkpoints recorded."
                       "  No sessions found.")
                     #:role 'dim #:dim? #t)
                   (selectable-list-widget
                     slist
                     #:item-renderer
                     (lambda (item selected?)
                       (if (eq? tab 'checkpoints)
                         (render-checkpoint-row item (cached-provenance-for-cp st item jents) selected? w)
                         (render-session-row item selected? w))))))
               (status (session-browser-status-message st))
               (status-w (if (and (string? status) (> (string-length status) 0))
                           (list (text-widget status #:role 'info))
                           '())))
          (vstack-widget (cons list-w status-w))))

(define (render-session-detail s w)
        (let* ((turns (or (assq-ref s 'recent-turns) '()))
               (hdr (format #f "Session ~a — ~a (~a turns, ~a)"
                            (session-id s) (session-title s) (session-turns s) (session-timestamp s)))
               (turn-ws (if (null? turns)
                          (list (text-widget "  (no recent turns recorded)" #:role 'dim #:dim? #t))
                          (map (lambda (t) (text-widget (clip-str (ensure-string t) w)))
                               (take turns (min 5 (length turns)))))))
          (vstack-widget (cons (text-widget (clip-str hdr w) #:bold? #t) turn-ws))))

(define (render-checkpoint-detail cp jentries w)
        (let* ((files (cp-files cp))
               (prov (find-provenance-for-cp cp jentries))
               (hdr (format #f "Checkpoint ~a — ~a (~a)"
                            (cp-id cp) (cp-desc cp) (cp-timestamp cp)))
               (prov-line
                 (if prov
                   (format #f "Provenance: feature=~a iter=~a verdict=~a spec=~a"
                           (or (jentry-field prov 'feature-id) "-")
                           (or (jentry-field prov 'iteration) "-")
                           (or (jentry-field prov 'test-verdict) "-")
                           (clip-str (ensure-string (or (jentry-field prov 'spec-hash) "-")) 12))
                   "Provenance: (no linked journal entry)"))
               (file-lines
                 (if (null? files)
                   (list (text-widget "  Files: (none)" #:role 'dim #:dim? #t))
                   (map (lambda (f)
                          (let ((p (if (pair? f) (car f) (ensure-string f))))
                            (text-widget (clip-str (string-append "  • " p) w))))
                        (take files (min 4 (length files)))))))
          (vstack-widget
            (append (list (text-widget (clip-str hdr w) #:bold? #t)
                          (text-widget (clip-str prov-line w) #:role 'info))
                    file-lines))))

(define (render-sb-detail ov st rect)
        (let* ((tab (modal-active-tab ov))
               (slist (sb-effective-list ov st tab))
               (item (list-selected-item slist))
               (w (rect-width rect)))
          (cond
            ((not item) (text-widget "Nothing selected." #:role 'dim #:dim? #t))
            ((eq? tab 'checkpoints)
              (render-checkpoint-detail item (session-browser-journal-entries st) w))
            (else (render-session-detail item w)))))

;;;
;;; Dialog & Action Handlers
;;;
(define (push-child-modal! key-ctx dlg)
        (let ((push-fn (assq-ref key-ctx 'push-modal!))
              (stack (assq-ref key-ctx 'overlay-stack)))
          (cond
            ((procedure? push-fn) (push-fn dlg) #t)
            ((overlay-stack? stack) (overlay-stack-push-modal! stack dlg) #t)
            (else #f))))

(define (handle-session-fork ov st item key-ctx)
        (let* ((sid (session-id item))
               (dlg (make-input-dialog
                      #:title "Fork Session"
                      #:prompt (format #f "Fork '~a' as:" sid)
                      #:initial (string-append sid "-fork")
                      #:validator (lambda (raw)
                                    (if (string-null? (string-trim-both raw))
                                      "Session name cannot be empty"
                                      #f))
                      #:on-submit
                      (lambda (raw)
                        (let ((name (string-trim-both raw)))
                          (when (procedure? (sb-on-fork st))
                            ((sb-on-fork st) item name))
                          (sb-refresh! st)
                          (let ((msg (format #f "Forked session ~a -> ~a" sid name)))
                            (set-sb-status-message! st msg)
                            (notify-info! msg #:title "Sessions" #:source "surfaces-session-browser")))))))
          (push-child-modal! key-ctx dlg)))

(define (handle-session-delete ov st item key-ctx)
        (let* ((sid (session-id item))
               (dlg (make-confirm-dialog
                      #:title "Delete Session"
                      #:message (format #f "Delete session '~a'?" sid)
                      #:confirm-label "Delete"
                      #:cancel-label "Cancel"
                      #:danger? #t
                      #:on-confirm
                      (lambda ()
                        (when (procedure? (sb-on-delete st))
                          ((sb-on-delete st) item))
                        (sb-refresh! st)
                        (let ((msg (format #f "Deleted session ~a" sid)))
                          (set-sb-status-message! st msg)
                          (notify-warn! msg #:title "Sessions" #:source "surfaces-session-browser"))))))
          (push-child-modal! key-ctx dlg)))

(define (handle-checkpoint-rollback ov st item key-ctx)
        (let* ((cid (cp-id item))
               (dlg (make-confirm-dialog
                      #:title "Rollback Checkpoint"
                      #:message (format #f "Rollback workspace to checkpoint '~a'?" cid)
                      #:confirm-label "Rollback"
                      #:cancel-label "Cancel"
                      #:danger? #t
                      #:on-confirm
                      (lambda ()
                        (cond
                          ((procedure? (sb-on-rollback st))
                            ((sb-on-rollback st) item))
                          ((rollback-manager? (sb-rollback-manager st))
                            (rollback-to-checkpoint! (sb-rollback-manager st) cid)))
                        (sb-refresh! st)
                        (let ((msg (format #f "Rolled back to ~a" cid)))
                          (set-sb-status-message! st msg)
                          (notify-info! msg #:title "Checkpoints" #:source "surfaces-session-browser"))))))
          (push-child-modal! key-ctx dlg)))

(define (handle-checkpoint-create ov st key-ctx)
        (let ((dlg (make-input-dialog
                     #:title "Create Checkpoint"
                     #:prompt "Checkpoint description:"
                     #:placeholder "Before refactor..."
                     #:validator (lambda (raw)
                                   (if (string-null? (string-trim-both raw))
                                     "Description cannot be empty"
                                     #f))
                     #:on-submit
                     (lambda (raw)
                       (let ((desc (string-trim-both raw)))
                         (cond
                           ((procedure? (sb-on-create-checkpoint st))
                             ((sb-on-create-checkpoint st) desc))
                           ((rollback-manager? (sb-rollback-manager st))
                             (create-checkpoint! (sb-rollback-manager st) desc)))
                         (sb-refresh! st)
                         (let ((msg (format #f "Created checkpoint: ~a" desc)))
                           (set-sb-status-message! st msg)
                           (notify-info! msg #:title "Checkpoints" #:source "surfaces-session-browser")))))))
          (push-child-modal! key-ctx dlg)))

;;;
;;; Public Overlay & TUI Registration
;;;
(define (normalize-tab tab)
        (if (eq? tab 'journal) 'checkpoints 'sessions))

(define* (make-session-browser-overlay #:key
                                       (active-tab 'sessions)
                                       (sessions '())
                                       (checkpoints '())
                                       (journal-entries '())
                                       (sessions-provider #f)
                                       (checkpoints-provider #f)
                                       (journal #f)
                                       (journal-provider #f)
                                       (rollback-manager #f)
                                       (on-resume #f)
                                       (on-fork #f)
                                       (on-delete #f)
                                       (on-rollback #f)
                                       (on-create-checkpoint #f))
         "Create the /sessions & /journal time-travel modal <overlay>."
         (let* ((s-prov (or sessions-provider (lambda () sessions)))
                (c-prov (or checkpoints-provider
                            (and (not rollback-manager) (lambda () checkpoints))))
                (j-prov (or journal-provider
                            (and (journal? journal) (lambda () (read-journal-entries journal)))
                            (lambda () journal-entries)))
                (st (%make-session-browser-state
                      (make-selectable-list '() #:max-visible 10)
                      (make-selectable-list '() #:max-visible 10)
                      '()
                      s-prov
                      c-prov
                      j-prov
                      rollback-manager
                      on-resume
                      on-fork
                      on-delete
                      on-rollback
                      on-create-checkpoint
                      #f
                      #f
                      #f
                      #f))
                (_ (sb-refresh! st)))
           (make-modal-overlay
             #:title "Sessions & Checkpoints"
             #:tabs '((sessions . "Sessions") (checkpoints . "Checkpoints"))
             #:active-tab (normalize-tab active-tab)
             #:state st
             #:width 0.85
             #:height 0.80
             #:filterable? #t
             #:filter-key-fn (lambda (it)
                               (if (or (checkpoint? it) (assq-ref it 'description))
                                 (cp-search-key it)
                                 (session-search-key it)))
             #:filter-placeholder "filter sessions/checkpoints..."
             #:body-renderer render-sb-body
             #:detail-renderer render-sb-detail
             #:show-detail? #t
             #:footer-hints
             (lambda (_s)
               '(("j/k" . "nav")
                 ("Tab" . "switch")
                 ("/" . "filter")
                 ("Enter" . "detail")
                 ("r" . "resume/rollback")
                 ("f/c" . "fork/ckpt")
                 ("d" . "delete")
                 ("q" . "close")))
             #:key-bindings
             (list
               (cons (list #\j 'down)
                     (lambda (ov s _ctx)
                       (let* ((tab (modal-active-tab ov))
                              (cur (sb-effective-list ov s tab)))
                         (sb-set-effective-list! s tab (list-move cur 1))
                         'handled)))
               (cons (list #\k 'up)
                     (lambda (ov s _ctx)
                       (let* ((tab (modal-active-tab ov))
                              (cur (sb-effective-list ov s tab)))
                         (sb-set-effective-list! s tab (list-move cur -1))
                         'handled)))
               (cons 'enter
                     (lambda (ov _s _ctx)
                       (set-modal-show-detail?! ov (not (modal-show-detail? ov)))
                       'handled))
               (cons #\r
                     (lambda (ov s key-ctx)
                       (let* ((tab (modal-active-tab ov))
                              (item (list-selected-item (sb-effective-list ov s tab))))
                         (when item
                           (if (eq? tab 'checkpoints)
                             (handle-checkpoint-rollback ov s item key-ctx)
                             (begin
                               (when (procedure? (sb-on-resume s))
                                 ((sb-on-resume s) item))
                               (let ((msg (format #f "Resumed session ~a" (session-id item))))
                                 (set-sb-status-message! s msg)
                                 (notify-info! msg #:title "Sessions" #:source "surfaces-session-browser"))))))
                       'handled))
               (cons #\f
                     (lambda (ov s key-ctx)
                       (when (eq? (modal-active-tab ov) 'sessions)
                         (let ((item (list-selected-item (sb-effective-list ov s 'sessions))))
                           (when item
                             (handle-session-fork ov s item key-ctx))))
                       'handled))
               (cons #\d
                     (lambda (ov s key-ctx)
                       (when (eq? (modal-active-tab ov) 'sessions)
                         (let ((item (list-selected-item (sb-effective-list ov s 'sessions))))
                           (when item
                             (handle-session-delete ov s item key-ctx))))
                       'handled))
               (cons #\c
                     (lambda (ov s key-ctx)
                       (when (eq? (modal-active-tab ov) 'checkpoints)
                         (handle-checkpoint-create ov s key-ctx))
                       'handled))
               (cons #\q
                     (lambda (_ov _s _ctx) 'close))))))

(define* (register-session-browser-tui! #:key
                                        register-category!
                                        register-command!
                                        open-overlay!
                                        (sessions-provider #f)
                                        (checkpoints-provider #f)
                                        (journal #f)
                                        (journal-provider #f)
                                        (rollback-manager #f)
                                        (on-resume #f)
                                        (on-fork #f)
                                        (on-delete #f)
                                        (on-rollback #f)
                                        (on-create-checkpoint #f))
         "Register the Sessions category and /sessions, /journal slash commands."
         (when (procedure? register-category!)
           (register-category! '("Sessions") "Browse sessions, checkpoints, and journal provenance"))
         (let ((make-ov
                 (lambda (tab)
                   (make-session-browser-overlay
                     #:active-tab tab
                     #:sessions-provider sessions-provider
                     #:checkpoints-provider checkpoints-provider
                     #:journal journal
                     #:journal-provider journal-provider
                     #:rollback-manager rollback-manager
                     #:on-resume on-resume
                     #:on-fork on-fork
                     #:on-delete on-delete
                     #:on-rollback on-rollback
                     #:on-create-checkpoint on-create-checkpoint))))
           (when (procedure? register-command!)
             (register-command!
               "sessions"
               "Open Sessions & Checkpoints browser"
               (lambda _
                 (when (procedure? open-overlay!)
                   (open-overlay! (make-ov 'sessions))))
               #:path '("Sessions"))
             (register-command!
               "journal"
               "Open Journal & Checkpoints browser"
               (lambda _
                 (when (procedure? open-overlay!)
                   (open-overlay! (make-ov 'journal))))
               #:path '("Sessions")))))
