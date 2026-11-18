;;; (geasa surfaces-geis-updater plugin) — Geasa Updater UI Extensions

(define-module
  (geasa surfaces-geis-updater plugin) #:use-module
  (ice-9 format)
  #:use-module
  (srfi srfi-1)
  #:use-module
  (geasa foundation-config plugin)
  #:use-module
  (geasa runtimes-geis-pkg plugin)
  #:use-module
  (geasa runtimes-geis-updater plugin)
  #:use-module
  (geasa surfaces-geis-manager plugin)
  #:use-module
  (geasa surfaces-widgets plugin)
  #:export
  (register-geis-updater-extensions!
    unregister-geis-updater-extensions!
    geis-updater-entry-badge
    geis-updater-banner-text
    geis-updater-cycle-mode!
    geis-updater-startup-notice))

(define (entry-fid entry)
        (if (pair? entry) (car entry) ""))

(define (entry-alist entry)
        (if (pair? entry) (cdr entry) '()))

(define (find-updater-entry scope fid workspace)
        (let* ((snap (or (geis-updater-snapshot scope)
                         (geis-updater-read-cache scope workspace)))
               (entries (or (and snap (assoc-ref snap 'entries)) '())))
          (assoc fid entries)))

(define %local-mod-cache (make-hash-table))

(define (clear-local-mod-cache!) (hash-clear! %local-mod-cache))

(define (cached-locally-modified? scope fid ws)
        (let*
            ((key (cons scope fid)) (existing (hash-get-handle %local-mod-cache key)))
          (if existing
            (cdr existing)
            (let
                ((mod?
                   (catch #t
                     (lambda ()
                       (geis-pkg-locally-modified? scope fid ws))
                     (lambda _
                       #f))))
              (hash-set! %local-mod-cache key mod?)
              mod?))))

(define*
  (geis-updater-entry-badge scope entry #:optional (workspace #f))
  "Render a status-badge-widget for ENTRY in SCOPE."
  (let*
      ((ws (or workspace (current-geis-manager-workspace) (getcwd)))
       (fid (entry-fid entry))
       (meta (entry-alist entry))
       (pinned? (assoc-ref meta 'pinned?))
       (modified? (cached-locally-modified? scope fid ws))
       (upd (find-updater-entry scope fid ws))
       (upd-meta
         (if (pair? upd)
           (cdr upd)
           '()))
       (status (assoc-ref upd-meta 'status))
       (remote-ver (assoc-ref upd-meta 'remote-version)))
    (cond
      (pinned? (status-badge-widget 'muted "pinned" #:glyph "📌"))
      (modified? (status-badge-widget 'warn "modified" #:glyph "✗"))
      ((eq? status 'error) (status-badge-widget 'error "failed" #:glyph "⚠"))
      ((eq? status 'update-available)
        (status-badge-widget 'info (or remote-ver "update") #:glyph "↑"))
      (else (status-badge-widget 'ok "current" #:glyph "●")))))

(define (format-age-seconds last-check now)
        (if (number? last-check)
          (let ((diff (max 0 (- now last-check))))
            (cond
              ((< diff 60) "just now")
              ((< diff 3600) (format #f "~am ago" (quotient diff 60)))
              ((< diff 86400) (format #f "~ah ago" (quotient diff 3600)))
              (else (format #f "~ad ago" (quotient diff 86400)))))
          "never"))

(define (load-scope-config scope workspace)
        (let
            ((local-path (string-append workspace "/.badb/config.scm")))
          (if (eq? scope 'project)
            (load-config #:global-path #f #:local-path local-path)
            (load-config #:local-path #f))))

(define*
  (geis-updater-banner-text scope #:optional (workspace #f) (now (current-time)))
  "Format the updater summary banner string for SCOPE."
  (let*
      ((ws (or workspace (current-geis-manager-workspace) (getcwd)))
       (cfg
         (catch #t
           (lambda ()
             (load-scope-config scope ws))
           (lambda _
             #f)))
       (upd-cfg
         (if cfg
           (badb-config-geasa-update cfg #:scope scope)
           '()))
       (mode (or (assoc-ref upd-cfg 'mode) 'notify))
       (snap (or (geis-updater-snapshot scope) (geis-updater-read-cache scope ws)))
       (last-check (and snap (assoc-ref snap 'last-check)))
       (entries (or (and snap (assoc-ref snap 'entries)) '()))
       (pending
         (count
           (lambda (e)
             (eq? (assoc-ref (cdr e) 'status) 'update-available))
           entries)))
    (format #f
            "Policy: ~a | Last check: ~a | Updates: ~a"
            mode
            (format-age-seconds last-check now)
            pending)))

(define*
  (geis-updater-cycle-mode! scope #:optional (workspace (getcwd)))
  "Cycle SCOPE's update policy (notify -> auto -> manual -> notify) and persist it."
  (let*
      ((cfg (load-scope-config scope workspace))
       (upd-cfg (badb-config-geasa-update cfg #:scope scope))
       (cur (or (assoc-ref upd-cfg 'mode) 'notify))
       (ttl (or (assoc-ref upd-cfg 'ttl-hours) 24))
       (nxt
         (case cur
           ((notify) 'auto)
           ((auto) 'manual)
           (else 'notify)))
       (new-upd `((mode . ,nxt) (ttl-hours . ,ttl))))
    (badb-config-set-geasa-update! cfg new-upd)
    (if (eq? scope 'project)
      (save-local-config! cfg (string-append workspace "/.badb/config.scm"))
      (save-global-config! cfg))
    nxt))

(define
  (update-single-entry! scope entry workspace)
  (let*
      ((meta (entry-alist entry))
       (src (assoc-ref meta 'source))
       (url (and (pair? src) (assoc-ref src 'url)))
       (path (and (pair? src) (assoc-ref src 'path)))
       (ref (or (and (pair? src) (assoc-ref src 'ref)) "main")))
    (if (and (string? url) (string? path))
      (begin
        (geis-pkg-install! url path #:ref ref #:scope scope #:workspace workspace)
        #t)
      #f)))

(define (geis-updater-startup-notice results)
        "Return a concise status-bar notice string from background check RESULTS, or #f."
        (if (list? results)
          (let loop
            ((rest results) (updated 0) (available 0))
            (if (null? rest)
              (cond
                ((> updated 0)
                  (format #f "Updated ~a geasa — takes effect next launch" updated))
                ((> available 0) (format #f "~a geasa update(s) available — /geasa" available))
                (else #f))
              (let*
                  ((res (car rest)) (mode (assoc-ref res 'mode))
                   (entries (or (assoc-ref res 'entries) '()))
                   (upd-n
                     (count
                       (lambda (e)
                         (eq? (assoc-ref (cdr e) 'status) 'updated))
                       entries))
                   (avail-n
                     (count
                       (lambda (e)
                         (eq? (assoc-ref (cdr e) 'status) 'update-available))
                       entries)))
                (loop (cdr rest)
                      (+ updated
                         (if (eq? mode 'auto)
                           upd-n
                           0)) (+ available avail-n)))))
          #f))

(define (handle-updater-check! scope ctx)
        (let*
            ((st (assq-ref ctx 'state))
             (ws
               (if st
                 (geis-manager-workspace st)
                 (getcwd))))
          (clear-local-mod-cache!)
          (catch #t
            (lambda ()
              (let
                  ((cfg
                     (catch #t
                       (lambda ()
                         (load-scope-config scope ws))
                       (lambda _
                         #f))))
                (geis-updater-check-scope! scope #:workspace ws #:force? #t #:config cfg))
              (when st
                (geis-manager-refresh! st)
                (set-geis-manager-status-message! st (format #f "Checked ~a updates" scope))))
            (lambda (k . _args)
              (when st (set-geis-manager-status-message! st (format #f "Check failed: ~a" k)))))))

(define
  (handle-updater-update-one! scope entry ctx)
  (let*
      ((st (assq-ref ctx 'state))
       (ws
         (if st
           (geis-manager-workspace st)
           (getcwd))))
    (when entry
      (catch #t
        (lambda ()
          (if (update-single-entry! scope entry ws)
            (when st
              (geis-manager-refresh! st)
              (set-geis-manager-status-message! st (format #f "Updated ~a" (entry-fid entry))))
            (when st
              (set-geis-manager-status-message! st "No remote source for selected geis"))))
        (lambda (k . _args)
          (when st
            (set-geis-manager-status-message! st (format #f "Update failed: ~a" k))))))))

(define (handle-updater-update-all! scope ctx)
        (let*
            ((st (assq-ref ctx 'state))
             (ws
               (if st
                 (geis-manager-workspace st)
                 (getcwd)))
             (entries
               (if st
                 (geis-manager-entries st)
                 '()))
             (count-ok 0))
          (for-each
            (lambda (e)
              (let*
                  ((fid (entry-fid e))
                   (upd (find-updater-entry scope fid ws))
                   (status (and (pair? upd) (assoc-ref (cdr upd) 'status))))
                (when (eq? status 'update-available)
                  (catch #t
                    (lambda ()
                      (when (update-single-entry! scope e ws) (set! count-ok (+ count-ok 1))))
                    (lambda _
                      #f)))))
            entries)
          (when st
            (geis-manager-refresh! st)
            (set-geis-manager-status-message! st (format #f "Updated ~a geasa" count-ok)))))

(define (handle-updater-sync! scope ctx)
        (let*
            ((st (assq-ref ctx 'state))
             (ws
               (if st
                 (geis-manager-workspace st)
                 (getcwd))))
          (catch #t
            (lambda ()
              (let*
                  ((cfg (load-scope-config scope ws))
                   (declared (badb-config-geasa cfg #:scope scope)))
                (geis-pkg-sync! declared #:scope scope #:workspace ws)
                (when st
                  (geis-manager-refresh! st)
                  (set-geis-manager-status-message! st (format #f "Synced ~a config" scope)))))
            (lambda (k . _args)
              (when st (set-geis-manager-status-message! st (format #f "Sync failed: ~a" k)))))))

(define (handle-updater-cycle-mode! scope ctx)
        (let*
            ((st (assq-ref ctx 'state))
             (ws
               (if st
                 (geis-manager-workspace st)
                 (getcwd))))
          (catch #t
            (lambda ()
              (let ((nxt (geis-updater-cycle-mode! scope ws)))
                (when st
                  (set-geis-manager-status-message! st (format #f "Update policy: ~a" nxt)))))
            (lambda (k . _args)
              (when st
                (set-geis-manager-status-message! st (format #f "Mode change failed: ~a" k)))))))

(define (register-geis-updater-extensions!)
        "Register updater status column, summary banner, and c/u/U/s/m actions into surfaces-geis-manager."
        (register-geis-manager-column! 'update-status
                                       #:header
                                       "Status"
                                       #:width
                                       16
                                       #:render
                                       (lambda (scope entry)
                                         (geis-updater-entry-badge scope entry)))
        (register-geis-manager-banner! 'updater-summary
                                       #:render
                                       (lambda (scope)
                                         (text-widget (geis-updater-banner-text scope) #:role 'dim #:dim? #t)))
        (register-geis-manager-action! #\c
                                       #:label
                                       "check"
                                       #:handler
                                       (lambda (scope _entry ctx)
                                         (handle-updater-check! scope ctx)))
        (register-geis-manager-action! #\u
                                       #:label
                                       "update"
                                       #:handler
                                       (lambda (scope entry ctx)
                                         (handle-updater-update-one! scope entry ctx)))
        (register-geis-manager-action! #\U
                                       #:label
                                       "update-all"
                                       #:handler
                                       (lambda (scope _entry ctx)
                                         (handle-updater-update-all! scope ctx)))
        (register-geis-manager-action! #\s
                                       #:label
                                       "sync"
                                       #:handler
                                       (lambda (scope _entry ctx)
                                         (handle-updater-sync! scope ctx)))
        (register-geis-manager-action! #\m
                                       #:label
                                       "mode"
                                       #:handler
                                       (lambda (scope _entry ctx)
                                         (handle-updater-cycle-mode! scope ctx))))

(define (unregister-geis-updater-extensions!)
        "Remove updater column, banner, and actions from surfaces-geis-manager."
        (unregister-geis-manager-column! 'update-status)
        (unregister-geis-manager-banner! 'updater-summary)
        (for-each unregister-geis-manager-action! '(#\c #\u #\U #\s #\m)))
