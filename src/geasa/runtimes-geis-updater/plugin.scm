(define-module (geasa runtimes-geis-updater plugin)
               #:use-module (ice-9 format)
               #:use-module (ice-9 threads)
               #:use-module (srfi srfi-1)
               #:use-module (badb geis bundle)
               #:use-module (geasa foundation-config plugin)
               #:use-module (geasa runtimes-geis-pkg plugin)
               #:export (geis-updater-cache-path
                          geis-updater-read-cache
                          geis-updater-write-cache!
                          geis-updater-cache-fresh?
                          geis-updater-snapshot
                          geis-updater-check-scope!
                          start-background-geasa-update-check!))

(define %updater-mutex (make-mutex))
(define %updater-snapshots '())

(define (set-snapshot! scope data)
        (with-mutex %updater-mutex
                    (set! %updater-snapshots
                          (assoc-set! %updater-snapshots scope data))))

(define (geis-updater-snapshot scope)
        "Return the latest thread-safe update check snapshot for SCOPE, or #f."
        (with-mutex %updater-mutex
                    (assoc-ref %updater-snapshots scope)))

(define* (geis-updater-cache-path scope #:optional (workspace (getcwd)))
         "Return the update cache file path for SCOPE ('user or 'project)."
         (string-append (dirname (geis-pkg-store-dir scope workspace))
                        "/geasa-update-cache.scm"))

(define (mkdir-p dir)
        (unless (or (string-null? dir) (string=? dir "/") (file-exists? dir))
          (mkdir-p (dirname dir))
          (catch #t
            (lambda () (mkdir dir))
            (lambda _ #f))))

(define* (geis-updater-read-cache scope #:optional (workspace (getcwd)))
         "Read the update cache alist for SCOPE, or return a fresh empty cache."
         (let ((path (geis-updater-cache-path scope workspace)))
           (if (file-exists? path)
             (catch #t
               (lambda ()
                 (let ((data (call-with-input-file path read)))
                   (if (list? data)
                     data
                     '((cache-version . 1) (last-check . #f) (repos) (entries)))))
               (lambda _
                 '((cache-version . 1) (last-check . #f) (repos) (entries))))
             '((cache-version . 1) (last-check . #f) (repos) (entries)))))

(define* (geis-updater-write-cache! scope cache #:optional (workspace (getcwd)))
         "Atomically write CACHE alist for SCOPE."
         (let* ((path (geis-updater-cache-path scope workspace))
                (dir (dirname path))
                (tmp (string-append path ".tmp." (number->string (getpid)))))
           (mkdir-p dir)
           (call-with-output-file tmp
                                  (lambda (port)
                                    (write cache port)
                                    (newline port)))
           (rename-file tmp path)
           path))

(define (geis-updater-cache-fresh? cache ttl-hours now)
        "Return #t when CACHE's last-check is within TTL-HOURS of NOW."
        (let ((last-check (assoc-ref cache 'last-check)))
          (and (number? last-check)
               (number? ttl-hours)
               (> ttl-hours 0)
               (number? now)
               (< (- now last-check) (* ttl-hours 3600)))))

(define (scope-update-policy scope workspace config-override)
        (let* ((cfg (or config-override
                        (catch #t
                          (lambda () (load-badb-config #:workspace workspace))
                          (lambda _ '()))))
               (policy (cond
                         ((list? cfg)
                           (or (assoc-ref cfg (if (eq? scope 'project)
                                                'project-geasa-update
                                                'user-geasa-update))
                               '()))
                         (else
                           (or (badb-config-geasa-update cfg #:scope scope) '()))))
               (mode (or (assoc-ref policy 'mode) 'notify))
               (ttl (or (assoc-ref policy 'ttl-hours) 24)))
          (values mode ttl)))

(define (entry-source-key entry)
        (let ((src (assoc-ref entry 'source)))
          (and (list? src)
               (eq? (assoc-ref src 'type) 'git)
               (let ((url (assoc-ref src 'url))
                     (ref (or (assoc-ref src 'ref) "main"))
                     (path (assoc-ref src 'path)))
                 (and (string? url) (not (string-null? url))
                      (string? path) (not (string-null? path))
                      (cons url ref))))))

(define (group-entries-by-repo entries)
        (let loop ((rest entries) (groups '()))
                  (if (null? rest)
                    (reverse groups)
                    (let* ((entry (car rest))
                           (key (entry-source-key entry)))
                      (if key
                        (let ((existing (assoc key groups)))
                          (if existing
                            (loop (cdr rest)
                                  (assoc-set! groups key (append (cdr existing) (list entry))))
                            (loop (cdr rest)
                                  (cons (cons key (list entry)) groups))))
                        (loop (cdr rest) groups))))))

(define (safe-ls-remote url ref)
        (catch #t
          (lambda () (geis-pkg-ls-remote url ref))
          (lambda _ #f)))

(define (safe-fetch-manifest url subpath ref)
        (catch #t
          (lambda () (geis-pkg-fetch-manifest! url subpath ref))
          (lambda _ #f)))

(define (evaluate-entry scope workspace mode entry remote-commit remote-version now)
        (let* ((fid (car entry))
               (local-ver (or (assoc-ref entry 'version) "0.0.0.0"))
               (pinned? (assoc-ref entry 'pinned?))
               (enabled? (if (assoc 'enabled? (cdr entry))
                           (assoc-ref (cdr entry) 'enabled?)
                           #t))
               (src (assoc-ref entry 'source))
               (url (assoc-ref src 'url))
               (ref (or (assoc-ref src 'ref) "main"))
               (subpath (assoc-ref src 'path)))
          (cond
            ((not remote-commit)
              `(,fid (status . offline)
                     (local-version . ,local-ver)
                     (remote-version . #f)
                     (remote-commit . #f)
                     (checked-at . ,now)))
            ((not remote-version)
              `(,fid (status . error)
                     (local-version . ,local-ver)
                     (remote-version . #f)
                     (remote-commit . ,remote-commit)
                     (checked-at . ,now)))
            (pinned?
              `(,fid (status . pinned)
                     (local-version . ,local-ver)
                     (remote-version . ,remote-version)
                     (remote-commit . ,remote-commit)
                     (checked-at . ,now)))
            ((geis-pkg-locally-modified? scope fid workspace)
              `(,fid (status . local-mod)
                     (local-version . ,local-ver)
                     (remote-version . ,remote-version)
                     (remote-commit . ,remote-commit)
                     (checked-at . ,now)))
            ((> (calver-compare remote-version local-ver) 0)
              (if (eq? mode 'auto)
                (let ((res (catch #t
                             (lambda ()
                               (geis-pkg-install! url subpath
                                                  #:ref ref
                                                  #:scope scope
                                                  #:workspace workspace
                                                  #:pin? #f
                                                  #:enabled? enabled?))
                             (lambda _ '((ok . #f))))))
                  (if (assoc-ref res 'ok)
                    `(,fid (status . updated)
                           (local-version . ,remote-version)
                           (remote-version . ,remote-version)
                           (remote-commit . ,remote-commit)
                           (checked-at . ,now))
                    `(,fid (status . failed)
                           (local-version . ,local-ver)
                           (remote-version . ,remote-version)
                           (remote-commit . ,remote-commit)
                           (checked-at . ,now))))
                `(,fid (status . update-available)
                       (local-version . ,local-ver)
                       (remote-version . ,remote-version)
                       (remote-commit . ,remote-commit)
                       (checked-at . ,now))))
            (else
              `(,fid (status . up-to-date)
                     (local-version . ,local-ver)
                     (remote-version . ,remote-version)
                     (remote-commit . ,remote-commit)
                     (checked-at . ,now))))))

(define (count-status entries target)
        (length (filter (lambda (e) (eq? (assoc-ref (cdr e) 'status) target)) entries)))

(define (format-summary-message mode avail updated)
        (cond
          ((and (eq? mode 'auto) (> updated 0))
            (format #f "Updated ~a geasa — takes effect next launch" updated))
          ((and (eq? mode 'notify) (> avail 0))
            (format #f "~a geasa update(s) available — /geasa" avail))
          (else #f)))

(define (make-result scope mode from-cache? checked-at entries)
        (let* ((avail (count-status entries 'update-available))
               (updated (count-status entries 'updated))
               (msg (format-summary-message mode avail updated)))
          `((ok . #t)
            (scope . ,scope)
            (mode . ,mode)
            (from-cache? . ,from-cache?)
            (checked-at . ,checked-at)
            (entries . ,entries)
            (available-count . ,avail)
            (updated-count . ,updated)
            (message . ,msg))))

(define (process-repo-group scope workspace mode group cached-repos cached-entries now)
        (let* ((key (car group))
               (url (car key))
               (ref (cdr key))
               (entries (cdr group))
               (remote-commit (safe-ls-remote url ref))
               (cached-repo (assoc-ref cached-repos key))
               (cached-commit (and cached-repo (assoc-ref cached-repo 'commit)))
               (repo-record (if remote-commit
                              `(,key (commit . ,remote-commit) (checked-at . ,now))
                              cached-repo))
               (evaluated
                 (map (lambda (entry)
                        (let* ((fid (car entry))
                               (subpath (assoc-ref (assoc-ref entry 'source) 'path))
                               (cached-e (assoc-ref cached-entries fid))
                               (cached-ver (and cached-e (assoc-ref cached-e 'remote-version)))
                               (remote-ver
                                 (cond
                                   ((not remote-commit) #f)
                                   ((and cached-commit
                                         (string=? remote-commit cached-commit)
                                         (string? cached-ver))
                                     cached-ver)
                                   (else
                                     (let ((mf (safe-fetch-manifest url subpath ref)))
                                       (and (list? mf) (assoc-ref mf 'version)))))))
                          (evaluate-entry scope workspace mode entry remote-commit remote-ver now)))
                      entries)))
          (values repo-record evaluated)))

(define* (geis-updater-check-scope! scope
                                    #:key
                                    (workspace (getcwd))
                                    (force? #f)
                                    (now (current-time))
                                    (config #f))
         "Check SCOPE ('user or 'project) for geasa updates, honoring TTL and policy."
         (call-with-values
           (lambda () (scope-update-policy scope workspace config))
           (lambda (mode ttl-hours)
             (let ((cache (geis-updater-read-cache scope workspace)))
               (cond
                 ((eq? mode 'manual)
                   (let ((res (make-result scope mode #t (assoc-ref cache 'last-check)
                                           (or (assoc-ref cache 'entries) '()))))
                     (set-snapshot! scope res)
                     res))
                 ((and (not force?) (geis-updater-cache-fresh? cache ttl-hours now))
                   (let ((res (make-result scope mode #t (assoc-ref cache 'last-check)
                                           (or (assoc-ref cache 'entries) '()))))
                     (set-snapshot! scope res)
                     res))
                 (else
                   (let* ((installed (geis-pkg-list scope workspace))
                          (groups (group-entries-by-repo installed))
                          (cached-repos (or (assoc-ref cache 'repos) '()))
                          (cached-entries (or (assoc-ref cache 'entries) '())))
                     (let loop ((rest groups) (new-repos '()) (new-entries '()))
                               (if (null? rest)
                                 (let* ((ordered-repos (reverse new-repos))
                                        (ordered-entries (reverse new-entries))
                                        (new-cache `((cache-version . 1)
                                                     (last-check . ,now)
                                                     (repos . ,ordered-repos)
                                                     (entries . ,ordered-entries)))
                                        (res (make-result scope mode #f now ordered-entries)))
                                   (geis-updater-write-cache! scope new-cache workspace)
                                   (set-snapshot! scope res)
                                   res)
                                 (call-with-values
                                   (lambda ()
                                     (process-repo-group scope workspace mode (car rest)
                                                         cached-repos cached-entries now))
                                   (lambda (repo-rec eval-list)
                                     (loop (cdr rest)
                                           (if repo-rec (cons repo-rec new-repos) new-repos)
                                           (append (reverse eval-list) new-entries)))))))))))))

(define* (start-background-geasa-update-check! #:key
                                               (scopes '(user project))
                                               (workspace (getcwd))
                                               (force? #f)
                                               (now #f)
                                               (config #f)
                                               (on-event #f)
                                               (on-complete #f))
         "Spawn a non-blocking background thread to check SCOPES for geasa updates."
         (call-with-new-thread
           (lambda ()
             (let ((results
                     (map (lambda (scope)
                            (catch #t
                              (lambda ()
                                (let ((res (geis-updater-check-scope! scope
                                                                      #:workspace workspace
                                                                      #:force? force?
                                                                      #:now (or now (current-time))
                                                                      #:config config)))
                                  (when on-event (on-event res))
                                  res))
                              (lambda _
                                `((ok . #f) (scope . ,scope) (status . offline)))))
                          scopes)))
               (when on-complete
                 (catch #t (lambda () (on-complete results)) (lambda _ #f)))
               results))))
