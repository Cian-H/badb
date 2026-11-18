(define-module (geasa runtimes-geis-pkg plugin)
               #:use-module (badb geis bundle)
               #:use-module (badb kernel digest)
               #:use-module (ice-9 ftw)
               #:use-module (ice-9 match)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 rdelim)
               #:use-module (srfi srfi-1)
               #:export (%core-protected-geasa
                          geis-pkg-store-dir
                          geis-pkg-lockfile-path
                          geis-pkg-rollback-root
                          geis-pkg-read-lockfile
                          geis-pkg-write-lockfile!
                          geis-pkg-lock-entry
                          geis-pkg-repo-cache-dir
                          geis-pkg-ls-remote
                          geis-pkg-fetch-manifest!
                          geis-pkg-install!
                          geis-pkg-remove!
                          geis-pkg-rollback!
                          geis-pkg-locally-modified?
                          geis-pkg-pin!
                          geis-pkg-enabled?
                          geis-pkg-enable!
                          geis-pkg-disable!
                          geis-pkg-list
                          geis-pkg-sync!))

(define %core-protected-geasa
        '("foundation-util"
          "foundation-config"
          "foundation-log"
          "runtimes-geis-loader"
          "runtimes-geis-pkg"
          "surfaces-theme"
          "surfaces-widgets"
          "surfaces-overlay"
          "surfaces-tui"
          "surfaces-geis-manager"))

(define (ensure-dir-p dir)
        (unless (or (string-null? dir) (string=? dir "/"))
          (unless (file-exists? dir)
            (ensure-dir-p (dirname dir))
            (catch 'system-error
              (lambda () (mkdir dir))
              (lambda _ #f)))))

(define (rm-rf path)
        (when (file-exists? path)
          (if (eq? 'directory (stat:type (lstat path)))
            (begin
              (catch #t
                (lambda ()
                  (chmod path #o755))
                (lambda _
                  #f))
              (for-each
                (lambda (entry)
                  (unless
                      (or (string=? entry ".") (string=? entry ".."))
                    (rm-rf (string-append path "/" entry))))
                (scandir path))
              (rmdir path))
            (begin
              (catch #t
                (lambda ()
                  (chmod path #o644))
                (lambda _
                  #f))
              (delete-file path)))))

(define (copy-tree src dst)
        (ensure-dir-p dst)
        (for-each (lambda (entry)
                    (unless (or (string=? entry ".") (string=? entry ".."))
                      (let ((s (string-append src "/" entry))
                            (d (string-append dst "/" entry)))
                        (if (eq? 'directory (stat:type (lstat s)))
                          (copy-tree s d)
                          (copy-file s d)))))
                  (scandir src)))

(define (run-cmd . args)
        (let* ((port (apply open-pipe* OPEN_READ args))
               (out (read-string port))
               (st (close-pipe port)))
          (cons (status:exit-val st) (or out ""))))

(define* (geis-pkg-store-dir scope #:optional (workspace (getcwd)))
         (if (eq? scope 'user)
           (user-geis-store-dir)
           (string-append (or workspace (getcwd)) "/.badb/geasa")))

(define* (geis-pkg-lockfile-path scope #:optional (workspace (getcwd)))
         (string-append (dirname (geis-pkg-store-dir scope workspace))
                        "/geasa-lock.scm"))

(define* (geis-pkg-rollback-root scope #:optional (workspace (getcwd)))
         (string-append (dirname (geis-pkg-store-dir scope workspace))
                        "/rollback"))

(define (default-lockfile)
        '((lock-version . 1)
          (last-check . #f)
          (repos . ())
          (geasa . ())))

(define*
  (geis-pkg-read-lockfile scope #:optional (workspace (getcwd)))
  (let*
      ((path (geis-pkg-lockfile-path scope workspace))
       (store-dir (geis-pkg-store-dir scope workspace))
       (base-lock
         (if (file-exists? path)
           (catch #t
             (lambda ()
               (let ((data (call-with-input-file path read)))
                 (if (and (list? data) (assoc 'lock-version data))
                   data
                   (default-lockfile))))
             (lambda _
               (default-lockfile)))
           (default-lockfile)))
       (existing (or (assoc-ref base-lock 'geasa) '()))
       (discovered
         (if
             (and (file-exists? store-dir) (eq? 'directory (stat:type (lstat store-dir))))
           (filter-map
             (lambda (name)
               (and
                 (not (or (string=? name ".") (string=? name "..")))
                 (not (assoc name existing))
                 (let
                     ((mpath (string-append store-dir "/" name "/manifest.scm")))
                   (and (file-exists? mpath)
                        (catch #t
                          (lambda ()
                            (let ((m (call-with-input-file mpath read)))
                              (and (list? m)
                                   (cons (or (assoc-ref m 'feature-id) name)
                                         `((source . ,(assoc-ref m 'source))
                                           (version . ,(or (assoc-ref m 'version) "0.0.0.0"))
                                           (commit . #f)
                                           (bundle-sha256 . #f)
                                           (enabled? . #t)
                                           (pinned? . #f))))))
                          (lambda _
                            #f))))))
             (or (scandir store-dir) '()))
           '())))
    (if (null? discovered)
      base-lock
      (cons (cons 'geasa (append existing discovered))
            (alist-delete 'geasa base-lock eq?)))))

(define* (geis-pkg-write-lockfile! scope lock-data #:optional (workspace (getcwd)))
         (let* ((path (geis-pkg-lockfile-path scope workspace))
                (dir (dirname path))
                (tmp (string-append path ".tmp." (number->string (getpid)))))
           (ensure-dir-p dir)
           (call-with-output-file tmp
                                  (lambda (p)
                                    (write lock-data p)
                                    (newline p)))
           (rename-file tmp path)
           path))

(define* (geis-pkg-lock-entry scope fid #:optional (workspace (getcwd)))
         (let* ((lock (geis-pkg-read-lockfile scope workspace))
                (geasa (or (assoc-ref lock 'geasa) '())))
           (assoc fid geasa)))

(define (alist-set alist key val)
        (cons (cons key val)
              (alist-delete key alist)))

(define (geis-pkg-repo-cache-dir url)
        (let ((base (or (getenv "BADB_REPO_CACHE_DIR")
                        (let ((xdg (getenv "XDG_CACHE_HOME")))
                          (if (and xdg (not (string-null? xdg)))
                            (string-append xdg "/badb/repos")
                            (string-append (or (getenv "HOME") "/tmp")
                                           "/.cache/badb/repos"))))))
          (string-append base "/" (sha256-hex url))))

(define* (geis-pkg-ls-remote url #:optional (ref "main"))
         (let* ((res (run-cmd "git" "ls-remote" url ref))
                (code (car res))
                (out (string-trim-both (cdr res))))
           (if (and (zero? code) (not (string-null? out)))
             (let* ((line (car (string-split out #\newline)))
                    (parts (string-split line #\tab)))
               (and (pair? parts) (car parts)))
             #f)))

(define (ensure-repo-cached! url ref)
        (let ((cache-dir (geis-pkg-repo-cache-dir url)))
          (ensure-dir-p (dirname cache-dir))
          (unless (file-exists? cache-dir)
            (let ((res (run-cmd "git" "clone" "--bare" "--filter=blob:none"
                                "--depth=1" "--branch" ref url cache-dir)))
              (unless (zero? (car res))
                (rm-rf cache-dir)
                (let ((fallback (run-cmd "git" "clone" "--bare" "--depth=1"
                                         "--branch" ref url cache-dir)))
                  (unless (zero? (car fallback))
                    (error "git clone failed" url ref (cdr fallback)))))))
          (let ((fetch (run-cmd "git" "-C" cache-dir "fetch" "--depth=1" "origin" ref)))
            (unless (zero? (car fetch))
              (error "git fetch failed" url ref (cdr fetch))))
          cache-dir))

(define* (geis-pkg-fetch-manifest! url subpath #:optional (ref "main"))
         (let* ((cache-dir (ensure-repo-cached! url ref))
                (spec (string-append "FETCH_HEAD:" subpath "/manifest.scm"))
                (res (run-cmd "git" "-C" cache-dir "show" spec)))
           (if (zero? (car res))
             (catch #t
               (lambda () (call-with-input-string (cdr res) read))
               (lambda _ #f))
             #f)))

(define (extract-subpath-to-staging! cache-dir subpath staging-dir)
        (ensure-dir-p staging-dir)
        (let* ((cmd (string-append "git -C " (prin1-to-string cache-dir)
                                   " archive FETCH_HEAD " (prin1-to-string subpath)
                                   " | tar -x -C " (prin1-to-string staging-dir)))
               (st (system cmd)))
          (unless (zero? st)
            (error "git archive extract failed" subpath))
          (string-append staging-dir "/" subpath)))

(define (prin1-to-string x)
        (call-with-output-string (lambda (p) (write x p))))

(define (prune-old-rollbacks! rb-dir keep-name)
        (when (file-exists? rb-dir)
          (for-each (lambda (entry)
                      (unless (or (string=? entry ".")
                                  (string=? entry "..")
                                  (string=? entry keep-name))
                        (rm-rf (string-append rb-dir "/" entry))))
                    (scandir rb-dir))))

(define (backup-existing-bundle! store-dir rb-root fid lock-entry)
        (let ((target (string-append store-dir "/" fid)))
          (if (file-exists? target)
            (let* ((old-hash (geis-bundle-hash target))
                   (rb-dir (string-append rb-root "/" fid))
                   (backup-dir (string-append rb-dir "/" old-hash))
                   (old-ver (and lock-entry (assoc-ref (cdr lock-entry) 'version)))
                   (old-commit (and lock-entry (assoc-ref (cdr lock-entry) 'commit))))
              (rm-rf backup-dir)
              (copy-tree target backup-dir)
              (prune-old-rollbacks! rb-dir old-hash)
              `((version . ,old-ver)
                (commit . ,old-commit)
                (bundle-sha256 . ,old-hash)
                (backup-dir . ,(string-append "rollback/" fid "/" old-hash))))
            #f)))

(define (atomic-swap-bundle! src-dir dst-dir)
        (let* ((parent (dirname dst-dir))
               (tmp (string-append dst-dir ".swap." (number->string (getpid))))
               (old (string-append dst-dir ".old." (number->string (getpid)))))
          (ensure-dir-p parent)
          (rm-rf tmp)
          (rm-rf old)
          (copy-tree src-dir tmp)
          (when (file-exists? dst-dir)
            (rename-file dst-dir old))
          (rename-file tmp dst-dir)
          (rm-rf old)))

(define (verify-bundle-via-gate bundle-dir)
        (let ((old-path (getenv "BADB_GEIS_PATH"))
              (parent (dirname bundle-dir)))
          (dynamic-wind
            (lambda ()
              (setenv "BADB_GEIS_PATH"
                      (if (and old-path (not (string-null? old-path)))
                        (string-append parent ":" old-path)
                        parent)))
            (lambda ()
              (geis-bundle-install bundle-dir #:compile? #f))
            (lambda ()
              (if old-path
                (setenv "BADB_GEIS_PATH" old-path)
                (unsetenv "BADB_GEIS_PATH"))))))

(define* (geis-pkg-install! url subpath
                            #:key (ref "main")
                            (scope 'user)
                            (workspace (getcwd))
                            (pin? #f)
                            (enabled? #t))
         (let* ((commit (geis-pkg-ls-remote url ref))
                (cache-dir (ensure-repo-cached! url ref))
                (staging (mkdtemp "/tmp/badb-pkg-stage-XXXXXX")))
           (catch #t
             (lambda ()
               (let* ((staged-bundle (extract-subpath-to-staging! cache-dir subpath staging))
                      (errors (geis-manifest-valid? staged-bundle)))
                 (if (not (null? errors))
                   (begin
                     (rm-rf staging)
                     `((ok . #f) (reason . "invalid manifest") (errors . ,errors)))
                   (let* ((manifest (call-with-input-file
                                      (string-append staged-bundle "/manifest.scm")
                                      read))
                          (fid (assoc-ref manifest 'feature-id))
                          (ver (assoc-ref manifest 'version))
                          (report (verify-bundle-via-gate staged-bundle))
                          (status (assoc-ref report 'verify-verdict)))
                     (if (not (equal? status "pass"))
                       (begin
                         (rm-rf staging)
                         `((ok . #f) (reason . "contract verification failed") (report . ,report)))
                       (let* ((store-dir (geis-pkg-store-dir scope workspace))
                              (rb-root (geis-pkg-rollback-root scope workspace))
                              (lock (geis-pkg-read-lockfile scope workspace))
                              (geasa (or (assoc-ref lock 'geasa) '()))
                              (prev-entry (assoc fid geasa))
                              (rb-rec (backup-existing-bundle! store-dir rb-root fid prev-entry))
                              (new-hash (geis-bundle-hash staged-bundle))
                              (now (current-time))
                              (new-entry
                                `((source (type . git)
                                          (url . ,url)
                                          (ref . ,ref)
                                          (path . ,subpath))
                                  (version . ,ver)
                                  (commit . ,commit)
                                  (bundle-sha256 . ,new-hash)
                                  (enabled? . ,enabled?)
                                  (pinned? . ,pin?)
                                  (installed-at . ,now)
                                  (rollback . ,rb-rec)))
                              (repos (or (assoc-ref lock 'repos) '()))
                              (new-repos (alist-set repos (cons url ref)
                                                    `((commit . ,commit)
                                                      (checked-at . ,now))))
                              (new-geasa (alist-set geasa fid new-entry))
                              (new-lock (alist-set (alist-set lock 'repos new-repos)
                                                   'geasa new-geasa)))
                         (atomic-swap-bundle! staged-bundle (string-append store-dir "/" fid))
                         (geis-pkg-write-lockfile! scope new-lock workspace)
                         (rm-rf staging)
                         `((ok . #t)
                           (feature-id . ,fid)
                           (version . ,ver)
                           (commit . ,commit)
                           (bundle-sha256 . ,new-hash))))))))
             (lambda (key . args)
               (rm-rf staging)
               `((ok . #f) (reason . ,(format #f "~a: ~a" key args)))))))

(define (enabled-dependents-of scope fid workspace)
        (let* ((store-dir (geis-pkg-store-dir scope workspace))
               (lock (geis-pkg-read-lockfile scope workspace))
               (geasa (or (assoc-ref lock 'geasa) '())))
          (filter-map
            (lambda (entry)
              (let* ((other-fid (car entry))
                     (props (cdr entry))
                     (enabled? (assoc-ref props 'enabled?))
                     (mpath (string-append store-dir "/" other-fid "/manifest.scm")))
                (and enabled?
                     (not (string=? other-fid fid))
                     (file-exists? mpath)
                     (let* ((m (call-with-input-file mpath read))
                            (deps (or (assoc-ref m 'depends) '()))
                            (geis-deps (or (assoc-ref deps 'geis) '())))
                       (and (any (lambda (d)
                                   (equal? (assoc-ref d 'feature-id) fid))
                                 geis-deps)
                            other-fid)))))
            geasa)))

(define* (geis-pkg-remove! fid #:key (scope 'user) (workspace (getcwd)))
         (cond
           ((member fid %core-protected-geasa)
             `((ok . #f) (reason . "protected: core geis")))
           ((pair? (enabled-dependents-of scope fid workspace))
             (let ((deps (enabled-dependents-of scope fid workspace)))
               `((ok . #f) (reason . ,(string-append "protected: required by " (car deps))))))
           (else
             (let* ((store-dir (geis-pkg-store-dir scope workspace))
                    (target (string-append store-dir "/" fid))
                    (lock (geis-pkg-read-lockfile scope workspace))
                    (geasa (or (assoc-ref lock 'geasa) '()))
                    (new-geasa (alist-delete fid geasa)))
               (rm-rf target)
               (geis-pkg-write-lockfile! scope (alist-set lock 'geasa new-geasa) workspace)
               `((ok . #t) (feature-id . ,fid))))))

(define* (geis-pkg-rollback! fid #:key (scope 'user) (workspace (getcwd)))
         (let* ((lock (geis-pkg-read-lockfile scope workspace))
                (geasa (or (assoc-ref lock 'geasa) '()))
                (entry (assoc fid geasa))
                (rb (and entry (assoc-ref (cdr entry) 'rollback))))
           (if (not rb)
             `((ok . #f) (reason . "no rollback snapshot available"))
             (let* ((store-dir (geis-pkg-store-dir scope workspace))
                    (store-parent (dirname store-dir))
                    (backup-rel (assoc-ref rb 'backup-dir))
                    (backup-abs (string-append store-parent "/" backup-rel)))
               (if (not (file-exists? backup-abs))
                 `((ok . #f) (reason . "rollback directory missing"))
                 (let* ((staging (mkdtemp "/tmp/badb-pkg-rb-XXXXXX"))
                        (staged-bundle (string-append staging "/" fid)))
                   (copy-tree backup-abs staged-bundle)
                   (let* ((report (verify-bundle-via-gate staged-bundle))
                          (status (assoc-ref report 'verify-verdict)))
                     (if (not (equal? status "pass"))
                       (begin
                         (rm-rf staging)
                         `((ok . #f) (reason . "rollback verification failed")))
                       (let* ((target (string-append store-dir "/" fid))
                              (props (cdr entry))
                              (new-props
                                (alist-set
                                  (alist-set
                                    (alist-set
                                      (alist-set props 'version (assoc-ref rb 'version))
                                      'commit (assoc-ref rb 'commit))
                                    'bundle-sha256 (assoc-ref rb 'bundle-sha256))
                                  'pinned? #t))
                              (new-geasa (alist-set geasa fid new-props)))
                         (atomic-swap-bundle! staged-bundle target)
                         (rm-rf staging)
                         (geis-pkg-write-lockfile! scope (alist-set lock 'geasa new-geasa) workspace)
                         `((ok . #t)
                           (feature-id . ,fid)
                           (version . ,(assoc-ref rb 'version))
                           (pinned? . #t)))))))))))

(define* (geis-pkg-locally-modified? scope fid #:optional (workspace (getcwd)))
         (let* ((entry (geis-pkg-lock-entry scope fid workspace))
                (recorded (and entry (assoc-ref (cdr entry) 'bundle-sha256)))
                (target (string-append (geis-pkg-store-dir scope workspace) "/" fid)))
           (if (and recorded (file-exists? target))
             (not (string=? recorded (geis-bundle-hash target)))
             #f)))

(define* (geis-pkg-pin! fid #:key (scope 'user) (workspace (getcwd)) (pinned? #t))
         (let* ((lock (geis-pkg-read-lockfile scope workspace))
                (geasa (or (assoc-ref lock 'geasa) '()))
                (entry (assoc fid geasa)))
           (if (not entry)
             `((ok . #f) (reason . "geis not found in lockfile"))
             (let* ((new-props (alist-set (cdr entry) 'pinned? pinned?))
                    (new-geasa (alist-set geasa fid new-props)))
               (geis-pkg-write-lockfile! scope (alist-set lock 'geasa new-geasa) workspace)
               `((ok . #t) (feature-id . ,fid) (pinned? . ,pinned?))))))

(define* (geis-pkg-enabled? scope fid #:optional (workspace (getcwd)))
         (let ((entry (geis-pkg-lock-entry scope fid workspace)))
           (if entry
             (not (eq? #f (assoc-ref (cdr entry) 'enabled?)))
             #t)))

(define* (geis-pkg-enable! fid #:key (scope 'user) (workspace (getcwd)))
         (let* ((lock (geis-pkg-read-lockfile scope workspace))
                (geasa (or (assoc-ref lock 'geasa) '()))
                (entry (assoc fid geasa)))
           (if (not entry)
             `((ok . #f) (reason . "geis not found in lockfile"))
             (let* ((new-props (alist-set (cdr entry) 'enabled? #t))
                    (new-geasa (alist-set geasa fid new-props)))
               (geis-pkg-write-lockfile! scope (alist-set lock 'geasa new-geasa) workspace)
               `((ok . #t) (feature-id . ,fid) (enabled? . #t))))))

(define* (geis-pkg-disable! fid #:key (scope 'user) (workspace (getcwd)))
         (cond
           ((member fid %core-protected-geasa)
             `((ok . #f) (reason . "protected: core geis")))
           ((pair? (enabled-dependents-of scope fid workspace))
             (let ((deps (enabled-dependents-of scope fid workspace)))
               `((ok . #f) (reason . ,(string-append "protected: required by " (car deps))))))
           (else
             (let* ((lock (geis-pkg-read-lockfile scope workspace))
                    (geasa (or (assoc-ref lock 'geasa) '()))
                    (entry (assoc fid geasa)))
               (if (not entry)
                 `((ok . #f) (reason . "geis not found in lockfile"))
                 (let* ((new-props (alist-set (cdr entry) 'enabled? #f))
                        (new-geasa (alist-set geasa fid new-props)))
                   (geis-pkg-write-lockfile! scope (alist-set lock 'geasa new-geasa) workspace)
                   `((ok . #t) (feature-id . ,fid) (enabled? . #f))))))))

(define* (geis-pkg-list scope #:optional (workspace (getcwd)))
         (let ((lock (geis-pkg-read-lockfile scope workspace)))
           (or (assoc-ref lock 'geasa) '())))

(define (%find-lock-entry-by-source geasa url path)
        (find (lambda (entry)
                (let ((src (assoc-ref (cdr entry) 'source)))
                  (and (list? src)
                       (equal? (assoc-ref src 'url) url)
                       (equal? (assoc-ref src 'path) path))))
              geasa))

(define* (geis-pkg-sync! declared-geasa #:key (scope 'user) (workspace (getcwd)))
         "Reconcile DECLARED-GEASA entries against the SCOPE lockfile."
         (let ((installed '())
               (unchanged '())
               (failed '()))
           (for-each
             (lambda (decl)
               (let* ((url (assoc-ref decl 'url))
                      (path (assoc-ref decl 'path))
                      (ref (or (assoc-ref decl 'ref) "main"))
                      (pin? (and (assoc-ref decl 'pin) #t))
                      (enabled? (if (assoc 'enabled decl) (and (assoc-ref decl 'enabled) #t) #t))
                      (lock (geis-pkg-read-lockfile scope workspace))
                      (geasa (or (assoc-ref lock 'geasa) '()))
                      (existing (%find-lock-entry-by-source geasa url path)))
                 (if existing
                   (let* ((fid (car existing))
                          (props (cdr existing))
                          (cur-pin? (and (assoc-ref props 'pinned?) #t))
                          (cur-en? (if (assoc 'enabled? props) (and (assoc-ref props 'enabled?) #t) #t)))
                     (when (not (eq? cur-pin? pin?))
                       (geis-pkg-pin! fid #:scope scope #:workspace workspace #:pinned? pin?))
                     (when (not (eq? cur-en? enabled?))
                       (if enabled?
                         (geis-pkg-enable! fid #:scope scope #:workspace workspace)
                         (geis-pkg-disable! fid #:scope scope #:workspace workspace)))
                     (set! unchanged (cons fid unchanged)))
                   (let ((res (geis-pkg-install! url path
                                                 #:ref ref
                                                 #:scope scope
                                                 #:workspace workspace
                                                 #:pin? pin?
                                                 #:enabled? enabled?)))
                     (if (assoc-ref res 'ok)
                       (set! installed (cons (assoc-ref res 'feature-id) installed))
                       (set! failed (cons `((url . ,url) (path . ,path)
                                            (reason . ,(assoc-ref res 'reason)))
                                          failed)))))))
             declared-geasa)
           `((ok . ,(null? failed))
             (installed . ,(reverse installed))
             (unchanged . ,(reverse unchanged))
             (failed . ,(reverse failed)))))
