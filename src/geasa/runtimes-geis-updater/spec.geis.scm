(define-module (test runtimes-geis-updater-spec)
               #:use-module (srfi srfi-64)
               #:use-module (ice-9 threads)
               #:use-module (badb spec)
               #:use-module (badb geis bundle)
               #:use-module (geasa runtimes-geis-pkg plugin)
               #:use-module (geasa runtimes-geis-updater plugin))

(define (sh cmd)
        (let ((st (system cmd)))
          (unless (zero? st)
            (error "shell command failed" cmd st))))

(define (write-text path content)
        (call-with-output-file path
                               (lambda (p) (display content p))))

(define (mkdir-p dir)
        (unless (or (string-null? dir) (string=? dir "/") (file-exists? dir))
          (mkdir-p (dirname dir))
          (mkdir dir)))

(define (make-demo-bundle! dir fid version)
        (mkdir-p dir)
        (write-text (string-append dir "/spec.geis.scm")
                    (format #f "(define-module (test ~a-spec)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec))
(spec-begin ~s)
(test-group \"ok\" (test-assert \"passes\" #t))
(spec-end)
(spec-exit)\n" fid fid))
        (write-text (string-append dir "/plugin.scm")
                    (format #f "(define-module (geasa ~a plugin) #:export (demo-ver))
(define (demo-ver) ~s)\n" fid version))
        (write-text (string-append dir "/manifest.scm")
                    (format #f "((bundle-version . 1)
 (feature-id . ~s)
 (version . ~s)
 (spec . \"spec.geis.scm\")
 (plugin (module geasa ~a plugin) (file . \"plugin.scm\")))\n"
                            fid version fid))
        (geis-bundle-pin! dir #:bump? #f))

(define (git-commit-all! repo-dir msg)
        (sh (format #f "git -C ~s add -A && git -C ~s commit -q -m ~s"
                    repo-dir repo-dir msg)))

(spec-begin "runtimes-geis-updater")

(let* ((tmp (mkdtemp "/tmp/badb-updater-spec-XXXXXX"))
       (repo (string-append tmp "/remote-repo"))
       (ws (string-append tmp "/workspace"))
       (cache-dir (string-append tmp "/repo-cache"))
       (url (string-append "file://" repo)))
  (setenv "BADB_REPO_CACHE_DIR" cache-dir)
  (mkdir-p repo)
  (mkdir-p ws)
  (sh (format #f "git -C ~s init -q -b main" repo))
  (sh (format #f "git -C ~s config user.email test@example.com" repo))
  (sh (format #f "git -C ~s config user.name Test" repo))
  (make-demo-bundle! (string-append repo "/geasa/demo-a") "demo-a" "2026.11.18.0")
  (make-demo-bundle! (string-append repo "/geasa/demo-b") "demo-b" "2026.11.18.0")
  (git-commit-all! repo "initial")

  (test-group "install initial bundles"
              (let ((r1 (geis-pkg-install! url "geasa/demo-a" #:ref "main" #:scope 'project #:workspace ws))
                    (r2 (geis-pkg-install! url "geasa/demo-b" #:ref "main" #:scope 'project #:workspace ws)))
                (test-assert "demo-a installed" (assoc-ref r1 'ok))
                (test-assert "demo-b installed" (assoc-ref r2 'ok))))

  (test-group "TTL caching and up-to-date check"
              (let* ((t0 1795000000)
                     (res1 (geis-updater-check-scope! 'project #:workspace ws #:now t0 #:config '()))
                     (res2 (geis-updater-check-scope! 'project #:workspace ws #:now (+ t0 3600) #:config '()))
                     (res3 (geis-updater-check-scope! 'project #:workspace ws #:now (+ t0 (* 25 3600)) #:config '())))
                (test-assert "first check hits remote" (not (assoc-ref res1 'from-cache?)))
                (test-equal "zero available" 0 (assoc-ref res1 'available-count))
                (test-assert "second check within TTL uses cache" (assoc-ref res2 'from-cache?))
                (test-assert "third check after TTL hits remote" (not (assoc-ref res3 'from-cache?)))))

  ;; Bump demo-a and demo-b in remote repo
  (make-demo-bundle! (string-append repo "/geasa/demo-a") "demo-a" "2026.11.18.1")
  (make-demo-bundle! (string-append repo "/geasa/demo-b") "demo-b" "2026.11.18.1")
  (git-commit-all! repo "bump to 2026.11.18.1")

  (test-group "manual mode skips remote"
              (let* ((cfg '((project-geasa-update (mode . manual) (ttl-hours . 24))))
                     (res (geis-updater-check-scope! 'project #:workspace ws #:force? #t #:now 1795100000 #:config cfg)))
                (test-assert "manual returns from-cache? #t" (assoc-ref res 'from-cache?))
                (test-equal "manual mode recorded" 'manual (assoc-ref res 'mode))))

  (test-group "notify mode reports available without mutating"
              (let* ((cfg '((project-geasa-update (mode . notify) (ttl-hours . 24))))
                     (res (geis-updater-check-scope! 'project #:workspace ws #:force? #t #:now 1795100100 #:config cfg))
                     (entry-a (geis-pkg-lock-entry 'project "demo-a" ws)))
                (test-equal "2 updates available" 2 (assoc-ref res 'available-count))
                (test-equal "notify message" "2 geasa update(s) available — /geasa" (assoc-ref res 'message))
                (test-equal "demo-a still at 2026.11.18.0" "2026.11.18.0" (assoc-ref entry-a 'version))))

  (test-group "pin and local-mod guards + auto update + rollback"
              ;; Pin demo-b and modify demo-a locally? No, pin demo-b, keep demo-a clean first.
              (geis-pkg-pin! "demo-b" #:scope 'project #:workspace ws #:pinned? #t)
              (let* ((cfg '((project-geasa-update (mode . auto) (ttl-hours . 24))))
                     (res (geis-updater-check-scope! 'project #:workspace ws #:force? #t #:now 1795100200 #:config cfg))
                     (entries (assoc-ref res 'entries))
                     (st-a (assoc-ref (assoc-ref entries "demo-a") 'status))
                     (st-b (assoc-ref (assoc-ref entries "demo-b") 'status))
                     (lock-a (geis-pkg-lock-entry 'project "demo-a" ws)))
                (test-equal "1 updated" 1 (assoc-ref res 'updated-count))
                (test-equal "auto message" "Updated 1 geasa — takes effect next launch" (assoc-ref res 'message))
                (test-equal "demo-a updated" 'updated st-a)
                (test-equal "demo-b pinned" 'pinned st-b)
                (test-equal "demo-a version bumped in lockfile" "2026.11.18.1" (assoc-ref lock-a 'version))
                (test-assert "rollback preserved" (list? (assoc-ref lock-a 'rollback))))

              ;; Bump demo-a again, then dirty demo-a locally
              (make-demo-bundle! (string-append repo "/geasa/demo-a") "demo-a" "2026.11.18.2")
              (git-commit-all! repo "bump demo-a to 2026.11.18.2")
              (write-text (string-append (geis-pkg-store-dir 'project ws) "/demo-a/plugin.scm")
                          "(define-module (geasa demo-a plugin))\n;; local edit\n")
              (let* ((cfg '((project-geasa-update (mode . auto) (ttl-hours . 24))))
                     (res (geis-updater-check-scope! 'project #:workspace ws #:force? #t #:now 1795100300 #:config cfg))
                     (entries (assoc-ref res 'entries))
                     (st-a (assoc-ref (assoc-ref entries "demo-a") 'status)))
                (test-equal "dirty demo-a skipped" 'local-mod st-a)))

  (test-group "background check & silent offline fallback"
              ;; Point lockfile source to non-existent repo to simulate offline/unreachable
              (let* ((lock (geis-pkg-read-lockfile 'project ws))
                     (geasa (assoc-ref lock 'geasa))
                     (bad-geasa
                       (map (lambda (e)
                              (if (string=? (car e) "demo-a")
                                (cons "demo-a"
                                      (assoc-set! (cdr e) 'source
                                                  '((type . git)
                                                    (url . "file:///nonexistent/badb-offline.git")
                                                    (ref . "main")
                                                    (path . "geasa/demo-a"))))
                                e))
                            geasa))
                     (new-lock (assoc-set! lock 'geasa bad-geasa)))
                (geis-pkg-write-lockfile! 'project new-lock ws)
                (let* ((th (start-background-geasa-update-check!
                             #:scopes '(project)
                             #:workspace ws
                             #:force? #t
                             #:now 1795100400
                             #:config '()))
                       (_ (join-thread th))
                       (snap (geis-updater-snapshot 'project))
                       (entries (assoc-ref snap 'entries))
                       (st-a (assoc-ref (assoc-ref entries "demo-a") 'status)))
                  (test-assert "snapshot populated" (list? snap))
                  (test-equal "offline recorded silently" 'offline st-a))))

  (sh (format #f "rm -rf ~s" tmp)))

(spec-end)
(spec-exit)
