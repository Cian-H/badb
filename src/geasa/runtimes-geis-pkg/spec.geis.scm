(define-module (test runtimes-geis-pkg-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (badb geis bundle)
               #:use-module (geasa runtimes-geis-pkg plugin))

(define (sh cmd)
        (let ((st (system cmd)))
          (unless (zero? st)
            (error "shell command failed" cmd st))))

(define (write-text path content)
        (call-with-output-file path
                               (lambda (p) (display content p))))

(define (make-demo-bundle! dir version pass?)
        (let ((spec-body
                (if pass?
                  "(define-module (test demo-pkg-spec)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec))
(spec-begin \"demo-pkg\")
(test-group \"ok\" (test-assert \"passes\" #t))
(spec-end)
(spec-exit)\n"
                  "(define-module (test demo-pkg-spec)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec))
(spec-begin \"demo-pkg\")
(test-group \"fail\" (test-assert \"fails\" #f))
(spec-end)
(spec-exit)\n")))
          (write-text (string-append dir "/spec.geis.scm") spec-body)
          (write-text (string-append dir "/plugin.scm")
                      "(define-module (geasa demo-pkg plugin) #:export (demo-hello))
(define (demo-hello) 'hello)\n")
          (write-text (string-append dir "/manifest.scm")
                      (format #f "((bundle-version . 1)
 (feature-id . \"demo-pkg\")
 (version . ~s)
 (spec . \"spec.geis.scm\")
 (plugin (module geasa demo-pkg plugin) (file . \"plugin.scm\")))\n"
                              version))
          (geis-bundle-pin! dir #:bump? #f)))

(spec-begin "runtimes-geis-pkg")

(let* ((root (mkdtemp "/tmp/badb-pkg-spec-XXXXXX"))
       (work-repo (string-append root "/work-repo"))
       (bare-repo (string-append root "/bare-repo.git"))
       (pkg-sub "geasa/demo-pkg")
       (pkg-dir (string-append work-repo "/" pkg-sub))
       (cache-dir (string-append root "/repo-cache"))
       (ws (string-append root "/workspace"))
       (url (string-append "file://" bare-repo)))
  (setenv "BADB_REPO_CACHE_DIR" cache-dir)
  (mkdir work-repo)
  (mkdir (string-append work-repo "/geasa"))
  (mkdir pkg-dir)
  (mkdir ws)

  (sh (format #f "git -C ~s init -b main >/dev/null 2>&1" work-repo))
  (sh (format #f "git -C ~s config user.email test@example.com" work-repo))
  (sh (format #f "git -C ~s config user.name Test" work-repo))
  (make-demo-bundle! pkg-dir "2026.11.18.0" #t)
  (sh (format #f "git -C ~s add . && git -C ~s commit -m v1 >/dev/null 2>&1" work-repo work-repo))
  (sh (format #f "git clone --bare ~s ~s >/dev/null 2>&1" work-repo bare-repo))

  (test-group "ls-remote & manifest inspection"
              (let ((sha (geis-pkg-ls-remote url "main")))
                (test-assert "ls-remote returns 40-char sha"
                             (and (string? sha) (= 40 (string-length sha)))))
              (let ((m (geis-pkg-fetch-manifest! url pkg-sub "main")))
                (test-equal "fetch-manifest reads version"
                            "2026.11.18.0"
                            (assoc-ref m 'version))))

  (test-group "install, failing gate preserves v1, passing v2 & rollback"
              (let ((res (geis-pkg-install! url pkg-sub #:ref "main" #:scope 'project #:workspace ws)))
                (test-assert "v1 install succeeds" (assoc-ref res 'ok))
                (test-equal "v1 version in lockfile"
                            "2026.11.18.0"
                            (assoc-ref (cdr (geis-pkg-lock-entry 'project "demo-pkg" ws)) 'version)))

              ;; Commit broken v2
              (make-demo-bundle! pkg-dir "2026.11.18.1" #f)
              (sh (format #f "git -C ~s add . && git -C ~s commit -m bad-v2 >/dev/null 2>&1" work-repo work-repo))
              (sh (format #f "git -C ~s push ~s main >/dev/null 2>&1" work-repo bare-repo))

              (let ((res (geis-pkg-install! url pkg-sub #:ref "main" #:scope 'project #:workspace ws)))
                (test-assert "broken v2 install rejected" (not (assoc-ref res 'ok)))
                (test-equal "v1 preserved in lockfile after failed v2"
                            "2026.11.18.0"
                            (assoc-ref (cdr (geis-pkg-lock-entry 'project "demo-pkg" ws)) 'version)))

              ;; Commit valid v2
              (make-demo-bundle! pkg-dir "2026.11.18.2" #t)
              (sh (format #f "git -C ~s add . && git -C ~s commit -m good-v2 >/dev/null 2>&1" work-repo work-repo))
              (sh (format #f "git -C ~s push ~s main >/dev/null 2>&1" work-repo bare-repo))

              (let ((res (geis-pkg-install! url pkg-sub #:ref "main" #:scope 'project #:workspace ws)))
                (test-assert "v2 install succeeds" (assoc-ref res 'ok))
                (test-equal "v2 version in lockfile"
                            "2026.11.18.2"
                            (assoc-ref (cdr (geis-pkg-lock-entry 'project "demo-pkg" ws)) 'version)))

              (let ((rb (geis-pkg-rollback! "demo-pkg" #:scope 'project #:workspace ws)))
                (test-assert "rollback succeeds" (assoc-ref rb 'ok))
                (test-equal "rollback restores v1" "2026.11.18.0" (assoc-ref rb 'version))
                (test-assert "rollback sets pinned? #t"
                             (assoc-ref (cdr (geis-pkg-lock-entry 'project "demo-pkg" ws)) 'pinned?))))

  (test-group "dirty check, enable/disable & core protection"
              (test-assert "clean bundle not locally modified"
                           (not (geis-pkg-locally-modified? 'project "demo-pkg" ws)))
              (write-text (string-append ws "/.badb/geasa/demo-pkg/extra.txt") "dirty\n")
              (test-assert "modified bundle detected"
                           (geis-pkg-locally-modified? 'project "demo-pkg" ws))

              (let ((dis (geis-pkg-disable! "demo-pkg" #:scope 'project #:workspace ws)))
                (test-assert "disable demo-pkg succeeds" (assoc-ref dis 'ok))
                (test-assert "enabled? is #f" (not (geis-pkg-enabled? 'project "demo-pkg" ws))))
              (let ((en (geis-pkg-enable! "demo-pkg" #:scope 'project #:workspace ws)))
                (test-assert "enable demo-pkg succeeds" (assoc-ref en 'ok))
                (test-assert "enabled? is #t" (geis-pkg-enabled? 'project "demo-pkg" ws)))
              (let ((core-dis (geis-pkg-disable! "runtimes-geis-pkg" #:scope 'project #:workspace ws)))
                (test-assert "core geis cannot be disabled" (not (assoc-ref core-dis 'ok)))))

  (sh (format #f "rm -rf ~s" root)))

(spec-end)
(spec-exit)
