;;; (geasa runtimes-geis-loader spec.geis) — Contract verification for Geis Loader

(define-module (test test-geis-loader)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (badb kernel registry)
               #:use-module (badb geis bundle)
               #:use-module (badb geis capabilities)
               #:use-module (geasa runtimes-geis-loader plugin)
               #:use-module (ice-9 ftw)
               #:use-module (ice-9 textual-ports)
               #:use-module (srfi srfi-1))

(spec-begin "runtimes-geis-loader")

(define (write-file! path content)
        (let ((dir (dirname path)))
          (unless (file-exists? dir)
            (mkdir-p dir)))
        (call-with-output-file path
                               (lambda (p) (display content p))))

(define (mkdir-p path)
        (unless (or (string-null? path) (string=? path "/") (file-exists? path))
          (mkdir-p (dirname path))
          (catch #t
            (lambda () (mkdir path))
            (lambda _ #f))))

(define (%direct-test-runner bundle-dir module-name plugin-file tool-name args grants _proc)
        (save-module-excursion
          (lambda ()
            (primitive-load (string-append bundle-dir "/" plugin-file))))
        (parameterize ((current-geis-capabilities grants))
                      (apply (module-ref (resolve-module module-name)
                                         (string->symbol tool-name))
                             args)))

(test-group "geis-loader-mount-managed"
            (let* ((tmp-dir (mkdtemp "/tmp/badb-geis-loader-test-XXXXXX"))
                   (geasa-dir (string-append tmp-dir "/.badb/geasa"))
                   (bundle-dir (string-append geasa-dir "/math-ops"))
                   (reg (make-registry)))
              (mkdir-p bundle-dir)
              ;; Create plugin.scm
              (write-file! (string-append bundle-dir "/plugin.scm")
                           "(define-module (project math-ops) #:export (cube-num sq-num))\n(define (cube-num x) (* x x x))\n(define (sq-num x) (* x x))\n")
              ;; Create spec.geis.scm
              (write-file! (string-append bundle-dir "/spec.geis.scm")
                           "(spec-begin \"math-ops\")(test-assert #t)(spec-end)\n")
              ;; Create manifest.scm
              (let* ((pins (compute-geis-sha256-pins bundle-dir))
                     (manifest `((bundle-version . 1)
                                 (feature-id . "math-ops")
                                 (version . "1.0.0")
                                 (spec . "spec.geis.scm")
                                 (plugin . ((module . (project math-ops))
                                            (file . "plugin.scm")))
                                 (tools . ("cube-num"))
                                 (sha256 . ,pins))))
                (call-with-output-file (string-append bundle-dir "/manifest.scm")
                                       (lambda (p) (write manifest p))))

              (let ((mounted (discover-and-mount-project-geasa!
                               reg #:workspace tmp-dir
                               #:sandbox-runner %direct-test-runner)))
                (test-assert "math-ops bundle mounted" (member "math-ops" mounted))
                ;; Check cube-num is attached
                (let ((fn (registry-lookup reg "cube-num")))
                  (test-assert "cube-num tool is registered" (procedure? fn))
                  (test-equal "cube-num computes cube" 27 (fn 3)))
                ;; Check sq-num was filtered out because tools only specified cube-num
                (test-assert "sq-num was filtered by tools manifest list"
                             (not (registry-lookup reg "sq-num"))))))

(test-group "geis-loader-fallback-synthesis"
            (let* ((tmp-dir (mkdtemp "/tmp/badb-geis-loader-fallback-XXXXXX"))
                   (geasa-dir (string-append tmp-dir "/.badb/geasa"))
                   (bundle-dir (string-append geasa-dir "/str-ops"))
                   (reg (make-registry)))
              (mkdir-p bundle-dir)
              ;; Create only plugin.scm (no manifest, no spec)
              (write-file! (string-append bundle-dir "/plugin.scm")
                           "(define-module (project str-ops) #:export (shout))\n(define (shout s) (string-append s \"!\"))\n")

              (let ((mounted (discover-and-mount-project-geasa!
                               reg #:workspace tmp-dir
                               #:sandbox-runner %direct-test-runner)))
                (test-assert "fallback plugin without declared tools is not mounted"
                             (not (member "str-ops" mounted)))
                ;; Verify spec and manifest were created
                (test-assert "spec.geis.scm was created"
                             (file-exists? (string-append bundle-dir "/spec.geis.scm")))
                (test-assert "manifest.scm was created"
                             (file-exists? (string-append bundle-dir "/manifest.scm")))
                (test-assert "undeclared fallback exports are not registered"
                             (not (registry-lookup reg "shout"))))))

(test-group "geis-loader-capability-broker-policy"
            (let* ((tmp-dir (mkdtemp "/tmp/badb-geis-loader-capability-XXXXXX"))
                   (bundle-dir (string-append tmp-dir "/.badb/geasa/cap-test"))
                   (reg (make-registry)))
              (mkdir-p bundle-dir)
              (write-file! (string-append bundle-dir "/plugin.scm")
                           "(define-module (project cap-test) #:use-module (badb geis capabilities) #:export (read-approved read-other))\n(define (read-approved path) ((geis-capability 'fs-read \"/approved\") path))\n(define (read-other path) ((geis-capability 'fs-read \"/other\") path))\n")
              (write-file! (string-append bundle-dir "/spec.geis.scm")
                           "(spec-begin \"cap-test\")(test-assert #t)(spec-end)\n")
              (let ((manifest `((bundle-version . 1)
                                (feature-id . "cap-test")
                                (version . "1.0.0")
                                (spec . "spec.geis.scm")
                                (capabilities . ((fs-read "/approved")))
                                (plugin . ((module . (project cap-test))
                                           (file . "plugin.scm")))
                                (tools . ("read-approved" "read-other"))
                                (sha256 . ,(compute-geis-sha256-pins bundle-dir))))
                    (provider (lambda (name target)
                                (and (eq? name 'fs-read)
                                     (string=? target "/approved")
                                     (lambda (path) (string-append "host-approved:" path))))))
                (call-with-output-file (string-append bundle-dir "/manifest.scm")
                                       (lambda (port) (write manifest port)))
                (let ((mounted (discover-and-mount-project-geasa!
                                 reg #:workspace tmp-dir
                                 #:capability-provider provider
                                 #:sandbox-runner %direct-test-runner)))
                  (test-assert "capability bundle mounted after host grant"
                               (member "cap-test" mounted))
                  (test-equal "capability grant is target-specific and brokered"
                              "host-approved:input"
                              ((registry-lookup reg "read-approved") "input"))
                  (test-assert "a plugin cannot widen a grant to another target"
                               (catch #t
                                 (lambda () ((registry-lookup reg "read-other") "input") #f)
                                 (lambda _ #t)))
                  (test-assert "capability closure expires after the tool call"
                               (catch #t
                                 (lambda () (geis-capability 'fs-read "/approved") #f)
                                 (lambda _ #t)))))))

(test-group "geis-loader-denied-capabilities"
            (let* ((tmp-dir (mkdtemp "/tmp/badb-geis-loader-denied-XXXXXX"))
                   (bundle-dir (string-append tmp-dir "/.badb/geasa/denied-test"))
                   (reg (make-registry)))
              (mkdir-p bundle-dir)
              (write-file! (string-append bundle-dir "/plugin.scm")
                           "(define-module (project denied-test) #:export (run))\n(define (run) #t)\n")
              (write-file! (string-append bundle-dir "/spec.geis.scm")
                           "(spec-begin \"denied-test\")(test-assert #t)(spec-end)\n")
              (let ((manifest `((bundle-version . 1)
                                (feature-id . "denied-test")
                                (version . "1.0.0")
                                (spec . "spec.geis.scm")
                                (capabilities . ((net-http "api.example.test")))
                                (plugin . ((module . (project denied-test))
                                           (file . "plugin.scm")))
                                (tools . ("run"))
                                (sha256 . ,(compute-geis-sha256-pins bundle-dir)))))
                (call-with-output-file (string-append bundle-dir "/manifest.scm")
                                       (lambda (port) (write manifest port))))
              (test-assert "requested capabilities without host approval fail closed"
                           (not (member "denied-test"
                                        (discover-and-mount-project-geasa!
                                          reg #:workspace tmp-dir
                                          #:sandbox-runner %direct-test-runner))))))

(test-group "geis-catalog-index-and-query"
            (let* ((index (geis-catalog-index))
                   (query-all (geis-index-query))
                   (query-spec (geis-index-query #:category "spec-gate"))
                   (query-none (geis-index-query #:query "non-existent-xyz-feature-12345")))
              (test-assert "catalog index is non-empty list" (and (list? index) (pair? index)))
              (test-assert "spec-gate is present in index"
                           (any (lambda (entry) (string=? (assoc-ref entry 'fid) "spec-gate")) index))
              (test-assert "query-all contains header and spec-gate"
                           (and (string-contains query-all "Available Geasa Index")
                                (string-contains query-all "spec-gate")))
              (test-assert "query-spec matches spec-gate"
                           (string-contains query-spec "spec-gate"))
              (test-assert "query-none reports no matching geasa"
                           (string-contains query-none "No geasa found"))))

(spec-end)
(spec-exit)
