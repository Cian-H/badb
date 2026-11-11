(use-modules (srfi srfi-64)
             (badb kernel registry)
             (badb geis bundle)
             (geasa runtimes-geis-loader plugin)
             (ice-9 ftw)
             (srfi srfi-13)
             (ice-9 textual-ports))

(define (write-file! path content)
        (call-with-output-file path (lambda (port) (display content port))))

(define (mkdir-p path)
        (unless (or (string-null? path) (string=? path "/") (file-exists? path))
          (mkdir-p (dirname path))
          (catch #t (lambda () (mkdir path)) (lambda _ #f))))

(define (bwrap-available?)
        (let ((bwrap (search-path (string-split (or (getenv "PATH") "") #\:) "bwrap"))
              (true-bin (search-path (string-split (or (getenv "PATH") "") #\:) "true")))
          (and bwrap true-bin
               (zero? (system* bwrap "--unshare-all" "--new-session"
                               "--dir" "/gnu"
                               "--ro-bind" "/gnu/store" "/gnu/store"
                               "--" true-bin)))))

(test-begin "ocap-sandbox")

(if (not (bwrap-available?))
  (begin
    (test-skip 1 "bubblewrap user and network namespaces are unavailable")
    (test-assert "bubblewrap sandbox integration skipped" #t))
  (let* ((tmp (mkdtemp "/tmp/badb-ocap-sandbox-XXXXXX"))
         (bundle (string-append tmp "/.badb/geasa/probe"))
         (secret (string-append tmp "/host-secret.scm"))
         (pid (getpid))
         (reg (make-registry)))
    (mkdir-p bundle)
    (setenv "BADB_OCAP_TEST_SECRET" "host-only-value")
    (write-file! secret "(define host-secret \"host-only-value\")\n")
    (write-file! (string-append bundle "/plugin.scm")
                 (string-append
                   "(define-module (project sandbox-probe) #:use-module (badb geis capabilities) #:export (probe))\n"
                   "(define (probe path pid)\n"
                   "  `((file-visible . ,(file-exists? path))\n"
                   "    (host-process-visible . ,(file-exists? (string-append \"/proc/\" (number->string pid))))\n"
                   "    (host-env-visible . ,(getenv \"BADB_OCAP_TEST_SECRET\"))\n"
                   "    (broker-result . ,((geis-capability 'fs-read \"/approved\") path))))\n"))
    (write-file! (string-append bundle "/spec.geis.scm")
                 "(spec-begin \"probe\")(test-assert #t)(spec-end)\n")
    (let ((manifest
            `((bundle-version . 1)
              (feature-id . "probe")
              (version . "1.0.0")
              (spec . "spec.geis.scm")
              (capabilities . ((fs-read "/approved")))
              (plugin . ((module . (project sandbox-probe))
                         (file . "plugin.scm")))
              (tools . ("probe"))
              (sha256 . ,(compute-geis-sha256-pins bundle)))))
      (call-with-output-file (string-append bundle "/manifest.scm")
                             (lambda (port) (write manifest port))))
    (let ((mounted
            (discover-and-mount-project-geasa!
              reg #:workspace tmp
              #:capability-provider
              (lambda (name target)
                (and (eq? name 'fs-read)
                     (string=? target "/approved")
                     (lambda (_path) "host-broker-value")))
              #:logger (lambda (_level message)
                         (format (current-error-port) "loader: ~a~%" message)))))
      (test-assert "project bundle mounted in worker" (member "probe" mounted))
      (let ((probe (registry-lookup reg "probe")))
        (test-assert "project tool registered" (procedure? probe))
        (when (procedure? probe)
          (let ((result (probe secret pid)))
            (test-assert "host workspace file is hidden"
                         (not (assoc-ref result "file-visible")))
            (test-assert "host process tree is hidden"
                         (not (assoc-ref result "host-process-visible")))
            (test-assert "host environment is cleared"
                         (not (assoc-ref result "host-env-visible")))
            (test-equal "declared capability is brokered by the host"
                        "host-broker-value"
                        (assoc-ref result "broker-result"))))))))

(test-end "ocap-sandbox")
