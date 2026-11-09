;;; test/specs/geis-bundle-cli.geis.scm — Tests for geis-bundle-pack / geis-bundle-install
;;;
;;; Covers pack (temp copy, stamped sha256 pins, tarball + sidecar) and
;;; install (validation, transitive dependency locking in order, locked-copy
;;; verification).  Fixtures live in a mkdtemp workspace with saved/restored
;;; BADB_GEIS_PATH and BADB_GEIS_GATE_ROOT.  Fixture spec files are built
;;; with the Q helper so no string literal embeds quote characters.

(define-module (test test-geis-bundle-cli)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (badb geis bundle)
               #:use-module (badb kernel digest)
               #:use-module (badb kernel geis-gate)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 textual-ports)
               #:use-module (srfi srfi-1))

(define Q (string #\"))

(define (write-file! path text)
        (call-with-output-file path (lambda (port) (display text port))))

(define (write-manifest! bundle-dir alist)
        (call-with-output-file (string-append bundle-dir "/manifest.scm")
                               (lambda (port) (write alist port))))

(define (read-file-string path)
        (call-with-input-file path
                              (lambda (port) (set-port-encoding! port "UTF-8") (get-string-all port))))

;; A runnable spec script that passes the gate's diagnostics contract.
;; Built from parts so no literal carries an embedded quote.
(define (passing-spec-script)
        (string-append
          "(use-modules (srfi srfi-64) (json))\n"
          "(test-begin " Q "dep" Q ")\n"
          "(test-assert " Q "dep ok" Q " #t)\n"
          "(test-end " Q "dep" Q ")\n"
          "(display (scm->json-string '((summary . ((total . 1) (passed . 1) (failed . 0)))"
          " (verdict . " Q "pass" Q ") (seed . " Q "cli" Q ") (failures . ()))))\n"
          "(newline)\n"))

(define %temp-dir (mkdtemp "/tmp/badb-geis-cli-test-XXXXXX"))
(define %geis-root (string-append %temp-dir "/geis"))
(define %gate-root (string-append %temp-dir "/gate"))
(mkdir %geis-root)
(define %saved-geis-path (getenv "BADB_GEIS_PATH"))
(define %saved-gate-root (getenv "BADB_GEIS_GATE_ROOT"))
(setenv "BADB_GEIS_PATH" %geis-root)
(setenv "BADB_GEIS_GATE_ROOT" %gate-root)

(spec-begin "geis-bundle-cli")

(test-group "pack"
            (let ((dir (string-append %geis-root "/cli-pack")))
              (mkdir dir)
              (write-file! (string-append dir "/spec.geis.scm") (passing-spec-script))
              (write-file! (string-append dir "/plugin.scm")
                           "(define-module (geis cli-pack plugin))\n")
              (write-manifest! dir '((bundle-version . 1) (feature-id . "cli-pack")
                                     (version . "1.0.0") (spec . "spec.geis.scm")))
              (let* ((result (geis-bundle-pack "cli-pack"))
                     (tarball (assoc-ref result 'tarball))
                     (bundle-hash (assoc-ref result 'bundle-sha256))
                     (sidecar (string-append tarball ".sha256")))
                (test-assert "pack returns tarball path and bundle hash"
                             (and (string? tarball) (string? bundle-hash)))
                (test-assert "tarball exists under dist/"
                             (and (string-contains tarball "dist/badb-geis-cli-pack-1.0.0.tar.gz")
                                  (file-exists? tarball)))
                (test-assert "sidecar contains the bundle hash"
                             (string= (string-trim-right (read-file-string sidecar)) bundle-hash))
                (test-assert "sidecar hash matches the original bundle's hash"
                             (string= (string-trim-right (read-file-string sidecar))
                                      (geis-bundle-hash dir)))
                (let ((xdir (mkdtemp "/tmp/badb-geis-cli-x-XXXXXX")))
                  (system* "tar" "-xzf" tarball "-C" xdir)
                  (let* ((manifest (read-geis-manifest
                                     (string-append xdir "/cli-pack/manifest.scm")))
                         (pins (manifest-field manifest 'sha256))
                         (spec-pin (assoc-ref pins "spec.geis.scm")))
                    (test-assert "packed manifest carries sha256 pins"
                                 (and (pair? pins) (string? spec-pin)))
                    (test-assert "packed spec pin matches content"
                                 (string= (sha256-hex (read-file-string
                                                        (string-append xdir "/cli-pack/spec.geis.scm")))
                                          spec-pin)))
                  (system* "rm" "-rf" xdir))
                (delete-file sidecar)
                (delete-file tarball)
                (catch #t (lambda () (rmdir "dist")) (lambda _ #f))
                (test-assert "pack artifacts cleaned up" (not (file-exists? tarball))))))

(test-group "pack-rejects-invalid"
            (let ((dir (string-append %geis-root "/pack-bad")))
              (mkdir dir)
              (write-manifest! dir '((bundle-version . 1) (feature-id . "wrong-name")
                                     (version . "1.0.0") (spec . "spec.geis.scm")))
              (test-assert "pack refuses a bundle whose feature-id mismatches its dir"
                           (catch 'geis-bundle-error
                             (lambda () (geis-bundle-pack "pack-bad") #f)
                             (lambda (key . args) (eq? key 'geis-bundle-error))))))

(test-group "install"
            ;; dependency bundle: a runnable spec that passes
            (let ((dep-dir (string-append %geis-root "/cli-dep")))
              (mkdir dep-dir)
              (write-file! (string-append dep-dir "/spec.geis.scm") (passing-spec-script))
              (write-manifest! dep-dir '((bundle-version . 1) (feature-id . "cli-dep")
                                         (version . "1.0.0") (spec . "spec.geis.scm")))
              (let* ((dep-hash (geis-bundle-hash dep-dir))
                     (dir (string-append %geis-root "/cli-target")))
                (mkdir dir)
                (write-file! (string-append dir "/plugin.scm")
                             (string-append "(define-module (geasa cli-target plugin)\n"
                                            "  #:export (answer))\n\n(define (answer) 42)\n"))
                (write-file! (string-append dir "/spec.geis.scm")
                             (string-append "(use-modules (geasa cli-target plugin) (srfi srfi-64) (json))\n"
                                            "(test-begin " Q "target" Q ")\n"
                                            "(test-assert " Q "answer is 42" Q " (= 42 (answer)))\n"
                                            "(test-end " Q "target" Q ")\n"
                                            "(display (scm->json-string '((summary . ((total . 1) (passed . 1) (failed . 0)))"
                                            " (verdict . " Q "pass" Q ") (seed . " Q "cli" Q ") (failures . ()))))\n"
                                            "(newline)\n"))
                (write-manifest! dir
                                 (list (cons 'bundle-version 1)
                                       (cons 'feature-id "cli-target")
                                       (cons 'version "1.0.0")
                                       (cons 'spec "spec.geis.scm")
                                       (cons 'plugin '((module . (geasa cli-target plugin)) (file . "plugin.scm")))
                                       (cons 'depends
                                             (list (cons 'geis
                                                         (list (cons "cli-dep"
                                                                     (list (cons 'version "1.0.0")
                                                                           (cons 'bundle-sha256 dep-hash)))))))))
                (let ((report (geis-bundle-install dir)))
                  (test-assert "install report names the feature"
                               (string= "cli-target" (assoc-ref report 'feature-id)))
                  (test-assert "locked list has dependency first, target last"
                               (equal? '("cli-dep" "cli-target") (assoc-ref report 'locked)))
                  (test-assert "report carries a 64-hex bundle hash"
                               (and (string? (assoc-ref report 'bundle-sha256))
                                    (= 64 (string-length (assoc-ref report 'bundle-sha256)))))
                  (test-assert "dependency locked under gate root"
                               (file-exists? (string-append %gate-root "/geasa/cli-dep/spec.geis.scm")))
                  (test-assert "target locked under gate root"
                               (file-exists? (string-append %gate-root "/geasa/cli-target/spec.geis.scm")))
                  (test-assert "locked plugin is read-only"
                               (let ((perms (stat:perms
                                              (stat (string-append %gate-root "/geasa/cli-target/plugin.scm")))))
                                 (= #o444 (logand perms #o777))))
                  (test-equal "install verifies the locked target: verdict pass"
                              "pass" (assoc-ref report 'verify-verdict))
                  ;; tamper the WORKING TREE plugin; the locked copy must still verify
                  (write-file! (string-append dir "/plugin.scm")
                               (string-append "(define-module (geasa cli-target plugin)\n"
                                              "  #:export (answer))\n\n(define (answer) 43)\n"))
                  (test-equal "locked copy used, not working tree"
                              "pass"
                              (assoc-ref (verify-locked-geis-bundle "cli-target"
                                                                    #:root %gate-root
                                                                    #:timeout 30)
                                         'verdict))))))

(test-group "install-rejects-invalid"
            (let ((dir (string-append %geis-root "/install-bad")))
              (mkdir dir)
              (write-manifest! dir '((bundle-version . 1) (feature-id . "wrong-name")
                                     (version . "1.0.0") (spec . "spec.geis.scm")))
              (test-assert "install refuses a bundle whose feature-id mismatches its dir"
                           (catch 'geis-bundle-error
                             (lambda () (geis-bundle-install dir) #f)
                             (lambda (key . args) (eq? key 'geis-bundle-error))))))

(if %saved-geis-path
  (setenv "BADB_GEIS_PATH" %saved-geis-path)
  (unsetenv "BADB_GEIS_PATH"))
(if %saved-gate-root
  (setenv "BADB_GEIS_GATE_ROOT" %saved-gate-root)
  (unsetenv "BADB_GEIS_GATE_ROOT"))
(system* "rm" "-rf" %temp-dir)

(spec-end)
(spec-exit)
