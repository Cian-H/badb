;;; test/test-dist.scm — tests for the relocatable pack distribution tooling.
;;;
;;; These tests exercise the pure, dependency-free parts of the
;;; distribution tooling (scripts/dist): argument parsing, pack naming,
;;; and the generated extraction helper.  The end-to-end `guix pack`
;;; build itself is intentionally NOT run here (it is expensive and
;;; needs a substitute-enabled guix); scripts/test-e2e covers a
;;; smoke build behind an env guard.

(define-module (test test-dist)
               #:use-module (ice-9 match)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-64)
               #:use-module (test dist-lib))

(test-begin "dist")

(test-group "pack-naming"
            ;; Pack file names must be deterministic, lowercase, and carry the
            ;; version + architecture so artifacts are unambiguous in CI caches.
            (test-equal "badb-0.1.0-x86_64-linux.tar.gz"
                        (pack-file-name "0.1.0" "x86_64-linux"))
            (test-assert "pack names are lowercase"
                         (string=? (string-downcase (pack-file-name "0.1.0" "X86_64-Linux"))
                                   (pack-file-name "0.1.0" "x86_64-linux")))
            (test-equal "version with prerelease suffix is preserved verbatim"
                        "badb-0.1.0-rc1-aarch64-linux.tar.gz"
                        (pack-file-name "0.1.0-rc1" "aarch64-linux")))

(test-group "argument-parsing"
            (test-equal "defaults: no args -> output dir + no -o override"
                        (list "dist" #f)
                        (match (parse-dist-args '())
                               ((out-dir extra) (list out-dir extra))
                               (_ 'parse-error)))
            (test-equal "--output-dir is honored"
                        (list "/tmp/packs" #f)
                        (match (parse-dist-args '("--output-dir" "/tmp/packs"))
                               ((out-dir extra) (list out-dir extra))
                               (_ 'parse-error)))
            (test-equal "-o forwards a raw pack output override"
                        (list "dist" "/tmp/x.tar.gz")
                        (match (parse-dist-args '("-o" "/tmp/x.tar.gz"))
                               ((out-dir extra) (list out-dir extra))
                               (_ 'parse-error))
                        )
            (test-assert "unknown option is rejected"
                         (match (parse-dist-args '("--bogus"))
                                ('error #t)
                                (_ #f)))

            (test-equal "unknown option reports error"
                        'error
                        (parse-dist-args '("--bogus"))))

(test-group "pack-command"
            (test-assert "tarball format (default) is relocatable, no -f flag"
                         (let ((argv (pack-command "/repo" "0.1.0")))
                           (and (equal? (list-head argv 2) '("pack" "-RR"))
                                (not (member "-f" argv))
                                (equal? (last argv) "/repo/dist/badb-0.1.0-x86_64-linux.tar.gz"))))
            (test-assert "docker format emits -f docker for OCI images"
                         (let ((argv (pack-command "/repo" "0.1.0" #:format "docker")))
                           (and (member "-f" argv) (member "docker" argv)
                                (equal? (last argv) "/repo/dist/badb-0.1.0-x86_64-linux.tar.gz")))))

(test-group "extract-helper"
            (define helper (extraction-helper-snippet "badb-0.1.0-x86_64-linux.tar.gz"))
            (test-assert "helper mentions the tarball"
                         (string-contains helper "badb-0.1.0-x86_64-linux.tar.gz"))
            (test-assert "helper uses relocatable-safe invocation (no daemon, no root)"
                         (and (string-contains helper "tar")
                              (not (string-contains helper "sudo"))
                              (not (string-contains helper "guix daemon"))))
            (test-assert "helper documents the run command"
                         (string-contains helper "./bin/badb")))

(test-end "dist")
