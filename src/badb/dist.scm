;;; dist.scm — Relocatable pack distribution tooling for badb.
;;;
;;; Implements the zero-install binary distribution roadmap item: builds
;;; `guix pack -RR' tarballs (PRoot/user-namespace relocatable packs) that
;;; run without a Guix daemon, root privileges, or pre-installed
;;; dependencies.  The pure helpers here (naming, argument parsing,
;;; helper text) are re-exported by (test dist-lib) so test/test-dist.scm
;;; exercises exactly what scripts/dist ships.

(define-module (badb dist)
               #:use-module
               (badb version) #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 match)
               #:use-module
               (srfi srfi-1)
               #:export
               (%default-version %default-output-dir
                                 pack-file-name
                                 host-arch
                                 pack-command
                                 parse-dist-args
                                 extraction-helper-snippet
                                 single-file-stub
                                 usage-text
                                 mkdir-p)
               ;; scripts/dist imports only this module; re-export the bits it needs.
               #:re-export
               (match last))

;; Recursive directory creation, defined locally so this module stays
;; dependency-free (the Guix build utils copy lives in the pack closure,
;; not on our load path).
(define (mkdir-p dir)
        "Create directory DIR and any missing parents."
        (let loop ((parts (string-split dir #\/)) (so-far ""))
                  (match parts
                         (() (values))
                         ((head . rest)
                          (let ((path (if (string-null? so-far)
                                        head
                                        (string-append so-far "/" head))))
                            (catch 'system-error
                              (lambda () (mkdir path))
                              (lambda args
                                (let ((errno (system-error-errno args)))
                                  (unless (or (= errno EEXIST) (string-null? head))
                                    (apply throw args)))))
                            (loop rest path))))))

;; Mirrors %version in (badb main); kept literal so this module stays
;; dependency-free (importing (badb main) would drag in the full TUI).
(define %default-version %badb-version)

(define %default-output-dir "dist")

(define (pack-file-name version arch)
        "Return the deterministic pack artifact name for VERSION on ARCH.
Names are lowercased so artifacts are unambiguous in CI caches."
        (string-downcase (format #f "badb-~a-~a.tar.gz" version arch)))

(define (host-arch)
        "Return the Guix-style system name of the running host,
e.g. x86_64-linux.  Indexes the uname vector directly (sysname=0,
machine=4) so this module stays dependency-free."
        (let ((u (uname)))
          (string-downcase
            (string-append (vector-ref u 4) "-" (vector-ref u 0)))))

(define* (pack-command root-dir version #:key (format "tarball"))
         "Return the `guix pack' argument vector (sans program name) that builds
a badb pack of VERSION from the repository at ROOT-DIR.  FORMAT is
\"tarball\" (default), a relocatable (-RR) archive, or \"docker\", a
Docker/OCI image archive (via `-f docker').  The last element is the
output path."
         (let* ((out (string-append root-dir "/" %default-output-dir "/"
                                    (pack-file-name version (host-arch))))
                (fmt (match format
                            ("tarball" '())
                            ("docker" '("-f" "docker"))
                            (_ (error "pack-command: unsupported format" format)))))
           `("pack" "-RR" ,@fmt
             "-L" ,(string-append root-dir "/guix")
             "-S" "/bin=bin"
             "-m" ,(string-append root-dir "/guix/badb/pack-manifest.scm")
             ,out)))

(define (parse-dist-args args)
        "Parse scripts/dist argument list ARGS.
Returns (list OUT-DIR RAW-OUT) (or (list OUT-DIR RAW-OUT 'single-file) when
--single-file is passed), or the symbol 'error on unrecognized options."
        (let loop ((args args) (out-dir #f) (raw-out #f) (single? #f))
                  (match args
                         (() (if single?
                               (list (or out-dir %default-output-dir) raw-out 'single-file)
                               (list (or out-dir %default-output-dir) raw-out)))
                         (("--output-dir" dir rest ...) (loop rest dir raw-out single?))
                         (("-o" path rest ...) (loop rest out-dir path single?))
                         (("--single-file" rest ...) (loop rest out-dir raw-out #t))
                         (_ 'error))))

(define (single-file-stub payload-hash)
        "Return the POSIX shell self-extracting stub header for PAYLOAD-HASH."
        (format #f "#!/bin/sh
set -eu
HASH=~s
CACHE_ROOT=\"${XDG_CACHE_HOME:-$HOME/.cache}/badb/runtime/$HASH\"
MARKER=\"$CACHE_ROOT/.ready\"
if [ ! -f \"$MARKER\" ]; then
  TMP=\"${CACHE_ROOT}.tmp.$$\"
  rm -rf \"$TMP\"
  mkdir -p \"$TMP\"
  ARCHIVE_LINE=$(awk '/^__BADB_PAYLOAD__$/ {print NR + 1; exit 0}' \"$0\")
  tail -n +\"$ARCHIVE_LINE\" \"$0\" | tar -xz -C \"$TMP\"
  touch \"$TMP/.ready\"
  mkdir -p \"$(dirname \"$CACHE_ROOT\")\"
  mv \"$TMP\" \"$CACHE_ROOT\" 2>/dev/null || rm -rf \"$TMP\"
fi
exec \"$CACHE_ROOT/bin/badb\" \"$@\"
__BADB_PAYLOAD__
" payload-hash))

(define (extraction-helper-snippet tarball)
        "Return user-facing instructions for extracting and running the
relocatable pack tarball TARBALL (no daemon, no root required)."
        (format #f "Extract and run (no Guix daemon, no root required):

  tar xf ~a
  cd ~a
  ./bin/badb --help

The pack is relocatable: it ships every dependency and falls back to
PRoot/user namespaces, so it runs on any Linux system.  The extracted
directory can be moved anywhere on disk."
                tarball
                (string-drop-right tarball (string-length ".tar.gz"))))

(define (usage-text)
        "Return the scripts/dist usage banner."
        "Usage: scripts/dist [--output-dir DIR] [-o FILE] [--help]

Build a zero-install relocatable badb tarball via `guix pack -RR'.

  --output-dir DIR   directory for the pack artifact (default: dist)
  -o FILE            raw `guix pack' output path override
  --help             show this help and exit")
