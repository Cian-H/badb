;;; badb — Guix package for the badb microharness, with pre-compiled
;;; modules.
;;;
;;; badb is a pure Guile project: the whole source tree (badb kernel,
;;; geasa capability bundles, and the vendored SRFI-253) installs
;;; straight onto the Guile load path.  This package copies src/ into
;;; share/guile/site/3.0, adds a bin/badb wrapper that runs the real
;;; entry point with GUILE_AUTO_COMPILE=0, and pre-compiles every .scm
;;; to a .go in lib/guile/3.0/site-ccache so startup is instant and
;;; silent: the interpreter never emits ";;; compiling" noise.  Sources
;;; are kept next to the ccache because Guile still wants them for
;;; macros and docstrings.
;;;
;;; The repository has no git remote to fetch from, so the origin is a
;;; recursive local-file of the src/ tree, resolved relative to this
;;; file (guix/badb/packages/): `guix build -L guix badb' from the repo
;;; root picks it up regardless of the caller's cwd.  Unpacking a
;;; directory origin drops its contents in the build root, hence the
;;; "." entry in the install plan.

(define-module (badb packages badb)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix search-paths)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages notcurses)
  #:use-module (badb packages sema)
  #:use-module ((guix licenses) #:prefix license:))

(define %repo-root
  ;; The repository checkout containing this package file, resolved from
  ;; the file's own location; falls back to the working directory (the
  ;; repo root when invoked as `guix build -L guix badb') if source
  ;; information is unavailable.
  (or (and=> (current-filename)
             (lambda (file)
               (canonicalize-path
                (string-append (dirname (dirname (dirname file))) "/.."))))
      (getcwd)))

(define %badb-launcher
  ;; Installed Guile launcher, the packaged twin of scripts/badb's tail:
  ;; add the geis bundle search roots (BADB_GEIS_PATH, default "geis")
  ;; to the load path so (geis <fid> plugin) modules resolve as raw
  ;; scheme, then import the real entry point from (badb main) so that
  ;; `guile -e main' can find it.
  "\
(let ((geis-path (getenv \"BADB_GEIS_PATH\")))
  (for-each
    (lambda (root)
      (unless (or (not root) (string-null? root))
        (catch #t
          (lambda () (add-to-load-path root))
          (lambda _ #f))))
    (if (or (not geis-path) (string-null? geis-path))
        '(\"geis\")
        (string-split geis-path #\\:))))
(use-modules (badb main))
")

(define-public badb
  (package
    (name "badb")
    (version "0.1.0")
    (source
     (local-file (string-append %repo-root "/src")
                 "badb-src"
                 #:recursive? #t))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "share/guile/site/3.0"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-launcher-and-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (guile (assoc-ref inputs "guile"))
                    (json-site
                     (string-append (assoc-ref inputs "guile-json")
                                    "/share/guile/site/3.0"))
                    (quickcheck-site
                     (string-append (assoc-ref inputs "guile-quickcheck")
                                    "/share/guile/site/3.0"))
                    (site (string-append out "/share/guile/site/3.0"))
                    (ccache (string-append out "/lib/guile/3.0/site-ccache"))
                    (share (string-append out "/share/badb"))
                    (bin (string-append out "/bin"))
                    (wrapper (string-append bin "/badb")))
               ;; Guile launcher imported by the wrapper.
               (mkdir-p share)
               (call-with-output-file (string-append share "/badb-launcher.scm")
                 (lambda (port)
                   (display ,%badb-launcher port)))
               ;; Shell wrapper: bake in the interpreter, the site dirs,
               ;; and the ccache so the program is self-contained and
               ;; never auto-compiles at runtime.
               (mkdir-p bin)
               (call-with-output-file wrapper
                 (lambda (port)
                   (format port "#!/bin/sh
# badb wrapper: run the pre-compiled badb microharness.
GUILE_AUTO_COMPILE=0
export GUILE_AUTO_COMPILE
GUILE_LOAD_COMPILED_PATH=~a${GUILE_LOAD_COMPILED_PATH:+:$GUILE_LOAD_COMPILED_PATH}
export GUILE_LOAD_COMPILED_PATH
exec ~a/bin/guile --no-auto-compile -L ~a -L ~a -L ~a \\
  -e main -s ~a \"$@\"
"
                           ccache guile site json-site quickcheck-site
                           (string-append share "/badb-launcher.scm"))))
               (chmod wrapper #o555))))
         (add-after 'install-launcher-and-wrapper 'pre-compile-modules
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site (string-append out "/share/guile/site/3.0"))
                    (ccache (string-append out "/lib/guile/3.0/site-ccache"))
                    (json-site
                     (string-append (assoc-ref inputs "guile-json")
                                    "/share/guile/site/3.0"))
                    (quickcheck-site
                     (string-append (assoc-ref inputs "guile-quickcheck")
                                    "/share/guile/site/3.0")))
               ;; guild needs the full load path (own site first) to
               ;; resolve dependencies while compiling; libnotcurses
               ;; must be on the loader path because loading the TUI FFI
               ;; module at macro-expansion time dlopens it.
               (setenv "GUILE_AUTO_COMPILE" "0")
               (setenv "GUILE_LOAD_PATH"
                       (string-join (list site json-site quickcheck-site) ":"))
               (setenv "GUILE_LOAD_COMPILED_PATH" ccache)
               (setenv "LD_LIBRARY_PATH"
                       (string-append (assoc-ref inputs "notcurses") "/lib"))
               (mkdir-p ccache)
               (for-each
                (lambda (file)
                  (let* ((rel (substring file (+ (string-length site) 1)))
                         (base (basename rel ".scm"))
                         (dir (dirname rel))
                         (target (string-append ccache "/" dir "/" base ".go")))
                    (mkdir-p (string-append ccache "/" dir))
                    (invoke "guild" "compile" "-o" target file)))
                (find-files site
                            (lambda (file stat)
                              ;; Bundle manifests are data alists, not
                              ;; modules, and fixtures are workflow test
                              ;; inputs (some are deliberately broken):
                              ;; keep both as sources only.
                              (and (string-suffix? ".scm" file)
                                   (not (string=? (basename file)
                                                  "manifest.scm"))
                                   (not (string-contains file
                                                         "/fixtures/"))))))))))))
    (inputs
     (list guile-3.0 guile-json-4))
    (propagated-inputs
     (list notcurses guile-quickcheck sema))
    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_LOAD_PATH")
            (files '("share/guile/site/3.0")))
           (search-path-specification
            (variable "GUILE_LOAD_COMPILED_PATH")
            (files '("lib/guile/3.0/site-ccache")))))
    (home-page #f)
    (synopsis "Microharness for driving LLM agents over Guile tool kernels")
    (description
     "badb is a Guile microharness: a small kernel (src/badb) plus geasa
capability bundles (src/geasa) that registers tools, MCP servers, and
contract specifications (geis) around a notcurses terminal user
interface.  The harness journals every run transactionally and can
verify, pack, and install capability bundles against their pinned
manifests.")
    (license license:lgpl3)))
