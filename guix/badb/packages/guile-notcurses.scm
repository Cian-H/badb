;;; guile-notcurses — Guix package for the guile-notcurses FFI bindings.

(define-module (badb packages guile-notcurses)
               #:use-module (guix packages)
               #:use-module (guix gexp)
               #:use-module (guix git-download)
               #:use-module (guix build-system guile)
               #:use-module ((guix licenses) #:prefix license:)
               #:use-module (gnu packages guile)
               #:use-module (gnu packages notcurses))

(define-public guile-notcurses
               (package
                 (name "guile-notcurses")
                 (version "3.0.16")
                 (source
                   (origin
                     (method git-fetch)
                     (uri (git-reference
                            (url "https://github.com/Cian-H/guile-notcurses")
                            (commit "7aa84cbc2e9fe76d0c56ec27e824d037ed3c61f3")))
                     (file-name (git-file-name name version))
                     (sha256
                       (base32 "1zkxf8dp8k93pd7q29h7ww0r1cpkzyrkmzf8l5g12dvy9yg4w1xh"))))
                 (build-system guile-build-system)
                 (native-inputs
                   (list guile-3.0))
                 (inputs
                   (list notcurses))
                 (arguments
                   (list
                     #:source-directory "."
                     #:not-compiled-file-regexp "(^|/)(guix\\.scm|tests/.*)$"
                     #:compile-flags #~'("-O2")
                     #:phases
                     #~(modify-phases %standard-phases
                                      (add-after 'unpack 'patch-library-paths
                                                 (lambda* (#:key inputs #:allow-other-keys)
                                                          (let ((nc-lib (string-append (assoc-ref inputs "notcurses") "/lib")))
                                                            (substitute* "notcurses/ffi.scm"
                                                                         (("\"libnotcurses\\.so\\.3\"")
                                                                          (string-append "\"" nc-lib "/libnotcurses.so.3\""))
                                                                         (("\"libnotcurses-core\\.so\\.3\"")
                                                                          (string-append "\"" nc-lib "/libnotcurses-core.so.3\""))
                                                                         (("\"libnotcurses-ffi\\.so\\.3\"")
                                                                          (string-append "\"" nc-lib "/libnotcurses-ffi.so.3\""))))))
                                      (add-after 'build 'check
                                                 (lambda* (#:key tests? #:allow-other-keys)
                                                          (when tests?
                                                            (setenv "GUILE_LOAD_PATH" (getcwd))
                                                            (setenv "GUILE_LOAD_COMPILED_PATH"
                                                                    (string-append (getcwd) ":" (or (getenv "GUILE_LOAD_COMPILED_PATH") "")))
                                                            (for-each
                                                              (lambda (t)
                                                                (invoke "guile" "--no-auto-compile" t))
                                                              '("tests/test-ffi-coverage.scm"
                                                                "tests/test-structs.scm"
                                                                "tests/test-channels-cells.scm"
                                                                "tests/test-compat-curses.scm"))))))))
                 (home-page "https://github.com/Cian-H/guile-notcurses")
                 (synopsis "Complete thin GNU Guile 3.x FFI wrapper for libnotcurses 3.x")
                 (description
                   "guile-notcurses provides complete thin GNU Guile 3.x FFI bindings to
libnotcurses 3.x (core, planes, cells, channels, input, visuals, direct mode,
and widgets), bytevector-backed C struct wrappers, terminal utilities, and an
optional curses compatibility layer.")
                 (license license:lgpl3+)))
