;;; guile-quickcheck — Guix package for Timothy Sample's Guile-QuickCheck.
;;;
;;; Guile-QuickCheck is a pure-Guile property-based testing library:
;;; no C code, no compiled extensions, just Scheme modules installed
;;; under share/guile/site/3.0.  Upstream (v0.1.0, 2021-03-01, tag
;;; b48ac6616e60a21b6cb7112dd27deebb5e8463e7 in the cgit repository at
;;; https://git.ngyro.com/guile-quickcheck) publishes a GPG-signed
;;; release tarball, so this package pins that exact artifact by
;;; checksum, like sema and tui-test above.  A git-fetch of the same
;;; tag was tried first but the host guix-daemon's builtin git
;;; download hangs for this origin, so the release tarball is used
;;; instead; it ships a pre-generated configure script and therefore
;;; needs no autotools.  Inputs mirror upstream Guix's own recipe for
;;; this package (gnu/packages/guile-xyz.scm).

(define-module (badb packages guile-quickcheck)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix search-paths)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define-public guile-quickcheck
  (package
    (name "guile-quickcheck")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.ngyro.com/guile-quickcheck/guile-quickcheck-"
             version ".tar.gz"))
       (sha256
        (base32 "03mwi1l3354x52nar0zwhcm0x29yai9xjln4p4gbchwvx5dsr6fb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list guile-3.0))
    (native-search-paths
     (list (search-path-specification
             (variable "GUILE_LOAD_PATH")
             (files '("share/guile/site/3.0")))
           (search-path-specification
             (variable "GUILE_LOAD_COMPILED_PATH")
             (files '("lib/guile/3.0/site-ccache")))))
    (home-page "https://ngyro.com/software/guile-quickcheck.html")
    (synopsis "Randomized property-based testing for Guile")
    (description
     "Guile-QuickCheck is a library for random testing of program
properties inspired by ghc-quickcheck.  You can use it to express
properties, which functions should satisfy, as Scheme code and then
check whether they hold in a large number of randomly generated test
cases.")
    (license license:gpl3+)))
