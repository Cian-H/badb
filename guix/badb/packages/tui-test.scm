;;; tui-test — Guix package for the prebuilt tui-test CLI release binary.
;;;
;;; microsoft/tui-test is a Rust workspace that requires rustc 1.90, but
;;; the guix channel pinned in this repo's channels.scm provides rust
;;; 1.85.1 at most, so a from-source build is not currently possible
;;; without bootstrapping a newer toolchain.  Upstream publishes
;;; statically-linked (musl) release binaries with published sha256
;;; digests; this package pins the x86_64-linux-musl binary by checksum
;;; instead.  Every install fetches the exact same bytes and the digest is
;;; verified by guix, but note this is integrity pinning, not a source
;;; build: guix cannot reproduce these bytes from source.

(define-module (badb packages tui-test)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public tui-test
  (package
    (name "tui-test")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/microsoft/tui-test/releases/download/"
             version "/tui-test-x86_64-unknown-linux-musl.tar.gz"))
       (sha256
        (base32 "15bp6wnldh671bc68m9rv9209i1rdvb3xja3f07qni5cmc3bgi3v"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("tui-test" "bin/"))))
    (home-page "https://github.com/microsoft/tui-test")
    (synopsis
     "Control, inspect, test, and record real shell sessions and TUI apps")
    (description
     "tui-test controls, inspects, tests, and records real shell sessions
and full-screen terminal applications.  The CLI spawns sessions backed by
a daemon, drives them headlessly, and reports terminal text, state, and
screenshots (text, SVG, or PNG), plus animated recordings of the session.")
    (supported-systems '("x86_64-linux"))
    (license license:expat)))
