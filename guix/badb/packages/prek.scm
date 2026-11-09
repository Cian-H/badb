;;; prek — Fast Git hook manager written in Rust.

(define-module (badb packages prek)
               #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix build-system copy)
               #:use-module ((guix licenses) #:prefix license:))

(define-public prek
               (package
                 (name "prek")
                 (version "0.5.10")
                 (source
                   (origin
                     (method url-fetch)
                     (uri (string-append
                            "https://github.com/j178/prek/releases/download/v"
                            version "/prek-x86_64-unknown-linux-musl.tar.gz"))
                     (sha256
                       (base32 "0c1w4l7hj0vlfpp6qnkcj0ygfjk4hys3zhhklapzx44cp6f662n7"))))
                 (build-system copy-build-system)
                 (arguments
                   '(#:install-plan '(("prek" "bin/"))))
                 (home-page "https://github.com/j178/prek")
                 (synopsis "Fast pre-commit runner written in Rust")
                 (description
                   "prek is a high-performance drop-in replacement for pre-commit written in
Rust.  It supports standard .pre-commit-config.yaml files with zero runtime
dependencies and concurrent hook execution.")
                 (supported-systems '("x86_64-linux"))
                 (license (list license:expat license:asl2.0))))
