;;; rtk — CLI proxy that reduces LLM token consumption.

(define-module (badb packages rtk)
               #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix build-system copy)
               #:use-module ((guix licenses) #:prefix license:))

(define-public rtk
               (package
                 (name "rtk")
                 (version "1.2.0")
                 (source
                   (origin
                     (method url-fetch)
                     (uri (string-append
                            "https://github.com/rtk-ai/rtk/releases/download/v"
                            version "/rtk-x86_64-unknown-linux-musl.tar.gz"))
                     (sha256
                       (base32 "0dys1syva2c1ihfnj4xh5yyb1089alsj6zn7jkg3vi28lnsfgzkh"))))
                 (build-system copy-build-system)
                 (arguments
                   '(#:install-plan '(("rtk" "bin/"))))
                 (home-page "https://github.com/rtk-ai/rtk")
                 (synopsis "CLI proxy that reduces LLM token consumption")
                 (description
                   "rtk (Rust Token Killer) is a CLI proxy that filters and compresses
command outputs before they reach LLM context windows.")
                 (supported-systems '("x86_64-linux"))
                 (license license:expat)))
