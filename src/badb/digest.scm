;;; badb — Cryptographic digest compatibility shim
(define-module (badb digest)
               #:use-module (badb kernel digest)
               #:re-export (sha256-hex))
