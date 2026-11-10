;;; dist-lib.scm — Pure, dependency-free surface of the distribution
;;; tooling for tests.  Re-exports the naming, argument-parsing, and
;;; helper-text bindings from (badb dist) so test/test-dist.scm verifies
;;; exactly what scripts/dist ships without needing a substitute-enabled
;;; guix.

(define-module (test dist-lib)
               #:use-module (badb dist)
               #:re-export (pack-file-name
                             pack-command
                             parse-dist-args
                             extraction-helper-snippet))
