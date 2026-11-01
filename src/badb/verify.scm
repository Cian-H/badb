;;; badb — Neuro-Symbolic Verification Engine
;;;
;;; Backwards compatibility facade for (badb verify), re-exporting all
;;; public symbols from unprivileged runtime subsystem in (badb runtimes verify).

(define-module (badb verify)
  #:use-module (badb runtimes verify)
  #:re-export (<verifier>
               verify-scheme-syntax
               verify-test-suite
               make-verifier
               verifier?
               verifier-check-syntax?
               set-verifier-check-syntax!
               verifier-run-tests?
               set-verifier-run-tests!
               verifier-test-command
               set-verifier-test-command!
               verifier-custom-validators
               set-verifier-custom-validators!
               add-verifier-validator!
               verify-action
               with-neuro-symbolic-gate))
