;;; badb — Spec-gate / Geis-gate compatibility shim
(define-module (badb spec-gate)
               #:use-module (badb kernel geis-gate)
               #:re-export (spec-gate-root
                             spec-gate-dir
                             sanitize-feature-id
                             locked-spec-path
                             lock-spec
                             verify-locked-spec
                             spec-gate-transaction-pins
                             spec-gate-ast-diff-hash
                             geis-gate-root
                             geis-gate-dir
                             locked-geis-path
                             lock-geis
                             verify-locked-geis
                             geis-gate-transaction-pins
                             geis-gate-ast-diff-hash))
