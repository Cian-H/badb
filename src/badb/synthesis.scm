;;; badb — Sema Synthesis Workflows & Dogfood (roadmap Phase 3).
;;;
;;; Backwards compatibility facade for (badb synthesis), re-exporting all
;;; public symbols from unprivileged runtime subsystem in (badb runtimes synthesis).

(define-module (badb synthesis)
  #:use-module (badb runtimes synthesis)
  #:re-export (find-sema-executable
               lock-feature-spec
               commit-synthesized-module!
               commit-synthesized-module-cli
               run-synthesis-workflow
               synthesize-feature!
               run-synthesis-cli))
