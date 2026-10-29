;;; Fixture spec: export-boundary reflection only, no property
;;; suites.  The module's export list is declared exactly, so the
;;; suite passes and emits verdict "pass" with exit 0.

(use-modules (srfi srfi-64)
             (srfi srfi-253)
             (quickcheck)
             (quickcheck arbitrary)
             (quickcheck property)
             (badb spec)
             (test fixtures lib math-utils))

(spec-begin "fixture-export-only")

(define-module-spec (test fixtures lib math-utils)
  ((double 1)
   (clamp 3 3)))

(spec-end)
(spec-exit)
