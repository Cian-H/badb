;;; Fixture spec: fails.  Combines an export-boundary violation
;;; (declared minimum arity 4 for clamp, which requires at least 3)
;;; with a falsifying quickcheck property (double does not return
;;; x + x + 1).  The export check is authored first so it fails fast,
;;; before the property suite runs.  Emits verdict "fail" and exits 1.

(use-modules (srfi srfi-64)
             (srfi srfi-253)
             (quickcheck)
             (quickcheck arbitrary)
             (quickcheck property)
             (badb spec)
             (test fixtures lib math-utils))

(spec-begin "fixture-fail")

(define-module-spec (test fixtures lib math-utils)
                    ((double 1)
                     (clamp 4)))

(define-contract-spec double
                      ((x $integer))
                      -> (result (= result (+ x x 1))))

(spec-end)
(spec-exit)
