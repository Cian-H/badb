;;; Fixture module under test for badb spec-engine fixtures.
;;;
;;; Deliberately tiny and obviously correct: it exists to be the
;;; target of the declarative specs in test/fixtures/spec-*.scm and
;;; to exercise the export-boundary reflection check (two exports,
;;; distinct arities: clamp 3, double 1).

(define-module (test fixtures lib math-utils)
               #:export (clamp double))

(define (clamp lo hi x)
        "Clamp X into the closed interval [LO, HI] (or [HI, LO] when the
bounds are swapped)."
        (min hi (max lo x)))

(define (double x)
        "Return X added to itself."
        (+ x x))
