;;; Fixture spec: everything passes.  Exercises the full documented
;;; spec-file shape — export-boundary check, two quickcheck contract
;;; properties, and SRFI-253 checked-wrapper boundary examples — and
;;; must emit the JSON diagnostics line the kernel gate accepts
;;; (verdict "pass", exit 0).

(use-modules (srfi srfi-64)
             (srfi srfi-253)
             (quickcheck)
             (quickcheck arbitrary)
             (quickcheck property)
             (badb spec)
             (test fixtures lib math-utils))

(spec-begin "fixture-pass")

(define-module-spec (test fixtures lib math-utils)
  ((double 1)
   (clamp 3 3)))

(define-contract-spec clamp
  ((lo $integer) (hi $integer) (x $integer))
  -> (result
      (and (<= (min lo hi) result)
           (<= result (max lo hi)))))

(define-contract-spec double
  ((x $integer))
  #:name "double is x + x and even"
  -> (result
      (and (= result (+ x x))
           (even? result))))

(define-checked-wrapper (checked-double double) ((x integer?)))
(define-checked-wrapper (checked-clamp clamp)
  ((lo integer?) (hi integer?) (x integer?)))

(test-assert "checked wrapper delegates on valid input"
  (= 84 (checked-double 42)))

(test-assert "checked clamp wrapper still clamps"
  (= 10 (checked-clamp 0 10 42)))

(test-assert "checked wrapper rejects non-integer input"
  (catch #t
    (lambda () (checked-double "not an integer") #f)
    (lambda args #t)))

(spec-end)
(spec-exit)
