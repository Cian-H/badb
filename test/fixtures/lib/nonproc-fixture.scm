;;; Fixture module for the non-procedure-export case of the
;;; export-boundary reflection check: both bindings are plain
;;; variables, so a declared export of either must record a
;;; structured "not a procedure" failure.

(define-module (test fixtures lib nonproc-fixture)
               #:export (a-number a-string))

(define a-number 42)
(define a-string "hello")
