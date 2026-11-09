;;; test-srfi-253.scm — Tests for (srfi srfi-253)
;;;
;;; Vendored SRFI 253: data checking and coercion.  Covers check-arg,
;;; values-checked, check-case, lambda-checked, case-lambda-checked,
;;; define-checked (procedure and variable forms), and
;;; define-record-type-checked, including the code examples from the
;;; SRFI document itself.  Checked failures raise R6RS &assertion
;;; conditions, verified both opaquely (test-error) and through
;;; condition accessors (guard + condition-who/message).

(define-module (test test-srfi-253)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (ice-9 format)
               #:use-module ((rnrs exceptions) #:select (guard))
               #:use-module ((rnrs conditions) #:select (assertion-violation?
                                                          condition-who
                                                          condition-message
                                                          condition-irritants))
               #:use-module (srfi srfi-253))

;; Runs THUNK expecting an &assertion failure; returns
;; (who message irritants), or 'no-error-raised if it returns.
(define (assertion-info thunk)
        (guard (ex ((assertion-violation? ex)
                    (list (catch #t
                            (lambda () (condition-who ex))
                            (lambda args #f))
                          (condition-message ex)
                          (condition-irritants ex))))
               (thunk)
               'no-error-raised))

(spec-begin "foundation-srfi-253")

;;;
;;; 1. check-arg
;;;

(test-group "srfi-253-check-arg"
            (test-equal "pass, two arguments, returns the value"
                        5 (check-arg integer? 5))
            (test-equal "pass, three arguments, returns the value"
                        "hi" (check-arg string? "hi" 'some-caller))
            (test-equal "predicate expression is evaluated"
                        7 (check-arg (lambda (x) (and (integer? x) (> x 3))) 7))
            (test-error "failure raises an error, two arguments" #t
                        (check-arg integer? "not-an-int"))
            (test-error "failure raises an error, three arguments" #t
                        (check-arg integer? "not-an-int" 'some-caller))
            (let ((info (assertion-info (lambda () (check-arg string? 42 'some-caller)))))
              (test-equal "error is an &assertion naming the caller" 'some-caller
                          (car info))
              (test-equal "error message describes the mismatch"
                          "argument should match the specification" (cadr info))
              (test-assert "error irritants carry the checked form and value"
                           (and (member '(string? 42) (caddr info))
                                (member 42 (caddr info)))))
            (let ((calls 0))
              (check-arg (lambda (x) (set! calls (1+ calls)) (integer? x)) 5)
              (test-equal "predicate applied exactly once" 1 calls))
            (let ((calls 0))
              (check-arg integer? (begin (set! calls (1+ calls)) 5) 'caller)
              (test-equal "value expression evaluated exactly once" 1 calls)))

;;;
;;; 2. values-checked
;;;

(test-group "srfi-253-values-checked"
            (test-equal "single value" 9 (values-checked (integer?) 9))
            (test-equal "multiple values"
                        '(9 "hello")
                        (call-with-values
                          (lambda () (values-checked (integer? string?) 9 "hello"))
                          list))
            ;; Spec example: (values-checked (integer? real? char?) 9 83.12 #\a)
            (test-equal "spec example: three checked values"
                        '(9 83.12 #\a)
                        (call-with-values
                          (lambda () (values-checked (integer? real? char?) 9 83.12 #\a))
                          list))
            ;; Spec example: (values-checked (integer?) 9.0) ;; 9 or 9.0
            (test-equal "spec example: integer? accepts 9.0" 9.0
                        (values-checked (integer?) 9.0))
            (test-error "spec example: string? rejects 9" #t
                        (values-checked (string?) 9))
            (test-error "later value failing raises" #t
                        (values-checked (integer? string?) 9 10))
            (test-equal "predicate expressions are evaluated" 5
                        (values-checked ((lambda (x) (and (integer? x) (> x 3)))) 5))
            (let ((calls 0))
              (values-checked (integer?) (begin (set! calls (1+ calls)) 9))
              (test-equal "value evaluated exactly once" 1 calls))
            ;; Failing slot reports values-checked as origin and the bad value.
            (let ((info (assertion-info (lambda () (values-checked (integer? string?) 9 10)))))
              (test-equal "error names values-checked as origin" 'values-checked
                          (car info))
              (test-assert "error irritants carry the bad value"
                           (member 10 (caddr info)))))

;;;
;;; 3. check-case
;;;

(test-group "srfi-253-check-case"
            ;; Spec example: integer branch
            (test-equal "spec example: integer branch" 5
                        (let ((x 4))
                          (check-case x
                                      (integer? (+ 1 x))
                                      (string? (+ 1 (string->number x))))))
            ;; Spec example: string branch
            (test-equal "spec example: string branch" 4
                        (let ((x "3"))
                          (check-case x
                                      (integer? (+ 1 x))
                                      (string? (+ 1 (string->number x))))))
            (test-equal "first matching clause wins" 'first
                        (check-case 1 (integer? 'first) (number? 'second)))
            (test-equal "clause body may be several expressions" 20
                        (check-case 1 (integer? 10 20)))
            (test-equal "else clause evaluated on no match" 'fallback
                        (check-case 'whatever (integer? 1) (string? 2) (else 'fallback)))
            (test-error "no match without else is an error" #t
                        (check-case 'whatever (integer? 1) (string? 2)))
            (let ((info (assertion-info
                          (lambda () (check-case 'whatever (integer? 1) (string? 2))))))
              (test-equal "no-match error names check-case as origin" 'check-case
                          (car info))
              (test-equal "no-match error lists the failed checks"
                          '((integer? v) (string? v))
                          (car (caddr info))))
            (let ((calls 0))
              (check-case (begin (set! calls (1+ calls)) 3) (integer? 'ok))
              (test-equal "value evaluated exactly once" 1 calls)))

;;;
;;; 4. lambda-checked
;;;

(test-group "srfi-253-lambda-checked"
            (test-equal "checked arguments"
                        4 ((lambda-checked ((x integer?) (s string?)) (+ x (string-length s)))
                           2 "ab"))
            (test-equal "mixed checked and unchecked arguments"
                        '(1 "a") ((lambda-checked (x (s string?)) (list x s)) 1 "a"))
            (test-equal "plain arguments behave like lambda"
                        6 ((lambda-checked (x) (* 2 x)) 3))
            (test-equal "empty argument list"
                        'ok ((lambda-checked () 'ok)))
            (test-error "checked argument failure raises" #t
                        ((lambda-checked ((x integer?)) x) "no"))
            (test-equal "error names lambda-checked as origin" 'lambda-checked
                        (car (assertion-info
                               (lambda () ((lambda-checked ((x integer?)) x) "no")))))
            (test-error "arity is preserved" #t
                        ((lambda-checked ((x integer?)) x) 1 2))
            (test-equal "rest argument collects extras"
                        '(a "b" (1 2))
                        ((lambda-checked ((w symbol?) (m string?) . rest) (list w m rest))
                         'a "b" 1 2))
            (test-equal "rest argument may be empty"
                        '(a ()) ((lambda-checked ((w symbol?) . rest) (list w rest)) 'a))
            (test-equal "unchecked rest argument"
                        '(1 (2 3)) ((lambda-checked (x . rest) (list x rest)) 1 2 3))
            (test-equal "argument-to-list lambda" 3
                        ((lambda-checked args (length args)) 1 2 3))
            ;; => return-value annotation (reference implementation extension)
            (test-equal "=> checks return value" "4"
                        ((lambda-checked ((x integer?)) => (string?) (number->string x)) 4))
            (test-error "=> rejects bad return value" #t
                        ((lambda-checked ((x integer?)) => (string?) (+ x 1)) 4))
            (test-equal "=> with no arguments" 41
                        ((lambda-checked () => (integer?) 41)))
            (test-equal "=> with argument-to-list lambda" 2
                        ((lambda-checked args => (integer?) (length args)) 1 2)))

;;;
;;; 5. case-lambda-checked
;;;
;;; A clause is ((arg ...) [=> (predicate ...)] body ...); checked
;;; arguments are written (name predicate) elements of the arg list.
;;;

(test-group "srfi-253-case-lambda-checked"
            (define counted-add
                    (case-lambda-checked
                      (() 'none)
                      (((x integer?)) x)
                      (((x integer?) (y integer?)) (+ x y))))
            (test-equal "zero-argument clause" 'none (counted-add))
            (test-equal "one-argument clause" 3 (counted-add 3))
            (test-equal "two-argument clause" 3 (counted-add 1 2))
            (test-error "argument check failure raises" #t (counted-add "a"))
            (test-error "second argument check failure raises" #t
                        (counted-add 1 "b"))
            (test-error "arity mismatch raises" #t (counted-add 1 2 3))
            (test-equal "error names case-lambda-checked as origin"
                        'case-lambda-checked
                        (car (assertion-info (lambda () (counted-add "a")))))
            (define mixed-dispatch
                    (case-lambda-checked
                      ((a (b string?)) (list a b))))
            (test-equal "mixed checked and unchecked clause arguments"
                        '(1 "z") (mixed-dispatch 1 "z"))
            (define cons-rest
                    (case-lambda-checked (((a symbol?) . rest) (cons a rest))))
            (test-equal "rest argument in a clause" '(x 1 2) (cons-rest 'x 1 2))
            (test-equal "empty rest argument in a clause" '(x) (cons-rest 'x))
            ;; => return-value annotations on clauses
            (define stringify
                    (case-lambda-checked
                      (() => (string?) "none")
                      (((x integer?)) => (string?) (number->string x))))
            (test-equal "=> on zero-argument clause" "none" (stringify))
            (test-equal "=> on one-argument clause" "5" (stringify 5))
            (test-error "=> rejects bad clause return" #t
                        ((case-lambda-checked (((x integer?)) => (string?) (+ x 1))) 4))
            (define count-all
                    (case-lambda-checked
                      (() => (integer?) 0)
                      (all => (integer?) (length all))))
            (test-equal "=> with argument-to-list clause, zero args" 0 (count-all))
            (test-equal "=> with argument-to-list clause" 3 (count-all 1 2 3)))

;;;
;;; 6. define-checked
;;;

(define-checked (dc-add (a integer?) (b integer?)) (+ a b))
(define-checked (dc-tag (name string?) tag)
                (cons tag name))
;; Spec example shape (renamed to avoid shadowing core `error'):
;; (define-checked (error (who symbol?) (message string?) . irritants) ...)
(define-checked (dc-error (who symbol?) (message string?) . irritants)
                (list who message irritants))
;; Spec example: (define-checked message string? "Hi!")
(define-checked message string? "Hi!")

(test-group "srfi-253-define-checked-procedure"
            (test-equal "procedure form works" 3 (dc-add 1 2))
            (test-equal "plain arguments are not checked" '(sym . "Bob")
                        (dc-tag "Bob" 'sym))
            (test-error "procedure argument failure raises" #t (dc-add "x" 2))
            (test-equal "error names the defined procedure" 'dc-add
                        (car (assertion-info (lambda () (dc-add "x" 2)))))
            (test-error "arity is preserved" #t (dc-add 1 2 3))
            (test-equal "rest arguments supported" '(x "m" (1 2))
                        (dc-error 'x "m" 1 2)))

(test-group "srfi-253-define-checked-variable"
            (test-equal "spec example: checked variable" "Hi!" message)
            (test-error "bad initial value raises at definition time" #t
                        (eval '(define-checked dc-bad-variable string? 42)
                              (current-module))))

;;;
;;; 7. define-record-type-checked
;;;

;; Poor person's union type, as in the SRFI's example.scm.
(define (disjoin . predicates)
        (lambda (x)
          (let rec ((predicates predicates))
                   (cond
                     ((null? predicates) #f)
                     (((car predicates) x) #t)
                     (else (rec (cdr predicates)))))))

(define-record-type-checked <ini-entry>
                            (make-ini-entry key value)
                            ini-entry?
                            (key string? ini-entry-key)
                            (value (disjoin number? string?) ini-entry-value set-ini-entry-value!))

;; Second record type in the same module: internal accessors of
;; distinct record types must not collide.
(define-record-type-checked <point>
                            (make-point x y)
                            point?
                            (x integer? point-x)
                            (y integer? point-y))

(test-group "srfi-253-define-record-type-checked"
            (define entry (make-ini-entry "a" 1))
            (test-assert "constructor accepts good values" (ini-entry? entry))
            (test-equal "field accessor returns field value" "a" (ini-entry-key entry))
            (test-equal "compound predicate accepts number value" 1
                        (ini-entry-value entry))
            (test-equal "compound predicate accepts string value" "b"
                        (ini-entry-value (make-ini-entry "a" "b")))
            (test-error "constructor rejects bad key" #t (make-ini-entry 'a 1))
            (test-error "constructor rejects bad value" #t (make-ini-entry "a" 'v))
            (test-error "accessor rejects non-record argument" #t
                        (ini-entry-key "not-a-record"))
            (test-error "accessor rejects non-record argument, value field" #t
                        (ini-entry-value 42))
            (test-equal "modifier accepts good new value" 2
                        (let ((e (make-ini-entry "a" 1)))
                          (set-ini-entry-value! e 2)
                          (ini-entry-value e)))
            (test-equal "modifier accepts string new value" "b"
                        (let ((e (make-ini-entry "a" 1)))
                          (set-ini-entry-value! e "b")
                          (ini-entry-value e)))
            (test-error "modifier rejects bad new value" #t
                        (set-ini-entry-value! (make-ini-entry "a" 1) 'sym))
            (test-error "modifier rejects non-record argument" #t
                        (set-ini-entry-value! "not-a-record" 1)))

;; The predicate of a field can stop holding between construction and
;; access: the accessor must check its RETURN value, not just its
;; record argument (spec: "accessor and modifier return checked data").
(define rt-flag #t)
(define-record-type-checked <flagged>
                            (make-flagged v)
                            flagged?
                            (v (lambda (x) rt-flag) flagged-v))

(test-group "srfi-253-record-accessor-return-check"
            (define f (make-flagged 1))
            (test-equal "accessor returns value while predicate holds" 1 (flagged-v f))
            (set! rt-flag #f)
            (test-error "accessor return check fails when predicate stops holding" #t
                        (flagged-v f))
            (set! rt-flag #t)
            (test-equal "accessor works again once predicate holds" 1 (flagged-v f)))

(test-group "srfi-253-record-second-type"
            (test-equal "second record type independent accessors" '(1 2)
                        (list (point-x (make-point 1 2)) (point-y (make-point 1 2))))
            (test-equal "first record type still intact" "k"
                        (ini-entry-key (make-ini-entry "k" 9)))
            (test-error "second record type constructor checks fields" #t
                        (make-point 1 "y"))
            (test-error "second record type immutable field has no modifier" #t
                        (eval '(set-point-x! (make-point 1 2) 3) (current-module))))

(spec-end)
(spec-exit)
