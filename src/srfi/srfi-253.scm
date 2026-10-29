;;; SRFI 253: Data Checking and Coercion, vendored for Guile.
;;;
;;; Guile 3.0 ships srfi-64 but not srfi-253 (nor srfi-252), so this
;;; module vendors the finalized SRFI 253 specification
;;; (https://srfi.schemers.org/srfi-253/srfi-253.html) as
;;; (srfi srfi-253).  With "guile -L src" the src/ tree precedes the
;;; system load path; Guile has no built-in srfi-253 to shadow.
;;;
;;; The implementation follows the SRFI's reference implementation by
;;; Artyom Bologov (MIT license), keeping its portable syntax-rules
;;; core and dropping cond-expand branches for non-Guile Schemes.
;;; Divergences from the reference implementation:
;;;
;;;   - Checks are unconditional: every "it is an error" case of the
;;;     spec raises an R6RS &assertion condition through
;;;     `assertion-violation', as the spec recommends ("at least in
;;;     debug mode").  The reference implementation can compile
;;;     checks away via SRFI 145 assume.
;;;   - Record accessors check their return values against the field
;;;     predicates, per the specification text ("These ensure that
;;;     accessor and modifier return checked data and check new data
;;;     respectively"); the reference implementation checks only the
;;;     record argument.
;;;   - The reference implementation's => return-value annotation for
;;;     lambda-checked/case-lambda-checked is kept; it is an
;;;     extension of the sample implementation, not the SRFI text.
;;;
;;; define-record-type-checked assumes the constructor takes the
;;; fields in declaration order, as does the reference
;;; implementation.

(define-module (srfi srfi-253)
  #:use-module (srfi srfi-9)
  #:use-module ((rnrs base) #:select (assertion-violation))
  #:export (check-arg
            values-checked
            check-case
            lambda-checked
            case-lambda-checked
            define-checked
            define-record-type-checked))

;;;
;;; check-arg
;;;
;;; Guarantees that VAL (evaluated) conforms to PRED (evaluated).
;;; CALLER (evaluated) names the error origin.  Returns VAL on
;;; success; raises &assertion otherwise.  Defined as a macro so the
;;; failing form can be quoted in the error irritants, like the
;;; reference implementation (the spec allows a procedure as well).
;;;

(define-syntax check-arg
  (syntax-rules ()
    ((_ pred val caller)
     (let ((v val))
       (if (pred v)
           v
           (assertion-violation
            caller
            "argument should match the specification"
            '(pred val)
            v))))
    ((_ pred val)
     (check-arg pred val 'check-arg))))

;;;
;;; values-checked
;;;
;;; Checks each value (evaluated) against the corresponding
;;; predicate (evaluated) and returns the values.  The single-value
;;; form keeps its parentheses so complex predicate expressions stay
;;; unambiguous.  The multi-value form re-enters the single form per
;;; value and delivers the results through `values'.
;;;

(define-syntax values-checked
  (syntax-rules ()
    ((_ (predicate) value)
     (let ((v value))
       (check-arg predicate v 'values-checked)
       v))
    ((_ (predicate ...) value ...)
     (values (values-checked (predicate) value) ...))))

;;;
;;; check-case
;;;
;;; Checks the value (evaluated once) against each clause predicate
;;; (evaluated in order) and evaluates the first matching clause's
;;; body.  An `(else body ...)' clause handles the no-match case;
;;; without one, a no-match raises &assertion.
;;;

(define-syntax %check-case
  (syntax-rules (else)
    ((_ val (clause ...) (else body ...))
     (cond
      clause ...
      (else body ...)))
    ((_ val ((clause-check clause-body ...) ...))
     (cond
      (clause-check clause-body ...)
      ...
      (else
       (assertion-violation
        'check-case
        "at least one branch of check-case should be true"
        '(clause-check ...)))))
    ((_ val (clause ...) (pred body ...) rest ...)
     (%check-case
      val
      (clause ... ((pred val) body ...))
      rest ...))))

(define-syntax check-case
  (syntax-rules ()
    ((_ value clause ...)
     (let ((v value))
       (%check-case v () clause ...)))))

;;;
;;; lambda-checked
;;;
;;; Lambda with arguments of the form (name predicate), checked on
;;; application.  Plain symbol arguments pass through unchecked.
;;; Rest arguments are supported on Guile.  The optional
;;; `=> (predicate ...)' annotation checks the return values.
;;;

(define-syntax %lambda-checked
  (syntax-rules (=>)
    ;; Terminating clauses, with and without return checks
    ((_ name (=> (returns ...) body ...) args (checks ...))
     (lambda args
       checks ...
       (values-checked
        (returns ...)
        (begin body ...))))
    ((_ name (body ...) args (checks ...))
     (lambda args
       checks ...
       body ...))
    ;; (arg pred) argument: insert a new check
    ((_ name body (args ...) (checks ...) (arg pred) . rest)
     (%lambda-checked
      name body
      (args ... arg) (checks ... (check-arg pred arg 'name)) . rest))
    ;; Regular symbol arg: no check
    ((_ name body (args ...) (checks ...) arg . rest)
     (%lambda-checked
      name body
      (args ... arg) (checks ...) . rest))
    ;; Rest argument
    ((_ name body (args ...) (checks ...) . last)
     (%lambda-checked
      name body
      (args ... . last) (checks ...)))))

(define-syntax lambda-checked
  (syntax-rules (=>)
    ;; Empty args with return type checking
    ((_ () => (returns ...) body ...)
     (lambda ()
       (values-checked (returns ...) (begin body ...))))
    ;; No need to parse as there are no args
    ((_ () body ...)
     (lambda () body ...))
    ;; Regular (positional ... [. rest]) arglist
    ((_ (arg . args) body ...)
     (%lambda-checked lambda-checked (body ...) () () arg . args))
    ;; Arg->list lambda with return checking
    ((_ arg => (returns ...) body ...)
     (lambda arg
       (values-checked (returns ...) (begin body ...))))
    ;; Arg->list lambda, no checks possible
    ((_ arg body ...)
     (lambda arg body ...))))

;;;
;;; case-lambda-checked
;;;
;;; case-lambda over the same (name predicate) argument syntax.
;;; Dispatch is on argument count only; checks run per clause.  The
;;; optional `=> (predicate ...)' clause annotation checks the
;;; clause's return values.
;;;

(define-syntax %case-lambda-checked
  (syntax-rules (=>)
    ;; Terminal case, generate the actual case-lambda
    ((_ (clauses-so-far ...)
        ()
        args-so-far (checks-so-far ...) (body ...))
     (case-lambda
       clauses-so-far ...
       (args-so-far
        checks-so-far ...
        body ...)))
    ;; Empty args with returns
    ((_ (clauses-so-far ...)
        ((() => (returns ...) body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      () () ((values-checked (returns ...) (begin body-to-process ...)))))
    ;; Empty args without returns
    ((_ (clauses-so-far ...)
        ((() body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      () () (body-to-process ...)))
    ;; Regular args with returns
    ((_ (clauses-so-far ...)
        (((arg . args-to-process) => (returns ...) body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      () () ((values-checked (returns ...) (begin body-to-process ...)))
      arg . args-to-process))
    ;; Regular args without returns
    ((_ (clauses-so-far ...)
        (((arg . args-to-process) body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      () () (body-to-process ...)
      arg . args-to-process))
    ;; Rest arg with returns
    ((_ (clauses-so-far ...)
        ((arg-to-process => (returns ...) body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      arg-to-process () ((values-checked (returns ...) (begin body-to-process ...)))))
    ;; Rest arg without returns
    ((_ (clauses-so-far ...)
        ((arg-to-process body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      arg-to-process () (body-to-process ...)))
    ;; Consume arg with predicate / check
    ((_ (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ...) (checks-so-far ...) (body ...) (arg pred) . args)
     (%case-lambda-checked
      (clauses-so-far ...) (clauses-to-process ...)
      (args-so-far ... arg)
      (checks-so-far ... (check-arg pred arg 'case-lambda-checked))
      (body ...) . args))
    ;; Consume regular arg
    ((_ (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ...) (checks-so-far ...) (body ...) arg . args)
     (%case-lambda-checked
      (clauses-so-far ...) (clauses-to-process ...)
      (args-so-far ... arg) (checks-so-far ...) (body ...) . args))
    ;; Consume rest arg
    ((_ (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ...) (checks-so-far ...) (body ...) . arg)
     (%case-lambda-checked
      (clauses-so-far ...) (clauses-to-process ...)
      (args-so-far ... . arg) (checks-so-far ...) (body ...)))))

(define-syntax case-lambda-checked
  (syntax-rules (=>)
    ((_ (() => (returns ...) first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) () ()
                           ((values-checked (returns ...) (begin first-body ...)))))
    ((_ (() first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) () () (first-body ...)))
    ((_ ((first-arg . first-args) => (returns ...) first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) () ()
                           ((values-checked (returns ...) (begin first-body ...)))
                           first-arg . first-args))
    ((_ ((first-arg . first-args) first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) () () (first-body ...)
                           first-arg . first-args))
    ((_ (args-var => (returns ...) first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) args-var ()
                           ((values-checked (returns ...) (begin first-body ...)))))
    ((_ (args-var first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) args-var () (first-body ...)))))

;;;
;;; define-checked
;;;
;;; Procedure form: define + lambda-checked.  Variable form: define a
;;; variable whose initial value is checked against the predicate.
;;;

(define-syntax define-checked
  (syntax-rules ()
    ;; Procedure
    ((_ (name . args) body ...)
     (define name (%lambda-checked name (body ...) () () . args)))
    ;; Variable
    ((_ name pred value)
     (define name (values-checked (pred) value)))))

;;;
;;; define-record-type-checked
;;;
;;; Record type with checked constructor, accessors, and modifiers.
;;; Constructor arguments must be plain symbols (checked in field
;;; order); fields are (field-name predicate accessor-name) or
;;; (field-name predicate accessor-name modifier-name).  Accessors
;;; check the record argument and their return value; modifiers check
;;; the record argument and the new value.
;;;
;;; The internal constructor/accessor/modifier names are hygienic
;;; introduced identifiers, distinct per field thanks to per-template
;;; instantiation renaming.
;;;

(define-syntax %define-record-type-checked
  (syntax-rules ()
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...))
     (begin
       (define-record-type
           type-name constructor predicate
           fields ...)
       field-wrappers ...))
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...) (field pred accessor modifier)
        fields-to-process ...)
     (%define-record-type-checked
      type-name constructor predicate
      (fields ... (field internal-accessor internal-modifier))
      (field-wrappers
       ...
       (define (accessor record)
         (check-arg predicate record 'type-name)
         (values-checked (pred) (internal-accessor record)))
       (define (modifier record new)
         (check-arg predicate record 'type-name)
         (check-arg pred new 'type-name)
         (internal-modifier record new)))
      fields-to-process ...))
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...) (field pred accessor)
        fields-to-process ...)
     (%define-record-type-checked
      type-name constructor predicate
      (fields ... (field internal-accessor))
      (field-wrappers
       ...
       (define (accessor record)
         (check-arg predicate record 'type-name)
         (values-checked (pred) (internal-accessor record))))
      fields-to-process ...))))

(define-syntax %wrap-constructor
  (syntax-rules ()
    ((_ constructor internal-constructor (arg-names ...) (args ...))
     (define-checked (constructor args ...)
       (internal-constructor arg-names ...)))
    ((_ constructor internal-constructor (arg-names ...) (args ...)
        (name pred rest ...) fields-to-process ...)
     (%wrap-constructor
      constructor internal-constructor
      (arg-names ... name) (args ... (name pred))
      fields-to-process ...))))

(define-syntax define-record-type-checked
  (syntax-rules ()
    ((_ type-name (constructor constructor-args ...) predicate field ...)
     (begin
       (%define-record-type-checked
        type-name
        (internal-constructor constructor-args ...)
        predicate
        () () field ...)
       (%wrap-constructor constructor internal-constructor () () field ...)))))
