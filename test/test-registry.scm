;;; test-registry.scm — Test suite for (badb registry)

(define-module (test test-registry)
  #:use-module (srfi srfi-64)
  #:use-module (badb kernel registry)
  #:use-module (ice-9 threads))

(test-begin "badb-registry")

;;;
;;; Registry Creation and Predicate
;;;

(test-group "registry-creation"
  (let ((reg (make-registry)))
    (test-assert "make-registry returns a registry" (registry? reg))
    (test-assert "non-registry values fail registry?" (not (registry? #f)))
    (test-assert "symbol fails registry?" (not (registry? 'foo)))
    (test-assert "string fails registry?" (not (registry? "registry")))
    (test-assert "number fails registry?" (not (registry? 42)))
    (test-assert "list fails registry?" (not (registry? '())))))

;;;
;;; Attachment, Lookup, and Detachment
;;;

(test-group "registry-attach-lookup-detach"
  (let ((reg (make-registry))
        (proc (lambda (x) (+ x 1))))
    ;; Attach with symbol
    (registry-attach! reg 'increment proc)
    (test-eq "lookup by symbol returns attached procedure" proc (registry-lookup reg 'increment))
    (test-eq "lookup by string returns same procedure" proc (registry-lookup reg "increment"))

    ;; Lookup non-existent
    (test-assert "lookup non-existent returns #f" (not (registry-lookup reg 'non-existent)))

    ;; Attach non-procedure (e.g. data or module)
    (registry-attach! reg "config" '((debug . #t)))
    (test-equal "lookup attached non-procedure value" '((debug . #t)) (registry-lookup reg 'config))

    ;; Detach
    (test-assert "detach existing feature returns #t" (registry-detach! reg 'increment))
    (test-assert "lookup after detach returns #f" (not (registry-lookup reg 'increment)))
    (test-assert "detach non-existent returns #f" (not (registry-detach! reg 'non-existent)))
    (test-assert "detach by string" (registry-detach! reg "config"))
    (test-assert "lookup detached config returns #f" (not (registry-lookup reg "config")))))

;;;
;;; Hot-swapping and Dynamic Dispatch
;;;

(test-group "registry-hot-swapping-and-dispatch"
  (let ((reg (make-registry))
        (v1 (lambda (x) (* x 2)))
        (v2 (lambda (x) (* x 10))))
    ;; Attach v1
    (registry-attach! reg 'scaler v1)
    (test-equal "dispatch calls v1" 20 (registry-dispatch reg 'scaler 10))
    (test-equal "dispatch by string calls v1" 14 (registry-dispatch reg "scaler" 7))

    ;; Hot-swap with v2 in-place
    (registry-attach! reg 'scaler v2)
    (test-equal "dispatch now calls v2 without restarting" 100 (registry-dispatch reg 'scaler 10))
    (test-equal "dispatch by string now calls v2" 70 (registry-dispatch reg "scaler" 7))

    ;; Multi-argument dispatch
    (registry-attach! reg 'sum (lambda (a b c) (+ a b c)))
    (test-equal "multi-arg dispatch works" 60 (registry-dispatch reg 'sum 10 20 30))

    ;; Error handling on dispatch
    (test-error "dispatch on missing feature raises error" #t
      (registry-dispatch reg 'missing-feature 1 2 3))

    (registry-attach! reg 'not-a-proc "static-string")
    (test-error "dispatch on non-procedure raises error" #t
      (registry-dispatch reg 'not-a-proc 1 2 3))))

;;;
;;; Metadata and Registry List
;;;

(test-group "registry-metadata-and-list"
  (let ((reg (make-registry)))
    ;; Empty registry listing
    (test-equal "empty registry has empty list" '() (registry-list reg))

    ;; Attach with metadata keyword
    (registry-attach! reg 'math-add (lambda (a b) (+ a b))
                      #:metadata '((version . "1.0.0") (doc . "Adds two numbers")))
    (test-equal "lookup metadata by symbol"
      '((version . "1.0.0") (doc . "Adds two numbers")) (registry-metadata reg 'math-add))
    (test-equal "lookup metadata by string"
      '((version . "1.0.0") (doc . "Adds two numbers")) (registry-metadata reg "math-add"))

    ;; Attach without metadata
    (registry-attach! reg "logger" (lambda (msg) msg))
    (test-equal "default metadata is empty list" '() (registry-metadata reg 'logger))

    ;; Update metadata on hot-swap
    (registry-attach! reg 'math-add (lambda (a b) (+ a b))
                      #:metadata '((version . "2.0.0") (doc . "Updated add")))
    (test-equal "updated metadata reflected"
      '((version . "2.0.0") (doc . "Updated add")) (registry-metadata reg 'math-add))

    ;; registry-list returns sorted list of ((id . <id>) (metadata . <metadata>))
    (let ((entries (registry-list reg)))
      (test-equal "registry-list contains 2 entries" 2 (length entries))
      ;; Sorted alphabetically: "logger" before "math-add"
      (test-equal "first entry id is logger" "logger" (assoc-ref (car entries) 'id))
      (test-equal "first entry metadata is empty" '() (assoc-ref (car entries) 'metadata))
      (test-equal "second entry id is math-add" 'math-add (assoc-ref (cadr entries) 'id))
      (test-equal "second entry metadata is version 2.0.0"
        '((version . "2.0.0") (doc . "Updated add")) (assoc-ref (cadr entries) 'metadata)))))

;;;
;;; Fresh Feature Module Creation and Isolation
;;;

(test-group "fresh-feature-modules"
  ;; Anonymous module
  (let ((mod (make-fresh-feature-module)))
    (eval-in-feature-module mod '(define (cube x) (* x x x)))
    (test-equal "eval-in-feature-module calls defined function" 27 (eval-in-feature-module mod '(cube 3)))

    ;; Define and ref directly
    (feature-module-define! mod 'token "secret-abc")
    (test-equal "feature-module-ref gets defined value" "secret-abc" (feature-module-ref mod 'token))
    (test-equal "feature-module-ref returns default when unbound"
      #f (feature-module-ref mod 'unknown-symbol #f)))

  ;; Named module with imports
  (let ((mod (make-fresh-feature-module 'badb-feature-tools
                                        #:imports '((guile) (srfi srfi-1)))))
    (eval-in-feature-module mod '(define (take-two lst) (take lst 2)))
    (test-equal "srfi-1 import works in feature module"
      '(a b) (eval-in-feature-module mod '(take-two '(a b c d)))))

  ;; Module isolation: variables do not leak into (current-module)
  (let ((mod (make-fresh-feature-module 'isolated-test-mod)))
    (eval-in-feature-module mod '(define %strictly-isolated-badb-token% 987654))
    (test-assert "variable in feature module is not defined in current-module"
      (not (defined? '%strictly-isolated-badb-token%)))
    (test-equal "variable is accessible inside feature module"
      987654 (feature-module-ref mod '%strictly-isolated-badb-token%)))

  ;; Two fresh modules are isolated from each other
  (let ((mod-a (make-fresh-feature-module 'mod-a))
        (mod-b (make-fresh-feature-module 'mod-b)))
    (eval-in-feature-module mod-a '(define shared-name "module-a-value"))
    (eval-in-feature-module mod-b '(define shared-name "module-b-value"))
    (test-equal "mod-a has its own value" "module-a-value" (feature-module-ref mod-a 'shared-name))
    (test-equal "mod-b has its own value" "module-b-value" (feature-module-ref mod-b 'shared-name))))

;;;
;;; End-to-end Feature Module to Registry Hot-Swap Integration
;;;

(test-group "end-to-end-module-registry-hotswap"
  (let ((reg (make-registry))
        ;; Module v1
        (mod-v1 (make-fresh-feature-module 'calc-v1)))
    (eval-in-feature-module mod-v1
      '(define (calculate x y) (+ x y)))
    (registry-attach! reg 'calculator
                      (feature-module-ref mod-v1 'calculate)
                      #:metadata '((version . 1) (op . "+")))

    (test-equal "v1 calculates sum" 15 (registry-dispatch reg 'calculator 10 5))
    (test-equal "v1 metadata" '((version . 1) (op . "+")) (registry-metadata reg 'calculator))

    ;; Module v2: dynamic hot-swap to multiplication
    (let ((mod-v2 (make-fresh-feature-module 'calc-v2)))
      (eval-in-feature-module mod-v2
        '(define (calculate x y) (* x y)))
      (registry-attach! reg 'calculator
                        (feature-module-ref mod-v2 'calculate)
                        #:metadata '((version . 2) (op . "*")))

      (test-equal "v2 hot-swap calculates product" 50 (registry-dispatch reg 'calculator 10 5))
      (test-equal "v2 metadata updated" '((version . 2) (op . "*")) (registry-metadata reg 'calculator)))))

;;;
;;; Thread Safety Test
;;;

(test-group "registry-concurrency"
  (let ((reg (make-registry))
        (num-threads 8)
        (ops-per-thread 25)
        (errors '())
        (err-mutex (make-mutex)))
    (define (record-error! err)
      (with-mutex err-mutex
        (set! errors (cons err errors))))
    ;; Spawn multiple threads reading and writing to registry concurrently.
    ;; Note: Worker threads record failures into `errors` rather than invoking
    ;; SRFI-64 runner concurrently, as the runner's internal state is single-threaded.
    (let ((threads
           (map (lambda (tid)
                  (call-with-new-thread
                   (lambda ()
                     (let loop ((i 0))
                       (when (< i ops-per-thread)
                         (catch #t
                           (lambda ()
                             (let ((id (string->symbol (format #f "feat-~a-~a" tid i))))
                               (registry-attach! reg id (lambda () i)
                                                 #:metadata `((thread . ,tid) (iteration . ,i)))
                               (unless (procedure? (registry-lookup reg id))
                                 (record-error! (list 'lookup-failed tid i)))
                               (unless (= (registry-dispatch reg id) i)
                                 (record-error! (list 'dispatch-failed tid i)))))
                           (lambda (k . a)
                             (record-error! (list 'exception tid i k a))))
                         (loop (+ i 1)))))))
                (iota num-threads))))
      ;; Join all threads
      (for-each join-thread threads)
      (test-equal "concurrency produced no errors" '() errors)
      (test-equal "all concurrent registrations present"
        (* num-threads ops-per-thread) (length (registry-list reg))))))

(test-end "badb-registry")
