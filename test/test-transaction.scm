;;; test-transaction.scm — Comprehensive Test Suite for (badb transaction)

(define-module (test test-transaction)
  #:use-module (srfi srfi-64)
  #:use-module (badb registry)
  #:use-module (badb transaction)
  #:use-module (badb journal))

(test-begin "badb-transaction")

;;;
;;; 1. Transaction Manager Record Construction & Predicates
;;;

(test-group "transaction-manager-basics"
  (let* ((reg (make-registry))
         (mgr-no-journal (make-transaction-manager reg))
         (mgr-kw-journal (make-transaction-manager reg #:journal #f))
         (mgr-mock-journal (make-transaction-manager reg #:journal 'mock-journal))
         (mgr-pos-journal (make-transaction-manager reg 'pos-journal)))

    (test-assert "transaction-manager? returns #t for manager instance"
      (transaction-manager? mgr-no-journal))
    (test-assert "transaction-manager? returns #f for registry"
      (not (transaction-manager? reg)))
    (test-assert "transaction-manager? returns #f for symbols and numbers"
      (and (not (transaction-manager? 'mgr))
           (not (transaction-manager? 12345))
           (not (transaction-manager? #f))))

    (test-eq "transaction-manager-registry returns configured registry"
      reg
      (transaction-manager-registry mgr-no-journal))

    (test-assert "transaction-manager-journal is #f when omitted"
      (not (transaction-manager-journal mgr-no-journal)))

    (test-assert "transaction-manager-journal is #f with explicit #:journal #f"
      (not (transaction-manager-journal mgr-kw-journal)))

    (test-eq "transaction-manager-journal returns configured journal (keyword)"
      'mock-journal
      (transaction-manager-journal mgr-mock-journal))

    (test-eq "transaction-manager-journal returns configured journal (positional)"
      'pos-journal
      (transaction-manager-journal mgr-pos-journal))))

;;;
;;; 2. Sandbox Staging & Module Isolation (stage-patch & eval-patch)
;;;

(test-group "sandbox-staging-and-eval-patch"
  ;; Fresh module eval-patch
  (let* ((mod (eval-patch '((define (square x) (* x x))
                            (define constant-val 42))))
         (sq (feature-module-ref mod 'square))
         (val (feature-module-ref mod 'constant-val)))
    (test-assert "eval-patch returns a valid module"
      (module? mod))
    (test-assert "eval-patch evaluates defined procedures"
      (procedure? sq))
    (test-equal "eval-patch procedure executes correctly"
      16
      (sq 4))
    (test-equal "eval-patch defines top-level values"
      42
      val))

  ;; eval-patch with string forms
  (let* ((mod (eval-patch "(define (cube x) (* x x x))"))
         (cb (feature-module-ref mod 'cube)))
    (test-assert "eval-patch parses string forms correctly"
      (procedure? cb))
    (test-equal "eval-patch string-defined procedure works"
      27
      (cb 3)))

  ;; stage-patch inheritance from base-module without mutating base
  (let* ((base (eval-patch '((define (greeting) "Hello")
                             (define (farewell) "Goodbye"))))
         (child (stage-patch base '((define (greeting) "Hi there")
                                    (define (extra) "Extra content")))))
    (test-assert "child module inherits unmodified bindings from base"
      (string=? "Goodbye" ((feature-module-ref child 'farewell))))
    (test-assert "child module overrides shadowed bindings"
      (string=? "Hi there" ((feature-module-ref child 'greeting))))
    (test-assert "child module adds new bindings"
      (string=? "Extra content" ((feature-module-ref child 'extra))))
    (test-assert "base module remains completely untouched by child redefinition"
      (string=? "Hello" ((feature-module-ref base 'greeting))))
    (test-assert "base module does not contain child-only new bindings"
      (not (feature-module-ref base 'extra #f))))

  ;; eval-patch with #:base-module keyword
  (let* ((base (eval-patch '((define num 100))))
         (child (eval-patch '((define (get-doubled) (* 2 num)))
                            #:base-module base)))
    (test-equal "eval-patch with #:base-module accesses base bindings"
      200
      ((feature-module-ref child 'get-doubled)))))

;;;
;;; 3. Successful Transaction Commit
;;;

(test-group "transaction-commit-success"
  (let* ((reg (make-registry))
         (mgr (make-transaction-manager reg))
         (result (apply-transaction! mgr 'calculator
                                     '((define (calculator op a b)
                                         (case op
                                           ((add) (+ a b))
                                           ((mul) (* a b))
                                           (else #f))))
                                     (lambda (mod)
                                       (let ((calc (feature-module-ref mod 'calculator)))
                                         (and calc
                                              (= (calc 'add 10 20) 30)
                                              (= (calc 'mul 3 4) 12))))
                                     #:metadata '((version . "1.0.0")
                                                  (author . "test-agent")))))

    (test-equal "commit returns committed status"
      "committed"
      (assoc-ref result 'status))

    (test-eq "commit returns feature-id"
      'calculator
      (assoc-ref result 'feature-id))

    ;; Registry lookup verification
    (let ((calc (registry-lookup reg 'calculator)))
      (test-assert "committed feature is stored in registry"
        (procedure? calc))
      (test-equal "committed feature is callable and returns correct result"
        50
        (calc 'add 20 30)))

    ;; Registry dispatch verification
    (test-equal "registry-dispatch invokes committed feature"
      15
      (registry-dispatch reg 'calculator 'mul 3 5))

    ;; Metadata verification
    (let ((meta (registry-metadata reg 'calculator)))
      (test-assert "registry metadata contains custom attributes"
        (and (equal? (assoc-ref meta 'version) "1.0.0")
             (equal? (assoc-ref meta 'author) "test-agent")))
      (test-assert "registry metadata stores reference to staged module"
        (module? (assoc-ref meta 'module))))))

;;;
;;; 4. Transaction Hot-Swap / Upgrade Commit
;;;

(test-group "transaction-hot-swap-upgrade"
  (let* ((reg (make-registry))
         (mgr (make-transaction-manager reg)))
    ;; Initial version
    (apply-transaction! mgr 'multiplier
                        '((define factor 2)
                          (define (multiplier x) (* x factor)))
                        (lambda (mod) #t)
                        #:metadata '((version . "1.0")))

    (test-equal "version 1 returns 20"
      20
      (registry-dispatch reg 'multiplier 10))

    ;; Upgrade version
    (let ((res (apply-transaction! mgr 'multiplier
                                   '((define factor 10)
                                     (define (multiplier x) (* x factor)))
                                   (lambda (mod)
                                     (= ((feature-module-ref mod 'multiplier) 5) 50))
                                   #:metadata '((version . "2.0")
                                                (updated . #t)))))

      (test-equal "upgrade transaction committed"
        "committed"
        (assoc-ref res 'status))

      (test-equal "version 2 returns 100"
        100
        (registry-dispatch reg 'multiplier 10))

      (let ((meta (registry-metadata reg 'multiplier)))
        (test-equal "version metadata updated"
          "2.0"
          (assoc-ref meta 'version))
        (test-assert "updated metadata flag present"
          (assoc-ref meta 'updated))))))

;;;
;;; 5. Rollback on Verification Failure (test-thunk returns #f)
;;;

(test-group "transaction-rollback-on-false"
  (let* ((reg (make-registry))
         (mgr (make-transaction-manager reg)))

    ;; Initial commit of working feature
    (apply-transaction! mgr 'safe-feature
                        '((define (safe-feature) "original"))
                        (lambda (m) #t))
    (test-equal "initial feature registered"
      "original"
      ((registry-lookup reg 'safe-feature)))

    ;; Attempt broken update where tests return #f
    (let ((result (apply-transaction! mgr 'safe-feature
                                      '((define (safe-feature) "broken-mutated"))
                                      (lambda (mod)
                                        ;; Test fails
                                        #f))))

      (test-equal "result status is reverted"
        "reverted"
        (assoc-ref result 'status))

      (test-assert "result contains error diagnostics"
        (string? (assoc-ref result 'error)))

      ;; Original registry state must be preserved
      (test-equal "registry implementation remains intact and callable"
        "original"
        ((registry-lookup reg 'safe-feature)))

      (test-equal "registry dispatch still returns original value"
        "original"
        (registry-dispatch reg 'safe-feature)))))

;;;
;;; 6. Rollback on Exception in Verification Thunk
;;;

(test-group "transaction-rollback-on-exception"
  (let* ((reg (make-registry))
         (mgr (make-transaction-manager reg)))

    ;; Setup original implementation
    (apply-transaction! mgr 'divider
                        '((define (divider x y) (/ x y)))
                        (lambda (m) #t)
                        #:metadata '((status . "stable")))

    (test-equal "original feature working"
      5
      ((registry-lookup reg 'divider) 10 2))

    ;; Attempt update that raises divide-by-zero exception in test thunk
    (let ((res (apply-transaction! mgr 'divider
                                   '((define (divider x y) (/ x 0)))
                                   (lambda (mod)
                                     ;; Deliberate exception during testing
                                     (/ 1 0)))))

      (test-equal "result status is reverted"
        "reverted"
        (assoc-ref res 'status))

      (test-assert "result error contains exception details"
        (string-contains (assoc-ref res 'error) "divide"))

      ;; Verify prior implementation remains callable and unchanged
      (test-equal "prior implementation still callable after exception rollback"
        4
        ((registry-lookup reg 'divider) 12 3))

      (test-equal "prior metadata untouched after exception rollback"
        "stable"
        (assoc-ref (registry-metadata reg 'divider) 'status)))))

;;;
;;; 7. Rollback on User Error / Assertion Failure
;;;

(test-group "transaction-rollback-on-assertion-error"
  (let* ((reg (make-registry))
         (mgr (make-transaction-manager reg)))

    ;; Setup base
    (apply-transaction! mgr 'echo
                        '((define (echo msg) msg))
                        (lambda (m) #t))

    ;; Attempt update that throws with (error ...)
    (let ((res (apply-transaction! mgr 'echo
                                   '((define (echo msg) "corrupted"))
                                   (lambda (mod)
                                     (error "Assertion failed: output mismatch" 99)))))

      (test-equal "result status is reverted"
        "reverted"
        (assoc-ref res 'status))

      (test-assert "error diagnostic captures error message"
        (string-contains (assoc-ref res 'error) "Assertion failed"))

      (test-equal "echo returns original message"
        "hello"
        ((registry-lookup reg 'echo) "hello")))))

;;;
;;; 8. Rollback on Staging Syntax or Runtime Error in Forms
;;;

(test-group "transaction-rollback-on-staging-error"
  (let* ((reg (make-registry))
         (mgr (make-transaction-manager reg)))

    (apply-transaction! mgr 'good-feat
                        '((define (good-feat) 100))
                        (lambda (m) #t))

    ;; Forms that fail at top-level evaluation during stage-patch
    (let ((res (apply-transaction! mgr 'good-feat
                                   '((define bad (/ 1 0)))
                                   (lambda (m) #t))))

      (test-equal "result status is reverted on staging failure"
        "reverted"
        (assoc-ref res 'status))

      (test-assert "error captures divide-by-zero from staging form"
        (string-contains (assoc-ref res 'error) "divide"))

      (test-equal "original feature preserved"
        100
        ((registry-lookup reg 'good-feat))))))

;;;
;;; 9. Checkpoint Journal Integration
;;;

(test-group "journal-integration"
  (let* ((j-path "/tmp/badb-test-transaction-journal.jsonl")
         (_ (when (file-exists? j-path) (delete-file j-path)))
         (journal (make-journal j-path))
         (reg (make-registry))
         (mgr (make-transaction-manager reg #:journal journal)))

    ;; Successful transaction
    (let ((res1 (apply-transaction! mgr 'feat-alpha
                                    '((define (feat-alpha) "alpha-v1"))
                                    (lambda (m) #t)
                                    #:metadata '((version . "1.0")))))
      (test-equal "feat-alpha committed"
        "committed"
        (assoc-ref res1 'status)))

    ;; Failed transaction with return #f
    (let ((res2 (apply-transaction! mgr 'feat-beta
                                    '((define (feat-beta) "beta-broken"))
                                    (lambda (m) #f))))
      (test-equal "feat-beta reverted"
        "reverted"
        (assoc-ref res2 'status)))

    ;; Failed transaction with exception
    (let ((res3 (apply-transaction! mgr 'feat-alpha
                                    '((define (feat-alpha) "alpha-v2"))
                                    (lambda (m) (error "validation failed")))))
      (test-equal "feat-alpha upgrade reverted"
        "reverted"
        (assoc-ref res3 'status)))

    ;; Inspect journal entries recorded
    (test-equal "journal recorded exactly 3 checkpoints"
      3
      (journal-count journal))

    (let* ((entries (journal-entries journal))
           (e1 (list-ref entries 0))
           (e2 (list-ref entries 1))
           (e3 (list-ref entries 2)))

      ;; Checkpoint 1: Commit pass
      (test-equal "entry 1 checkpoint is commit"
        "commit"
        (assoc-ref e1 "checkpoint"))
      (test-equal "entry 1 status is pass"
        "pass"
        (assoc-ref e1 "status"))
      (test-equal "entry 1 feature-id matches"
        "feat-alpha"
        (assoc-ref e1 "feature-id"))

      ;; Checkpoint 2: Revert fail
      (test-equal "entry 2 checkpoint is revert"
        "revert"
        (assoc-ref e2 "checkpoint"))
      (test-equal "entry 2 status is fail"
        "fail"
        (assoc-ref e2 "status"))
      (test-equal "entry 2 feature-id matches"
        "feat-beta"
        (assoc-ref e2 "feature-id"))
      (test-assert "entry 2 contains error detail"
        (string? (assoc-ref e2 "error")))

      ;; Checkpoint 3: Revert fail with exception
      (test-equal "entry 3 checkpoint is revert"
        "revert"
        (assoc-ref e3 "checkpoint"))
      (test-equal "entry 3 status is fail"
        "fail"
        (assoc-ref e3 "status"))
      (test-assert "entry 3 error mentions validation failed"
        (string-contains (assoc-ref e3 "error") "validation failed")))

    ;; Journal integrity check
    (let ((integrity (journal-verify-integrity journal)))
      (test-assert "journal integrity verifies successfully"
        (assoc-ref integrity 'valid)))))

;;;
;;; 10. Multi-Iteration Chaining & State Integrity
;;;

(test-group "multi-iteration-chaining"
  (let* ((reg (make-registry))
         (mgr (make-transaction-manager reg)))

    ;; Iteration 1: Add accumulator
    (apply-transaction! mgr 'acc
                        '((define total 0)
                          (define (acc n) (set! total (+ total n)) total))
                        (lambda (m) #t))
    (test-equal "acc iteration 1 step 1" 5 (registry-dispatch reg 'acc 5))
    (test-equal "acc iteration 1 step 2" 15 (registry-dispatch reg 'acc 10))

    ;; Iteration 2: Failed attempt to patch accumulator (returns #f)
    (let ((res (apply-transaction! mgr 'acc
                                   '((define total 9999))
                                   (lambda (m) #f))))
      (test-equal "failed patch reverted" "reverted" (assoc-ref res 'status)))

    ;; Verify total was not corrupted
    (test-equal "acc continues with preserved state" 20 (registry-dispatch reg 'acc 5))

    ;; Iteration 3: Successful patch adding reset!
    (let ((res (apply-transaction! mgr 'acc
                                   '((define (acc-reset!) (set! total 0) total)
                                     (define (acc-read) total))
                                   (lambda (m) #t))))
      (test-equal "upgrade committed" "committed" (assoc-ref res 'status)))

    ;; Dispatches to new function in updated module
    (let ((mod (assoc-ref (registry-metadata reg 'acc) 'module)))
      (test-assert "new procedure acc-reset! exists in module"
        (procedure? (feature-module-ref mod 'acc-reset!)))
      (test-equal "acc-reset! resets total"
        0
        ((feature-module-ref mod 'acc-reset!))))))

(test-end "badb-transaction")
