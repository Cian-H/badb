;;; geis/runtimes-synthesis/spec.geis.scm — Sema synthesis workflow geis bundle.
;;;
;;; Roadmap Phase 3 Point 1:
;;; - Sema plugin-synthesis workflow: given a locked spec.scm, run a Sema workflow
;;;   (defagent, AST MCP + spec runner tools, llm/with-budget, cassette-backed tests)
;;;   that iteratively synthesizes the Guile module from structured counterexample feedback
;;;   until all contracts pass, then triggers kernel commit + hot-attach.

(define-module (test test-synthesis)
  #:use-module (badb kernel digest)
  #:use-module (badb spec)
  #:use-module (badb kernel journal)
  #:use-module (badb kernel registry)
  #:use-module (badb kernel geis-gate)
  #:use-module (badb runtimes synthesis)
  #:use-module (badb kernel transaction)
  #:use-module (badb surfaces tui)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (srfi srfi-64))

(spec-begin "runtimes-synthesis")

(define %temp-dir (mkdtemp "/tmp/badb-synthesis-test-XXXXXX"))
(define %target-path "test/fixtures/lib/math-utils.scm")
(define %spec-source (spec-bundle-ref "fixtures/spec-pass.scm"))
(define %feature-id "math-utils-synthesized")
(define %cassette (spec-bundle-ref "cassettes/synthesis-fixture.jsonl"))

(define %original-target-content
  (call-with-input-file %target-path get-string-all))

(define (write-broken-math-utils!)
  (call-with-output-file %target-path
    (lambda (p)
      (display "(define-module (test fixtures lib math-utils)\n  #:export (clamp double))\n\n(define (clamp lo hi x)\n  (min hi (max lo x)))\n\n(define (double x)\n  (+ x 1))\n" p))))

(define (restore-math-utils!)
  (call-with-output-file %target-path
    (lambda (p)
      (display %original-target-content p))))

;; Ensure spec is locked for the feature
(lock-spec %spec-source %feature-id)

;;;
;;; 1. Sema binary discovery
;;;
(test-group "sema-discovery"
  (let ((bin (find-sema-executable)))
    (test-assert "find-sema-executable returns a non-empty string"
      (and (string? bin) (not (string-null? bin))))
    (test-assert "located sema binary exists on disk"
      (file-exists? bin))))

;;;
;;; 2. Spec locking helper
;;;
(test-group "spec-locking"
  (let ((hash (lock-feature-spec %spec-source "synth-lock-helper-test")))
    (test-assert "lock-feature-spec returns 64-char hex digest"
      (and (string? hash) (= (string-length hash) 64)))
    (test-assert "locked spec file exists in kernel locked-spec store"
      (file-exists? (locked-spec-path "synth-lock-helper-test")))))

;;;
;;; 3. Deterministic cassette-backed synthesis workflow
;;;
(test-group "cassette-backed-synthesis"
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      ;; Initialize target file in a broken state
      (write-broken-math-utils!)

      ;; Verify that the broken file fails contract verification initially
      (let ((initial-verify (verify-locked-spec %feature-id)))
        (test-equal "initial spec verification fails on broken double"
          "fail" (assoc-ref initial-verify 'verdict)))

      ;; Run the synthesis workflow in replay mode using the recorded cassette
      (let* ((res (run-synthesis-workflow %feature-id
                                         #:spec-path (locked-spec-path %feature-id)
                                         #:target-path %target-path
                                         #:cassette %cassette
                                         #:cassette-mode "replay"
                                         #:run-dir (string-append %temp-dir "/runs")))
             (wf-result (assoc-ref res 'result))
             (pins (and wf-result (assoc-ref wf-result "pins"))))

        (test-equal "workflow execution status is success"
          "success" (assoc-ref res 'status))
        (test-equal "workflow exit code is 0"
          0 (assoc-ref res 'exit-code))
        (test-assert "workflow result payload present"
          (and (pair? wf-result) (list? wf-result)))
        (test-equal "synthesis verdict is pass"
          "pass" (assoc-ref wf-result "verdict"))

        ;; Verify identity pins on the kernel commit
        (test-assert "pins structure present" (pair? pins))
        (test-equal "commit status is committed"
          "committed" (assoc-ref pins "status"))
        (test-equal "quickcheck verdict pinned as pass"
          "pass" (assoc-ref pins "quickcheck-verdict"))
        (test-equal "quickcheck seed pinned"
          "42" (assoc-ref pins "quickcheck-seed"))
        (test-equal "kernel replay verdict pinned as passed"
          "passed" (assoc-ref pins "kernel-replay-verdict"))
        (test-assert "spec-hash pinned as 64-char hex"
          (and (string? (assoc-ref pins "spec-hash"))
               (= 64 (string-length (assoc-ref pins "spec-hash")))))
        (test-assert "ast-diff-hash pinned as 64-char hex"
          (and (string? (assoc-ref pins "ast-diff-hash"))
               (= 64 (string-length (assoc-ref pins "ast-diff-hash")))))

        ;; Verify that target file on disk was repaired by the agent
        (let ((repaired-content (call-with-input-file %target-path get-string-all)))
          (test-assert "target file now contains (+ x x)"
            (string-contains repaired-content "(+ x x)")))))
    (lambda ()
      (restore-math-utils!))))

;;;
;;; 4. High-level synthesize-feature! with hot-attach
;;;
(test-group "synthesize-feature-hot-attach"
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (write-broken-math-utils!)
      (let* ((jrn-path (string-append %temp-dir "/attach-journal.jsonl"))
             (jrn (make-journal jrn-path))
             (reg (make-registry))
             (mgr (make-transaction-manager reg jrn))
             (res (synthesize-feature! %feature-id
                                      #:spec-path (locked-spec-path %feature-id)
                                      #:target-path %target-path
                                      #:mgr mgr
                                      #:cassette %cassette
                                      #:cassette-mode "replay"
                                      #:run-dir (string-append %temp-dir "/attach-runs"))))
        (test-equal "synthesize-feature! reports success"
          "success" (assoc-ref res 'status))
        ;; Check that feature is attached in the registry
        (test-assert "synthesized feature is attached to registry"
          (registry-lookup reg %feature-id))
        ;; Check that journal records the commit checkpoint with identity pins
        (let ((latest (journal-latest jrn)))
          (test-assert "journal commit entry exists" latest)
          (test-equal "journal checkpoint is commit"
            "commit" (assoc-ref latest "checkpoint"))
          (test-equal "journal pins quickcheck verdict"
            "pass" (assoc-ref latest "quickcheck-verdict"))
          (test-equal "journal pins kernel replay verdict"
            "passed" (assoc-ref latest "kernel-replay-verdict")))))
    (lambda ()
      (restore-math-utils!))))

;;;
;;; 5. Budget enforcement
;;;
(test-group "budget-enforcement"
  ;; When budget limit is exceeded (e.g. 1 token cap), workflow fails closed
  (let* ((res (run-synthesis-workflow %feature-id
                                     #:spec-path (locked-spec-path %feature-id)
                                     #:target-path %target-path
                                     #:budget '((max-tokens . 1))
                                     #:cassette %cassette
                                     #:cassette-mode "replay"
                                     #:run-dir (string-append %temp-dir "/budget-runs")))
         (wf-result (assoc-ref res 'result)))
    (test-equal "budget-exceeded workflow reports failed status"
      "failed" (assoc-ref res 'status))
    (test-assert "error details indicate budget was exceeded"
      (let ((err-str (or (and wf-result (assoc-ref wf-result "error"))
                         (assoc-ref res 'stdout)
                         "")))
        (string-contains err-str "budget exceeded")))))

;;;
;;; 6. Failing spec reverts and fails closed
;;;
(test-group "failing-spec-gate-reverts"
  (let* ((fail-id "synthesis-fail-gate-test")
         (fail-spec "test/fixtures/spec-fail.scm")
         (jrn-path (string-append %temp-dir "/fail-journal.jsonl"))
         (jrn (make-journal jrn-path))
         (reg (make-registry))
         (mgr (make-transaction-manager reg jrn)))
    (lock-spec fail-spec fail-id)
    (let ((res (commit-synthesized-module! mgr fail-id %target-path)))
      (test-equal "commit-synthesized-module! reverts on failing spec"
        "reverted" (assoc-ref res 'status))
      (test-assert "reverted feature is not in the registry"
        (not (registry-lookup reg fail-id)))
      (let ((latest (journal-latest jrn)))
        (test-equal "revert checkpoint is logged"
          "revert" (assoc-ref latest "checkpoint"))))))

;;;
;;; 7. TUI tool registration
;;;
(test-group "tui-tool-registration"
  (let ((reg (make-registry)))
    (register-spec-tools! reg)
    (test-assert "spec.synthesize is registered as a procedure"
      (procedure? (registry-lookup reg "spec.synthesize")))
    (test-assert "spec.synthesize metadata describes synthesis workflow"
      (let ((desc (assoc-ref (registry-metadata reg "spec.synthesize") 'description)))
        (and (string? desc)
             (string-contains desc "Sema plugin synthesis workflow"))))))

(spec-end)
(spec-exit)
