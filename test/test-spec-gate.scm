;;; test/test-spec-gate.scm — kernel-owned spec verification gate tests.
;;;
;;; Exercises the Phase 2 spec gate end-to-end with fixture specs that
;;; use plain SRFI-64 (no quickcheck yet): locking (read-only immutable
;;; copy), the disposable-subprocess verdict (pass, fail, JSON/exit
;;; mismatch failing closed, subprocess timeout), the corrupted-source
;;; case (the gate re-reads the locked copy, never the agent's working
;;; copy), and the journal pins observed on gated commits made through
;;; the transaction manager.

(define-module (test test-spec-gate)
  #:use-module (badb digest)
  #:use-module (badb journal)
  #:use-module (badb registry)
  #:use-module (badb spec-gate)
  #:use-module (badb transaction)
  #:use-module (badb util)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;;;
;;; Helpers: temp workspace and fixture spec authoring
;;;

(define %temp-dir #f)
(define %root #f)
(define %saved-root #f)

(define (write-spec! path text)
  (call-with-output-file path (lambda (port) (display text port))))

(define (sha256-of text)
  (sha256-hex text))

(define %pass-spec-text
  "(use-modules (srfi srfi-64) (json))
(test-begin \"fixture-pass\")
(test-assert \"addition holds\" (= 4 (+ 2 2)))
(test-assert \"identity holds\" (= 42 (* 1 42)))
(test-assert \"ordering holds\" (<= 1 2))
(test-end \"fixture-pass\")
(display (scm->json-string
          '((summary . ((total . 3) (passed . 3) (failed . 0)))
            (verdict . \"pass\")
            (seed . \"fixture-seed-pass\")
            (failures . #()))))
(newline)
")

(define %fail-spec-text
  "(use-modules (srfi srfi-64) (json))
(test-begin \"fixture-fail\")
(test-assert \"always fails\" #f)
(test-end \"fixture-fail\")
(display (scm->json-string
          '((summary . ((total . 1) (passed . 0) (failed . 1)))
            (verdict . \"fail\")
            (seed . \"fixture-seed-fail\")
            (failures . #(((name . \"always fails\")
                           (kind . \"example\")
                           (seed . \"fixture-seed-fail\")
                           (inputs . #(false))
                           (exception . \"fixture exception text\")
                           (backtrace . \"fixture backtrace text\")))))))
(newline)
(primitive-exit 1)
")

(define %mismatch-spec-text
  "(use-modules (srfi srfi-64) (json))
(test-begin \"fixture-mismatch\")
(test-assert \"locally fine\" (= 2 (+ 1 1)))
(test-end \"fixture-mismatch\")
(display (scm->json-string
          '((summary . ((total . 1) (passed . 1) (failed . 0)))
            (verdict . \"pass\")
            (seed . \"fixture-seed-mismatch\")
            (failures . #()))))
(newline)
(primitive-exit 1)
")

(define %timeout-spec-text
  "(use-modules (srfi srfi-64) (json))
(test-begin \"fixture-timeout\")
(test-assert \"slow assertion\" (begin (sleep 120) #t))
(test-end \"fixture-timeout\")
(display (scm->json-string
          '((summary . ((total . 1) (passed . 1) (failed . 0)))
            (verdict . \"pass\")
            (seed . \"fixture-seed-timeout\")
            (failures . #()))))
(newline)
")

(test-begin "spec-gate")

;;;
;;; Shared fixture workspace: a temp locked-spec root for the whole suite.
;;;
(let ((dir (mkdtemp "/tmp/badb-spec-gate-test-XXXXXX")))
  (set! %temp-dir dir)
  (set! %root (string-append dir "/gate-root"))
  (set! %saved-root (getenv "BADB_SPEC_GATE_ROOT"))
  (setenv "BADB_SPEC_GATE_ROOT" %root))

(test-group "feature-id-sanitization"
  (test-equal "path-unsafe characters become underscores"
    "feat.1_pass_x" (sanitize-feature-id "feat.1 pass/x"))
  (test-equal "a path-unsafe-only feature id still yields a component"
    "___" (sanitize-feature-id "///"))
  (test-equal "locked spec path lives under the configured root"
    (string-append %root "/specs/gate-feature.scm")
    (locked-spec-path "gate-feature" %root)))

(test-group "lock-spec"
  (let ((source (string-append %temp-dir "/pass-spec.scm")))
    (write-spec! source %pass-spec-text)
    (let ((hash (lock-spec source "lock-me"))
          (locked (locked-spec-path "lock-me" %root)))
      (test-assert "lock-spec returns a 64-char hex digest"
        (and (= 64 (string-length hash))
             (every (lambda (c) (memv c (string->list "0123456789abcdef")))
                    (string->list hash))))
      (test-assert "the locked copy exists"
        (file-exists? locked))
      (test-equal "the locked copy is byte-identical to the source"
        %pass-spec-text
        (call-with-input-file locked
          (lambda (port)
            (set-port-encoding! port "UTF-8")
            (get-string-all port))))
      (test-error "the locked copy is read-only"
        'system-error
        (call-with-output-file locked (lambda (port) #f)))
      (test-equal "re-locking the same content yields the same hash"
        hash
        (lock-spec source "lock-me")))))

(test-group "verify-locked-spec-pass"
  (let ((source (string-append %temp-dir "/pass-spec.scm")))
    (lock-spec source "gate-pass-feature")
    (let ((result (verify-locked-spec "gate-pass-feature" #:timeout 60)))
      (test-equal "a passing spec yields verdict pass"
        "pass" (assoc-ref result 'verdict))
      (test-equal "the exit status is captured"
        0 (assoc-ref result 'exit-status))
      (test-equal "the quickcheck seed is parsed from the JSON"
        "fixture-seed-pass" (assoc-ref result 'seed))
      (test-equal "the summary counts are parsed"
        3 (assoc-ref result 'summary-total))
      (test-equal "the spec hash matches the locked copy digest"
        (sha256-of %pass-spec-text)
        (assoc-ref result 'spec-hash))
      (test-equal "the verdict is a string"
        "pass" (ensure-string (assoc-ref result 'verdict))))))

(test-group "verify-locked-spec-fail"
  (let ((source (string-append %temp-dir "/fail-spec.scm")))
    (write-spec! source %fail-spec-text)
    (lock-spec source "gate-fail-feature")
    (let ((result (verify-locked-spec "gate-fail-feature" #:timeout 60)))
      (test-equal "a failing spec yields verdict fail"
        "fail" (assoc-ref result 'verdict))
      (test-equal "the nonzero exit status is captured"
        1 (assoc-ref result 'exit-status))
      (test-equal "the quickcheck seed is parsed from the JSON"
        "fixture-seed-fail" (assoc-ref result 'seed))
      (test-equal "the summary failed count is parsed"
        1 (assoc-ref result 'summary-failed))
      (test-assert "the failure record is parsed from the JSON"
        (let ((failures (assoc-ref result 'failures)))
          (and (pair? failures)
               (let ((first (car failures)))
                 (and (string=? (assoc-ref first "name") "always fails")
                      (string=? (assoc-ref first "kind") "example")))))))))

(test-group "verify-locked-spec-mismatch-fails-closed"
  (let ((source (string-append %temp-dir "/mismatch-spec.scm")))
    (write-spec! source %mismatch-spec-text)
    (lock-spec source "gate-mismatch-feature")
    (let ((result (verify-locked-spec "gate-mismatch-feature" #:timeout 60)))
      (test-equal "JSON pass + nonzero exit fails closed"
        "fail" (assoc-ref result 'verdict))
      (test-assert "the mismatch details mention the disagreement"
        (string-contains (assoc-ref result 'reason) "mismatch")))))

(test-group "verify-locked-spec-no-spec"
  (let ((result (verify-locked-spec "gate-never-locked-feature" #:timeout 60)))
    (test-equal "a feature with no locked spec fails closed at this level"
      "fail" (assoc-ref result 'verdict))
    (test-assert "the reason points at the missing lock"
      (string-contains (assoc-ref result 'reason) "no locked spec"))))

(test-group "locked-copy-is-trusted-not-the-source"
  (let ((source (string-append %temp-dir "/corrupt-source.scm")))
    (write-spec! source %pass-spec-text)
    (lock-spec source "gate-corrupt-feature")
    ;; The agent corrupts its working copy after locking.
    (write-spec! source %fail-spec-text)
    (let ((result (verify-locked-spec "gate-corrupt-feature" #:timeout 60)))
      (test-equal "the gate still passes from the locked copy"
        "pass" (assoc-ref result 'verdict)))))

(test-group "verify-locked-spec-timeout"
  (let ((source (string-append %temp-dir "/timeout-spec.scm")))
    (write-spec! source %timeout-spec-text)
    (lock-spec source "gate-timeout-feature")
    (let ((result (verify-locked-spec "gate-timeout-feature" #:timeout 5)))
      (test-equal "a hanging spec subprocess fails closed on timeout"
        "fail" (assoc-ref result 'verdict))
      (test-assert "the timeout is reported"
        (assoc-ref result 'timed-out?))
      (test-assert "the details mention the timeout"
        (string-contains (assoc-ref result 'reason) "timed out")))))

(test-group "transaction-pins"
  ;; Passing gate through the transaction manager pins the journal.
  (let* ((jrn (make-journal (string-append %temp-dir "/journal-pass.jsonl")))
         (mgr (make-transaction-manager (make-registry) jrn))
         (forms '((define (gate-pin-feature x) (+ x 1)))))
    (lock-spec (string-append %temp-dir "/pass-spec.scm") "gate-pin-feature")
    (test-equal "gated commit succeeds"
      "committed"
      (assoc-ref (apply-transaction! mgr "gate-pin-feature" forms
                                    (lambda (mod) #t)
                                    #:metadata '((author . "spec-gate-test")))
                 'status))
    (let ((latest (journal-latest jrn)))
      (test-equal "commit record pins the spec hash"
        (sha256-of %pass-spec-text)
        (assoc-ref latest "spec-hash"))
      (test-equal "commit record pins the quickcheck seed"
        "fixture-seed-pass" (assoc-ref latest "quickcheck-seed"))
      (test-equal "commit record pins the quickcheck verdict"
        "pass" (assoc-ref latest "quickcheck-verdict"))
      (test-equal "commit record pins the kernel replay verdict"
        "passed" (assoc-ref latest "kernel-replay-verdict"))
      (test-equal "commit record pins the AST diff hash"
        (spec-gate-ast-diff-hash forms)
        (assoc-ref latest "ast-diff-hash"))
      (test-assert "commit record carries no contract-id pin"
        (not (assoc-ref latest "contract-id"))))))

(test-group "transaction-pins-fail-reverts"
  ;; Failing gate reverts the transaction (fail closed).
  (let* ((jrn (make-journal (string-append %temp-dir "/journal-fail.jsonl")))
         (mgr (make-transaction-manager (make-registry) jrn)))
    (lock-spec (string-append %temp-dir "/fail-spec.scm") "gate-revert-feature")
    (test-equal "failed gate verdict reverts the transaction"
      "reverted"
      (assoc-ref (apply-transaction! mgr "gate-revert-feature"
                                      '((define (gate-revert-feature x) x))
                                      (lambda (mod) #t))
                 'status))
    (test-assert "the reverted feature is not in the registry"
      (not (registry-lookup (transaction-manager-registry mgr)
                            "gate-revert-feature")))
    (test-equal "a revert checkpoint is logged for the failed gate"
      "revert" (assoc-ref (journal-latest jrn) "checkpoint"))))

(test-group "transaction-pins-no-spec"
  ;; No locked spec: zero pinned fields, behavior identical to no gate.
  (let* ((jrn (make-journal (string-append %temp-dir "/journal-nospec.jsonl")))
         (mgr (make-transaction-manager (make-registry) jrn)))
    (test-equal "a commit without a locked spec succeeds"
      "committed"
      (assoc-ref (apply-transaction! mgr "gate-nospec-feature"
                                      '((define (gate-nospec-feature x) x))
                                      (lambda (mod) #t)
                                      #:metadata '((author . "spec-gate-test")))
                 'status))
    (let ((latest (journal-latest jrn)))
      (test-assert "no spec-hash pinned without a locked spec"
        (not (assoc-ref latest "spec-hash")))
      (test-assert "no quickcheck-seed pinned without a locked spec"
        (not (assoc-ref latest "quickcheck-seed")))
      (test-assert "no quickcheck-verdict pinned without a locked spec"
        (not (assoc-ref latest "quickcheck-verdict")))
      (test-assert "no kernel-replay-verdict pinned without a locked spec"
        (not (assoc-ref latest "kernel-replay-verdict")))
      (test-assert "no ast-diff-hash pinned without a locked spec"
        (not (assoc-ref latest "ast-diff-hash"))))))

(test-group "ast-diff-hash"
  (test-assert "the AST diff hash is a 64-char hex digest"
    (let ((digest (spec-gate-ast-diff-hash '((define (f x) x) (define y 1)))))
      (and (= 64 (string-length digest))
           (every (lambda (c)
                    (memv c (string->list "0123456789abcdef")))
                  (string->list digest)))))
  (test-equal "each top-level form is rendered with ~s and a newline"
    (spec-gate-ast-diff-hash '((define (f x) x) (define y 1)))
    (spec-gate-ast-diff-hash "(define (f x) x)\n(define y 1)\n")))

;;;
;;; Teardown: restore the environment and drop the temp workspace.
;;;
(test-end "spec-gate")

(let ((cleanup
       (lambda ()
         (if %saved-root
             (setenv "BADB_SPEC_GATE_ROOT" %saved-root)
             (unsetenv "BADB_SPEC_GATE_ROOT")))))
  (cleanup))
