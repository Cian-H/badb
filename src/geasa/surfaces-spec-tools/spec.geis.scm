;;; test/test-spec-tools.scm — kernel-owned spec gate TUI tool tests.
;;;
;;; Exercises the spec.lock / spec.verify / spec.status kernel tools
;;; that register-spec-tools! (src/badb/tui.scm) attaches to the TUI
;;; agent registry — the roadmap Phase 2/3 dogfood half: the agent
;;; authors a spec, the kernel locks and hashes it, and verdicts come
;;; only from kernel subprocess replay of the LOCKED copy. The tool
;;; handlers carry no TUI state, so the suite builds a plain registry
;;; the way the other registry tests do and dispatches through it —
;;; no TUI launch, no notcurses. All locking happens under a
;;; throwaway BADB_SPEC_GATE_ROOT so nothing touches .badb/specs/.

(define-module (test test-spec-tools)
  #:use-module (badb kernel digest)
  #:use-module (badb spec)
  #:use-module (badb kernel registry)
  #:use-module (badb kernel geis-gate)
  #:use-module (geasa surfaces-tui plugin)
  #:use-module (srfi srfi-64))

;;;
;;; Helpers: temp workspace and fixture spec authoring
;;;

(define %temp-dir #f)
(define %root #f)
(define %empty-root #f)
(define %saved-root #f)

(define (write-spec! path text)
  (call-with-output-file path (lambda (port) (display text port))))

(define (locked-exists? feature-id)
  (file-exists? (locked-spec-path feature-id)))

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

(spec-begin "surfaces-spec-tools")

;;;
;;; Shared fixture workspace: a temp locked-spec root for the suite.
;;;
(let ((dir (mkdtemp "/tmp/badb-spec-tools-test-XXXXXX")))
  (set! %temp-dir dir)
  (set! %root (string-append dir "/gate-root"))
  (set! %empty-root (string-append dir "/empty-root"))
  (set! %saved-root (getenv "BADB_SPEC_GATE_ROOT"))
  (setenv "BADB_SPEC_GATE_ROOT" %root))

;;; Build the registry the way the other registry tests do and mount
;;; the kernel spec tools on it exactly as the TUI does at startup.
(define %reg (make-registry))
(register-spec-tools! %reg)

(test-group "tool-attachment"
  (test-assert "spec.lock is attached as a procedure"
    (procedure? (registry-lookup %reg "spec.lock")))
  (test-assert "spec.verify is attached as a procedure"
    (procedure? (registry-lookup %reg "spec.verify")))
  (test-assert "spec.status is attached as a procedure"
    (procedure? (registry-lookup %reg "spec.status")))
  (test-assert "spec.lock description states the kernel re-reads the locked copy at commit time"
    (string-contains (assoc-ref (registry-metadata %reg "spec.lock") 'description)
                     "kernel re-reads the LOCKED copy at commit time"))
  (test-assert "spec.lock description states locking makes the contract binding"
    (string-contains (assoc-ref (registry-metadata %reg "spec.lock") 'description)
                     "Locking is how a contract becomes binding"))
  (test-assert "spec.verify description states verdicts come only from kernel subprocess replay"
    (string-contains (assoc-ref (registry-metadata %reg "spec.verify") 'description)
                     "only from kernel subprocess replay"))
  (test-assert "spec.status description states the kernel re-reads the locked copies"
    (string-contains (assoc-ref (registry-metadata %reg "spec.status") 'description)
                     "kernel re-reads the LOCKED copies at commit time"))
  (test-assert "spec.lock schema requires spec-path and feature-id"
    (let* ((meta (registry-metadata %reg "spec.lock"))
           (params (assoc-ref meta 'parameters))
           (required (assoc-ref params 'required)))
      (and (vector? required)
           (member "spec-path" (vector->list required))
           (member "feature-id" (vector->list required)))))
  (test-assert "spec.verify schema requires feature-id"
    (let* ((meta (registry-metadata %reg "spec.verify"))
           (params (assoc-ref meta 'parameters))
           (required (assoc-ref params 'required)))
      (and (vector? required)
           (member "feature-id" (vector->list required))))))

(test-group "spec-lock-tool"
  (let ((source (string-append %temp-dir "/tool-pass-spec.scm")))
    (write-spec! source %pass-spec-text)
    (let* ((out (registry-dispatch %reg "spec.lock" source "tool-lock-feature"))
           (locked (locked-spec-path "tool-lock-feature")))
      (test-assert "dispatch returns the locked copy path"
        (string-contains out locked))
      (test-assert "dispatch returns the SHA-256 spec-hash of the spec text"
        (string-contains out (sha256-hex %pass-spec-text)))
      (test-assert "the locked copy exists inside the throwaway store"
        (and (file-exists? locked)
             (string-prefix? %root locked)))
      (test-assert "the locked copy is read-only"
        (eqv? (logand (stat:perms (stat locked)) #o222) 0))
      (test-assert "the lock result states the kernel re-reads the locked copy"
        (string-contains out "kernel re-reads the LOCKED copy at commit time")))
    (test-assert "locking a missing file reports an error"
      (string-contains
       (registry-dispatch %reg "spec.lock"
                          (string-append %temp-dir "/no-such-spec.scm")
                          "tool-missing-feature")
       "does not exist"))
    (test-assert "the failed lock left no locked copy behind"
      (not (locked-exists? "tool-missing-feature")))
    (test-assert "an empty feature-id is rejected"
      (string-contains (registry-dispatch %reg "spec.lock" source "")
                       "requires non-empty string"))
    (test-assert "an empty feature-id leaves no locked copy behind"
      (not (locked-exists? "")))))

(test-group "spec-verify-tool"
  (let ((pass-source (string-append %temp-dir "/verify-pass.scm"))
        (fail-source (string-append %temp-dir "/verify-fail.scm")))
    (write-spec! pass-source %pass-spec-text)
    (write-spec! fail-source %fail-spec-text)
    (registry-dispatch %reg "spec.lock" pass-source "tool-verify-pass")
    (registry-dispatch %reg "spec.lock" fail-source "tool-verify-fail")
    (let ((out (registry-dispatch %reg "spec.verify" "tool-verify-pass")))
      (test-assert "a passing kernel replay reports verdict PASS"
        (string-contains out "Kernel replay verdict: PASS"))
      (test-assert "the output states verdicts come only from kernel subprocess replay"
        (string-contains out "only from kernel subprocess replay"))
      (test-assert "the output carries the locked copy spec-hash"
        (string-contains out (sha256-hex %pass-spec-text)))
      (test-assert "the output reports the summary counts"
        (and (string-contains out "total=3")
             (string-contains out "passed=3")
             (string-contains out "failed=0")))
      (test-assert "a passing replay lists no failures"
        (string-contains out "failures: none")))
    (let ((out (registry-dispatch %reg "spec.verify" "tool-verify-fail")))
      (test-assert "a failing kernel replay reports verdict FAIL"
        (string-contains out "Kernel replay verdict: FAIL"))
      (test-assert "the failing spec-hash is reported"
        (string-contains out (sha256-hex %fail-spec-text)))
      (test-assert "the failure record name is parsed from the diagnostics JSON"
        (string-contains out "always fails"))
      (test-assert "the failure record kind is parsed from the diagnostics JSON"
        (string-contains out "example"))
      (test-assert "the summary failed count is reported"
        (string-contains out "failed=1")))
    (let ((out (registry-dispatch %reg "spec.verify" "tool-never-locked")))
      (test-assert "a feature with no locked spec fails closed"
        (string-contains out "Kernel replay verdict: FAIL"))
      (test-assert "the reason points at the missing locked copy"
        (string-contains out "no locked spec")))))

(test-group "spec-status-tool"
  (let ((out (registry-dispatch %reg "spec.status")))
    (test-assert "the store directory is reported"
      (string-contains out (string-append %root "/specs")))
    (test-assert "the locked specs are listed with recomputed hashes"
      (and (string-contains out (sha256-hex %pass-spec-text))
           (string-contains out (sha256-hex %fail-spec-text))))
    (test-assert "the locked feature ids are listed"
      (and (string-contains out "tool-verify-pass")
           (string-contains out "tool-verify-fail"))))
  (test-assert "a locked feature is reported present"
    (string-contains (registry-dispatch %reg "spec.status" "tool-verify-pass")
                     "present"))
  (test-assert "an unlocked feature is reported to have no locked spec"
    (string-contains (registry-dispatch %reg "spec.status" "tool-unknown-feature")
                     "none"))
  ;; Point the store at a fresh empty root: nothing is locked there.
  (mkdir %empty-root)
  (setenv "BADB_SPEC_GATE_ROOT" %empty-root)
  (test-assert "an empty store reports no locked specs"
    (string-contains (registry-dispatch %reg "spec.status")
                     "No locked specs yet."))
  (test-assert "an empty store reports a feature as unlocked"
    (string-contains (registry-dispatch %reg "spec.status" "tool-verify-pass")
                     "none"))
  (setenv "BADB_SPEC_GATE_ROOT" %root))

;;;
;;; Teardown: restore the environment and drop the whole temp
;;; workspace (read-only locked copies first, then the directories)
;;; so nothing leaks into .badb/specs/ or /tmp.
;;;

(spec-end)
(spec-exit)

(let ((cleanup
       (lambda ()
         (when %temp-dir
           (for-each
            (lambda (feature-id)
              (catch #t
                (lambda () (delete-file (locked-spec-path feature-id %root)))
                (lambda _ #f)))
            '("tool-lock-feature" "tool-verify-pass" "tool-verify-fail"))
           (for-each
            (lambda (path)
              (catch #t (lambda () (delete-file path)) (lambda _ #f)))
            (list (string-append %temp-dir "/tool-pass-spec.scm")
                  (string-append %temp-dir "/verify-pass.scm")
                  (string-append %temp-dir "/verify-fail.scm")))
           (for-each
            (lambda (dir)
              (catch #t (lambda () (rmdir dir)) (lambda _ #f)))
            (list (string-append %root "/specs")
                  %root
                  %empty-root
                  %temp-dir)))
         (if %saved-root
             (setenv "BADB_SPEC_GATE_ROOT" %saved-root)
             (unsetenv "BADB_SPEC_GATE_ROOT")))))
  (cleanup))
