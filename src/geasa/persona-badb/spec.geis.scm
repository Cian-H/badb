;;; test-persona-badb.scm — Tests for the Default Harness Persona
;;; (geasa persona-badb plugin)
;;;
;;; The core contract: PERSONA-IDENTITY serves exactly the bundle's
;;; pinned prompts/identity.md, byte for byte.  Also verifies the
;;; persona is a well-formed non-empty identity naming the harness,
;;; resolves stably across calls, and that the identity file is part
;;; of the bundle.

(define-module (test test-persona-badb)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (ice-9 textual-ports)
  #:use-module (geasa persona-badb plugin))

(spec-begin "persona-badb")

;;;
;;; 1. Persona identity content
;;;

(test-group "persona-identity-content"
  (test-equal "persona id is badb" "badb" (persona-id))

  (let ((identity (persona-identity)))
    (test-assert "identity is a non-empty string"
      (and (string? identity) (> (string-length identity) 0)))
    (test-assert "identity has no trailing newline"
      (not (char=? #\newline (string-ref identity (- (string-length identity) 1)))))
    (test-assert "identity names the harness"
      (string-contains identity "You are badb"))
    (test-assert "identity demands read-before-edit discipline"
      (string-contains identity "Read before editing"))
    (test-assert "identity commits to verification"
      (string-contains identity "verify every step"))
    (test-assert "identity advertises the tool set"
      (string-contains identity "run_tests"))
    (test-assert "identity advertises checkpoint/rollback safety nets"
      (string-contains identity "checkpoint_rollback"))
    (test-assert "identity mandates the triage-first delegation loop"
      (string-contains identity "DO NOT brute-force it in context"))
    (test-assert "identity teaches the agent-side synthesis path"
      (and (string-contains identity "spec.lock")
           (string-contains identity "spec.synthesize")
           (string-contains identity "spec.verify")))
    (test-assert "identity teaches the project-local landing convention"
      (and (string-contains identity "geis/<feature-id>/")
           (string-contains identity "(geis <feature-id> plugin)")
           (string-contains identity "(geasa <feature-id> plugin)")))
    (test-assert "identity forbids hand-writing plugin implementations"
      (string-contains identity "Do not hand-write the plugin implementation"))
    (test-assert "identity forbids bypassing the synthesis workflow"
      (string-contains identity "do not bypass the workflow"))
    (test-assert "identity trusts only the kernel gate's verdict"
      (string-contains identity "never self-assessment"))
    (test-assert "identity states the error posture"
      (string-contains identity "admit uncertainty"))))

;;;
;;; 2. The pinned-file contract
;;;

(test-group "persona-pinned-file-contract"
  (test-assert "identity file is part of the bundle"
    (file-exists? (spec-bundle-ref "prompts/identity.md")))
  (test-equal "identity file is the canonical persona text"
    (string-trim-right
     (call-with-input-file (spec-bundle-ref "prompts/identity.md")
       (lambda (port)
         (set-port-encoding! port "UTF-8")
         (get-string-all port)))
     #\newline)
    (persona-identity))
  (test-assert "served identity resolves to a file inside this bundle"
    (string=? (spec-bundle-ref "prompts/identity.md")
              (persona-identity-file))))

;;;
;;; 3. Stability
;;;

(test-group "persona-stability"
  (test-equal "identity is stable across calls"
    (persona-identity) (persona-identity))
  (test-equal "identity does not mutate between reads"
    (persona-identity)
    (string-trim-right
     (call-with-input-file (persona-identity-file)
       (lambda (port)
         (set-port-encoding! port "UTF-8")
         (get-string-all port)))
     #\newline)))

(spec-end)
(spec-exit)
