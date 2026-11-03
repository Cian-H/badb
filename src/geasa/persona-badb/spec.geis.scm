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
    (test-assert "identity declares tool access"
      (string-contains identity "local tools"))
    (test-assert "identity commits to methodical verification"
      (string-contains identity "verifying each step"))))

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
