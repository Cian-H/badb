;;; test-persona-synthesizer.scm — Tests for the Synthesizer-Agent Persona
;;; (geasa persona-synthesizer plugin)
;;;
;;; Same contract shape as persona-badb, pinned to the synthesizer's
;;; identity text: served identity == the bundle's prompts/identity.md,
;;; byte for byte.

(define-module (test test-persona-synthesizer)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (ice-9 textual-ports)
  #:use-module (geasa persona-synthesizer plugin))

(spec-begin "persona-synthesizer")

(test-group "persona-synthesizer-content"
  (test-equal "persona id is synthesizer" "synthesizer" (persona-id))

  (let ((identity (persona-identity)))
    (test-assert "identity is a non-empty string"
      (and (string? identity) (> (string-length identity) 0)))
    (test-assert "identity names the synthesizer role"
      (string-contains identity "badb synthesizer"))
    (test-assert "identity commits to contract-bound synthesis"
      (string-contains identity "contract-bound Guile modules"))
    (test-assert "identity defers to the runner's verdict"
      (string-contains identity "only the runner's verdict counts"))
    (test-assert "identity forbids modifying the spec"
      (string-contains identity "Never modify the specification"))))

(test-group "persona-synthesizer-pinned-file-contract"
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

(test-group "persona-synthesizer-stability"
  (test-equal "identity is stable across calls"
    (persona-identity) (persona-identity)))

;;;
;;; Shared directive features (structural Scheme editing)
;;;

(test-group "persona-synthesizer-directives"
  (let ((prompt (persona-prompt)))
    (test-assert "composed prompt starts with the identity"
      (string-prefix? (persona-identity) prompt))
    (test-assert "composed prompt carries the structural-editing directive"
      (string-contains prompt "AST MCP structural-editing tools"))
    (test-assert "directive bans raw-text edits while tools are connected"
      (string-contains prompt "never rewrite code as raw strings"))
    (test-assert "directive allows the disconnected fallback"
      (string-contains prompt "only when no structural tools are connected"))
    (test-equal "composed prompt is stable across calls"
      prompt (persona-prompt))))

(spec-end)
(spec-exit)
