;;; badb — Synthesizer-Agent Persona
;;;
;;; The identity of the contract-synthesis agent driven by the Sema
;;; plugin-synthesis workflow, as pinned bundle data served through the
;;; generic persona service from the persona-base geasa bundle (a
;;; hash-pinned geis dependency).  The persona carries only stable
;;; identity and discipline; per-run task specifics (paths, feature ids)
;;; travel in the workflow's task prompt, not here.

(define-module (geasa persona-synthesizer plugin)
               #:use-module (geasa persona-base plugin)
               #:export (persona-id
                          persona-identity
                          persona-identity-file
                          persona-prompt))

(define %service
        (make-persona-service "synthesizer"
                              #:directives '("scheme-structural-editing")))

(define (persona-id)
        "Identifier of this persona."
        (persona-service-id %service))

(define (persona-identity-file)
        "Path of the canonical persona text within this bundle."
        (persona-service-identity-file %service))

(define (persona-identity)
        "The synthesizer persona: the content of the bundle's prompts/identity.md."
        (persona-service-identity %service))

(define (persona-prompt)
        "The full synthesizer prompt: the persona composed with its shared
directive features (structural Scheme editing)."
        (persona-service-prompt %service))
