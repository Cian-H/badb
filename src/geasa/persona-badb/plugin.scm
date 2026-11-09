;;; badb — Main-Agent Harness Persona
;;;
;;; The main agent's identity, as pinned bundle data: prompts/identity.md
;;; is the canonical persona text, served through the generic persona
;;; service from the persona-base geasa bundle (a hash-pinned geis
;;; dependency).  The kernel gate hashes and replays both the text and
;;; the loader.  Swap the harness persona by installing a different
;;; persona bundle and repointing the dependents.

(define-module (geasa persona-badb plugin)
               #:use-module (geasa persona-base plugin)
               #:export (persona-id
                          persona-identity
                          persona-identity-file))

(define %service (make-persona-service "badb"))

(define (persona-id)
        "Identifier of this persona."
        (persona-service-id %service))

(define (persona-identity-file)
        "Path of the canonical persona text within this bundle."
        (persona-service-identity-file %service))

(define (persona-identity)
        "The harness persona: the content of the bundle's prompts/identity.md."
        (persona-service-identity %service))
