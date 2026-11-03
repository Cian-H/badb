;;; badb — Default Harness Persona
;;;
;;; The harness persona (the identity string the system prompt opens
;;; with) as pinned bundle data: prompts/identity.md is the canonical
;;; text, and PERSONA-IDENTITY serves exactly that file, so the persona
;;; is content the kernel gate hashes and replays — not code.  Swap the
;;; harness persona by installing a different persona bundle.

(define-module (geasa persona-badb plugin)
  #:use-module (ice-9 textual-ports)
  #:export (persona-id
            persona-identity
            persona-identity-file))

(define %identity-rel-path "geasa/persona-badb/prompts/identity.md")

(define %identity-file-cache #f)
(define %identity-cache #f)

(define (persona-id)
  "Identifier of this persona."
  "badb")

(define (persona-identity-file)
  "Path of the canonical persona text: the prompts/identity.md of this
bundle, resolved against the load path exactly as the module itself
was.  Throws when not found."
  (or %identity-file-cache
      (begin
        (set! %identity-file-cache
              (or (search-path %load-path %identity-rel-path)
                  (error (format #f "persona-badb: ~a not found on the load path"
                                 %identity-rel-path))))
        %identity-file-cache)))

(define (persona-identity)
  "The harness persona: the content of the bundle's prompts/identity.md
with any trailing newline trimmed.  Throws when the file is unreadable."
  (unless %identity-cache
    (set! %identity-cache
          (string-trim-right
           (call-with-input-file (persona-identity-file)
             (lambda (port)
               (set-port-encoding! port "UTF-8")
               (get-string-all port)))
           #\newline)))
  %identity-cache)
