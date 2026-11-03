(define-module (geasa persona-base plugin)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<persona-service>
            make-persona-service
            persona-service?
            persona-service-id
            persona-service-directives
            persona-service-identity
            persona-service-identity-file
            persona-service-directive-text
            persona-service-prompt))

(define-record-type <persona-service>
  (%make-persona-service id rel-path directives file-cache identity-cache
                         directive-cache)
  persona-service?
  (id persona-service-id)
  (rel-path persona-service-rel-path)
  (directives persona-service-directives)
  (file-cache persona-service-file-cache set-persona-service-file-cache!)
  (identity-cache persona-service-identity-cache set-persona-service-identity-cache!)
  (directive-cache persona-service-directive-cache set-persona-service-directive-cache!))

(define* (make-persona-service id #:key (identity-rel-path #f) (directives '()))
  "Build the persona service for persona ID (e.g. \"badb\" for the
persona-badb bundle).  The canonical persona text is the bundle's
prompts/identity.md, resolved against the load path exactly as the
module itself was; pass #:identity-rel-path to override the location
(bundles testing this service with their own fixture use this).
DIRECTIVES is a list of shared persona features (by name, from this
bundle's prompts/directives/<name>.md library) composed after the
identity by PERSONA-SERVICE-PROMPT."
  (unless (and (string? id) (not (string-null? id)))
    (error "make-persona-service: id must be a non-empty string"))
  (let ((rel (or identity-rel-path
                (format #f "geasa/persona-~a/prompts/identity.md" id))))
    (unless (and (string? rel) (not (string-null? rel)))
      (error "make-persona-service: identity-rel-path must be a non-empty string"))
    (unless (and (list? directives) (every (lambda (d)
                                             (and (string? d)
                                                  (not (string-null? d))))
                                           directives))
      (error "make-persona-service: directives must be a list of names"))
    ;; Fail fast: every declared directive must resolve from this
    ;; bundle's directive library, so a misconfigured persona errors at
    ;; module load instead of at prompt-assembly time.
    (for-each
     (lambda (name)
       (unless (search-path %load-path
                            (format #f "geasa/persona-base/prompts/directives/~a.md"
                                    name))
         (error (format #f "make-persona-service: no such directive: ~a" name))))
     directives)
    (%make-persona-service id rel directives #f #f '())))

(define (persona-service-identity-file service)
  "Path of the canonical persona text: the identity file of the persona
bundle, resolved against the load path.  Throws when not found."
  (or (persona-service-file-cache service)
      (begin
        (set-persona-service-file-cache!
         service
         (or (search-path %load-path (persona-service-rel-path service))
             (error (format #f "persona ~a: ~a not found on the load path"
                            (persona-service-id service)
                            (persona-service-rel-path service)))))
        (persona-service-file-cache service))))

(define (read-utf8-trimmed path)
  (string-trim-right
   (call-with-input-file path
     (lambda (port)
       (set-port-encoding! port "UTF-8")
       (get-string-all port)))
   #\newline))

(define (persona-service-identity service)
  "The persona: the content of the bundle's identity file with any
trailing newline trimmed.  Throws when the file is unreadable."
  (or (persona-service-identity-cache service)
      (begin
        (set-persona-service-identity-cache!
         service
         (read-utf8-trimmed (persona-service-identity-file service)))
        (persona-service-identity-cache service))))

(define (persona-service-directive-file service name)
  "Path of the shared directive NAME from this bundle's directive
library, resolved against the load path.  Throws when not found."
  (or (search-path %load-path
                   (format #f "geasa/persona-base/prompts/directives/~a.md" name))
      (error (format #f "persona ~a: no such directive: ~a"
                     (persona-service-id service) name))))

(define (persona-service-directive-text service name)
  "The text of shared directive NAME, or #f when the persona does not
include it."
  (let ((cached (assoc name (persona-service-directive-cache service))))
    (if cached
        (cdr cached)
        (and (member name (persona-service-directives service))
             (let ((text (read-utf8-trimmed
                          (persona-service-directive-file service name))))
               (set-persona-service-directive-cache!
                service
                (acons name text (persona-service-directive-cache service)))
               text)))))

(define (persona-service-prompt service)
  "The full persona prompt: the identity followed by every directive
the persona includes, in declaration order."
  (string-join
   (cons (persona-service-identity service)
         (filter-map (lambda (name)
                       (persona-service-directive-text service name))
                     (persona-service-directives service)))
   "\n\n"))
