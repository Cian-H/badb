;;; (badb geis capabilities) — dynamically scoped object capabilities for geis tools
(define-module (badb geis capabilities)
               #:use-module (srfi srfi-1)
               #:export (current-geis-capabilities geis-capability))

(define current-geis-capabilities (make-parameter '()))

(define* (geis-capability name #:optional (target #f))
         "Return the granted closure for capability NAME and optional TARGET.
Raise an error when the capability was not declared and granted."
         (let ((entry (find (lambda (grant)
                              (and (equal? (caar grant) name)
                                   (or (not target) (equal? (cadar grant) target))))
                            (current-geis-capabilities))))
           (if entry
             (cdr entry)
             (error "geis capability is not available" name target))))
