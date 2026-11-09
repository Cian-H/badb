;;; badb — Registry compatibility shim
;;; DEV-DEBT [shim]: re-export of (badb kernel registry) kept for older callers
;;; (same for journal/digest/transaction.scm). Remove once no callers remain.
(define-module (badb registry)
               #:use-module (badb kernel registry)
               #:re-export (<registry>
                             make-registry
                             registry?
                             registry-attach!
                             registry-detach!
                             registry-lookup
                             registry-metadata
                             registry-list
                             registry-dispatch
                             make-fresh-feature-module
                             eval-in-feature-module
                             feature-module-define!
                             feature-module-ref))
