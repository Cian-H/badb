;;; badb — Registry compatibility shim
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
