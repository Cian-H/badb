;;; badb — Transaction compatibility shim
(define-module (badb transaction)
  #:use-module (badb kernel transaction)
  #:re-export (<transaction-manager>
               make-transaction-manager
               transaction-manager?
               transaction-manager-registry
               transaction-manager-journal
               stage-patch
               eval-patch
               apply-transaction!))