;;; badb — Transactional Time-Travel & Rollback Engine
;;;
;;; Backwards compatibility facade for (badb rollback), re-exporting all
;;; public symbols from unprivileged runtime subsystem in (badb runtimes rollback).

(define-module (badb rollback)
  #:use-module (badb runtimes rollback)
  #:re-export (<rollback-manager>
               make-rollback-manager
               rollback-manager?
               rollback-manager-workspace-dir
               rollback-manager-registry
               rollback-manager-journal
               rollback-manager-storage-dir
               rollback-manager-checkpoints

               <checkpoint>
               make-checkpoint
               checkpoint?
               checkpoint-id
               checkpoint-description
               checkpoint-timestamp
               checkpoint-files
               checkpoint-registry-state

               ;; Operations
               scan-text-files
               create-checkpoint!
               list-checkpoints
               get-checkpoint
               rollback-to-checkpoint!
               rollback-latest!
               delete-checkpoint!
               with-checkpoint))
