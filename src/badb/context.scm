;;; badb — Pluggable Context Compaction & Token Management
;;;
;;; Backwards compatibility facade for (badb context), re-exporting all
;;; public symbols from unprivileged runtime subsystem in (badb runtimes context).

(define-module (badb context)
  #:use-module (badb runtimes context)
  #:re-export (<context-compactor>
               make-context-compactor
               context-compactor?
               context-compactor-max-tokens
               context-compactor-reserve-tokens
               context-compactor-estimator
               context-compactor-strategy
               context-compactor-pinned-roles
               context-compactor-effective-ceiling
               set-context-compactor-max-tokens!
               set-context-compactor-reserve-tokens!
               set-context-compactor-estimator!
               set-context-compactor-strategy!
               char-heuristic-estimator
               word-heuristic-estimator
               estimate-message-tokens
               estimate-conversation-tokens
               strategy-prune-tool-results
               strategy-sliding-window
               strategy-summarize-older-turns
               strategy-journal-offload
               strategy-pipeline
               strategy-cascading
               latest-user-message
               compaction-needed?
               compact-messages))
