;;; badb — Dynamic Project Context & Rules Assembly
;;;
;;; Backwards compatibility facade for (badb prompt), re-exporting all
;;; public symbols from unprivileged runtime subsystem in (badb runtimes prompt).

(define-module (badb prompt)
  #:use-module (badb runtimes prompt)
  #:re-export (<prompt-composer>
               make-prompt-composer
               prompt-composer?
               prompt-composer-system-identity
               prompt-composer-include-git-status?
               prompt-composer-include-file-tree?
               prompt-composer-file-tree-depth
               prompt-composer-file-tree-max-files
               prompt-composer-rule-files
               prompt-composer-custom-rules
               prompt-composer-tool-style
               prompt-composer-context-limit-chars
               set-prompt-composer-system-identity!
               set-prompt-composer-custom-rules!

               ;; Environment & Git Inspection
               get-git-branch
               get-git-status-summary
               build-workspace-file-tree
               collect-environment-info

               ;; Rules Discovery
               discover-project-rules
               read-rule-file

               ;; Tool Formatting
               format-tools-for-prompt

               ;; Prompt Assembly
               assemble-system-prompt
               default-system-prompt))
