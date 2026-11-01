;;; badb — Filetree MCP Server Facade
;;;
;;; Backwards compatibility facade for (badb filetree-mcp), re-exporting all
;;; public symbols from unprivileged capability subsystem in (badb capabilities filetree-mcp).

(define-module (badb filetree-mcp)
  #:use-module (badb capabilities filetree-mcp)
  #:re-export (%filetree-version
               %tree-entry-cap
               %find-result-cap
               %read-max-chars
               make-filetree-server
               run-filetree-mcp
               filetree-tree
               filetree-find
               filetree-outline
               filetree-read))
