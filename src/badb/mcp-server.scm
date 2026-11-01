;;; badb — Minimal MCP Server Core Facade
;;;
;;; Backwards compatibility facade for (badb mcp-server), re-exporting all
;;; public symbols from unprivileged capability subsystem in (badb capabilities mcp-server).

(define-module (badb mcp-server)
  #:use-module (badb capabilities mcp-server)
  #:re-export (<mcp-server>
               make-mcp-server
               mcp-server?
               mcp-server-name
               mcp-server-version
               mcp-server-tools
               mcp-server-handlers
               mcp-serve-stdio
               write-response!
               result-response
               error-response
               handle-message))
