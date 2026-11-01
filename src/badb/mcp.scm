;;; badb — MCP Client Facade (JSON-RPC 2.0 over stdio)
;;;
;;; Backwards compatibility facade for (badb mcp), re-exporting all
;;; public symbols and record type descriptors from unprivileged capability
;;; subsystem in (badb capabilities mcp).

(define-module (badb mcp)
  #:use-module (badb capabilities mcp)
  #:re-export (<mcp-client>
               make-mcp-client
               mcp-client?
               mcp-client-in-port
               mcp-client-out-port
               mcp-client-pid
               mcp-client-next-id
               mcp-client-closed?
               set-mcp-client-in-port!
               set-mcp-client-out-port!
               set-mcp-client-pid!
               set-mcp-client-next-id!
               set-mcp-client-closed?!
               <mcp-manager>
               make-mcp-manager
               mcp-manager?
               mcp-manager-config
               mcp-manager-servers
               mcp-manager-tool-index
               set-mcp-manager-servers!
               set-mcp-manager-config!
               set-mcp-manager-tool-index!
               <mcp-channel>
               make-mcp-channel
               mcp-channel?
               mcp-channel-in-port
               mcp-channel-out-port
               mcp-channel-pid
               mcp-channel-closed?
               mcp-channel-close!
               <mcp-tool>
               make-mcp-tool
               mcp-tool?
               mcp-tool-name
               mcp-tool-description
               mcp-tool-input-schema
               <mcp-prompt>
               make-mcp-prompt
               mcp-prompt?
               mcp-prompt-name
               mcp-prompt-description
               mcp-prompt-arguments
               <mcp-resource>
               make-mcp-resource
               mcp-resource?
               mcp-resource-uri
               mcp-resource-name
               mcp-resource-description
               mcp-resource-mime-type
               mcp-spawn-client
               mcp-send-request!
               mcp-send-notification!
               mcp-read-message
               mcp-call
               mcp-notify
               mcp-initialize
               mcp-list-tools
               mcp-call-tool
               mcp-close-client
               tools->list
               mcp-manager-list-servers
               mcp-manager-get-server
               mcp-manager-add-server!
               mcp-manager-remove-server!
               mcp-manager-start-server!
               mcp-manager-stop-server!
               mcp-manager-stop-all!
               mcp-manager-list-tools
               mcp-manager-call-tool
               mcp-manager-tool-index
               mcp-manager-rebuild-tool-index!))