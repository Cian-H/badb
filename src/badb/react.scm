;;; badb — Autonomous ReAct Tool-Calling Engine
;;;
;;; Backwards compatibility facade for (badb react), re-exporting all
;;; public symbols from unprivileged runtime subsystem in (badb runtimes react).

(define-module (badb react)
  #:use-module (badb runtimes react)
  #:re-export (<react-session>
               <steering-queue>
               <react-agent>
               make-react-agent
               react-agent?
               react-step
               react-run
               make-react-session
               react-session?
               react-session-messages
               react-session-status
               react-session-iteration
               react-session-max-iterations
               react-session-client
               react-session-reg
               react-session-mcp-mgr
               react-session-jrn
               react-session-mgr
               react-session-hooks
               react-session-steering-queue
               react-session-max-tokens
               react-session-compactor
               set-react-session-messages!
               set-react-session-status!
               set-react-session-iteration!
               set-react-session-max-tokens!
               set-react-session-compactor!
               set-react-session-steering-queue!
               make-steering-queue
               steering-queue?
               steering-push!
               steering-drain!
               steering-empty?
               steering-count
               collect-available-tools
               registry-feature->tool-spec
               mcp-tool->tool-spec
               sanitize-tool-parameters
               parse-tool-calls
               determine-step-content
               reasoning-fallback-content
               stream-model-completion
               parse-xml-tool-calls
               execute-tool-call
               react-step!
               react-run!))
