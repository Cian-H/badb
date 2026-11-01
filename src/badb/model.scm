;;; badb — Model Endpoint API (HTTP + SSE, JSON)
;;;
;;; Backwards compatibility facade for (badb model), re-exporting all
;;; public symbols from unprivileged provider subsystems in (badb providers *).

(define-module (badb model)
  #:use-module (badb providers common)
  #:use-module (badb providers openai)
  #:use-module (badb providers anthropic)
  #:use-module (badb providers mock)
  #:re-export (<model-client>
               make-model-client
               model-client?
               model-client-endpoint
               model-client-api-key
               model-client-model
               valid-api-message?
               normalize-messages
               format-chat-request
               parse-sse-line
               read-sse-events
               extract-chat-delta
               extract-chat-delta-item
               extract-chat-content
               extract-chat-tool-calls
               extract-usage
               current-http-post
               default-http-post
               current-http-get
               %request-timeout-seconds
               extract-tool-call-deltas
               merge-tool-call-fragment
               finalize-tool-call-fragments
               parse-models-response
               generic-model?
               generic-model-name?
               choose-model
               list-models
               model-available?
               auto-detect-model
               model-complete
               model-stream))