;;; geasa providers-model — Model Endpoint API (HTTP + SSE, JSON)
;;;
;;; Bundle plugin re-exporting all public symbols from the provider
;;; support modules (geasa providers-model common/openai/anthropic/mock).

(define-module (geasa providers-model plugin)
  #:use-module (geasa providers-model common)
  #:use-module (geasa providers-model openai)
  #:use-module (geasa providers-model anthropic)
  #:use-module (geasa providers-model mock)
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