;;; geasa providers-model — Model Endpoint API (HTTP + SSE, JSON)
;;;
;;; Bundle plugin re-exporting all public symbols from the provider
;;; support modules (geasa providers-model common/openai/anthropic/cache/profiles/mock).

(define-module (geasa providers-model plugin)
               #:use-module
               (geasa providers-model common)
               #:use-module
               (geasa providers-model openai)
               #:use-module
               (geasa providers-model anthropic)
               #:use-module
               (geasa providers-model cache)
               #:use-module
               (geasa providers-model profiles)
               #:use-module
               (geasa providers-model mock)
               #:re-export
               (current-send-reasoning? <model-client>
                                        make-model-client
                                        model-client?
                                        model-client-endpoint
                                        model-client-api-key
                                        model-client-model
                                        model-client-allow-missing-thinking?
                                        model-client-cache-markers?
                                        valid-api-message?
                                        validate-conversation-messages
                                        repair-conversation-messages
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
                                        current-http-retry-sleeper
                                        current-on-http-retry
                                        parse-retry-delay-seconds
                                        classify-http-error
                                        http-post-with-retry
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
                                        model-stream
                                        ephemeral-cache-control
                                        apply-cache-breakpoints
                                        strip-cache-markers
                                        ollama-endpoint
                                        vllm-endpoint
                                        gemini-endpoint
                                        mistral-endpoint
                                        anthropic-endpoint
                                        make-ollama-client
                                        make-vllm-client
                                        make-gemini-client
                                        make-mistral-client
                                        make-anthropic-client
                                        make-provider-client
                                        known-provider-profiles))