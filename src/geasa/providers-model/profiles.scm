;;; badb — Native Provider Endpoint Profiles
;;;
;;; Preset <model-client> constructors for common model providers:
;;; local Ollama and vLLM servers, Google Gemini's OpenAI-compatible
;;; endpoint, and the strict Mistral and Anthropic cloud APIs.  Each
;;; profile pins the endpoint and wire strictness (allow-missing-thinking,
;;; cache markers) to match the provider's documented behavior, and reads
;;; credentials from the provider's conventional environment variables.

(define-module (geasa providers-model profiles)
               #:use-module
               (geasa providers-model common)
               #:use-module
               (web uri)
               #:export
               (ollama-endpoint vllm-endpoint
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

(define (first-env . vars)
        "Return the first non-empty value among the environment variables
VARS, or #f."
        (let loop ((rest vars))
                  (if (null? rest)
                    #f
                    (let ((v (getenv (car rest))))
                      (if (and v (not (string-null? (string-trim-both v))))
                        v
                        (loop (cdr rest)))))))

(define (normalize-base-url base)
        "Ensure BASE carries an http(s) scheme and no trailing slashes,
so request dispatch (resolve-endpoint-uri) can append /chat/completions."
        (let* ((trimmed (string-trim-both base))
               (with-scheme
                 (if (or (string-prefix? "http://" trimmed)
                         (string-prefix? "https://" trimmed))
                   trimmed
                   (string-append "http://" trimmed))))
          (string-trim-right with-scheme #\/)))

(define (with-api-base base)
        "Ensure BASE points at the provider's OpenAI-compatible /v1 API
segment: a bare host or host:port receives /v1 appended, while URLs
that already carry a real path (e.g. .../v1 or a full chat-completions
URL) pass through untouched."
        (let*
            ((normalized (normalize-base-url base))
             (path (string-trim-both (or (and=> (string->uri normalized) uri-path) "") #\/)))
          (if (string-null? path)
            (string-append normalized "/v1")
            normalized)))

(define (ollama-endpoint)
        "Ollama OpenAI-compatible base URL: OLLAMA_ENDPOINT (full base-URL
override), else OLLAMA_HOST (scheme optional), else localhost:11434."
        (or
          (and=> (first-env "OLLAMA_ENDPOINT" "OLLAMA_HOST") with-api-base)
          "http://localhost:11434/v1"))

(define (vllm-endpoint)
        "vLLM OpenAI-compatible base URL: VLLM_ENDPOINT or VLLM_HOST
(scheme optional), else localhost:8000."
        (or
          (and=> (first-env "VLLM_ENDPOINT" "VLLM_HOST") with-api-base)
          "http://localhost:8000/v1"))

(define (gemini-endpoint)
        "Google Gemini OpenAI-compatible base URL (the OpenAI-compat
bridge on generativelanguage.googleapis.com)."
        "https://generativelanguage.googleapis.com/v1beta/openai")

(define (mistral-endpoint)
        "Mistral La Plateforme OpenAI-compatible base URL."
        "https://api.mistral.ai/v1")

(define (anthropic-endpoint)
        "Anthropic Messages API (OpenAI-compat bridge) base URL."
        "https://api.anthropic.com/v1")

(define* (make-ollama-client #:key (model #f) (endpoint #f) (api-key #f))
         "Client preset for a local Ollama server: OpenAI-compatible
endpoint (OLLAMA_HOST / OLLAMA_ENDPOINT), Qwen3-style
allow_missing_thinking enabled for local reasoning models, and no
cache markers (Ollama has implicit prompt caching)."
         (make-model-client #:endpoint (or endpoint (ollama-endpoint))
                            #:api-key (or api-key (first-env "OLLAMA_API_KEY"))
                            #:model (or model "default")
                            #:allow-missing-thinking? #t
                            #:cache-markers? #f))

(define* (make-vllm-client #:key (model #f) (endpoint #f) (api-key #f))
         "Client preset for a vLLM OpenAI-compatible server: VLLM_ENDPOINT
or VLLM_HOST base URL, strict wire profile (vLLM rejects unknown body
fields), and no cache markers (vLLM does automatic prefix caching)."
         (make-model-client #:endpoint (or endpoint (vllm-endpoint))
                            #:api-key (or api-key (first-env "VLLM_API_KEY"))
                            #:model (or model "default")
                            #:allow-missing-thinking? #f
                            #:cache-markers? #f))

(define* (make-gemini-client #:key (model #f) (endpoint #f) (api-key #f))
         "Client preset for Google Gemini via its OpenAI-compatible
endpoint: GEMINI_API_KEY (or GOOGLE_API_KEY) as bearer token, strict
wire profile (Google rejects unknown body fields), model defaulting
to gemini-2.0-flash when MODEL is not given."
         (make-model-client #:endpoint (or endpoint (gemini-endpoint))
                            #:api-key (or api-key (first-env "GEMINI_API_KEY" "GOOGLE_API_KEY"))
                            #:model (or model "gemini-2.0-flash")
                            #:allow-missing-thinking? #f
                            #:cache-markers? #f))

(define* (make-mistral-client #:key (model #f) (endpoint #f) (api-key #f))
         "Client preset for the Mistral La Plateforme API: MISTRAL_API_KEY
bearer token, strict wire profile (api.mistral.ai rejects unknown body
fields and reasoning traces), automatic server-side caching (no
explicit markers)."
         (make-model-client #:endpoint (or endpoint (mistral-endpoint))
                            #:api-key (or api-key (first-env "MISTRAL_API_KEY" "MISTRAL_KEY"))
                            #:model (or model "mistral-large-latest")
                            #:allow-missing-thinking? #f
                            #:cache-markers? #f))

(define* (make-anthropic-client #:key (model #f) (endpoint #f) (api-key #f))
         "Client preset for the Anthropic Messages API via its OpenAI-compat
bridge: ANTHROPIC_API_KEY bearer token, strict wire profile, and
explicit ephemeral cache_control markers on the stable system prompt
prefix (Anthropic charges dramatically less for cache-hit input
tokens)."
         (make-model-client #:endpoint (or endpoint (anthropic-endpoint))
                            #:api-key (or api-key (first-env "ANTHROPIC_API_KEY"))
                            #:model (or model "default")
                            #:allow-missing-thinking? #f
                            #:cache-markers? #t))

(define known-provider-profiles '(ollama vllm gemini mistral anthropic))

(define* (make-provider-client profile
                               #:key
                               (model #f)
                               (endpoint #f)
                               (api-key #f))
         "Construct a <model-client> for the named provider PROFILE using
its preset endpoint and wire profile.  PROFILE is one of
known-provider-profiles: 'ollama, 'vllm, 'gemini, 'mistral, or
'anthropic.  ENDPOINT and API-KEY default to the profile's
conventional environment variable; MODEL defaults to the profile's
own default (\"default\" lets auto-detect-model pick from /models)."
         (case profile
           ((ollama) (make-ollama-client #:model model #:endpoint endpoint #:api-key api-key))
           ((vllm) (make-vllm-client #:model model #:endpoint endpoint #:api-key api-key))
           ((gemini) (make-gemini-client #:model model #:endpoint endpoint #:api-key api-key))
           ((mistral) (make-mistral-client #:model model #:endpoint endpoint #:api-key api-key))
           ((anthropic) (make-anthropic-client #:model model #:endpoint endpoint #:api-key api-key))
           (else (error "make-provider-client: unknown provider profile" profile))))
