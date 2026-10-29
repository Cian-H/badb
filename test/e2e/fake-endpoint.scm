#!/usr/bin/env -S guile --no-auto-compile
!#
;; test/e2e/fake-endpoint.scm - static OpenAI-compatible endpoint for e2e.
;;
;; A deliberately dumb server: every POST gets the same canned chat
;; completion, GET /v1/models lists the fake model. The TUI's streaming
;; reader accepts a plain single-line completion body even when it asked
;; to stream (the non-SSE path in read-sse-events), so no SSE framing is
;; needed. Bind loopback only; port via E2E_FAKE_PORT (default 18034).
;;
;; Used by test/e2e/10-chat-roundtrip.sh when no real E2E_CHAT_ENDPOINT
;; is configured, so the chat scenario is fully hermetic.

(use-modules (web server) (web request) (web uri))

(define %port
  (or (and=> (getenv "E2E_FAKE_PORT") string->number) 18034))

(define %fake-response-content "e2e fake model ready.")

(define %completion
  (string-append
    "{\"id\":\"fake-completion-1\","
    "\"object\":\"chat.completion\","
    "\"created\":0,"
    "\"model\":\"fake-model\","
    "\"choices\":[{\"index\":0,"
    "\"message\":{\"role\":\"assistant\","
    "\"content\":\"" %fake-response-content "\"},"
    "\"finish_reason\":\"stop\"}],"
    "\"usage\":{\"prompt_tokens\":1,\"completion_tokens\":6,"
    "\"total_tokens\":7}}"))

(define %models
  (string-append
    "{\"object\":\"list\","
    "\"data\":[{\"id\":\"fake-model\",\"object\":\"model\","
    "\"created\":0,\"owned_by\":\"e2e\"}]}"))

(define (handler request body)
  (let ((path (uri-path (request-uri request))))
    (values
      '((content-type . (application/json)))
      (if (string-suffix? "/models" path) %models %completion))))

(format (current-error-port)
        "fake endpoint on 127.0.0.1:~a (model: fake-model)~%"
        %port)
(run-server handler 'http `(#:addr ,INADDR_LOOPBACK #:port ,%port))
