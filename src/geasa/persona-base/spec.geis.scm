;;; test-persona-base.scm — Tests for the Generic Persona Service
;;; (geasa persona-base plugin)
;;;
;;; The service behind every persona geasa: identity files resolve
;;; against the load path, the served identity is the file's content
;;; byte for byte, and services are independent (no shared cache).

(define-module (test test-persona-base)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (ice-9 textual-ports)
               #:use-module (geasa persona-base plugin))

(spec-begin "persona-base")

(define (fixture-text)
        (string-trim-right
          (call-with-input-file (spec-bundle-ref "fixtures/identity.md")
                                (lambda (port)
                                  (set-port-encoding! port "UTF-8")
                                  (get-string-all port)))
          #\newline))

;;;
;;; 1. Service construction
;;;

(test-group "persona-service-construction"
            (let ((svc (make-persona-service "fixture"
                                             #:identity-rel-path
                                             "geasa/persona-base/fixtures/identity.md")))
              (test-assert "persona-service? recognizes the record" (persona-service? svc))
              (test-equal "id preserved" "fixture" (persona-service-id svc))
              (test-assert "non-persona-service rejected" (not (persona-service? "nope"))))

            (test-assert "empty id rejected"
                         (catch #t
                           (lambda () (make-persona-service "") #f)
                           (lambda args #t)))
            (test-assert "non-string id rejected"
                         (catch #t
                           (lambda () (make-persona-service 42) #f)
                           (lambda args #t))))

;;;
;;; 2. The pinned-file contract
;;;

(test-group "persona-service-pinned-file-contract"
            (let ((svc (make-persona-service "fixture"
                                             #:identity-rel-path
                                             "geasa/persona-base/fixtures/identity.md")))
              (test-assert "identity file resolves on the load path"
                           (string? (persona-service-identity-file svc)))
              (test-assert "resolved file is this bundle's fixture"
                           (string=? (spec-bundle-ref "fixtures/identity.md")
                                     (persona-service-identity-file svc)))
              (test-equal "served identity equals the pinned file byte for byte"
                          (fixture-text) (persona-service-identity svc))
              (test-assert "identity has no trailing newline"
                           (not (string-suffix? "\n" (persona-service-identity svc))))
              (test-equal "identity is stable across calls"
                          (persona-service-identity svc) (persona-service-identity svc))))

;;;
;;; 3. Service independence
;;;

(test-group "persona-service-independence"
            (let* ((rel "geasa/persona-base/fixtures/identity.md")
                   (a (make-persona-service "fixture-a" #:identity-rel-path rel))
                   (b (make-persona-service "fixture-b" #:identity-rel-path rel)))
              (test-equal "distinct ids stay distinct"
                          "fixture-a" (persona-service-id a))
              (test-equal "second service id"
                          "fixture-b" (persona-service-id b))
              (test-equal "both services serve the same pinned text"
                          (persona-service-identity a) (persona-service-identity b))
              (test-assert "services share the file cache only per instance"
                           (and (string? (persona-service-identity-file a))
                                (string? (persona-service-identity-file b))))))

;;;
;;; 4. Shared directive features
;;;

(test-group "persona-service-directives"
            (let ((plain (make-persona-service "fixture"
                                               #:identity-rel-path
                                               "geasa/persona-base/fixtures/identity.md")))
              (test-equal "no directives by default" '() (persona-service-directives plain))
              (test-equal "prompt without directives is the identity"
                          (persona-service-identity plain) (persona-service-prompt plain))
              (test-equal "directive-text for a non-included directive is #f"
                          #f (persona-service-directive-text plain "scheme-structural-editing")))

            (let* ((rel "geasa/persona-base/fixtures/identity.md")
                   (svc (make-persona-service "fixture"
                                              #:identity-rel-path rel
                                              #:directives '("scheme-structural-editing"))))
              (test-equal "directives preserved in declaration order"
                          '("scheme-structural-editing") (persona-service-directives svc))
              (test-assert "directive text is non-empty and trimmed"
                           (and (string? (persona-service-directive-text svc "scheme-structural-editing"))
                                (> (string-length
                                     (persona-service-directive-text svc "scheme-structural-editing"))
                                   0)
                                (not (string-suffix?
                                       "\n"
                                       (persona-service-directive-text svc "scheme-structural-editing")))))
              (test-assert "scheme directive demands structural edits"
                           (string-contains
                             (persona-service-directive-text svc "scheme-structural-editing")
                             "structural-editing tools"))
              (test-assert "composed prompt starts with the identity"
                           (string-prefix? (persona-service-identity svc) (persona-service-prompt svc)))
              (test-assert "composed prompt includes the directive"
                           (string-contains (persona-service-prompt svc)
                                            "AST MCP structural-editing tools"))
              (test-assert "composed prompt separates identity and directive"
                           (string-contains (persona-service-prompt svc) "\n\n"))
              (test-equal "composed prompt is stable across calls"
                          (persona-service-prompt svc) (persona-service-prompt svc))
              (test-equal "identity alone stays directive-free"
                          (persona-service-identity svc) (fixture-text)))

            (test-assert "unknown directive names are rejected at construction"
                         (catch #t
                           (lambda ()
                             (make-persona-service "x" #:identity-rel-path "some/where.md"
                                                   #:directives '("no-such-directive"))
                             #f)
                           (lambda args #t)))
            (test-assert "non-list directives rejected"
                         (catch #t
                           (lambda () (make-persona-service "x" #:directives "not-a-list") #f)
                           (lambda args #t))))

(spec-end)
(spec-exit)

