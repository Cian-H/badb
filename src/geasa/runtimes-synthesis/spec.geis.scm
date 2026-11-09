;;; geis/runtimes-synthesis/spec.geis.scm — Sema synthesis workflow geis bundle.
;;;
;;; Roadmap Phase 3 Point 1:
;;; - Sema plugin-synthesis workflow: given a locked spec.scm, run a Sema workflow
;;;   (defagent, AST MCP + spec runner tools, llm/with-budget, cassette-backed tests)
;;;   that iteratively synthesizes the Guile module from structured counterexample feedback
;;;   until all contracts pass, then triggers kernel commit + hot-attach.

(define-module (test test-synthesis)
               #:use-module (badb kernel digest)
               #:use-module (badb spec)
               #:use-module (badb kernel journal)
               #:use-module (badb kernel registry)
               #:use-module (badb kernel geis-gate)
               #:use-module (geasa runtimes-synthesis plugin)
               #:use-module (geasa foundation-config plugin)
               #:use-module (badb kernel transaction)
               #:use-module (geasa surfaces-tui plugin)
               #:use-module (ice-9 textual-ports)
               #:use-module (json)
               #:use-module (srfi srfi-64))

(spec-begin "runtimes-synthesis")

(define %temp-dir (mkdtemp "/tmp/badb-synthesis-test-XXXXXX"))
(define %target-path "test/fixtures/lib/math-utils.scm")
(define %spec-source (spec-bundle-ref "fixtures/spec-pass.scm"))
(define %feature-id "math-utils-synthesized")
(define %cassette (spec-bundle-ref "cassettes/synthesis-fixture.jsonl"))
(define %persona (persona-prompt-text "persona-synthesizer"))

(define %original-target-content
        (call-with-input-file %target-path get-string-all))

(define (write-broken-math-utils!)
        (call-with-output-file %target-path
                               (lambda (p)
                                 (display "(define-module (test fixtures lib math-utils)\n  #:export (clamp double))\n\n(define (clamp lo hi x)\n  (min hi (max lo x)))\n\n(define (double x)\n  (+ x 1))\n" p))))

(define (restore-math-utils!)
        (call-with-output-file %target-path
                               (lambda (p)
                                 (display %original-target-content p))))

;; Ensure spec is locked for the feature
(lock-spec %spec-source %feature-id)

;;;
;;; 1. Sema binary discovery
;;;
(test-group "sema-discovery"
            (let ((bin (find-sema-executable)))
              (test-assert "find-sema-executable returns a non-empty string"
                           (and (string? bin) (not (string-null? bin))))
              (test-assert "located sema binary exists on disk"
                           (file-exists? bin))))

;;;
;;; 2. Spec locking helper
;;;
(test-group "spec-locking"
            (let ((hash (lock-feature-spec %spec-source "synth-lock-helper-test")))
              (test-assert "lock-feature-spec returns 64-char hex digest"
                           (and (string? hash) (= (string-length hash) 64)))
              (test-assert "locked spec file exists in kernel locked-spec store"
                           (file-exists? (locked-spec-path "synth-lock-helper-test")))))

;;;
;;; 2b. Synthesis agent role resolution
;;;
(test-group "synthesis-role-resolution"
            (let ((text (persona-prompt-text "persona-synthesizer")))
              (test-assert "persona-prompt-text serves the composed persona"
                           (string-contains text "badb synthesizer"))
              (test-assert "composed persona carries the structural-editing directive"
                           (string-contains text "AST MCP structural-editing tools"))
              (test-equal "persona text matches the spec fixture persona"
                          %persona text))

            ;; Bare identity fallback: persona-badb exports no persona-prompt, so
            ;; persona-prompt-text falls back to persona-identity.
            (test-assert "persona-prompt-text falls back to the bare identity"
                         (string-contains (persona-prompt-text "persona-badb") "You are badb"))

            ;; Role resolution against a fixture config.
            (let* ((cfg (parse-config-sexp
                          '(badb-config
                            (options
                             (agents
                              (synthesis
                               (provider "mistral")
                               (model "mistral-large-latest")
                               (persona "persona-synthesizer")))
                             (providers
                              (mistral (endpoint "https://api.mistral.ai/v1")
                               (api-key "mstrl-test")
                               (models ("mistral-large-latest"))))))))
                   (role (resolve-synthesis-role cfg)))
              (test-equal "role resolves the persona id"
                          "persona-synthesizer" (assoc-ref role 'persona-id))
              (test-equal "role resolves the composed persona text"
                          %persona (assoc-ref role 'persona-text))
              (test-equal "role resolves the model"
                          "mistral-large-latest" (assoc-ref role 'model))
              (test-equal "role resolves the provider"
                          "mistral" (assoc-ref role 'provider))
              (test-equal "role resolves the provider endpoint"
                          "https://api.mistral.ai/v1" (assoc-ref role 'endpoint))
              (test-equal "role resolves the provider api key"
                          "mstrl-test" (assoc-ref role 'api-key)))

            ;; The provider-credential bridge: a mistral role exports
            ;; MISTRAL_API_KEY to the sema subprocess when the env lacks it.
            (let ((role (resolve-synthesis-role
                          (parse-config-sexp
                            '(badb-config
                              (options
                               (agents (synthesis (provider "mistral")))
                               (providers
                                (mistral (endpoint "https://api.mistral.ai/v1")
                                 (api-key "mstrl-secret")
                                 (models ("mistral-large-latest"))))))))))
              (test-equal "mistral role exports MISTRAL_API_KEY"
                          '(("MISTRAL_API_KEY" . "mstrl-secret"))
                          (synthesis-env-alist role)))
            (let ((role (resolve-synthesis-role (make-badb-config))))
              (test-equal "no provider -> no env exports"
                          '() (synthesis-env-alist role)))
            (let ((role `((endpoint . "http://127.0.0.1:1234/v1")
                          (api-key . "local"))))
              (test-equal "non-mistral endpoint -> no env exports"
                          '() (synthesis-env-alist role)))

            ;; The provider hop: non-native OpenAI-compatible endpoints thread
            ;; through to the workflow for llm/configure; native ones (mistral)
            ;; stay with sema's own providers via the env bridge.
            (let ((lm-role (resolve-synthesis-role
                             (parse-config-sexp
                               '(badb-config
                                 (options
                                  (agents (synthesis (provider "lmstudio")))
                                  (providers
                                   (lmstudio (endpoint "http://127.0.0.1:1234/v1")
                                    (api-key "lm-studio")
                                    (models ("minicpm5-2b"))))))))))
              (test-equal "lmstudio endpoint threads to the workflow"
                          '((llm-endpoint . "http://127.0.0.1:1234/v1") (llm-api-key . "lm-studio"))
                          (synthesis-llm-args lm-role)))
            (let ((ms-role (resolve-synthesis-role
                             (parse-config-sexp
                               '(badb-config
                                 (options
                                  (agents (synthesis (provider "mistral")))
                                  (providers
                                   (mistral (endpoint "https://api.mistral.ai/v1")
                                    (api-key "mstrl-x")
                                    (models ("mistral-large-latest"))))))))))
              (test-equal "mistral stays with sema's native provider (env bridge)"
                          '() (synthesis-llm-args ms-role)))
            (test-equal "no provider -> no llm args"
                        '() (synthesis-llm-args (resolve-synthesis-role (make-badb-config))))

            ;; Project-local synthesis landing: synthesized geasa default to the
            ;; project's geis/ root (BADB_GEIS_PATH-overridable), never the
            ;; harness source tree, and installs report errors instead of throwing.
            (test-group "project-local-synthesis-targets"
                        (test-equal "default root is the project-local geis/ directory"
                                    "geis" (default-synthesis-root))
                        (test-equal "default target under the project geis root"
                                    "geis/greeter/plugin.scm" (default-synthesis-target "greeter"))
                        (test-equal "BADB_GEIS_PATH overrides the root"
                                    "/custom/root"
                                    (begin (setenv "BADB_GEIS_PATH" "/custom/root:/other")
                                           (let ((r (default-synthesis-root)))
                                             (unsetenv "BADB_GEIS_PATH")
                                             r)))
                        (test-equal "feature-id is sanitized in the target"
                                    "geis/weird_id/plugin.scm" (default-synthesis-target "weird id"))
                        (test-equal "bundle dir follows an explicit plugin target"
                                    "geis/greeter" (synthesis-bundle-dir "greeter" "geis/greeter/plugin.scm"))
                        (test-equal "bundle dir defaults to the project geis root"
                                    "geis/greeter" (synthesis-bundle-dir "greeter" #f))
                        (test-assert "bundle dir never lands in the harness source tree"
                                     (not (string-prefix? "src/" (synthesis-bundle-dir "greeter" #f))))
                        (let ((report (install-synthesized-bundle!
                                        "no-such-fid" "/dev/null/impossible/plugin.scm")))
                          (test-equal "failed install reports an error status, never throws"
                                      "error" (assoc-ref report 'install-status))))

            ;; Defaults: no config -> persona-synthesizer, no provider/model.
            (let ((role (resolve-synthesis-role (make-badb-config))))
              (test-equal "default persona is persona-synthesizer"
                          "persona-synthesizer" (assoc-ref role 'persona-id))
              (test-assert "default persona text still resolves"
                           (string-contains (assoc-ref role 'persona-text) "badb synthesizer"))
              (test-assert "no provider by default" (not (assoc-ref role 'provider)))
              (test-assert "no model by default" (not (assoc-ref role 'model)))))

;;;
;;; 3. Deterministic cassette-backed synthesis workflow
;;;
(test-group "cassette-backed-synthesis"
            (dynamic-wind
              (lambda () #f)
              (lambda ()
                ;; Initialize target file in a broken state
                (write-broken-math-utils!)

                ;; Verify that the broken file fails contract verification initially
                (let ((initial-verify (verify-locked-spec %feature-id)))
                  (test-equal "initial spec verification fails on broken double"
                              "fail" (assoc-ref initial-verify 'verdict)))

                ;; Run the synthesis workflow in replay mode using the recorded cassette
                (let* ((res (run-synthesis-workflow %feature-id
                                                    #:spec-path (locked-spec-path %feature-id)
                                                    #:target-path %target-path
                                                    #:persona %persona
                                                    #:cassette %cassette
                                                    #:cassette-mode "replay"
                                                    #:run-dir (string-append %temp-dir "/runs")))
                       (wf-result (assoc-ref res 'result))
                       (pins (and wf-result (assoc-ref wf-result "pins"))))

                  (test-equal "workflow execution status is success"
                              "success" (assoc-ref res 'status))
                  (test-equal "workflow exit code is 0"
                              0 (assoc-ref res 'exit-code))
                  (test-assert "workflow result payload present"
                               (and (pair? wf-result) (list? wf-result)))
                  (test-equal "synthesis verdict is pass"
                              "pass" (assoc-ref wf-result "verdict"))

                  ;; Verify identity pins on the kernel commit
                  (test-assert "pins structure present" (pair? pins))
                  (test-equal "commit status is committed"
                              "committed" (assoc-ref pins "status"))
                  (test-equal "quickcheck verdict pinned as pass"
                              "pass" (assoc-ref pins "quickcheck-verdict"))
                  (test-equal "quickcheck seed pinned"
                              "42" (assoc-ref pins "quickcheck-seed"))
                  (test-equal "kernel replay verdict pinned as passed"
                              "passed" (assoc-ref pins "kernel-replay-verdict"))
                  (test-assert "spec-hash pinned as 64-char hex"
                               (and (string? (assoc-ref pins "spec-hash"))
                                    (= 64 (string-length (assoc-ref pins "spec-hash")))))
                  (test-assert "ast-diff-hash pinned as 64-char hex"
                               (and (string? (assoc-ref pins "ast-diff-hash"))
                                    (= 64 (string-length (assoc-ref pins "ast-diff-hash")))))

                  ;; Verify that target file on disk was repaired by the agent
                  (let ((repaired-content (call-with-input-file %target-path get-string-all)))
                    (test-assert "target file now contains (+ x x)"
                                 (string-contains repaired-content "(+ x x)")))))
              (lambda ()
                (restore-math-utils!))))

;;;
;;; 4. High-level synthesize-feature! with hot-attach
;;;
(test-group "synthesize-feature-hot-attach"
            (dynamic-wind
              (lambda () #f)
              (lambda ()
                (write-broken-math-utils!)
                (let* ((jrn-path (string-append %temp-dir "/attach-journal.jsonl"))
                       (jrn (make-journal jrn-path))
                       (reg (make-registry))
                       (mgr (make-transaction-manager reg jrn))
                       (res (synthesize-feature! %feature-id
                                                 #:spec-path (locked-spec-path %feature-id)
                                                 #:target-path %target-path
                                                 #:mgr mgr
                                                 #:persona %persona
                                                 #:cassette %cassette
                                                 #:cassette-mode "replay"
                                                 #:run-dir (string-append %temp-dir "/attach-runs"))))
                  (test-equal "synthesize-feature! reports success"
                              "success" (assoc-ref res 'status))
                  ;; Check that feature is attached in the registry
                  (test-assert "synthesized feature is attached to registry"
                               (registry-lookup reg %feature-id))
                  ;; Check that journal records the commit checkpoint with identity pins
                  (let ((latest (journal-latest jrn)))
                    (test-assert "journal commit entry exists" latest)
                    (test-equal "journal checkpoint is commit"
                                "commit" (assoc-ref latest "checkpoint"))
                    (test-equal "journal pins quickcheck verdict"
                                "pass" (assoc-ref latest "quickcheck-verdict"))
                    (test-equal "journal pins kernel replay verdict"
                                "passed" (assoc-ref latest "kernel-replay-verdict")))))
              (lambda ()
                (restore-math-utils!))))

;;;
;;; 5. Budget enforcement
;;;
(test-group "budget-enforcement"
            ;; When budget limit is exceeded (e.g. 1 token cap), workflow fails closed
            (let* ((res (run-synthesis-workflow %feature-id
                                                #:spec-path (locked-spec-path %feature-id)
                                                #:target-path %target-path
                                                #:budget '((max-tokens . 1))
                                                #:persona %persona
                                                #:cassette %cassette
                                                #:cassette-mode "replay"
                                                #:run-dir (string-append %temp-dir "/budget-runs")))
                   (wf-result (assoc-ref res 'result)))
              (test-equal "budget-exceeded workflow reports failed status"
                          "failed" (assoc-ref res 'status))
              (test-assert "error details indicate budget was exceeded"
                           (let ((err-str (or (and wf-result (assoc-ref wf-result "error"))
                                              (assoc-ref res 'stdout)
                                              "")))
                             (string-contains err-str "budget exceeded")))))

;;;
;;; 6. Failing spec reverts and fails closed
;;;
(test-group "failing-spec-gate-reverts"
            (let* ((fail-id "synthesis-fail-gate-test")
                   (fail-spec "test/fixtures/spec-fail.scm")
                   (jrn-path (string-append %temp-dir "/fail-journal.jsonl"))
                   (jrn (make-journal jrn-path))
                   (reg (make-registry))
                   (mgr (make-transaction-manager reg jrn)))
              (lock-spec fail-spec fail-id)
              (let ((res (commit-synthesized-module! mgr fail-id %target-path)))
                (test-equal "commit-synthesized-module! reverts on failing spec"
                            "reverted" (assoc-ref res 'status))
                (test-assert "reverted feature is not in the registry"
                             (not (registry-lookup reg fail-id)))
                (let ((latest (journal-latest jrn)))
                  (test-equal "revert checkpoint is logged"
                              "revert" (assoc-ref latest "checkpoint"))))))

;;;
;;; 7. TUI tool registration
;;;
(test-group "tui-tool-registration"
            (let ((reg (make-registry)))
              (register-spec-tools! reg)
              (test-assert "spec.synthesize is registered as a procedure"
                           (procedure? (registry-lookup reg "spec.synthesize")))
              (test-assert "spec.synthesize metadata describes synthesis workflow"
                           (let ((desc (assoc-ref (registry-metadata reg "spec.synthesize") 'description)))
                             (and (string? desc)
                                  (string-contains desc "Sema plugin synthesis workflow"))))))

(spec-end)
(spec-exit)
