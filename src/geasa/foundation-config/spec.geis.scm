;;; test-config.scm — Tests for (geasa foundation-config plugin)
;;;
;;; Verifies hierarchical configuration loading, parsing, serialization,
;;; merging, and MCP server specification handling.

(define-module (test test-config)
               #:use-module (srfi srfi-1)
               #:use-module (badb spec)
               #:use-module (srfi srfi-64)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa foundation-config plugin))

(spec-begin "foundation-config")

;;;
;;; 1. Configuration Construction and Accessors
;;;
(test-group "config-construction"
            (let ((cfg (make-badb-config)))
              (test-assert "badb-config? recognizes config record" (badb-config? cfg))
              (test-assert "default model-name is string" (string? (badb-config-model-name cfg)))
              (test-assert "default model-endpoint is string" (string? (badb-config-model-endpoint cfg)))
              (test-equal "default temperature is 0.7" 0.7 (badb-config-model-temperature cfg))
              (test-equal "default max-tokens is 4096" 4096 (badb-config-model-max-tokens cfg))
              (test-equal "default journal path is .badb/journal.jsonl"
                          ".badb/journal.jsonl" (badb-config-journal-path cfg))
              (test-assert "default mcp-servers is list" (list? (badb-config-mcp-servers cfg))))

            (let ((custom (make-badb-config
                            #:model-name "custom-model:v1"
                            #:model-endpoint "http://test-server:8000"
                            #:model-temperature 0.2
                            #:model-max-tokens 8192
                            #:journal-path "/tmp/custom-jrn.jsonl")))
              (test-equal "custom model-name set" "custom-model:v1" (badb-config-model-name custom))
              (test-equal "custom endpoint set" "http://test-server:8000" (badb-config-model-endpoint custom))
              (test-equal "custom temperature set" 0.2 (badb-config-model-temperature custom))
              (test-equal "custom max-tokens set" 8192 (badb-config-model-max-tokens custom))
              (test-equal "custom journal path set" "/tmp/custom-jrn.jsonl" (badb-config-journal-path custom))))

;;;
;;; 2. MCP Server Specification Management
;;;
(test-group "config-mcp-servers"
            (let ((cfg (make-badb-config)))
              (test-assert "initially no mcp servers" (null? (badb-config-mcp-servers cfg)))

              ;; Add server
              (badb-config-add-mcp-server! cfg "filesystem" "npx" '("-y" "@mcp/fs" ".") #:auto-start #t)
              (test-equal "mcp-servers count is 1" 1 (length (badb-config-mcp-servers cfg)))

              (let ((fs-spec (badb-config-get-mcp-server cfg "filesystem")))
                (test-assert "retrieved filesystem spec" fs-spec)
                (test-equal "command matches" "npx" (assoc-ref fs-spec 'command))
                (test-equal "args match" '("-y" "@mcp/fs" ".") (assoc-ref fs-spec 'args))
                (test-equal "auto-start is #t" #t (assoc-ref fs-spec 'auto-start)))

              ;; Add second server
              (badb-config-add-mcp-server! cfg "memory" "mcp-server-memory")
              (test-equal "mcp-servers count is 2" 2 (length (badb-config-mcp-servers cfg)))

              ;; Update existing server
              (badb-config-add-mcp-server! cfg "filesystem" "deno" '("run" "fs.ts"))
              (test-equal "count remains 2 after update" 2 (length (badb-config-mcp-servers cfg)))
              (test-equal "command updated" "deno" (assoc-ref (badb-config-get-mcp-server cfg "filesystem") 'command))

              ;; Remove server
              (badb-config-remove-mcp-server! cfg "filesystem")
              (test-equal "count is 1 after removal" 1 (length (badb-config-mcp-servers cfg)))
              (test-assert "filesystem is gone" (not (badb-config-get-mcp-server cfg "filesystem")))
              (test-assert "memory remains" (badb-config-get-mcp-server cfg "memory"))))

;;;
;;; 3. S-Expression Parsing and Serialization
;;;
(test-group "config-sexp-parsing"
            (let* ((sexp '(badb-config
                           (model
                            (name "llama-3-8b")
                            (endpoint "http://ai.lan:11434")
                            (temperature 0.4))
                           (journal
                            (path "/var/log/badb.jsonl"))
                           (mcp
                            (server "git"
                             (command "mcp-git")
                             (args "--repo" "."))
                            (server "fetch"
                             (command "uvx" "mcp-fetch"))
                            (server "jev"
                             (command "npx")
                             (args "-y" "@jkudish/jev-mcp")
                             (env (("API_KEY" . "sec123")))))))
                   (cfg (parse-config-sexp sexp)))
              (test-equal "parsed model name" "llama-3-8b" (badb-config-model-name cfg))
              (test-equal "parsed endpoint" "http://ai.lan:11434" (badb-config-model-endpoint cfg))
              (test-equal "parsed temperature" 0.4 (badb-config-model-temperature cfg))
              (test-equal "parsed journal path" "/var/log/badb.jsonl" (badb-config-journal-path cfg))
              (test-equal "parsed 3 mcp servers" 3 (length (badb-config-mcp-servers cfg)))
              (test-assert "git server present" (badb-config-get-mcp-server cfg "git"))
              (test-assert "fetch server present" (badb-config-get-mcp-server cfg "fetch"))
              (let ((jev-spec (badb-config-get-mcp-server cfg "jev")))
                (test-assert "jev server present" jev-spec)
                (test-equal "jev env matches" '(("API_KEY" . "sec123")) (assoc-ref jev-spec 'env)))

              ;; Roundtrip back to s-expression and re-parse
              (let* ((serialized (config->sexp cfg))
                     (re-parsed (parse-config-sexp serialized)))
                (test-equal "roundtripped model name" (badb-config-model-name cfg) (badb-config-model-name re-parsed))
                (test-equal "roundtripped endpoint"
                            (badb-config-model-endpoint cfg) (badb-config-model-endpoint re-parsed))
                (test-equal "roundtripped journal path"
                            (badb-config-journal-path cfg) (badb-config-journal-path re-parsed))
                (test-equal "roundtripped server count"
                            (length (badb-config-mcp-servers cfg)) (length (badb-config-mcp-servers re-parsed))))))

;;;
;;; 4. File Read and Write Persistence
;;;
(test-group "config-file-io"
            (let* ((tmp-path (string-append "/tmp/badb-test-config-" (number->string (current-time)) ".scm"))
                   (cfg (make-badb-config
                          #:model-name "persisted-model"
                          #:model-endpoint "http://localhost:9999"
                          #:journal-path ".badb/test-jrn.jsonl")))
              (badb-config-add-mcp-server! cfg "mock-fs" "npx" '("mock"))

              ;; Write file
              (test-assert "write-config-file succeeds" (write-config-file cfg tmp-path))
              (test-assert "config file exists on disk" (file-exists? tmp-path))

              ;; Read file
              (let ((loaded (read-config-file tmp-path)))
                (test-assert "read-config-file returns config" (badb-config? loaded))
                (test-equal "loaded model name matches" "persisted-model" (badb-config-model-name loaded))
                (test-equal "loaded endpoint matches" "http://localhost:9999" (badb-config-model-endpoint loaded))
                (test-assert "loaded mcp server exists" (badb-config-get-mcp-server loaded "mock-fs"))
                (test-equal "source path is recorded" tmp-path (badb-config-source-path loaded)))))

;;;
;;; 5. Configuration Merging (Hierarchical Resolution)
;;;
(test-group "config-merging"
            (let ((global-cfg (make-badb-config
                                #:model-name "global-model"
                                #:model-endpoint "http://global-server:8000"
                                #:journal-path "/global/jrn.jsonl"))
                  (local-cfg (make-badb-config
                               #:model-name "project-model"
                               #:model-endpoint #f
                               #:journal-path ".badb/local.jsonl")))
              ;; Global has servers: git, memory
              (badb-config-add-mcp-server! global-cfg "git" "mcp-git" '("global-args"))
              (badb-config-add-mcp-server! global-cfg "memory" "mcp-memory")

              ;; Local has servers: git (overridden with local args), project-db (new)
              (badb-config-add-mcp-server! local-cfg "git" "mcp-git" '("local-args"))
              (badb-config-add-mcp-server! local-cfg "project-db" "sqlite-mcp" '("dev.db"))

              (let ((merged (merge-configs global-cfg local-cfg)))
                ;; Local overrides global model name
                (test-equal "local model-name overrides global" "project-model" (badb-config-model-name merged))
                ;; Global endpoint preserved if local was #f
                (test-equal "global endpoint preserved" "http://global-server:8000" (badb-config-model-endpoint merged))
                ;; Local journal path overrides global
                (test-equal "local journal path overrides global" ".badb/local.jsonl" (badb-config-journal-path merged))

                ;; MCP servers merged:
                ;; - "git" from local
                ;; - "project-db" from local
                ;; - "memory" retained from global
                (test-equal "merged mcp servers count is 3" 3 (length (badb-config-mcp-servers merged)))
                (test-equal "git server has local args"
                            '("local-args") (assoc-ref (badb-config-get-mcp-server merged "git") 'args))
                (test-assert "project-db is present" (badb-config-get-mcp-server merged "project-db"))
                (test-assert "global memory server is preserved" (badb-config-get-mcp-server merged "memory")))))

(test-group "config-providers"
            (let*
                ((sexp
                   '(badb-config
                     (model (name "default-model") (endpoint "http://localhost:8000"))
                     (options
                      (providers
                       (lmstudio (endpoint "http://127.0.0.1:1234/v1")
                        (api-key "lm-studio")
                        (models ("meta-llama-3.1-8b-instruct" "qwen2.5-coder")))
                       (ollama (endpoint "http://127.0.0.1:11434/v1")
                        (api-key "ollama")
                        (models "llama3.2"))))))
                 (cfg (parse-config-sexp sexp)))
              (test-assert
                "badb-config-providers returns list of providers"
                (pair? (badb-config-providers cfg)))
              (test-equal "badb-config-providers count is 2"
                          2
                          (length (badb-config-providers cfg)))
              (test-assert
                "badb-config-get-provider finds lmstudio by symbol"
                (badb-config-get-provider cfg 'lmstudio))
              (test-assert
                "badb-config-get-provider finds lmstudio by string"
                (badb-config-get-provider cfg "lmstudio"))
              (test-assert
                "badb-config-get-provider returns #f for unknown provider"
                (not (badb-config-get-provider cfg "nonexistent")))
              (test-assert "switch to lmstudio succeeds"
                           (badb-config-switch-provider! cfg "lmstudio"))
              (test-equal
                "active endpoint switched to lmstudio"
                "http://127.0.0.1:1234/v1"
                (badb-config-model-endpoint cfg))
              (test-equal
                "active model switched to first model in lmstudio list"
                "meta-llama-3.1-8b-instruct"
                (badb-config-model-name cfg))
              (test-equal
                "active api-key switched to lmstudio key"
                "lm-studio"
                (badb-config-model-api-key cfg))
              (test-assert "switch to ollama succeeds"
                           (badb-config-switch-provider! cfg 'ollama))
              (test-equal "active endpoint switched to ollama"
                          "http://127.0.0.1:11434/v1"
                          (badb-config-model-endpoint cfg))
              (test-equal
                "active model switched to ollama string model"
                "llama3.2"
                (badb-config-model-name cfg))
              (test-assert
                "switch to unknown provider returns #f"
                (not (badb-config-switch-provider! cfg "unknown")))))

(test-group "config-provider-endpoint-lookup"
            (let*
                ((sexp
                   '(badb-config
                     (model (name "default-model") (endpoint "http://localhost:8000"))
                     (options
                      (providers
                       (lmstudio (endpoint "http://127.0.0.1:1234/v1")
                        (api-key "lm-studio")
                        (models ("qwen3")))
                       (mistral (endpoint "https://api.mistral.ai/v1")
                        (api-key "mstrl-test")
                        (allow-missing-thinking #f)
                        (models ("mistral-large-latest")))))))
                 (cfg (parse-config-sexp sexp)))
              (test-assert "preset found by endpoint"
                           (badb-config-provider-for-endpoint cfg "https://api.mistral.ai/v1"))
              (test-assert "preset lookup by lmstudio endpoint"
                           (badb-config-provider-for-endpoint cfg "http://127.0.0.1:1234/v1"))
              (test-assert "unknown endpoint returns #f"
                           (not (badb-config-provider-for-endpoint cfg "http://nowhere:9/v1")))
              (test-assert "non-string endpoint returns #f"
                           (not (badb-config-provider-for-endpoint cfg 42)))
              (test-assert "mistral preset disables allow-missing-thinking"
                           (not (badb-config-allow-missing-thinking? cfg "https://api.mistral.ai/v1")))
              (test-assert "lmstudio preset keeps the default flag"
                           (badb-config-allow-missing-thinking? cfg "http://127.0.0.1:1234/v1"))
              (test-assert "endpoints without a preset default to the flag on"
                           (badb-config-allow-missing-thinking? cfg "http://localhost:8000"))
              ;; Switching to mistral and asking about the active endpoint
              ;; yields the strict profile.
              (badb-config-switch-provider! cfg 'mistral)
              (test-assert "active mistral endpoint is strict"
                           (not (badb-config-allow-missing-thinking?
                                  cfg (badb-config-model-endpoint cfg))))))

(test-group "config-agent-roles"
            (let*
                ((sexp
                   '(badb-config
                     (model (name "default-model") (endpoint "http://localhost:8000"))
                     (options
                      (agents
                       (synthesis
                        (provider "mistral")
                        (model "mistral-large-latest")
                        (persona "persona-synthesizer")))
                      (providers
                       (mistral (endpoint "https://api.mistral.ai/v1")
                        (api-key "mstrl-test")
                        (models ("mistral-large-latest")))))))
                 (cfg (parse-config-sexp sexp)))
              (test-assert "agents catalog present"
                           (pair? (badb-config-agents cfg)))
              (test-assert "synthesis role found by symbol"
                           (badb-config-get-agent cfg 'synthesis))
              (test-assert "synthesis role found by string"
                           (badb-config-get-agent cfg "synthesis"))
              (test-assert "unknown role returns #f"
                           (not (badb-config-get-agent cfg "reviewer")))
              (let ((role (badb-config-get-agent cfg "synthesis")))
                (test-equal "role provider field"
                            "mistral" (get-prop role 'provider #f))
                (test-equal "role model field"
                            "mistral-large-latest" (get-prop role 'model #f))
                (test-equal "role persona field"
                            "persona-synthesizer" (get-prop role 'persona #f)))
              ;; The role's provider names a real preset in the catalog.
              (test-assert "role provider resolves against provider presets"
                           (badb-config-get-provider cfg
                                                     (get-prop (badb-config-get-agent cfg "synthesis") 'provider #f)))
              ;; No agents section: empty catalog, no roles.
              (let ((bare (make-badb-config)))
                (test-equal "no agents section yields empty catalog"
                            '() (badb-config-agents bare))
                (test-assert "no roles on bare config"
                             (not (badb-config-get-agent bare "synthesis"))))))

(test-group "config-react-iterations"
            (let ((cfg (parse-config-sexp
                         '(badb-config
                           (options
                            (react (max-iterations 50)))))))
              (test-equal "react max-iterations read from options"
                          50 (badb-config-react-max-iterations cfg)))
            (test-assert "no react section -> unlimited (#f)"
                         (not (badb-config-react-max-iterations (make-badb-config))))
            (let ((cfg (parse-config-sexp
                         '(badb-config
                           (options
                            (react (max-iterations "lots")))))))
              (test-assert "non-numeric cap ignored -> unlimited"
                           (not (badb-config-react-max-iterations cfg))))
            (let ((cfg (parse-config-sexp
                         '(badb-config
                           (options
                            (react (max-iterations 0)))))))
              (test-assert "non-positive cap ignored -> unlimited"
                           (not (badb-config-react-max-iterations cfg)))))

;;;
;;; 6. UI Theme Configuration
;;;
(test-group "config-ui-theme"
            (let ((cfg (make-badb-config)))
              (test-assert "default ui-theme is #f" (not (badb-config-ui-theme cfg)))
              (badb-config-set-ui-theme! cfg "nord")
              (test-equal "ui-theme setter stores name" "nord" (badb-config-ui-theme cfg))
              (set-badb-config-ui-theme! cfg "paper")
              (test-equal "record setter alias works" "paper" (badb-config-ui-theme cfg))))

(test-group "config-ui-theme-parsing"
            (let ((cfg (parse-config-sexp '(badb-config
                                            (model (name "llama-3-8b"))
                                            (ui (theme "midnight"))))))
              (test-equal "parsed ui theme" "midnight" (badb-config-ui-theme cfg))
              (test-equal "parsed model name alongside ui"
                          "llama-3-8b" (badb-config-model-name cfg)))

            (let ((cfg (parse-config-sexp '(badb-config
                                            (model (name "llama-3-8b"))
                                            (journal (path "/var/log/badb.jsonl"))))))
              (test-assert "config without ui section still loads"
                           (badb-config? cfg))
              (test-assert "missing ui section yields #f theme"
                           (not (badb-config-ui-theme cfg))))

            (let ((cfg (parse-config-sexp '(badb-config (ui (theme nord))))))
              (test-equal "ui theme symbol coerced to string" "nord" (badb-config-ui-theme cfg))))

(test-group "config-ui-theme-roundtrip"
            (let* ((cfg (make-badb-config #:model-name "themed-model" #:ui-theme "solarized-dark"))
                   (serialized (config->sexp cfg)))
              (test-assert "ui section emitted when theme set"
                           (member '(ui (theme "solarized-dark")) (cdr serialized)))
              (let ((re-parsed (parse-config-sexp serialized)))
                (test-equal "roundtripped ui theme preserved"
                            "solarized-dark" (badb-config-ui-theme re-parsed))
                (test-equal "roundtripped model name preserved"
                            "themed-model" (badb-config-model-name re-parsed))))

            (let* ((cfg (make-badb-config))
                   (serialized (config->sexp cfg)))
              (test-assert "no ui section when theme unset"
                           (not (member 'ui (map car (cdr serialized))))))

            ;; Overlay onto a base config preserves the base ui theme
            (let* ((base (make-badb-config #:ui-theme "nord"))
                   (cfg (parse-config-sexp '(badb-config (journal (path "/tmp/j.jsonl"))) base)))
              (test-equal "parse overlay preserves base ui theme"
                          "nord" (badb-config-ui-theme cfg)))

            ;; Merging: overlay wins, base preserved when overlay unset
            (let ((base (make-badb-config #:ui-theme "badb-dark"))
                  (overlay (make-badb-config #:ui-theme "paper"))
                  (empty (make-badb-config)))
              (test-equal "overlay ui theme wins in merge"
                          "paper" (badb-config-ui-theme (merge-configs base overlay)))
              (test-equal "base ui theme kept when overlay unset"
                          "badb-dark" (badb-config-ui-theme (merge-configs base empty)))))

(test-group "load-config-hierarchical-files"
            (let ((gpath "/tmp/badb-test-global-cfg.scm")
                  (lpath "/tmp/badb-test-local-cfg.scm"))
              (call-with-output-file gpath
                                     (lambda (p)
                                       (write '(badb-config
                                                (model (name "global-default")
                                                 (endpoint "http://global-host:9000")
                                                 (temperature 0.25))
                                                (journal (path "/var/log/global-jrn.jsonl")))
                                              p)))
              ;; Local config only overrides model name; global endpoint, temperature, and journal must survive
              (call-with-output-file lpath
                                     (lambda (p)
                                       (write '(badb-config
                                                (model (name "local-override")))
                                              p)))
              (let ((loaded (load-config #:global-path gpath #:local-path lpath)))
                (test-equal "local model name wins" "local-override" (badb-config-model-name loaded))
                (test-equal "global endpoint preserved when omitted in local file"
                            "http://global-host:9000" (badb-config-model-endpoint loaded))
                (test-equal "global temperature preserved when omitted in local file"
                            0.25 (badb-config-model-temperature loaded))
                (test-equal "global journal path preserved when omitted in local file"
                            "/var/log/global-jrn.jsonl" (badb-config-journal-path loaded)))
              (when (file-exists? gpath) (delete-file gpath))
              (when (file-exists? lpath) (delete-file lpath))))

(spec-end)
(spec-exit)
