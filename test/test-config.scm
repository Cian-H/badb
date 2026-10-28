;;; test-config.scm — Tests for (badb config)
;;;
;;; Verifies hierarchical configuration loading, parsing, serialization,
;;; merging, and MCP server specification handling.

(define-module (test test-config)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (badb config))

(test-begin "badb-config")

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

(test-end "badb-config")
