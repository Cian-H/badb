;;; badb — Configuration System
;;;
;;; Hierarchical configuration discovery and management in GNU Guile Scheme.
;;; Discovers and merges global (~/.config/badb/config.scm) and local project
;;; (.badb/config.scm) configurations. Supports model settings, MCP server
;;; declarations, checkpoint journal paths, UI theme selection, and custom
;;; options.

(define-module (geasa foundation-config plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 pretty-print)
               #:use-module
               (lens)
               #:export
               (badb-config-react-max-iterations
                 make-badb-config
                 badb-config?
                 badb-config-model-name
                 badb-config-model-endpoint
                 badb-config-model-api-key
                 badb-config-model-temperature
                 badb-config-model-max-tokens
                 badb-config-journal-path
                 badb-config-ui-theme
                 badb-config-mcp-servers
                 badb-config-options
                 badb-config-source-path
                 set-badb-config-model-name!
                 set-badb-config-model-endpoint!
                 set-badb-config-model-api-key!
                 set-badb-config-model-temperature!
                 set-badb-config-model-max-tokens!
                 set-badb-config-journal-path!
                 set-badb-config-ui-theme!
                 set-badb-config-mcp-servers!
                 set-badb-config-options!
                 set-badb-config-source-path!
                 badb-config-set-model-name!
                 badb-config-set-model-endpoint!
                 badb-config-set-model-api-key!
                 badb-config-set-model-temperature!
                 badb-config-set-model-max-tokens!
                 badb-config-set-journal-path!
                 badb-config-set-ui-theme!
                 badb-config-set-mcp-servers!
                 badb-config-set-options!
                 badb-config-set-source-path!
                 default-global-config-path
                 default-local-config-path
                 make-mcp-server-spec
                 badb-config-get-mcp-server
                 badb-config-add-mcp-server!
                 badb-config-merge-mcp-servers!
                 badb-config-remove-mcp-server!
                 badb-config-providers
                 badb-config-get-provider
                 badb-config-provider-for-endpoint
                 badb-config-allow-missing-thinking?
                 badb-config-agents
                 badb-config-get-agent
                 badb-config-synthesis-model
                 badb-config-set-synthesis-model!
                 badb-config-switch-provider!
                 parse-config-sexp
                 config->sexp
                 read-config-file
                 write-config-file
                 merge-configs
                 load-config
                 save-local-config!
                 save-global-config!))

;;;
;;; Configuration Record Definition
;;;
(define-record-type <badb-config>
                    (%make-badb-config model-name
                                       model-endpoint
                                       model-api-key
                                       model-temperature
                                       model-max-tokens
                                       journal-path
                                       ui-theme
                                       mcp-servers
                                       options
                                       source-path)
                    badb-config?
                    (model-name badb-config-model-name set-badb-config-model-name!)
                    (model-endpoint badb-config-model-endpoint set-badb-config-model-endpoint!)
                    (model-api-key badb-config-model-api-key set-badb-config-model-api-key!)
                    (model-temperature badb-config-model-temperature set-badb-config-model-temperature!)
                    (model-max-tokens badb-config-model-max-tokens set-badb-config-model-max-tokens!)
                    (journal-path badb-config-journal-path set-badb-config-journal-path!)
                    (ui-theme badb-config-ui-theme set-badb-config-ui-theme!)
                    (mcp-servers badb-config-mcp-servers set-badb-config-mcp-servers!)
                    (options badb-config-options set-badb-config-options!)
                    (source-path badb-config-source-path set-badb-config-source-path!))

(define*
  (make-badb-config #:key
                    (model-name (or (getenv "BADB_MODEL_NAME") "minicpm5-1b:gguf:q8"))
                    (model-endpoint
                      (or (getenv "BADB_MODEL_ENDPOINT") "http://127.0.0.1:10100/inference/v1"))
                    (model-api-key (or (getenv "BADB_API_KEY") (getenv "OPENAI_API_KEY") #f))
                    (model-temperature 0.7)
                    (model-max-tokens (or (and=> (getenv "BADB_MAX_TOKENS") string->number) 4096))
                    (journal-path ".badb/journal.jsonl")
                    (ui-theme #f)
                    (mcp-servers '())
                    (options '())
                    (source-path #f))
  "Create a new <badb-config> record instance with sensible defaults."
  (%make-badb-config model-name
                     model-endpoint
                     model-api-key
                     model-temperature
                     model-max-tokens
                     journal-path
                     ui-theme
                     mcp-servers
                     options
                     source-path))

(define badb-config-set-model-name! set-badb-config-model-name!)
(define badb-config-set-model-endpoint! set-badb-config-model-endpoint!)
(define badb-config-set-model-api-key! set-badb-config-model-api-key!)
(define badb-config-set-model-temperature! set-badb-config-model-temperature!)
(define badb-config-set-model-max-tokens! set-badb-config-model-max-tokens!)
(define badb-config-set-journal-path! set-badb-config-journal-path!)
(define badb-config-set-ui-theme! set-badb-config-ui-theme!)
(define badb-config-set-mcp-servers! set-badb-config-mcp-servers!)
(define badb-config-set-options! set-badb-config-options!)
(define badb-config-set-source-path! set-badb-config-source-path!)

(define* (make-mcp-server-spec name command #:optional (args '()) #:key (env '()) (auto-start #f))
         "Construct an MCP server specification alist."
         `((name . ,(ensure-string name))
           (command . ,command)
           (args . ,(if (list? args) args (list args)))
           (env . ,env)
           (auto-start . ,auto-start)))

;;;
;;; Standard Paths
;;;
(define (default-global-config-path)
        "Return the standard path for badb's global user configuration."
        (let ((xdg (getenv "XDG_CONFIG_HOME"))
              (home (getenv "HOME")))
          (cond
            ((and xdg (not (string-null? xdg)))
              (string-append xdg "/badb/config.scm"))
            ((and home (not (string-null? home)))
              (string-append home "/.config/badb/config.scm"))
            (else
              "/tmp/badb/config.scm"))))

(define (default-local-config-path)
        "Return the standard path for badb's project-local configuration."
        ".badb/config.scm")

;;;
;;; MCP Server Helpers
;;;
(define (parse-mcp-server-spec entry)
        "Parse a raw MCP server declaration into canonical form:
(name (command . cmd) (args . (arg ...)) (env . ()) (auto-start . #t/#f))"
        (let
            ((name-and-props
               (cond
                 ((and (pair? entry)
                       (or (eq? (car entry) 'server) (eq? (car entry) 'mcp-server))
                       (pair? (cdr entry)))
                   (cons (ensure-string (cadr entry)) (cddr entry)))
                 ((and (pair? entry) (or (string? (car entry)) (symbol? (car entry))))
                   (cons (ensure-string (car entry)) (cdr entry)))
                 (else #f))))
          (and name-and-props
               (let*
                   ((name (car name-and-props))
                    (props (cdr name-and-props))
                    (cmd (get-prop props 'command ""))
                    (raw-args (get-prop props 'args '()))
                    (args
                      (if (list? raw-args)
                        raw-args
                        (list raw-args)))
                    (env (normalize-env-alist (get-prop props 'env '())))
                    (auto? (or (get-prop props 'auto-start #f) (get-prop props 'auto-start? #f))))
                 (cons name
                       `((command . ,cmd) (args . ,args) (env . ,env) (auto-start . ,auto?)))))))

(define (badb-config-get-mcp-server config name)
        "Retrieve the specification alist for MCP server NAME from CONFIG, or #f."
        (let* ((name-str (ensure-string name))
               (entry (assoc name-str (badb-config-mcp-servers config))))
          (and entry (cdr entry))))

(define* (badb-config-add-mcp-server! config name command
                                      #:optional (args '())
                                      #:key (env '()) (auto-start #f))
         "Add or update MCP server NAME in CONFIG."
         (let* ((name-str (ensure-string name))
                (spec (if (list? command)
                        ;; User passed a full spec alist in command
                        command
                        `((command . ,command)
                          (args . ,(if (list? args) args (list args)))
                          (env . ,env)
                          (auto-start . ,auto-start))))
                (current (badb-config-mcp-servers config)))
           (set-badb-config-mcp-servers! config (cons (cons name-str spec)
                                                      (alist-remove name-str current)))
           config))

(define (badb-config-merge-mcp-servers! config new-entries)
        "Batch add or update a list of (NAME . SPEC) MCP server entries in CONFIG."
        (set-badb-config-mcp-servers! config
                                      (alist-merge (badb-config-mcp-servers config) new-entries))
        config)

(define
  (badb-config-remove-mcp-server! config name)
  "Remove MCP server NAME from CONFIG."
  (set-badb-config-mcp-servers! config
                                (alist-remove name (badb-config-mcp-servers config)))
  config)

(define (badb-config-providers config)
        "Extract the providers alist from CONFIG options, or '().
Uses a raw alist lookup: GET-PROP unwraps single-provider catalogs,
which would break badb-config-get-provider on them."
        (let ((opts (badb-config-options config))
              (providers-lens (key-ref 'providers)))
          (or (focus providers-lens opts) '())))

(define (badb-config-get-provider config name)
        "Retrieve provider entry for NAME (symbol or string) from CONFIG options, or #f."
        (let*
            ((name-sym (ensure-symbol name))
             (name-str (symbol->string name-sym))
             (provs (badb-config-providers config)))
          (or (assq-ref provs name-sym) (assoc-ref provs name-str))))

(define
  (badb-config-switch-provider! config name)
  "Switch active model configuration in CONFIG to match named provider preset.
Returns #t on success, or #f if provider NAME was not found."
  (let ((prov (badb-config-get-provider config name))
        (endpoint-lens (prop-lens 'endpoint))
        (api-key-lens (prop-lens 'api-key))
        (models-lens (prop-lens 'models)))
    (if prov
      (let* ((ep (focus endpoint-lens prov))
             (key (focus api-key-lens prov))
             (models (focus models-lens prov))
             (m
               (cond
                 ((string? models) models)
                 ((and (pair? models) (string? (car models))) (car models))
                 ((and (pair? models) (pair? (car models)) (string? (caar models)))
                   (caar models))
                 (else #f))))
        (when ep (set-badb-config-model-endpoint! config ep))
        (when key (set-badb-config-model-api-key! config key))
        (when m (set-badb-config-model-name! config m))
        #t)
      #f)))

(define
  (badb-config-provider-for-endpoint config endpoint)
  "Return the provider preset entry (its field alist) whose endpoint
matches ENDPOINT, or #f.  The first matching preset wins."
  (let ((ep (and (string? endpoint) endpoint))
        (endpoint-lens (prop-lens 'endpoint)))
    (and ep
         (let loop ((provs (badb-config-providers config)))
                   (cond
                     ((null? provs) #f)
                     ((equal? (focus endpoint-lens (cdar provs)) ep)
                       (cdar provs))
                     (else (loop (cdr provs))))))))

(define
  (badb-config-allow-missing-thinking? config endpoint)
  "Whether a chat request to ENDPOINT should carry the Qwen3-style
allow_missing_thinking flag: #t by default, #f when the provider preset
matching ENDPOINT sets (allow-missing-thinking #f)."
  (let* ((prov (badb-config-provider-for-endpoint config endpoint))
         (flag-lens (prop-lens 'allow-missing-thinking #t))
         (flag (if prov (focus flag-lens prov) #t)))
    (not (eq? flag #f))))

(define
  (badb-config-agents config)
  "Extract the agent-role catalog from CONFIG options — the value of
(options (agents (role ...) ...)) — or '().  Uses a raw alist lookup:
GET-PROP unwraps single-entry catalogs ((one-role ...)) into the role
form itself, which would corrupt single-role configurations."
  (let ((opts (badb-config-options config))
        (agents-lens (key-ref 'agents)))
    (or (focus agents-lens opts) '())))

(define
  (badb-config-get-agent config name)
  "Retrieve the role entry for NAME (symbol or string) — e.g.
'synthesis — from the agents catalog, or #f."
  (let* ((name-sym (ensure-symbol name))
         (name-str (symbol->string name-sym))
         (roles (badb-config-agents config)))
    (or (assq-ref roles name-sym) (assoc-ref roles name-str))))

(define*
  (badb-config-synthesis-model config #:optional (default (or (getenv "BADB_SYNTHESIS_MODEL") "qwen3.6-35b-a3b")))
  "Return the model configured for geis synthesis, or DEFAULT if not explicitly set."
  (let ((role (badb-config-get-agent config "synthesis")))
    (or (and role (get-prop role 'model #f))
        default)))

(define
  (badb-config-set-synthesis-model! config model-name)
  "Set or update the model for the 'synthesis' agent role in CONFIG options."
  (let* ((opts (badb-config-options config))
         (agents-entry (or (assq 'agents opts) '(agents)))
         (roles (cdr agents-entry))
         (synth-entry (or (assq 'synthesis roles) '(synthesis)))
         (synth-body (cdr synth-entry))
         (new-synth-body (cons `(model ,(ensure-string model-name))
                               (filter (lambda (prop)
                                         (not (and (pair? prop) (eq? (car prop) 'model))))
                                       synth-body)))
         (new-synth-entry (cons 'synthesis new-synth-body))
         (new-roles (cons new-synth-entry
                          (filter (lambda (r)
                                    (not (and (pair? r) (eq? (car r) 'synthesis))))
                                  roles)))
         (new-agents-entry (cons 'agents new-roles))
         (new-opts (cons new-agents-entry
                         (filter (lambda (opt)
                                   (not (and (pair? opt) (eq? (car opt) 'agents))))
                                 opts))))
    (set-badb-config-options! config new-opts)
    config))

;;;
;;; Tolerant property lens (guile-lens over get-prop matching)
;;;
;; Configuration forms mix (key . val), (key val), and #:key val shapes
;; with symbol/string/keyword keys; get-prop (foundation-util) resolves
;; them all.  prop-lens focuses such properties using guile-lens.
(define (%entry-key-matches? entry sym str kw)
        (and (pair? entry)
             (let ((k (if (pair? (car entry)) (caar entry) (car entry))))
               (or (equal? k sym) (equal? k str) (equal? k kw)))))

(define* (prop-lens key #:optional (default #f))
         "Return a guile-lens lens focusing KEY in property lists and alists
with get-prop's tolerant matching (symbol, string, or keyword keys;
dotted or single-element-list values).  DEFAULT is the focus default."
         (let*
             ((sym (ensure-symbol key))
              (str (symbol->string sym))
              (kw (symbol->keyword sym)))
           (lens
             (lambda (obj)
               (get-prop
                 (if (list? obj)
                   obj
                   '())
                 key
                 default))
             (lambda (obj f)
               (let
                   ((alist
                      (if (list? obj)
                        obj
                        '()))
                    (val
                      (get-prop
                        (if (list? obj)
                          obj
                          '())
                        key
                        default)))
                 (cons (list sym (f val))
                       (filter
                         (lambda (entry)
                           (not (%entry-key-matches? entry sym str kw)))
                         alist)))))))

(define (badb-config-react-max-iterations config)
        "Optional ReAct loop iteration cap from (options (react (max-iterations N))):
a positive integer, or #f (the default) for an unlimited loop."
        (let* ((opts (badb-config-options config))
               (react-lens (key-ref 'react))
               (max-iter-lens (prop-lens 'max-iterations))
               (react (and (pair? opts) (focus react-lens opts)))
               (limit (and (pair? react) (focus max-iter-lens react))))
          (if
              (and (number? limit) (integer? limit) (> limit 0))
            limit
            #f)))

;;;
;;; S-Expression Parsing and Serialization
;;;
(define (apply-model-config-form! cfg form-body)
        "Apply a (model ...) body form to <badb-config> CFG."
        (let* ((body (if (and (pair? form-body) (eq? (car form-body) 'model))
                       (cdr form-body)
                       form-body))
               (name-lens (prop-lens 'name))
               (default-lens (prop-lens 'default))
               (endpoint-lens (prop-lens 'endpoint))
               (api-key-lens (prop-lens 'api-key))
               (temperature-lens (prop-lens 'temperature))
               (max-tokens-lens (prop-lens 'max-tokens))
               (m (or (focus name-lens body)
                      (focus default-lens body)
                      (badb-config-model-name cfg)))
               (ep (or (focus endpoint-lens body) (badb-config-model-endpoint cfg)))
               (key (or (focus api-key-lens body) (badb-config-model-api-key cfg)))
               (temp (or (focus temperature-lens body) (badb-config-model-temperature cfg)))
               (max-tok (or (focus max-tokens-lens body) (badb-config-model-max-tokens cfg))))
          (when m (set-badb-config-model-name! cfg m))
          (when ep (set-badb-config-model-endpoint! cfg ep))
          (when key (set-badb-config-model-api-key! cfg key))
          (when temp (set-badb-config-model-temperature! cfg temp))
          (when max-tok (set-badb-config-model-max-tokens! cfg max-tok))))

(define (apply-journal-config-form! cfg form-body)
        "Apply a (journal ...) body form to <badb-config> CFG."
        (let* ((body (if (and (pair? form-body) (eq? (car form-body) 'journal))
                       (cdr form-body)
                       form-body))
               (path-lens (prop-lens 'path))
               (p (focus path-lens body)))
          (when p (set-badb-config-journal-path! cfg p))))

(define (apply-ui-config-form! cfg form-body)
        "Apply a (ui ...) body form to <badb-config> CFG."
        (let* ((body (if (and (pair? form-body) (eq? (car form-body) 'ui))
                       (cdr form-body)
                       form-body))
               (theme-lens (prop-lens 'theme))
               (t (focus theme-lens body)))
          (when t (set-badb-config-ui-theme! cfg (ensure-string t)))))

(define (apply-mcp-config-form! cfg form-body)
        "Apply an (mcp ...) body form to <badb-config> CFG."
        (let* ((body (if (and (pair? form-body) (eq? (car form-body) 'mcp))
                       (cdr form-body)
                       form-body))
               (servers-lens (prop-lens 'servers))
               (servers-entry (focus servers-lens body))
               (entries (if (list? servers-entry) servers-entry body))
               (parsed (filter-map parse-mcp-server-spec entries)))
          (badb-config-merge-mcp-servers! cfg parsed)))

(define* (parse-config-sexp sexp #:optional (base-config #f))
         "Parse SEXP into a <badb-config> record, optionally overlaying onto BASE-CONFIG."
         (let ((cfg (if base-config
                      (make-badb-config #:model-name (badb-config-model-name base-config)
                                        #:model-endpoint (badb-config-model-endpoint base-config)
                                        #:model-api-key (badb-config-model-api-key base-config)
                                        #:model-temperature (badb-config-model-temperature base-config)
                                        #:model-max-tokens (badb-config-model-max-tokens base-config)
                                        #:journal-path (badb-config-journal-path base-config)
                                        #:ui-theme (badb-config-ui-theme base-config)
                                        #:mcp-servers (badb-config-mcp-servers base-config)
                                        #:options (badb-config-options base-config)
                                        #:source-path (badb-config-source-path base-config))
                      (make-badb-config)))
               (forms (cond
                        ((and (pair? sexp) (eq? (car sexp) 'badb-config)) (cdr sexp))
                        ((list? sexp) sexp)
                        (else '()))))
           (for-each
             (lambda (form)
               (when (pair? form)
                 (let ((head (car form)) (body (cdr form)))
                   (case head
                     ((model)      (apply-model-config-form! cfg body))
                     ((journal)    (apply-journal-config-form! cfg body))
                     ((ui)         (apply-ui-config-form! cfg body))
                     ((mcp)        (apply-mcp-config-form! cfg body))
                     ((mcp-server) (let ((parsed (parse-mcp-server-spec form)))
                                     (when parsed (badb-config-add-mcp-server! cfg (car parsed) (cdr parsed)))))
                     ((options)    (set-badb-config-options! cfg (append (badb-config-options cfg) body)))))))
             forms)
           cfg))

(define (config->sexp config)
        "Convert CONFIG to an idiomatic Guile s-expression suitable for serialization."
        `(badb-config
           (model
             (name ,(badb-config-model-name config))
             (endpoint ,(badb-config-model-endpoint config))
             (api-key ,(badb-config-model-api-key config))
             (temperature ,(badb-config-model-temperature config))
             ,@(if (badb-config-model-max-tokens config)
                 (list `(max-tokens ,(badb-config-model-max-tokens config)))
                 '()))

           (journal
             (path ,(badb-config-journal-path config)))

           ,@(if (badb-config-ui-theme config)
               `((ui (theme ,(badb-config-ui-theme config))))
               '())

           (mcp
             ,@(map (lambda (entry)
                      (let* ((name (car entry))
                             (spec (cdr entry))
                             (cmd (or (assoc-ref spec 'command) ""))
                             (args (or (assoc-ref spec 'args) '()))
                             (env (or (assoc-ref spec 'env) '()))
                             (auto? (or (assoc-ref spec 'auto-start) #f)))
                        `(server ,name
                                 (command ,cmd)
                                 (args ,@args)
                                 ,@(if (and (list? env) (not (null? env)))
                                     `((env ,@env))
                                     '())
                                 (auto-start ,auto?))))
                    (badb-config-mcp-servers config)))

           ,@(if (null? (badb-config-options config))
               '()
               `((options ,@(badb-config-options config))))))

;;;
;;; File I/O
;;;
(define* (read-config-file path #:optional (base-config #f))
         "Read and parse a configuration file from PATH. Returns <badb-config> or #f if not found."
         (if (file-exists? path)
           (catch #t
             (lambda ()
               (call-with-input-file path
                                     (lambda (port)
                                       (let loop
                                         ((sexps '()))
                                         (let ((expr (read port)))
                                           (if (eof-object? expr)
                                             (let*
                                                 ((combined
                                                    (cond
                                                      ((null? sexps) '())
                                                      ((and (null? (cdr sexps)) (pair? (car sexps)) (eq? (caar sexps) 'badb-config))
                                                        (car sexps))
                                                      (else (reverse sexps))))
                                                  (cfg (parse-config-sexp combined base-config)))
                                               (set-badb-config-source-path! cfg path)
                                               cfg)
                                             (loop (cons expr sexps))))))))
             (lambda (_key . args)
               (format (current-error-port)
                       "Warning: error reading config file ~a: ~s
"
                       path
                       args)
               #f))
           #f))

(define (write-config-file config path)
        "Serialize CONFIG to PATH as readable Scheme code."
        (ensure-parent-directory! path)
        (call-with-output-file path
                               (lambda (port)
                                 (display ";;; badb configuration\n" port)
                                 (display ";;; Generated and managed by badb. Manual edits are preserved.\n\n" port)
                                 (pretty-print (config->sexp config) port)
                                 (newline port)
                                 #t)))

;;;
;;; Configuration Merging & Discovery
;;;
(define (merge-configs base overlay)
        "Merge OVERLAY into BASE. Overlay scalar values override base values.
MCP servers from overlay override base servers with the same name, while
unique base servers are retained."
        (let*
            ((merged
               (make-badb-config #:model-name
                                 (or (badb-config-model-name overlay) (badb-config-model-name base))
                                 #:model-endpoint
                                 (or (badb-config-model-endpoint overlay) (badb-config-model-endpoint base))
                                 #:model-api-key
                                 (or (badb-config-model-api-key overlay) (badb-config-model-api-key base))
                                 #:model-temperature
                                 (or (badb-config-model-temperature overlay)
                                     (badb-config-model-temperature base))
                                 #:model-max-tokens
                                 (or (badb-config-model-max-tokens overlay) (badb-config-model-max-tokens base))
                                 #:journal-path
                                 (or (badb-config-journal-path overlay) (badb-config-journal-path base))
                                 #:ui-theme
                                 (or (badb-config-ui-theme overlay) (badb-config-ui-theme base))
                                 #:options
                                 (append (badb-config-options base) (badb-config-options overlay))
                                 #:source-path
                                 (or (badb-config-source-path overlay) (badb-config-source-path base))))
             ;; Merge MCP servers: overlay servers take precedence over base servers with same name
             (combined-mcp (alist-merge (badb-config-mcp-servers base)
                                        (badb-config-mcp-servers overlay))))
          (set-badb-config-mcp-servers! merged combined-mcp)
          merged))

(define (make-unset-overlay-config)
        "Create a <badb-config> with #f scalar fields so only explicitly parsed fields override base."
        (make-badb-config #:model-name #f
                          #:model-endpoint #f
                          #:model-api-key #f
                          #:model-temperature #f
                          #:model-max-tokens #f
                          #:journal-path #f
                          #:ui-theme #f))

(define* (load-config #:key
                      (global-path (default-global-config-path))
                      (local-path (default-local-config-path)))
         "Discover and load badb configuration with hierarchical resolution:
Defaults -> Global (~/.config/badb/config.scm) -> Local (.badb/config.scm)."
         (let* ((cfg (make-badb-config))
                (global-cfg (and global-path (read-config-file global-path (make-unset-overlay-config))))
                (local-cfg (and local-path (read-config-file local-path (make-unset-overlay-config))))
                (with-global (if global-cfg (merge-configs cfg global-cfg) cfg))
                (final-cfg (if local-cfg (merge-configs with-global local-cfg) with-global)))
           final-cfg))

(define* (save-local-config! config #:optional (path (default-local-config-path)))
         "Persist CONFIG to the local project configuration file (.badb/config.scm)."
         (write-config-file config path))

(define* (save-global-config! config #:optional (path (default-global-config-path)))
         "Persist CONFIG to the global user configuration file (~/.config/badb/config.scm)."
         (write-config-file config path))
