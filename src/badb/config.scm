;;; badb — Configuration System
;;;
;;; Hierarchical configuration discovery and management in GNU Guile Scheme.
;;; Discovers and merges global (~/.config/badb/config.scm) and local project
;;; (.badb/config.scm) configurations. Supports model settings, MCP server
;;; declarations, checkpoint journal paths, and custom options.

(define-module (badb config) #:use-module
               (badb util)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 pretty-print)
               #:export
               (make-badb-config badb-config?
                                 badb-config-model-name
                                 badb-config-model-endpoint
                                 badb-config-model-api-key
                                 badb-config-model-temperature
                                 badb-config-model-max-tokens
                                 badb-config-journal-path
                                 badb-config-mcp-servers
                                 badb-config-options
                                 badb-config-source-path
                                 set-badb-config-model-name!
                                 set-badb-config-model-endpoint!
                                 set-badb-config-model-api-key!
                                 set-badb-config-model-temperature!
                                 set-badb-config-model-max-tokens!
                                 set-badb-config-journal-path!
                                 set-badb-config-mcp-servers!
                                 set-badb-config-options!
                                 set-badb-config-source-path!
                                 badb-config-set-model-name!
                                 badb-config-set-model-endpoint!
                                 badb-config-set-model-api-key!
                                 badb-config-set-model-temperature!
                                 badb-config-set-model-max-tokens!
                                 badb-config-set-journal-path!
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
                     mcp-servers
                     options
                     source-path))

(define badb-config-set-model-name! set-badb-config-model-name!)
(define badb-config-set-model-endpoint! set-badb-config-model-endpoint!)
(define badb-config-set-model-api-key! set-badb-config-model-api-key!)
(define badb-config-set-model-temperature! set-badb-config-model-temperature!)
(define badb-config-set-model-max-tokens! set-badb-config-model-max-tokens!)
(define badb-config-set-journal-path! set-badb-config-journal-path!)
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
;;; Property Extraction Helper
;;;
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
        "Extract the providers alist from CONFIG options, or '()."
        (let ((opts (badb-config-options config)))
          (or (get-prop opts 'providers #f) '())))

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
  (let
      ((prov (badb-config-get-provider config name)))
    (if prov
      (let*
          ((ep (get-prop prov 'endpoint #f))
           (key (get-prop prov 'api-key #f))
           (models (get-prop prov 'models #f))
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

;;;
;;; S-Expression Parsing and Serialization
;;;
(define (apply-model-config-form! cfg form-body)
  "Apply a (model ...) body form to <badb-config> CFG."
  (let* ((body (if (and (pair? form-body) (eq? (car form-body) 'model))
                   (cdr form-body)
                   form-body))
         (m (or (get-prop body 'name #f)
                (get-prop body 'default #f)
                (badb-config-model-name cfg)))
         (ep (get-prop body 'endpoint (badb-config-model-endpoint cfg)))
         (key (get-prop body 'api-key (badb-config-model-api-key cfg)))
         (temp (get-prop body 'temperature (badb-config-model-temperature cfg)))
         (max-tok (get-prop body 'max-tokens (badb-config-model-max-tokens cfg))))
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
         (p (get-prop body 'path (badb-config-journal-path cfg))))
    (when p (set-badb-config-journal-path! cfg p))))

(define (apply-mcp-config-form! cfg form-body)
  "Apply an (mcp ...) body form to <badb-config> CFG."
  (let* ((body (if (and (pair? form-body) (eq? (car form-body) 'mcp))
                   (cdr form-body)
                   form-body))
         (servers-entry (get-prop body 'servers #f))
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
                                   #:journal-path (badb-config-journal-path base-config)
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
              ((mcp)        (apply-mcp-config-form! cfg body))
              ((mcp-server) (let ((parsed (parse-mcp-server-spec form)))
                              (when parsed (badb-config-add-mcp-server! cfg (car parsed) (cdr parsed)))))
              ((options ui) (set-badb-config-options! cfg (append (badb-config-options cfg) body)))))))
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
(define (read-config-file path)
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
                                                 (cfg (parse-config-sexp combined)))
                                              (set-badb-config-source-path! cfg path)
                                              cfg)
                                            (loop (cons expr sexps))))))))
            (lambda (key . args)
              (format (current-error-port)
                      "Warning: error reading config file ~a: ~s
"
                      path
                      args)
              #f))
          #f))

(define (ensure-directory-for-file path)
        "Ensure the parent directory for PATH exists."
        (ensure-parent-directory! path))

(define (write-config-file config path)
  "Serialize CONFIG to PATH as readable Scheme code."
  (ensure-directory-for-file path)
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
                                 #:options
                                 (append (badb-config-options base) (badb-config-options overlay))
                                 #:source-path
                                 (or (badb-config-source-path overlay) (badb-config-source-path base))))
             ;; Merge MCP servers: overlay servers take precedence over base servers with same name
             (combined-mcp (alist-merge (badb-config-mcp-servers base)
                                        (badb-config-mcp-servers overlay))))
          (set-badb-config-mcp-servers! merged combined-mcp)
          merged))

(define* (load-config #:key
                      (global-path (default-global-config-path))
                      (local-path (default-local-config-path)))
  "Discover and load badb configuration with hierarchical resolution:
Defaults -> Global (~/.config/badb/config.scm) -> Local (.badb/config.scm)."
  (let* ((cfg (make-badb-config))
         (global-cfg (and global-path (read-config-file global-path)))
         (local-cfg (and local-path (read-config-file local-path)))
         (with-global (if global-cfg (merge-configs cfg global-cfg) cfg))
         (final-cfg (if local-cfg (merge-configs with-global local-cfg) with-global)))
    final-cfg))

(define* (save-local-config! config #:optional (path (default-local-config-path)))
  "Persist CONFIG to the local project configuration file (.badb/config.scm)."
  (write-config-file config path))

(define* (save-global-config! config #:optional (path (default-global-config-path)))
  "Persist CONFIG to the global user configuration file (~/.config/badb/config.scm)."
  (write-config-file config path))
