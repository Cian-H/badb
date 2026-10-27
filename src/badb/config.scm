;;; badb — Configuration System
;;;
;;; Hierarchical configuration discovery and management in GNU Guile Scheme.
;;; Discovers and merges global (~/.config/badb/config.scm) and local project
;;; (.badb/config.scm) configurations. Supports model settings, MCP server
;;; declarations, checkpoint journal paths, and custom options.

(define-module (badb config)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 pretty-print)
  #:export (make-badb-config
            badb-config?
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
            badb-config-remove-mcp-server!

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

(define* (make-badb-config #:key
                           (model-name (or (getenv "BADB_MODEL_NAME")
                                           "minicpm5-1b:gguf:q8"))
                           (model-endpoint (or (getenv "BADB_MODEL_ENDPOINT")
                                               "http://127.0.0.1:10100/inference/v1"))
                           (model-api-key (or (getenv "BADB_API_KEY")
                                              (getenv "OPENAI_API_KEY")
                                              #f))
                           (model-temperature 0.7)
                           (model-max-tokens (or (and (getenv "BADB_MAX_TOKENS")
                                                      (string->number (getenv "BADB_MAX_TOKENS")))
                                                 4096))
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
  `((name . ,(if (symbol? name) (symbol->string name) name))
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
(define (get-prop lst key default)
  "Safely extract KEY from property list or alist LST, supporting both
(key . val), (key val), and #:key val syntax."
  (let* ((sym (if (symbol? key) key (string->symbol key)))
         (str (symbol->string sym))
         (kw (symbol->keyword sym)))
    (let loop ((items lst))
      (cond
       ((null? items) default)
       ((not (pair? items)) default)
       ((and (pair? (car items))
             (or (equal? (caar items) sym)
                 (equal? (caar items) str)))
        (let ((val (cdar items)))
          (if (and (pair? val) (null? (cdr val)))
              (car val)
              val)))
       ((and (keyword? (car items)) (equal? (car items) kw) (pair? (cdr items)))
        (cadr items))
       (else (loop (cdr items)))))))

;;;
;;; MCP Server Helpers
;;;
(define (parse-mcp-server-spec entry)
        "Parse a raw MCP server declaration into canonical form:
(name (command . cmd) (args . (arg ...)) (env . ()) (auto-start . #t/#f))"
        (let*
            ((normalize-name
               (lambda (n)
                 (if (symbol? n)
                   (symbol->string n)
                   n)))
             (normalize-env
               (lambda (raw-env)
                 (cond
                   ((null? raw-env) '())
                   ((and (pair? raw-env) (or (symbol? (car raw-env)) (string? (car raw-env))))
                     (list raw-env))
                   ((list? raw-env) raw-env)
                   (else '()))))
             (name-and-props
               (cond
                 ((and (pair? entry)
                       (or (eq? (car entry) 'server) (eq? (car entry) 'mcp-server))
                       (pair? (cdr entry)))
                   (cons (normalize-name (cadr entry)) (cddr entry)))
                 ((and (pair? entry) (or (string? (car entry)) (symbol? (car entry))))
                   (cons (normalize-name (car entry)) (cdr entry)))
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
                    (env (normalize-env (get-prop props 'env '())))
                    (auto? (or (get-prop props 'auto-start #f) (get-prop props 'auto-start? #f))))
                 (cons name
                       `((command . ,cmd) (args . ,args) (env . ,env) (auto-start . ,auto?)))))))

(define (badb-config-get-mcp-server config name)
  "Retrieve the specification alist for MCP server NAME from CONFIG, or #f."
  (let* ((name-str (if (symbol? name) (symbol->string name) name))
         (entry (assoc name-str (badb-config-mcp-servers config))))
    (and entry (cdr entry))))

(define* (badb-config-add-mcp-server! config name command
                                     #:optional (args '())
                                     #:key (env '()) (auto-start #f))
  "Add or update MCP server NAME in CONFIG."
  (let* ((name-str (if (symbol? name) (symbol->string name) name))
         (spec (if (list? command)
                   ;; User passed a full spec alist in command
                   command
                   `((command . ,command)
                     (args . ,(if (list? args) args (list args)))
                     (env . ,env)
                     (auto-start . ,auto-start))))
         (current (badb-config-mcp-servers config))
         (filtered (remove (lambda (e) (string=? (car e) name-str)) current)))
    (set-badb-config-mcp-servers! config (cons (cons name-str spec) filtered))
    config))

(define (badb-config-remove-mcp-server! config name)
  "Remove MCP server NAME from CONFIG."
  (let* ((name-str (if (symbol? name) (symbol->string name) name))
         (current (badb-config-mcp-servers config))
         (filtered (remove (lambda (e) (string=? (car e) name-str)) current)))
    (set-badb-config-mcp-servers! config filtered)
    config))

;;;
;;; S-Expression Parsing and Serialization
;;;
(define (apply-model-config-form! cfg body)
        "Apply a (model ...) body form to <badb-config> CFG."
        (let
            ((body
               (if (and (pair? body) (eq? (car body) 'model))
                 (cdr body)
                 body)))
          (let
              ((m (get-prop body 'name (get-prop body 'default (badb-config-model-name cfg))))
               (ep (get-prop body 'endpoint (badb-config-model-endpoint cfg)))
               (key (get-prop body 'api-key (badb-config-model-api-key cfg)))
               (temp (get-prop body 'temperature (badb-config-model-temperature cfg)))
               (max-tok (get-prop body 'max-tokens (badb-config-model-max-tokens cfg))))
            (when m (set-badb-config-model-name! cfg m))
            (when ep (set-badb-config-model-endpoint! cfg ep))
            (when key (set-badb-config-model-api-key! cfg key))
            (when temp (set-badb-config-model-temperature! cfg temp))
            (when max-tok (set-badb-config-model-max-tokens! cfg max-tok)))))

(define (apply-journal-config-form! cfg body)
        "Apply a (journal ...) body form to <badb-config> CFG."
        (let
            ((body
               (if (and (pair? body) (eq? (car body) 'journal))
                 (cdr body)
                 body)))
          (let
              ((p (get-prop body 'path (badb-config-journal-path cfg))))
            (when p (set-badb-config-journal-path! cfg p)))))

(define (apply-mcp-config-form! cfg body)
        "Apply an (mcp ...) body form to <badb-config> CFG."
        (let*
            ((body
               (if (and (pair? body) (eq? (car body) 'mcp))
                 (cdr body)
                 body))
             (servers-entry (get-prop body 'servers #f))
             (entries
               (cond
                 ((list? servers-entry) servers-entry)
                 (else body)))
             (parsed (filter-map parse-mcp-server-spec entries)))
          (for-each
            (lambda (entry)
              (badb-config-add-mcp-server! cfg (car entry) (cdr entry)))
            parsed)))

(define*
  (parse-config-sexp sexp #:optional (base-config #f))
  "Parse SEXP into a <badb-config> record, optionally overlaying onto BASE-CONFIG."
  (let
      ((cfg
         (if base-config
           (make-badb-config #:model-name
                             (badb-config-model-name base-config)
                             #:model-endpoint
                             (badb-config-model-endpoint base-config)
                             #:model-api-key
                             (badb-config-model-api-key base-config)
                             #:model-temperature
                             (badb-config-model-temperature base-config)
                             #:journal-path
                             (badb-config-journal-path base-config)
                             #:mcp-servers
                             (badb-config-mcp-servers base-config)
                             #:options
                             (badb-config-options base-config)
                             #:source-path
                             (badb-config-source-path base-config))
           (make-badb-config)))
       (forms
         (cond
           ;; (badb-config ...) wrapper
           ((and (pair? sexp) (eq? (car sexp) 'badb-config)) (cdr sexp))
           ;; List of section lists
           ((list? sexp) sexp)
           (else '()))))
    (for-each
      (lambda (form)
        (when (pair? form)
          (let ((head (car form)) (body (cdr form)))
            (cond
              ;; Model Section: (model ...)
              ((eq? head 'model) (apply-model-config-form! cfg body))
              ;; Journal Section: (journal ...)
              ((eq? head 'journal) (apply-journal-config-form! cfg body))
              ;; MCP Section: (mcp (server ...) ...) or (mcp (servers ...))
              ((eq? head 'mcp) (apply-mcp-config-form! cfg body))
              ;; Standalone MCP Server: (mcp-server "name" ...)
              ((eq? head 'mcp-server)
                (let ((parsed (parse-mcp-server-spec form)))
                  (when parsed (badb-config-add-mcp-server! cfg (car parsed) (cdr parsed)))))
              ;; Options / Extensions: (options ...)
              ((or (eq? head 'options) (eq? head 'ui))
                (set-badb-config-options! cfg (append (badb-config-options cfg) body)))
              (else #f)))))
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
              (let loop ((sexps '()))
                (let ((expr (read port)))
                  (if (eof-object? expr)
                      (let* ((combined (if (null? sexps) '() (car (reverse sexps))))
                             (cfg (parse-config-sexp combined)))
                        (set-badb-config-source-path! cfg path)
                        cfg)
                      (loop (cons expr sexps))))))))
        (lambda (key . args)
          (format (current-error-port) "Warning: error reading config file ~a: ~s\n" path args)
          #f))
      #f))

(define (mkdir-p dir)
  "Recursively ensure that directory DIR exists."
  (unless (or (string-null? dir)
              (string=? dir ".")
              (string=? dir "/")
              (file-exists? dir))
    (let ((parent (dirname dir)))
      (unless (or (string=? parent dir)
                  (string-null? parent)
                  (string=? parent "."))
        (mkdir-p parent))
      (catch 'system-error
        (lambda () (mkdir dir))
        (lambda args
          (unless (file-exists? dir)
            (apply throw args)))))))

(define (ensure-directory-for-file path)
  "Ensure the parent directory for PATH exists."
  (let ((dir (dirname path)))
    (unless (or (string=? dir ".") (string=? dir "/") (file-exists? dir))
      (catch #t
        (lambda () (mkdir-p dir))
        (lambda _ #f)))))

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
  (let* ((merged (make-badb-config
                  #:model-name (or (badb-config-model-name overlay)
                                   (badb-config-model-name base))
                  #:model-endpoint (or (badb-config-model-endpoint overlay)
                                       (badb-config-model-endpoint base))
                  #:model-api-key (or (badb-config-model-api-key overlay)
                                      (badb-config-model-api-key base))
                  #:model-temperature (or (badb-config-model-temperature overlay)
                                          (badb-config-model-temperature base))
                  #:model-max-tokens (or (badb-config-model-max-tokens overlay)
                                         (badb-config-model-max-tokens base))
                  #:journal-path (or (badb-config-journal-path overlay)
                                     (badb-config-journal-path base))
                  #:options (append (badb-config-options base)
                                    (badb-config-options overlay))
                  #:source-path (or (badb-config-source-path overlay)
                                    (badb-config-source-path base))))
         ;; Merge MCP servers: overlay servers take precedence over base servers with same name
         (base-mcp (badb-config-mcp-servers base))
         (overlay-mcp (badb-config-mcp-servers overlay))
         (overlay-names (map car overlay-mcp))
         (retained-base (remove (lambda (e) (member (car e) overlay-names)) base-mcp))
         (combined-mcp (append overlay-mcp retained-base)))
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
