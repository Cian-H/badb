;;; badb — Module System & Indirection Registry
;;;
;;; Provides dynamic creation of isolated fresh user modules and a centralized,
;;; thread-safe registry for hot-attaching, hot-swapping, detaching, and dynamically
;;; dispatching features, tools, and extensions.

(define-module (badb registry)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 threads)
  #:export (make-registry
            registry?
            registry-attach!
            registry-detach!
            registry-lookup
            registry-metadata
            registry-list
            registry-dispatch
            make-fresh-feature-module
            eval-in-feature-module
            feature-module-define!
            feature-module-ref))

;;;
;;; Feature entry record
;;;

(define-record-type <feature-entry>
  (make-feature-entry id implementation metadata)
  feature-entry?
  (id entry-id)
  (implementation entry-implementation set-entry-implementation!)
  (metadata entry-metadata set-entry-metadata!))

;;;
;;; Indirection Registry record
;;;

(define-record-type <registry>
  (%make-registry table mutex)
  registry?
  (table registry-table)
  (mutex registry-mutex))

(define (make-registry)
  "Create a new indirection registry instance."
  (%make-registry (make-hash-table 31) (make-mutex)))

;;;
;;; Feature ID normalization
;;;

(define (canonical-feature-id id)
  "Convert feature-id (symbol or string) into canonical symbol key."
  (cond
   ((symbol? id) id)
   ((string? id) (string->symbol id))
   (else (error "feature-id must be a symbol or string" id))))

(define (entry-id->string id)
  "Convert entry id to string for sorting."
  (cond
   ((string? id) id)
   ((symbol? id) (symbol->string id))
   (else (object->string id))))

;;;
;;; Registry operations
;;;

(define* (registry-attach! reg feature-id implementation
                           #:optional (meta-pos '())
                           #:key (metadata '()))
  "Attach or hot-swap an IMPLEMENTATION under FEATURE-ID (symbol or string)
with optional METADATA alist. Atomically replaces any prior attachment."
  (unless (registry? reg)
    (error "registry-attach!: expected <registry>" reg))
  (let ((key (canonical-feature-id feature-id))
        (meta (if (null? metadata) meta-pos metadata)))
    (with-mutex (registry-mutex reg)
      (let ((entry (make-feature-entry feature-id implementation meta)))
        (hash-set! (registry-table reg) key entry)
        entry))))

(define (registry-detach! reg feature-id)
  "Remove FEATURE-ID from REG. Returns #t if removed, #f if not found."
  (unless (registry? reg)
    (error "registry-detach!: expected <registry>" reg))
  (let ((key (canonical-feature-id feature-id)))
    (with-mutex (registry-mutex reg)
      (let ((entry (hash-ref (registry-table reg) key)))
        (if entry
            (begin
              (hash-remove! (registry-table reg) key)
              #t)
            #f)))))

(define (registry-lookup reg feature-id)
  "Look up FEATURE-ID in REG. Returns attached implementation or #f."
  (unless (registry? reg)
    (error "registry-lookup: expected <registry>" reg))
  (let ((key (canonical-feature-id feature-id)))
    (with-mutex (registry-mutex reg)
      (let ((entry (hash-ref (registry-table reg) key)))
        (if entry
            (entry-implementation entry)
            #f)))))

(define (registry-metadata reg feature-id)
  "Look up metadata alist for FEATURE-ID in REG. Returns metadata alist or #f."
  (unless (registry? reg)
    (error "registry-metadata: expected <registry>" reg))
  (let ((key (canonical-feature-id feature-id)))
    (with-mutex (registry-mutex reg)
      (let ((entry (hash-ref (registry-table reg) key)))
        (if entry
            (entry-metadata entry)
            #f)))))

(define (registry-list reg)
  "Return a sorted list of registered feature entries: ((id . <id>) (metadata . <metadata>))."
  (unless (registry? reg)
    (error "registry-list: expected <registry>" reg))
  (with-mutex (registry-mutex reg)
    (let ((entries (hash-map->list (lambda (k v) v) (registry-table reg))))
      (map (lambda (entry)
             (list (cons 'id (entry-id entry))
                   (cons 'metadata (entry-metadata entry))))
           (sort entries
                 (lambda (a b)
                   (string<? (entry-id->string (entry-id a))
                             (entry-id->string (entry-id b)))))))))

(define (registry-dispatch reg feature-id . args)
  "Look up FEATURE-ID in REG and apply the attached procedure to ARGS.
Raises an error if the feature is not found or is not a procedure."
  (let ((impl (registry-lookup reg feature-id)))
    (cond
     ((not impl)
      (error "registry-dispatch: feature not found" feature-id))
     ((procedure? impl)
      (apply impl args))
     (else
      (error "registry-dispatch: implementation is not a procedure" feature-id impl)))))

;;;
;;; Fresh Feature Module system
;;;

(define (normalize-module-name name)
  "Normalize module name to a list of symbols, or #f for anonymous."
  (cond
   ((not name) #f)
   ((list? name) name)
   ((symbol? name) (list name))
   ((string? name) (list (string->symbol name)))
   (else (error "make-fresh-feature-module: invalid module name" name))))

(define* (make-fresh-feature-module #:optional (name #f)
                                   #:key (imports '((guile))))
  "Create an isolated anonymous or named user module with the specified IMPORTS
(defaults to '((guile))), isolated from the harness kernel."
  (let ((mod (make-module))
        (norm-name (normalize-module-name name)))
    (when norm-name
      (set-module-name! mod norm-name)
      (call-with-module-autoload-lock
       (lambda ()
         (nested-define-module! (resolve-module '() #f) norm-name mod))))
    (beautify-user-module! mod)
    (set-module-declarative?! mod #f)
    (unless (member '(guile) imports)
      (purify-module! mod))
    (for-each
     (lambda (imp)
       (unless (equal? imp '(guile))
         (module-use! mod (resolve-interface imp))))
     imports)
    mod))

(define (eval-in-feature-module mod form)
  "Evaluate expression FORM within the environment of feature module MOD."
  (eval form mod))

(define (feature-module-define! mod sym val)
  "Define symbol SYM with value VAL in feature module MOD."
  (module-define! mod sym val))

(define (feature-module-ref mod sym . rest)
  "Retrieve value of symbol SYM in feature module MOD, with optional default."
  (apply module-ref mod sym rest))
