;;; (badb kernel registry) — Module System & Indirection Registry
(define-module (badb kernel registry)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 format)
  #:export (<registry>
            make-registry registry? registry-attach! registry-detach!
            registry-lookup registry-metadata registry-list registry-dispatch
            make-fresh-feature-module eval-in-feature-module
            feature-module-define! feature-module-ref))

(define-record-type <feature-entry>
  (make-feature-entry id implementation metadata)
  feature-entry?
  (id entry-id)
  (implementation entry-implementation set-entry-implementation!)
  (metadata entry-metadata set-entry-metadata!))

(define-record-type <registry>
  (%make-registry table mutex)
  registry?
  (table registry-table)
  (mutex registry-mutex))

(define (make-registry)
  (%make-registry (make-hash-table 31) (make-mutex)))

(define (canonical-feature-id id)
  (cond ((symbol? id) id) ((string? id) (string->symbol id))
        (else (error "feature-id must be a symbol or string" id))))

(define (entry-id->string id)
  (cond ((string? id) id) ((symbol? id) (symbol->string id))
        ((keyword? id) (symbol->string (keyword->symbol id)))
        (else (format #f "~a" id))))

(define* (registry-attach! reg feature-id implementation
                           #:optional (meta-pos '()) #:key (metadata '()))
  (unless (registry? reg) (error "registry-attach!: expected <registry>" reg))
  (let ((key (canonical-feature-id feature-id))
        (meta (if (null? metadata) meta-pos metadata)))
    (with-mutex (registry-mutex reg)
      (let ((entry (make-feature-entry feature-id implementation meta)))
        (hashq-set! (registry-table reg) key entry)
        entry))))

(define (registry-detach! reg feature-id)
  (unless (registry? reg) (error "registry-detach!: expected <registry>" reg))
  (let ((key (canonical-feature-id feature-id)))
    (with-mutex (registry-mutex reg)
      (let ((entry (hashq-ref (registry-table reg) key)))
        (if entry (begin (hashq-remove! (registry-table reg) key) #t) #f)))))

(define (registry-lookup reg feature-id)
  (unless (registry? reg) (error "registry-lookup: expected <registry>" reg))
  (let ((key (canonical-feature-id feature-id)))
    (with-mutex (registry-mutex reg)
      (let ((entry (hashq-ref (registry-table reg) key)))
        (if entry (entry-implementation entry) #f)))))

(define (registry-metadata reg feature-id)
  (unless (registry? reg) (error "registry-metadata: expected <registry>" reg))
  (let ((key (canonical-feature-id feature-id)))
    (with-mutex (registry-mutex reg)
      (let ((entry (hashq-ref (registry-table reg) key)))
        (if entry (entry-metadata entry) #f)))))

(define (registry-list reg)
  (unless (registry? reg) (error "registry-list: expected <registry>" reg))
  (let ((entries (with-mutex (registry-mutex reg)
                   (hash-map->list (lambda (k v) v) (registry-table reg)))))
    (map (lambda (e) (list (cons 'id (entry-id e)) (cons 'metadata (entry-metadata e))))
         (sort entries
               (lambda (a b)
                 (string<? (entry-id->string (entry-id a))
                           (entry-id->string (entry-id b))))))))

(define (registry-dispatch reg feature-id . args)
  (let ((impl (registry-lookup reg feature-id)))
    (cond
     ((not impl) (error "registry-dispatch: feature not found" feature-id))
     ((procedure? impl) (apply impl args))
     (else (error "registry-dispatch: implementation is not a procedure" feature-id impl)))))

(define (normalize-module-name name)
  (cond ((not name) #f) ((list? name) name) ((symbol? name) (list name))
        ((string? name) (list (string->symbol name)))
        (else (error "make-fresh-feature-module: invalid module name" name))))

(define* (make-fresh-feature-module #:optional (name #f) #:key (imports '((guile))))
  (let ((mod (make-module)) (norm-name (normalize-module-name name)))
    (when norm-name
      (set-module-name! mod norm-name)
      (call-with-module-autoload-lock
       (lambda () (nested-define-module! (resolve-module '() #f) norm-name mod))))
    (beautify-user-module! mod)
    (set-module-declarative?! mod #f)
    (unless (member '(guile) imports) (purify-module! mod))
    (for-each (lambda (imp)
                (unless (equal? imp '(guile))
                  (module-use! mod (resolve-interface imp))))
              imports)
    mod))

(define (eval-in-feature-module mod form) (eval form mod))
(define (feature-module-define! mod sym val) (module-define! mod sym val))
(define (feature-module-ref mod sym . rest) (apply module-ref mod sym rest))
