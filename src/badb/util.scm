;;; badb — Shared Utility Library
;;;
;;; Reusable primitives and cross-cutting helpers for badb Scheme modules.
;;; Centralizes filesystem creation, coercion, property list & alist lookup,
;;; safe JSON serialization, and environment variable formatting to keep
;;; subsystems DRY and maintainable.

(define-module (badb util) #:use-module
               (ice-9 format)
               #:use-module
               (json)
               #:use-module
               (srfi srfi-1)
               #:export
               (mkdir-p ensure-parent-directory!
                        ensure-string
                        ensure-symbol
                        ensure-keyword
                        normalize-role
                        get-prop
                        prop-ref
                        prop-set
                        safe-json-string
                        safe-json-parse
                        normalize-env-alist
                        env-alist->exec-strings))

;;;
;;; Filesystem Helpers
;;;
(define (mkdir-p dir)
  "Recursively ensure that directory DIR exists."
  (unless (or (not (string? dir))
              (string-null? dir)
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

(define (ensure-parent-directory! file-path)
  "Ensure that the enclosing parent directory of FILE-PATH exists."
  (when (string? file-path)
    (let ((dir (dirname file-path)))
      (when (and dir (not (string-null? dir)) (not (string=? dir ".")))
        (mkdir-p dir)))))

;;;
;;; Coercion & Identifier Normalization
;;;
(define (ensure-string val)
  "Convert VAL to string representation."
  (cond
   ((string? val) val)
   ((symbol? val) (symbol->string val))
   ((keyword? val) (symbol->string (keyword->symbol val)))
   (else (object->string val))))

(define (ensure-symbol val)
  "Convert VAL to symbol representation."
  (cond
   ((symbol? val) val)
   ((string? val) (string->symbol val))
   ((keyword? val) (keyword->symbol val))
   (else (string->symbol (object->string val)))))

(define (ensure-keyword val)
  "Convert VAL to Guile keyword representation."
  (cond
   ((keyword? val) val)
   ((symbol? val) (symbol->keyword val))
   ((string? val) (symbol->keyword (string->symbol val)))
   (else (symbol->keyword (string->symbol (object->string val))))))

(define (normalize-role role)
        "Normalize message ROLE into canonical string ('system', 'user', 'assistant', 'tool')."
        (cond
          ((or (eq? role 'user) (equal? role "user")) "user")
          ((or (eq? role 'assistant) (equal? role "assistant")) "assistant")
          ((or (eq? role 'system) (equal? role "system")) "system")
          ((or (eq? role 'tool) (equal? role "tool")) "tool")
          (else
            (let ((s (string-downcase (ensure-string role))))
              (if (string-prefix? ":" s)
                (substring s 1)
                s)))))

;;;
;;; Property & Alist Accessors
;;;
(define (get-prop lst key default)
  "Safely extract KEY from property list or alist LST, supporting both
(key . val), (key val), and #:key val syntax."
  (let* ((sym (ensure-symbol key))
         (str (symbol->string sym))
         (kw (symbol->keyword sym)))
    (let loop ((items lst))
      (cond
       ((null? items) default)
       ((not (pair? items)) default)
       ((and (pair? (car items))
             (or (equal? (caar items) sym)
                 (equal? (caar items) str)
                 (equal? (caar items) kw)))
        (let ((val (cdar items)))
          (if (and (pair? val) (null? (cdr val)))
              (car val)
              val)))
       ((and (keyword? (car items)) (equal? (car items) kw) (pair? (cdr items)))
        (cadr items))
       (else (loop (cdr items)))))))

(define*
  (prop-ref alist key #:optional (default #f))
  "Safely retrieve value for KEY from ALIST, checking symbol, string, or keyword variants."
  (if (not (pair? alist))
    default
    (let*
        ((sym (ensure-symbol key))
         (str (symbol->string sym))
         (kw (symbol->keyword sym)))
      (let loop
        ((rest alist))
        (if (null? rest)
          default
          (let ((pair (car rest)))
            (if
                (and (pair? pair)
                     (let ((k (car pair)))
                       (or (eq? k sym) (eq? k kw) (and (string? k) (string=? k str)) (equal? k key))))
              (cdr pair)
              (loop (cdr rest)))))))))

(define (prop-set alist key val)
  "Update or insert KEY with VAL in ALIST, preserving or using symbol representation."
  (let* ((sym (ensure-symbol key))
         (str (symbol->string sym))
         (filtered (if (pair? alist)
                       (remove (lambda (pair)
                                 (and (pair? pair)
                                      (or (equal? (car pair) sym)
                                          (equal? (car pair) str))))
                               alist)
                       '())))
    (cons (cons sym val) filtered)))

;;;
;;; Safe JSON Serialization
;;;
(define (safe-json-string val)
  "Return VAL as string. If not already a string, serializes VAL to JSON."
  (cond
   ((string? val) val)
   (else (catch #t
           (lambda () (scm->json-string val))
           (lambda _ (object->string val))))))

(define* (safe-json-parse str #:optional (fallback #f))
  "Parse JSON string STR, returning FALLBACK if parsing fails."
  (if (or (not (string? str)) (string-null? (string-trim-both str)))
      fallback
      (catch #t
        (lambda () (json-string->scm str))
        (lambda _ fallback))))

;;;
;;; Environment Formatting
;;;
(define (normalize-env-alist raw-env)
        "Normalize heterogenous environment specifications into canonical ((str-key . str-val) ...)."
        (cond
          ((null? raw-env) '())
          ((and (pair? raw-env) (or (symbol? (car raw-env)) (string? (car raw-env))))
            (list (cons (ensure-string (car raw-env)) (ensure-string (cdr raw-env)))))
          ((list? raw-env)
            (filter-map
              (lambda (item)
                (cond
                  ((and (pair? item) (or (symbol? (car item)) (string? (car item))))
                    (cons (ensure-string (car item)) (ensure-string (cdr item))))
                  ((string? item)
                    (let ((idx (string-contains item "=")))
                      (if idx
                        (cons (substring item 0 idx) (substring item (+ idx 1)))
                        #f)))
                  (else #f)))
              raw-env))
          (else '())))

(define (env-alist->exec-strings env-alist)
  "Convert canonical environment alist into list of 'KEY=VAL' strings."
  (map (lambda (pair)
         (format #f "~a=~a" (ensure-string (car pair)) (ensure-string (cdr pair))))
       env-alist))
