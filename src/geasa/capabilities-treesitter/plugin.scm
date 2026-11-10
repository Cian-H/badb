;;; (geasa capabilities-treesitter plugin) — Tree-sitter Structural Polyglot Editing
;;;
;;; Integrates guile-treesitter to parse and query non-Lisp languages
;;; (Python, Rust, C, TypeScript) as concrete syntax trees (CSTs)
;;; represented natively as S-expressions.
;;;
;;; When native guile-treesitter bindings are installed they are used
;;; directly; otherwise a pure-Scheme structural fallback produces
;;; CST-like S-expressions so the capability degrades gracefully and
;;; always exposes the same tool surface.

(define-module (geasa capabilities-treesitter plugin)
               #:use-module (badb kernel registry)
               #:use-module (geasa foundation-util plugin)
               #:use-module (ice-9 format)
               #:use-module (ice-9 match)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-13)
               #:export (parse-language
                          cst->sexp
                          parse-python
                          parse-rust
                          parse-c
                          parse-typescript
                          list-supported-languages
                          ts-available?
                          register-treesitter-tools!))

;; ---- native backend detection ------------------------------------------

;; The guile-treesitter bindings expose (treesitter parser).  Resolve the
;; module without error if it is absent; in that case the pure-Scheme
;; fallback parser below takes over transparently.
(define %native-parser
        (false-if-exception (resolve-module '(treesitter parser) #:ensure #f)))

(define (ts-available?)
        "Return #t when native guile-treesitter bindings are loadable."
        (and %native-parser #t))

(define (%native-parse lang source-text)
        "Parse with native guile-treesitter bindings.  Requires the resolved
(treesitter parser) module to export `parse-string'."
        (let ((proc (false-if-exception (module-ref %native-parser 'parse-string))))
          (unless proc
            (error "native treesitter module lacks parse-string"))
          (proc (symbol->string lang) source-text)))

;; ---- language detection helpers ----------------------------------------

(define (%language-identifier lang-name)
        "Return the canonical language symbol for LANGUAGE-NAME."
        (let ((norm (string-downcase (if (symbol? lang-name)
                                       (symbol->string lang-name)
                                       lang-name))))
          (cond ((string-contains norm "python") 'python)
                ((string-contains norm "rust")   'rust)
                ((string=? norm "c")             'c)
                ((string-contains norm "typescript") 'typescript)
                (else 'unknown))))

;; ---- fallback structural parser (pure Scheme) ---------------------------

(define (%strip-comment line lang)
        (let ((idx (cond ((eq? lang 'python) (string-index line #\#))
                         ((memq lang '(c rust typescript))
                           (string-contains line "//"))
                         (else #f))))
          (if idx (string-take line idx) line)))

(define (%indent-width line)
        (- (string-length line) (string-length (string-trim line))))

(define (%python-blocks lines)
        "Nest Python LINES by indentation into a (module ...) S-expression CST."
        (define (parse-level lines indent)
                (let loop ((rest lines) (nodes '()))
                          (if (null? rest)
                            (values (reverse nodes) '())
                            (let* ((raw (car rest))
                                   (stripped (string-trim-right (%strip-comment raw 'python)))
                                   (cur (string-trim stripped)))
                              (cond
                                ((string=? cur "")
                                  (loop (cdr rest) nodes))
                                ((< (%indent-width stripped) indent)
                                  (values (reverse nodes) rest))
                                (else
                                  (let* ((head (string-take cur
                                                            (or (string-index cur #\space)
                                                                (string-length cur))))
                                         (kind (match head
                                                      ("def" 'function-def)
                                                      ("class" 'class-def)
                                                      (_ 'statement)))
                                         (name (match (string-split cur #\space)
                                                      ((_ nm . _) nm)
                                                      (_ "?"))))
                                    (call-with-values
                                      (lambda () (parse-level (cdr rest) (+ indent 1)))
                                      (lambda (children remaining)
                                        (loop remaining
                                              (cons (list kind name children) nodes)))))))))))
        (call-with-values
          (lambda () (parse-level lines 0))
          (lambda (nodes _) (cons 'module nodes))))

(define (%brace-blocks text lang)
        "Produce a structural outline for brace-language TEXT (C / Rust /
TypeScript): one (item DEPTH TEXT) node per non-blank line, DEPTH being
the brace nesting depth at which the line occurs."
        (let loop ((lines (string-split text #\newline))
                   (depth 0)
                   (out '()))
                  (match lines
                         (() (cons (if (eq? lang 'c) 'translation-unit 'source-file)
                                   (reverse out)))
                         ((line . more)
                          (let ((clean (string-trim (%strip-comment line lang))))
                            (loop more
                                  (+ depth (string-count clean #\{)
                                     (- (string-count clean #\})))
                                  (if (string=? clean "")
                                    out
                                    (cons (list 'item depth clean) out))))))))

;; ---- CST serialisation & querying ---------------------------------------

(define (cst->sexp node)
        "Serialise CST NODE to a plain S-expression (nested proper lists)."
        (if (pair? node)
          (map cst->sexp node)
          node))

(define (walk-cst node pred)
        "Depth-first collect of CST NODE's subnodes for which PRED returns true."
        (cond ((not (pair? node)) '())
              ((pred node)
                (cons node (append-map (lambda (child) (walk-cst child pred))
                                       (cdr node))))
              (else (append-map (lambda (child) (walk-cst child pred))
                                (cdr node)))))

;; ---- public parsing API --------------------------------------------------

(define* (parse-language lang source-text
                         #:key (backend (if (ts-available?) 'native 'fallback)))
         "Parse SOURCE-TEXT as LANG and return an S-expression CST.
BACKEND selects the native guile-treesitter parser when available;
otherwise the structural fallback parser is used.  If the native
backend errors, the fallback parser engages automatically."
         (let ((lang-sym (%language-identifier lang)))
           (cond
             ((eq? lang-sym 'unknown)
               (error "unsupported language" lang))
             ((and (eq? backend 'native) %native-parser)
               (or (false-if-exception (cst->sexp (%native-parse lang-sym source-text)))
                   (parse-language lang source-text #:backend 'fallback)))
             (else
               (match lang-sym
                      ('python
                        (cst->sexp (%python-blocks (string-split source-text #\newline))))
                      ((or 'rust 'c 'typescript)
                       (cst->sexp (%brace-blocks source-text lang-sym)))
                      (_ (error "unsupported language" lang)))))))

(define (parse-python source-text)
        "Parse Python SOURCE-TEXT into an S-expression CST."
        (parse-language "python" source-text #:backend 'fallback))

(define (parse-rust source-text)
        "Parse Rust SOURCE-TEXT into an S-expression CST."
        (parse-language "rust" source-text #:backend 'fallback))

(define (parse-c source-text)
        "Parse C SOURCE-TEXT into an S-expression CST."
        (parse-language "c" source-text #:backend 'fallback))

(define (parse-typescript source-text)
        "Parse TypeScript SOURCE-TEXT into an S-expression CST."
        (parse-language "typescript" source-text #:backend 'fallback))

(define (list-supported-languages)
        "Return the languages this capability supports."
        '(python rust c typescript))

;; ---- tool registration ---------------------------------------------------

(define %ts-parse-schema
        `((name . "treesitter_parse")
          (description . "Parse source text in a supported language into an S-expression CST.")
          (parameters . ((type . "object")
                         (properties . ((lang . ((type . "string")))
                                        (source . ((type . "string")))))
                         (required . #("lang" "source"))))
          (_badb_defer . #t)))

(define %ts-query-schema
        `((name . "treesitter_query")
          (description . "Query an S-expression CST for nodes matching PATTERN.")
          (parameters . ((type . "object")
                         (properties . ((cst . ((type . "string")))
                                        (pattern . ((type . "string")))))
                         (required . #("cst" "pattern"))))
          (_badb_defer . #t)))

(define %ts-languages-schema
        `((name . "treesitter_languages")
          (description . "List languages supported by the treesitter capability.")
          (parameters . ((type . "object") (properties . ())))
          (_badb_defer . #t)))

(define* (register-treesitter-tools! reg)
         "Register the treesitter capability tools in registry REG."
         (registry-attach!
           reg "treesitter_parse"
           (lambda (args)
             (with-tool-args args ((lang lang "")
                                   (source source ""))
                             (call-with-output-string
                               (lambda (port)
                                 (write (parse-language lang source) port)))))
           #:metadata %ts-parse-schema)

         (registry-attach!
           reg "treesitter_query"
           (lambda (args)
             (with-tool-args args ((cst cst "")
                                   (pattern pattern ""))
                             (let* ((sexp (call-with-input-string cst read))
                                    (hits (walk-cst sexp
                                                    (lambda (node)
                                                      (string-contains (format #f "~s" node)
                                                                       pattern)))))
                               (call-with-output-string
                                 (lambda (port)
                                   (for-each (lambda (hit) (write hit port) (newline port))
                                             hits))))))
           #:metadata %ts-query-schema)

         (registry-attach!
           reg "treesitter_languages"
           (lambda (args)
             (format #f "~s" (list-supported-languages)))
           #:metadata %ts-languages-schema))
