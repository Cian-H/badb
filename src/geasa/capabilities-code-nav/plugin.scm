;;; (geasa capabilities-code-nav plugin) — AST Code Navigation & Memory
;;;
;;; Serena-style symbol inspection, surgical symbol reading and body replacement,
;;; top-level AST outline generation across languages, and project memory management.

(define-module (geasa capabilities-code-nav plugin)
  #:use-module (badb kernel registry)
  #:use-module (geasa foundation-util plugin)
  #:use-module (json)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign-library)
  #:use-module (ts language)
  #:use-module (ts parser)
  #:use-module (ts tree)
  #:export (register-code-nav-tools!
            extract-symbols-from-file
            symbols-overview
            find-symbol-in-workspace
            read-symbol-from-file
            replace-symbol-in-file
            memory-read
            memory-write
            %symbols-overview-schema
            %find-symbol-schema
            %read-symbol-schema
            %replace-symbol-schema
            %memory-read-schema
            %memory-write-schema))

;;;
;;; Tree-sitter integration
;;;

;; Memoization cache for loaded languages
(define %ts-language-cache (make-hash-table 31))

;; Map file extensions to tree-sitter language names and symbol types
(define %ts-lang-map
  '((".scm" . ("tree_sitter_scheme" . scheme))
    (".sema" . ("tree_sitter_scheme" . scheme))
    (".py" . ("tree_sitter_python" . python))
    (".js" . ("tree_sitter_javascript" . javascript))
    (".ts" . ("tree_sitter_javascript" . javascript))
    (".c" . ("tree_sitter_c" . c))
    (".h" . ("tree_sitter_c" . c))
    (".rs" . ("tree_sitter_rust" . rust))))

(define (file-extension path)
  (let ((pos (string-rindex path #\.)))
    (if pos (substring path pos) #f)))

(define (ts-language-ref lang-name)
  (or (hash-ref %ts-language-cache lang-name)
      (catch 'system-error
        (lambda ()
          (let* ((env-path (getenv "TREE_SITTER_GRAMMAR_PATH"))
                 (search-paths (if (and env-path (not (string-null? env-path)))
                                   (string-split env-path #\:)
                                   '()))
                 (lib (find (lambda (dir)
                              (let ((lib-path (string-append dir "/libtree-sitter-" lang-name ".so")))
                                (and (file-exists? lib-path)
                                     (load-foreign-library lib-path))))
                            search-paths))
                 (lang (and lib (get-ts-language-from-file lib lang-name))))
            (when lang
              (hash-set! %ts-language-cache lang-name lang))
            lang))
        (lambda _ #f))))

(define (get-ts-language path)
  (let ((ext (file-extension path)))
    (and ext
         (let ((entry (assoc ext %ts-lang-map)))
           (and entry (ts-language-ref (car (cdr entry))))))))

(define (node->text node source)
  (substring source (ts-node-start-byte node) (ts-node-end-byte node)))

(define (node->line node)
  (+ 1 (car (ts-node-start-point node))))

(define (node->line-range node)
  (cons (+ 1 (car (ts-node-start-point node)))
        (+ 1 (car (ts-node-end-point node)))))

(define (find-named-child node field-name source)
  (let ((child-count (ts-node-child-count node)))
    (let loop ((i 0))
      (if (< i child-count)
          (let ((child (ts-node-child node i))
                (field (ts-node-field-name-for-child node i)))
            (if (and field (string=? field field-name))
                child
                (loop (+ i 1))))
          #f))))


(define (ts-extract-symbols path)
  (let* ((lang (get-ts-language path))
         (source (call-with-input-file path get-string-all))
         (parser (and lang (ts-parser-new #:language lang)))
         (tree (and parser (false-if-exception (ts-parser-parse-string parser source)))))
    (if (and tree lang)
        (let* ((root (ts-tree-root-node tree))
               (ext (file-extension path))
               (lang-type (cdr (assoc ext %ts-lang-map))))
          (cond
           ((eq? lang-type 'scheme) (extract-ts-scheme root source))
           ((eq? lang-type 'python) (extract-ts-python root source))
           ((eq? lang-type 'javascript) (extract-ts-js root source))
           ((eq? lang-type 'c) (extract-ts-c root source))
           ((eq? lang-type 'rust) (extract-ts-rust root source))
           (else '())))
        #f)))

(define (ts-walk-collect root classify-node)
  "Walk Tree-sitter tree from ROOT, invoking CLASSIFY-NODE on each node.
CLASSIFY-NODE should return (list name type line) or #f. Returns list of symbols."
  (let collect ((node root) (symbols '()))
    (let ((sym (classify-node node))
          (nc (ts-node-child-count node)))
      (let loop-c ((i 0)
                   (acc (if sym (cons sym symbols) symbols)))
        (if (< i nc)
            (loop-c (+ i 1) (collect (ts-node-child node i) acc))
            acc)))))

(define (classify-ts-scheme node source)
  (let ((nt (ts-node-type node))
        (nc (ts-node-child-count node)))
    (and (string=? nt "list")
         (>= nc 2)
         (let ((fc (ts-node-child node 1)))
           (and (ts-node-named? fc)
                (let ((ft (node->text fc source)))
                  (and (or (string=? ft "define")
                           (string=? ft "define*")
                           (string=? ft "define-syntax")
                           (string=? ft "define-module")
                           (string=? ft "define-record-type"))
                       (let* ((nn (ts-node-child node 2))
                              (nn-type (ts-node-type nn))
                              (name
                               (cond
                                ((string=? ft "define-module")
                                 (if (and nn (string=? nn-type "list"))
                                     (let loop-names ((j 0) (parts '()))
                                       (if (< j (ts-node-child-count nn))
                                           (let ((ch (ts-node-child nn j)))
                                             (if (ts-node-named? ch)
                                                 (loop-names (+ j 1) (cons (node->text ch source) parts))
                                                 (loop-names (+ j 1) parts)))
                                           (string-join (reverse parts) " ")))
                                     (and nn (ts-node-named? nn) (node->text nn source))))
                                ((string=? ft "define-record-type")
                                 (and nn (ts-node-named? nn) (node->text nn source)))
                                ((string=? ft "define-syntax")
                                 (and nn (ts-node-named? nn) (node->text nn source)))
                                ((or (string=? ft "define") (string=? ft "define*"))
                                 (if (and nn (string=? nn-type "list") (> (ts-node-child-count nn) 1))
                                     (let ((first-param (ts-node-child nn 1)))
                                       (and (ts-node-named? first-param) (node->text first-param source)))
                                     (and nn (ts-node-named? nn) (node->text nn source))))
                                (else #f)))
                              (ln (node->line node))
                              (st (cond
                                   ((string=? ft "define-module") 'module)
                                   ((string=? ft "define-record-type") 'record)
                                   ((string=? ft "define-syntax") 'syntax)
                                   ((or (string=? ft "define") (string=? ft "define*"))
                                    (if (and nn (string=? nn-type "list") (> (ts-node-child-count nn) 0))
                                        'function
                                        'variable))
                                   (else 'variable))))
                         (and name (not (string-null? name))
                              (list name st ln))))))))))

(define (extract-ts-scheme root source)
  (ts-walk-collect root (lambda (n) (classify-ts-scheme n source))))

(define (classify-ts-node node source type-mapping)
  "Classify NODE if its type matches TYPE-MAPPING: ((type-str . kind-sym) ...)."
  (let* ((nt (ts-node-type node))
         (pair (assoc nt type-mapping)))
    (and pair
         (let* ((nn (find-named-child node "name" source))
                (name (and nn (node->text nn source)))
                (ln (node->line node)))
           (and name (not (string-null? name))
                (list name (cdr pair) ln))))))

(define %ts-python-types '(("function_definition" . function) ("class_definition" . class)))
(define %ts-js-types '(("function_declaration" . function) ("class_declaration" . class)
                       ("method_definition" . method) ("lexical_declaration" . variable)))
(define %ts-c-types '(("function_definition" . function) ("preproc_def" . macro)))
(define %ts-rust-types '(("function_item" . function) ("struct_item" . struct)
                         ("enum_item" . enum) ("trait_item" . trait) ("impl_item" . impl)))

(define (classify-ts-python node source) (classify-ts-node node source %ts-python-types))
(define (extract-ts-python root source) (ts-walk-collect root (lambda (n) (classify-ts-python n source))))

(define (classify-ts-js node source) (classify-ts-node node source %ts-js-types))
(define (extract-ts-js root source) (ts-walk-collect root (lambda (n) (classify-ts-js n source))))

(define (classify-ts-c node source) (classify-ts-node node source %ts-c-types))
(define (extract-ts-c root source) (ts-walk-collect root (lambda (n) (classify-ts-c n source))))

(define (classify-ts-rust node source) (classify-ts-node node source %ts-rust-types))
(define (extract-ts-rust root source) (ts-walk-collect root (lambda (n) (classify-ts-rust n source))))


(define (ts-find-symbol-node path symbol-name)
  (let* ((lang (get-ts-language path))
         (source (call-with-input-file path get-string-all))
         (parser (and lang (ts-parser-new #:language lang)))
         (tree (and parser (false-if-exception (ts-parser-parse-string parser source)))))
    (if (and tree lang)
        (let* ((root (ts-tree-root-node tree))
               (ext (file-extension path))
               (lang-pair (assoc ext %ts-lang-map))
               (lang-type (and lang-pair (cdr lang-pair))))
          (if lang-type
              (let ((result (find-ts-node root source symbol-name lang-type)))
                (and result (node->line-range result)))
              #f))
        #f)))

(define (find-ts-node node source symbol-name lang-type)
  (let ((nc (ts-node-child-count node))
        (nt (ts-node-type node)))
    (define (is-def? n)
      (cond
       ((eq? lang-type 'scheme)
        (and (string=? (ts-node-type n) "list")
             (>= (ts-node-child-count n) 2)
             (let ((fc (ts-node-child n 1)))
               (and (ts-node-named? fc)
                    (let ((ft (node->text fc source)))
                      (or (string=? ft "define")
                          (string=? ft "define*")
                          (string=? ft "define-syntax")
                          (string=? ft "define-module")
                          (string=? ft "define-record-type")))))))
       ((eq? lang-type 'python)
        (or (string=? (ts-node-type n) "function_definition")
            (string=? (ts-node-type n) "class_definition")))
       ((eq? lang-type 'javascript)
        (or (string=? (ts-node-type n) "function_declaration")
            (string=? (ts-node-type n) "class_declaration")
            (string=? (ts-node-type n) "method_definition")
            (string=? (ts-node-type n) "lexical_declaration")))
       ((eq? lang-type 'c)
        (or (string=? (ts-node-type n) "function_definition")
            (string=? (ts-node-type n) "preproc_def")))
       ((eq? lang-type 'rust)
        (or (string=? (ts-node-type n) "function_item")
            (string=? (ts-node-type n) "struct_item")
            (string=? (ts-node-type n) "enum_item")
            (string=? (ts-node-type n) "trait_item")
            (string=? (ts-node-type n) "impl_item")))
       (else #f)))
    (define (get-name n)
      (cond
       ((eq? lang-type 'scheme)
        (and (string=? (ts-node-type n) "list")
             (>= (ts-node-child-count n) 3)
             (let* ((fc (ts-node-child n 1))
                    (ft (node->text fc source))
                    (nn (ts-node-child n 2))
                    (nn-type (ts-node-type nn)))
               (cond
                ((or (string=? ft "define-module") (string=? ft "define-record-type") (string=? ft "define-syntax"))
                 (and nn (ts-node-named? nn) (node->text nn source)))
                ((or (string=? ft "define") (string=? ft "define*"))
                 (if (and (string=? nn-type "list") (> (ts-node-child-count nn) 1))
                     (let ((first-param (ts-node-child nn 1)))
                       (and (ts-node-named? first-param) (node->text first-param source)))
                     (and nn (ts-node-named? nn) (node->text nn source))))
                (else #f)))))
       (else
        (let ((nn (find-named-child n "name" source)))
          (and nn (node->text nn source))))))
    (if (and (is-def? node)
            (let ((nt2 (get-name node)))
              (and nt2 (string=? nt2 symbol-name))))
        node
        (let loop-c ((i 0))
          (if (< i nc)
              (let* ((ch (ts-node-child node i))
                     (r (find-ts-node ch source symbol-name lang-type)))
                (if r r (loop-c (+ i 1))))
              #f)))))

;;; Line and File Reading Helpers
;;;
(define (read-file-lines path)
  (if (file-exists? path)
      (call-with-input-file path
        (lambda (p)
          (let loop ((lines '()))
            (let ((line (read-line p)))
              (if (eof-object? line)
                  (reverse lines)
                  (loop (cons line lines)))))))
      '()))

;;;
;;; Symbol Extraction
;;;
(define (extract-symbols-scheme lines)
  "Extract Scheme declarations: define, define*, define-record-type, define-syntax, define-module."
  (let loop ((rest lines) (line-num 1) (symbols '()))
    (if (null? rest)
        (reverse symbols)
        (let ((line (string-trim-both (car rest))))
          (cond
           ((string-prefix? "(define-module " line)
            (let ((m (string-match "\\(define-module[ \t]+\\(([^)]+)\\)" line)))
              (if m
                  (loop (cdr rest) (+ line-num 1)
                        (cons (list (match:substring m 1) 'module line-num) symbols))
                  (loop (cdr rest) (+ line-num 1) symbols))))
           ((string-prefix? "(define-record-type " line)
            (let ((m (string-match "\\(define-record-type[ \t]+<([^>]+)>" line)))
              (if m
                  (loop (cdr rest) (+ line-num 1)
                        (cons (list (format #f "<~a>" (match:substring m 1)) 'record line-num) symbols))
                  (loop (cdr rest) (+ line-num 1) symbols))))
           ((string-prefix? "(define-syntax " line)
            (let ((m (string-match "\\(define-syntax[ \t]+([^ \t\n()]+)" line)))
              (if m
                  (loop (cdr rest) (+ line-num 1)
                        (cons (list (match:substring m 1) 'syntax line-num) symbols))
                  (loop (cdr rest) (+ line-num 1) symbols))))
           ((or (string-prefix? "(define* (" line) (string-prefix? "(define (" line))
            (let ((m (string-match "\\(define\\*?[ \t]+\\(([^ \t\n()]+)" line)))
              (if m
                  (loop (cdr rest) (+ line-num 1)
                        (cons (list (match:substring m 1) 'function line-num) symbols))
                  (loop (cdr rest) (+ line-num 1) symbols))))
           ((or (string-prefix? "(define* " line) (string-prefix? "(define " line))
            (let ((m (string-match "\\(define\\*?[ \t]+([^ \t\n()]+)" line)))
              (if m
                  (loop (cdr rest) (+ line-num 1)
                        (cons (list (match:substring m 1) 'variable line-num) symbols))
                  (loop (cdr rest) (+ line-num 1) symbols))))
           (else
            (loop (cdr rest) (+ line-num 1) symbols)))))))

(define (extract-symbols-python lines)
  "Extract Python def and class declarations."
  (let loop ((rest lines) (line-num 1) (symbols '()))
    (if (null? rest)
        (reverse symbols)
        (let ((line (string-trim-both (car rest))))
          (cond
           ((string-prefix? "def " line)
            (let ((m (string-match "def[ \t]+([a-zA-Z0-9_]+)" line)))
              (if m
                  (loop (cdr rest) (+ line-num 1)
                        (cons (list (match:substring m 1) 'function line-num) symbols))
                  (loop (cdr rest) (+ line-num 1) symbols))))
           ((string-prefix? "class " line)
            (let ((m (string-match "class[ \t]+([a-zA-Z0-9_]+)" line)))
              (if m
                  (loop (cdr rest) (+ line-num 1)
                        (cons (list (match:substring m 1) 'class line-num) symbols))
                  (loop (cdr rest) (+ line-num 1) symbols))))
           (else
            (loop (cdr rest) (+ line-num 1) symbols)))))))

(define (extract-symbols-rust lines)
  "Extract Rust fn, struct, enum, impl, trait declarations."
  (let loop ((rest lines) (line-num 1) (symbols '()))
    (if (null? rest)
        (reverse symbols)
        (let ((line (string-trim-both (car rest))))
          (cond
           ((string-contains line "fn ")
            (let ((m (string-match "fn[ \t]+([a-zA-Z0-9_]+)" line)))
              (if m
                  (loop (cdr rest) (+ line-num 1)
                        (cons (list (match:substring m 1) 'function line-num) symbols))
                  (loop (cdr rest) (+ line-num 1) symbols))))
           ((string-contains line "struct ")
            (let ((m (string-match "struct[ \t]+([a-zA-Z0-9_]+)" line)))
              (if m
                  (loop (cdr rest) (+ line-num 1)
                        (cons (list (match:substring m 1) 'struct line-num) symbols))
                  (loop (cdr rest) (+ line-num 1) symbols))))
           ((string-contains line "enum ")
            (let ((m (string-match "enum[ \t]+([a-zA-Z0-9_]+)" line)))
              (if m
                  (loop (cdr rest) (+ line-num 1)
                        (cons (list (match:substring m 1) 'enum line-num) symbols))
                  (loop (cdr rest) (+ line-num 1) symbols))))
           (else
            (loop (cdr rest) (+ line-num 1) symbols)))))))

(define (extract-symbols-from-file path)
  "Extract symbols from file at PATH based on file extension.
   Tries tree-sitter first, falls back to regex."
  (or (ts-extract-symbols path)
      (let ((lines (read-file-lines path)))
        (cond
         ((or (string-suffix? ".scm" path) (string-suffix? ".sema" path))
          (extract-symbols-scheme lines))
         ((string-suffix? ".py" path)
          (extract-symbols-python lines))
         ((string-suffix? ".rs" path)
          (extract-symbols-rust lines))
         (else
          (append (extract-symbols-scheme lines)
                  (extract-symbols-python lines)
                  (extract-symbols-rust lines)))))))

(define (symbols-overview target-path)
  "Return formatted symbols overview for TARGET-PATH (file or directory)."
  (if (file-is-directory? target-path)
      (let* ((files (scandir-valid-entries target-path
                                           (lambda (n)
                                             (and (not (string=? n ".git"))
                                                  (or (string-suffix? ".scm" n)
                                                      (string-suffix? ".py" n)
                                                      (string-suffix? ".rs" n))))))
             (sections
              (filter-map
               (lambda (f)
                 (let* ((full-p (string-append target-path "/" f))
                        (syms (extract-symbols-from-file full-p)))
                   (if (null? syms)
                       #f
                       (format #f "## ~a\n~a" f
                               (string-join
                                (map (lambda (s) (format #f "- ~a (~a, line ~a)" (car s) (cadr s) (caddr s)))
                                     syms)
                                "\n")))))
               files)))
        (if (null? sections)
            "No declarations found."
            (string-join sections "\n\n")))
      (let ((syms (extract-symbols-from-file target-path)))
        (if (null? syms)
            (format #f "No declarations found in ~a." target-path)
            (format #f "## Symbols in ~a\n~a"
                    target-path
                    (string-join
                     (map (lambda (s) (format #f "- ~a (~a, line ~a)" (car s) (cadr s) (caddr s)))
                          syms)
                     "\n"))))))

;;;
;;; Symbol Locating & Range Finding
;;;
(define (find-symbol-range-scheme lines start-idx)
  "Find the line range (start . end) of a Scheme sexp starting at 0-based START-IDX."
  (let loop ((idx start-idx) (depth 0) (seen-open? #f))
    (if (>= idx (length lines))
        (cons (+ start-idx 1) (length lines))
        (let* ((line (list-ref lines idx))
               (chars (string->list line))
               (new-depth depth)
               (has-open? seen-open?))
          (for-each
           (lambda (c)
             (cond
              ((char=? c #\()
               (set! new-depth (+ new-depth 1))
               (set! has-open? #t))
              ((char=? c #\))
               (set! new-depth (- new-depth 1)))))
           chars)
          (if (and has-open? (<= new-depth 0))
              (cons (+ start-idx 1) (+ idx 1))
              (loop (+ idx 1) new-depth has-open?))))))

(define (find-symbol-in-file path symbol-name)
  "Look for SYMBOL-NAME in PATH and return (file start-line end-line) or #f.
   Tries tree-sitter first for exact ranges, falls back to regex."
  (let ((ts-range (ts-find-symbol-node path symbol-name)))
    (if ts-range
        (list path (car ts-range) (cdr ts-range))
        (let* ((lines (read-file-lines path))
               (syms (extract-symbols-from-file path))
               (match (find (lambda (s) (equal? (car s) symbol-name)) syms)))
          (if match
              (let* ((start-line (caddr match))
                     (range (if (or (string-suffix? ".scm" path) (string-suffix? ".sema" path))
                                (find-symbol-range-scheme lines (- start-line 1))
                                (cons start-line (+ start-line 10)))))
                (list path (car range) (cdr range)))
              #f)))))


(define* (find-symbol-in-workspace name #:optional (root (getcwd)))
  "Search workspace ROOT for declarations matching NAME."
  (let ((found '()))
    (define (scan! d)
      (let ((entries (scandir-valid-entries d (lambda (n) (not (member n '(".git" ".badb")))))))
        (for-each
         (lambda (e)
           (let ((full (string-append d "/" e)))
             (cond
              ((file-is-directory? full) (scan! full))
              ((or (string-suffix? ".scm" e) (string-suffix? ".py" e) (string-suffix? ".rs" e))
               (let ((hit (find-symbol-in-file full name)))
                 (when hit (set! found (cons hit found))))))))
         entries)))
    (if (file-is-directory? root)
        (scan! root)
        (let ((hit (find-symbol-in-file root name)))
          (when hit (set! found (list hit)))))
    (if (null? found)
        (format #f "Symbol '~a' not found." name)
        (string-join
         (map (lambda (h) (format #f "Found symbol '~a' in ~a: lines ~a-~a"
                                  name (car h) (cadr h) (caddr h)))
              found)
         "\n"))))

(define (read-symbol-from-file file name)
  "Read the exact body of symbol NAME in FILE."
  (let ((hit (find-symbol-in-file file name)))
    (if hit
        (let* ((lines (read-file-lines file))
               (start (- (cadr hit) 1))
               (end (caddr hit))
               (target-lines (take (drop lines start) (- end start))))
          (string-join target-lines "\n"))
        (format #f "Symbol '~a' not found in '~a'." name file))))

(define (replace-symbol-in-file file name new-body)
  "Replace the declaration/body of symbol NAME in FILE with NEW-BODY."
  (let ((hit (find-symbol-in-file file name)))
    (if hit
        (let* ((lines (read-file-lines file))
               (start (- (cadr hit) 1))
               (end (caddr hit))
               (prefix (take lines start))
               (suffix (drop lines end))
               (new-lines (append prefix (list new-body) suffix)))
          (call-with-output-file file
            (lambda (p)
              (for-each
               (lambda (l) (display l p) (newline p))
               new-lines)))
          (format #f "Successfully updated symbol '~a' in ~a (lines ~a-~a)"
                  name file (cadr hit) (caddr hit)))
        (format #f "Symbol '~a' not found in '~a'." name file))))

;;;
;;; Project Memories
;;;
(define* (memory-read topic #:key (workspace (getcwd)))
  "Read project memory from WORKSPACE/.badb/memories/<topic>.md.
If topic is 'all' or empty, lists all available memories."
  (let ((mem-dir (string-append workspace "/.badb/memories")))
    (if (or (string-null? topic) (string=? topic "all"))
        (if (file-exists? mem-dir)
            (let ((files (or (scandir mem-dir (lambda (n) (string-suffix? ".md" n))) '())))
              (if (null? files)
                  "No project memories found."
                  (format #f "Available project memories:\n~a"
                          (string-join
                           (map (lambda (f) (format #f "- ~a" (string-drop-right f 3))) files)
                           "\n"))))
            "No project memories directory (.badb/memories) found.")
        (let* ((file (string-append mem-dir "/" topic (if (string-suffix? ".md" topic) "" ".md"))))
          (if (file-exists? file)
              (call-with-input-file file get-string-all)
              (format #f "Memory topic '~a' not found." topic))))))

(define* (memory-write topic content #:key (workspace (getcwd)))
  "Write CONTENT to WORKSPACE/.badb/memories/<topic>.md."
  (let* ((mem-dir (string-append workspace "/.badb/memories"))
         (file (string-append mem-dir "/" topic (if (string-suffix? ".md" topic) "" ".md"))))
    (mkdir-p mem-dir)
    (call-with-output-file file
      (lambda (p) (display content p)))
    (format #f "Saved memory '~a' (~a characters)" topic (string-length content))))

;;;
;;; Schemas & Registration
;;;
(define %symbols-overview-schema
  `((name . "symbols_overview")
    (description . "Extract high-density symbol outline (functions, classes, records, macros) from a file or directory.")
    (parameters .
     ((type . "object")
      (properties .
       ((path . ((type . "string") (description . "Path to source file or directory")))))
      (required . #("path"))))))

(define %find-symbol-schema
  `((name . "find_symbol")
    (description . "Locate declarations of a symbol across workspace files with exact line numbers.")
    (parameters .
     ((type . "object")
      (properties .
       ((name . ((type . "string") (description . "Symbol name to search for")))
        (path . ((type . "string") (description . "Optional directory or file root")))))
      (required . #("name"))))))

(define %read-symbol-schema
  `((name . "read_symbol")
    (description . "Read ONLY the body of a specific symbol/function, omitting all other file contents.")
    (parameters .
     ((type . "object")
      (properties .
       ((name . ((type . "string") (description . "Symbol name to read")))
        (file . ((type . "string") (description . "File path containing the symbol")))))
      (required . #("name" "file"))))))

(define %replace-symbol-schema
  `((name . "replace_symbol_body")
    (description . "Replace the body of a specific symbol cleanly while preserving the surrounding file.")
    (parameters .
     ((type . "object")
      (properties .
       ((name . ((type . "string") (description . "Symbol name to replace")))
        (file . ((type . "string") (description . "File path containing the symbol")))
        (new_body . ((type . "string") (description . "Replacement code for the symbol")))))
      (required . #("name" "file" "new_body"))))))

(define %memory-read-schema
  `((name . "memory_read")
    (description . "Read persistent project memory from .badb/memories/<topic>.md (or 'all' to list topics).")
    (parameters .
     ((type . "object")
      (properties .
       ((topic . ((type . "string") (description . "Memory topic name or 'all'")))))
      (required . #("topic"))))))

(define %memory-write-schema
  `((name . "memory_write")
    (description . "Save or update persistent project memory under .badb/memories/<topic>.md.")
    (parameters .
     ((type . "object")
      (properties .
       ((topic . ((type . "string") (description . "Memory topic name")))
        (content . ((type . "string") (description . "Markdown memory content")))))
      (required . #("topic" "content"))))))

(define* (register-code-nav-tools! reg #:key (workspace (getcwd)))
  "Register code-nav and memory tools in REG."
  (registry-attach!
   reg
   "symbols_overview"
   (lambda (args)
     (with-tool-args args ((p path workspace))
       (symbols-overview p)))
   #:metadata %symbols-overview-schema)

  (registry-attach!
   reg
   "find_symbol"
   (lambda (args)
     (with-tool-args args ((name name "")
                           (p path workspace))
       (find-symbol-in-workspace name p)))
   #:metadata %find-symbol-schema)

  (registry-attach!
   reg
   "read_symbol"
   (lambda (args)
     (with-tool-args args ((name name "")
                           (f file ""))
       (read-symbol-from-file f name)))
   #:metadata %read-symbol-schema)

  (registry-attach!
   reg
   "replace_symbol_body"
   (lambda (args)
     (with-tool-args args ((name name "")
                           (f file "")
                           (body new_body ""))
       (replace-symbol-in-file f name body)))
   #:metadata %replace-symbol-schema)

  (registry-attach!
   reg
   "memory_read"
   (lambda (args)
     (with-tool-args args ((t topic "all"))
       (memory-read t #:workspace workspace)))
   #:metadata %memory-read-schema)

  (registry-attach!
   reg
   "memory_write"
   (lambda (args)
     (with-tool-args args ((t topic "")
                           (c content ""))
       (memory-write t c #:workspace workspace)))
   #:metadata %memory-write-schema))
