;;; badb/filetree-mcp.scm — Filetree MCP server.
;;;
;;; Serves the directory it was started in as an MCP server over
;;; stdio, built on the (badb mcp-server) core. The tools expose a
;;; read-only view of the workspace: "tree" renders an indented project
;;; tree, "find" does a case-insensitive substring search over file
;;; names, "outline" returns the structure of a single file, and "read"
;;; returns bounded file contents. Hidden entries and .git are never
;;; listed; the outline and read tools reject paths that escape the
;;; workspace root.

(define-module (badb filetree-mcp)
  #:use-module (badb mcp-server)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:export (%filetree-version
            %tree-entry-cap
            %find-result-cap
            make-filetree-server
            run-filetree-mcp
            filetree-tree
            filetree-find
            filetree-outline
            filetree-read))

(define %filetree-version "0.1.0")
(define %tree-entry-cap 500)
(define %find-result-cap 100)
(define %read-max-chars 10000)

;;;
;;; Tool argument helpers (inbound tool args are string-keyed alists).
;;;

(define (arg-string args key)
  "Return the string value of KEY in tool ARGS, raising a tool error
when it is missing or not a string."
  (let ((v (assoc-ref args key)))
    (if (string? v)
        v
        (throw 'filetree-error (format #f "missing argument: ~a" key)))))

(define (arg-number args key default)
  "Return the numeric value of KEY in tool ARGS, falling back to
DEFAULT when absent. String values are parsed; anything unparsable
raises a tool error."
  (let ((v (assoc-ref args key)))
    (cond ((number? v) v)
          ((string? v) (or (string->number v)
                           (throw 'filetree-error
                                  (format #f "not a number: ~a" v))))
          ((not v) default)
          (else (throw 'filetree-error
                        (format #f "not a number: ~a" v))))))

(define (guard-tool thunk)
  "Run THUNK, converting filetree tool errors into their message
strings so the MCP core reports them as readable tool results."
  (catch 'filetree-error
    thunk
    (lambda (key message) message)))

;;;
;;; Workspace guards.
;;;

(define (hidden-entry? name)
  "Return #t when NAME should never be listed: dot-prefixed entries
and .git."
  (or (string-prefix? "." name) (string=? name ".git")))

(define (entry-dir? path)
  "Return #t when PATH is a directory; #f for files, symlinks, and
unreadable entries."
  (catch 'system-error
    (lambda () (eq? (stat:type (lstat path)) 'directory))
    (lambda _ #f)))

(define (resolve-in-root root rel)
  "Canonicalize REL against workspace ROOT and return the absolute
path, raising a tool error when the path does not exist or resolves
outside ROOT."
  (let* ((joined (if (string-prefix? "/" rel)
                     rel
                     (string-append root "/" rel)))
         (real (catch 'system-error
                 (lambda () (canonicalize-path joined))
                 (lambda _ (throw 'filetree-error
                                  (format #f "no such path: ~a" rel))))))
    (if (and (string? real)
             (or (string=? real root)
                 (string-prefix? (string-append root "/") real)))
        real
        (throw 'filetree-error
               (format #f "path escapes workspace root: ~a" rel)))))

(define (dir-entries dir)
  "Return DIR's listable entries as (NAME . ABSOLUTE-PATH) pairs
sorted by name, skipping hidden entries and .git."
  (let ((names (or (scandir dir (lambda (name)
                                  (not (hidden-entry? name))))
                   '())))
    (map (lambda (name)
           (cons name (string-append dir "/" name)))
         (sort names string<?))))

;;;
;;; tree: indented project tree.
;;;

(define (tree-entries root max-depth)
  "Walk ROOT collecting (DEPTH NAME DIR?) rows for the tree, listing
entries up to MAX-DEPTH levels below the root. Return (ROWS FILES DIRS
TRUNCATED?) where FILES and DIRS count the listed entries and
TRUNCATED? is #t when the entry cap or the depth limit cut the walk
short."
  (let ((rows '()) (files 0) (dirs 0) (truncated? #f) (count 0))
    (define (visit dir depth)
      (for-each
        (lambda (entry)
          (if (>= count %tree-entry-cap)
              (set! truncated? #t)
              (let* ((name (car entry))
                     (path (cdr entry))
                     (dir? (entry-dir? path)))
                (set! count (+ count 1))
                (if dir?
                    (set! dirs (+ dirs 1))
                    (set! files (+ files 1)))
                (set! rows (cons (list depth name dir?) rows))
                (when dir?
                  (if (< (+ depth 1) max-depth)
                      (visit path (+ depth 1))
                      (when (not (null? (dir-entries path)))
                        (set! truncated? #t)))))))
        (dir-entries dir)))
    (visit root 0)
    (values (reverse rows) files dirs truncated?)))

(define (filetree-tree root args)
  "Handle the tree tool for workspace ROOT: render an indented tree to
ARGS' depth (default 3), capped at %tree-entry-cap entries, with file
and directory counts."
  (guard-tool
    (lambda ()
      (let ((depth (max 0 (arg-number args "depth" 3))))
        (call-with-values (lambda () (tree-entries root depth))
          (lambda (rows files dirs truncated?)
            (let ((lines (map (lambda (row)
                                (format #f "~a~a~a"
                                        (make-string (* 2 (car row)) #\space)
                                        (cadr row)
                                        (if (caddr row) "/" "")))
                              rows)))
              (string-append
                ".\n"
                (string-join lines "\n")
                (if (null? lines) "" "\n")
                (format #f "~a directories, ~a files~a"
                        dirs files
                        (if truncated? " (truncated)" ""))))))))))

;;;
;;; find: case-insensitive name search.
;;;

(define (find-matches root needle)
  "Walk workspace ROOT collecting relative paths of entries whose name
contains NEEDLE (case-insensitive). Return (MATCHES TOTAL) with
MATCHES capped at %find-result-cap."
  (let ((matches '()) (total 0))
    (define (visit dir rel)
      (for-each
        (lambda (entry)
          (let* ((name (car entry))
                 (path (cdr entry))
                 (child-rel (if (string-null? rel)
                                name
                                (string-append rel "/" name))))
            (when (string-contains (string-downcase name) needle)
              (set! total (+ total 1))
              (when (<= total %find-result-cap)
                (set! matches (cons child-rel matches))))
            (when (entry-dir? path)
              (visit path child-rel))))
        (dir-entries dir)))
    (visit root "")
    (values (reverse matches) total)))

(define (filetree-find root args)
  "Handle the find tool for workspace ROOT: case-insensitive substring
search over file names, returning matching relative paths capped at
%find-result-cap with the total match count."
  (guard-tool
    (lambda ()
      (let ((needle (string-downcase (arg-string args "pattern"))))
        (call-with-values (lambda () (find-matches root needle))
          (lambda (matches total)
            (string-append
              (format #f "~a match~a:~%" total (if (= total 1) "" "es"))
              (string-join matches "\n")
              (if (> total (length matches))
                  (format #f "~%(showing first ~a)" (length matches))
                  ""))))))))

;;;
;;; outline: per-file structure.
;;;

(define (read-lines path)
  "Return the lines of PATH as a list of strings without newlines."
  (call-with-input-file path
    (lambda (port)
      (let loop ((acc '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse acc)
              (loop (cons line acc))))))))

(define (first-token s)
  "Return the leading token of S, cut at the first delimiter
character."
  (let loop ((i 0))
    (cond ((>= i (string-length s)) (substring s 0 i))
          ((memv (string-ref s i) '(#\space #\( #\) #\[ #\]))
           (substring s 0 i))
          (else (loop (+ i 1))))))

(define (define-form? line)
  "Return #t when LINE opens a top-level define or define* form."
  (or (string-prefix? "(define " line)
      (string-prefix? "(define(" line)
      (string-prefix? "(define* " line)))

(define (define-name line)
  "Extract the defined name from a define form line: the procedure
name for (define (name args) ...) forms, the bound name otherwise."
  (let* ((rest (substring line (string-length "(define")))
         (rest (if (string-prefix? "*" rest) (substring rest 1) rest))
         (rest (string-trim rest)))
    (first-token (if (string-prefix? "(" rest)
                     (substring rest 1)
                     rest))))

(define (scheme-outline path)
  "Return the outline of Scheme file PATH: one line per top-level
define form with its line number and name."
  (let loop ((rest (read-lines path)) (n 1) (acc '()))
    (if (null? rest)
        (string-join (reverse acc) "\n")
        (let ((line (car rest)))
          (loop (cdr rest)
                (+ n 1)
                (if (define-form? line)
                    (cons (format #f "~a: define ~a" n (define-name line)) acc)
                    acc))))))

(define (plain-outline path)
  "Return the outline of a non-Scheme file PATH: its first line and
line count."
  (let* ((lines (read-lines path))
         (first (if (null? lines) "" (car lines))))
    (format #f "~a line~a~%~a"
            (length lines)
            (if (= (length lines) 1) "" "s")
            first)))

(define (filetree-outline root args)
  "Handle the outline tool for workspace ROOT: the structure of the
file at ARGS' path — top-level define forms for Scheme files, first
line and line count for anything else."
  (guard-tool
    (lambda ()
      (let* ((rel (arg-string args "path"))
             (path (resolve-in-root root rel)))
        (cond ((entry-dir? path)
               (throw 'filetree-error
                      (format #f "outline: path is a directory: ~a" rel)))
              ((string-suffix? ".scm" path) (scheme-outline path))
              (else (plain-outline path)))))))

;;;
;;; read: bounded file contents.
;;;

(define (filetree-read root args)
  "Handle the read tool for workspace ROOT: the contents of the file at
ARGS' path, truncated to ARGS' max-chars (default
%read-max-chars)."
  (guard-tool
    (lambda ()
      (let* ((rel (arg-string args "path"))
             (limit (max 0 (min (arg-number args "max-chars" %read-max-chars)
                                1000000)))
             (path (resolve-in-root root rel)))
        (if (entry-dir? path)
            (throw 'filetree-error
                   (format #f "read: path is a directory: ~a" rel))
            (call-with-input-file path
              (lambda (port)
                (let* ((text (get-string-n port limit))
                       (truncated? (catch #t
                                     (lambda ()
                                       (not (eof-object? (peek-char port))))
                                     (lambda _ #t))))
                  (if truncated?
                      (string-append
                        text
                        (format #f "~%...(truncated at ~a chars)" limit))
                      text)))))))))

;;;
;;; Server assembly.
;;;

(define (filetree-tools)
  "Return the filetree tool definitions for the MCP tools/list
response."
  (list
    `((name . "tree")
      (description
        . "Render the workspace as an indented project tree, skipping hidden entries and .git, capped at 500 entries.")
      (inputSchema
        . ((type . "object")
           (properties
             . ((depth . ((type . "integer")
                          (description . "Maximum depth below the root (default 3)."))))))))
    `((name . "find")
      (description
        . "Case-insensitive substring search over file names, returning matching relative paths (cap 100).")
      (inputSchema
        . ((type . "object")
           (properties
             . ((pattern . ((type . "string")
                            (description . "Substring to look for in file names.")))))
           (required . #("pattern")))))
    `((name . "outline")
      (description
        . "Outline of one file: top-level define forms for Scheme files, first line and line count otherwise.")
      (inputSchema
        . ((type . "object")
           (properties
             . ((path . ((type . "string")
                         (description . "Workspace-relative file path.")))))
           (required . #("path")))))
    `((name . "read")
      (description
        . "Read a file's contents, truncated at max-chars (default 10000).")
      (inputSchema
        . ((type . "object")
           (properties
             . ((path . ((type . "string")
                         (description . "Workspace-relative file path.")))
                (max-chars . ((type . "integer")
                              (description . "Character limit (default 10000).")))))
           (required . #("path")))))))

(define (make-filetree-server root)
  "Build the filetree MCP server definition serving workspace ROOT."
  `(("name" . "filetree")
    ("version" . ,%filetree-version)
    ("tools" . ,(list->vector (filetree-tools)))
    ("handlers"
      . (("tree" . ,(lambda (args) (filetree-tree root args)))
         ("find" . ,(lambda (args) (filetree-find root args)))
         ("outline" . ,(lambda (args) (filetree-outline root args)))
         ("read" . ,(lambda (args) (filetree-read root args)))))))

(define (run-filetree-mcp)
  "Serve the filetree MCP for the current working directory over
stdio."
  (mcp-serve-stdio (make-filetree-server (canonicalize-path (getcwd)))))
