;;; badb — Standalone Filesystem & Shell Tools
;;;
;;; Provides bounded file reading, surgical file editing with pre-mutation verification,
;;; atomic file writing with rollback checkpoints, and deduplicated shell execution.

(define-module
  (geasa capabilities-fs-tools plugin) #:use-module
  (badb kernel registry)
  #:use-module
  (geasa runtimes-rollback plugin)
  #:use-module
  (geasa runtimes-verify plugin)
  #:use-module
  (geasa foundation-util plugin)
  #:use-module
  (ice-9 format)
  #:use-module
  (ice-9 rdelim)
  #:use-module
  (ice-9 string-fun)
  #:use-module
  (fslib)
  #:use-module
  (srfi srfi-1)
  #:use-module
  (srfi srfi-13)
  #:use-module
  (rnrs bytevectors)
  #:export
  (fs-read-file fs-write-file
                fs-edit-file
                fs-line-anchor
                fs-file-lines
                fs-edit-lines
                register-fs-tools!))

(define %read-file-max-chars-per-line 2000)

;;; Read-cache: avoid re-dumping file ranges already returned this session.
(define %read-file-cache (make-hash-table 64))

(define (read-range-covered? path start end)
        (let ((ranges (hash-ref %read-file-cache path '())))
          (any (lambda (r) (and (<= (car r) start) (>= (cdr r) end))) ranges)))

(define (read-cache-record! path start end)
        (hash-set! %read-file-cache path
                   (cons (cons start end) (hash-ref %read-file-cache path '()))))

(define (read-cache-invalidate! path)
        (hash-remove! %read-file-cache path))

(define (fs-path-escapes? path)
        "Return #t if PATH attempts directory traversal: it carries `..'
components that escape above its root, or is otherwise a complex
filesystem reference (shell or variable metacharacters), as judged by
guile-fslib's complex-fsing?."
        (or (not (string? path))
            (complex-fsing? path)
            (let loop ((parts (fsing-split path))
                       (depth (if (absolute-fsing? path) 1 0)))
                      (cond
                        ((null? parts) #f)
                        ((or (string-null? (car parts)) (string=? (car parts) "."))
                          (loop (cdr parts) depth))
                        ((string=? (car parts) "..")
                          (or (<= depth 0) (loop (cdr parts) (1- depth))))
                        (else (loop (cdr parts) (1+ depth)))))))

(define*
  (fs-read-file path #:optional (offset 0) (limit 800))
  "Read a bounded slice of the file at PATH with 1-based OFFSET and LIMIT."
  (catch #t
    (lambda ()
      (if (file-exists? path)
        (let*
            ((raw-lines
               (call-with-input-file path
                                     (lambda (p)
                                       (let loop
                                         ((acc '()))
                                         (let ((l (read-line p)))
                                           (if (eof-object? l)
                                             (reverse acc)
                                             (loop (cons l acc))))))))
             (total (length raw-lines))
             (start
               (max 0
                    (if (integer? offset)
                      offset
                      (string->number (or offset "0")))))
             (max-lines
               (max 1
                    (if (integer? limit)
                      limit
                      (string->number (or limit "800")))))
             (end (min total (+ start max-lines))))
          (let loop
            ((i start) (cur (drop raw-lines (min total start))) (acc '()))
            (cond
              ((or (>= i end) (null? cur))
                (let*
                    ((shown (reverse acc))
                     (last-shown
                       (if (null? shown)
                         start
                         (+ i -1)))
                     (header
                       (format #f
                               "[file ~a - ~a total lines - showing lines ~a..~a]~%"
                               path
                               total
                               start
                               last-shown))
                     (body (string-join shown "\n"))
                     (footer
                       (if (>= end total)
                         (format #f "~%[end of file at line ~a of ~a]" total total)
                         (format #f
                                 "~%[~a more lines elided - call read_file with offset=~a to continue (total ~a lines)]"
                                 (- total i)
                                 i
                                 total)))
                     (tip
                       (if (> total 200)
                         (format #f
                                 "~%[Tip: For large files, use symbols_overview or find_symbol to inspect specific definitions, or ctx_search for keyword search.]")
                         "")))
                  (string-append header body footer tip)))
              (else
                (let*
                    ((line (car cur))
                     (trim
                       (if
                           (> (string-length line) %read-file-max-chars-per-line)
                         (string-append
                           (substring line 0 %read-file-max-chars-per-line)
                           (format #f
                                   "... [line ~a truncated: ~a of ~a chars]"
                                   i
                                   (string-length line)
                                   (string-length line)))
                         line))
                     (anchored (format #f "~a:~a→~a" i (fs-line-anchor line) trim)))
                  (loop (+ i 1) (cdr cur) (cons anchored acc)))))))
        (format #f "Error: file '~a' does not exist" path)))
    (lambda (_k . a)
      (format #f "Error reading file '~a': ~s" path a))))

(define* (fs-write-file path content #:key (rollback-mgr #f) (verifier #f))
         "Write CONTENT to PATH, checkpointing with ROLLBACK-MGR and verifying with VERIFIER."
         (read-cache-invalidate! path)
         (if (fs-path-escapes? path)
           (format #f "Error: refusing to write to '~a': path traversal is not permitted" path)
           (catch #t
             (lambda ()
               (when rollback-mgr
                 (create-checkpoint! rollback-mgr (format #f "pre-write: ~a" path) #:files (list path)))
               (let ((do-write!
                       (lambda ()
                         (ensure-parent-directory! path)
                         (call-with-output-file path (lambda (p) (display content p)))
                         (let ((lines (length (string-split content #\newline))))
                           (format #f "Wrote ~a (~a lines)" path lines)))))
                 (if verifier
                   (let ((v-res (verify-action verifier #:code content #:file path)))
                     (if (assoc-ref v-res 'passed?)
                       (do-write!)
                       (begin
                         (when rollback-mgr (rollback-latest! rollback-mgr))
                         (format #f "Write aborted: verification failed: ~s" (assoc-ref v-res 'failures)))))
                   (do-write!))))
             (lambda (_k . a)
               (format #f "Error writing file '~a': ~s" path a)))))

(define (find-all-substring-indices text target)
        "Return sorted 0-based character offsets where TARGET occurs in TEXT."
        (let ((t-len (string-length target)))
          (let loop ((start 0) (acc '()))
                    (let ((idx (string-contains text target start)))
                      (if idx
                        (loop (+ idx (max 1 t-len)) (cons idx acc))
                        (reverse acc))))))

(define (char-offset->line-number text offset)
        "Convert 0-based character OFFSET in TEXT to 1-based line number."
        (let loop ((i 0) (line 1))
                  (cond
                    ((>= i offset) line)
                    ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ line 1)))
                    (else (loop (+ i 1) line)))))

(define* (fs-edit-file path target replacement #:key (replace-all? #f) (rollback-mgr #f) (verifier #f))
         "Surgically replace TARGET with REPLACEMENT in PATH."
         (catch #t
           (lambda ()
             (cond
               ((fs-path-escapes? path)
                 (format #f "Error: refusing to edit '~a': path traversal is not permitted" path))
               ((not (file-exists? path))
                 (format #f "Error: file '~a' not found." path))
               ((or (not target) (string-null? target))
                 (format #f "Error: target text cannot be empty."))
               (else
                 (let* ((old-text (file->string path))
                        (matches (find-all-substring-indices old-text target)))
                   (cond
                     ((null? matches)
                       (format #f "Error: target text not found in '~a'. Re-read the relevant lines with read_file before retrying." path))
                     ((and (> (length matches) 1) (not replace-all?))
                       (let* ((lines (map (lambda (idx) (char-offset->line-number old-text idx)) matches))
                              (lines-str (string-join (map number->string lines) ", ")))
                         (format #f "Error: target text matched ~a locations in '~a' (lines ~a). Include more surrounding lines in 'target' or set replace_all=true."
                                 (length matches) path lines-str)))
                     (else
                       (let* ((replaced-count (if replace-all? (length matches) 1))
                              (new-text
                                (if replace-all?
                                  (string-replace-substring old-text target replacement)
                                  (let* ((idx (car matches))
                                         (prefix (substring old-text 0 idx))
                                         (suffix (substring old-text (+ idx (string-length target)))))
                                    (string-append prefix replacement suffix)))))
                         (let ((do-apply-edit!
                                 (lambda ()
                                   (when rollback-mgr
                                     (create-checkpoint! rollback-mgr (format #f "pre-edit: ~a" path) #:files (list path)))
                                   (call-with-output-file path (lambda (p) (display new-text p)))
                                   (let ((old-lines (length (string-split old-text #\newline)))
                                         (new-lines (length (string-split new-text #\newline))))
                                     (format #f "Edited '~a': replaced ~a occurrence(s) (~a -> ~a lines, file total ~a lines)"
                                             path replaced-count old-lines new-lines new-lines)))))
                           (if verifier
                             (let ((v-res (verify-action verifier #:code new-text #:file path)))
                               (if (assoc-ref v-res 'passed?)
                                 (do-apply-edit!)
                                 (format #f "Edit aborted: verification failed: ~s" (assoc-ref v-res 'failures))))
                             (do-apply-edit!))))))))))
           (lambda (_k . a)
             (format #f "Error editing file '~a': ~s" path a))))

(define (fs-line-anchor line)
        "Return the 3-char lowercase hex content anchor for LINE: FNV-1a over
its trailing-whitespace-trimmed UTF-8 bytes, reduced to 12 bits.  Line
anchors let edits verify the content at a position even when the file
has shifted since it was last read."
        (let*
            ((bv (string->utf8 (string-trim-right line)))
             (hash
               (let loop
                 ((i 0) (h 2166136261))
                 (if (>= i (bytevector-length bv))
                   h
                   (loop (+ i 1)
                         (modulo (* (logxor h (bytevector-u8-ref bv i)) 16777619) 4294967296))))))
          (format #f "~3,'0x" (modulo hash 4096))))

(define (fs-file-lines path)
        "Return the lines of PATH as a list without line terminators."
        (call-with-input-file path
                              (lambda (port)
                                (let loop
                                  ((acc '()))
                                  (let ((l (read-line port)))
                                    (if (eof-object? l)
                                      (reverse acc)
                                      (loop (cons l acc))))))))

(define (%edit-field entry key)
        "Look up KEY (symbol or string) in the edit alist ENTRY."
        (or (and (symbol? key) (assoc-ref entry key))
            (and (string? key) (assoc-ref entry key))
            (and (string? key) (assoc-ref entry (string->symbol key)))
            (and (symbol? key) (assoc-ref entry (symbol->string key)))))

(define (%edit-int entry key)
        "Coerce ENTRY's KEY to an integer, or return #f."
        (let ((v (%edit-field entry key)))
          (cond
            ((integer? v) v)
            ((and (string? v) (string->number v) (integer? (string->number v)))
              (string->number v))
            (else #f))))

(define (%edit-entries edits)
        "Normalize the 'edits' argument (JSON array, list, or single edit
alist) to a list of edit alists; return #f when it is none of those."
        (cond
          ((vector? edits) (vector->list edits))
          ((list? edits) edits)
          ((pair? edits) (list edits))
          (else #f)))

(define
  (%edit-lines-check path parsed lines total)
  "Check PARSED tuples against LINES (in-range endpoints, endpoint
anchors, overlaps); return the first error string or #f when the batch
is safe to apply."
  (let check
    ((rest parsed) (i 1))
    (if (null? rest)
      (let*
          ((sorted
             (sort
               (map
                 (lambda (e)
                   (list (car e) (caddr e)))
                 parsed)
               (lambda (a b)
                 (< (car a) (car b)))))
           (overlap?
             (let oloop
               ((rest sorted))
               (cond
                 ((or (null? rest) (null? (cdr rest))) #f)
                 ((>= (cadr (car rest)) (caadr rest)) #t)
                 (else (oloop (cdr rest)))))))
        (and overlap?
             "Error: edits overlap - combine overlapping ranges into one edit."))
      (let*
          ((e (car rest)) (from (car e))
           (from-hash (cadr e))
           (to (caddr e))
           (to-hash (cadddr e)))
        (cond
          ((or (< from 0) (>= to total) (> from to))
            (format #f
                    "Error: edit ~a range from=~a to=~a is outside lines 0..~a of '~a'."
                    i
                    from
                    to
                    (- total 1)
                    path))
          ((not (string= from-hash (fs-line-anchor (list-ref lines from))))
            (format #f
                    "Error: stale read for '~a': edit ~a anchors line ~a with hash ~a, but that line now reads '~a' (hash ~a). Lines were inserted above the target or the file changed since your last read - re-read the file and retry with fresh anchors."
                    path
                    i
                    from
                    from-hash
                    (list-ref lines from)
                    (fs-line-anchor (list-ref lines from))))
          ((not (string= to-hash (fs-line-anchor (list-ref lines to))))
            (format #f
                    "Error: stale read for '~a': edit ~a anchors line ~a with hash ~a, but that line now reads '~a' (hash ~a). Lines were inserted above the target or the file changed since your last read - re-read the file and retry with fresh anchors."
                    path
                    i
                    to
                    to-hash
                    (list-ref lines to)
                    (fs-line-anchor (list-ref lines to))))
          (else (check (cdr rest) (+ i 1))))))))

(define (%parsed-edits entries)
        "Normalize ENTRIES to (from from-hash to to-hash new-text) tuples; an
invalid entry normalizes to #f."
        (map
          (lambda (e)
            (let
                ((from (%edit-int e "from"))
                 (from-hash (%edit-field e "from_hash"))
                 (to (%edit-int e "to"))
                 (to-hash (%edit-field e "to_hash"))
                 (new-text (%edit-field e "new_text")))
              (and from
                   (string? from-hash)
                   to
                   (string? to-hash)
                   (or (not new-text) (string? new-text))
                   (list from from-hash to to-hash (or new-text "")))))
          entries))

(define (%edit-lines-apply lines parsed)
        "Apply PARSED tuples to LINES bottom-up (highest range first) so line
numbers of earlier ranges remain valid; return the new lines list."
        (let loop
          ((rest
             (sort parsed
                   (lambda (a b)
                     (> (caddr a) (caddr b)))))
           (acc lines))
          (if (null? rest)
            acc
            (let*
                ((e (car rest)) (from (car e))
                 (to (caddr e))
                 (txt (list-ref e 4))
                 (segs
                   (if (or (not txt) (string-null? txt))
                     '()
                     (string-split txt #\newline))))
              (loop (cdr rest) (append (take acc from) segs (drop acc (+ to 1))))))))

(define*
  (fs-edit-lines path edits #:key (rollback-mgr #f) (verifier #f))
  "Apply a batch of hash-verified line-range edits to PATH.  Each edit
is an alist with from/from_hash/to/to_hash/new_text: line numbers are
0-based, the inclusive range [from..to] is replaced by new_text
(multi-line allowed; empty deletes the range), and both endpoint
anchors must match the current content at those lines.  The whole
batch is rejected before any write when an anchor mismatches - the
file shifted or changed since the last read - preventing edits that
would silently land on the wrong lines."
  (catch #t
    (lambda ()
      (let ((entries (%edit-entries edits)))
        (cond
          ((fs-path-escapes? path)
            (format #f "Error: refusing to edit '~a': path traversal is not permitted" path))
          ((not (file-exists? path)) (format #f "Error: file '~a' not found." path))
          ((not entries)
            (format #f
                    "Error: 'edits' must be an array of {from, from_hash, to, to_hash, new_text} objects."))
          ((null? entries) (format #f "Error: provide at least one edit."))
          (else
            (let*
                ((lines (fs-file-lines path))
                 (total (length lines))
                 (parsed (%parsed-edits entries))
                 (invalid
                   (any
                     (lambda (p)
                       (not p))
                     parsed)))
              (cond
                (invalid
                  (format #f
                          "Error: every edit needs integer 'from'/'to' and string 'from_hash'/'to_hash'/'new_text' - got: ~s"
                          entries))
                ((%edit-lines-check path parsed lines total))
                (else
                  (let*
                      ((new-lines (%edit-lines-apply lines parsed))
                       (new-total (length new-lines))
                       (new-text-body (string-join new-lines "\n"))
                       (final-newline? (string-suffix? "\n" (call-with-input-file path read-string)))
                       (new-text
                         (if
                             (and final-newline? (not (string-null? new-text-body)))
                           (string-append new-text-body "\n")
                           new-text-body)))
                    (let
                        ((do-apply!
                           (lambda ()
                             (when rollback-mgr
                               (create-checkpoint! rollback-mgr
                                                   (format #f "pre-edit-lines: ~a" path)
                                                   #:files
                                                   (list path)))
                             (call-with-output-file path
                                                    (lambda (p)
                                                      (display new-text p)))
                             (format #f
                                     "Edited '~a': applied ~a edit(s) (~a -> ~a lines, file total ~a lines)"
                                     path
                                     (length parsed)
                                     total
                                     new-total
                                     new-total))))
                      (if verifier
                        (let
                            ((v-res (verify-action verifier #:code new-text #:file path)))
                          (if (assoc-ref v-res 'passed?)
                            (do-apply!)
                            (format #f "Edit aborted: verification failed: ~s" (assoc-ref v-res 'failures))))
                        (do-apply!)))))))))))
    (lambda (_k . a)
      (format #f "Error editing lines in '~a': ~s" path a))))

(define*
  (register-fs-tools! reg
                      #:key
                      (rollback-mgr #f)
                      (verifier #f)
                      (shell-executor #f))
  "Register read_file, write_file, edit_file, edit_lines, and single bash tool into REG."
  (registry-attach! reg
                    "read_file"
                    (lambda (path offset limit)
                      (let* ((start (max 0 (if (integer? offset) offset (or (string->number (or offset "0")) 0))))
                             (lim (max 1 (if (integer? limit) limit (or (string->number (or limit "800")) 800))))
                             (end (+ start lim)))
                        (if (read-range-covered? path start end)
                          (format #f "[file ~a - lines ~a..~a already read in this session; cached result reused. Use symbols_overview/find_symbol for structure, or a different offset/limit for new lines.]" path start (- end 1))
                          (let ((res (fs-read-file path offset limit)))
                            (read-cache-record! path start end)
                            res))))
                    '((description .
                       "Read a bounded slice of the file at PATH (line-based offset and limit; header shows total line count; call again with offset to continue). Re-reading an already-read range returns a cached note; prefer symbols_overview/find_symbol for structure.")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((path . ((type . "string") (description . "Path to file to read")))
                          (offset .
                           ((type . "integer")
                            (description . "0-based line number to start from (default 0)")))
                          (limit .
                           ((type . "integer")
                            (description . "Maximum number of lines to return (default 800)")))))
                        (required . #("path"))))))
  (registry-attach! reg
                    "write_file"
                    (lambda (path content)
                      (fs-write-file path content #:rollback-mgr rollback-mgr #:verifier verifier))
                    '((description . "Write content to a file at PATH")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((path . ((type . "string") (description . "File path to write")))
                          (content . ((type . "string") (description . "Full content to write")))))
                        (required . #("path" "content"))))))
  (registry-attach! reg
                    "edit_file"
                    (lambda (path target replacement replace_all)
                      (let
                          ((replace-all? (or (eq? replace_all #t) (equal? replace_all "true"))))
                        (fs-edit-file path
                                      target
                                      replacement
                                      #:replace-all?
                                      replace-all?
                                      #:rollback-mgr
                                      rollback-mgr
                                      #:verifier
                                      verifier)))
                    '((description .
                       "Surgically edit PATH by replacing TARGET with REPLACEMENT. Target must match exact text.")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((path . ((type . "string") (description . "Path of file to modify")))
                          (target .
                           ((type . "string") (description . "Exact character sequence to be replaced")))
                          (replacement . ((type . "string") (description . "Replacement string")))
                          (replace_all .
                           ((type . "boolean")
                            (description .
                             "If true, replace all occurrences; if false, fail if target matches multiple times")))))
                        (required . #("path" "target" "replacement"))))))
  (registry-attach! reg
                    "edit_lines"
                    (lambda (path edits)
                      (fs-edit-lines path edits #:rollback-mgr rollback-mgr #:verifier verifier))
                    '((description .
                       "Apply a batch of hash-verified line-range edits (atomic: all-or-nothing). read_file shows every line as N:HHH→content; pass those anchors as from/from_hash/to/to_hash so edits based on stale reads are rejected instead of corrupting the file.")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((path . ((type . "string") (description . "Path of file to modify")))
                          (edits .
                           ((type . "array")
                            (description . "Edits to apply atomically")
                            (items .
                             ((type . "object")
                              (properties .
                               ((from .
                                 ((type . "integer") (description . "0-based first line of the replaced range")))
                                (from_hash .
                                 ((type . "string") (description . "Anchor of line 'from' shown by read_file")))
                                (to .
                                 ((type . "integer")
                                  (description . "0-based last line of the replaced range (inclusive)")))
                                (to_hash .
                                 ((type . "string") (description . "Anchor of line 'to' shown by read_file")))
                                (new_text .
                                 ((type . "string")
                                  (description .
                                   "Replacement text for the range (may span lines; empty string deletes the range)")))))
                              (required . #("from" "from_hash" "to" "to_hash" "new_text"))))))))
                        (required . #("path" "edits"))))))
  (when shell-executor
    (registry-attach! reg
                      "bash"
                      (lambda (command)
                        (shell-executor command))
                      '((description .
                         "Execute a shell command in bash and return stdout/stderr and exit code. Do not use for reading files—use read_file/symbols_overview/find_symbol instead.")
                        (parameters .
                         ((type . "object")
                          (properties .
                           ((command . ((type . "string") (description . "Command to execute")))))
                          (required . #("command"))))))))