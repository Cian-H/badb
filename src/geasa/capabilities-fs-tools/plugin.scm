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
  (geasa capabilities-context-index plugin)
  #:use-module
  (badb kernel digest)
  #:use-module
  (ice-9 format)
  #:use-module
  (ice-9 rdelim)
  #:use-module
  (ice-9 regex)
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
                fs-file-search
                fs-edit-lines
                register-fs-tools!))

(define %read-file-max-chars-per-line 2000)

;;; Read-cache: avoid re-dumping file ranges already returned this session.
;;; Each entry pairs a (start . end) range with a content hash of the file
;;; at read time, so any change to the file (by us or anything else)
;;; invalidates the cached ranges automatically.
(define %read-file-cache (make-hash-table 64))

(define (read-range-covered? path start end current-hash)
        (let ((entries (hash-ref %read-file-cache path '())))
          (any (lambda (e)
                 (let ((r (car e)) (h (cdr e)))
                   (and (string=? h current-hash)
                        (<= (car r) start)
                        (>= (cdr r) end))))
               entries)))

(define (read-cache-record! path start end current-hash)
        (hash-set! %read-file-cache path
                   (cons (cons (cons start end) current-hash)
                         (hash-ref %read-file-cache path '()))))

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

(define (%coerce-int-or-false v)
        "Coerce V (#f, exact integer, or numeric string) to an integer; return
#f when V is absent, non-numeric, or fractional."
        (cond
          ((not v) #f)
          ((integer? v) v)
          ((and (string? v) (string->number v) (integer? (string->number v)))
            (string->number v))
          (else #f)))

(define*
  (fs-read-file path
                #:optional
                (offset 0)
                (limit 800)
                #:key
                (start-line #f)
                (end-line #f)
                (force #f)
                (ctx-idx #f))
  "Read a bounded slice of the file at PATH.  OFFSET and LIMIT form the
0-based line window that matches the N:HHH→anchors shown in the output,
so a follow-up edit_lines call can copy the displayed line numbers and
hashes verbatim.  Alternatively pass 1-based START-LINE and END-LINE
(inclusive) exactly as compilers and stack traces report them - they
take precedence over OFFSET and LIMIT and map 1:1 onto the numbers you
were given, without the -1 arithmetic that makes sed -n tempting.
FORCE bypasses the per-session read cache to obtain fresh anchors for a
range already read (still bounded by LIMIT / max chars).  CTX-IDX, when
supplied, is the shared BM25 context index used to make cached ranges
retrievable via ctx_search under source_id \"read_file:<path>\"."
  (let
      ((sline (%coerce-int-or-false start-line))
       (eline (%coerce-int-or-false end-line)))
    (if (and sline eline (< eline sline))
      (format #f "Error: end_line ~a is before start_line ~a" eline sline)
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
                        (or (and sline (- sline 1))
                            (if (integer? offset)
                              offset
                              (string->number (or offset "0"))))))
                 (max-lines
                   (max 1
                        (or (and sline eline (- eline start))
                            (and (not sline) eline (- eline start))
                            (if (integer? limit)
                              limit
                              (string->number (or limit "800"))))))
                 (end (min total (+ start max-lines)))
                 (file-hash (sha256-hex (string-join raw-lines "\n")))
                 (covered? (read-range-covered? path start end file-hash)))
              (if (and covered? (not force))
                (let ((idx-hint
                        (if ctx-idx
                          (string-append
                            (format #f "Indexed under source_id \"read_file:~a\" (accumulates all read slices of this file).~%" path)
                            (format #f "To pull specific lines without re-dumping: ctx_search(query, source_id=\"read_file:~a\").~%" path))
                          (format #f "Not indexed (no context index attached).~%"))))
                  (format #f
                          "[file ~a - lines ~a..~a already read in this session (file unchanged since that read); cached result reused.~%~aYOU ALREADY HAVE THIS CONTENT in your context from the earlier read. Do NOT re-read it.~%• Recall specific lines cheaply: ctx_search(query, source_id=\"read_file:~a\").~%• If you need the EXACT text/anchors of these lines again (e.g. after context compaction), call read_file(path, offset=~a, limit=~a, force=true) — the file is unchanged so the result is identical, but it is returned fresh.~%• Do NOT use bash/sed to re-read.]"
                          path
                          start
                          (- end 1)
                          idx-hint
                          path
                          start
                          (max 1 (- end start))))
                (begin
                  (read-cache-record! path start end file-hash)
                  (let ((result
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
                                  (loop (+ i 1) (cdr cur) (cons anchored acc))))))))
                    (when (and ctx-idx (not covered?))
                      (ctx-index-content! ctx-idx (string-append "read_file:" path) result #f))
                    result))))
            (format #f "Error: file '~a' does not exist" path)))
        (lambda (_k . a)
          (format #f "Error reading file '~a': ~s" path a))))))

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
                                   (read-cache-invalidate! path)
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
                             (read-cache-invalidate! path)
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

(define (%file-binary? path)
        "Return #t when PATH begins with a NUL byte in its first 8000
characters (best-effort binary detection for search skipping)."
        (catch #t
          (lambda ()
            (call-with-input-file path
                                  (lambda (p)
                                    (let ((chunk (read-string 8000 p)))
                                      (and (string? chunk) (string-index chunk #\nul))))))
          (lambda _
            #f)))

(define (%glob->regexp glob)
        "Convert GLOB (e.g. \"*.scm\") into an anchored regexp matching whole basenames."
        (let loop
          ((i 0) (acc '()))
          (if (>= i (string-length glob))
            (make-regexp (string-append "^" (string-concatenate (reverse acc)) "$"))
            (let ((c (string-ref glob i)))
              (cond
                ((char=? c #\*) (loop (+ i 1) (cons "[^/]*" acc)))
                ((char=? c #\?) (loop (+ i 1) (cons "." acc)))
                ((char=? c #\.) (loop (+ i 1) (cons "\\." acc)))
                (else (loop (+ i 1) (cons (make-string 1 c) acc))))))))

(define %file-search-skip-dirs
        '(".git" "node_modules" ".badb" "dist" "build" ".cache" "target" ".guix-profile"))

(define (%walk-search-files dir)
        "Return all regular file paths under DIR in sorted order, skipping
ignore-listed and dot directories, dot-log files cannot sneak in."
        (let loop
          ((items (sort (%dir-entry-names dir) string<?)) (acc '()))
          (cond
            ((null? items) (reverse acc))
            ((or (string=? (car items) ".")
                 (string=? (car items) "..")
                 (member (car items) %file-search-skip-dirs)
                 (string-prefix? "." (car items)))
              (loop (cdr items) acc))
            (else
              (let ((full (string-append dir "/" (car items))))
                (cond
                  ((and (file-exists? full) (file-is-directory? full))
                    (loop (cdr items) (append acc (%walk-search-files full))))
                  ((string-suffix? ".log" (car items)) (loop (cdr items) acc))
                  (else (loop (cdr items) (cons full acc)))))))))

(define (%dir-entry-names dir)
        "Return the entry names of DIR in sorted order, or the empty list when
DIR cannot be read."
        (catch 'system-error
          (lambda ()
            (let ((stream (opendir dir)))
              (let loop
                ((accum '()))
                (let ((entry (readdir stream)))
                  (if (eof-object? entry)
                    (begin (closedir stream) (reverse accum))
                    (loop (cons entry accum)))))))
          (lambda _
            '())))

(define*
  (fs-file-search pattern
                  #:key
                  (path ".")
                  (glob #f)
                  (context 0)
                  (output-mode "content")
                  (max-results 200))
  "Search files under PATH (directory or single file) for the POSIX
regexp PATTERN.  OUTPUT-MODE selects the report shape: 'content'
renders each match together with surrounding CONTEXT lines as
N:HHH→text using read_file's 0-based line numbers and content anchors,
so a hit can be routed straight into an edit_lines call without a
re-read; 'files_with_matches' lists only matching paths; 'count' lists
path and match count.  GLOB restricts searched file basenames (e.g.
\"*.scm\").  Dot and ignore-listed directories, dot-log files, and
binary files are skipped.  This is the structured replacement for raw
bash grep -r/find: bounded, ignore-aware output instead of unfiltered
stdout."
  (if (string? pattern)
    (let*
        ((rx
           (catch #t
             (lambda ()
               (make-regexp pattern))
             (lambda _
               #f)))
         (root
           (if
               (and (string? path) (not (string-null? (string-trim path))))
             path
             ".")))
      (cond
        ((not rx) (format #f "Error: invalid regular expression: ~a" pattern))
        ((not (file-exists? root)) (format #f "Error: path '~a' does not exist" root))
        (else
          (let*
              ((glob-rx
                 (and glob
                      (string? glob)
                      (not (string-null? (string-trim glob)))
                      (%glob->regexp glob)))
               (ctxt (max 0 (or (%coerce-int-or-false context) 0)))
               (maxr (max 1 (or (%coerce-int-or-false max-results) 200)))
               (mode
                 (let
                     ((m (string-downcase (ensure-string output-mode))))
                   (cond
                     ((string-contains m "count") 'count)
                     ((string-contains m "files") 'files)
                     (else 'content))))
               (files (%fs-search-filelist root glob-rx))
               (%search
                 (lambda (f)
                   (let
                       ((lines
                          (catch #t
                            (lambda ()
                              (fs-file-lines f))
                            (lambda _
                              #f))))
                     (and lines (cons (%fs-search-indexes rx lines) lines))))))
            (let loop
              ((rest files) (hits '()) (blocks '()) (truncated? #f))
              (if (or (null? rest) truncated?)
                (let*
                    ((n-matches (fold + 0 (map cdr hits)))
                     (hit-list (reverse hits))
                     (header
                       (format #f
                               "[file_search pattern=~a path=~a - ~a match(es) in ~a file(s) of ~a searched~a]"
                               pattern
                               root
                               n-matches
                               (length hits)
                               (length files)
                               (if truncated?
                                 " - RESULT LIMIT REACHED"
                                 "")))
                     (body
                       (cond
                         ((eq? mode 'count)
                           (string-join
                             (map
                               (lambda (h)
                                 (format #f "~a:~a" (car h) (cdr h)))
                               hit-list)
                             "\n"))
                         ((eq? mode 'files) (string-join (map car hit-list) "\n"))
                         (else (string-join (reverse blocks) "\n"))))
                     (footer
                       (cond
                         (truncated?
                           (format #f
                                   "~%[result limit ~a reached - narrow with glob, switch to output_mode=files_with_matches or count, or raise max_results]"
                                   maxr))
                         ((and (eq? mode 'content) (> n-matches 0))
                           "\n[Tip: the N:HHH→anchors above are read_file's 0-based line numbers and content hashes - pass them directly to edit_lines from/from_hash/to/to_hash.]")
                         (else ""))))
                  (string-append (string-append header "\n" body) footer))
                (let ((r (%search (car rest))))
                  (cond
                    ((or (not r) (null? (car r))) (loop (cdr rest) hits blocks truncated?))
                    (else
                      (let*
                          ((f (car rest)) (idxs (car r))
                           (lines (cdr r))
                           (hits* (cons (cons f (length idxs)) hits))
                           (rendered
                             (if (eq? mode 'content)
                               (%fs-search-blocks f lines idxs ctxt)
                               '())))
                        (loop (cdr rest) hits* (append blocks rendered)
                              (and (eq? mode 'content) (>= (+ (length blocks) (length rendered)) maxr)))))))))))))
    (format #f "Error: 'pattern' must be a search string")))

(define (%fs-search-indexes rx lines)
        "Return the 0-based indexes of LINES matching RX."
        (let lp
          ((i 0) (ls lines) (acc '()))
          (cond
            ((null? ls) (reverse acc))
            ((regexp-exec rx (car ls)) (lp (+ i 1) (cdr ls) (cons i acc)))
            (else (lp (+ i 1) (cdr ls) acc)))))

(define (%fs-search-blocks f lines indexes ctxt)
        "Render content blocks for match INDEXES in LINES of file F, each
surrounded by CTXT context lines, as N:HHH→text using read_file's
0-based line numbering and content anchors."
        (let
            ((render-range
               (lambda (lo hi)
                 (let rb
                   ((idx lo) (stop hi) (acc '()))
                   (cond
                     ((> idx stop) (reverse acc))
                     (else
                       (rb (+ idx 1)
                           stop
                           (cons
                             (format #f
                                     "~a:~a:~a→~a"
                                     f
                                     idx
                                     (fs-line-anchor (list-ref lines idx))
                                     (list-ref lines idx))
                             acc))))))))
          (map
            (lambda (r)
              (string-join (render-range (car r) (cadr r)) "\n"))
            (%merge-ranges
              (map
                (lambda (i)
                  (list (max 0 (- i ctxt)) (min (- (length lines) 1) (+ i ctxt))))
                indexes)))))

(define (%fs-search-filelist root glob-rx)
        "Return the files to search under ROOT: ROOT itself when it is a
file, else every walkable file below it, filtered by GLOB-RX basenames
and binary detection."
        (let
            ((all
               (if (file-is-directory? root)
                 (%walk-search-files root)
                 (list root))))
          (filter
            (lambda (f)
              (and
                (or (not glob-rx) (and (regexp-exec glob-rx (basename f)) #t))
                (not (%file-binary? f))))
            all)))

(define (%merge-ranges rs)
        "Merge overlapping or adjacent (START END) line ranges into a
descending... ascending-ordered list of disjoint ranges."
        (reverse
          (fold
            (lambda (r acc)
              (if
                  (and (pair? acc) (<= (car r) (+ 1 (cadr (car acc)))))
                (cons (list (car (car acc)) (max (cadr (car acc)) (cadr r))) (cdr acc))
                (cons r acc)))
            '()
            (sort rs
                  (lambda (a b)
                    (< (car a) (car b)))))))

(define*
  (register-fs-tools! reg
                      #:key
                      (rollback-mgr #f)
                      (verifier #f)
                      (shell-executor #f)
                      (ctx-idx #f))
  "Register read_file, write_file, edit_file, edit_lines, file_search, and
a single bash tool into REG."
  (registry-attach! reg
                    "read_file"
                    (lambda (path offset limit start_line end_line force)
                      (fs-read-file path offset limit #:start-line start_line #:end-line end_line #:force force #:ctx-idx ctx-idx))
                    '((description .
                       "Read a bounded slice of the file at PATH. Every line comes back as N:HHH→content with N the 0-based line number and HHH the line's content anchor, and offset/limit use the same 0-based numbering, so edit_lines from/from_hash/to/to_hash can copy them directly. For the 1-based line numbers compilers and stack traces give you, pass start_line/end_line (inclusive) instead of offset/limit - no offset arithmetic, no sed -n. A cached note means you ALREADY received those lines earlier in this session (file unchanged since); do NOT re-read them - recall via ctx_search(source_id) or use force=true only after editing the file.")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((path . ((type . "string") (description . "Path to file to read")))
                          (offset .
                           ((type . "integer")
                            (description .
                             "0-based first line to show, as printed by the header/footer (default 0)")))
                          (limit .
                           ((type . "integer")
                            (description . "Maximum number of lines to return (default 800)")))
                          (start_line .
                           ((type . "integer")
                            (description .
                             "1-based first line to show, exactly as compilers/stack traces report it (overrides offset)")))
                          (end_line .
                           ((type . "integer")
                            (description .
                             "1-based last line to show, inclusive (trims limit; typically paired with start_line)")))
                          (force .
                           ((type . "boolean")
                            (description .
                             "Bypass the per-session read cache and return the file content fresh, even for a range already read. The file is unchanged so the result is identical, but it is re-emitted (useful after context compaction or when you need exact N:HHH anchors for edit_lines). Still bounded by limit/max chars. Prefer ctx_search(source_id) when you only need specific lines.")))))
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
  (registry-attach! reg
                    "file_search"
                    (lambda
                        (pattern path glob context output_mode max_results)
                      (fs-file-search pattern
                                      #:path
                                      (or path ".")
                                      #:glob
                                      glob
                                      #:context
                                      (or context 0)
                                      #:output-mode
                                      (or output_mode "content")
                                      #:max-results
                                      (or max_results 200)))
                    '((description .
                       "Bounded workspace search for the POSIX regex PATTERN under PATH: the structured replacement for bash grep -r/find, respecting ignore dirs, skipping binaries, and offering CONTEXT lines, a GLOB basename filter, and OUTPUT_MODE content|files_with_matches|count so output stays small. Content lines use read_file's N:HHH→anchors (0-based), so edit_lines can consume them without a re-read.")
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((pattern .
                           ((type . "string") (description . "POSIX regular expression to search for")))
                          (path .
                           ((type . "string")
                            (description . "File or directory to search (default \".\")")))
                          (glob .
                           ((type . "string") (description . "Glob filter for file basenames, e.g. *.scm")))
                          (context .
                           ((type . "integer")
                            (description . "Lines of context around each content match (default 0)")))
                          (output_mode .
                           ((type . "string")
                            (description . "content | files_with_matches | count (default content)")))
                          (max_results .
                           ((type . "integer")
                            (description . "Maximum content chunks to return (default 200)")))))
                        (required . #("pattern"))))))
  (when shell-executor
    (registry-attach! reg
                      "bash"
                      (lambda (command)
                        (shell-executor command))
                      '((description .
                         "Execute a shell command for builds, tests, git, and other process pipelines. Do not use it for reading or searching files - use read_file (0-based offset/limit or 1-based start_line/end_line), file_search, or symbols_overview/find_symbol instead. grep, sed, awk and find belong inside stdout pipes here (cmd | grep ...); running them directly against workspace files is guardrail-blocked because read_file, edit_lines/edit_file (hash-verified, indentation-safe), and file_search do the same work with verification sed and awk cannot give you.")
                        (parameters .
                         ((type . "object")
                          (properties .
                           ((command .
                             ((type . "string")
                              (description .
                               "The shell command to run. Do not read files with cat/sed -n or edit them with sed -i/awk here: direct file reads/edits bypass hash verification and indentation safety and are guardrail-blocked - use read_file (start_line/end_line), edit_lines or edit_file instead, and append '# allow-unix-edit' to this command only for deliberate bulk transformations (e.g. one symbol across many files). Piped filtering of command stdout with grep/sed/awk is always fine.")))))
                          (required . #("command"))))))))