;;; badb — Standalone Filesystem & Shell Tools
;;;
;;; Provides bounded file reading, surgical file editing with pre-mutation verification,
;;; atomic file writing with rollback checkpoints, and deduplicated shell execution.

(define-module (geasa capabilities-fs-tools plugin)
  #:use-module (badb kernel registry)
  #:use-module (geasa runtimes-rollback plugin)
  #:use-module (geasa runtimes-verify plugin)
  #:use-module (geasa foundation-util plugin)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (fslib)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (fs-read-file
            fs-write-file
            fs-edit-file
            register-fs-tools!))

(define %read-file-max-chars-per-line 2000)

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

(define* (fs-read-file path #:optional (offset 0) (limit 800))
  "Read a bounded slice of the file at PATH with 1-based OFFSET and LIMIT."
  (catch #t
    (lambda ()
      (if (file-exists? path)
          (let* ((raw-lines (call-with-input-file path
                              (lambda (p)
                                (let loop ((acc '()))
                                  (let ((l (read-line p)))
                                    (if (eof-object? l)
                                        (reverse acc)
                                        (loop (cons l acc))))))))
                 (total (length raw-lines))
                 (start (max 0 (if (integer? offset) offset (string->number (or offset "0")))))
                 (max-lines (max 1 (if (integer? limit) limit (string->number (or limit "800")))))
                 (end (min total (+ start max-lines))))
            (let loop ((i start) (cur (drop raw-lines (min total start))) (acc '()))
              (cond
               ((or (>= i end) (null? cur))
                (let* ((shown (reverse acc))
                       (last-shown (if (null? shown) start (+ i -1)))
                       (header (format #f "[file ~a - ~a total lines - showing lines ~a..~a]~%"
                                       path total start last-shown))
                       (body (string-join shown "\n"))
                       (footer (if (>= end total)
                                   (format #f "~%[end of file at line ~a of ~a]" total total)
                                   (format #f "~%[~a more lines elided - call read_file with offset=~a to continue (total ~a lines)]"
                                           (- total i) i total)))
                       (tip (if (> total 200)
                                (format #f "~%[Tip: For large files, use symbols_overview or find_symbol to inspect specific definitions, or ctx_search for keyword search.]")
                                "")))
                  (string-append header body footer tip)))
               (else
                (let* ((line (car cur))
                       (trim (if (> (string-length line) %read-file-max-chars-per-line)
                                 (string-append (substring line 0 %read-file-max-chars-per-line)
                                                (format #f "... [line ~a truncated: ~a of ~a chars]"
                                                        i (string-length line) (string-length line)))
                                 line)))
                  (loop (+ i 1) (cdr cur) (cons trim acc)))))))
          (format #f "Error: file '~a' does not exist" path)))
    (lambda (_k . a)
      (format #f "Error reading file '~a': ~s" path a))))

(define* (fs-write-file path content #:key (rollback-mgr #f) (verifier #f))
  "Write CONTENT to PATH, checkpointing with ROLLBACK-MGR and verifying with VERIFIER."
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

(define* (register-fs-tools! reg #:key (rollback-mgr #f) (verifier #f) (shell-executor #f))
  "Register read_file, write_file, edit_file, and single bash tool into REG."
  (registry-attach!
   reg "read_file"
   (lambda (path offset limit)
     (fs-read-file path offset limit))
   '((description . "Read a bounded slice of the file at PATH (line-based offset and limit; header shows total line count; call again with offset to continue)")
     (parameters .
      ((type . "object")
       (properties .
        ((path . ((type . "string") (description . "Path to file to read")))
         (offset . ((type . "integer") (description . "0-based line number to start from (default 0)")))
         (limit . ((type . "integer") (description . "Maximum number of lines to return (default 800)")))))
       (required . #("path"))))))

  (registry-attach!
   reg "write_file"
   (lambda (path content)
     (fs-write-file path content #:rollback-mgr rollback-mgr #:verifier verifier))
   '((description . "Write content to a file at PATH")
     (parameters .
      ((type . "object")
       (properties .
        ((path . ((type . "string") (description . "File path to write")))
         (content . ((type . "string") (description . "Full content to write")))))
       (required . #("path" "content"))))))

  (registry-attach!
   reg "edit_file"
   (lambda (path target replacement replace_all)
     (let ((replace-all? (or (eq? replace_all #t) (equal? replace_all "true"))))
       (fs-edit-file path target replacement #:replace-all? replace-all? #:rollback-mgr rollback-mgr #:verifier verifier)))
   '((description . "Surgically edit PATH by replacing TARGET with REPLACEMENT. Target must match exact text.")
     (parameters .
      ((type . "object")
       (properties .
        ((path . ((type . "string") (description . "Path of file to modify")))
         (target . ((type . "string") (description . "Exact character sequence to be replaced")))
         (replacement . ((type . "string") (description . "Replacement string")))
         (replace_all . ((type . "boolean") (description . "If true, replace all occurrences; if false, fail if target matches multiple times")))))
       (required . #("path" "target" "replacement"))))))

  (when shell-executor
    (registry-attach!
     reg "bash"
     (lambda (command)
       (shell-executor command))
     '((description . "Execute a shell command in bash and return stdout/stderr and exit code")
       (parameters .
        ((type . "object")
         (properties .
          ((command . ((type . "string") (description . "Command to execute")))))
         (required . #("command"))))))))
