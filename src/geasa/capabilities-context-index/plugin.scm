;;; (geasa capabilities-context-index plugin) — Context Virtualization & Indexing
;;;
;;; Implements context-mode style output virtualization, pure-Guile BM25 token
;;; search over oversized tool outputs and build logs, and dynamic offloading.

(define-module
  (geasa capabilities-context-index plugin)
  #:use-module
  (badb kernel registry)
  #:use-module
  (geasa capabilities-context-index bm25)
  #:use-module
  (geasa foundation-util plugin)
  #:use-module
  (ice-9 format)
  #:use-module
  (srfi srfi-1)
  #:use-module
  (srfi srfi-13)
  #:export
  (ctx-index-file! ctx-index-content!
                   ctx-search
                   ctx-search-batch
                   ctx-exec!
                   ctx-execute-file!
                   ctx-batch-exec!
                   make-ctx-offload-hook
                   %context-mode-threshold-bytes
                   ctx-bash-hook-enabled?
                   set-ctx-bash-hook-enabled!
                   make-ctx-bash-post-hook
                   format-ctx-stats
                   register-context-index-tools!
                   %ctx-exec-schema
                   %ctx-execute-file-schema
                   %ctx-search-schema
                   %ctx-index-schema
                   %ctx-batch-exec-schema
                   %ctx-stats-schema)
  #:re-export
  (make-bm25-index load-bm25-index
                   save-bm25-index!
                   bm25-purge-source!
                   chunk-code-lines))

;;;
;;; Default Shell Execution
;;;
(define default-shell-exec exec-shell-command)

;;;
;;; File and Content Indexing
;;;
(define (ctx-index-file! idx path)
        "Index a file from PATH into IDX. Returns the number of chunks added."
        (if (file-exists? path)
          (let* ((text (file->string path))
                 (is-md? (string-suffix? ".md" path)))
            (ctx-index-content! idx path text is-md?))
          0))

(define*
  (ctx-index-content! idx source-id text #:optional (is-markdown? #f))
  "Index TEXT under SOURCE-ID into IDX. Purges prior chunks for SOURCE-ID first.
Returns the number of chunks added."
  (bm25-purge-source! idx source-id)
  (let*
      ((raw-len (string-length text))
       (ctype
         (if is-markdown?
           "prose"
           "code"))
       (chunks
         (if is-markdown?
           (chunk-markdown source-id text)
           (chunk-code-lines text))))
    (set-index-raw-bytes! idx (+ (index-raw-bytes idx) raw-len))
    (for-each
      (lambda (c)
        (let
            ((sec (car c)) (sl (cadr c)) (el (caddr c)) (txt (cadddr c)))
          (bm25-add-chunk! idx source-id sec sl el txt ctype)))
      chunks)
    (length chunks)))

;;;
;;; Context Search
;;;
(define*
  (ctx-search idx
              query
              #:key
              (source-id #f)
              (top-k 5)
              (max-chars 2400)
              (content-type #f)
              (sort 'relevance))
  "Search the FTS5/BM25 index for QUERY and return a formatted context slice."
  (let*
      ((matches
         (bm25-search idx
                      query
                      #:source-id
                      source-id
                      #:top-k
                      top-k
                      #:content-type
                      content-type
                      #:sort
                      sort)))
    (if (null? matches)
      (format #f
              "No relevant context matches found for query: ~s~a"
              query
              (if source-id
                (format #f " in source ~s" source-id)
                ""))
      (let loop
        ((rem matches) (acc '()) (total-len 0))
        (if (null? rem)
          (let
              ((result (string-join (reverse acc) "\n---\n")))
            (set-index-returned-bytes! idx
                                       (+ (index-returned-bytes idx) (string-length result)))
            result)
          (let*
              ((match (car rem)) (score (car match))
               (c (cdr match))
               (sec-part
                 (if (chunk-section c)
                   (format #f " sec=~s" (chunk-section c))
                   ""))
               (entry
                 (format #f
                         "[~a:L~a-L~a score=~,2f~a]\n~a"
                         (chunk-source-id c)
                         (chunk-start-line c)
                         (chunk-end-line c)
                         score
                         sec-part
                         (chunk-text c)))
               (entry-len (string-length entry)))
            (if
                (and (> total-len 0) (> (+ total-len entry-len) max-chars))
              (let
                  ((result
                     (string-append
                       (string-join (reverse acc) "\n---\n")
                       "\n... [Remaining results truncated for token budget]")))
                (set-index-returned-bytes! idx
                                           (+ (index-returned-bytes idx) (string-length result)))
                result)
              (loop (cdr rem) (cons entry acc) (+ total-len entry-len)))))))))

(define*
  (ctx-search-batch idx
                    queries
                    #:key
                    (source-id #f)
                    (top-k 3)
                    (max-chars 2400)
                    (content-type #f)
                    (sort 'relevance))
  "Run multiple search QUERIES in one call and combine results."
  (let*
      ((per-query-budget (max 400 (quotient max-chars (max 1 (length queries)))))
       (parts
         (map
           (lambda (q)
             (format #f
                     "=== Query: ~a ===\n~a"
                     q
                     (ctx-search idx
                                 q
                                 #:source-id
                                 source-id
                                 #:top-k
                                 top-k
                                 #:max-chars
                                 per-query-budget
                                 #:content-type
                                 content-type
                                 #:sort
                                 sort)))
           queries)))
    (string-join parts "\n\n")))

;;;
;;; Command Execution & Virtualization
;;;
(define %error-pattern
        (make-regexp "(^[[:space:]]*(ERROR|FAIL|FAILED|FATAL|CRITICAL|PANIC|TRACEBACK|EXCEPTION|ASSERTION FAILED|DENIED)(:|[[:space:]])|:[[:space:]]*(error|failure|failed|fatal|panic|exception|assertion failed|denied)(:|[[:space:]])|^[[:space:]]*Traceback([[:space:]]|:))" regexp/icase))

(define*
  (ctx-exec! idx
             command
             #:key
             (shell-executor #f)
             (intent-query #f)
             (max-return-chars 1600)
             (offload-dir ".badb/ctx"))
  "Execute COMMAND with virtualized output management. Automatically offloads
oversized outputs and indexes them into BM25."
  (let
      ((exec-fn (or shell-executor default-shell-exec)))
    (call-with-values
      (lambda ()
        (exec-fn command))
      (lambda (out . rest)
        (let*
            ((exit-code
               (if (pair? rest)
                 (car rest)
                 0))
             (err
               (if (and (pair? rest) (pair? (cdr rest)))
                 (cadr rest)
                 #f))
             (clean-out (or out "")))
          (if err
            (format #f "[Command execution failed with exit=~a]: ~a" exit-code err)
            (let*
                ((doc-n (index-next-doc-id idx))
                 (doc-id (format #f "exec:~a" doc-n))
                 (raw-bytes (string-length clean-out))
                 (lines (string-split clean-out #\newline))
                 (line-count (length lines)))
              (set-index-next-doc-id! idx (+ doc-n 1))
              ;; Always index into BM25
              (ctx-index-content! idx doc-id clean-out)
              (if
                  (and (<= raw-bytes max-return-chars) (not intent-query))
                ;; Output is small enough to return directly
                (begin
                  (set-index-returned-bytes! idx (+ (index-returned-bytes idx) raw-bytes))
                  clean-out)
                ;; Output is large or intent-query was specified: virtualize!
                (let*
                    ((header
                       (format #f
                               "[~a exit=~a lines=~a bytes=~a indexed=~a]\n"
                               doc-id
                               exit-code
                               line-count
                               raw-bytes
                               doc-id))
                     (summary-body
                       (if intent-query
                         ;; Extract intent query chunks
                         (ctx-search idx
                                     intent-query
                                     #:source-id
                                     doc-id
                                     #:top-k
                                     3
                                     #:max-chars
                                     (- max-return-chars 300))
                         ;; Extract failure lines or window
                         (let*
                             ((tagged-lines
                                (let loop
                                  ((i 1) (rem lines) (acc '()))
                                  (if (null? rem)
                                    (reverse acc)
                                    (let ((l (car rem)))
                                      (if (regexp-exec %error-pattern l)
                                        (loop (+ i 1) (cdr rem) (cons (cons i l) acc))
                                        (loop (+ i 1) (cdr rem) acc))))))
                              (error-count (length tagged-lines)))
                           (cond
                             ((> error-count 0)
                               (let*
                                   ((top-errors (take-n tagged-lines 15))
                                    (formatted-errors
                                      (map
                                        (lambda (pair)
                                          (format #f "  L~a: ~a" (car pair) (cdr pair)))
                                        top-errors))
                                    (tail-lines (take-n (reverse lines) 5))
                                    (tail-str (string-join (reverse tail-lines) "\n")))
                                 (format #f
                                         "Identified ~a diagnostic error/failure lines:\n~a\n\nTail summary:\n~a"
                                         error-count
                                         (string-join formatted-errors "\n")
                                         tail-str)))
                             (else
                               (let*
                                   ((head-lines (take-n lines 8))
                                    (tail-lines (take-n (reverse lines) 5))
                                    (head-str (string-join head-lines "\n"))
                                    (tail-str (string-join (reverse tail-lines) "\n")))
                                 (format #f
                                         "~a\n... [~a lines virtualized into ~a] ...\n~a"
                                         head-str
                                         (- line-count 13)
                                         doc-id
                                         tail-str)))))))
                     (footer
                       (format #f
                               "\n[Output virtualized to save tokens. Use ctx_search(query: \"...\", source_id: ~s) for details]"
                               doc-id))
                     (full-response (string-append header summary-body footer)))
                  (set-index-returned-bytes! idx
                                             (+ (index-returned-bytes idx) (string-length full-response)))
                  full-response)))))))))

(define (shell-single-quote str)
        "Quote STR safely for POSIX sh using single quotes."
        (string-append "'" (string-join (string-split (or str "") #\') "'\"'\"'") "'"))

(define*
  (ctx-execute-file! idx
                     path
                     language
                     code
                     #:key
                     (shell-executor #f)
                     (intent-query #f)
                     (max-return-chars 1600))
  "Execute CODE in LANGUAGE with FILE_CONTENT set to the contents of PATH."
  (if (file-exists? path)
    (let*
        ((cmd
           (string-append "FILE_CONTENT=$(cat "
                          (shell-single-quote path)
                          "); export FILE_CONTENT; "
                          (build-language-exec-command language code))))
      (ctx-exec! idx
                 cmd
                 #:shell-executor
                 shell-executor
                 #:intent-query
                 intent-query
                 #:max-return-chars
                 max-return-chars))
    (format #f "Error: file not found: ~a" path)))

(define
  (build-language-exec-command language code)
  "Build a POSIX shell command that executes CODE in LANGUAGE ('shell', 'python', 'javascript', 'scheme')."
  (let ((qcode (shell-single-quote code)))
    (cond
      ((or (string=? language "python") (string=? language "python3"))
        (format #f "python3 -c ~a" qcode))
      ((or (string=? language "javascript")
           (string=? language "node")
           (string=? language "typescript"))
        (format #f "node -e ~a" qcode))
      ((or (string=? language "scheme") (string=? language "guile"))
        (format #f "guile -c ~a" qcode))
      (else code))))

;;;
;;; Batch Execution
;;;
(define*
  (ctx-batch-exec! idx
                   #:key
                   (commands '())
                   (queries '())
                   (shell-executor #f)
                   (max-return-chars 2800))
  "Execute a batch of commands (strings or (label/command) alists) and queries in a single turn."
  (let*
      ((cmd-results
         (map
           (lambda (cmd-spec)
             (let*
                 ((label
                    (and (pair? cmd-spec)
                         (or (assoc-ref cmd-spec 'label) (assoc-ref cmd-spec "label"))))
                  (cmd
                    (if (string? cmd-spec)
                      cmd-spec
                      (or (assoc-ref cmd-spec 'command) (assoc-ref cmd-spec "command") "")))
                  (res (ctx-exec! idx cmd #:shell-executor shell-executor #:max-return-chars 800)))
               (if label
                 (format #f "=== ~a ===\n~a" label res)
                 (format #f "$ ~a\n~a" cmd res))))
           commands))
       (query-results
         (map
           (lambda (q)
             (let
                 ((res (ctx-search idx q #:top-k 3 #:max-chars 800)))
               (format #f "? ~a\n~a" q res)))
           queries))
       (combined
         (string-append
           (if (pair? cmd-results)
             (string-append "=== Command Executions ===\n"
                            (string-join cmd-results "\n\n")
                            "\n\n")
             "")
           (if (pair? query-results)
             (string-append "=== Query Searches ===\n" (string-join query-results "\n\n"))
             ""))))
    (if (> (string-length combined) max-return-chars)
      (string-append
        (substring combined 0 max-return-chars)
        "\n... [Batch results truncated]")
      combined)))

;;;
;;; Offload Hook
;;;
(define* (make-ctx-offload-hook idx #:key (offload-dir ".badb/ctx"))
         "Return a hook procedure (lambda (filename tc-name content-str) ...) that
indexes oversized tool outputs and returns a compact virtualized pointer."
         (lambda (filename tc-name content-str)
           (let* ((doc-id (or filename tc-name "offload"))
                  (bytes (string-length content-str)))
             (ctx-index-content! idx doc-id content-str)
             (format #f "[Output virtualized to ~a (~a bytes). Query via ctx_search(query: \"...\", source_id: ~s)]"
                     doc-id bytes doc-id))))

(define %context-mode-threshold-bytes 5120)

(define %ctx-bash-hook-enabled #t)

(define (ctx-bash-hook-enabled?)
        "Return #t when the context-mode bash post-execution hook is enabled."
        %ctx-bash-hook-enabled)

(define (set-ctx-bash-hook-enabled! enabled?)
        "Enable or disable the context-mode bash post-execution hook."
        (set! %ctx-bash-hook-enabled (and enabled? #t)))

(define*
  (make-ctx-bash-post-hook idx
                           #:key
                           (threshold-bytes %context-mode-threshold-bytes))
  "Return a toggleable bash post-execution hook (lambda (command output) ...).
Passes outputs <= THRESHOLD-BYTES (5KB) through directly; indexes larger outputs
into IDX and returns a compact summary with ctx_search instructions."
  (lambda (_command output)
    (if
        (or (not (ctx-bash-hook-enabled?))
            (not (string? output))
            (<= (string-length output) threshold-bytes))
      output
      (let*
          ((doc-n (index-next-doc-id idx))
           (doc-id (format #f "bash:~a" doc-n))
           (raw-bytes (string-length output))
           (lines (string-split output #\newline))
           (line-count (length lines)))
        (set-index-next-doc-id! idx (+ doc-n 1))
        (ctx-index-content! idx doc-id output)
        (let*
            ((tagged-lines
               (let loop
                 ((i 1) (rem lines) (acc '()))
                 (if (null? rem)
                   (reverse acc)
                   (let ((l (car rem)))
                     (if (regexp-exec %error-pattern l)
                       (loop (+ i 1) (cdr rem) (cons (cons i l) acc))
                       (loop (+ i 1) (cdr rem) acc))))))
             (error-count (length tagged-lines))
             (head-str (string-join (take-n lines 8) "\n"))
             (tail-str (string-join (reverse (take-n (reverse lines) 5)) "\n"))
             (summary
               (if (> error-count 0)
                 (format #f
                         "Identified ~a diagnostic error/failure lines:\n~a\n\nTail summary:\n~a"
                         error-count
                         (string-join
                           (map
                             (lambda (p)
                               (format #f "  L~a: ~a" (car p) (cdr p)))
                             (take-n tagged-lines 15))
                           "\n")
                         tail-str)
                 (format #f
                         "~a\n... [~a lines virtualized into ~a] ...\n~a"
                         head-str
                         (max 0 (- line-count 13))
                         doc-id
                         tail-str)))
             (resp
               (format #f
                       "[~a lines=~a bytes=~a (> ~aB threshold) indexed=~a]\n~a\n[Output virtualized by context-mode bash hook. Use ctx_search(queries=[\"...\"], source=~s) for details]"
                       doc-id
                       line-count
                       raw-bytes
                       threshold-bytes
                       doc-id
                       summary
                       doc-id)))
          (set-index-returned-bytes! idx
                                     (+ (index-returned-bytes idx) (string-length resp)))
          resp)))))

;;;
;;; Stats Formatting
;;;
(define (format-ctx-stats idx)
        "Format virtualization metrics (bytes ingested, tokens saved, reduction %)."
        (let* ((raw (index-raw-bytes idx))
               (ret (index-returned-bytes idx))
               (saved (max 0 (- raw ret)))
               (pct (if (> raw 0) (* 100.0 (/ saved raw)) 0.0))
               (chunks (index-doc-count idx)))
          (format #f "Context Index Stats:\n  Raw Bytes Ingested: ~a (~a estimated tokens)\n  Bytes Returned to Context: ~a (~a estimated tokens)\n  Tokens Saved: ~a (~,1f% reduction)\n  Indexed Chunks: ~a"
                  raw (inexact->exact (round (/ raw 3.8)))
                  ret (inexact->exact (round (/ ret 3.8)))
                  (inexact->exact (round (/ saved 3.8)))
                  pct
                  chunks)))

;;;
;;; Tool Schemas & Registration
;;;
(define %ctx-exec-schema
        `((name . "ctx_exec")
          (description .
                       "Execute a shell command or code snippet (shell, python, javascript, scheme) with virtualized output management. Only stdout summary enters context; large outputs are indexed into SQLite FTS5.")
          (parameters .
                      ((type . "object")
                       (properties .
                                   ((command .
                                             ((type . "string")
                                              (description . "Shell command to execute (when language/code not used)")))
                                    (language .
                                              ((type . "string")
                                               (enum . #("shell" "python" "javascript" "scheme"))
                                               (description . "Runtime language for code execution")))
                                    (code .
                                          ((type . "string")
                                           (description . "Code to execute in language runtime; print only the summary")))
                                    (intent .
                                            ((type . "string")
                                             (description . "Optional search query to extract only relevant output chunks")))))
                       (required . #())))
          (_badb_defer . #f)
          (_badb_alist_args . #t)))

(define %ctx-search-schema
        `((name . "ctx_search")
          (description .
                       "Search the FTS5/BM25 context index. Keywords only (no regex). Pass ALL search questions as 'queries' array in one call (or a single 'query'). Use 'source' (partial match) to scope results.")
          (parameters .
                      ((type . "object")
                       (properties .
                                   ((queries .
                                             ((type . "array") (items . ((type . "string")))
                                              (description .
                                                           "Array of keyword search queries (no regex). Batch ALL questions in one call.")))
                                    (query .
                                           ((type . "string")
                                            (description .
                                                         "Single keyword search query (no regex; use 'queries' to batch multiple)")))
                                    (source .
                                            ((type . "string")
                                             (description . "Filter to a specific indexed source (partial match)")))
                                    (source_id . ((type . "string") (description . "Alias for source")))
                                    (limit .
                                           ((type . "integer")
                                            (description . "Results per query (default 3 for queries, 5 for query)")))
                                    (top_k . ((type . "integer") (description . "Alias for limit")))
                                    (contentType .
                                                 ((type . "string") (enum . #("code" "prose"))
                                                  (description . "Filter results by content type")))
                                    (sort .
                                          ((type . "string")
                                           (enum . #("relevance" "timeline"))
                                           (description . "Sort mode: relevance (default) or timeline")))))
                       (required . #())))
          (_badb_defer . #f)
          (_badb_alist_args . #t)))

(define %ctx-index-schema
        `((name . "ctx_index")
          (description .
                       "Index a file or raw content into the FTS5/BM25 context search engine without loading its full text into LLM context.")
          (parameters .
                      ((type . "object")
                       (properties .
                                   ((path . ((type . "string") (description . "File path to read and index")))
                                    (content .
                                             ((type . "string")
                                              (description . "Raw text/markdown to index (provide this OR path)")))
                                    (source .
                                            ((type . "string") (description . "Optional source label for indexed content")))))
                       (required . #())))
          (_badb_defer . #t)
          (_badb_alist_args . #t)))

(define %ctx-batch-exec-schema
        `((name . "ctx_batch_exec")
          (description .
                       "Execute multiple commands (strings or {label, command} objects) and queries in a single turn, indexing outputs into FTS5 and returning combined query slices.")
          (parameters .
                      ((type . "object")
                       (properties .
                                   ((commands .
                                              ((type . "array")
                                               (description . "List of shell commands or {label, command} objects")))
                                    (queries .
                                             ((type . "array") (items . ((type . "string")))
                                              (description . "List of search queries")))))
                       (required . #("commands"))))
          (_badb_defer . #t)
          (_badb_alist_args . #t)))

(define %ctx-stats-schema
        `((name . "ctx_stats")
          (description .
                       "Report context virtualization statistics: raw bytes ingested, bytes returned, tokens saved percentage.")
          (parameters . ((type . "object") (properties . ())))
          (_badb_defer . #t)
          (_badb_alist_args . #t)))

(define %ctx-execute-file-schema
        `((name . "ctx_execute_file")
          (description .
                       "Read a file into FILE_CONTENT inside a sandboxed subprocess and process it with code without loading raw file contents into context. Only printed summary enters context.")
          (parameters .
                      ((type . "object")
                       (properties .
                                   ((path .
                                          ((type . "string") (description . "File path to read into FILE_CONTENT")))
                                    (language .
                                              ((type . "string")
                                               (enum . #("shell" "python" "javascript" "scheme"))
                                               (description . "Runtime language")))
                                    (code .
                                          ((type . "string")
                                           (description . "Code to process FILE_CONTENT and print summary")))
                                    (intent .
                                            ((type . "string") (description . "Optional search query if output is large")))))
                       (required . #("path" "language" "code"))))
          (_badb_defer . #f)
          (_badb_alist_args . #t)))

(define*
  (register-context-index-tools! reg idx #:key (shell-executor #f))
  "Register context-mode tools in REG."
  (registry-attach! reg
                    "ctx_exec"
                    (lambda (args)
                      (with-tool-args args
                                      ((cmd command "") (lang language #f) (code code #f) (intent intent #f))
                                      (let
                                          ((effective-cmd
                                             (if (and lang code (not (string-null? code)))
                                               (build-language-exec-command lang code)
                                               cmd)))
                                        (ctx-exec! idx
                                                   effective-cmd
                                                   #:shell-executor
                                                   shell-executor
                                                   #:intent-query
                                                   intent))))
                    #:metadata
                    %ctx-exec-schema)
  (registry-attach! reg
                    "ctx_execute_file"
                    (lambda (args)
                      (with-tool-args args
                                      ((p path "") (lang language "shell") (code code "") (intent intent #f))
                                      (ctx-execute-file! idx
                                                         p
                                                         lang
                                                         code
                                                         #:shell-executor
                                                         shell-executor
                                                         #:intent-query
                                                         intent)))
                    #:metadata
                    %ctx-execute-file-schema)
  (registry-attach! reg
                    "ctx_search"
                    (lambda (args)
                      (with-tool-args args
                                      ((qs queries #f) (q query "")
                                       (src source #f)
                                       (sid source_id #f)
                                       (lim limit #f)
                                       (top-k top_k #f)
                                       (ctype contentType #f)
                                       (sort-str sort "relevance"))
                                      (let*
                                          ((eff-source (or src sid))
                                           (eff-sort
                                             (if (equal? sort-str "timeline")
                                               'timeline
                                               'relevance))
                                           (q-list
                                             (cond
                                               ((vector? qs) (vector->list qs))
                                               ((pair? qs) qs)
                                               (else #f))))
                                        (if (and q-list (pair? q-list))
                                          (ctx-search-batch idx
                                                            q-list
                                                            #:source-id
                                                            eff-source
                                                            #:top-k
                                                            (or lim top-k 3)
                                                            #:content-type
                                                            ctype
                                                            #:sort
                                                            eff-sort)
                                          (ctx-search idx
                                                      q
                                                      #:source-id
                                                      eff-source
                                                      #:top-k
                                                      (or lim top-k 5)
                                                      #:content-type
                                                      ctype
                                                      #:sort
                                                      eff-sort)))))
                    #:metadata
                    %ctx-search-schema)
  (registry-attach! reg
                    "ctx_index"
                    (lambda (args)
                      (with-tool-args args
                                      ((p path #f) (content content #f) (src source #f))
                                      (cond
                                        ((and content (not (string-null? content)))
                                          (let*
                                              ((label (or src "inline:content"))
                                               (n (ctx-index-content! idx label content (string-contains label ".md"))))
                                            (format #f "Indexed ~a chunks from content under source ~s" n label)))
                                        ((and p (not (string-null? p)))
                                          (let ((n (ctx-index-file! idx p)))
                                            (format #f "Indexed ~a chunks from '~a'" n p)))
                                        (else "Error: ctx_index requires either 'path' or 'content'."))))
                    #:metadata
                    %ctx-index-schema)
  (registry-attach! reg
                    "ctx_batch_exec"
                    (lambda (args)
                      (with-tool-args args
                                      ((cmds commands '()) (queries queries '()))
                                      (ctx-batch-exec! idx
                                                       #:commands
                                                       (if (vector? cmds)
                                                         (vector->list cmds)
                                                         cmds)
                                                       #:queries
                                                       (if (vector? queries)
                                                         (vector->list queries)
                                                         queries)
                                                       #:shell-executor
                                                       shell-executor)))
                    #:metadata
                    %ctx-batch-exec-schema)
  (registry-attach! reg
                    "ctx_stats"
                    (lambda (_args)
                      (format-ctx-stats idx))
                    #:metadata
                    %ctx-stats-schema))