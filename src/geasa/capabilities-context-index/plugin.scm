;;; (geasa capabilities-context-index plugin) — Context Virtualization & Indexing
;;;
;;; Implements context-mode style output virtualization, pure-Guile BM25 token
;;; search over oversized tool outputs and build logs, and dynamic offloading.

(define-module (geasa capabilities-context-index plugin)
  #:use-module (badb kernel registry)
  #:use-module (geasa capabilities-context-index bm25)
  #:use-module (geasa foundation-util plugin)
  #:use-module (json)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (ctx-index-file!
            ctx-index-content!
            ctx-search
            ctx-exec!
            ctx-batch-exec!
            make-ctx-offload-hook
            format-ctx-stats
            register-context-index-tools!
            %ctx-exec-schema
            %ctx-search-schema
            %ctx-index-schema
            %ctx-batch-exec-schema
            %ctx-stats-schema)
  #:re-export (make-bm25-index))

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

(define* (ctx-index-content! idx source-id text #:optional (is-markdown? #f))
  "Index TEXT under SOURCE-ID into IDX. Returns the number of chunks added."
  (let* ((raw-len (string-length text))
         (chunks (if is-markdown?
                     (chunk-markdown source-id text)
                     (chunk-plain-lines text))))
    (set-index-raw-bytes! idx (+ (index-raw-bytes idx) raw-len))
    (for-each
     (lambda (c)
       (let ((sec (car c))
             (sl (cadr c))
             (el (caddr c))
             (txt (cadddr c)))
         (bm25-add-chunk! idx source-id sec sl el txt)))
     chunks)
    (length chunks)))

;;;
;;; Context Search
;;;

(define* (ctx-search idx query #:key (source-id #f) (top-k 5) (max-chars 2400))
  "Search the BM25 index for QUERY and return a formatted context slice."
  (let* ((matches (bm25-search idx query #:source-id source-id #:top-k top-k)))
    (if (null? matches)
        (format #f "No relevant context matches found for query: ~s~a"
                query
                (if source-id (format #f " in source ~s" source-id) ""))
        (let loop ((rem matches)
                   (acc '())
                   (total-len 0))
          (if (null? rem)
              (let ((result (string-join (reverse acc) "\n---\n")))
                (set-index-returned-bytes! idx (+ (index-returned-bytes idx) (string-length result)))
                result)
              (let* ((match (car rem))
                     (score (car match))
                     (c (cdr match))
                     (sec-part (if (chunk-section c) (format #f " sec=~s" (chunk-section c)) ""))
                     (entry (format #f "[~a:L~a-L~a score=~,2f~a]\n~a"
                                    (chunk-source-id c)
                                    (chunk-start-line c)
                                    (chunk-end-line c)
                                    score
                                    sec-part
                                    (chunk-text c)))
                     (entry-len (string-length entry)))
                (if (and (> total-len 0) (> (+ total-len entry-len) max-chars))
                    (let ((result (string-append (string-join (reverse acc) "\n---\n")
                                                 "\n... [Remaining results truncated for token budget]")))
                      (set-index-returned-bytes! idx (+ (index-returned-bytes idx) (string-length result)))
                      result)
                    (loop (cdr rem)
                          (cons entry acc)
                          (+ total-len entry-len)))))))))

;;;
;;; Command Execution & Virtualization
;;;

(define %error-pattern
  (make-regexp "(error|fail|traceback|exception|panic|assert|fatal|denied)" regexp/icase))

(define* (ctx-exec! idx command #:key (shell-executor #f) (intent-query #f)
                   (max-return-chars 1600) (offload-dir ".badb/ctx"))
  "Execute COMMAND with virtualized output management. Automatically offloads
oversized outputs and indexes them into BM25."
  (let ((exec-fn (or shell-executor default-shell-exec)))
    (call-with-values
      (lambda () (exec-fn command))
      (lambda (out . rest)
        (let* ((exit-code (if (pair? rest) (car rest) 0))
               (err (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) #f))
               (clean-out (or out "")))
          (if err
              (format #f "[Command execution failed with exit=~a]: ~a" exit-code err)
              (let* ((doc-n (index-next-doc-id idx))
                     (doc-id (format #f "exec:~a" doc-n))
                     (raw-bytes (string-length clean-out))
                     (lines (string-split clean-out #\newline))
                     (line-count (length lines)))
                (set-index-next-doc-id! idx (+ doc-n 1))
                ;; Always index into BM25
                (ctx-index-content! idx doc-id clean-out)

                (if (and (<= raw-bytes max-return-chars) (not intent-query))
                    ;; Output is small enough to return directly
                    (begin
                      (set-index-returned-bytes! idx (+ (index-returned-bytes idx) raw-bytes))
                      clean-out)
                    ;; Output is large or intent-query was specified: virtualize!
                    (begin
                      (mkdir-p offload-dir)
                      (let ((log-path (format #f "~a/exec-~a.log" offload-dir doc-n)))
                        (catch #t
                          (lambda ()
                            (call-with-output-file log-path
                              (lambda (p) (display clean-out p))))
                          (lambda _ #f))

                        (let* ((header (format #f "[~a exit=~a lines=~a bytes=~a offloaded=~a]\n"
                                               doc-id exit-code line-count raw-bytes log-path))
                               (summary-body
                                (if intent-query
                                    ;; Extract intent query chunks
                                    (ctx-search idx intent-query #:source-id doc-id #:top-k 3 #:max-chars (- max-return-chars 300))
                                    ;; Extract failure lines or window
                                    (let* ((tagged-lines
                                            (let loop ((i 1) (rem lines) (acc '()))
                                              (if (null? rem)
                                                  (reverse acc)
                                                  (let ((l (car rem)))
                                                    (if (regexp-exec %error-pattern l)
                                                        (loop (+ i 1) (cdr rem) (cons (cons i l) acc))
                                                        (loop (+ i 1) (cdr rem) acc))))))
                                           (error-count (length tagged-lines)))
                                      (cond
                                        ((> error-count 0)
                                         (let* ((top-errors (take-n tagged-lines 15))
                                                (formatted-errors
                                                 (map (lambda (pair)
                                                        (format #f "  L~a: ~a" (car pair) (cdr pair)))
                                                      top-errors))
                                                (tail-lines (take-n (reverse lines) 5))
                                                (tail-str (string-join (reverse tail-lines) "\n")))
                                           (format #f "Identified ~a diagnostic error/failure lines:\n~a\n\nTail summary:\n~a"
                                                   error-count
                                                   (string-join formatted-errors "\n")
                                                   tail-str)))
                                        (else
                                         (let* ((head-lines (take-n lines 8))
                                                (tail-lines (take-n (reverse lines) 5))
                                                (head-str (string-join head-lines "\n"))
                                                (tail-str (string-join (reverse tail-lines) "\n")))
                                           (format #f "~a\n... [~a lines virtualized into ~a] ...\n~a"
                                                   head-str (- line-count 13) doc-id tail-str)))))))
                               (footer (format #f "\n[Output virtualized to save tokens. Use ctx_search(query: \"...\", source_id: ~s) for details]"
                                               doc-id))
                               (full-response (string-append header summary-body footer)))
                          (set-index-returned-bytes! idx (+ (index-returned-bytes idx) (string-length full-response)))
                          full-response)))))))))))

;;;
;;; Batch Execution
;;;

(define* (ctx-batch-exec! idx #:key (commands '()) (queries '()) (shell-executor #f)
                         (max-return-chars 2800))
  "Execute a batch of commands and queries in a single turn."
  (let* ((cmd-results
          (map (lambda (cmd)
                 (let ((res (ctx-exec! idx cmd #:shell-executor shell-executor #:max-return-chars 800)))
                   (format #f "$ ~a\n~a" cmd res)))
               commands))
         (query-results
          (map (lambda (q)
                 (let ((res (ctx-search idx q #:top-k 3 #:max-chars 800)))
                   (format #f "? ~a\n~a" q res)))
               queries))
         (combined (string-append
                    (if (pair? cmd-results)
                        (string-append "=== Command Executions ===\n" (string-join cmd-results "\n\n") "\n\n")
                        "")
                    (if (pair? query-results)
                        (string-append "=== Query Searches ===\n" (string-join query-results "\n\n"))
                        ""))))
    (if (> (string-length combined) max-return-chars)
        (string-append (substring combined 0 max-return-chars) "\n... [Batch results truncated]")
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
    (description . "Execute a shell command with virtualized output management. Automatically offloads and indexes large outputs into the BM25 context index, returning a concise summary or intent-matched slices.")
    (parameters .
     ((type . "object")
      (properties .
       ((command . ((type . "string") (description . "Shell command to execute")))
        (intent . ((type . "string") (description . "Optional search query to extract only relevant output chunks")))))
      (required . #("command"))))
    (_badb_defer . #f)))

(define %ctx-search-schema
  `((name . "ctx_search")
    (description . "Search the BM25 context index for keywords, error patterns, or function names across indexed files and execution outputs.")
    (parameters .
     ((type . "object")
      (properties .
       ((query . ((type . "string") (description . "Search keywords or exact phrase")))
        (source_id . ((type . "string") (description . "Optional source ID to restrict search (e.g. 'exec:1' or file path)")))
        (top_k . ((type . "integer") (description . "Maximum number of chunks to return (default 5)")))))
      (required . #("query"))))
    (_badb_defer . #f)))

(define %ctx-index-schema
  `((name . "ctx_index")
    (description . "Index a file into the BM25 context search engine without loading its full text into LLM context.")
    (parameters .
     ((type . "object")
      (properties .
       ((path . ((type . "string") (description . "File path to index")))))
      (required . #("path"))))
    (_badb_defer . #t)))

(define %ctx-batch-exec-schema
  `((name . "ctx_batch_exec")
    (description . "Execute multiple commands and queries in a single turn, indexing outputs and returning combined query slices.")
    (parameters .
     ((type . "object")
      (properties .
       ((commands . ((type . "array") (items . ((type . "string"))) (description . "List of shell commands")))
        (queries . ((type . "array") (items . ((type . "string"))) (description . "List of search queries")))))
      (required . #("commands"))))
    (_badb_defer . #t)))

(define %ctx-stats-schema
  `((name . "ctx_stats")
    (description . "Report context virtualization statistics: raw bytes ingested, bytes returned, tokens saved percentage.")
    (parameters .
     ((type . "object")
      (properties . ())))
    (_badb_defer . #t)))

(define* (register-context-index-tools! reg idx #:key (shell-executor #f))
  "Register context-mode tools in REG."
  (registry-attach!
   reg
   "ctx_exec"
   (lambda (args)
     (with-tool-args args ((cmd command "")
                           (intent intent #f))
       (ctx-exec! idx cmd #:shell-executor shell-executor #:intent-query intent)))
   #:metadata %ctx-exec-schema)

  (registry-attach!
   reg
   "ctx_search"
   (lambda (args)
     (with-tool-args args ((q query "")
                           (sid source_id #f)
                           (top-k top_k 5))
       (ctx-search idx q #:source-id sid #:top-k top-k)))
   #:metadata %ctx-search-schema)

  (registry-attach!
   reg
   "ctx_index"
   (lambda (args)
     (with-tool-args args ((p path ""))
       (let ((n (ctx-index-file! idx p)))
         (format #f "Indexed ~a chunks from '~a'" n p))))
   #:metadata %ctx-index-schema)

  (registry-attach!
   reg
   "ctx_batch_exec"
   (lambda (args)
     (with-tool-args args ((cmds commands '())
                           (queries queries '()))
       (ctx-batch-exec! idx #:commands (if (vector? cmds) (vector->list cmds) cmds)
                            #:queries (if (vector? queries) (vector->list queries) queries)
                            #:shell-executor shell-executor)))
   #:metadata %ctx-batch-exec-schema)

  (registry-attach!
   reg
   "ctx_stats"
   (lambda (_args)
     (format-ctx-stats idx))
   #:metadata %ctx-stats-schema))
