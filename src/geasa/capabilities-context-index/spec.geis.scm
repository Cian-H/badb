;;; (geasa capabilities-context-index spec.geis) — Contract verification for Context Index

(define-module (test test-capabilities-context-index)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (badb kernel registry)
               #:use-module (geasa capabilities-context-index bm25)
               #:use-module (geasa capabilities-context-index plugin)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1))

(spec-begin "capabilities-context-index")

(test-group "bm25-tokenizer-and-stemmer"
            (let ((tokens (tokenize-text "verify-locked-bundle")))
              (test-assert "tokens extracted" (member "verify" tokens))
              (test-assert "stemming -ed to stem" (member "lock" tokens))
              (test-assert "bundle retained" (member "bundle" tokens)))

            (let ((stemmed-ies (tokenize-text "categories repositories")))
              (test-assert "categories -> category" (member "category" stemmed-ies))
              (test-assert "repositories -> repository" (member "repository" stemmed-ies)))

            (let ((stemmed-ing (tokenize-text "deploying running workers")))
              (test-assert "deploying -> deploy" (member "deploy" stemmed-ing))
              (test-assert "running -> run" (member "run" stemmed-ing))
              (test-assert "workers -> worker" (member "worker" stemmed-ing))))

(test-group "chunking"
            ;; Markdown chunking
            (let* ((md-text "# Title\nOverview paragraph.\n\n## Architecture\nSystem uses Guile.\n\n## Testing\nRun verify script.")
                   (chunks (chunk-markdown "doc.md" md-text)))
              (test-equal "markdown chunks count" 3 (length chunks))
              (test-equal "first chunk section" "Title" (caar chunks))
              (test-equal "second chunk section" "Architecture" (caadr chunks))
              (test-equal "third chunk section" "Testing" (caaddr chunks)))

            ;; Plain lines chunking
            (let* ((lines (map (lambda (i) (format #f "Line ~a of log output" i)) (iota 80)))
                   (text (string-join lines "\n"))
                   (pchunks (chunk-plain-lines text #:window-size 35 #:overlap 5)))
              (test-assert "plain lines produce multiple overlapping windows" (> (length pchunks) 1))
              (test-equal "first chunk starts at line 1" 1 (cadar pchunks))
              (test-equal "first chunk ends at line 35" 35 (caddar pchunks))))

(test-group "bm25-search-ranking"
            (let ((idx (make-bm25-index)))
              ;; Document 1: database migration
              (bm25-add-chunk! idx "doc1" "migration" 1 10
                               "We need to execute database migration rollback when schema deployment fails.")
              ;; Document 2: HTTP server
              (bm25-add-chunk! idx "doc2" "network" 1 10
                               "The HTTP web server listens on port 8080 and handles incoming JSON request payloads.")

              (let ((res-mig (bm25-search idx "database rollback")))
                (test-assert "migration search finds results" (pair? res-mig))
                (test-equal "migration search ranks doc1 top" "doc1" (chunk-source-id (cdar res-mig))))

              (let ((res-http (bm25-search idx "web server payload")))
                (test-assert "http search finds results" (pair? res-http))
                (test-equal "http search ranks doc2 top" "doc2" (chunk-source-id (cdar res-http))))

              ;; Substring boost
              (let ((res-exact (bm25-search idx "incoming JSON request")))
                (test-assert "exact phrase finds doc2" (pair? res-exact))
                (test-equal "exact phrase gives doc2 top score" "doc2" (chunk-source-id (cdar res-exact))))))

(test-group "output-virtualization-and-token-reduction"
            (let* ((tmp-dir (mkdtemp "/tmp/badb-ctx-test-XXXXXX"))
                   (idx (make-bm25-index))
                   ;; Generate 150 lines of log (around 6,000 characters)
                   (log-lines
                     (map (lambda (i)
                            (cond
                              ((= i 42) "ERROR: Test suite assertion failed: expected 200 but got 500 in auth-check")
                              ((= i 85) "FATAL: Connection pool exhausted during load test")
                              (else (format #f "info: [worker-~a] normal operation cycle completed with status ok" i))))
                          (iota 150)))
                   (full-log (string-join log-lines "\n"))
                   (raw-len (string-length full-log))
                   (mock-shell (lambda (cmd) (values full-log 1 #f)))
                   (res (ctx-exec! idx "make test"
                                   #:shell-executor mock-shell
                                   #:max-return-chars 1200
                                   #:offload-dir tmp-dir)))

              (test-assert "returned summary is bounded" (<= (string-length res) 1600))
              (test-assert "summary identifies indexed output" (string-contains res "indexed=exec:0"))
              (test-assert "large output does not create a log file"
                           (not (file-exists? (string-append tmp-dir "/exec-0.log"))))
              (test-assert "summary contains error line 42" (string-contains res "assertion failed"))
              (test-assert "summary contains fatal line 85" (string-contains res "Connection pool exhausted"))

              ;; Virtualization stats
              (let ((stats (format-ctx-stats idx)))
                (test-assert "stats report high reduction" (string-contains stats "reduction"))
                (test-assert "stats report raw bytes" (string-contains stats (number->string raw-len))))

              ;; BM25 search over indexed log
              (let ((search-res (ctx-search idx "auth-check failure expected 200")))
                (test-assert "search finds failing chunk" (string-contains search-res "assertion failed")))

              (system* "rm" "-rf" tmp-dir)))

(test-group "intent-query-virtualization"
            (let* ((tmp-dir (mkdtemp "/tmp/badb-ctx-intent-XXXXXX"))
                   (idx (make-bm25-index))
                   (lines (map (lambda (i)
                                 (if (= i 25)
                                   "CRITICAL: Special intent token XYZ999 found in configuration parser"
                                   (format #f "debug line ~a with generic diagnostics" i)))
                               (iota 80)))
                   (log-text (string-join lines "\n"))
                   (mock-shell (lambda (cmd) (values log-text 0 #f)))
                   (res (ctx-exec! idx "cat config.log"
                                   #:shell-executor mock-shell
                                   #:intent-query "XYZ999 configuration"
                                   #:max-return-chars 1000
                                   #:offload-dir tmp-dir)))
              (test-assert "intent query extracts relevant chunk" (string-contains res "XYZ999"))
              (system* "rm" "-rf" tmp-dir)))

(test-group "batch-exec-and-hook"
            (let* ((tmp-dir (mkdtemp "/tmp/badb-ctx-batch-XXXXXX"))
                   (idx (make-bm25-index))
                   (mock-shell (lambda (cmd) (values (format #f "output of ~a with diagnostic alpha" cmd) 0 #f)))
                   (batch-res (ctx-batch-exec! idx
                                               #:commands '("echo first" "echo second")
                                               #:queries '("alpha")
                                               #:shell-executor mock-shell)))
              (test-assert "batch exec contains first command" (string-contains batch-res "echo first"))
              (test-assert "batch exec contains second command" (string-contains batch-res "echo second"))
              (test-assert "batch search contains alpha query" (string-contains batch-res "alpha"))
              (system* "rm" "-rf" tmp-dir))

            ;; Offload hook
            (let* ((idx (make-bm25-index))
                   (hook (make-ctx-offload-hook idx))
                   (hook-res (hook "large-tool.json" "tool_exec" "Lots of content with keyword FOOBAR123")))
              (test-assert "hook returns pointer" (string-contains hook-res "Output virtualized to large-tool.json"))
              (let ((search-res (ctx-search idx "FOOBAR123")))
                (test-assert "search finds offloaded content" (string-contains search-res "FOOBAR123")))))

(test-group "index-persistence"
            (let* ((tmp-dir (mkdtemp "/tmp/badb-ctx-persist-XXXXXX"))
                   (index-file (string-append tmp-dir "/index.scm"))
                   (idx (make-bm25-index)))
              (bm25-add-chunk! idx "f1" "sec1" 1 10 "Persistent index storage test verification token ZETA42")
              (set-index-raw-bytes! idx 1234)
              (set-index-returned-bytes! idx 234)
              (save-bm25-index! idx index-file)

              (test-assert "index file created" (file-exists? index-file))

              (let ((loaded-idx (load-bm25-index index-file)))
                (test-equal "loaded raw bytes match" 1234 (index-raw-bytes loaded-idx))
                (test-equal "loaded returned bytes match" 234 (index-returned-bytes loaded-idx))
                (let ((res (bm25-search loaded-idx "ZETA42")))
                  (test-assert "search on loaded index finds chunk" (pair? res))
                  (test-equal "source matches" "f1" (chunk-source-id (cdar res)))))
              (system* "rm" "-rf" tmp-dir)))

(test-group "tool-registration"
            (let ((reg (make-registry))
                  (idx (make-bm25-index)))
              (register-context-index-tools! reg idx)
              (test-assert "ctx_exec registered" (procedure? (registry-lookup reg "ctx_exec")))
              (test-assert "ctx_search registered" (procedure? (registry-lookup reg "ctx_search")))
              (test-assert "ctx_index registered" (procedure? (registry-lookup reg "ctx_index")))
              (test-assert "ctx_batch_exec registered" (procedure? (registry-lookup reg "ctx_batch_exec")))
              (test-assert "ctx_stats registered" (procedure? (registry-lookup reg "ctx_stats")))

              ;; Check _badb_defer flags via registry-metadata
              (let ((exec-meta (registry-metadata reg "ctx_exec"))
                    (search-meta (registry-metadata reg "ctx_search"))
                    (index-meta (registry-metadata reg "ctx_index")))
                (test-equal "ctx_exec is core (_badb_defer #f)"
                            #f (assoc-ref exec-meta '_badb_defer))
                (test-equal "ctx_search is core (_badb_defer #f)"
                            #f (assoc-ref search-meta '_badb_defer))
                (test-equal "ctx_index is deferred (_badb_defer #t)"
                            #t (assoc-ref index-meta '_badb_defer)))))

(spec-end)
(spec-exit)
