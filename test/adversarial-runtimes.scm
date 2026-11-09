;;; test/adversarial-runtimes.scm — Empirical Challenger Stress Harness for M3 Runtimes
;;;
;;; Validates hostile inputs, failure modes, boundary conditions, and stress limits across:
;;; 1. (geasa runtimes-react plugin)
;;; 2. (geasa runtimes-context plugin)
;;; 3. (geasa runtimes-rollback plugin)
;;; 4. (geasa runtimes-prompt plugin)

(define-module (test adversarial-runtimes)
               #:use-module (srfi srfi-64)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:use-module (web response)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 format)
               #:use-module (ice-9 optargs)
               #:use-module (json)
               #:use-module (badb kernel registry)
               #:use-module (badb kernel journal)
               #:use-module (badb kernel transaction)
               #:use-module (geasa providers-model common)
               #:use-module (geasa providers-model openai)
               #:use-module (geasa providers-model mock)
               #:use-module (geasa runtimes-react plugin)
               #:use-module (geasa runtimes-context plugin)
               #:use-module (geasa runtimes-prompt plugin)
               #:use-module (geasa runtimes-rollback plugin)
               #:use-module (geasa policies-guardrail plugin))

(test-begin "adversarial-runtimes")

;;; =========================================================================
;;; 1. ADVERSARIAL REACT EXECUTION ENGINE ((geasa runtimes-react plugin))
;;; =========================================================================
(test-group "challenger-react-hostile-inputs"

            ;; --- 1.1 Unknown tool fails closed, records error in journal, doesn't crash ---
            (test-assert "unknown-tool-fails-closed-and-records-in-journal"
                         (let* ((tmp-dir (mkdtemp "/tmp/badb-challenger-react-XXXXXX"))
                                (jrn-path (string-append tmp-dir "/jrn.jsonl"))
                                (jrn (make-journal jrn-path))
                                (reg (make-registry))
                                (call '((id . "call_unknown")
                                        (name . "non_existent_tool_999")
                                        (arguments . ((param . "val"))))))
                           (dynamic-wind
                             (lambda () #f)
                             (lambda ()
                               (call-with-values
                                 (lambda () (execute-tool-call call reg #f #:jrn jrn))
                                 (lambda (res err?)
                                   (let ((entries (journal-entries jrn)))
                                     (and (eq? err? #t)
                                          (pair? res)
                                          (string-contains (assoc-ref res 'error) "not found in registry")
                                          (= (length entries) 1)
                                          (string=? (assoc-ref (car entries) "status") "error"))))))
                             (lambda ()
                               (system* "rm" "-rf" tmp-dir)))))

            ;; --- 1.2 Tool throwing Scheme exception is caught, formatted, and logged ---
            (test-assert "crashing-tool-handled-gracefully"
                         (let* ((tmp-dir (mkdtemp "/tmp/badb-challenger-react-XXXXXX"))
                                (jrn-path (string-append tmp-dir "/jrn.jsonl"))
                                (jrn (make-journal jrn-path))
                                (reg (make-registry))
                                (_ (registry-attach! reg "explode"
                                                     (lambda () (/ 1 0)) ;; Division by zero
                                                     '((description . "Throws division by zero"))))
                                (call '((id . "call_explode")
                                        (name . "explode")
                                        (arguments . ()))))
                           (dynamic-wind
                             (lambda () #f)
                             (lambda ()
                               (call-with-values
                                 (lambda () (execute-tool-call call reg #f #:jrn jrn))
                                 (lambda (res err?)
                                   (let ((entries (journal-entries jrn)))
                                     (and (eq? err? #t)
                                          (pair? res)
                                          (string-contains (assoc-ref res 'error) "Execution of tool 'explode' failed")
                                          (string-contains (assoc-ref res 'error) "numerical-overflow")
                                          (= (length entries) 1)
                                          (string=? (assoc-ref (car entries) "status") "error"))))))
                             (lambda ()
                               (system* "rm" "-rf" tmp-dir)))))

            ;; --- 1.3 Non-procedure registry entry fails safely ---
            (test-assert "non-procedure-registry-entry-fails-safely"
                         (let* ((tmp-dir (mkdtemp "/tmp/badb-challenger-react-XXXXXX"))
                                (jrn-path (string-append tmp-dir "/jrn.jsonl"))
                                (jrn (make-journal jrn-path))
                                (reg (make-registry))
                                (_ (registry-attach! reg "bogus-entry" 12345 '((description . "Not a proc"))))
                                (call '((id . "call_bogus")
                                        (name . "bogus-entry")
                                        (arguments . ()))))
                           (dynamic-wind
                             (lambda () #f)
                             (lambda ()
                               (call-with-values
                                 (lambda () (execute-tool-call call reg #f #:jrn jrn))
                                 (lambda (res err?)
                                   (and (eq? err? #t)
                                        (pair? res)
                                        (string-contains (assoc-ref res 'error) "not a procedure")))))
                             (lambda ()
                               (system* "rm" "-rf" tmp-dir)))))

            ;; --- 1.4 Guardrail denial variants ---
            (test-assert "guardrail-denial-pair"
                         (let* ((reg (make-registry))
                                (called? #f)
                                (_ (registry-attach! reg "dangerous_action"
                                                     (lambda () (set! called? #t) "done")
                                                     '((description . "Dangerous"))))
                                (call '((id . "call_danger")
                                        (name . "dangerous_action")
                                        (arguments . ()))))
                           (call-with-values
                             (lambda () (execute-tool-call call reg #f #:guardrail (lambda (n a) (cons 'deny "Forbidden"))))
                             (lambda (res err?)
                               (and (eq? err? #t)
                                    (not called?)
                                    (equal? (assoc-ref res 'error) "Forbidden"))))))

            (test-assert "guardrail-denial-symbol"
                         (let* ((reg (make-registry))
                                (call '((id . "call_danger") (name . "dangerous_action") (arguments . ()))))
                           (call-with-values
                             (lambda () (execute-tool-call call reg #f #:guardrail (lambda (n a) 'deny)))
                             (lambda (res err?)
                               (and (eq? err? #t)
                                    (string-contains (assoc-ref res 'error) "Permission denied"))))))

            (test-assert "guardrail-denial-false"
                         (let* ((reg (make-registry))
                                (call '((id . "call_danger") (name . "dangerous_action") (arguments . ()))))
                           (call-with-values
                             (lambda () (execute-tool-call call reg #f #:guardrail (lambda (n a) #f)))
                             (lambda (res err?)
                               (eq? err? #t)))))

            (test-assert "guardrail-exception-fails-closed"
                         (let* ((reg (make-registry))
                                (call '((id . "call_danger") (name . "dangerous_action") (arguments . ()))))
                           (call-with-values
                             (lambda () (execute-tool-call call reg #f #:guardrail (lambda (n a) (error "Guardrail explosion!"))))
                             (lambda (res err?)
                               (and (eq? err? #t)
                                    (string-contains (assoc-ref res 'error) "Guardrail exception"))))))

            ;; --- 1.5 Diverse tool return types ---
            (test-assert "tool-returns-diverse-types"
                         (let* ((reg (make-registry))
                                (_ (registry-attach! reg "ret_vec" (lambda () #(1 2 3)) '((description . "vector"))))
                                (_ (registry-attach! reg "ret_bool" (lambda () #t) '((description . "bool"))))
                                (_ (registry-attach! reg "ret_num" (lambda () 42.5) '((description . "num"))))
                                (_ (registry-attach! reg "ret_null" (lambda () '()) '((description . "null")))))
                           (and
                             (call-with-values (lambda () (execute-tool-call '((name . "ret_vec") (arguments . ())) reg #f))
                                               (lambda (res err?) (and (not err?) (vector? res))))
                             (call-with-values (lambda () (execute-tool-call '((name . "ret_bool") (arguments . ())) reg #f))
                                               (lambda (res err?) (and (not err?) (eq? res #t))))
                             (call-with-values (lambda () (execute-tool-call '((name . "ret_num") (arguments . ())) reg #f))
                                               (lambda (res err?) (and (not err?) (= res 42.5))))
                             (call-with-values (lambda () (execute-tool-call '((name . "ret_null") (arguments . ())) reg #f))
                                               (lambda (res err?) (and (not err?) (null? res)))))))

            ;; --- 1.6 Malformed arguments handling ---
            (test-assert "malformed-tool-arguments"
                         (let* ((reg (make-registry))
                                (_ (registry-attach! reg "identity_tool" (lambda (x) x) '((description . "Echo")))))
                           (and
                             (call-with-values (lambda () (execute-tool-call '((name . "identity_tool") (arguments . "simple string")) reg #f))
                                               (lambda (res err?) (and (not err?) (equal? res "simple string"))))
                             (call-with-values (lambda () (execute-tool-call '((name . "identity_tool") (arguments . 999)) reg #f))
                                               (lambda (res err?) (and (not err?) (equal? res 999)))))))

            ;; --- 1.7 Deep multi-turn loop and max-iterations ceiling ---
            (test-assert "deep-multi-turn-max-iterations-ceiling"
                         (let* ((reg (make-registry))
                                (_ (registry-attach! reg "tick" (lambda () "tock") '((description . "Tick"))))
                                (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
                                (turn-count 0)
                                (mock-poster
                                  (lambda* (uri #:key body headers streaming?)
                                           (set! turn-count (+ turn-count 1))
                                           (let ((resp-body (scm->json-string
                                                              (list (cons 'choices
                                                                          (vector (list (cons 'message
                                                                                              (list (cons 'role "assistant")
                                                                                                    (cons 'content "Ticking")
                                                                                                    (cons 'tool_calls
                                                                                                          (vector (list (cons 'id (format #f "call_~a" turn-count))
                                                                                                                        (cons 'type "function")
                                                                                                                        (cons 'function
                                                                                                                              (list (cons 'name "tick")
                                                                                                                                    (cons 'arguments "{}")))))))))))))))
                                             (values (build-response #:code 200) resp-body)))))
                           (parameterize ((current-http-post mock-poster))
                                         (let* ((session (make-react-session
                                                           #:client client
                                                           #:messages '(((role . "user") (content . "Start ticking")))
                                                           #:reg reg
                                                           #:max-iterations 25)))
                                           (call-with-values (lambda () (react-run! session))
                                                             (lambda (verdict content sess)
                                                               (and (eq? verdict 'max-iterations-reached)
                                                                    (eq? (react-session-status session) 'max-iterations-reached)
                                                                    (= (react-session-iteration session) 25)
                                                                    (= turn-count 25)
                                                                    (= (length (react-session-messages session)) 51))))))))

            ;; --- 1.8 Steering queue: FIFO, multiple interrupts, empty checks ---
            (test-assert "steering-queue-comprehensive-fifo-and-drain"
                         (let ((q (make-steering-queue)))
                           (and
                             (steering-empty? q)
                             (= (steering-count q) 0)
                             (begin
                               (steering-push! q "msg1")
                               (steering-push! q "msg2")
                               (steering-push! q 'interrupt)
                               (steering-push! q "msg3")
                               (not (steering-empty? q)))
                             (= (steering-count q) 4)
                             (equal? (steering-drain! q) '("msg1" "msg2" interrupt "msg3"))
                             (steering-empty? q)
                             (= (steering-count q) 0)
                             (null? (steering-drain! q)))))

            ;; --- 1.9 XML Tool Call Fallback Parser Edge Cases ---
            (test-assert "xml-tool-call-parser-adversarial"
                         (let* ((empty-tags "<tool_call></tool_call>")
                                (unclosed "<tool_call>{\"name\": \"cmd\"")
                                (broken-json "<tool_call>{not valid json: 123}</tool_call>")
                                (multiple "<tool_call>{\"name\": \"t1\", \"arguments\": {\"a\": 1}}</tool_call>\n<tool_call>{\"name\": \"t2\", \"arguments\": {\"b\": 2}}</tool_call>")
                                (nested-json "<tool_call>{\"name\": \"t3\", \"arguments\": {\"nested\": {\"deep\": [1, 2, 3]}}}</tool_call>"))
                           (and
                             (null? (parse-xml-tool-calls empty-tags))
                             (null? (parse-xml-tool-calls unclosed))
                             (null? (parse-xml-tool-calls broken-json))
                             (let ((multi (parse-xml-tool-calls multiple)))
                               (and (= (length multi) 2)
                                    (equal? (assoc-ref (car multi) 'name) "t1")
                                    (equal? (assoc-ref (cadr multi) 'name) "t2")))
                             (let ((nest (parse-xml-tool-calls nested-json)))
                               (and (= (length nest) 1)
                                    (equal? (assoc-ref (car nest) 'name) "t3"))))))

            ;; --- 1.10 Token budget compaction in ReAct loop ---
            (test-assert "react-loop-invokes-context-compactor-when-budget-exceeded"
                         (let* ((reg (make-registry))
                                (_ (registry-attach! reg "huge_data"
                                                     (lambda () (make-string 2000 #\Z))
                                                     '((description . "Returns huge data"))))
                                (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
                                (turn 0)
                                (mock-poster
                                  (lambda* (uri #:key body headers streaming?)
                                           (set! turn (+ turn 1))
                                           (if (= turn 1)
                                             (let ((resp-body (scm->json-string
                                                                (list (cons 'choices
                                                                            (vector (list (cons 'message
                                                                                                (list (cons 'role "assistant")
                                                                                                      (cons 'content "Fetching data")
                                                                                                      (cons 'tool_calls
                                                                                                            (vector (list (cons 'id "call_h1")
                                                                                                                          (cons 'type "function")
                                                                                                                          (cons 'function
                                                                                                                                (list (cons 'name "huge_data")
                                                                                                                                      (cons 'arguments "{}")))))))))))))))
                                               (values (build-response #:code 200) resp-body))
                                             (let ((resp-body (scm->json-string
                                                                (list (cons 'choices
                                                                            (vector (list (cons 'message
                                                                                                (list (cons 'role "assistant")
                                                                                                      (cons 'content "Data processed."))))))))))
                                               (values (build-response #:code 200) resp-body)))))
                                (compactor (make-context-compactor
                                             #:max-tokens 200
                                             #:reserve-tokens 50
                                             #:strategy (strategy-cascading #:keep-recent-turns 0 #:max-tool-chars 80))))
                           (parameterize ((current-http-post mock-poster))
                                         (let* ((session (make-react-session
                                                           #:client client
                                                           #:messages '(((role . "user") (content . "Fetch big data")))
                                                           #:reg reg
                                                           #:compactor compactor
                                                           #:max-iterations 5)))
                                           (call-with-values (lambda () (react-run! session))
                                                             (lambda (verdict content sess)
                                                               (and (eq? verdict 'completed)
                                                                    (equal? content "Data processed.")
                                                                    ;; Verify tool output in transcript was pruned by context compactor
                                                                    (any (lambda (m)
                                                                           (and (equal? (assoc-ref m 'role) "tool")
                                                                                (string-contains (assoc-ref m 'content) "[Pruned tool output")))
                                                                         (react-session-messages session))))))))))

;;; =========================================================================
;;; 2. ADVERSARIAL CONTEXT COMPACTOR ((geasa runtimes-context plugin))
;;; =========================================================================
(test-group "challenger-context-compactor"

            ;; --- 2.1 Token Estimator Extreme Boundary Cases ---
            (test-assert "token-estimators-extreme-inputs"
                         (let* ((empty-char (char-heuristic-estimator ""))
                                (spaces-char (char-heuristic-estimator "     \n\t  "))
                                (single-char (char-heuristic-estimator "x"))
                                (huge-str (make-string 80000 #\a))
                                (huge-tokens (char-heuristic-estimator huge-str))
                                (empty-word (word-heuristic-estimator ""))
                                (spaces-word (word-heuristic-estimator "   \n\t\r   "))
                                (single-word (word-heuristic-estimator "hello"))
                                (many-words (word-heuristic-estimator "one two three four five six seven eight nine ten")))
                           (and (= empty-char 0)
                                (= spaces-char 0)
                                (= single-char 1)
                                (= huge-tokens 20000)
                                (= empty-word 0)
                                (= spaces-word 0)
                                (>= single-word 1)
                                (>= many-words 10))))

            ;; --- 2.2 Surgical Tool Pruning: Boundaries, Multi-turn, Idempotence ---
            (test-assert "prune-tool-results-boundary-and-idempotence"
                         (let* ((pruner (strategy-prune-tool-results #:keep-recent-turns 1 #:max-tool-chars 80))
                                (empty-res (pruner '() 100 char-heuristic-estimator))
                                (no-tools (list '((role . "system") (content . "Sys"))
                                                '((role . "user") (content . "Hello"))
                                                '((role . "assistant") (content . "Hi"))))
                                (no-tools-res (pruner no-tools 100 char-heuristic-estimator))
                                (verbose-1 (make-string 300 #\A))
                                (verbose-2 (make-string 300 #\B))
                                (recent-tool (make-string 30 #\C))
                                (convo (list '((role . "system") (content . "Sys"))
                                             '((role . "user") (content . "Turn 1"))
                                             `((role . "tool") (name . "t1") (content . ,verbose-1))
                                             '((role . "assistant") (content . "Ans 1"))
                                             '((role . "user") (content . "Turn 2"))
                                             `((role . "tool") (name . "t2") (content . ,verbose-2))
                                             '((role . "assistant") (content . "Ans 2"))
                                             '((role . "user") (content . "Turn 3"))
                                             `((role . "tool") (name . "t3") (content . ,recent-tool))
                                             '((role . "assistant") (content . "Ans 3"))))
                                (compacted-1 (pruner convo 1000 char-heuristic-estimator))
                                (compacted-2 (pruner compacted-1 1000 char-heuristic-estimator)))
                           (and (null? empty-res)
                                (= (length no-tools-res) 3)
                                (= (length compacted-1) 10)
                                (string-contains (assoc-ref (list-ref compacted-1 2) 'content) "[Pruned tool output")
                                (string-contains (assoc-ref (list-ref compacted-1 5) 'content) "[Pruned tool output")
                                (equal? (assoc-ref (list-ref compacted-1 8) 'content) recent-tool)
                                (equal? (assoc-ref (list-ref compacted-1 2) 'content)
                                        (assoc-ref (list-ref compacted-2 2) 'content)))))

            ;; --- 2.3 Sliding Window: System Pinning & Budget Starvation ---
            (test-assert "sliding-window-pinning-and-starvation"
                         (let* ((sys-msg '((role . "system") (content . "System Prompt")))
                                (u1 '((role . "user") (content . "Message 1")))
                                (a1 '((role . "assistant") (content . "Reply 1")))
                                (u2 '((role . "user") (content . "Message 2")))
                                (a2 '((role . "assistant") (content . "Reply 2")))
                                (messages (list sys-msg u1 a1 u2 a2))
                                (window-fn (strategy-sliding-window #:keep-system-prompt #t))
                                (tiny-compacted (window-fn messages 1 char-heuristic-estimator))
                                (zero-compacted (window-fn messages 0 char-heuristic-estimator))
                                (generous-compacted (window-fn messages 10000 char-heuristic-estimator)))
                           (and (>= (length tiny-compacted) 1)
                                (equal? (assoc-ref (car tiny-compacted) 'role) "system")
                                (>= (length zero-compacted) 1)
                                (equal? (assoc-ref (car zero-compacted) 'role) "system")
                                (= (length generous-compacted) 5))))

            ;; --- 2.4 Summarization Strategy ---
            (test-assert "summarize-older-turns-strategy"
                         (let* ((summarizer (lambda (msgs) (format #f "Summarized ~a older messages" (length msgs))))
                                (strat (strategy-summarize-older-turns summarizer #:keep-recent-turns 1))
                                (convo (list '((role . "system") (content . "Initial system instructions"))
                                             '((role . "user") (content . "Turn 1"))
                                             '((role . "assistant") (content . "Answer 1"))
                                             '((role . "user") (content . "Turn 2"))
                                             '((role . "assistant") (content . "Answer 2"))
                                             '((role . "user") (content . "Turn 3"))
                                             '((role . "assistant") (content . "Answer 3"))))
                                (res (strat convo 50 char-heuristic-estimator)))
                           (and (equal? (assoc-ref (car res) 'role) "system")
                                (string-contains (assoc-ref (cadr res) 'content) "Context Summary of preceding turns")
                                (string-contains (assoc-ref (cadr res) 'content) "Summarized 4 older messages")
                                (equal? (assoc-ref (last res) 'content) "Answer 3"))))

            ;; --- 2.5 Cascading Multi-Strategy Pipeline ---
            (test-assert "cascading-pipeline-reduction"
                         (let* ((compactor (make-context-compactor
                                             #:max-tokens 100
                                             #:reserve-tokens 20
                                             #:strategy (strategy-cascading)))
                                (verbose-tool (make-string 400 #\x))
                                (messages (list
                                            '((role . "system") (content . "System instructions"))
                                            '((role . "user") (content . "Task step 1"))
                                            `((role . "tool") (name . "search") (content . ,verbose-tool))
                                            '((role . "assistant") (content . "Found data"))
                                            '((role . "user") (content . "Task step 2"))
                                            '((role . "assistant") (content . "Continuing"))))
                                (needed? (compaction-needed? compactor messages))
                                (compacted (compact-messages compactor messages))
                                (tokens-after (estimate-conversation-tokens compacted)))
                           (and needed?
                                (<= tokens-after (context-compactor-effective-ceiling compactor))
                                (equal? (assoc-ref (car compacted) 'role) "system")))))

;;; =========================================================================
;;; 3. ADVERSARIAL TIME-TRAVEL ROLLBACK ((geasa runtimes-rollback plugin))
;;; =========================================================================
(test-group "challenger-rollback-engine"

            (let* ((ws (mkdtemp "/tmp/badb-challenger-rollback-ws-XXXXXX"))
                   (storage (string-append ws "/.badb/snapshots"))
                   (jrn-path (string-append ws "/.badb/journal.jsonl"))
                   (jrn (make-journal jrn-path))
                   (reg (make-registry))
                   (mgr (make-rollback-manager #:workspace-dir ws
                                               #:storage-dir storage
                                               #:registry reg
                                               #:journal jrn)))

              (let ((f1 (string-append ws "/alpha.txt"))
                    (f2 (string-append ws "/beta.txt"))
                    (sub (string-append ws "/sub")))
                (mkdir sub)
                (call-with-output-file f1 (lambda (p) (display "Alpha v1\n" p)))
                (call-with-output-file f2 (lambda (p) (display "Beta v1\n" p)))
                (registry-attach! reg "feat_one" (lambda () 1) #:metadata '((version . 1)))

                ;; --- 3.1 Checkpoint Creation & File Restoration ---
                (test-assert "rollback-restores-mutated-and-deleted-files"
                             (let ((chkpt (create-checkpoint! mgr "Initial State" #:files '("alpha.txt" "beta.txt"))))
                               (call-with-output-file f1 (lambda (p) (display "Alpha TAMPERED\n" p)))
                               (delete-file f2)
                               (let ((res (rollback-to-checkpoint! mgr (checkpoint-id chkpt))))
                                 (and (equal? (assoc-ref res 'status) "rolled_back")
                                      (equal? (call-with-input-file f1 read-line) "Alpha v1")
                                      (file-exists? f2)
                                      (equal? (call-with-input-file f2 read-line) "Beta v1")))))

                ;; --- 3.2 Full-scan Checkpoint Prunes Newly Created Files ---
                (test-assert "full-scan-rollback-deletes-new-files"
                             (let* ((chkpt (create-checkpoint! mgr "Before new files"))
                                    (new-f1 (string-append ws "/gamma.txt"))
                                    (new-f2 (string-append sub "/delta.txt")))
                               (call-with-output-file new-f1 (lambda (p) (display "Gamma\n" p)))
                               (call-with-output-file new-f2 (lambda (p) (display "Delta\n" p)))
                               (let ((res (rollback-to-checkpoint! mgr (checkpoint-id chkpt))))
                                 (and (equal? (assoc-ref res 'status) "rolled_back")
                                      (not (file-exists? new-f1))
                                      (not (file-exists? new-f2))))))

                ;; --- 3.3 Registry State Rollback: Detaches Uncommitted & Restores Prior ---
                (test-assert "rollback-restores-registry-state"
                             (let ((chkpt (create-checkpoint! mgr "Registry State 1")))
                               (registry-attach! reg "feat_one" (lambda () 100) #:metadata '((version . 100)))
                               (registry-attach! reg "feat_two" (lambda () 2) #:metadata '((version . 2)))
                               (registry-attach! reg "feat_three" (lambda () 3) #:metadata '((version . 3)))
                               (let ((res (rollback-to-checkpoint! mgr (checkpoint-id chkpt))))
                                 (and (equal? (assoc-ref res 'status) "rolled_back")
                                      (= (assoc-ref (registry-metadata reg "feat_one") 'version) 1)
                                      (not (registry-lookup reg "feat_two"))
                                      (not (registry-lookup reg "feat_three"))))))

                ;; --- 3.4 Multi-Stage Nested Checkpoints Time-Travel ---
                (test-assert "multi-stage-nested-checkpoints-time-travel"
                             (let* ((chkpt-a (create-checkpoint! mgr "State A" #:files '("alpha.txt")))
                                    (_ (call-with-output-file f1 (lambda (p) (display "Alpha v2\n" p))))
                                    (chkpt-b (create-checkpoint! mgr "State B" #:files '("alpha.txt")))
                                    (_ (call-with-output-file f1 (lambda (p) (display "Alpha v3\n" p))))
                                    (chkpt-c (create-checkpoint! mgr "State C" #:files '("alpha.txt"))))
                               (rollback-to-checkpoint! mgr (checkpoint-id chkpt-b))
                               (let ((v-b (call-with-input-file f1 read-line)))
                                 (rollback-to-checkpoint! mgr (checkpoint-id chkpt-a))
                                 (let ((v-a (call-with-input-file f1 read-line)))
                                   (rollback-to-checkpoint! mgr (checkpoint-id chkpt-c))
                                   (let ((v-c (call-with-input-file f1 read-line)))
                                     (and (equal? v-b "Alpha v2")
                                          (equal? v-a "Alpha v1")
                                          (equal? v-c "Alpha v3")))))))

                ;; --- 3.5 with-checkpoint Transactional Block Semantics ---
                (test-assert "with-checkpoint-macro-commit-and-rollback"
                             (let* ((ok-res (with-checkpoint mgr "Block success"
                                                             (lambda ()
                                                               (call-with-output-file f1 (lambda (p) (display "Alpha OK\n" p)))
                                                               "success-return")))
                                    (f1-after-ok (call-with-input-file f1 read-line))
                                    (fail-res (with-checkpoint mgr "Block returning #f"
                                                               (lambda ()
                                                                 (call-with-output-file f1 (lambda (p) (display "Alpha Corrupted\n" p)))
                                                                 #f)))
                                    (f1-after-fail (call-with-input-file f1 read-line))
                                    (err-res (with-checkpoint mgr "Block returning error alist"
                                                              (lambda ()
                                                                (call-with-output-file f1 (lambda (p) (display "Alpha Corrupted 2\n" p)))
                                                                '((status . "error") (error . "something failed")))))
                                    (f1-after-err (call-with-input-file f1 read-line))
                                    (caught? #f)
                                    (_ (catch 'test-abort
                                         (lambda ()
                                           (with-checkpoint mgr "Block throwing exception"
                                                            (lambda ()
                                                              (call-with-output-file f1 (lambda (p) (display "Alpha Crash\n" p)))
                                                              (throw 'test-abort "intentional exception"))))
                                         (lambda _ (set! caught? #t))))
                                    (f1-after-throw (call-with-input-file f1 read-line)))
                               (and (equal? ok-res "success-return")
                                    (equal? f1-after-ok "Alpha OK")
                                    (not fail-res)
                                    (equal? f1-after-fail "Alpha OK")
                                    (equal? (assoc-ref err-res 'status) "error")
                                    (equal? f1-after-err "Alpha OK")
                                    caught?
                                    (equal? f1-after-throw "Alpha OK"))))

                ;; --- 3.6 Disk Snapshot Persistence and Reload ---
                (test-assert "snapshot-disk-persistence-and-reload"
                             (let* ((chkpt (create-checkpoint! mgr "Persisted Checkpoint" #:files '("alpha.txt")))
                                    (_ (call-with-output-file f1 (lambda (p) (display "Alpha Dirty\n" p))))
                                    (new-mgr (make-rollback-manager #:workspace-dir ws
                                                                    #:storage-dir storage
                                                                    #:registry reg
                                                                    #:journal jrn))
                                    (loaded-chkpts (list-checkpoints new-mgr)))
                               (and (any (lambda (c) (equal? (checkpoint-id c) (checkpoint-id chkpt))) loaded-chkpts)
                                    (let ((res (rollback-to-checkpoint! new-mgr (checkpoint-id chkpt))))
                                      (and (equal? (assoc-ref res 'status) "rolled_back")
                                           (equal? (call-with-input-file f1 read-line) "Alpha OK")))))))

              (system* "rm" "-rf" ws)))

;;; =========================================================================
;;; 4. ADVERSARIAL PROMPT COMPOSER ((geasa runtimes-prompt plugin))
;;; =========================================================================
(test-group "challenger-prompt-composer"

            ;; --- 4.1 Non-git workspace handles missing git gracefully ---
            (test-assert "prompt-composer-non-git-dir"
                         (let* ((non-git-dir (mkdtemp "/tmp/badb-challenger-prompt-XXXXXX"))
                                (composer (make-prompt-composer
                                            #:include-git-status? #t
                                            #:include-file-tree? #t))
                                (prompt (assemble-system-prompt composer #:workspace-dir non-git-dir)))
                           (system* "rm" "-rf" non-git-dir)
                           (and (string? prompt)
                                (string-contains prompt "You are badb")
                                (not (string-null? prompt)))))

            ;; --- 4.2 File tree depth and max-files cap ---
            (test-assert "prompt-composer-file-tree-cap"
                         (let* ((ws (mkdtemp "/tmp/badb-challenger-tree-XXXXXX")))
                           (do ((i 0 (+ i 1)))
                               ((= i 20))
                             (call-with-output-file (format #f "~a/file_~a.txt" ws i)
                                                    (lambda (p) (display "x" p))))
                           (let* ((composer (make-prompt-composer
                                              #:include-git-status? #f
                                              #:include-file-tree? #t
                                              #:file-tree-max-files 5))
                                  (prompt (assemble-system-prompt composer #:workspace-dir ws)))
                             (system* "rm" "-rf" ws)
                             (and (string? prompt)
                                  (string-contains prompt "(additional files truncated)")))))

            ;; --- 4.3 Context limit configuration ---
            (test-assert "prompt-composer-context-limit-config"
                         (let* ((composer (make-prompt-composer
                                            #:custom-rules '("Rule 1" "Rule 2" "Rule 3")
                                            #:context-limit-chars 100))
                                (prompt (assemble-system-prompt composer)))
                           (and (= (prompt-composer-context-limit-chars composer) 100)
                                (string? prompt)
                                (string-contains prompt "Rule 1")))))

(test-end "adversarial-runtimes")
