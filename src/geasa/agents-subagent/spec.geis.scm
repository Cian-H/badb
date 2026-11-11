;;; (geasa agents-subagent spec.geis) — Contract verification for Sub-Agent Specialization & Worktree Transactions

(define-module (test test-agents-subagent)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (badb kernel registry)
               #:use-module (geasa providers-model common)
               #:use-module (geasa providers-model mock)
               #:use-module (geasa agents-subagent plugin)
               #:use-module (web response)
               #:use-module (json)
               #:use-module (ice-9 ftw)
               #:use-module (ice-9 textual-ports)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 rdelim)
               #:use-module (srfi srfi-1))

(spec-begin "agents-subagent")

(test-group "subagent-investigate-execution"
            (let* ((tmp-dir (mkdtemp "/tmp/badb-subagent-test-XXXXXX"))
                   (journal-dir (string-append tmp-dir "/subagent-journal"))
                   (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
                   (mock-poster
                     (lambda* (uri #:key body headers streaming?)
                              (let ((resp-body (scm->json-string
                                                 '((choices . #(((message . ((role . "assistant")
                                                                             (content . "Synthesized finding: Symbol X is defined in core.scm."))))))))))
                                (values (build-response #:code 200) resp-body))))
                   (reg (make-registry))
                   (_ (registry-attach! reg "read_file" (lambda (p) "file content") '((description . "read"))))
                   (_ (registry-attach! reg "dangerous_delete" (lambda (p) "deleted") '((description . "write")))))

              (parameterize ((current-http-post mock-poster))
                            (register-subagent-tools! reg #:client client #:journal-dir journal-dir)

                            (let ((investigate-fn (registry-lookup reg "investigate")))
                              (test-assert "investigate tool is attached" (procedure? investigate-fn))

                              (let ((result (investigate-fn `((prompt . "Inspect Symbol X")))))
                                (test-assert "returns synthesized answer string" (string? result))
                                (test-assert "answer contains findings"
                                             (string-contains result "Symbol X is defined in core.scm"))

                                ;; Verify trajectory was written to journal-dir
                                (test-assert "journal directory exists" (file-exists? journal-dir))
                                (let ((files (or (scandir journal-dir (lambda (n) (string-suffix? ".jsonl" n))) '())))
                                  (test-assert "trajectory jsonl file created" (pair? files))
                                  (let* ((traj-path (string-append journal-dir "/" (car files)))
                                         (content (call-with-input-file traj-path get-string-all)))
                                    (test-assert "trajectory contains user request" (string-contains content "Inspect Symbol X"))
                                    (test-assert "trajectory contains assistant response" (string-contains content "Symbol X is defined")))))))
              (system* "rm" "-rf" tmp-dir)))

(test-group "subagent-parallel-execution"
            (let* ((tmp-dir (mkdtemp "/tmp/badb-subagent-par-XXXXXX"))
                   (journal-dir (string-append tmp-dir "/subagent-journal"))
                   (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
                   (mock-poster
                     (lambda* (uri #:key body headers streaming?)
                              (let ((resp-body (scm->json-string
                                                 '((choices . #(((message . ((role . "assistant")
                                                                             (content . "Answer for query."))))))))))
                                (values (build-response #:code 200) resp-body))))
                   (reg (make-registry)))

              (parameterize ((current-http-post mock-poster))
                            (register-subagent-tools! reg #:client client #:journal-dir journal-dir)
                            (let ((par-fn (registry-lookup reg "investigate_parallel")))
                              (test-assert "investigate_parallel tool is attached" (procedure? par-fn))

                              (let ((result (par-fn `((queries . #("What is A?" "What is B?"))))))
                                (test-assert "contains Query A" (string-contains result "What is A?"))
                                (test-assert "contains Query B" (string-contains result "What is B?"))
                                (test-assert "contains Answer" (string-contains result "Answer for query.")))))
              (system* "rm" "-rf" tmp-dir)))

(test-group "subagent-worktree-lifecycle"
            (let* ((tmp-repo (mkdtemp "/tmp/badb-git-repo-XXXXXX"))
                   ;; Initialize git repo
                   (_ (system* "git" "-C" tmp-repo "init"))
                   (_ (system* "git" "-C" tmp-repo "config" "user.name" "Test User"))
                   (_ (system* "git" "-C" tmp-repo "config" "user.email" "test@example.com"))
                   (_ (call-with-output-file (string-append tmp-repo "/README.md")
                                             (lambda (p) (display "# Test Repo\n" p))))
                   (_ (system* "git" "-C" tmp-repo "add" "README.md"))
                   (_ (system* "git" "-C" tmp-repo "commit" "-m" "Initial commit"))
                   (sw (create-subagent-worktree #:repo tmp-repo #:base "HEAD")))

              (test-assert "subagent-worktree record created" (subagent-worktree? sw))
              (test-assert "subagent-worktree path exists" (file-exists? (subagent-worktree-path sw)))
              (test-assert "subagent-worktree is git" (subagent-worktree-is-git? sw))
              (test-assert "subagent-worktree status is active" (eq? (subagent-worktree-status sw) 'active))
              (test-assert "README.md exists in worktree" (file-exists? (string-append (subagent-worktree-path sw) "/README.md")))

              ;; Test cleanup
              (let ((cleaned? (cleanup-subagent-worktree! sw)))
                (test-assert "cleanup returns true" cleaned?)
                (test-assert "worktree path no longer exists" (not (file-exists? (subagent-worktree-path sw))))
                (test-assert "status is cleaned" (eq? (subagent-worktree-status sw) 'cleaned)))

              ;; Test call-with-subagent-worktree
              (let ((inside-path #f))
                (call-with-subagent-worktree
                  (lambda (worktree)
                    (set! inside-path (subagent-worktree-path worktree))
                    (test-assert "worktree active inside closure" (file-exists? inside-path)))
                  #:repo tmp-repo)
                (test-assert "worktree cleaned after closure" (not (file-exists? inside-path))))

              (system* "rm" "-rf" tmp-repo)))

(test-group
  "subagent-isolated-registry-filesystem"
  (let*
      ((tmp-repo (mkdtemp "/tmp/badb-iso-reg-XXXXXX"))
       (_ (system* "git" "-C" tmp-repo "init"))
       (_ (system* "git" "-C" tmp-repo "config" "user.name" "Test User"))
       (_ (system* "git" "-C" tmp-repo "config" "user.email" "test@example.com"))
       (_
         (call-with-output-file
           (string-append tmp-repo "/base.txt")
           (lambda (p)
             (display "base content\n" p))))
       (_ (system* "git" "-C" tmp-repo "add" "base.txt"))
       (_ (system* "git" "-C" tmp-repo "commit" "-m" "Initial commit"))
       (sw (create-subagent-worktree #:repo tmp-repo))
       (wt-path (subagent-worktree-path sw))
       (sub-reg
         (make-isolated-subagent-registry (make-registry) wt-path #:parent-repo tmp-repo)))
    (test-assert "isolated registry has read_file"
                 (procedure? (registry-lookup sub-reg "read_file")))
    (test-assert "isolated registry has write_file"
                 (procedure? (registry-lookup sub-reg "write_file")))
    (test-assert "isolated registry has edit_file"
                 (procedure? (registry-lookup sub-reg "edit_file")))
    (test-assert "isolated registry has bash"
                 (procedure? (registry-lookup sub-reg "bash")))
    ;; Write file in worktree
    (let
        ((write-fn (registry-lookup sub-reg "write_file")))
      (write-fn "subfolder/hello.txt" "Hello from worktree!\nLine 2\n")
      (test-assert "written file exists in worktree"
                   (file-exists? (string-append wt-path "/subfolder/hello.txt")))
      (test-assert
        "written file does NOT exist in parent repo"
        (not (file-exists? (string-append tmp-repo "/subfolder/hello.txt")))))
    ;; Read file in worktree
    (let*
        ((read-fn (registry-lookup sub-reg "read_file"))
         (content (read-fn "subfolder/hello.txt" 0 10)))
      (test-assert "read_file returns content"
                   (string-contains content "Hello from worktree!")))
    ;; Edit file in worktree
    (let
        ((edit-fn (registry-lookup sub-reg "edit_file")))
      (edit-fn "subfolder/hello.txt" "Hello" "Greetings" #f)
      (let*
          ((read-fn (registry-lookup sub-reg "read_file"))
           (content (read-fn "subfolder/hello.txt" 0 10)))
        (test-assert "edit_file modified content"
                     (string-contains content "Greetings from worktree!"))))
    ;; Shell tool in worktree
    (let*
        ((bash-fn (registry-lookup sub-reg "bash")) (out (bash-fn "pwd")))
      (test-assert "bash runs in worktree directory" (string-contains out wt-path)))
    (cleanup-subagent-worktree! sw)
    (system* "rm" "-rf" tmp-repo)))

(test-group "subagent-worktree-verify-and-merge"
            (let* ((tmp-repo (mkdtemp "/tmp/badb-v-merge-XXXXXX"))
                   (_ (system* "git" "-C" tmp-repo "init"))
                   (_ (system* "git" "-C" tmp-repo "config" "user.name" "Test User"))
                   (_ (system* "git" "-C" tmp-repo "config" "user.email" "test@example.com"))
                   (_ (call-with-output-file (string-append tmp-repo "/initial.txt")
                                             (lambda (p) (display "Initial\n" p))))
                   (_ (system* "git" "-C" tmp-repo "add" "initial.txt"))
                   (_ (system* "git" "-C" tmp-repo "commit" "-m" "Initial commit"))
                   (sw (create-subagent-worktree #:repo tmp-repo))
                   (wt-path (subagent-worktree-path sw)))

              ;; Add a file and commit in worktree
              (call-with-output-file (string-append wt-path "/new-feature.txt")
                                     (lambda (p) (display "Feature code\n" p)))
              (system* "git" "-C" wt-path "add" "new-feature.txt")
              (system* "git" "-C" wt-path "commit" "-m" "Add new feature")

              ;; Verify with passing command
              (let ((v-res (subagent-worktree-verify sw #:command "true")))
                (test-assert "verify passes with exit 0" (assoc-ref v-res 'passed?))
                (test-assert "worktree status is verified" (eq? (subagent-worktree-status sw) 'verified)))

              ;; Merge into parent repo
              (let ((m-res (subagent-worktree-merge! sw #:verify-first? #t)))
                (test-assert "merge succeeds" (assoc-ref m-res 'merged?))
                (test-assert "parent repo now contains new feature"
                             (file-exists? (string-append tmp-repo "/new-feature.txt"))))

              (cleanup-subagent-worktree! sw)
              (system* "rm" "-rf" tmp-repo)))

(test-group "subagent-transaction-execution"
            (let* ((tmp-repo (mkdtemp "/tmp/badb-txn-exec-XXXXXX"))
                   (journal-dir (string-append tmp-repo "/journal"))
                   (_ (system* "git" "-C" tmp-repo "init"))
                   (_ (system* "git" "-C" tmp-repo "config" "user.name" "Test User"))
                   (_ (system* "git" "-C" tmp-repo "config" "user.email" "test@example.com"))
                   (_ (call-with-output-file (string-append tmp-repo "/file.txt")
                                             (lambda (p) (display "hello\n" p))))
                   (_ (system* "git" "-C" tmp-repo "add" "file.txt"))
                   (_ (system* "git" "-C" tmp-repo "commit" "-m" "Initial commit"))
                   (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
                   (mock-poster
                     (lambda* (uri #:key body headers streaming?)
                              (let ((resp-body (scm->json-string
                                                 '((choices . #(((message . ((role . "assistant")
                                                                             (content . "Task accomplished in worktree."))))))))))
                                (values (build-response #:code 200) resp-body))))
                   (reg (make-registry)))

              (parameterize ((current-http-post mock-poster))
                            (register-subagent-tools! reg #:client client #:journal-dir journal-dir)

                            (let ((txn-tool (registry-lookup reg "subagent_transaction")))
                              (test-assert "subagent_transaction tool is registered" (procedure? txn-tool))

                              ;; Execute subagent transaction directly
                              (let ((res (run-subagent-worktree "Add feature X"
                                                                #:client client
                                                                #:parent-reg reg
                                                                #:repo tmp-repo
                                                                #:verify? #f
                                                                #:auto-merge? #f
                                                                #:keep-worktree? #f
                                                                #:journal-dir journal-dir)))
                                (test-assert "result has sub-id" (string? (assoc-ref res 'sub-id)))
                                (test-assert "result has content" (string-contains (assoc-ref res 'content) "Task accomplished"))
                                (test-assert "trajectory was written" (file-exists? journal-dir)))))

              (system* "rm" "-rf" tmp-repo)))

(spec-end)
(spec-exit)
