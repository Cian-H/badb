;;; (geasa agents-subagent spec.geis) — Contract verification for Sub-Agent Specialization & Worktree Transactions

(define-module (test test-agents-subagent)
               #:use-module
               (srfi srfi-64)
               #:use-module
               (badb spec)
               #:use-module
               (badb kernel registry)
               #:use-module
               (geasa providers-model common)
               #:use-module
               (geasa providers-model mock)
               #:use-module
               (geasa surfaces-acp client)
               #:use-module
               (geasa agents-subagent plugin)
               #:use-module
               (web response)
               #:use-module
               (json)
               #:use-module
               (ice-9 ftw)
               #:use-module
               (ice-9 textual-ports)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 threads)
               #:use-module
               (srfi srfi-1))

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

(test-group
  "acp-separated-client-server-and-handshake"
  (let*
      ((tmp-dir (mkdtemp "/tmp/badb-sep-cs-XXXXXX"))
       (client
         (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
       (mock-poster
         (lambda* (uri #:key body headers streaming?)
                  (values (build-response #:code 200)
                          (scm->json-string
                            '((choices .
                               #(((message . ((role . "assistant") (content . "Separated client/server ok."))))))))))))
    (parameterize ((current-http-post mock-poster))
                  (call-with-values create-agent-pipe-pair
                                    (lambda (srv-in srv-out cli-in cli-out)
                                      (let*
                                          ((srv
                                             (create-agent-server #:in-port
                                                                  srv-in
                                                                  #:out-port
                                                                  srv-out
                                                                  #:client
                                                                  client
                                                                  #:workspace
                                                                  tmp-dir))
                                           (acp-mod (resolve-module '(geasa surfaces-acp plugin)))
                                           (serve-fn (module-ref acp-mod 'acp-serve-stdio))
                                           (thr
                                             (call-with-new-thread
                                               (lambda ()
                                                 (catch #t
                                                   (lambda ()
                                                     (serve-fn srv))
                                                   (lambda _
                                                     #f)))))
                                           (cli (create-agent-client #:in-port cli-in #:out-port cli-out)))
                                        (let ((sid (agent-handshake! cli #:cwd tmp-dir)))
                                          (test-assert "handshake initialized client" (acp-client-initialized? cli))
                                          (test-assert "handshake created sessionId" (string? sid))
                                          (let
                                              ((res (acp-client-prompt! cli "Verify handshake")))
                                            (test-equal "stopReason is end_turn" "end_turn" (assoc-ref res 'stop-reason))
                                            (test-assert "response text delivered"
                                                         (string-contains (assoc-ref res 'content) "Separated client/server ok."))))
                                        (acp-client-close! cli)
                                        (join-thread thr (+ (car (gettimeofday)) 2))
                                        (close-port srv-in)
                                        (close-port srv-out)))))
    (system* "rm" "-rf" tmp-dir)))

(test-group "local-subagent-full-badb-instance"
            (let*
                ((tmp-dir (mkdtemp "/tmp/badb-lsub-XXXXXX"))
                 (client
                   (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
                 (turn 0)
                 (perm-asked? #f)
                 (tool-args-json
                   (scm->json-string '((path . "sub-out.txt") (content . "local-badb"))))
                 (mock-poster
                   (lambda* (uri #:key body headers streaming?)
                            (set! turn (+ turn 1))
                            (let
                                ((resp
                                   (if (= turn 1)
                                     `((choices .
                                                #(((message .
                                                            ((role . "assistant") (content . "")
                                                             (tool_calls .
                                                                         #(((id . "tc_1") (type . "function")
                                                                            (function . ((name . "write_file") (arguments . ,tool-args-json))))))))))))
                                     '((choices .
                                        #(((message . ((role . "assistant") (content . "Local badb completed."))))))))))
                              (values (build-response #:code 200) (scm->json-string resp)))))
                 (agent
                   (make-local-subagent #:workspace
                                        tmp-dir
                                        #:model-client
                                        client
                                        #:permission-policy
                                        'ask
                                        #:permission-handler
                                        (lambda (_params)
                                          (set! perm-asked? #t)
                                          'allow))))
              (test-assert "local-subagent? true" (local-subagent? agent))
              (test-assert "subagent? true" (subagent? agent))
              (test-equal "initial status is created" 'created (subagent-status agent))
              (parameterize ((current-http-post mock-poster))
                            (let
                                ((res
                                   (call-with-subagent agent
                                                       (lambda (ag)
                                                         (test-equal "running status" 'running (subagent-status ag))
                                                         (test-assert
                                                           "both client and server attached locally"
                                                           (and (acp-client? (local-subagent-client ag)) (local-subagent-server ag)))
                                                         (test-assert "session-id active" (string? (subagent-session-id ag)))
                                                         (subagent-prompt! ag "Write sub-out.txt")))))
                              (test-assert "permission gate fired over ACP" perm-asked?)
                              (test-assert "file written in subagent workspace"
                                           (file-exists? (string-append tmp-dir "/sub-out.txt")))
                              (test-assert "final assistant content returned"
                                           (string-contains (assoc-ref res 'content) "Local badb completed."))))
              (test-equal "stopped after call-with-subagent" 'stopped (subagent-status agent))
              (test-assert "stop is idempotent" (local-subagent-stop! agent))
              (system* "rm" "-rf" tmp-dir)))

(test-group
  "remote-superagent-and-remote-subagent-tcp"
  (let*
      ((tmp-dir (mkdtemp "/tmp/badb-rsub-tcp-XXXXXX"))
       (client
         (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
       (mock-poster
         (lambda* (uri #:key body headers streaming?)
                  (values (build-response #:code 200)
                          (scm->json-string
                            '((choices .
                               #(((message . ((role . "assistant") (content . "TCP remote response.")))))))))))
       (sup
         (make-remote-superagent #:transport
                                 'tcp
                                 #:host
                                 "127.0.0.1"
                                 #:port
                                 0
                                 #:workspace
                                 tmp-dir
                                 #:model-client
                                 client)))
    (test-assert "remote-superagent? true" (remote-superagent? sup))
    (test-equal "superagent starts in created status"
                'created
                (subagent-status sup))
    (parameterize ((current-http-post mock-poster))
                  (remote-superagent-start! sup)
                  (test-equal "superagent status running" 'running (subagent-status sup))
                  (test-assert "ephemeral tcp port assigned" (> (remote-superagent-port sup) 0))
                  (let*
                      ((ep (remote-superagent-endpoint sup))
                       (sub
                         (make-remote-subagent/tcp (assoc-ref ep 'host)
                                                   (assoc-ref ep 'port)
                                                   #:workspace
                                                   tmp-dir)))
                    (test-assert "remote-subagent? true" (remote-subagent? sub))
                    (let
                        ((res
                           (call-with-subagent sub
                                               (lambda (ag)
                                                 (test-equal "remote subagent running" 'running (subagent-status ag))
                                                 (test-assert "remote subagent has client" (acp-client? (subagent-client ag)))
                                                 (subagent-prompt! ag "Hello over TCP")))))
                      (test-equal "tcp stop-reason end_turn" "end_turn" (assoc-ref res 'stop-reason))
                      (test-assert "tcp subagent received response"
                                   (string-contains (assoc-ref res 'content) "TCP remote response.")))
                    (test-equal "remote subagent stopped" 'stopped (subagent-status sub)))
                  (remote-superagent-stop! sup)
                  (test-equal "superagent status stopped" 'stopped (subagent-status sup))
                  (test-assert "superagent stop is idempotent" (remote-superagent-stop! sup)))
    (system* "rm" "-rf" tmp-dir)))

(test-group
  "remote-superagent-and-remote-subagent-unix"
  (let*
      ((tmp-dir (mkdtemp "/tmp/badb-rsub-unix-XXXXXX"))
       (sock-path (string-append tmp-dir "/badb-acp.sock"))
       (client
         (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
       (mock-poster
         (lambda* (uri #:key body headers streaming?)
                  (values (build-response #:code 200)
                          (scm->json-string
                            '((choices .
                               #(((message . ((role . "assistant") (content . "Unix remote response.")))))))))))
       (sup
         (make-remote-superagent #:transport
                                 'unix
                                 #:socket-path
                                 sock-path
                                 #:workspace
                                 tmp-dir
                                 #:model-client
                                 client)))
    (parameterize ((current-http-post mock-poster))
                  (remote-superagent-start! sup)
                  (test-assert "unix socket file created" (file-exists? sock-path))
                  (let*
                      ((ep (remote-superagent-endpoint sup))
                       (sub
                         (make-remote-subagent/unix (assoc-ref ep 'socket-path) #:workspace tmp-dir)))
                    (let
                        ((res
                           (call-with-subagent sub
                                               (lambda (ag)
                                                 (subagent-prompt! ag "Hello unix")))))
                      (test-assert "unix subagent received response"
                                   (string-contains (assoc-ref res 'content) "Unix remote response.")))
                    (test-equal "unix subagent stopped" 'stopped (subagent-status sub)))
                  (remote-superagent-stop! sup)
                  (test-assert "unix socket cleaned up on stop" (not (file-exists? sock-path))))
    (system* "rm" "-rf" tmp-dir)))

(test-group "remote-subagent-spawn-subprocess"
            (let*
                ((tmp-dir (mkdtemp "/tmp/badb-rsub-spawn-XXXXXX"))
                 (script (string-append tmp-dir "/echo-acp.scm"))
                 (agent #f))
              (call-with-output-file script
                                     (lambda (p)
                                       (display
                                         "(use-modules (ice-9 rdelim) (json))\n(let loop ()\n  (let ((line (read-line)))\n    (unless (eof-object? line)\n      (let* ((msg (json-string->scm line))\n             (id (assoc-ref msg \"id\"))\n             (method (assoc-ref msg \"method\")))\n        (cond\n          ((equal? method \"initialize\")\n           (display (scm->json-string `((jsonrpc . \"2.0\") (id . ,id) (result . ((protocolVersion . 1) (agentCapabilities . ()) (agentInfo . ((name . \"mock-acp\") (version . \"0.1.0\"))))))))\n           (newline) (force-output))\n          ((equal? method \"session/new\")\n           (display (scm->json-string `((jsonrpc . \"2.0\") (id . ,id) (result . ((sessionId . \"spawn_sess_1\"))))))\n           (newline) (force-output))\n          ((equal? method \"session/prompt\")\n           (display (scm->json-string `((jsonrpc . \"2.0\") (method . \"session/update\") (params . ((sessionId . \"spawn_sess_1\") (update . ((sessionUpdate . \"agent_message_chunk\") (content . ((type . \"text\") (text . \"spawn-ok\"))))))))))\n           (newline) (force-output)\n           (display (scm->json-string `((jsonrpc . \"2.0\") (id . ,id) (result . ((stopReason . \"end_turn\"))))))\n           (newline) (force-output)))\n        (loop)))))\n"
                                         p)))
              (set! agent
                    (make-remote-subagent/spawn (list "guile" script) #:workspace tmp-dir))
              (test-assert "remote-subagent? true" (remote-subagent? agent))
              (let
                  ((res
                     (call-with-subagent agent
                                         (lambda (ag)
                                           (test-equal "session id from spawned handshake"
                                                       "spawn_sess_1"
                                                       (subagent-session-id ag))
                                           (subagent-prompt! ag "Ping spawned ACP")))))
                (test-equal "spawned prompt stopReason" "end_turn" (assoc-ref res 'stop-reason))
                (test-equal "spawned prompt content" "spawn-ok" (assoc-ref res 'content)))
              (test-equal "spawned subagent stopped" 'stopped (subagent-status agent))
              (system* "rm" "-rf" tmp-dir)))

(spec-end)
(spec-exit)
