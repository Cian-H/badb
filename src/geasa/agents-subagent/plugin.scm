;;; (geasa agents-subagent plugin) — Sub-Agent Task Specialization & Worktree Transactions
;;;
;;; Spawns isolated, disposable ReAct sub-agents with filtered read-only tool
;;; permissions to investigate code, search indexes, or run parallel queries.
;;; Trajectories are offloaded to disk, returning only the synthesized answer
;;; to eliminate context token waste.
;;;
;;; Worktree-Isolated Subagent Transactions:
;;; Binds subagents to ephemeral Git worktrees or isolated sandboxes.
;;; Virtualizes filesystem and git tools to target the subagent's worktree sandbox.
;;; Supports verification before merge and atomic merge back to parent checkout.

(define-module (geasa agents-subagent plugin)
               #:use-module
               (badb kernel registry)
               #:use-module
               (geasa surfaces-acp client)
               #:use-module
               (geasa runtimes-react plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (geasa capabilities-fs-tools plugin)
               #:use-module
               (geasa runtimes-verify plugin)
               #:use-module
               (json)
               #:use-module
               (ice-9 threads)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 ftw)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:use-module
               (srfi srfi-13)
               #:use-module
               (uuid generate)
               #:use-module
               (uuid utils)
               #:export
               (<subagent-worktree> make-subagent-worktree
                                    subagent-worktree?
                                    subagent-worktree-id
                                    subagent-worktree-path
                                    subagent-worktree-branch
                                    subagent-worktree-base
                                    subagent-worktree-repo
                                    subagent-worktree-status
                                    subagent-worktree-is-git?
                                    set-subagent-worktree-status!
                                    create-subagent-worktree
                                    cleanup-subagent-worktree!
                                    call-with-subagent-worktree
                                    make-isolated-subagent-registry
                                    subagent-worktree-verify
                                    subagent-worktree-merge!
                                    create-agent-pipe-pair
                                    create-agent-server
                                    create-agent-client
                                    agent-handshake!
                                    <local-subagent>
                                    make-local-subagent
                                    local-subagent?
                                    local-subagent-id
                                    local-subagent-workspace
                                    local-subagent-client
                                    local-subagent-server
                                    local-subagent-status
                                    local-subagent-session-id
                                    local-subagent-start!
                                    local-subagent-prompt!
                                    local-subagent-cancel!
                                    local-subagent-stop!
                                    <remote-subagent>
                                    make-remote-subagent
                                    make-remote-subagent/spawn
                                    make-remote-subagent/tcp
                                    make-remote-subagent/unix
                                    make-remote-subagent/ports
                                    remote-subagent?
                                    remote-subagent-id
                                    remote-subagent-transport
                                    remote-subagent-workspace
                                    remote-subagent-client
                                    remote-subagent-status
                                    remote-subagent-session-id
                                    remote-subagent-start!
                                    remote-subagent-prompt!
                                    remote-subagent-cancel!
                                    remote-subagent-stop!
                                    <remote-superagent>
                                    make-remote-superagent
                                    remote-superagent?
                                    remote-superagent-id
                                    remote-superagent-transport
                                    remote-superagent-host
                                    remote-superagent-port
                                    remote-superagent-socket-path
                                    remote-superagent-workspace
                                    remote-superagent-status
                                    remote-superagent-endpoint
                                    remote-superagent-start!
                                    remote-superagent-stop!
                                    remote-superagent-wait
                                    subagent?
                                    subagent-start!
                                    subagent-prompt!
                                    subagent-cancel!
                                    subagent-stop!
                                    subagent-status
                                    subagent-session-id
                                    subagent-client
                                    call-with-subagent
                                    run-agent-transaction
                                    run-subagent-worktree
                                    run-subagent
                                    run-subagents-parallel
                                    register-subagent-tools!
                                    load-wt-premerge-command
                                    %default-investigate-tools
                                    %default-worktree-tools
                                    %investigate-tool-schema
                                    %investigate-parallel-tool-schema
                                    %subagent-transaction-tool-schema))

(define %default-investigate-tools
        '("read_file" "fs-read-file" "ctx_search" "find_symbol" "symbols_overview"
          "skill_load" "skill_list"))

(define %default-worktree-tools
        '("read_file" "write_file"
          "edit_file"
          "edit_lines"
          "bash"
          "sh"
          "git_status"
          "git_diff"
          "git_log"
          "git_commit"
          "ctx_search"
          "find_symbol"
          "symbols_overview"
          "skill_load"
          "skill_list"))

;; ---- Subprocess & Git Plumbing ----------------------------------------------

(define (%shell-quote s)
        "Quote string S as a single POSIX shell word."
        (string-append "'"
                       (list->string
                         (fold-right append '()
                                     (map (lambda (c)
                                            (if (eqv? c #\')
                                              (list #\' #\\ #\' #\')
                                              (list c)))
                                          (string->list (if (string? s) s (format #f "~a" s))))))
                       "'"))

(define (%run-cmd cmd)
        "Run shell command CMD capturing stdout/stderr and exit status."
        (catch #t
          (lambda ()
            (let* ((pipe (open-input-pipe (string-append cmd " 2>&1")))
                   (out (let loop ((acc '()))
                                  (let ((line (read-line pipe)))
                                    (if (eof-object? line)
                                      (string-join (reverse acc) "\n")
                                      (loop (cons line acc))))))
                   (status (close-pipe pipe)))
              (cons (zero? (status:exit-val status)) out)))
          (lambda (k . a)
            (cons #f (format #f "exec error (~a): ~s" k a)))))

(define (%git repo . args)
        "Run git command ARGS inside REPO."
        (let ((cmd (string-append "git -C " (%shell-quote repo) " "
                                  (string-join
                                    (map %shell-quote
                                         (filter (lambda (a) (and (string? a) (not (string-null? a))))
                                                 args))
                                    " "))))
          (%run-cmd cmd)))

(define (%is-git-repo? dir)
        (let ((res (%git dir "rev-parse" "--is-inside-work-tree")))
          (and (car res) (string-contains (cdr res) "true"))))

;; ---- <subagent-worktree> Record & Lifecycle ---------------------------------

(define-record-type <subagent-worktree>
                    (%make-subagent-worktree id path branch base repo status is-git?)
                    subagent-worktree?
                    (id subagent-worktree-id)
                    (path subagent-worktree-path)
                    (branch subagent-worktree-branch)
                    (base subagent-worktree-base)
                    (repo subagent-worktree-repo)
                    (status subagent-worktree-status set-subagent-worktree-status!)
                    (is-git? subagent-worktree-is-git?))

(define* (make-subagent-worktree #:key
                                 (id (uuid->string (generate-uuid 'uuidv4)))
                                 (path #f)
                                 (branch #f)
                                 (base "HEAD")
                                 (repo (getcwd))
                                 (status 'active)
                                 (is-git? #t))
         (%make-subagent-worktree id path branch base repo status is-git?))

(define* (create-subagent-worktree #:key
                                   (repo (getcwd))
                                   (base "HEAD")
                                   (id #f)
                                   (branch-prefix "badb-subagent-")
                                   (path-prefix #f))
         "Create an ephemeral Git worktree or sandbox directory for a subagent transaction.
Returns a <subagent-worktree> record."
         (let* ((clean-repo (canonicalize-path repo))
                (sub-id (or id (substring (uuid->string (generate-uuid 'uuidv4)) 0 8)))
                (branch (string-append branch-prefix sub-id))
                (wt-path (or path-prefix
                             (string-append clean-repo "/.badb/worktrees/" sub-id)))
                (is-git? (%is-git-repo? clean-repo)))
           (if is-git?
             (begin
               (let ((parent-dir (dirname wt-path)))
                 (unless (file-exists? parent-dir)
                   (mkdir-p parent-dir)))
               (when (file-exists? wt-path)
                 (catch #t
                   (lambda () (%git clean-repo "worktree" "remove" "--force" wt-path))
                   (lambda _ (system* "rm" "-rf" wt-path))))
               (let ((res (%git clean-repo "worktree" "add" "-b" branch wt-path base)))
                 (unless (car res)
                   (%git clean-repo "worktree" "prune")
                   (let ((retry-res (%git clean-repo "worktree" "add" "-B" branch wt-path base)))
                     (unless (car retry-res)
                       (error (format #f "create-subagent-worktree: failed to add worktree: ~a" (cdr retry-res)))))))
               (make-subagent-worktree #:id sub-id
                                       #:path (canonicalize-path wt-path)
                                       #:branch branch
                                       #:base base
                                       #:repo clean-repo
                                       #:status 'active
                                       #:is-git? #t))
             (begin
               (mkdir-p wt-path)
               (make-subagent-worktree #:id sub-id
                                       #:path (canonicalize-path wt-path)
                                       #:branch branch
                                       #:base base
                                       #:repo clean-repo
                                       #:status 'active
                                       #:is-git? #f)))))

(define* (cleanup-subagent-worktree! sw #:key (delete-branch? #t) (force? #t))
         "Remove ephemeral worktree SW and optionally delete its branch."
         (let ((repo (subagent-worktree-repo sw))
               (path (subagent-worktree-path sw))
               (branch (subagent-worktree-branch sw))
               (is-git? (subagent-worktree-is-git? sw)))
           (when (and path (file-exists? path))
             (if is-git?
               (begin
                 (%git repo "worktree" "remove" (if force? "--force" "") path)
                 (%git repo "worktree" "prune")
                 (when (file-exists? path)
                   (system* "rm" "-rf" path)))
               (system* "rm" "-rf" path)))
           (when (and is-git? delete-branch? branch)
             (catch #t
               (lambda () (%git repo "branch" "-D" branch))
               (lambda _ #f)))
           (set-subagent-worktree-status! sw 'cleaned)
           #t))

(define* (call-with-subagent-worktree proc
                                      #:key
                                      (repo (getcwd))
                                      (base "HEAD")
                                      (cleanup? #t)
                                      (delete-branch? #t))
         "Execute PROC with a fresh <subagent-worktree>, ensuring cleanup on exit."
         (let ((sw (create-subagent-worktree #:repo repo #:base base)))
           (dynamic-wind
             (lambda () #f)
             (lambda () (proc sw))
             (lambda ()
               (when cleanup?
                 (cleanup-subagent-worktree! sw #:delete-branch? delete-branch?))))))

;; ---- Config & Verification Helpers ------------------------------------------

(define (load-wt-premerge-command repo)
        "Extract pre-merge test suite command from .config/wt.toml if present."
        (let ((wt-file (string-append repo "/.config/wt.toml")))
          (if (file-exists? wt-file)
            (catch #t
              (lambda ()
                (call-with-input-file wt-file
                                      (lambda (p)
                                        (let loop ((in-premerge? #f))
                                                  (let ((line (read-line p)))
                                                    (cond
                                                      ((eof-object? line) #f)
                                                      ((string-prefix? "[pre-merge]" (string-trim-both line))
                                                        (loop #t))
                                                      ((and in-premerge? (string-prefix? "[" (string-trim-both line)))
                                                        #f)
                                                      ((and in-premerge? (string-contains line "suite"))
                                                        (let ((eq-pos (string-index line #\=)))
                                                          (if eq-pos
                                                            (string-trim-both
                                                              (substring line (+ eq-pos 1))
                                                              (char-set #\space #\tab #\" #\'))
                                                            (loop in-premerge?))))
                                                      (else (loop in-premerge?))))))))
              (lambda _ #f))
            #f)))

(define* (subagent-worktree-verify sw #:key (command #f) (verifier #f))
         "Verify test suite or contracts inside SW's worktree.
Returns '((passed? . #t/#f) (exit-code . code) (output . output))."
         (let* ((wt-path (subagent-worktree-path sw))
                (repo (subagent-worktree-repo sw))
                (cmd (or command
                         (and verifier (verifier-test-command verifier))
                         (load-wt-premerge-command repo)
                         "./scripts/test")))
           (let ((res (verify-test-suite #:command cmd #:dir wt-path)))
             (if (assoc-ref res 'passed?)
               (begin
                 (set-subagent-worktree-status! sw 'verified)
                 res)
               res))))

(define* (subagent-worktree-merge! sw #:key (target-branch #f) (verify-first? #t) (verifier #f) (command #f))
         "Merge SW's branch into the current (or TARGET-BRANCH) checkout after optional verification.
Returns '((merged? . #t/#f) (reason . reason) ...)."
         (let* ((repo (subagent-worktree-repo sw))
                (branch (subagent-worktree-branch sw))
                (status (subagent-worktree-status sw))
                (is-git? (subagent-worktree-is-git? sw)))
           (if is-git?
             (if (and verify-first? (not (eq? status 'verified)))
               (let ((v-res (subagent-worktree-verify sw #:verifier verifier #:command command)))
                 (if (assoc-ref v-res 'passed?)
                   (%execute-worktree-merge repo branch target-branch sw)
                   `((merged? . #f)
                     (reason . "verification-failed")
                     (verification . ,v-res))))
               (%execute-worktree-merge repo branch target-branch sw))
             '((merged? . #f) (reason . "not-a-git-repo")))))

(define (%execute-worktree-merge repo branch target-branch sw)
        (let* ((msg (format #f "Merge subagent transaction branch ~a" branch))
               (res (%git repo "merge" "--no-ff" "-m" msg branch)))
          (if (car res)
            (begin
              (set-subagent-worktree-status! sw 'merged)
              `((merged? . #t)
                (branch . ,branch)
                (output . ,(cdr res))))
            (begin
              (%git repo "merge" "--abort")
              `((merged? . #f)
                (reason . "merge-conflict")
                (output . ,(cdr res)))))))

;; ---- Worktree-Scoped Isolated Registry --------------------------------------

(define* (make-isolated-subagent-registry parent-reg worktree-path
                                          #:key (allowed-tools #f) (parent-repo #f))
         "Create an isolated registry whose filesystem and git tools are sandboxed inside WORKTREE-PATH."
         (let* ((sub-reg (make-registry))
                (parent-repo-norm (and parent-repo (file-exists? parent-repo) (canonicalize-path parent-repo)))
                (wt-path-norm (canonicalize-path worktree-path))
                (resolve-path
                  (lambda (p)
                    (cond
                      ((not (string? p)) p)
                      ((and parent-repo-norm (string-prefix? parent-repo-norm p))
                        (let ((rel (substring p (string-length parent-repo-norm))))
                          (string-append wt-path-norm "/" (string-trim rel (char-set #\/)))))
                      ((string-prefix? wt-path-norm p) p)
                      ((string-prefix? "/" p)
                        (string-append wt-path-norm "/" (string-trim p (char-set #\/))))
                      (else
                        (string-append wt-path-norm "/" p)))))
                (tool-allowed?
                  (lambda (name)
                    (if allowed-tools
                      (member name allowed-tools)
                      #t))))

           ;; 1. read_file
           (when (tool-allowed? "read_file")
             (registry-attach!
               sub-reg "read_file"
               (lambda (path offset limit)
                 (let ((target (resolve-path path)))
                   (fs-read-file target offset limit)))
               '((description . "Read a bounded slice of the file at PATH inside the isolated worktree")
                 (parameters .
                  ((type . "object")
                   (properties .
                    ((path . ((type . "string") (description . "Path to file to read")))
                     (offset . ((type . "integer") (description . "0-based line number (default 0)")))
                     (limit . ((type . "integer") (description . "Maximum lines to return (default 800)")))))
                   (required . #("path")))))))

           ;; 2. write_file
           (when (tool-allowed? "write_file")
             (registry-attach!
               sub-reg "write_file"
               (lambda (path content)
                 (let* ((target (resolve-path path))
                        (parent (dirname target)))
                   (unless (file-exists? parent)
                     (mkdir-p parent))
                   (fs-write-file target content)))
               '((description . "Write content to a file at PATH inside the isolated worktree")
                 (parameters .
                  ((type . "object")
                   (properties .
                    ((path . ((type . "string") (description . "File path to write")))
                     (content . ((type . "string") (description . "Full content to write")))))
                   (required . #("path" "content")))))))

           ;; 3. edit_file
           (when (tool-allowed? "edit_file")
             (registry-attach!
               sub-reg "edit_file"
               (lambda (path target replacement replace_all)
                 (let ((target-file (resolve-path path))
                       (replace-all? (or (eq? replace_all #t) (equal? replace_all "true"))))
                   (fs-edit-file target-file target replacement #:replace-all? replace-all?)))
               '((description . "Surgically edit PATH inside the isolated worktree by replacing TARGET with REPLACEMENT.")
                 (parameters .
                  ((type . "object")
                   (properties .
                    ((path . ((type . "string") (description . "Path of file to modify")))
                     (target . ((type . "string") (description . "Exact character sequence to be replaced")))
                     (replacement . ((type . "string") (description . "Replacement string")))
                     (replace_all . ((type . "boolean") (description . "If true, replace all occurrences")))))
                   (required . #("path" "target" "replacement")))))))

           ;; 4. bash / sh / execute_command
           (let ((shell-fn
                   (lambda (command)
                     (let* ((cmd-str (cond
                                       ((pair? command)
                                         (or (assoc-ref command "command")
                                             (assoc-ref command 'command)
                                             (assoc-ref command "cmd")
                                             (assoc-ref command 'cmd)
                                             ""))
                                       ((string? command) command)
                                       (else (format #f "~a" command))))
                            (res (%run-cmd (string-append "cd " (%shell-quote wt-path-norm) " && " cmd-str))))
                       (cdr res)))))
             (when (tool-allowed? "bash")
               (registry-attach!
                 sub-reg "bash" shell-fn
                 '((description . "Execute a bash shell command inside the isolated worktree sandbox")
                   (parameters .
                    ((type . "object")
                     (properties .
                      ((command . ((type . "string") (description . "Command to execute")))))
                     (required . #("command")))))))
             (when (tool-allowed? "sh")
               (registry-attach!
                 sub-reg "sh" shell-fn
                 '((description . "Execute a shell command inside the isolated worktree sandbox")
                   (parameters .
                    ((type . "object")
                     (properties .
                      ((command . ((type . "string") (description . "Command to execute")))))
                     (required . #("command")))))))
             (when (tool-allowed? "execute_command")
               (registry-attach!
                 sub-reg "execute_command" shell-fn
                 '((description . "Execute a shell command inside the isolated worktree sandbox")
                   (parameters .
                    ((type . "object")
                     (properties .
                      ((cmd . ((type . "string") (description . "Command to run")))))
                     (required . #("cmd"))))))))

           ;; 5. git tools
           (when (tool-allowed? "git_status")
             (registry-attach!
               sub-reg "git_status"
               (lambda _
                 (let ((res (%git wt-path-norm "status" "--porcelain=v1" "-b")))
                   (if (car res)
                     (if (string-null? (cdr res)) "(clean working tree)" (cdr res))
                     "git: status failed")))
               '((description . "Check git status in the isolated worktree")
                 (parameters . ((type . "object") (properties . ()) (required . #()))))))

           (when (tool-allowed? "git_diff")
             (registry-attach!
               sub-reg "git_diff"
               (lambda (staged)
                 (let ((res (%git wt-path-norm "diff" "--no-color"
                                  (if (or (eq? staged #t) (equal? staged "true")) "--cached" ""))))
                   (if (car res)
                     (if (string-null? (cdr res)) "(no changes)" (cdr res))
                     "git: diff failed")))
               '((description . "Inspect git diff in the isolated worktree")
                 (parameters .
                  ((type . "object")
                   (properties .
                    ((staged . ((type . "boolean") (description . "View staged changes only")))))
                   (required . #()))))))

           (when (tool-allowed? "git_log")
             (registry-attach!
               sub-reg "git_log"
               (lambda (limit)
                 (let* ((n (or limit 10))
                        (res (%git wt-path-norm "log" "--oneline" (format #f "-n ~a" n))))
                   (if (car res) (cdr res) "git: log failed")))
               '((description . "Inspect git log in the isolated worktree")
                 (parameters .
                  ((type . "object")
                   (properties .
                    ((limit . ((type . "integer") (description . "Max commits to show (default 10)")))))
                   (required . #()))))))

           (when (tool-allowed? "git_commit")
             (registry-attach!
               sub-reg "git_commit"
               (lambda (message stage_all)
                 (when (or (eq? stage_all #t) (equal? stage_all "true"))
                   (%git wt-path-norm "add" "-A"))
                 (let ((res (%git wt-path-norm "commit" "-m" (or message "subagent commit"))))
                   (if (car res) (cdr res) (string-append "git: commit failed: " (cdr res)))))
               '((description . "Commit changes in the isolated worktree")
                 (parameters .
                  ((type . "object")
                   (properties .
                    ((message . ((type . "string") (description . "Commit message")))
                     (stage_all . ((type . "boolean") (description . "Stage all modified files before committing")))))
                   (required . #("message")))))))

           ;; 6. Clone other allowed tools from parent-reg
           (when parent-reg
             (for-each
               (lambda (entry)
                 (let* ((name-raw (assoc-ref entry 'id))
                        (name (if (symbol? name-raw) (symbol->string name-raw) name-raw)))
                   (unless (registry-lookup sub-reg name)
                     (when (tool-allowed? name)
                       (let ((impl (registry-lookup parent-reg name))
                             (meta (registry-metadata parent-reg name)))
                         (when impl
                           (registry-attach! sub-reg name impl #:metadata meta)))))))
               (registry-list parent-reg)))

           sub-reg))

;; ---- Subagent Runners -------------------------------------------------------

(define (%acp-server-proc sym)
        (module-ref (resolve-module '(geasa surfaces-acp plugin)) sym))

(define (%gen-subagent-id)
        (substring (uuid->string (generate-uuid 'uuidv4)) 0 8))

(define (create-agent-pipe-pair)
        "Create a bidirectional pair of OS pipes for in-process ACP communication.
Returns (values server-in server-out client-in client-out)."
        (let ((c2s (pipe)) (s2c (pipe)))
          (values (car c2s) (cdr s2c) (car s2c) (cdr c2s))))

(define*
  (create-agent-server #:key
                       (in-port (current-input-port))
                       (out-port (current-output-port))
                       (client #f)
                       (workspace (getcwd))
                       (permission-policy 'allow)
                       (notifier #f)
                       (client-capabilities '()))
  "Create the ACP server side (a literal badb instance) for an agent transaction."
  (let
      ((make-srv (%acp-server-proc 'make-acp-server)))
    (make-srv #:in-port
              in-port
              #:out-port
              out-port
              #:client
              client
              #:workspace
              workspace
              #:permission-policy
              permission-policy
              #:notifier
              notifier
              #:client-capabilities
              client-capabilities)))

(define*
  (create-agent-client #:key
                       in-port
                       out-port
                       (pid #f)
                       (on-update #f)
                       (permission-handler #f)
                       (client-info '((name . "badb-subagent-client") (version . "0.1.0")))
                       (client-capabilities '())
                       (on-close #f))
  "Create the ACP client side for an agent transaction."
  (make-acp-client #:in-port
                   in-port
                   #:out-port
                   out-port
                   #:pid
                   pid
                   #:on-update
                   on-update
                   #:permission-handler
                   permission-handler
                   #:client-info
                   client-info
                   #:client-capabilities
                   client-capabilities
                   #:on-close
                   on-close))

(define*
  (agent-handshake! client
                    #:key
                    (cwd (getcwd))
                    (allowed-tools #f)
                    (max-iterations #f)
                    (system-prompt #f)
                    (journal-path #f)
                    (capabilities #f)
                    (client-info #f))
  "Perform the ACP handshake (initialize + session/new) on CLIENT and
return the created sessionId."
  (acp-client-handshake! client
                         #:cwd
                         cwd
                         #:allowed-tools
                         allowed-tools
                         #:max-iterations
                         max-iterations
                         #:system-prompt
                         system-prompt
                         #:journal-path
                         journal-path
                         #:capabilities
                         capabilities
                         #:client-info
                         client-info))

(define-record-type <local-subagent>
                    (%make-local-subagent id
                                          workspace
                                          model-client
                                          permission-policy
                                          permission-handler
                                          on-update
                                          allowed-tools
                                          max-iterations
                                          system-prompt
                                          journal-path
                                          client
                                          server
                                          server-thread
                                          server-in
                                          server-out
                                          status)
                    local-subagent?
                    (id local-subagent-id)
                    (workspace local-subagent-workspace)
                    (model-client local-subagent-model-client)
                    (permission-policy local-subagent-permission-policy)
                    (permission-handler local-subagent-permission-handler)
                    (on-update local-subagent-on-update)
                    (allowed-tools local-subagent-allowed-tools)
                    (max-iterations local-subagent-max-iterations)
                    (system-prompt local-subagent-system-prompt)
                    (journal-path local-subagent-journal-path)
                    (client local-subagent-client set-local-subagent-client!)
                    (server local-subagent-server set-local-subagent-server!)
                    (server-thread local-subagent-server-thread set-local-subagent-server-thread!)
                    (server-in local-subagent-server-in set-local-subagent-server-in!)
                    (server-out local-subagent-server-out set-local-subagent-server-out!)
                    (status local-subagent-status set-local-subagent-status!))

(define*
  (make-local-subagent #:key
                       (id #f)
                       (workspace (getcwd))
                       (model-client #f)
                       (permission-policy 'allow)
                       (permission-handler #f)
                       (on-update #f)
                       (allowed-tools #f)
                       (max-iterations #f)
                       (system-prompt #f)
                       (journal-path #f))
  "Create a <local-subagent> that attaches both an ACP client and an ACP
server (a full badb instance) to the current badb instance."
  (%make-local-subagent
    (or id (string-append "lsub_" (%gen-subagent-id)))
    workspace
    model-client
    permission-policy
    permission-handler
    on-update
    allowed-tools
    max-iterations
    system-prompt
    journal-path
    #f
    #f
    #f
    #f
    #f
    'created))

(define (local-subagent-session-id agent)
        (let ((cli (local-subagent-client agent)))
          (and cli (acp-client-session-id cli))))

(define (local-subagent-start! agent)
        "Attach both an ACP server (literal badb instance) and an ACP client to the
current badb instance over in-process pipes, start the server loop thread, and
perform the ACP handshake."
        (call-with-values create-agent-pipe-pair
                          (lambda (srv-in srv-out cli-in cli-out)
                            (let*
                                ((srv
                                   (create-agent-server #:in-port
                                                        srv-in
                                                        #:out-port
                                                        srv-out
                                                        #:client
                                                        (local-subagent-model-client agent)
                                                        #:workspace
                                                        (local-subagent-workspace agent)
                                                        #:permission-policy
                                                        (local-subagent-permission-policy agent)))
                                 (acp-serve (%acp-server-proc 'acp-serve-stdio))
                                 (thr
                                   (call-with-new-thread
                                     (lambda ()
                                       (catch #t
                                         (lambda ()
                                           (acp-serve srv))
                                         (lambda _
                                           #f)))))
                                 (cli
                                   (create-agent-client #:in-port
                                                        cli-in
                                                        #:out-port
                                                        cli-out
                                                        #:permission-handler
                                                        (local-subagent-permission-handler agent)
                                                        #:on-update
                                                        (local-subagent-on-update agent))))
                              (set-local-subagent-server-in! agent srv-in)
                              (set-local-subagent-server-out! agent srv-out)
                              (set-local-subagent-server! agent srv)
                              (set-local-subagent-server-thread! agent thr)
                              (set-local-subagent-client! agent cli)
                              (catch #t
                                (lambda ()
                                  (agent-handshake! cli
                                                    #:cwd
                                                    (local-subagent-workspace agent)
                                                    #:allowed-tools
                                                    (local-subagent-allowed-tools agent)
                                                    #:max-iterations
                                                    (local-subagent-max-iterations agent)
                                                    #:system-prompt
                                                    (local-subagent-system-prompt agent)
                                                    #:journal-path
                                                    (local-subagent-journal-path agent))
                                  (set-local-subagent-status! agent 'running)
                                  agent)
                                (lambda (key . args)
                                  (set-local-subagent-status! agent 'failed)
                                  (catch #t
                                    (lambda ()
                                      (local-subagent-stop! agent))
                                    (lambda _
                                      #f))
                                  (apply throw key args)))))))

(define*
  (local-subagent-prompt! agent prompt #:key (on-update #f))
  "Execute PROMPT on AGENT's local badb ACP server via its ACP client."
  (let ((cli (local-subagent-client agent)))
    (unless cli (error "local-subagent-prompt!: agent is not started" agent))
    (acp-client-prompt! cli prompt #:on-update on-update)))

(define (local-subagent-cancel! agent)
        "Send session/cancel to AGENT's local ACP server."
        (let ((cli (local-subagent-client agent))) (when cli (acp-client-cancel! cli))))

(define (local-subagent-stop! agent)
        "Close AGENT's local ACP client, stop its in-process ACP server, and join
the server thread. Idempotent."
        (unless
            (eq? (local-subagent-status agent) 'stopped)
          (let
              ((cli (local-subagent-client agent))
               (s-in (local-subagent-server-in agent))
               (s-out (local-subagent-server-out agent))
               (thr (local-subagent-server-thread agent)))
            (when cli
              (catch #t
                (lambda ()
                  (acp-client-close! cli))
                (lambda _
                  #f)))
            (when thr
              (catch #t
                (lambda ()
                  (join-thread thr (+ (car (gettimeofday)) 2)))
                (lambda _
                  #f)))
            (when (and s-in (not (port-closed? s-in)))
              (catch #t
                (lambda ()
                  (close-port s-in))
                (lambda _
                  #f)))
            (when (and s-out (not (port-closed? s-out)))
              (catch #t
                (lambda ()
                  (close-port s-out))
                (lambda _
                  #f)))
            (set-local-subagent-status! agent 'stopped)))
        #t)

(define-record-type <remote-subagent>
                    (%make-remote-subagent id
                                           transport
                                           command
                                           host
                                           port
                                           socket-path
                                           in-port
                                           out-port
                                           workspace
                                           permission-handler
                                           on-update
                                           allowed-tools
                                           max-iterations
                                           system-prompt
                                           journal-path
                                           client
                                           status)
                    remote-subagent?
                    (id remote-subagent-id)
                    (transport remote-subagent-transport)
                    (command remote-subagent-command)
                    (host remote-subagent-host)
                    (port remote-subagent-port)
                    (socket-path remote-subagent-socket-path)
                    (in-port remote-subagent-in-port)
                    (out-port remote-subagent-out-port)
                    (workspace remote-subagent-workspace)
                    (permission-handler remote-subagent-permission-handler)
                    (on-update remote-subagent-on-update)
                    (allowed-tools remote-subagent-allowed-tools)
                    (max-iterations remote-subagent-max-iterations)
                    (system-prompt remote-subagent-system-prompt)
                    (journal-path remote-subagent-journal-path)
                    (client remote-subagent-client set-remote-subagent-client!)
                    (status remote-subagent-status set-remote-subagent-status!))

(define*
  (make-remote-subagent #:key
                        (id #f)
                        (transport 'tcp)
                        (command #f)
                        (host "127.0.0.1")
                        (port #f)
                        (socket-path #f)
                        (in-port #f)
                        (out-port #f)
                        (workspace (getcwd))
                        (permission-handler #f)
                        (on-update #f)
                        (allowed-tools #f)
                        (max-iterations #f)
                        (system-prompt #f)
                        (journal-path #f))
  "Create a <remote-subagent> that attaches an ACP client in the current
badb instance to a remote ACP server."
  (%make-remote-subagent
    (or id (string-append "rsub_" (%gen-subagent-id)))
    transport
    command
    host
    port
    socket-path
    in-port
    out-port
    workspace
    permission-handler
    on-update
    allowed-tools
    max-iterations
    system-prompt
    journal-path
    #f
    'created))

(define*
  (make-remote-subagent/spawn command #:rest rest)
  (apply make-remote-subagent #:transport 'spawn #:command command rest))

(define*
  (make-remote-subagent/tcp host port #:rest rest)
  (apply make-remote-subagent #:transport 'tcp #:host host #:port port rest))

(define*
  (make-remote-subagent/unix socket-path #:rest rest)
  (apply make-remote-subagent #:transport 'unix #:socket-path socket-path rest))

(define*
  (make-remote-subagent/ports in-port out-port #:rest rest)
  (apply make-remote-subagent
         #:transport
         'ports
         #:in-port
         in-port
         #:out-port
         out-port
         rest))

(define (remote-subagent-session-id agent)
        (let ((cli (remote-subagent-client agent)))
          (and cli (acp-client-session-id cli))))

(define (remote-subagent-start! agent)
        "Connect AGENT's client to the remote ACP server and perform the handshake."
        (let*
            ((tr (remote-subagent-transport agent))
             (on-upd (remote-subagent-on-update agent))
             (perm-h (remote-subagent-permission-handler agent))
             (cwd (remote-subagent-workspace agent))
             (cli
               (cond
                 ((eq? tr 'spawn)
                   (acp-spawn-client
                     (or (remote-subagent-command agent) '("badb" "acp"))
                     #:cwd
                     cwd
                     #:on-update
                     on-upd
                     #:permission-handler
                     perm-h))
                 ((eq? tr 'tcp)
                   (acp-connect-tcp
                     (or (remote-subagent-host agent) "127.0.0.1")
                     (remote-subagent-port agent)
                     #:on-update
                     on-upd
                     #:permission-handler
                     perm-h))
                 ((eq? tr 'unix)
                   (acp-connect-unix
                     (remote-subagent-socket-path agent)
                     #:on-update
                     on-upd
                     #:permission-handler
                     perm-h))
                 ((eq? tr 'ports)
                   (create-agent-client #:in-port
                                        (remote-subagent-in-port agent)
                                        #:out-port
                                        (remote-subagent-out-port agent)
                                        #:on-update
                                        on-upd
                                        #:permission-handler
                                        perm-h))
                 (else (error "remote-subagent-start!: unsupported transport" tr)))))
          (set-remote-subagent-client! agent cli)
          (catch #t
            (lambda ()
              (agent-handshake! cli
                                #:cwd
                                cwd
                                #:allowed-tools
                                (remote-subagent-allowed-tools agent)
                                #:max-iterations
                                (remote-subagent-max-iterations agent)
                                #:system-prompt
                                (remote-subagent-system-prompt agent)
                                #:journal-path
                                (remote-subagent-journal-path agent))
              (set-remote-subagent-status! agent 'running)
              agent)
            (lambda (key . args)
              (set-remote-subagent-status! agent 'failed)
              (catch #t
                (lambda ()
                  (acp-client-close! cli))
                (lambda _
                  #f))
              (apply throw key args)))))

(define*
  (remote-subagent-prompt! agent prompt #:key (on-update #f))
  "Execute PROMPT on the remote badb ACP server attached to AGENT."
  (let ((cli (remote-subagent-client agent)))
    (unless cli (error "remote-subagent-prompt!: agent is not started" agent))
    (acp-client-prompt! cli prompt #:on-update on-update)))

(define (remote-subagent-cancel! agent)
        "Send session/cancel to AGENT's remote ACP server."
        (let ((cli (remote-subagent-client agent))) (when cli (acp-client-cancel! cli))))

(define (remote-subagent-stop! agent)
        "Close the remote ACP client attached to AGENT. Idempotent."
        (unless
            (eq? (remote-subagent-status agent) 'stopped)
          (let ((cli (remote-subagent-client agent)))
            (when cli
              (catch #t
                (lambda ()
                  (acp-client-close! cli))
                (lambda _
                  #f)))
            (set-remote-subagent-status! agent 'stopped)))
        #t)

(define-record-type <remote-superagent>
                    (%make-remote-superagent id
                                             transport
                                             host
                                             port
                                             socket-path
                                             in-port
                                             out-port
                                             workspace
                                             model-client
                                             permission-policy
                                             notifier
                                             listener-socket
                                             accept-thread
                                             servers
                                             client-ports
                                             client-threads
                                             stop?
                                             lock
                                             status)
                    remote-superagent?
                    (id remote-superagent-id)
                    (transport remote-superagent-transport)
                    (host remote-superagent-host)
                    (port remote-superagent-port set-remote-superagent-port!)
                    (socket-path remote-superagent-socket-path)
                    (in-port remote-superagent-in-port)
                    (out-port remote-superagent-out-port)
                    (workspace remote-superagent-workspace)
                    (model-client remote-superagent-model-client)
                    (permission-policy remote-superagent-permission-policy)
                    (notifier remote-superagent-notifier)
                    (listener-socket
                      remote-superagent-listener-socket
                      set-remote-superagent-listener-socket!)
                    (accept-thread remote-superagent-accept-thread
                                   set-remote-superagent-accept-thread!)
                    (servers remote-superagent-servers set-remote-superagent-servers!)
                    (client-ports remote-superagent-client-ports
                                  set-remote-superagent-client-ports!)
                    (client-threads remote-superagent-client-threads
                                    set-remote-superagent-client-threads!)
                    (stop? remote-superagent-stop? set-remote-superagent-stop?!)
                    (lock remote-superagent-lock)
                    (status remote-superagent-status set-remote-superagent-status!))

(define*
  (make-remote-superagent #:key
                          (id #f)
                          (transport 'tcp)
                          (host "127.0.0.1")
                          (port 0)
                          (socket-path #f)
                          (in-port #f)
                          (out-port #f)
                          (workspace (getcwd))
                          (model-client #f)
                          (permission-policy 'allow)
                          (notifier #f))
  "Create a <remote-superagent> that spins up an ACP server exposing a full
badb instance over TRANSPORT ('tcp, 'unix, 'stdio, or 'ports)."
  (%make-remote-superagent
    (or id (string-append "sup_" (%gen-subagent-id)))
    transport
    host
    port
    socket-path
    in-port
    out-port
    workspace
    model-client
    permission-policy
    notifier
    #f
    #f
    '()
    '()
    '()
    #f
    (make-mutex)
    'created))

(define (remote-superagent-endpoint superagent)
        "Return an endpoint descriptor alist suitable for connecting a <remote-subagent>."
        (case (remote-superagent-transport superagent)
          ((tcp)
            `((transport . tcp)
              (host . ,(remote-superagent-host superagent))
              (port . ,(remote-superagent-port superagent))))
          ((unix)
            `((transport . unix) (socket-path . ,(remote-superagent-socket-path superagent))))
          (else `((transport . ,(remote-superagent-transport superagent))))))

(define (remote-superagent-start! superagent)
        "Spin up SUPERAGENT's ACP server over its configured transport."
        (set-remote-superagent-stop?! superagent #f)
        (let
            ((tr (remote-superagent-transport superagent)))
          (cond
            ((eq? tr 'tcp)
              (let ((sock (%superagent-bind-tcp superagent)))
                (set-remote-superagent-listener-socket! superagent sock)
                (set-remote-superagent-accept-thread! superagent
                                                      (call-with-new-thread
                                                        (lambda ()
                                                          (%superagent-accept-loop superagent sock))))))
            ((eq? tr 'unix)
              (let ((sock (%superagent-bind-unix superagent)))
                (set-remote-superagent-listener-socket! superagent sock)
                (set-remote-superagent-accept-thread! superagent
                                                      (call-with-new-thread
                                                        (lambda ()
                                                          (%superagent-accept-loop superagent sock))))))
            ((or (eq? tr 'stdio) (eq? tr 'ports))
              (let*
                  ((in (or (remote-superagent-in-port superagent) (current-input-port)))
                   (out (or (remote-superagent-out-port superagent) (current-output-port)))
                   (srv
                     (create-agent-server #:in-port
                                          in
                                          #:out-port
                                          out
                                          #:client
                                          (remote-superagent-model-client superagent)
                                          #:workspace
                                          (remote-superagent-workspace superagent)
                                          #:permission-policy
                                          (remote-superagent-permission-policy superagent)
                                          #:notifier
                                          (remote-superagent-notifier superagent)))
                   (acp-serve (%acp-server-proc 'acp-serve-stdio)))
                (set-remote-superagent-servers! superagent (list srv))
                (set-remote-superagent-accept-thread! superagent
                                                      (call-with-new-thread
                                                        (lambda ()
                                                          (catch #t
                                                            (lambda ()
                                                              (acp-serve srv))
                                                            (lambda _
                                                              #f)))))))
            (else (error "remote-superagent-start!: unsupported transport" tr)))
          (set-remote-superagent-status! superagent 'running)
          superagent))

(define (%superagent-bind-tcp superagent)
        (let
            ((sock (socket PF_INET SOCK_STREAM 0))
             (host (remote-superagent-host superagent))
             (port (or (remote-superagent-port superagent) 0)))
          (catch #t
            (lambda ()
              (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
              (bind sock AF_INET (inet-pton AF_INET host) port)
              (listen sock 8)
              (set-remote-superagent-port! superagent (sockaddr:port (getsockname sock)))
              sock)
            (lambda (key . args)
              (catch #t
                (lambda ()
                  (close-port sock))
                (lambda _
                  #f))
              (apply throw key args)))))

(define (%superagent-bind-unix superagent)
        (let
            ((path (remote-superagent-socket-path superagent)))
          (unless
              (and (string? path) (not (string-null? path)))
            (error "remote-superagent-start!: unix transport requires socket-path"))
          (when (file-exists? path) (delete-file path))
          (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
            (catch #t
              (lambda ()
                (bind sock AF_UNIX path)
                (listen sock 8)
                sock)
              (lambda (key . args)
                (catch #t
                  (lambda ()
                    (close-port sock))
                  (lambda _
                    #f))
                (apply throw key args))))))

(define (%superagent-accept-loop superagent sock)
        (let loop
          ()
          (unless (remote-superagent-stop? superagent)
            (let
                ((ready
                   (catch #t
                     (lambda ()
                       (select (list sock) '() '() 0 50000))
                     (lambda _
                       '(() () ())))))
              (when
                  (and (not (remote-superagent-stop? superagent)) (pair? (car ready)))
                (let
                    ((pair
                       (catch #t
                         (lambda ()
                           (accept sock))
                         (lambda _
                           #f))))
                  (when (pair? pair) (%superagent-serve-conn! superagent (car pair)))))
              (unless (remote-superagent-stop? superagent) (loop))))))

(define (%superagent-serve-conn! superagent conn)
        (let
            ((srv
               (create-agent-server #:in-port
                                    conn
                                    #:out-port
                                    conn
                                    #:client
                                    (remote-superagent-model-client superagent)
                                    #:workspace
                                    (remote-superagent-workspace superagent)
                                    #:permission-policy
                                    (remote-superagent-permission-policy superagent)
                                    #:notifier
                                    (remote-superagent-notifier superagent)))
             (acp-serve (%acp-server-proc 'acp-serve-stdio)))
          (with-mutex (remote-superagent-lock superagent)
                      (set-remote-superagent-servers! superagent
                                                      (cons srv (remote-superagent-servers superagent)))
                      (set-remote-superagent-client-ports! superagent
                                                           (cons conn (remote-superagent-client-ports superagent))))
          (let
              ((t
                 (call-with-new-thread
                   (lambda ()
                     (catch #t
                       (lambda ()
                         (acp-serve srv))
                       (lambda _
                         #f))
                     (catch #t
                       (lambda ()
                         (unless (port-closed? conn) (close-port conn)))
                       (lambda _
                         #f))))))
            (with-mutex (remote-superagent-lock superagent)
                        (set-remote-superagent-client-threads! superagent
                                                               (cons t (remote-superagent-client-threads superagent)))))))

(define (remote-superagent-stop! superagent)
        "Stop SUPERAGENT's listener, close active client connections, and join
background threads. Idempotent."
        (unless
            (eq? (remote-superagent-status superagent) 'stopped)
          (set-remote-superagent-stop?! superagent #t)
          (let
              ((sock (remote-superagent-listener-socket superagent))
               (t-acc (remote-superagent-accept-thread superagent))
               (ports
                 (with-mutex (remote-superagent-lock superagent)
                             (remote-superagent-client-ports superagent)))
               (threads
                 (with-mutex (remote-superagent-lock superagent)
                             (remote-superagent-client-threads superagent))))
            (when sock
              (catch #t
                (lambda ()
                  (shutdown sock 2))
                (lambda _
                  #f))
              (catch #t
                (lambda ()
                  (close-port sock))
                (lambda _
                  #f))
              (set-remote-superagent-listener-socket! superagent #f))
            (for-each
              (lambda (p)
                (catch #t
                  (lambda ()
                    (shutdown p 2))
                  (lambda _
                    #f))
                (when (and p (not (port-closed? p)))
                  (catch #t
                    (lambda ()
                      (close-port p))
                    (lambda _
                      #f))))
              ports)
            (let ((deadline (+ (car (gettimeofday)) 2)))
              (when t-acc
                (catch #t
                  (lambda ()
                    (join-thread t-acc deadline))
                  (lambda _
                    #f)))
              (for-each
                (lambda (t)
                  (catch #t
                    (lambda ()
                      (join-thread t deadline))
                    (lambda _
                      #f)))
                threads))
            (when
                (and
                  (eq? (remote-superagent-transport superagent) 'unix)
                  (string? (remote-superagent-socket-path superagent))
                  (file-exists? (remote-superagent-socket-path superagent)))
              (catch #t
                (lambda ()
                  (delete-file (remote-superagent-socket-path superagent)))
                (lambda _
                  #f)))
            (set-remote-superagent-status! superagent 'stopped)))
        #t)

(define (remote-superagent-wait superagent)
        "Wait for SUPERAGENT's accept/serve thread to finish."
        (let
            ((t (remote-superagent-accept-thread superagent)))
          (when t (join-thread t)))
        #t)

(define (subagent? x) (or (local-subagent? x) (remote-subagent? x)))

(define (subagent-start! agent)
        (cond
          ((local-subagent? agent) (local-subagent-start! agent))
          ((remote-subagent? agent) (remote-subagent-start! agent))
          ((remote-superagent? agent) (remote-superagent-start! agent))
          (else (error "subagent-start!: expected subagent or superagent" agent))))

(define*
  (subagent-prompt! agent prompt #:key (on-update #f))
  (cond
    ((local-subagent? agent)
      (local-subagent-prompt! agent prompt #:on-update on-update))
    ((remote-subagent? agent)
      (remote-subagent-prompt! agent prompt #:on-update on-update))
    (else
      (error "subagent-prompt!: expected <local-subagent> or <remote-subagent>" agent))))

(define (subagent-cancel! agent)
        (cond
          ((local-subagent? agent) (local-subagent-cancel! agent))
          ((remote-subagent? agent) (remote-subagent-cancel! agent))
          (else
            (error "subagent-cancel!: expected <local-subagent> or <remote-subagent>" agent))))

(define (subagent-stop! agent)
        (cond
          ((local-subagent? agent) (local-subagent-stop! agent))
          ((remote-subagent? agent) (remote-subagent-stop! agent))
          ((remote-superagent? agent) (remote-superagent-stop! agent))
          (else (error "subagent-stop!: expected subagent or superagent" agent))))

(define (subagent-status agent)
        (cond
          ((local-subagent? agent) (local-subagent-status agent))
          ((remote-subagent? agent) (remote-subagent-status agent))
          ((remote-superagent? agent) (remote-superagent-status agent))
          (else (error "subagent-status: expected subagent or superagent" agent))))

(define (subagent-session-id agent)
        (cond
          ((local-subagent? agent) (local-subagent-session-id agent))
          ((remote-subagent? agent) (remote-subagent-session-id agent))
          (else
            (error
              "subagent-session-id: expected <local-subagent> or <remote-subagent>"
              agent))))

(define (subagent-client agent)
        (cond
          ((local-subagent? agent) (local-subagent-client agent))
          ((remote-subagent? agent) (remote-subagent-client agent))
          (else
            (error "subagent-client: expected <local-subagent> or <remote-subagent>" agent))))

(define (call-with-subagent agent proc)
        "Start AGENT (performing ACP handshake), invoke (PROC AGENT), and always
stop AGENT on exit (normal or exceptional)."
        (dynamic-wind
          (lambda ()
            (subagent-start! agent))
          (lambda ()
            (proc agent))
          (lambda ()
            (catch #t
              (lambda ()
                (subagent-stop! agent))
              (lambda _
                #f)))))

(define
  (%write-subagent-trajectory! journal-dir sub-id prompt res)
  (catch #t
    (lambda ()
      (unless (file-exists? journal-dir) (mkdir-p journal-dir))
      (let
          ((traj-file (string-append journal-dir "/" sub-id ".jsonl")))
        (call-with-output-file traj-file
                               (lambda (p)
                                 (display (safe-json-string `((role . "user") (content . ,prompt))) p)
                                 (newline p)
                                 (for-each
                                   (lambda (tc)
                                     (display (safe-json-string `((role . "tool_call") (update . ,tc))) p)
                                     (newline p))
                                   (or (assoc-ref res 'tool-calls) '()))
                                 (let ((content (assoc-ref res 'content)))
                                   (when (string? content)
                                     (display (safe-json-string `((role . "assistant") (content . ,content))) p)
                                     (newline p)))))))
    (lambda _
      #f)))

(define*
  (run-subagent-worktree prompt
                         #:key
                         (client #f)
                         (parent-reg #f)
                         (repo (getcwd))
                         (base "HEAD")
                         (allowed-tools #f)
                         (max-iterations #f)
                         (verify? #t)
                         (auto-merge? #f)
                         (keep-worktree? #f)
                         (verifier #f)
                         (verify-command #f)
                         (permission-policy 'allow)
                         (permission-handler #f)
                         (on-update #f)
                         (remote #f)
                         (system-prompt #f)
                         (journal-dir ".badb/journal/subagents"))
  "Execute an isolated subagent transaction in an ephemeral Git worktree
using a literal badb instance over ACP (separated client & server + handshake).
Returns an alist summarizing status, content, worktree-path, branch, verification,
and merge results."
  (let*
      ((sw (create-subagent-worktree #:repo repo #:base base))
       (sub-id (subagent-worktree-id sw))
       (wt-path (subagent-worktree-path sw))
       (sys-prompt
         (or system-prompt
             "You are an autonomous sub-agent working in an isolated Git worktree sandbox. Inspect, modify, or test files inside your sandbox to fulfill the task. Changes are committed and verified before merging."))
       (agent
         (cond
           ((remote-subagent? remote) remote)
           ((pair? remote)
             (make-remote-subagent #:id
                                   sub-id
                                   #:transport
                                   (or (assoc-ref remote 'transport) 'tcp)
                                   #:host
                                   (or (assoc-ref remote 'host) "127.0.0.1")
                                   #:port
                                   (assoc-ref remote 'port)
                                   #:socket-path
                                   (assoc-ref remote 'socket-path)
                                   #:command
                                   (assoc-ref remote 'command)
                                   #:workspace
                                   wt-path
                                   #:permission-handler
                                   permission-handler
                                   #:on-update
                                   on-update
                                   #:allowed-tools
                                   (or allowed-tools %default-worktree-tools)
                                   #:max-iterations
                                   max-iterations
                                   #:system-prompt
                                   sys-prompt))
           (else
             (make-local-subagent #:id
                                  sub-id
                                  #:workspace
                                  wt-path
                                  #:model-client
                                  client
                                  #:permission-policy
                                  permission-policy
                                  #:permission-handler
                                  permission-handler
                                  #:on-update
                                  on-update
                                  #:allowed-tools
                                  (or allowed-tools %default-worktree-tools)
                                  #:max-iterations
                                  max-iterations
                                  #:system-prompt
                                  sys-prompt)))))
    (let*
        ((res
           (call-with-subagent agent
                               (lambda (ag)
                                 (subagent-prompt! ag prompt))))
         (status (assoc-ref res 'status))
         (content (assoc-ref res 'content)))
      (%write-subagent-trajectory! journal-dir sub-id prompt res)
      (let*
          ((status-res (%git wt-path "status" "--porcelain=v1"))
           (has-uncommitted? (and (car status-res) (not (string-null? (cdr status-res)))))
           (_
             (when has-uncommitted?
               (%git wt-path "add" "-A")
               (%git wt-path
                     "commit"
                     "-m"
                     (format #f "Subagent transaction ~a auto-commit" sub-id))))
           (diff-after (%git wt-path "diff" "HEAD" base))
           (has-real-changes? (and (car diff-after) (not (string-null? (cdr diff-after)))))
           (v-res
             (if (and has-real-changes? verify?)
               (subagent-worktree-verify sw #:verifier verifier #:command verify-command)
               '((passed? . #t))))
           (verified? (assoc-ref v-res 'passed?))
           (m-res
             (if (and auto-merge? verified? has-real-changes?)
               (subagent-worktree-merge! sw #:verifier verifier #:command verify-command)
               `((merged? . #f)
                 (reason .
                         ,(if auto-merge?
                            "no-changes-to-merge"
                            "auto-merge-disabled")))))
           (merged? (assoc-ref m-res 'merged?)))
        (unless keep-worktree? (cleanup-subagent-worktree! sw))
        `((sub-id . ,sub-id) (status . ,status)
          (content .
                   ,(or
                      (and (string? content) (not (string-null? content)) content)
                      (format #f "[Sub-agent finished with status: ~a]" status)))
          (worktree-path . ,wt-path)
          (branch . ,(subagent-worktree-branch sw))
          (has-changes? . ,has-real-changes?)
          (verified? . ,verified?)
          (merged? . ,merged?)
          (verification . ,v-res)
          (merge-result . ,m-res))))))

(define run-agent-transaction run-subagent-worktree)

(define*
  (run-subagent prompt
                #:key
                (client #f)
                (parent-reg #f)
                (allowed-tools %default-investigate-tools)
                (max-iterations #f)
                (worktree-isolated? #f)
                (repo (getcwd))
                (base "HEAD")
                (verify? #t)
                (auto-merge? #f)
                (keep-worktree? #f)
                (workspace (getcwd))
                (permission-policy 'allow)
                (permission-handler #f)
                (on-update #f)
                (remote #f)
                (system-prompt #f)
                (journal-dir ".badb/journal/subagents"))
  "Execute a full badb sub-agent over ACP. When WORKTREE-ISOLATED? is true,
runs inside an ephemeral Git worktree sandbox. When REMOTE is provided (a
<remote-subagent> or endpoint alist), delegates to the remote ACP server."
  (if worktree-isolated?
    (let
        ((res
           (run-subagent-worktree prompt
                                  #:client
                                  client
                                  #:parent-reg
                                  parent-reg
                                  #:allowed-tools
                                  (or allowed-tools %default-worktree-tools)
                                  #:max-iterations
                                  max-iterations
                                  #:repo
                                  repo
                                  #:base
                                  base
                                  #:verify?
                                  verify?
                                  #:auto-merge?
                                  auto-merge?
                                  #:keep-worktree?
                                  keep-worktree?
                                  #:permission-policy
                                  permission-policy
                                  #:permission-handler
                                  permission-handler
                                  #:on-update
                                  on-update
                                  #:remote
                                  remote
                                  #:journal-dir
                                  journal-dir)))
      (assoc-ref res 'content))
    (let*
        ((sub-id (%gen-subagent-id))
         (sys
           (or system-prompt
               "You are a focused research sub-agent. Analyze files and query search tools to answer the user request directly, concisely, and factually. Do not provide fluff."))
         (agent
           (cond
             ((remote-subagent? remote) remote)
             ((local-subagent? remote) remote)
             ((pair? remote)
               (make-remote-subagent #:id
                                     sub-id
                                     #:transport
                                     (or (assoc-ref remote 'transport) 'tcp)
                                     #:host
                                     (or (assoc-ref remote 'host) "127.0.0.1")
                                     #:port
                                     (assoc-ref remote 'port)
                                     #:socket-path
                                     (assoc-ref remote 'socket-path)
                                     #:command
                                     (assoc-ref remote 'command)
                                     #:in-port
                                     (assoc-ref remote 'in-port)
                                     #:out-port
                                     (assoc-ref remote 'out-port)
                                     #:workspace
                                     workspace
                                     #:permission-handler
                                     permission-handler
                                     #:on-update
                                     on-update
                                     #:allowed-tools
                                     allowed-tools
                                     #:max-iterations
                                     max-iterations
                                     #:system-prompt
                                     sys))
             (else
               (make-local-subagent #:id
                                    sub-id
                                    #:workspace
                                    workspace
                                    #:model-client
                                    client
                                    #:permission-policy
                                    permission-policy
                                    #:permission-handler
                                    permission-handler
                                    #:on-update
                                    on-update
                                    #:allowed-tools
                                    allowed-tools
                                    #:max-iterations
                                    max-iterations
                                    #:system-prompt
                                    sys))))
         (res
           (call-with-subagent agent
                               (lambda (ag)
                                 (subagent-prompt! ag prompt))))
         (status (assoc-ref res 'status))
         (content (assoc-ref res 'content)))
      (%write-subagent-trajectory! journal-dir sub-id prompt res)
      (or
        (and (string? content) (not (string-null? content)) content)
        (format #f "[Sub-agent finished with status: ~a]" status)))))

(define* (run-subagents-parallel queries
                                 #:key
                                 (client #f)
                                 (parent-reg #f)
                                 (allowed-tools %default-investigate-tools)
                                 (max-iterations #f)
                                 (journal-dir ".badb/journal/subagents"))
         "Execute multiple subagents in parallel across QUERIES and collect formatted findings."
         (let ((results
                 (par-map
                   (lambda (q)
                     (let ((ans (run-subagent q
                                              #:client client
                                              #:parent-reg parent-reg
                                              #:allowed-tools allowed-tools
                                              #:max-iterations max-iterations
                                              #:journal-dir journal-dir)))
                       (format #f "### Query: ~a\n~a" q ans)))
                   queries)))
           (string-join results "\n\n")))

;; ---- Tool Schemas & Registration --------------------------------------------

(define %investigate-tool-schema
        `((name . "investigate")
          (description . "Spawn a disposable sub-agent to research or inspect files with 0 context pollution. Returns only findings.")
          (parameters .
                      ((type . "object")
                       (properties .
                                   ((prompt . ((type . "string") (description . "Research question for the sub-agent")))
                                    (allowed_tools . ((type . "array") (items . ((type . "string"))) (description . "Tool names to grant")))
                                    (isolated . ((type . "boolean") (description . "Run in an isolated ephemeral worktree sandbox (default: false)")))
                                    (max_iterations . ((type . "integer") (description . "Max iterations (default: unlimited)")))))
                       (required . #("prompt"))))))

(define %investigate-parallel-tool-schema
        `((name . "investigate_parallel")
          (description . "Run multiple disposable sub-agents concurrently to investigate independent questions.")
          (parameters .
                      ((type . "object")
                       (properties .
                                   ((queries . ((type . "array") (items . ((type . "string"))) (description . "List of questions to investigate")))
                                    (max_iterations . ((type . "integer") (description . "Max iterations per sub-agent (default: unlimited)")))))
                       (required . #("queries"))))))

(define %subagent-transaction-tool-schema
        `((name . "subagent_transaction")
          (description . "Spawn an isolated sub-agent in an ephemeral Git worktree sandbox to safely inspect and modify code. Changes are validated against test gates and can optionally be merged.")
          (parameters .
                      ((type . "object")
                       (properties .
                                   ((prompt . ((type . "string") (description . "Task instruction for the isolated sub-agent")))
                                    (allowed_tools . ((type . "array") (items . ((type . "string"))) (description . "Tools to grant inside the worktree")))
                                    (verify . ((type . "boolean") (description . "Verify changes via test suite before completing (default: true)")))
                                    (auto_merge . ((type . "boolean") (description . "Merge verified changes back to parent branch (default: false)")))
                                    (keep_worktree . ((type . "boolean") (description . "Keep worktree on disk for inspection (default: false)")))
                                    (max_iterations . ((type . "integer") (description . "Max ReAct iterations (default: unlimited)")))))
                       (required . #("prompt"))))))

(define*
  (register-subagent-tools! reg
                            #:key
                            (client #f)
                            (journal-dir ".badb/journal/subagents")
                            (default-allowed-tools %default-investigate-tools))
  "Register 'investigate', 'investigate_parallel', 'subagent_transaction',
and deferred 'remote_subagent' tools in REG."
  (registry-attach! reg
                    "investigate"
                    (lambda (args)
                      (with-tool-args args
                                      ((prompt prompt "") (tools-raw allowed_tools #f)
                                       (isolated-raw isolated #f)
                                       (max-iter max_iterations #f))
                                      (let
                                          ((tools
                                             (cond
                                               ((vector? tools-raw) (vector->list tools-raw))
                                               ((list? tools-raw) tools-raw)
                                               (else default-allowed-tools))))
                                        (run-subagent prompt
                                                      #:client
                                                      client
                                                      #:parent-reg
                                                      reg
                                                      #:allowed-tools
                                                      tools
                                                      #:max-iterations
                                                      max-iter
                                                      #:worktree-isolated?
                                                      (eq? isolated-raw #t)
                                                      #:journal-dir
                                                      journal-dir))))
                    #:metadata
                    %investigate-tool-schema)
  (registry-attach! reg
                    "investigate_parallel"
                    (lambda (args)
                      (with-tool-args args
                                      ((queries-raw queries '()) (max-iter max_iterations #f))
                                      (let
                                          ((queries
                                             (cond
                                               ((vector? queries-raw) (vector->list queries-raw))
                                               ((list? queries-raw) queries-raw)
                                               ((string? queries-raw) (list queries-raw))
                                               (else '()))))
                                        (run-subagents-parallel queries
                                                                #:client
                                                                client
                                                                #:parent-reg
                                                                reg
                                                                #:allowed-tools
                                                                default-allowed-tools
                                                                #:max-iterations
                                                                max-iter
                                                                #:journal-dir
                                                                journal-dir))))
                    #:metadata
                    %investigate-parallel-tool-schema)
  (registry-attach! reg
                    "subagent_transaction"
                    (lambda (args)
                      (with-tool-args args
                                      ((prompt prompt "") (tools-raw allowed_tools #f)
                                       (verify-raw verify #t)
                                       (auto-merge-raw auto_merge #f)
                                       (keep-wt-raw keep_worktree #f)
                                       (max-iter max_iterations #f))
                                      (let*
                                          ((tools
                                             (cond
                                               ((vector? tools-raw) (vector->list tools-raw))
                                               ((list? tools-raw) tools-raw)
                                               (else %default-worktree-tools)))
                                           (res
                                             (run-subagent-worktree prompt
                                                                    #:client
                                                                    client
                                                                    #:parent-reg
                                                                    reg
                                                                    #:allowed-tools
                                                                    tools
                                                                    #:verify?
                                                                    (not (eq? verify-raw #f))
                                                                    #:auto-merge?
                                                                    (eq? auto-merge-raw #t)
                                                                    #:keep-worktree?
                                                                    (eq? keep-wt-raw #t)
                                                                    #:max-iterations
                                                                    max-iter
                                                                    #:journal-dir
                                                                    journal-dir)))
                                        (format #f
                                                "[Transaction ~a | status: ~a, verified: ~a, merged: ~a]:\n~a"
                                                (assoc-ref res 'sub-id)
                                                (assoc-ref res 'status)
                                                (assoc-ref res 'verified?)
                                                (assoc-ref res 'merged?)
                                                (assoc-ref res 'content)))))
                    #:metadata
                    %subagent-transaction-tool-schema)
  (registry-attach! reg
                    "remote_subagent"
                    (lambda (args)
                      (with-tool-args args
                                      ((prompt prompt "")
                                       (transport-raw transport "tcp")
                                       (host-raw host "127.0.0.1")
                                       (port-raw port #f)
                                       (socket-raw socket_path #f)
                                       (command-raw command #f)
                                       (cwd-raw cwd (getcwd))
                                       (tools-raw allowed_tools #f)
                                       (max-iter max_iterations #f))
                                      (let*
                                          ((tools
                                             (cond
                                               ((vector? tools-raw) (vector->list tools-raw))
                                               ((list? tools-raw) tools-raw)
                                               (else #f)))
                                           (transport-sym
                                             (cond
                                               ((symbol? transport-raw) transport-raw)
                                               ((string? transport-raw) (string->symbol transport-raw))
                                               (else 'tcp)))
                                           (cmd-list
                                             (cond
                                               ((vector? command-raw) (vector->list command-raw))
                                               ((list? command-raw) command-raw)
                                               ((string? command-raw) (list command-raw))
                                               (else #f)))
                                           (agent
                                             (make-remote-subagent #:transport
                                                                   transport-sym
                                                                   #:host
                                                                   (or host-raw "127.0.0.1")
                                                                   #:port
                                                                   (and (integer? port-raw) port-raw)
                                                                   #:socket-path
                                                                   socket-raw
                                                                   #:command
                                                                   cmd-list
                                                                   #:workspace
                                                                   (or cwd-raw (getcwd))
                                                                   #:allowed-tools
                                                                   tools
                                                                   #:max-iterations
                                                                   max-iter))
                                           (res
                                             (call-with-subagent agent
                                                                 (lambda (ag)
                                                                   (subagent-prompt! ag prompt)))))
                                        (or (assoc-ref res 'content)
                                            (format #f "[Remote subagent finished with status: ~a]" (assoc-ref res 'status))))))
                    #:metadata
                    '((description .
                       "Connect to a remote badb ACP server over TCP, Unix socket, or spawned subprocess, perform the ACP handshake, and execute a prompt.")
                      (_badb_defer . #t)
                      (parameters .
                       ((type . "object")
                        (properties .
                         ((prompt .
                           ((type . "string") (description . "Task prompt for the remote badb instance")))
                          (transport .
                           ((type . "string")
                            (description . "Transport: tcp, unix, or spawn (default: tcp)")))
                          (host . ((type . "string") (description . "TCP host (default: 127.0.0.1)")))
                          (port . ((type . "integer") (description . "TCP port")))
                          (socket_path . ((type . "string") (description . "Unix domain socket path")))
                          (command .
                           ((type . "array") (items . ((type . "string")))
                            (description . "Command argv for spawn transport")))
                          (cwd .
                           ((type . "string") (description . "Workspace directory for session/new")))
                          (allowed_tools .
                           ((type . "array") (items . ((type . "string")))
                            (description . "Optional tool allowlist")))
                          (max_iterations . ((type . "integer") (description . "Max ReAct iterations")))))
                        (required . #("prompt")))))))