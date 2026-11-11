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
               #:use-module (badb kernel registry)
               #:use-module (geasa runtimes-react plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa capabilities-fs-tools plugin)
               #:use-module (geasa runtimes-verify plugin)
               #:use-module (json)
               #:use-module (ice-9 threads)
               #:use-module (ice-9 format)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 ftw)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:use-module (srfi srfi-13)
               #:use-module (uuid generate)
               #:use-module (uuid utils)
               #:export (<subagent-worktree>
                          make-subagent-worktree
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

(define* (run-subagent-worktree prompt
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
                                (journal-dir ".badb/journal/subagents"))
         "Execute an isolated subagent in an ephemeral Git worktree sandbox.
Returns an alist describing the subagent transaction result."
         (let* ((sw (create-subagent-worktree #:repo repo #:base base))
                (sub-id (subagent-worktree-id sw))
                (wt-path (subagent-worktree-path sw))
                (sub-reg (make-isolated-subagent-registry parent-reg wt-path
                                                          #:allowed-tools (or allowed-tools %default-worktree-tools)
                                                          #:parent-repo repo))
                (sys-prompt
                  "You are an autonomous sub-agent working in an isolated Git worktree sandbox. Inspect, modify, or test files inside your sandbox to fulfill the task. Changes stay on an isolated branch until verified.")
                (session (make-react-session client
                                             #:initial-messages
                                             `(((role . "system") (content . ,sys-prompt))
                                               ((role . "user") (content . ,prompt)))
                                             #:reg sub-reg
                                             #:max-iterations max-iterations)))
           (call-with-values
             (lambda () (react-run! session))
             (lambda (status content final-session)
               ;; Dump trajectory
               (catch #t
                 (lambda ()
                   (mkdir-p journal-dir)
                   (let ((traj-file (format #f "~a/~a.jsonl" journal-dir sub-id)))
                     (call-with-output-file traj-file
                                            (lambda (p)
                                              (for-each
                                                (lambda (msg)
                                                  (display (safe-json-string msg) p)
                                                  (newline p))
                                                (react-session-messages final-session))))))
                 (lambda _ #f))

               ;; Check if changes were made
               (let* ((status-res (%git wt-path "status" "--porcelain=v1"))
                      (has-uncommitted? (and (car status-res) (not (string-null? (cdr status-res)))))
                      (diff-res (%git wt-path "diff" "HEAD" base))
                      (has-commits? (and (car diff-res) (not (string-null? (cdr diff-res)))))
                      (has-changes? (or has-uncommitted? has-commits?))
                      ;; Stage and commit uncommitted changes if any
                      (_ (when has-uncommitted?
                           (%git wt-path "add" "-A")
                           (%git wt-path "commit" "-m" (format #f "Subagent transaction ~a auto-commit" sub-id))))
                      (diff-after (%git wt-path "diff" "HEAD" base))
                      (has-real-changes? (and (car diff-after) (not (string-null? (cdr diff-after))))))

                 (let* ((v-res (if (and has-real-changes? verify?)
                                 (subagent-worktree-verify sw #:verifier verifier #:command verify-command)
                                 '((passed? . #t))))
                        (verified? (assoc-ref v-res 'passed?))
                        (m-res (if (and auto-merge? verified? has-real-changes?)
                                 (subagent-worktree-merge! sw #:verifier verifier #:command verify-command)
                                 `((merged? . #f) (reason . ,(if auto-merge? "no-changes-to-merge" "auto-merge-disabled")))))
                        (merged? (assoc-ref m-res 'merged?)))

                   (unless keep-worktree?
                     (cleanup-subagent-worktree! sw #:delete-branch? (not merged?)))

                   `((sub-id . ,sub-id)
                     (status . ,status)
                     (content . ,(or content (format #f "[Sub-agent finished with status: ~a]" status)))
                     (worktree-path . ,wt-path)
                     (branch . ,(subagent-worktree-branch sw))
                     (has-changes? . ,has-real-changes?)
                     (verified? . ,verified?)
                     (verification . ,v-res)
                     (merged? . ,merged?)
                     (merge-result . ,m-res))))))))

(define* (run-subagent prompt
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
                       (journal-dir ".badb/journal/subagents"))
         "Execute a ReAct sub-agent. When WORKTREE-ISOLATED? is true, runs inside an ephemeral Git worktree sandbox."
         (if worktree-isolated?
           (let ((res (run-subagent-worktree prompt
                                             #:client client
                                             #:parent-reg parent-reg
                                             #:repo repo
                                             #:base base
                                             #:allowed-tools allowed-tools
                                             #:max-iterations max-iterations
                                             #:verify? verify?
                                             #:auto-merge? auto-merge?
                                             #:keep-worktree? keep-worktree?
                                             #:journal-dir journal-dir)))
             (or (assoc-ref res 'content) ""))
           (let* ((sub-id (uuid->string (generate-uuid 'uuidv4)))
                  (sub-reg (make-registry))
                  (_ (when parent-reg
                       (for-each
                         (lambda (t-name)
                           (let ((impl (registry-lookup parent-reg t-name))
                                 (meta (registry-metadata parent-reg t-name)))
                             (when impl
                               (registry-attach! sub-reg t-name impl #:metadata meta))))
                         (or allowed-tools %default-investigate-tools))))
                  (sys-prompt
                    "You are a focused research sub-agent. Analyze files and query search tools to answer the user request directly, concisely, and factually. Do not provide fluff.")
                  (session (make-react-session client
                                               #:initial-messages
                                               `(((role . "system") (content . ,sys-prompt))
                                                 ((role . "user") (content . ,prompt)))
                                               #:reg sub-reg
                                               #:max-iterations max-iterations)))
             (call-with-values
               (lambda () (react-run! session))
               (lambda (status content final-session)
                 ;; Dump trajectory to journal-dir
                 (catch #t
                   (lambda ()
                     (mkdir-p journal-dir)
                     (let ((traj-file (format #f "~a/~a.jsonl" journal-dir sub-id)))
                       (call-with-output-file traj-file
                                              (lambda (p)
                                                (for-each
                                                  (lambda (msg)
                                                    (display (safe-json-string msg) p)
                                                    (newline p))
                                                  (react-session-messages final-session))))))
                   (lambda _ #f))
                 (or content (format #f "[Sub-agent finished with status: ~a]" status)))))))

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

(define* (register-subagent-tools! reg
                                   #:key
                                   (client #f)
                                   (journal-dir ".badb/journal/subagents")
                                   (default-allowed-tools %default-investigate-tools))
         "Register 'investigate', 'investigate_parallel', and 'subagent_transaction' tools in REG."
         (registry-attach!
           reg
           "investigate"
           (lambda (args)
             (with-tool-args args ((prompt prompt "")
                                   (tools-raw allowed_tools #f)
                                   (isolated-raw isolated #f)
                                   (max-iter max_iterations #f))
                             (let ((tools (cond
                                            ((vector? tools-raw) (vector->list tools-raw))
                                            ((list? tools-raw) tools-raw)
                                            (else default-allowed-tools)))
                                   (isolated? (and isolated-raw (not (eq? isolated-raw #f)))))
                               (run-subagent prompt
                                             #:client client
                                             #:parent-reg reg
                                             #:allowed-tools tools
                                             #:worktree-isolated? isolated?
                                             #:max-iterations max-iter
                                             #:journal-dir journal-dir))))
           #:metadata %investigate-tool-schema)

         (registry-attach!
           reg
           "investigate_parallel"
           (lambda (args)
             (with-tool-args args ((queries-raw queries '())
                                   (max-iter max_iterations #f))
                             (let ((queries (cond
                                              ((vector? queries-raw) (vector->list queries-raw))
                                              ((list? queries-raw) queries-raw)
                                              ((string? queries-raw) (list queries-raw))
                                              (else '()))))
                               (run-subagents-parallel queries
                                                       #:client client
                                                       #:parent-reg reg
                                                       #:allowed-tools default-allowed-tools
                                                       #:max-iterations max-iter
                                                       #:journal-dir journal-dir))))
           #:metadata %investigate-parallel-tool-schema)

         (registry-attach!
           reg
           "subagent_transaction"
           (lambda (args)
             (with-tool-args args ((prompt prompt "")
                                   (tools-raw allowed_tools #f)
                                   (verify-raw verify #t)
                                   (auto-merge-raw auto_merge #f)
                                   (keep-wt-raw keep_worktree #f)
                                   (max-iter max_iterations #f))
                             (let* ((tools (cond
                                             ((vector? tools-raw) (vector->list tools-raw))
                                             ((list? tools-raw) tools-raw)
                                             (else #f)))
                                    (verify? (not (eq? verify-raw #f)))
                                    (auto-merge? (and auto-merge-raw (not (eq? auto-merge-raw #f))))
                                    (keep-wt? (and keep-wt-raw (not (eq? keep-wt-raw #f))))
                                    (res (run-subagent-worktree prompt
                                                                #:client client
                                                                #:parent-reg reg
                                                                #:allowed-tools tools
                                                                #:verify? verify?
                                                                #:auto-merge? auto-merge?
                                                                #:keep-worktree? keep-wt?
                                                                #:max-iterations max-iter
                                                                #:journal-dir journal-dir)))
                               (format #f "Subagent Transaction Result [id: ~a, status: ~a, verified: ~a, merged: ~a]:\n~a"
                                       (assoc-ref res 'sub-id)
                                       (assoc-ref res 'status)
                                       (assoc-ref res 'verified?)
                                       (assoc-ref res 'merged?)
                                       (assoc-ref res 'content)))))
           #:metadata %subagent-transaction-tool-schema))
