;;; badb — Dynamic Project Context & Rules Assembly
;;;
;;; Discovers workspace guidelines (AGENTS.md, .badb/rules.scm, .cursorrules, CLAUDE.md),
;;; inspects git status and file trees, and dynamically synthesizes modular
;;; system prompts tailored for autonomous ReAct loops.

(define-module (badb prompt)
  #:use-module (badb util)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<prompt-composer>
            make-prompt-composer
            prompt-composer?
            prompt-composer-system-identity
            prompt-composer-include-git-status?
            prompt-composer-include-file-tree?
            prompt-composer-file-tree-depth
            prompt-composer-file-tree-max-files
            prompt-composer-rule-files
            prompt-composer-custom-rules
            prompt-composer-tool-style
            prompt-composer-context-limit-chars
            set-prompt-composer-system-identity!
            set-prompt-composer-custom-rules!

            ;; Environment & Git Inspection
            get-git-branch
            get-git-status-summary
            build-workspace-file-tree
            collect-environment-info

            ;; Rules Discovery
            discover-project-rules
            read-rule-file

            ;; Tool Formatting
            format-tools-for-prompt

            ;; Prompt Assembly
            assemble-system-prompt
            default-system-prompt))

;;;
;;; Shell & Process Helper
;;;
(define* (exec-cmd-output cmd args #:optional (dir #f))
  "Execute CMD with ARGS, optionally within DIR, and capture stdout as a trimmed string.
The child's stderr is routed to /dev/null: under the notcurses backend
any stderr bytes land on the same tty as the diff-rendered frame and
corrupt it mid-session (git's 'fatal: not a repository' noise was
bleeding straight through the UI). Returns #f if execution fails or
command exits non-zero."
  (catch #t
    (lambda ()
      (let ((orig-dir (and dir (getcwd))))
        (dynamic-wind
          (lambda ()
            (when dir (chdir dir)))
          (lambda ()
            (let*
                ;; sh -c 'exec "$0" "$@"' passes the command and
                ;; arguments through sh's own safe word splitting - no
                ;; quoting to get wrong - while the trailing redirect
                ;; silences the child's stderr.
                ((pipe (apply open-pipe* "r"
                              "/bin/sh" "-c"
                              "exec \"$0\" \"$@\" 2>/dev/null"
                              cmd args))
                 (output (read-string pipe))
                 (status (close-pipe pipe)))
              (if (zero? (status:exit-val status))
                (if (string? output)
                  (string-trim-both output)
                  "")
                #f)))
          (lambda ()
            (when orig-dir (chdir orig-dir))))))
    (lambda _
      #f)))

;;;
;;; Git Inspection
;;;
(define* (get-git-branch #:optional (workspace-dir (getcwd)))
  "Return current git branch name in WORKSPACE-DIR, or #f if not a git repository."
  (exec-cmd-output "git" '("branch" "--show-current") workspace-dir))

(define* (get-git-status-summary #:optional (workspace-dir (getcwd)) #:key (max-lines 25))
  "Return a concise summary of git status in WORKSPACE-DIR, or #f if not a git repository."
  (let ((raw (exec-cmd-output "git" '("status" "--porcelain") workspace-dir)))
    (cond
     ((not raw) #f)
     ((string-null? raw) "clean (no changes)")
     (else
      (let* ((lines (filter (lambda (l) (not (string-null? l)))
                            (string-split raw #\newline)))
             (count (length lines))
             (trimmed (take lines (min count max-lines)))
             (body (string-join trimmed "\n")))
        (if (> count max-lines)
            (format #f "~a\n... (~a more changes truncated)" body (- count max-lines))
            body))))))

;;;
;;; File Tree Scanning
;;;
(define %default-ignore-patterns
  '(".git" ".cache" "node_modules" "target" ".badb" "dist" "build" ".guix-profile"))

(define*
  (build-workspace-file-tree #:optional
                             (workspace-dir (getcwd))
                             #:key
                             (max-depth 2)
                             (max-files 80)
                             (ignore-patterns %default-ignore-patterns))
  "Generate a clean, indented file tree outline of WORKSPACE-DIR."
  (let ((file-count 0))
    (define (scan-dir dir depth prefix acc)
            (if (> depth max-depth)
              acc
              (catch 'system-error
                (lambda ()
                  (let*
                      ((dir-stream (opendir dir))
                       (entries
                         (let loop
                           ((accum '()))
                           (let ((entry (readdir dir-stream)))
                             (if (eof-object? entry)
                               (begin (closedir dir-stream) (reverse accum))
                               (loop (cons entry accum)))))))
                    (let
                        ((sorted
                           (sort
                             (filter
                               (lambda (name)
                                 (and (not (member name '("." "..")))
                                      (not (member name ignore-patterns))
                                      (not (string-prefix? "." name))
                                      (not (string-suffix? ".log" name))))
                               entries)
                             string<?)))
                      (let loop
                        ((items sorted) (current-acc acc))
                        (if (or (null? items) (>= file-count max-files))
                          current-acc
                          (let*
                              ((name (car items))
                               (full (string-append dir "/" name))
                               (is-dir? (and (file-exists? full) (file-is-directory? full))))
                            (set! file-count (+ file-count 1))
                            (if is-dir?
                              (let*
                                  ((entry-line (format #f "~a├── ~a/" prefix name))
                                   (sub-acc
                                     (scan-dir full
                                               (+ depth 1)
                                               (string-append prefix "│   ")
                                               (cons entry-line current-acc))))
                                (loop (cdr items) sub-acc))
                              (let
                                  ((entry-line (format #f "~a├── ~a" prefix name)))
                                (loop (cdr items) (cons entry-line current-acc))))))))))
                (lambda _
                  acc))))
    (let
        ((result (reverse (scan-dir workspace-dir 1 "" '()))))
      (if (null? result)
        "(empty or unreadable workspace)"
        (let ((tree-str (string-join result "\n")))
          (if (>= file-count max-files)
            (format #f "~a\n... (additional files truncated)" tree-str)
            tree-str))))))

;;;
;;; Environment Info
;;;
(define (collect-environment-info)
  "Collect host environment metadata."
  `((os . "Linux")
    (guile-version . ,(version))
    (host . ,(or (gethostname) "localhost"))
    (working-dir . ,(getcwd))))

;;;
;;; Project Rules Discovery
;;;
(define %standard-rule-candidates
  '(".badb/rules.scm"
    "AGENTS.md"
    ".cursorrules"
    ".windsurfrules"
    "CLAUDE.md"
    ".github/copilot-instructions.md"))

(define (read-rule-file filepath)
  "Safely read a project rule file. If it is a Scheme file (.scm), attempts to extract
docstrings or expressions; otherwise returns the raw text content."
  (catch #t
    (lambda ()
      (if (string-suffix? ".scm" filepath)
          (call-with-input-file filepath
            (lambda (port)
              (let loop ((acc '()))
                (let ((sexp (read port)))
                  (if (eof-object? sexp)
                      (string-join (reverse acc) "\n\n")
                      (let ((chunk (cond
                                    ((string? sexp) sexp)
                                    ((and (pair? sexp) (eq? (car sexp) 'rules))
                                     (format #f "~a" (cdr sexp)))
                                    ((and (pair? sexp) (eq? (car sexp) 'instructions))
                                     (format #f "~a" (cadr sexp)))
                                    (else (format #f "~s" sexp)))))
                        (loop (cons chunk acc))))))))
          (call-with-input-file filepath read-string)))
    (lambda _ #f)))

(define* (discover-project-rules #:optional (workspace-dir (getcwd))
                                 #:key (candidates %standard-rule-candidates))
  "Discover and read rule files residing in WORKSPACE-DIR.
Returns an alist: ((path . filename) (content . content-string))."
  (let loop ((rest candidates)
             (found '()))
    (if (null? rest)
        (reverse found)
        (let* ((rel-path (car rest))
               (full-path (if (string-prefix? "/" rel-path)
                              rel-path
                              (string-append workspace-dir "/" rel-path))))
          (if (file-exists? full-path)
              (let ((content (read-rule-file full-path)))
                (if (non-empty-string? content)
                    (loop (cdr rest)
                          (cons `((path . ,rel-path)
                                  (content . ,(string-trim-both content)))
                                found))
                    (loop (cdr rest) found)))
              (loop (cdr rest) found))))))

;;;
;;; Tool Formatting
;;;
(define* (format-tools-for-prompt tools #:key (style 'xml))
  "Format available tool specifications for prompt reinforcement.
STYLE can be 'xml (structured tags) or 'markdown."
  (if (or (null? tools) (not (list? tools)))
      ""
      (case style
        ((xml)
         (let ((entries
                (map (lambda (tool)
                       (let* ((f (assoc-ref tool "function"))
                              (name (assoc-ref f "name"))
                              (desc (or (assoc-ref f "description") ""))
                              (params (assoc-ref f "parameters")))
                         (format #f "<tool>\n  <name>~a</name>\n  <description>~a</description>\n</tool>"
                                 name desc)))
                     tools)))
           (format #f "# Available Tools\n\nYou may invoke tools by emitting:\n<tool_call name=\"tool_name\">{\"arg\": \"val\"}</tool_call>\n\n~a\n"
                   (string-join entries "\n"))))
        (else
         (let ((entries
                (map (lambda (tool)
                       (let* ((f (assoc-ref tool "function"))
                              (name (assoc-ref f "name"))
                              (desc (or (assoc-ref f "description") "")))
                         (format #f "- **~a**: ~a" name desc)))
                     tools)))
           (format #f "## Available Tools\n~a\n" (string-join entries "\n")))))))

;;;
;;; Prompt Composer Record
;;;
(define-record-type <prompt-composer>
  (%make-prompt-composer identity git? tree? depth max-files rule-files custom-rules tool-style limit)
  prompt-composer?
  (identity prompt-composer-system-identity set-prompt-composer-system-identity!)
  (git? prompt-composer-include-git-status?)
  (tree? prompt-composer-include-file-tree?)
  (depth prompt-composer-file-tree-depth)
  (max-files prompt-composer-file-tree-max-files)
  (rule-files prompt-composer-rule-files)
  (custom-rules prompt-composer-custom-rules set-prompt-composer-custom-rules!)
  (tool-style prompt-composer-tool-style)
  (limit prompt-composer-context-limit-chars))

(define* (make-prompt-composer #:key
                               (system-identity #f)
                               (include-git-status? #t)
                               (include-file-tree? #t)
                               (file-tree-depth 2)
                               (file-tree-max-files 60)
                               (rule-files %standard-rule-candidates)
                               (custom-rules '())
                               (tool-style 'native)
                               (context-limit-chars 16000))
  "Construct a <prompt-composer> configuration record."
  (%make-prompt-composer
   (or system-identity
       "You are badb, an expert autonomous coding agent and interactive harness.\nYou have access to local tools and execute tasks methodically, verifying each step.")
   include-git-status?
   include-file-tree?
   file-tree-depth
   file-tree-max-files
   rule-files
   custom-rules
   tool-style
   context-limit-chars))

;;;
;;; Prompt Assembly
;;;
(define* (assemble-system-prompt composer
                                #:key
                                (workspace-dir (getcwd))
                                (tools '())
                                (mode 'code))
  "Assemble a unified, structured markdown system prompt incorporating persona,
workspace state, project guidelines, and tool instructions."
  (let* ((sections '())
         (add-sec! (lambda (header content)
                     (when (non-empty-string? content)
                       (set! sections (cons (format #f "## ~a\n~a" header (string-trim-both content))
                                            sections))))))

    ;; 1. Base Identity
    (set! sections (cons (prompt-composer-system-identity composer) sections))

    ;; 2. Mode Guidance
    (case mode
      ((code)
       (add-sec! "Operational Directives"
                 "Maintain code integrity. Inspect files before editing. Run tests after changes.
Never commit untracked roadmap files or temporary scratch files."))
      ((plan)
       (add-sec! "Operational Directives"
                 "Analyze the problem statement, identify dependencies, and outline a step-by-step
execution plan. Do not perform write operations."))
      ((fast)
       (add-sec! "Operational Directives"
                 "Provide direct, concise assessments with minimal preamble.")))

    ;; 3. Environment & Workspace
    (let* ((env (collect-environment-info))
           (env-str (format #f "- OS: ~a\n- Guile: ~a\n- Directory: ~a"
                            (assoc-ref env 'os)
                            (assoc-ref env 'guile-version)
                            workspace-dir)))
      (add-sec! "Environment" env-str))

    ;; 4. Git State
    (when (prompt-composer-include-git-status? composer)
      (let ((branch (get-git-branch workspace-dir))
            (status (get-git-status-summary workspace-dir)))
        (when (or branch status)
          (let ((git-str (format #f "- Branch: ~a\n- Status: ~a"
                                 (or branch "unknown")
                                 (or status "clean"))))
            (add-sec! "Git State" git-str)))))

    ;; 5. File Tree
    (when (prompt-composer-include-file-tree? composer)
      (let ((tree (build-workspace-file-tree
                   workspace-dir
                   #:max-depth (prompt-composer-file-tree-depth composer)
                   #:max-files (prompt-composer-file-tree-max-files composer))))
        (when tree
          (add-sec! "Workspace Outline" (format #f "```\n~a\n```" tree)))))

    ;; 6. Discovered Project Rules
    (let ((discovered (discover-project-rules
                       workspace-dir
                       #:candidates (prompt-composer-rule-files composer))))
      (for-each (lambda (rule-entry)
                  (let ((path (assoc-ref rule-entry 'path))
                        (content (assoc-ref rule-entry 'content)))
                    (add-sec! (format #f "Project Rule: ~a" path) content)))
                discovered))

    ;; 7. Custom Rules
    (let ((custom (prompt-composer-custom-rules composer)))
      (for-each (lambda (cr)
                  (if (string? cr)
                      (add-sec! "Instruction" cr)
                      (when (pair? cr)
                        (add-sec! (format #f "Rule: ~a" (car cr)) (cdr cr)))))
                custom))

    ;; 8. Tool Instructions (if non-native XML tool style)
    (when (and (eq? (prompt-composer-tool-style composer) 'xml)
               (pair? tools))
      (set! sections (cons (format-tools-for-prompt tools #:style 'xml) sections)))

    (string-join (reverse sections) "\n\n")))

(define* (default-system-prompt #:key
                                (workspace-dir (getcwd))
                                (tools '())
                                (mode 'code)
                                (custom-rules '()))
  "Convenience wrapper to construct and format default system prompt."
  (let ((composer (make-prompt-composer #:custom-rules custom-rules)))
    (assemble-system-prompt composer
                            #:workspace-dir workspace-dir
                            #:tools tools
                            #:mode mode)))
