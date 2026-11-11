;;; badb — Dynamic Project Context & Rules Assembly
;;;
;;; Discovers workspace guidelines (AGENTS.md, .badb/rules.scm, .cursorrules, CLAUDE.md),
;;; inspects git status and file trees, and dynamically synthesizes modular
;;; system prompts tailored for autonomous ReAct loops.

(define-module (geasa runtimes-prompt plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (geasa persona-badb plugin)
               #:use-module
               (git bindings)
               #:use-module
               (git repository)
               #:use-module
               (git reference)
               #:use-module
               (git branch)
               #:use-module
               (git status)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 optargs)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:use-module
               (pipe)
               #:export
               (<prompt-composer> make-prompt-composer
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
                                  split-system-prompt
                                  compile-system-prompt
                                  format-skill-catalog-section
                                  format-memory-catalog-section
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
         (ignore-errors
           (with-directory dir
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
                               #f)))))

;;;
;;; Git Inspection
;;;
(define* (get-git-branch #:optional (workspace-dir (getcwd)))
         "Return current git branch name in WORKSPACE-DIR, or #f if not a git repository."
         (catch #t
           (lambda ()
             (libgit2-init!)
             (let* ((repo (repository-open workspace-dir))
                    (head (repository-head repo)))
               (or (branch-name head)
                   (exec-cmd-output "git" '("branch" "--show-current") workspace-dir))))
           (lambda _
             (exec-cmd-output "git" '("branch" "--show-current") workspace-dir))))

(define* (get-git-status-summary #:optional (workspace-dir (getcwd)) #:key (max-lines 25))
         "Return a concise summary of git status in WORKSPACE-DIR, or #f if not a git repository."
         (catch #t
           (lambda ()
             (libgit2-init!)
             (let* ((repo (repository-open workspace-dir))
                    (status (status-list-new repo (make-status-options)))
                    (count (status-list-entry-count status)))
               (if (zero? count)
                 "clean (no changes)"
                 (let ((raw (exec-cmd-output "git" '("status" "--porcelain") workspace-dir)))
                   (cond
                     ((not raw) #f)
                     ((string-null? raw) "clean (no changes)")
                     (else
                       (let* ((lines (filter (lambda (l) (not (string-null? l)))
                                             (string-split raw #\newline)))
                              (n (length lines))
                              (trimmed (take lines (min n max-lines)))
                              (body (string-join trimmed "\n")))
                         (if (> n max-lines)
                           (format #f "~a\n... (~a more changes truncated)" body (- n max-lines))
                           body))))))))
           (lambda _
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
                       body))))))))

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
                               (loop (cons entry accum))))))
                       (sorted
                         (sort
                           (->> entries
                                (filter (lambda (name)
                                          (and (valid-dir-entry? name)
                                               (not (member name ignore-patterns))
                                               (not (string-prefix? "." name))
                                               (not (string-suffix? ".log" name))))))
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
                              (loop (cdr items) (cons entry-line current-acc)))))))))
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
              (file->string filepath)))
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
                       (->> tools
                            (map (lambda (tool)
                                   (let* ((f (assoc-ref tool "function"))
                                          (name (assoc-ref f "name"))
                                          (desc (or (assoc-ref f "description") "")))
                                     (format #f "<tool>\n  <name>~a</name>\n  <description>~a</description>\n</tool>"
                                             name desc)))))))
                 (format #f "# Available Tools\n\nYou may invoke tools by emitting:\n<tool_call name=\"tool_name\">{\"arg\": \"val\"}</tool_call>\n\n~a\n"
                         (string-join entries "\n"))))
             (else
               (let ((entries
                       (->> tools
                            (map (lambda (tool)
                                   (let* ((f (assoc-ref tool "function"))
                                          (name (assoc-ref f "name"))
                                          (desc (or (assoc-ref f "description") "")))
                                     (format #f "- **~a**: ~a" name desc)))))))
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
         "Construct a <prompt-composer> configuration record.  The default
system identity comes from the persona-badb geasa bundle; pass
#:system-identity to override it."
         (%make-prompt-composer
           (or system-identity (persona-identity))
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
(define (format-skill-catalog-section skills)
        "Format a list of skills or a formatted skill string into a markdown section.
SKILLS can be:
- a string (pre-rendered catalog)
- a list of skill alists, each having ((name . \"\") (description . \"\"))
Returns a string (or #f if empty)."
        (cond
          ((and (string? skills) (not (string-null? (string-trim-both skills))))
            (format #f "## Skills\n~a" (string-trim-both skills)))
          ((and (pair? skills) (not (null? skills)))
            (let ((lines (filter-map
                           (lambda (s)
                             (cond
                               ((string? s) (format #f "- ~a" s))
                               ((pair? s)
                                 (let ((name (or (assoc-ref s 'name) (assoc-ref s "name")))
                                       (desc (or (assoc-ref s 'description) (assoc-ref s "description") "")))
                                   (if name
                                     (format #f "- ~a: ~a" name desc)
                                     #f)))
                               (else #f)))
                           skills)))
              (if (null? lines)
                #f
                (format #f "## Available Skills\n~a" (string-join lines "\n")))))
          (else #f)))

(define (format-memory-catalog-section memories)
        "Format a list of memories or a formatted memory string into a markdown section.
MEMORIES can be:
- a string (pre-rendered memories)
- a list of memory alists or strings
Returns a string (or #f if empty)."
        (cond
          ((and (string? memories) (not (string-null? (string-trim-both memories))))
            (format #f "## Project Memories\n~a" (string-trim-both memories)))
          ((and (pair? memories) (not (null? memories)))
            (let ((lines (filter-map
                           (lambda (m)
                             (cond
                               ((string? m) (format #f "- ~a" m))
                               ((pair? m)
                                 (let ((title (or (prop-ref m 'title) (prop-ref m 'name)))
                                       (content (or (prop-ref m 'content) (prop-ref m 'description ""))))
                                   (if title
                                     (format #f "- ~a: ~a" title content)
                                     #f)))
                               (else #f)))
                           memories)))
              (if (null? lines)
                #f
                (format #f "## Project Memories\n~a" (string-join lines "\n")))))
          (else #f)))

(define*
  (%assemble-system-sections composer
                             #:key
                             (workspace-dir (getcwd))
                             (tools '())
                             (mode 'code)
                             (skills '())
                             (memories '()))
  "Build the system prompt's static and dynamic section lists in one
pass.  Returns (values STATIC-SECTIONS DYNAMIC-SECTIONS), each in
assembly order: identity, mode directives and tool-usage guidance are
static (stable within a session and safe for prompt-prefix caching);
environment, git state, workspace outline, discovered and custom
rules, skill/memory catalogs and XML tool instructions are dynamic."
  (let ((static '()) (dynamic '()))
    (define (add-static-sec! header content)
            (when (non-empty-string? content)
              (set! static
                    (cons (format #f "## ~a\n~a" header (string-trim-both content)) static))))
    (define (add-dynamic-sec! header content)
            (when (non-empty-string? content)
              (set! dynamic
                    (cons (format #f "## ~a\n~a" header (string-trim-both content)) dynamic))))
    ;; 1. Base Identity
    (set! static (cons (prompt-composer-system-identity composer) static))
    ;; 2. Mode Guidance
    (case mode
      ((code)
        (add-static-sec! "Operational Directives"
                         "Maintain code integrity. Inspect files before editing. Run tests after changes.
Never commit untracked roadmap files or temporary scratch files."))
      ((plan)
        (add-static-sec! "Operational Directives"
                         "Analyze the problem statement, identify dependencies, and outline a step-by-step
execution plan. Do not perform write operations."))
      ((fast)
        (add-static-sec! "Operational Directives"
                         "Provide direct, concise assessments with minimal preamble.")))
    ;; 2b. Tool Usage Guidance
    (add-static-sec! "Tool Usage"
                     "Prefer the read_file tool (path, offset, limit) to inspect files rather than
shelling out with cat/sed. read_file reports the file's total line count in its
header and annotates every line as N:HHH→content, where N is the 0-based line
number and HHH is the line's content anchor. For surgical edits, prefer the
edit_lines tool: pass ranges as {from, from_hash, to, to_hash, new_text} using
the anchors read_file showed you — the harness verifies them against the live
file and rejects edits based on stale reads instead of corrupting the wrong
lines. edit_file remains available for exact-text replacements. When you need
more than the default window, plan a few windowed reads (explicit offset/limit)
in advance instead of many tiny ones. Avoid re-reading a file you have already
read in this session — retain what you learned and move forward.")
    ;; 3. Environment & Workspace
    (let*
        ((env (collect-environment-info))
         (env-str
           (format #f
                   "- OS: ~a\n- Guile: ~a\n- Directory: ~a"
                   (assoc-ref env 'os)
                   (assoc-ref env 'guile-version)
                   workspace-dir)))
      (add-dynamic-sec! "Environment" env-str))
    ;; 4. Git State
    (when
        (prompt-composer-include-git-status? composer)
      (let
          ((branch (get-git-branch workspace-dir))
           (status (get-git-status-summary workspace-dir)))
        (when (or branch status)
          (let
              ((git-str
                 (format #f
                         "- Branch: ~a\n- Status: ~a"
                         (or branch "unknown")
                         (or status "clean"))))
            (add-dynamic-sec! "Git State" git-str)))))
    ;; 5. File Tree
    (when
        (prompt-composer-include-file-tree? composer)
      (let
          ((tree
             (build-workspace-file-tree workspace-dir
                                        #:max-depth
                                        (prompt-composer-file-tree-depth composer)
                                        #:max-files
                                        (prompt-composer-file-tree-max-files composer))))
        (when tree
          (add-dynamic-sec! "Workspace Outline" (format #f "```\n~a\n```" tree)))))
    ;; 6. Discovered Project Rules
    (let
        ((discovered
           (discover-project-rules workspace-dir
                                   #:candidates
                                   (prompt-composer-rule-files composer))))
      (for-each
        (lambda (rule-entry)
          (let
              ((path (assoc-ref rule-entry 'path)) (content (assoc-ref rule-entry 'content)))
            (add-dynamic-sec! (format #f "Project Rule: ~a" path) content)))
        discovered))
    ;; 7. Custom Rules
    (let
        ((custom (prompt-composer-custom-rules composer)))
      (for-each
        (lambda (cr)
          (if (string? cr)
            (add-dynamic-sec! "Instruction" cr)
            (when (pair? cr) (add-dynamic-sec! (format #f "Rule: ~a" (car cr)) (cdr cr)))))
        custom))
    ;; 7b. Skills Catalog
    (let
        ((sec (format-skill-catalog-section skills)))
      (when sec (set! dynamic (cons sec dynamic))))
    ;; 7c. Project Memories
    (let
        ((sec (format-memory-catalog-section memories)))
      (when sec (set! dynamic (cons sec dynamic))))
    ;; 8. Tool Instructions (if non-native XML tool style)
    (when
        (and (eq? (prompt-composer-tool-style composer) 'xml) (pair? tools))
      (set! dynamic (cons (format-tools-for-prompt tools #:style 'xml) dynamic)))
    (values (reverse static) (reverse dynamic))))

(define*
  (assemble-system-prompt composer
                          #:key
                          (workspace-dir (getcwd))
                          (tools '())
                          (mode 'code)
                          (skills '())
                          (memories '())
                          (static-only? #f))
  "Assemble a unified, structured markdown system prompt incorporating persona,
workspace state, project guidelines, and tool instructions.  With
STATIC-ONLY?, return just the stable prefix (identity and directives)
that callers may keep in a provider prompt cache (see split-system-prompt
for the two-piece form)."
  (call-with-values
    (lambda ()
      (%assemble-system-sections composer
                                 #:workspace-dir
                                 workspace-dir
                                 #:tools
                                 tools
                                 #:mode
                                 mode
                                 #:skills
                                 skills
                                 #:memories
                                 memories))
    (lambda (static-secs dynamic-secs)
      (string-join
        (if static-only?
          static-secs
          (append static-secs dynamic-secs))
        "\n\n"))))

(define*
  (split-system-prompt composer
                       #:key
                       (workspace-dir (getcwd))
                       (tools '())
                       (mode 'code)
                       (skills '())
                       (memories '()))
  "Return (STATIC . DYNAMIC): the system prompt split into its stable
and volatile halves for explicit prompt-prefix caching.  STATIC is the
identity/directives prefix (identical to assemble-system-prompt with
STATIC-ONLY?); DYNAMIC is the environment, git-state, rules and
catalog remainder.  (string-append STATIC \"\\n\\n\" DYNAMIC) is
byte-identical to (assemble-system-prompt COMPOSER ...) for the same
arguments.  Cache the STATIC half in the provider KV cache and pair
with (geasa providers-model cache) apply-cache-breakpoints to place
the cache breakpoint after it."
  (call-with-values
    (lambda ()
      (%assemble-system-sections composer
                                 #:workspace-dir
                                 workspace-dir
                                 #:tools
                                 tools
                                 #:mode
                                 mode
                                 #:skills
                                 skills
                                 #:memories
                                 memories))
    (lambda (static-secs dynamic-secs)
      (cons (string-join static-secs "\n\n") (string-join dynamic-secs "\n\n")))))

(define* (compile-system-prompt #:key
                                (composer #f)
                                (workspace-dir (getcwd))
                                (tools '())
                                (mode 'code)
                                (skills '())
                                (memories '())
                                (custom-rules '())
                                (static-only? #f))
         "Compile a modular system prompt with support for static prefix caching."
         (let ((comp (or composer (make-prompt-composer #:custom-rules custom-rules))))
           (assemble-system-prompt comp
                                   #:workspace-dir workspace-dir
                                   #:tools tools
                                   #:mode mode
                                   #:skills skills
                                   #:memories memories
                                   #:static-only? static-only?)))

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
