;;; test-prompt.scm — Tests for Project Context & Rules Assembly (badb prompt)

(define-module (test test-prompt)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (srfi srfi-1)
  #:use-module (badb runtimes prompt))

(spec-begin "runtimes-prompt")

;;;
;;; 1. Composer Construction
;;;

(test-group "prompt-composer-construction"
  (let ((composer (make-prompt-composer)))
    (test-assert "prompt-composer? recognizes record" (prompt-composer? composer))
    (test-assert "default system identity is string" (string? (prompt-composer-system-identity composer)))
    (test-assert "git status enabled by default" (prompt-composer-include-git-status? composer))
    (test-assert "file tree enabled by default" (prompt-composer-include-file-tree? composer))
    (test-equal "default tree depth is 2" 2 (prompt-composer-file-tree-depth composer))
    (test-equal "default tool style is native" 'native (prompt-composer-tool-style composer)))

  (let ((custom (make-prompt-composer
                 #:system-identity "Custom system identity"
                 #:include-git-status? #f
                 #:file-tree-depth 4
                 #:tool-style 'xml
                 #:custom-rules '("Never use tabs" "Always write tests"))))
    (test-equal "custom identity preserved" "Custom system identity" (prompt-composer-system-identity custom))
    (test-assert "git status disabled" (not (prompt-composer-include-git-status? custom)))
    (test-equal "custom tree depth" 4 (prompt-composer-file-tree-depth custom))
    (test-equal "custom tool style" 'xml (prompt-composer-tool-style custom))
    (test-equal "custom rules count is 2" 2 (length (prompt-composer-custom-rules custom)))))

;;;
;;; 2. Environment & Git Inspection
;;;

(test-group "prompt-environment-and-git"
  (let ((env (collect-environment-info)))
    (test-assert "environment contains os" (assoc-ref env 'os))
    (test-assert "environment contains guile-version" (assoc-ref env 'guile-version))
    (test-assert "environment contains working-dir" (assoc-ref env 'working-dir)))

  ;; Current project directory is a valid git repository
  (let ((branch (get-git-branch)))
    (test-assert "git branch returns string" (string? branch))
    (test-assert "branch name is not empty" (not (string-null? branch))))

  (let ((status (get-git-status-summary)))
    (test-assert "git status returns string" (string? status))))

;;;
;;; 3. File Tree Scanning
;;;

(test-group "prompt-file-tree"
  (let ((tree (build-workspace-file-tree (getcwd) #:max-depth 2 #:max-files 30)))
    (test-assert "file tree is string" (string? tree))
    (test-assert "file tree contains src directory" (string-contains tree "src/"))
    (test-assert "file tree contains branch glyphs" (string-contains tree "├── "))
    (test-assert "file tree ignores .git directory" (not (string-contains tree ".git/")))))

;;;
;;; 4. Project Rules Discovery
;;;

(test-group "prompt-rules-discovery"
  (let* ((tmp-dir "/tmp/badb-test-rules")
         (_ (system (format #f "rm -rf ~a && mkdir -p ~a/.badb" tmp-dir tmp-dir)))
         (rule-md (string-append tmp-dir "/AGENTS.md"))
         (rule-scm (string-append tmp-dir "/.badb/rules.scm")))

    (call-with-output-file rule-md
      (lambda (p) (display "# Project Guidelines\n- All code must pass make check.\n" p)))
    (call-with-output-file rule-scm
      (lambda (p) (display "(rules (preserve-comments #t) (tab-width 2))\n" p)))

    (let ((discovered (discover-project-rules tmp-dir)))
      (test-assert "discovered at least 2 rules files" (>= (length discovered) 2))

      (let ((agents-rule (find (lambda (r) (string=? (assoc-ref r 'path) "AGENTS.md")) discovered)))
        (test-assert "found AGENTS.md" (not (not agents-rule)))
        (test-assert "AGENTS.md content preserved"
          (string-contains (assoc-ref agents-rule 'content) "All code must pass make check")))

      (let ((scm-rule (find (lambda (r) (string=? (assoc-ref r 'path) ".badb/rules.scm")) discovered)))
        (test-assert "found .badb/rules.scm" (not (not scm-rule)))
        (test-assert "rules.scm content extracted"
          (string-contains (assoc-ref scm-rule 'content) "preserve-comments"))))

    (system (format #f "rm -rf ~a" tmp-dir))))

;;;
;;; 5. Tool Formatting for Prompts
;;;

(test-group "prompt-tool-formatting"
  (let* ((tools (list `(("type" . "function")
                        ("function" . (("name" . "execute_command")
                                       ("description" . "Run a shell command")
                                       ("parameters" . (("type" . "object"))))))))
         (xml-str (format-tools-for-prompt tools #:style 'xml))
         (md-str (format-tools-for-prompt tools #:style 'markdown)))

    (test-assert "xml prompt contains tool tags" (and (string-contains xml-str "<tool>")
           (string-contains xml-str "<name>execute_command</name>")
           (string-contains xml-str "Run a shell command")))

    (test-assert "md prompt contains markdown bold name"
      (string-contains md-str "- **execute_command**: Run a shell command"))))

;;;
;;; 6. Unified Prompt Assembly
;;;

(test-group "prompt-assembly"
  (let* ((composer (make-prompt-composer
                    #:system-identity "You are badb system prompt."
                    #:include-git-status? #t
                    #:include-file-tree? #t
                    #:tool-style 'xml
                    #:custom-rules '("Always maintain documentation integrity.")))
         (tools (list `(("type" . "function")
                        ("function" . (("name" . "read_file")
                                       ("description" . "Read file contents"))))))
         (prompt (assemble-system-prompt composer
                                         #:workspace-dir (getcwd)
                                         #:tools tools
                                         #:mode 'code)))

    (test-assert "prompt contains identity" (string-contains prompt "You are badb system prompt."))
    (test-assert "prompt contains Operational Directives" (string-contains prompt "## Operational Directives"))
    (test-assert "prompt contains Environment section" (string-contains prompt "## Environment"))
    (test-assert "prompt contains Git State section" (string-contains prompt "## Git State"))
    (test-assert "prompt contains Workspace Outline" (string-contains prompt "## Workspace Outline"))
    (test-assert "prompt contains Custom Instruction"
      (string-contains prompt "Always maintain documentation integrity."))
    (test-assert "prompt contains XML tool descriptions" (string-contains prompt "<name>read_file</name>")))

  ;; Test default-system-prompt convenience wrapper
  (let ((default-prompt (default-system-prompt #:custom-rules '("Rule 1"))))
    (test-assert "default-system-prompt generates non-empty string"
      (and (string? default-prompt) (> (string-length default-prompt) 100)))
    (test-assert "default-system-prompt includes custom rule" (string-contains default-prompt "Rule 1"))))

(spec-end)
(spec-exit)
