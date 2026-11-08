;;; (geasa surfaces-tui commands) — Slash Command Dispatch & Formatting Helpers
;;;
;;; Extracted from surfaces-tui plugin.scm. Provides slash command execution
;;; (/mcp, /config, /rules, /context, /usage, /features, /journal, /help).

(define-module (geasa surfaces-tui commands)
  #:use-module (geasa surfaces-tui menus)
  #:use-module (geasa foundation-config plugin)
  #:use-module (geasa capabilities-mcp plugin)
  #:use-module (geasa capabilities-context-index plugin)
  #:use-module (geasa capabilities-skills plugin)
  #:use-module (geasa capabilities-code-nav plugin)
  #:use-module (geasa runtimes-workflows plugin)
  #:use-module (geasa foundation-util plugin)
  #:use-module (geasa runtimes-prompt plugin)
  #:use-module (geasa runtimes-context plugin)
  #:use-module (geasa policies-guardrail plugin)
  #:use-module (badb kernel registry)
  #:use-module (badb kernel journal)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (execute-mcp-subcommand!
            execute-ctx-subcommand!
            execute-skills-subcommand!
            execute-memories-subcommand!
            execute-geis-bootstrap!
            execute-geis-distill!
            format-config-summary
            format-provider-presets
            format-rules-and-tree
            format-context-metrics
            format-usage-report
            format-features-list
            format-journal-recent
            %tui-help-message))

(define (execute-mcp-subcommand! mcp-mgr arg add-sys-msg! add-err-msg!)
  "Dispatch /mcp subcommands (list, start, stop, restart, add, remove, tools)."
  (let* ((tokens (split-words arg))
         (subcmd (if (null? tokens) "list" (car tokens)))
         (rest (if (null? tokens) '() (cdr tokens))))
    (string-case (if (string-null? arg) "list" subcmd)
      (("list" "ls")
       (let ((servers (mcp-manager-list-servers mcp-mgr)))
         (if (null? servers)
             (add-sys-msg!
              "No MCP servers configured.\nUse /mcp add <name> <cmd> [args...] to register one.")
             (let ((desc (map (lambda (s)
                                (let-props s (name
                                              command
                                              (s-args args '())
                                              (running? running? #f)
                                              (pid pid #f)
                                              tools
                                              (auto? auto-start #f))
                                  (let* ((t-count (if (list? tools) (length tools) 0))
                                         (auto-str (if auto? " (auto-start)" ""))
                                         (status (if running?
                                                     (format #f "[RUNNING pid:~a tools:~a]" pid t-count)
                                                     "[STOPPED]")))
                                    (format #f "• ~a ~a~a\n  command: ~a ~a"
                                            name status auto-str command (string-join s-args " ")))))
                              servers)))
               (add-sys-msg! (string-append "Configured MCP Servers:\n" (string-join desc "\n")))))))
      ("start"
      (let ((name (and (pair? rest) (car rest))))
        (if (or (not name) (string=? name "all"))
            (let* ((servers (mcp-manager-list-servers mcp-mgr))
                   (results (map (lambda (s)
                                   (let ((s-name (assoc-ref s 'name))
                                         (running? (assoc-ref s 'running?)))
                                     (if running?
                                         (format #f "• ~a: already running" s-name)
                                         (call-with-values
                                           (lambda () (mcp-manager-start-server! mcp-mgr s-name))
                                           (lambda (_client tools err)
                                             (if err
                                                 (format #f "• ~a: error (~a)" s-name err)
                                                 (format #f "• ~a: started (~a tools)" s-name (length tools))))))))
                                 servers)))
              (add-sys-msg! (string-append "Starting MCP servers:\n" (string-join results "\n"))))
            (call-with-values
              (lambda () (mcp-manager-start-server! mcp-mgr name))
              (lambda (_client tools err)
                (if err
                    (add-err-msg! err)
                    (add-sys-msg!
                     (format #f "Started MCP server '~a' (~a tools available)." name (length tools)))))))))
      ("stop"
       (let ((name (and (pair? rest) (car rest))))
         (if (or (not name) (string=? name "all"))
             (let ((servers (mcp-manager-list-servers mcp-mgr)))
               (mcp-manager-stop-all! mcp-mgr)
               (add-sys-msg! (format #f "Stopped all MCP servers (~a total)." (length servers))))
             (if (mcp-manager-stop-server! mcp-mgr name)
                 (add-sys-msg! (format #f "Stopped MCP server '~a'." name))
                 (add-err-msg! (format #f "MCP server '~a' was not running." name))))))
      ("restart"
       (let ((name (and (pair? rest) (car rest))))
         (if (or (not name) (string=? name "all"))
             (begin
               (mcp-manager-stop-all! mcp-mgr)
               (let* ((servers (mcp-manager-list-servers mcp-mgr))
                      (results (map (lambda (s)
                                      (let ((s-name (assoc-ref s 'name)))
                                        (call-with-values
                                          (lambda () (mcp-manager-start-server! mcp-mgr s-name))
                                          (lambda (_client tools err)
                                            (if err
                                                (format #f "• ~a: error (~a)" s-name err)
                                                (format #f "• ~a: restarted (~a tools)" s-name (length tools)))))))
                                    servers)))
                 (add-sys-msg! (string-append "Restarted MCP servers:\n" (string-join results "\n")))))
             (begin
               (mcp-manager-stop-server! mcp-mgr name)
               (call-with-values
                 (lambda () (mcp-manager-start-server! mcp-mgr name))
                 (lambda (_client tools err)
                   (if err
                       (add-err-msg! (format #f "Failed to restart '~a': ~a" name err))
                       (add-sys-msg!
                        (format #f "Restarted MCP server '~a' (~a tools available)."
                                name (length tools))))))))))
      ("add"
       (if (< (length rest) 2)
           (add-err-msg! "Usage: /mcp add <name> <command> [args...]")
           (let ((name (car rest)) (cmd (cadr rest)) (cmd-args (cddr rest)))
             (mcp-manager-add-server! mcp-mgr name cmd cmd-args #:auto-start #t)
             (add-sys-msg!
              (format #f "Added MCP server '~a' (~a ~a).\nSaved to ~a."
                      name cmd (string-join cmd-args " ") (default-local-config-path))))))
      (("remove" "rm")
       (if (null? rest)
           (add-err-msg! "Usage: /mcp remove <name>")
           (let ((name (car rest)))
             (if (mcp-manager-remove-server! mcp-mgr name)
                 (add-sys-msg! (format #f "Removed MCP server '~a'. Configuration updated." name))
                 (add-err-msg! (format #f "MCP server '~a' not found." name))))))
      ("tools"
       (let* ((target-server (and (pair? rest) (car rest)))
              (tools (mcp-manager-list-tools mcp-mgr target-server)))
         (if (null? tools)
             (add-sys-msg!
              (if target-server
                  (format #f "No tools available from server '~a' (is it running?)." target-server)
                  "No MCP tools available. Make sure servers are running with /mcp start <name>."))
             (let ((desc (map (lambda (t)
                                (format #f "• ~a (~a): ~a"
                                        (assoc-ref t 'name)
                                        (assoc-ref t 'server)
                                        (or (assoc-ref t 'description) "no description")))
                              tools)))
               (add-sys-msg! (string-append "Discovered MCP Tools:\n" (string-join desc "\n")))))))
      (else
       (add-err-msg!
        (format #f "Unknown /mcp subcommand '~a'. Valid: list, start, stop, restart, add, remove, tools"
                subcmd))))))

(define (format-config-summary cfg cur-model cur-endpoint cur-journal-path)
  "Format the /config show status report."
  (let* ((global-path (default-global-config-path))
         (local-path (default-local-config-path))
         (global? (file-exists? global-path))
         (local? (file-exists? local-path))
         (mcp-count (length (badb-config-mcp-servers cfg))))
    (format #f
            "badb Configuration:\n~
            • Model: ~a\n~
            • Endpoint: ~a\n~
            • Temperature: ~a\n~
            • Journal Path: ~a\n~
            • Global Config: ~a (~a)\n~
            • Local Config:  ~a (~a)\n~
            • MCP Servers Configured: ~a"
            cur-model
            cur-endpoint
            (badb-config-model-temperature cfg)
            cur-journal-path
            global-path
            (if global? "active" "not found")
            local-path
            (if local? "active" "not found")
            mcp-count)))

(define (format-provider-presets provs)
  "Format the list of configured provider presets for /provider."
  (if (null? provs)
      "No provider presets configured.\nDefine presets under (options (providers ...)) in .badb/config.scm."
      (string-append "Configured Provider Presets:\n"
                     (string-join
                      (map (lambda (p)
                             (format #f "• ~a -> ~a"
                                     (car p)
                                     (or (get-prop (cdr p) 'endpoint #f) "(no endpoint)")))
                           provs)
                      "\n")
                     "\nUsage: /provider <name> to switch the active preset.")))

(define (format-rules-and-tree workspace-dir)
  "Format discovered project rules and workspace file tree for /rules."
  (let* ((rules (discover-project-rules workspace-dir))
         (tree (build-workspace-file-tree workspace-dir #:max-depth 2 #:max-files 20))
         (rule-desc (if (null? rules)
                        "No project rule files found."
                        (string-join
                         (map (lambda (r) (format #f "• ~a" (assoc-ref r 'path))) rules)
                         "\n"))))
    (format #f "Discovered Project Rules:\n~a\n\nWorkspace Tree:\n~a" rule-desc tree)))

(define (format-context-metrics compactor llm-history messages)
  "Format conversation context metrics for /context."
  (let* ((tokens (estimate-conversation-tokens llm-history))
         (ceiling (- (context-compactor-max-tokens compactor)
                     (context-compactor-reserve-tokens compactor)))
         (needed? (compaction-needed? compactor llm-history)))
    (format #f
            "Conversation Context Metrics:\n• LLM Message Count: ~a\n• UI Cards Count: ~a\n• Estimated Tokens: ~a / ~a ceiling\n• Status: ~a"
            (length llm-history)
            (length messages)
            tokens
            ceiling
            (if needed? "COMPACTION REQUIRED" "NORMAL"))))

(define (execute-ctx-subcommand! ctx-idx arg add-sys-msg! add-err-msg!)
  "Dispatch /ctx subcommands: stats or search query."
  (let ((trimmed (string-trim-both arg)))
    (cond
     ((or (string-null? trimmed) (string=? trimmed "stats"))
      (add-sys-msg! (format-ctx-stats ctx-idx)))
     (else
      (add-sys-msg! (ctx-search ctx-idx trimmed))))))

(define (execute-skills-subcommand! skill-reg arg add-sys-msg! add-err-msg!)
  "Dispatch /skills subcommand: list or inspect active skills."
  (add-sys-msg! (skill-catalog-summary skill-reg)))

(define (execute-memories-subcommand! arg add-sys-msg! add-err-msg!)
  "Dispatch /memories subcommand: read project memories."
  (let ((topic (if (string-null? (string-trim-both arg)) "all" (string-trim-both arg))))
    (add-sys-msg! (memory-read topic))))

(define (execute-geis-bootstrap! add-sys-msg! add-err-msg!)
  "Bootstrap project geasa infrastructure (.badb/geasa, workflows, memories, rules)."
  (catch #t
    (lambda ()
      (bootstrap-project-geasa!)
      (add-sys-msg! "Project Geasa bootstrapped successfully:\n• .badb/geasa/\n• .badb/workflows/verify.sema\n• .badb/memories/architecture.md\n• .badb/rules.scm"))
    (lambda (k . a)
      (add-err-msg! (format #f "Failed to bootstrap project geasa: ~a ~s" k a)))))

(define (execute-geis-distill! arg add-sys-msg! add-err-msg!)
  "Distill recent trajectory patterns into a Geis bundle."
  (catch #t
    (lambda ()
      (let* ((analyze? (string-contains arg "--analyze-only"))
             (res (distill-exec-patterns! #:workspace (getcwd) #:analyze-only? (and analyze? #t))))
        (add-sys-msg! (format #f "Distill result: ~s" res))))
    (lambda (k . a)
      (add-err-msg! (format #f "Failed to distill trajectory: ~a ~s" k a)))))

(define* (format-usage-report compactor llm-history prompt-tokens completion-tokens estimated-tokens turns
                              #:key (ctx-idx #f))
  "Format per-session token usage report for /usage."
  (let* ((used (estimate-conversation-tokens llm-history))
         (ceiling (context-compactor-effective-ceiling compactor))
         (k format-token-kilo)
         (est-suffix (if (> estimated-tokens 0)
                         (format #f " (+~a estimated)" estimated-tokens)
                         ""))
         (base (format #f
                       "Session token usage:\n• Prompt tokens: ~a~a\n• Completion tokens: ~a\n• Turns: ~a\n• Context: ~a/~a tokens (estimated)"
                       prompt-tokens
                       est-suffix
                       completion-tokens
                       turns
                       (if (number? used) (k used) "?")
                       (if (> ceiling 0) (k ceiling) "?"))))
    (if ctx-idx
        (string-append base "\n\n" (format-ctx-stats ctx-idx))
        base)))

(define (format-features-list reg)
  "Format active registry features for /features."
  (let ((feats (registry-list reg)))
    (if (null? feats)
        "No features currently registered in kernel."
        (string-append
         "Registered Features:\n"
         (string-join
          (map (lambda (f)
                 (format #f "• ~a (~a)" (assoc-ref f 'id) (assoc-ref f 'metadata)))
               feats)
          "\n")))))

(define (format-journal-recent jrn)
  "Format recent checkpoint provenance entries for /journal."
  (let ((entries (journal-entries jrn)))
    (if (null? entries)
        "Journal is empty (0 checkpoints on disk)."
        (let* ((recent (take-right entries (min 8 (length entries))))
               (formatted (map (lambda (e)
                                 (format #f "  #~a: [~a] ~a -> ~a (~a)"
                                         (assoc-ref e "iteration")
                                         (assoc-ref e "checkpoint")
                                         (assoc-ref e "feature-id")
                                         (assoc-ref e "status")
                                         (assoc-ref e "timestamp")))
                               recent)))
          (string-append "Recent Checkpoints:\n" (string-join formatted "\n"))))))

(define %tui-help-message
  (string-append "Available Commands:\n"
                 "  /help             Display this help message\n"
                 "  /model [NAME]     Switch LLM model; no argument opens a menu\n"
                 "  /models           List models available on the endpoint\n"
                 "  /endpoint [URL]   Switch endpoint URL; no argument opens a menu\n"
                 "  /provider [NAME]  Switch or inspect provider preset\n"
                 "  /mcp [CMD]        Manage MCP servers (list, start, stop, add, remove, tools)\n"
                 "  /config [CMD]     Inspect, reload, save, set configuration; no argument opens a menu\n"
                 "  /guardrail [LVL]  Set policy level (disabled, permissive, cautious, strict, paranoid)\n"
                 "  /compact          Compact conversation history\n"
                 "  /resume           Resume a previous chat session from disk\n"
                 "  /checkpoint [DSC] Create a rollback checkpoint snapshot\n"
                 "  /rollback [ID]    Revert to latest or specified checkpoint\n"
                 "  /rules            View discovered project rules & workspace tree\n"
                 "  /geis [CMD]       Contract-gated dynamic synthesis (/geis add <desc>, list, verify, status, bootstrap, distill)\n"
                 "  /context          View conversation token metrics\n"
                 "  /ctx [stats|query] Search BM25 context index or show token virtualization savings\n"
                 "  /skills           List active agent skills\n"
                 "  /memories [TOPIC] Inspect persistent project memories\n"
                 "  /usage            Show per-session token usage report\n"
                 "  /steer [TEXT]     Queue steering text for the running or next agent run\n"
                 "  /interrupt        Halt the agent at its next iteration boundary\n"
                 "  /features         List active registry features\n"
                 "  /journal          View recent checkpoint provenance\n"
                 "  /test             Run the bootstrap test suite\n"
                 "  /clear            Clear chat history\n"
                 "  /quit, /exit      Exit TUI\n"
                 "  /dump [PATH]      Export the conversation to a text file\n"
                 "  !<command>        Execute shell command\n"
                 "  ,<expr> or (..)   Evaluate Scheme code\n"
                 "  ctrl+up / ctrl+down   Select a message block\n"
                 "  ctrl+e            Collapse or expand the selected message\n"
                 "  while agent runs  type to steer it: enter sends, ctrl+i interrupts\n"
                 "  menus             up/down move, enter apply, esc close (/config, /model, /endpoint)\n"
                 "  /sh <cmd>         Alias for shell command\n"
                 "  /eval <expr>      Alias for Scheme eval"))
