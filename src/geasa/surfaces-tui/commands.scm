;;; (geasa surfaces-tui commands) — Slash Command Dispatch & Formatting Helpers
;;;
;;; Extracted from surfaces-tui plugin.scm. Provides slash command execution
;;; (/mcp, /config, /rules, /context, /usage, /features, /journal, /help).

(define-module (geasa surfaces-tui commands)
               #:use-module
               (geasa surfaces-tui menus)
               #:use-module
               (geasa foundation-config plugin)
               #:use-module
               (geasa capabilities-mcp plugin)
               #:use-module
               (geasa capabilities-context-index plugin)
               #:use-module
               (geasa capabilities-skills plugin)
               #:use-module
               (geasa capabilities-code-nav plugin)
               #:use-module
               (geasa runtimes-workflows plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (geasa runtimes-prompt plugin)
               #:use-module
               (geasa runtimes-context plugin)
               #:use-module
               (geasa policies-guardrail plugin)
               #:use-module
               (badb kernel registry)
               #:use-module
               (badb kernel journal)
               #:use-module
               (ice-9 format)
               #:use-module
               (srfi srfi-1)
               #:export
               (execute-mcp-subcommand!
                 execute-context-subcommand!
                 execute-skills-subcommand!
                 execute-memories-subcommand!
                 execute-geis-bootstrap!
                 execute-geis-onboard!
                 execute-geis-distill!
                 %command-aliases
                 resolve-command-alias
                 deduplicate-slash-commands
                 format-config-summary
                 format-provider-presets
                 format-guardrail-status
                 format-rules-and-tree
                 format-context-metrics
                 sparkline-plot
                 format-geis-bindings-graph
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

;;;
;;; Command Aliasing & Palette Deduplication
;;;
(define %command-aliases
        '(("ctx" . "context") ("geasa" . "geis")
          ("exit" . "quit")
          ("shell" . "sh")
          ("scheme" . "eval")
          ("?" . "help")))

(define (resolve-command-alias cmd)
        "Resolve CMD (with or without leading '/') through %command-aliases to
its canonical command name."
        (let* ((clean (string-downcase (string-trim-both (ensure-string (or cmd "")))))
               (stripped (if (string-prefix? "/" clean) (substring clean 1) clean)))
          (or (assoc-ref %command-aliases stripped) stripped)))

(define (deduplicate-slash-commands commands)
        "Consolidate alias entries (e.g. 'ctx' -> 'context', 'exit' -> 'quit')
out of COMMANDS alist so the palette shows each command once, annotating the
canonical entry's description with any folded aliases."
        (let* ((present-aliases
                 (filter (lambda (pair)
                           (assoc (car pair) commands))
                         %command-aliases))
               (alias-names (map car %command-aliases)))
          (filter-map
            (lambda (entry)
              (and (pair? entry)
                   (let ((name (car entry))
                         (desc (ensure-string (cdr entry))))
                     (if (member name alias-names string=?)
                       #f
                       (let ((folded (filter-map
                                       (lambda (a)
                                         (and (string=? (cdr a) name)
                                              (string-append "/" (car a))))
                                       present-aliases)))
                         (if (or (null? folded)
                                 (string-contains (string-downcase desc) "alias"))
                           (cons name desc)
                           (cons name
                                 (format #f "~a (alias: ~a)"
                                         desc
                                         (string-join folded ", ")))))))))
            commands)))

;;;
;;; Formatting Helpers (Config, Providers, Guardrails, Features)
;;;
(define (%pad-col text width)
        "Pad TEXT on the right with spaces to at least WIDTH characters."
        (let* ((s (ensure-string (or text "")))
               (len (string-length s)))
          (if (>= len width)
            s
            (string-append s (make-string (- width len) #\space)))))

(define (%single-line-summary text max-len)
        "Collapse whitespace in TEXT to a single line truncated to MAX-LEN."
        (let* ((clean (string-join (string-tokenize (string-trim-both (ensure-string (or text "")))) " ")))
          (truncate-string clean max-len)))

(define* (format-config-summary cfg cur-model cur-endpoint cur-journal-path #:key (policy #f))
         "Format the /config show status report with aligned hierarchy and badges."
         (let* ((global-path (default-global-config-path))
                (local-path (default-local-config-path))
                (global? (file-exists? global-path))
                (local? (file-exists? local-path))
                (mcp-count (length (badb-config-mcp-servers cfg)))
                (prov-count (length (badb-config-providers cfg)))
                (synth-role (badb-config-get-agent cfg "synthesis"))
                (synth-prov (and synth-role (get-prop synth-role 'provider #f)))
                (synth-model (badb-config-synthesis-model cfg))
                (theme-name (badb-config-ui-theme cfg))
                (max-tok (badb-config-model-max-tokens cfg))
                (guard-line (if policy
                              (format #f "\n  ~a : [~a]"
                                      (%pad-col "Guardrail" 14)
                                      (string-upcase (symbol->string (guardrail-policy-level policy))))
                              "")))
           (format #f
                   "badb Configuration\n~a\n~
                    Model & Inference\n~
                      ~a : ~a\n~
                      ~a : ~a\n~
                      ~a : ~a\n~
                      ~a : ~a~a\n~
                    Synthesiser\n~
                      ~a : ~a\n~
                      ~a : ~a\n~
                    Storage & Files\n~
                      ~a : ~a\n~
                      ~a : ~a ~a\n~
                      ~a : ~a ~a\n~
                    Integrations\n~
                      ~a : ~a preset(s)\n~
                      ~a : ~a configured\n~
                      ~a : ~a"
                   (make-string 60 #\─)
                   (%pad-col "Model" 14) cur-model
                   (%pad-col "Endpoint" 14) cur-endpoint
                   (%pad-col "Temperature" 14) (badb-config-model-temperature cfg)
                   (%pad-col "Max Tokens" 14) (or max-tok "default")
                   guard-line
                   (%pad-col "Provider" 14) (or synth-prov "inherit active chat provider")
                   (%pad-col "Model" 14) synth-model
                   (%pad-col "Journal Path" 14) cur-journal-path
                   (%pad-col "Global Config" 14) (if global? "[ACTIVE] " "[MISSING]") global-path
                   (%pad-col "Local Config" 14) (if local? "[ACTIVE] " "[MISSING]") local-path
                   (%pad-col "Providers" 14) prov-count
                   (%pad-col "MCP Servers" 14) mcp-count
                   (%pad-col "UI Theme" 14) (or theme-name "badb-dark (default)"))))

(define (%provider-models-summary props)
        "Return a compact summary of the models configured in provider PROPS."
        (let ((raw (get-prop props 'models #f)))
          (cond
            ((string? raw) raw)
            ((and (list? raw) (not (null? raw)))
              (let* ((first-m (car raw))
                     (first-name (cond
                                   ((string? first-m) first-m)
                                   ((pair? first-m) (ensure-string (car first-m)))
                                   (else (ensure-string first-m))))
                     (extra (- (length raw) 1)))
                (if (> extra 0)
                  (format #f "~a (+~a)" first-name extra)
                  first-name)))
            (else "-"))))

(define* (format-provider-presets provs #:optional (active-endpoint #f))
         "Format the list of configured provider presets with aligned columns
and status badges for /provider."
         (if (null? provs)
           "Provider Presets (0)\n  No provider presets configured.\n  Define presets under (options (providers ...)) in .badb/config.scm."
           (let* ((name-w (min 22 (max 10 (+ 2 (fold (lambda (p acc)
                                                       (max acc (string-length (ensure-string (car p)))))
                                                     0
                                                     provs)))))
                  (ep-w (min 38 (max 16 (+ 2 (fold (lambda (p acc)
                                                     (let ((ep (or (get-prop (cdr p) 'endpoint #f) "(no endpoint)")))
                                                       (max acc (string-length ep))))
                                                   0
                                                   provs)))))
                  (header (format #f "  ~a ~a ~a ~a ~a"
                                  (%pad-col "STATUS" 9)
                                  (%pad-col "NAME" name-w)
                                  (%pad-col "KIND" 7)
                                  (%pad-col "ENDPOINT" ep-w)
                                  "MODELS"))
                  (rows
                    (map (lambda (p)
                           (let* ((name (ensure-string (car p)))
                                  (props (cdr p))
                                  (ep (or (get-prop props 'endpoint #f) "(no endpoint)"))
                                  (kind (ensure-string (or (get-prop props 'kind #f) "chat")))
                                  (active? (and (string? active-endpoint) (string=? ep active-endpoint)))
                                  (badge (if active? "[ACTIVE]" "[PRESET]"))
                                  (models (%provider-models-summary props)))
                             (format #f "  ~a ~a ~a ~a ~a"
                                     (%pad-col badge 9)
                                     (%pad-col name name-w)
                                     (%pad-col kind 7)
                                     (%pad-col (truncate-string ep (- ep-w 1)) ep-w)
                                     models)))
                         provs)))
             (string-append
               (format #f "Provider Presets (~a)\n~a\n~a\n"
                       (length provs)
                       (make-string 60 #\─)
                       header)
               (string-join rows "\n")
               "\nTip: /provider <name> to switch, or /provider for interactive menu."))))

(define (format-guardrail-status policy)
        "Format guardrail policy level, badge, rules summary, and level table."
        (let* ((level (if (guardrail-policy? policy)
                        (guardrail-policy-level policy)
                        (if (symbol? policy) policy 'cautious)))
               (rules (if (guardrail-policy? policy)
                        (guardrail-policy-rules policy)
                        '()))
               (evaluator (and (guardrail-policy? policy)
                               (guardrail-policy-evaluator policy)))
               (badge (string-upcase (symbol->string level)))
               (desc (or (assq-ref %guardrail-levels level) "Custom policy level"))
               (level-lines
                 (map (lambda (entry)
                        (let* ((lvl (car entry))
                               (marker (if (eq? lvl level) "▸" " "))
                               (lvl-name (%pad-col (symbol->string lvl) 12)))
                          (format #f "  ~a ~a ~a" marker lvl-name (cdr entry))))
                      %guardrail-levels)))
          (format #f
                  "Guardrail Policy Status\n~a\n~
                    ~a : [~a] ~a\n~
                    ~a : ~a custom rule(s)\n~
                    ~a : ~a\n\n~
                   Levels\n~a\n\n~
                   Usage: /guardrail <disabled|permissive|cautious|strict|paranoid>"
                  (make-string 60 #\─)
                  (%pad-col "Active Level" 14) badge desc
                  (%pad-col "Pattern Rules" 14) (length rules)
                  (%pad-col "Evaluator" 14) (if evaluator "[CONFIGURED]" "[DEFAULT]")
                  (string-join level-lines "\n"))))

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

;;;
;;; Context Status Graph & Geis Bindings Plot
;;;
(define %plot-bar-glyphs #("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"))

(define (sparkline-plot values)
        "Render a numeric list VALUES as a compact Unicode block sparkline (▁..█)."
        (if (or (not (list? values)) (null? values))
          ""
          (let* ((nums (map (lambda (v) (if (number? v) (max 0 v) 0)) values))
                 (hi (apply max nums)))
            (if (<= hi 0)
              (string-join (map (lambda (_) "▁") nums) "")
              (string-join
                (map (lambda (v)
                       (let* ((ratio (/ v hi))
                              (idx (min 7 (max 0 (inexact->exact (floor (* ratio 7.99)))))))
                         (vector-ref %plot-bar-glyphs idx)))
                     nums)
                "")))))

(define (%hbar count fill-char)
        "Repeat FILL-CHAR COUNT times (clamped >= 0)."
        (make-string (max 0 count) fill-char))

(define (%role-token-breakdown llm-history estimator)
        "Return an alist of ((role . tokens) ...) across LLM-HISTORY."
        (let ((sys 0) (usr 0) (ast 0) (tool 0) (other 0))
          (for-each
            (lambda (msg)
              (let ((role (message-role msg))
                    (tok (estimate-message-tokens msg estimator)))
                (cond
                  ((string=? role "system") (set! sys (+ sys tok)))
                  ((string=? role "user") (set! usr (+ usr tok)))
                  ((string=? role "assistant") (set! ast (+ ast tok)))
                  ((string=? role "tool") (set! tool (+ tool tok)))
                  (else (set! other (+ other tok))))))
            (if (list? llm-history) llm-history '()))
          `(("system" . ,sys)
            ("user" . ,usr)
            ("assistant" . ,ast)
            ("tool" . ,tool)
            ,@(if (> other 0) `(("other" . ,other)) '()))))

(define* (format-context-metrics compactor llm-history messages #:key (width 36))
         "Format conversation context metrics for /context with a segmented
usage bar and per-role token breakdown."
         (let* ((estimator (context-compactor-estimator compactor))
                (tokens (estimate-conversation-tokens llm-history estimator))
                (max-tok (context-compactor-max-tokens compactor))
                (reserve (context-compactor-reserve-tokens compactor))
                (ceiling (max 1 (- max-tok reserve)))
                (needed? (compaction-needed? compactor llm-history))
                (pct (min 999 (inexact->exact (round (* 100 (/ tokens ceiling))))))
                (bar-w (max 12 width))
                (breakdown (%role-token-breakdown llm-history estimator))
                (sys-tok (or (assoc-ref breakdown "system") 0))
                (usr-tok (or (assoc-ref breakdown "user") 0))
                (ast-tok (or (assoc-ref breakdown "assistant") 0))
                (tool-tok (or (assoc-ref breakdown "tool") 0))
                (denom (max ceiling tokens 1))
                (seg-cells (lambda (t)
                             (if (<= t 0)
                               0
                               (max 1 (inexact->exact (round (* bar-w (/ t denom))))))))
                (c-sys (seg-cells sys-tok))
                (c-usr (seg-cells usr-tok))
                (c-ast (seg-cells ast-tok))
                (c-tool (seg-cells tool-tok))
                ;; Clamp filled cells to bar-w while keeping proportions readable
                (filled (min bar-w (+ c-sys c-usr c-ast c-tool)))
                (free-cells (max 0 (- bar-w filled)))
                (raw-bar (string-append
                           (%hbar c-sys #\█)
                           (%hbar c-usr #\▓)
                           (%hbar c-ast #\▒)
                           (%hbar c-tool #\░)))
                (clipped-bar (if (> (string-length raw-bar) bar-w)
                               (substring raw-bar 0 bar-w)
                               (string-append raw-bar (%hbar free-cells #\·))))
                (role-lines
                  (map (lambda (entry)
                         (let* ((r-name (car entry))
                                (r-tok (cdr entry))
                                (r-pct (* 100.0 (/ r-tok (max 1 ceiling))))
                                (glyph-idx (if (<= r-tok 0)
                                             0
                                             (min 7 (max 0 (inexact->exact (floor (* (/ r-tok (max 1 ceiling)) 7.99)))))))
                                (glyph (if (<= r-tok 0) "·" (vector-ref %plot-bar-glyphs glyph-idx))))
                           (format #f "    ~a ~7d tok (~5,1f%)  ~a"
                                   (%pad-col r-name 11)
                                   r-tok
                                   r-pct
                                   glyph)))
                       breakdown)))
           (format #f
                   "Conversation Context Usage\n~a\n~
                    [~a] ~3d% (~a / ~a ceiling)\n~
                    Legend: █ system  ▓ user  ▒ assistant  ░ tool  · free\n\n~
                    Breakdown by Role\n~a\n~
                    ~a ~7d tok (window: ~a max)\n\n~
                    Messages: ~a LLM turns · ~a UI cards · Status: ~a"
                   (make-string 60 #\─)
                   clipped-bar pct tokens ceiling
                   (string-join role-lines "\n")
                   (%pad-col "    reserve" 15) reserve max-tok
                   (length llm-history)
                   (length messages)
                   (if needed? "[COMPACTION REQUIRED]" "[NORMAL]"))))

(define (%entry-verdict-kind entry)
        "Classify a journal entry into 'commit, 'revert, or 'other."
        (let ((status (string-downcase (ensure-string (or (journal-entry-ref entry 'status) ""))))
              (chk (string-downcase (ensure-string (or (journal-entry-ref entry 'checkpoint) ""))))
              (verdict (string-downcase (ensure-string (or (journal-entry-ref entry 'test-verdict) "")))))
          (cond
            ((or (string=? status "revert")
                 (string=? chk "revert")
                 (string=? verdict "fail")
                 (string=? verdict "revert"))
              'revert)
            ((or (string=? status "pass")
                 (string=? status "commit")
                 (string=? chk "commit")
                 (string=? verdict "pass"))
              'commit)
            (else 'other))))

(define* (format-geis-bindings-graph jrn #:optional (reg #f) #:key (limit 24))
         "Render a Unicode plot of shifting geis bindings and transaction history
from journal JRN (and optional active registry REG)."
         (let* ((entries (if (journal? jrn) (journal-entries jrn) '()))
                (reg-feats (if (registry? reg) (registry-list reg) '()))
                (reg-ids (map (lambda (f) (ensure-string (assoc-ref f 'id))) reg-feats)))
           (if (null? entries)
             (format #f
                     "Geis Bindings & Transaction Plot\n~a\n~
                      No transaction history recorded in journal yet.\n~
                      Active Registry Bindings: ~a"
                     (make-string 60 #\─)
                     (length reg-ids))
             (let* ((recent (take-right entries (min (max 1 limit) (length entries))))
                    ;; Track cumulative active bindings trajectory + commit/revert counts
                    (running 0)
                    (commits 0)
                    (reverts 0)
                    (per-feature (make-hash-table 31))
                    (series '())
                    (outcomes '()))
               (for-each
                 (lambda (e)
                   (let* ((fid (ensure-string (or (journal-entry-ref e 'feature-id) "unknown")))
                          (kind (%entry-verdict-kind e))
                          (ts (ensure-string (or (journal-entry-ref e 'timestamp) "")))
                          (prev (or (hash-ref per-feature fid) (list 0 0 0 "" #f))))
                     (case kind
                       ((commit)
                         (set! commits (+ commits 1))
                         (set! running (+ running 1))
                         (hash-set! per-feature fid
                                    (list (+ (car prev) 1) (+ (cadr prev) 1) (caddr prev) ts "COMMIT")))
                       ((revert)
                         (set! reverts (+ reverts 1))
                         (set! running (max 0 (- running 1)))
                         (hash-set! per-feature fid
                                    (list (+ (car prev) 1) (cadr prev) (+ (caddr prev) 1) ts "REVERT")))
                       (else
                         (hash-set! per-feature fid
                                    (list (+ (car prev) 1) (cadr prev) (caddr prev) ts "EVENT"))))
                     (set! series (cons (max 1 running) series))
                     (set! outcomes
                           (cons (case kind ((commit) "✓") ((revert) "✕") (else "·"))
                                 outcomes))))
                 recent)
               (let* ((spark (sparkline-plot (reverse series)))
                      (outcome-line (string-join (reverse outcomes) ""))
                      (feat-rows
                        (sort
                          (hash-map->list
                            (lambda (fid stats)
                              (list fid (car stats) (cadr stats) (caddr stats) (cadddr stats) (car (cddddr stats))))
                            per-feature)
                          (lambda (a b) (> (cadr a) (cadr b)))))
                      (max-events (fold (lambda (r acc) (max acc (cadr r))) 1 feat-rows))
                      (fid-w (min 24 (max 10 (+ 2 (fold (lambda (r acc) (max acc (string-length (car r))))
                                                        0 feat-rows)))))
                      (bar-lines
                        (map (lambda (r)
                               (let* ((fid (car r))
                                      (total (cadr r))
                                      (c-cnt (caddr r))
                                      (r-cnt (cadddr r))
                                      (last-ts (car (cddddr r)))
                                      (last-st (cadr (cddddr r)))
                                      (bound? (member fid reg-ids string=?))
                                      (badge (if bound? "[BOUND]" (format #f "[~a]" last-st)))
                                      (bar-len (max 1 (inexact->exact (round (* 14 (/ total max-events))))))
                                      (bar (make-string bar-len #\█)))
                                 (format #f "  ~a ~a ~a ~a (~a✓/~a✕) ~a"
                                         (%pad-col (truncate-string fid (- fid-w 1)) fid-w)
                                         (%pad-col badge 9)
                                         (%pad-col bar 14)
                                         (%pad-col (number->string total) 3)
                                         c-cnt
                                         r-cnt
                                         (if (string-null? last-ts) "" last-ts))))
                             (take feat-rows (min 10 (length feat-rows))))))
                 (format #f
                         "Geis Bindings & Transaction Plot (last ~a)\n~a\n~
                          Shift Trajectory : ~a  (net ~a)\n~
                          Tx Outcomes      : ~a  (✓ ~a commit · ✕ ~a revert)\n~a\n\n~
                          Per-Feature Binding Activity\n~a"
                         (length recent)
                         (make-string 60 #\─)
                         spark running
                         outcome-line commits reverts
                         (if (registry? reg)
                           (format #f "Active Bindings  : ~a feature(s) attached in registry" (length reg-ids))
                           (format #f "Total Journal Tx : ~a entries on disk" (length entries)))
                         (string-join bar-lines "\n")))))))

(define* (execute-context-subcommand! compactor llm-history messages ctx-idx arg
                                      add-sys-msg! add-err-msg!
                                      #:key (jrn #f) (reg #f))
         "Unified /context (and /ctx alias) subcommand dispatcher:
- empty or 'metrics': segmented context usage graph
- 'bindings' | 'plot' | 'graph': shifting geis bindings plot
- 'stats': BM25 context index stats
- 'search <query>' | '<query>': BM25 context index search"
         (let* ((trimmed (string-trim-both (or arg "")))
                (tokens (split-words trimmed))
                (sub (if (null? tokens) "" (string-downcase (car tokens))))
                (rest-str (if (and (pair? tokens) (pair? (cdr tokens)))
                            (string-join (cdr tokens) " ")
                            "")))
           (cond
             ((or (string-null? trimmed) (string=? sub "metrics"))
               (add-sys-msg! (format-context-metrics compactor llm-history messages)))
             ((member sub '("bindings" "plot" "graph"))
               (if (or (journal? jrn) (registry? reg))
                 (add-sys-msg! (format-geis-bindings-graph jrn reg))
                 (add-err-msg! "No journal or registry available for bindings plot.")))
             ((string=? sub "stats")
               (add-sys-msg! (format-ctx-stats ctx-idx)))
             ((string=? sub "search")
               (if (string-null? rest-str)
                 (add-err-msg! "Usage: /context search <query>")
                 (add-sys-msg! (ctx-search ctx-idx rest-str))))
             (else
               (add-sys-msg! (ctx-search ctx-idx trimmed))))))

(define (execute-skills-subcommand! skill-reg _arg add-sys-msg! _add-err-msg!)
        "Dispatch /skills subcommand: list or inspect active skills."
        (add-sys-msg! (skill-catalog-summary skill-reg)))

(define (execute-memories-subcommand! arg add-sys-msg! _add-err-msg!)
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

(define* (execute-geis-onboard! reg add-sys-msg! add-err-msg!)
         "Run project onboarding: detect toolchains and synthesize baseline project geasa."
         (catch #t
           (lambda ()
             (let ((res (onboard-project-geasa! (getcwd) #:reg reg)))
               (add-sys-msg!
                 (format #f "Project onboarding complete (~a):\n• Test runner synthesized: .badb/geasa/project-test-runner/\n• Discovered geasa index: .badb/geasa/index.json\n• Mounted into active registry: ~s"
                         (assoc-ref res 'language)
                         (assoc-ref res 'bundles)))))
           (lambda (k . a)
             (add-err-msg! (format #f "Failed to onboard project geasa: ~a ~s" k a)))))

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

(define (%feature-badge meta)
        "Infer a short status/origin badge for a registry entry's METADATA."
        (let ((source (and (list? meta) (get-prop meta 'source #f)))
              (bundle (and (list? meta) (get-prop meta 'bundle #f)))
              (server (and (list? meta) (get-prop meta 'server #f)))
              (params (and (list? meta) (get-prop meta 'parameters #f))))
          (cond
            ((or bundle (equal? source "project-geis")) "[GEIS]")
            ((or server (equal? source "mcp")) "[MCP]")
            (params "[TOOL]")
            (else "[ACTIVE]"))))

(define (%feature-summary meta)
        "Extract a readable one-line description for a registry entry's METADATA."
        (if (list? meta)
          (let
              ((desc (or (get-prop meta 'description #f) (get-prop meta 'summary #f)))
               (bundle (get-prop meta 'bundle #f))
               (ver (get-prop meta 'version #f)))
            (cond
              ((string? desc) (%single-line-summary desc 54))
              ((and bundle ver) (format #f "bundle ~a (v~a)" bundle ver))
              (bundle (format #f "bundle ~a" bundle))
              (ver (format #f "v~a" ver))
              (else "registered")))
          "registered"))

(define (format-features-list reg)
        "Format active registry features for /features with aligned columns and
status badges."
        (let ((feats (registry-list reg)))
          (if (null? feats)
            "Available Geasa / Registered Features (0)\n  No features currently registered in kernel."
            (let* ((id-w (min 28 (max 14 (+ 2 (fold (lambda (f acc)
                                                      (max acc (string-length (ensure-string (assoc-ref f 'id)))))
                                                    0
                                                    feats)))))
                   (header (format #f "  ~a ~a ~a"
                                   (%pad-col "FEATURE" id-w)
                                   (%pad-col "BADGE" 9)
                                   "DESCRIPTION"))
                   (rows (map (lambda (f)
                                (let* ((fid (ensure-string (assoc-ref f 'id)))
                                       (meta (assoc-ref f 'metadata))
                                       (badge (%feature-badge meta))
                                       (summary (%feature-summary meta)))
                                  (format #f "  ~a ~a ~a"
                                          (%pad-col (truncate-string fid (- id-w 1)) id-w)
                                          (%pad-col badge 9)
                                          summary)))
                              feats)))
              (string-append
                (format #f "Available Geasa / Registered Features (~a)\n~a\n~a\n"
                        (length feats)
                        (make-string 60 #\─)
                        header)
                (string-join rows "\n"))))))

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
        (string-append
          "Available Commands (grouped by category)\n"
          (make-string 60 #\─) "\n"
          "Session & History\n"
          "  /resume           Resume a previous chat session from disk\n"
          "  /compact          Compact conversation history\n"
          "  /checkpoint [DSC] Create a rollback checkpoint snapshot\n"
          "  /rollback [ID]    Revert to latest or specified checkpoint\n"
          "  /fork [ID]        Branch conversation at current or saved checkpoint\n"
          "  /replay <ID>      Restore conversation and workspace to a checkpoint\n"
          "  /dump [PATH]      Export the conversation to a text file\n"
          "  /clear            Clear chat history\n\n"
          "Model & Configuration\n"
          "  /model [NAME]     Switch LLM model (no argument opens menu)\n"
          "  /models           List models available on the endpoint\n"
          "  /provider [NAME]  Switch or inspect provider preset\n"
          "  /endpoint [URL]   Switch endpoint URL (no argument opens menu)\n"
          "  /config [CMD]     Inspect, reload, save, set config (no arg opens menu)\n"
          "  /theme [NAME]     Switch UI theme with live preview\n"
          "  /guardrail [LVL]  Set policy level (disabled..paranoid)\n\n"
          "Context & Knowledge\n"
          "  /context [SUB]    Token metrics, bindings plot, or BM25 index\n"
          "                    Subcommands: metrics | bindings | stats | search <q>\n"
          "                    Alias: /ctx [stats|query]\n"
          "  /usage            Show per-session token usage report\n"
          "  /rules            View discovered project rules & workspace tree\n"
          "  /skills           List active agent skills\n"
          "  /memories [TOPIC] Inspect persistent project memories\n\n"
          "Geasa & Synthesis\n"
          "  /geis [CMD]       Contract synthesis (add, model, list, verify, status, bootstrap, distill)\n"
          "  /features         List available geasa & kernel registry features\n"
          "  /journal          View recent checkpoint provenance\n"
          "  /test             Run the bootstrap test suite\n\n"
          "Tools & Execution\n"
          "  /mcp [CMD]        Manage MCP servers (list, start, stop, add, remove, tools)\n"
          "  /sh <cmd>         Execute shell command (!<cmd>, alias: /shell)\n"
          "  /eval <expr>      Evaluate Scheme code (,<expr> or (..), alias: /scheme)\n"
          "  /steer [TEXT]     Queue steering text for the running or next agent run\n"
          "  /interrupt        Halt the agent at its next iteration boundary\n\n"
          "Navigation & Shortcuts\n"
          "  /copy [code]      Copy last assistant reply or fenced code block\n"
          "  /fold [state]     Fold or unfold all tool & thinking cards (ctrl+o)\n"
          "  /find [query]     Search transcript with n/N match navigation (ctrl+f)\n"
          "  /help             Display this help message (alias: /?)\n"
          "  /quit             Exit TUI (alias: /exit)\n"
          "  ctrl+up / down    Select a message block  |  ctrl+e: expand/collapse\n"
          "  ctrl+z / ctrl+_   Undo input buffer edit  |  ctrl+u / ctrl+w: kill line/word\n"
          "  while running     type to steer: enter sends, ctrl+i interrupts\n"
          "  menus             up/down move, enter apply, esc/left back or close"))
