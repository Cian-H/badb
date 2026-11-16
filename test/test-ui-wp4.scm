;;; test/test-ui-wp4.scm — SRFI-64 tests for WP4 (Menus, Commands, Views, Plots, CLI)

(define-module (test test-ui-wp4)
               #:use-module (srfi srfi-64)
               #:use-module (srfi srfi-1)
               #:use-module (ice-9 regex)
               #:use-module (geasa surfaces-tui menus)
               #:use-module (geasa surfaces-tui commands)
               #:use-module (geasa surfaces-tui editor)
               #:use-module (geasa surfaces-tui session)
               #:use-module (geasa foundation-config plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa policies-guardrail plugin)
               #:use-module (geasa runtimes-context plugin)
               #:use-module (geasa capabilities-context-index plugin)
               #:use-module (badb kernel registry)
               #:use-module (badb kernel journal)
               #:use-module (badb surfaces cli)
               #:use-module (badb main))

(define (%strip-ansi s)
        (regexp-substitute/global #f "\x1b\\[[0-9;]*m" s 'pre "" 'post))

(test-begin "ui-wp4")

;;;
;;; WP4.1: Nested Menus, Live Theme Preview & Provider/Synthesiser/Session Menus
;;;
(test-group "wp4.1-nested-menus-and-live-preview"
            (let* ((seen '())
                   (cancelled? #f)
                   (root (make-menu "Root"
                                    '((("A" . "First") . a)
                                      (("B" . "Second") . b)
                                      (("C" . "Third") . c))
                                    #:on-highlight (lambda (item)
                                                     (set! seen (cons (menu-item-action item) seen)))
                                    #:on-cancel (lambda () (set! cancelled? #t))))
                   (moved (menu-move root 1))
                   (child (menu-push moved "Child" '((("X" . "Sub") . x)))))
              (test-assert "menu? holds for extended <menu>" (menu? root))
              (test-equal "menu-move triggers on-highlight" '(b) seen)
              (test-equal "root depth is 0" 0 (menu-depth root))
              (test-equal "child depth is 1" 1 (menu-depth child))
              (test-equal "breadcrumbs list root and child" '("Root" "Child") (menu-breadcrumbs child))
              (test-equal "breadcrumb title joins with chevron" "Root › Child" (menu-breadcrumb-title child))
              (let ((popped (menu-pop child)))
                (test-assert "menu-pop from child returns parent menu" (menu? popped))
                (test-equal "popped menu title is Root" "Root" (menu-title popped))
                (test-assert "parent on-cancel not called when popping child" (not cancelled?))
                (let ((at-root (menu-pop popped)))
                  (test-equal "menu-pop from root returns #f" #f at-root)
                  (test-assert "menu-pop invokes on-cancel" cancelled?))))

            ;; Empty menu remains a strict no-op
            (let ((empty (make-menu "Empty" '())))
              (test-assert "menu-move on empty returns eq? menu" (eq? (menu-move empty 1) empty)))

            ;; Live theme preview & restore on cancel
            (let* ((active "badb-dark")
                   (m (make-theme-preview-menu "badb-dark"
                                               (lambda (name) (set! active name))))
                   (m1 (menu-move m 1)))
              (test-equal "moving theme preview menu updates active theme immediately"
                          (cdr (menu-item-action (menu-selected-item m1)))
                          active)
              (test-assert "previewed theme differs from initial theme"
                           (not (string=? active "badb-dark")))
              (menu-pop m1)
              (test-equal "canceling theme preview menu restores initial theme" "badb-dark" active))

            ;; Provider, Synthesiser & Session Resurrection menus
            (let* ((cfg (make-badb-config
                          #:model-endpoint "http://127.0.0.1:1234/v1"
                          #:options '((providers
                                       (lmstudio (endpoint "http://127.0.0.1:1234/v1")
                                        (models ("qwen3.5-35b" "qwen3-8b")))
                                       (mistral (endpoint "https://api.mistral.ai/v1")
                                        (models ("mistral-large"))))
                                      (agents
                                       (reviewer (model "r1"))
                                       (synthesis (persona "persona-synthesizer")
                                        (model "qwen3.6-35b-a3b"))))))
                   (prov-items (provider-menu-items cfg))
                   (synth-items (synthesizer-config-menu-items cfg)))
              (test-assert "provider-menu-items includes lmstudio marked active"
                           (any (lambda (it)
                                  (and (string=? (menu-item-label it) "lmstudio")
                                       (string-contains (menu-item-description it) "[active]")))
                                prov-items))
              (test-assert "provider-menu-items includes trailing prefill-endpoint"
                           (any (lambda (it) (eq? (menu-item-action it) 'prefill-endpoint)) prov-items))
              (test-equal "synthesizer-config-menu-items has Provider and Model items"
                          2 (length synth-items))
              (apply-synthesis-provider-config! cfg "mistral")
              (test-equal "apply-synthesis-provider-config! sets synthesis provider"
                          "mistral"
                          (get-prop (badb-config-get-agent cfg "synthesis") 'provider #f))
              (test-equal "apply-synthesis-provider-config! preserves synthesis model"
                          "qwen3.6-35b-a3b"
                          (badb-config-synthesis-model cfg))
              (test-equal "apply-synthesis-provider-config! preserves sibling agent role"
                          "r1"
                          (get-prop (badb-config-get-agent cfg "reviewer") 'model #f))
              (test-assert "synthesizer-provider-menu-items marks mistral active"
                           (any (lambda (it)
                                  (and (string=? (menu-item-label it) "mistral")
                                       (string-contains (menu-item-description it) "[active]")))
                                (synthesizer-provider-menu-items cfg)))
              (test-assert "synthesizer-model-menu-items includes recorded models"
                           (any (lambda (it) (string=? (menu-item-label it) "mistral-large"))
                                (synthesizer-model-menu-items cfg)))
              (apply-synthesis-provider-config! cfg #f)
              (test-assert "apply-synthesis-provider-config! #f clears override"
                           (not (get-prop (badb-config-get-agent cfg "synthesis") 'provider #f))))

            ;; Session resurrection menu items
            (let* ((tmp-dir (mkdtemp "/tmp/badb-wp4-sessions-XXXXXX"))
                   (sfile (string-append tmp-dir "/20261116T120000Z-123.jsonl")))
              (persist-session-file!
                sfile
                '(((role . "system") (content . "sys"))
                  ((role . "user") (content . "Fix alignment in CLI usage"))
                  ((role . "assistant") (content . "Sure"))))
              (let ((items (session-resurrection-menu-items tmp-dir)))
                (test-equal "session-resurrection-menu-items finds 1 session" 1 (length items))
                (test-assert "session description includes turn count and first user snippet"
                             (let ((desc (menu-item-description (car items))))
                               (and (string-contains desc "2 turn(s)")
                                    (string-contains desc "Fix alignment in CLI usage"))))
                (test-equal "session action carries resume-session path"
                            (cons 'resume-session sfile)
                            (menu-item-action (car items))))
              (system* "rm" "-rf" tmp-dir)))

;;;
;;; WP4.2: Command Deduplication, Aliasing & Nested Categories
;;;
(test-group "wp4.2-command-dedup-and-categories"
            (test-equal "ctx resolves to context" "context" (resolve-command-alias "ctx"))
            (test-equal "/CTX resolves to context" "context" (resolve-command-alias "/CTX"))
            (test-equal "exit resolves to quit" "quit" (resolve-command-alias "exit"))
            (test-equal "unknown command returns itself" "features" (resolve-command-alias "features"))

            (let ((deduped (deduplicate-slash-commands %slash-commands)))
              (test-assert "deduplicated palette keeps context" (assoc "context" deduped))
              (test-assert "deduplicated palette removes duplicate ctx" (not (assoc "ctx" deduped)))
              (test-assert "deduplicated palette removes duplicate exit" (not (assoc "exit" deduped)))
              (test-assert "context entry notes /ctx alias"
                           (string-contains (cdr (assoc "context" deduped)) "/ctx")))

            (let* ((root (make-commands-root-menu))
                   (sub (open-command-category-menu root "Model & Config")))
              (test-assert "commands root menu has categories" (> (menu-count root) 0))
              (test-equal "category submenu breadcrumbs" "Commands › Model & Config"
                          (menu-breadcrumb-title sub))
              (test-assert "Context & Knowledge category folds ctx into /context"
                           (let ((items (command-items-for-category "Context & Knowledge")))
                             (and (any (lambda (it) (string=? (menu-item-label it) "/context")) items)
                                  (not (any (lambda (it) (string=? (menu-item-label it) "/ctx")) items))))))

            ;; Unified execute-context-subcommand!
            (let* ((comp (make-context-compactor #:max-tokens 1000 #:reserve-tokens 200))
                   (hist '(((role . "user") (content . "hello"))))
                   (idx (make-bm25-index))
                   (out '())
                   (errs '())
                   (add-out (lambda (s) (set! out (cons s out))))
                   (add-err (lambda (s) (set! errs (cons s errs)))))
              (execute-context-subcommand! comp hist '() idx "metrics" add-out add-err)
              (execute-context-subcommand! comp hist '() idx "stats" add-out add-err)
              (test-equal "no errors from context subcommands" '() errs)
              (test-equal "two messages emitted" 2 (length out))
              (test-assert "metrics output has visual usage bar"
                           (string-contains (cadr out) "Conversation Context Usage"))))

;;;
;;; WP4.3: Clean Up Available Geasa (/features), Guardrails & Config UI Views
;;;
(test-group "wp4.3-views-cleanup"
            (let ((reg (make-registry)))
              (registry-attach! reg "fs_read" (lambda _ #t)
                                #:metadata '((description . "Read a file from workspace")
                                             (parameters . ((type . "object")))))
              (registry-attach! reg "custom_geis" (lambda _ #t)
                                #:metadata '((source . "project-geis")
                                             (bundle . "demo-bundle")
                                             (version . "1.2.0")))
              (let ((txt (format-features-list reg)))
                (test-assert "features view includes TOOL badge" (string-contains txt "[TOOL]"))
                (test-assert "features view includes GEIS badge" (string-contains txt "[GEIS]"))
                (test-assert "features view omits raw parameters alist" (not (string-contains txt "(parameters")))))

            (let* ((cfg (make-badb-config
                          #:model-name "qwen3.5"
                          #:model-endpoint "http://127.0.0.1:1234/v1"
                          #:options '((providers
                                       (local (endpoint "http://127.0.0.1:1234/v1")
                                        (models ("qwen3.5" "qwen3-8b")))))))
                   (policy (make-guardrail-policy #:level 'strict
                                                  #:rules (list (make-pattern-rule "bash" "rm" 'deny))))
                   (cfg-txt (format-config-summary cfg "qwen3.5" "http://127.0.0.1:1234/v1"
                                                   ".badb/journal.jsonl" #:policy policy))
                   (prov-txt (format-provider-presets (badb-config-providers cfg)
                                                      "http://127.0.0.1:1234/v1"))
                   (guard-txt (format-guardrail-status policy)))
              (test-assert "config view has sections and Guardrail badge"
                           (and (string-contains cfg-txt "Model & Inference")
                                (string-contains cfg-txt "Synthesiser")
                                (string-contains cfg-txt "[STRICT]")))
              (test-assert "provider view marks active preset"
                           (and (string-contains prov-txt "[ACTIVE]")
                                (string-contains prov-txt "qwen3.5 (+1)")))
              (test-assert "guardrail view marks active level and rule count"
                           (and (string-contains guard-txt "[STRICT]")
                                (string-contains guard-txt "1 custom rule(s)")
                                (string-contains guard-txt "▸ strict")))))

;;;
;;; WP4.4: Context Status Graph View & Shifting Geis Bindings Plot
;;;
(test-group "wp4.4-context-and-bindings-plots"
            (let* ((comp (make-context-compactor #:max-tokens 200 #:reserve-tokens 50))
                   (hist '(((role . "system") (content . "System rules here"))
                           ((role . "user") (content . "Please inspect the repo"))
                           ((role . "assistant") (content . "Checking now"))
                           ((role . "tool") (content . "output ok"))))
                   (txt (format-context-metrics comp hist '(1 2 3 4))))
              (test-assert "context graph includes segmented bar legend and roles"
                           (and (string-contains txt "Legend:")
                                (string-contains txt "system")
                                (string-contains txt "user")
                                (string-contains txt "assistant")
                                (string-contains txt "tool")
                                (string-contains txt "4 LLM turns")
                                (string-contains txt "4 UI cards"))))

            (test-equal "sparkline-plot renders ascending unicode blocks"
                        "▂▄▅▇█"
                        (sparkline-plot '(1 2 3 4 5)))
            (test-equal "sparkline-plot handles empty list" "" (sparkline-plot '()))

            (let* ((tmp-dir (mkdtemp "/tmp/badb-wp4-jrn-XXXXXX"))
                   (jrn (make-journal (string-append tmp-dir "/journal.jsonl")))
                   (reg (make-registry)))
              (journal-append! jrn '((feature-id . "feat-a") (checkpoint . "commit") (status . "pass")))
              (journal-append! jrn '((feature-id . "feat-b") (checkpoint . "commit") (status . "pass")))
              (journal-append! jrn '((feature-id . "feat-b") (checkpoint . "revert") (status . "revert")))
              (journal-append! jrn '((feature-id . "feat-c") (checkpoint . "commit") (status . "pass")))
              (registry-attach! reg "feat-a" (lambda () #t))
              (registry-attach! reg "feat-c" (lambda () #t))
              (let ((plot-txt (format-geis-bindings-graph jrn reg)))
                (test-assert "bindings plot includes sparkline, outcomes, and BOUND badge"
                             (and (string-contains plot-txt "Shift Trajectory")
                                  (string-contains plot-txt "✓✓✕✓")
                                  (string-contains plot-txt "[BOUND]")
                                  (string-contains plot-txt "2 feature(s) attached"))))
              (system* "rm" "-rf" tmp-dir)))

;;;
;;; WP4.5: Consistent CLI Item Formatting & Spacing
;;;
(test-group "wp4.5-cli-formatting-and-spacing"
            (let ((p (open-output-string)))
              (setenv "FORCE_COLOR" "1")
              (let* ((table (format-cli-table p '(("short" . "first desc")
                                                  ("much-longer" . "second desc"))
                                              #:width 14))
                     (plain (%strip-ansi table))
                     (lines (string-split plain #\newline)))
                (unsetenv "FORCE_COLOR")
                (test-equal "table emits 2 lines" 2 (length lines))
                (test-equal "ANSI colors do not misalign description columns"
                            (string-contains (car lines) "first desc")
                            (string-contains (cadr lines) "second desc"))))

            ;; Verify main --help and geis help alignment on plain output
            (let ((help-out (with-output-to-string
                                (lambda ()
                                        (setenv "NO_COLOR" "1")
                                        (main '("badb" "--help"))
                                        (unsetenv "NO_COLOR")))))
              (let* ((lines (string-split help-out #\newline))
                     (tui-line (find (lambda (l) (string-contains l "tui, (none)")) lines))
                     (acp-line (find (lambda (l) (string-contains l "acp")) lines))
                     (onb-line (find (lambda (l) (string-contains l "onboard")) lines)))
                (test-assert "help output includes onboard command" (string? onb-line))
                (test-equal "tui and acp descriptions align to identical column"
                            (string-contains tui-line "launch")
                            (string-contains acp-line "serve")))))

(define %wp4-fails (test-runner-fail-count (test-runner-current)))
(test-end "ui-wp4")

(when (and (pair? (command-line))
           (string=? (basename (car (command-line))) "test-ui-wp4.scm")
           (> %wp4-fails 0))
  (exit 1))
