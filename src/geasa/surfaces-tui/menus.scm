;;; (geasa surfaces-tui menus) — Interactive Menu Model & Configuration Menus
;;;
;;; Extracted from surfaces-tui plugin.scm. Provides pure menu data structures,
;;; item normalization, navigation, live model menus, and configuration flows.

(define-module (geasa surfaces-tui menus)
               #:use-module
               (geasa foundation-config plugin)
               #:use-module
               (geasa surfaces-theme plugin)
               #:use-module
               (geasa surfaces-tui session)
               #:use-module
               (geasa providers-model openai)
               #:use-module
               (geasa policies-guardrail plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (ice-9 format)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:export
               (%menu-max-visible <menu>
                                  menu?
                                  menu-title
                                  menu-items
                                  menu-selected-index
                                  menu-on-highlight
                                  menu-on-cancel
                                  menu-parent
                                  normalize-menu-item
                                  make-menu
                                  menu-count
                                  menu-move
                                  menu-trigger-highlight!
                                  menu-push
                                  menu-pop
                                  menu-depth
                                  menu-breadcrumbs
                                  menu-breadcrumb-title
                                  menu-selected-item
                                  menu-visible-slice
                                  menu-item-label
                                  menu-item-description
                                  menu-item-action
                                  %theme-descriptions
                                  %guardrail-levels
                                  detect-model-decision
                                  build-model-menu-items
                                  config-recorded-models
                                  fetch-live-models
                                  build-live-or-recorded-model-menu-items
                                  endpoint-menu-items-for-config
                                  provider-menu-items
                                  synthesizer-config-menu-items
                                  synthesizer-provider-menu-items
                                  synthesizer-model-menu-items
                                  apply-synthesis-provider-config!
                                  session-resurrection-menu-items
                                  theme-menu-items
                                  make-theme-preview-menu
                                  guardrail-menu-items
                                  config-menu-items-for-state
                                  %command-categories
                                  command-category-menu-items
                                  command-items-for-category
                                  make-commands-root-menu
                                  open-command-category-menu))

;;;
;;; Interactive Menu Model
;;;
(define %menu-max-visible 12)

(define-record-type <menu>
                    (%make-menu title items selected-index on-highlight on-cancel parent)
                    menu?
                    (title menu-title)
                    (items menu-items)
                    (selected-index menu-selected-index)
                    (on-highlight menu-on-highlight)
                    (on-cancel menu-on-cancel)
                    (parent menu-parent))

(define (normalize-menu-item item)
        "Coerce raw ITEM into canonical ((label . description) . action) form."
        (let* ((head (if (pair? item) (car item) item))
               (label (if (pair? head) (car head) head))
               (desc (if (pair? head) (cdr head) #f))
               (action (if (pair? item) (cdr item) #f)))
          (cons (cons (ensure-string label) (ensure-string (or desc ""))) action)))

(define* (make-menu title items
                    #:key (selected-index 0)
                    (on-highlight #f)
                    (on-cancel #f)
                    (parent #f))
         "Create a menu titled TITLE from ITEMS, each of the form
((label . description) . action), with SELECTED-INDEX preselected,
optional ON-HIGHLIGHT / ON-CANCEL callbacks, and optional PARENT menu."
         (let* ((norm-items (map normalize-menu-item (if (list? items) items '())))
                (count (length norm-items))
                (idx (min (max 0 selected-index) (max 0 (- count 1)))))
           (%make-menu (ensure-string (or title "Menu"))
                       norm-items
                       idx
                       (and (procedure? on-highlight) on-highlight)
                       (and (procedure? on-cancel) on-cancel)
                       (and (menu? parent) parent))))

(define (menu-count menu)
        "Return the number of items in MENU."
        (length (menu-items menu)))

(define (menu-trigger-highlight! menu)
        "Invoke MENU's on-highlight callback with its currently selected item, if any."
        (let ((cb (and (menu? menu) (menu-on-highlight menu)))
              (item (and (menu? menu) (menu-selected-item menu))))
          (when (and (procedure? cb) item)
            (catch #t
              (lambda () (cb item))
              (lambda _ #f)))))

(define (menu-move menu delta)
        "Return a new MENU with the selection moved by DELTA positions, wrapping
around at both ends, and invoke any on-highlight callback for live preview.
Empty menus are returned unchanged."
        (let ((count (menu-count menu)))
          (if (= count 0)
            menu
            (let* ((m (remainder (+ (menu-selected-index menu) delta) count))
                   (next (%make-menu (menu-title menu)
                                     (menu-items menu)
                                     (if (< m 0) (+ m count) m)
                                     (menu-on-highlight menu)
                                     (menu-on-cancel menu)
                                     (menu-parent menu))))
              (menu-trigger-highlight! next)
              next))))

(define* (menu-push parent child-or-title
                    #:optional (items '())
                    #:key (selected-index 0)
                    (on-highlight #f)
                    (on-cancel #f))
         "Push a child menu onto PARENT's breadcrumb stack. CHILD-OR-TITLE may
be an existing <menu> record or a title string paired with ITEMS."
         (let ((parent-menu (and (menu? parent) parent)))
           (if (menu? child-or-title)
             (%make-menu (menu-title child-or-title)
                         (menu-items child-or-title)
                         (menu-selected-index child-or-title)
                         (menu-on-highlight child-or-title)
                         (menu-on-cancel child-or-title)
                         parent-menu)
             (make-menu child-or-title
                        items
                        #:selected-index selected-index
                        #:on-highlight on-highlight
                        #:on-cancel on-cancel
                        #:parent parent-menu))))

(define (menu-pop menu)
        "Cancel/close MENU by invoking its optional on-cancel callback and
returning its parent <menu> (or #f when already at root)."
        (and (menu? menu)
             (let ((cb (menu-on-cancel menu)))
               (when (procedure? cb)
                 (catch #t
                   (lambda ()
                     (let ((arity (procedure-minimum-arity cb)))
                       (if (and (pair? arity) (= (car arity) 0))
                         (cb)
                         (cb menu))))
                   (lambda _
                     #f)))
               (menu-parent menu))))

(define (menu-depth menu)
        "Return the nesting depth of MENU (0 for a root menu)."
        (if (and (menu? menu) (menu-parent menu))
          (+ 1 (menu-depth (menu-parent menu)))
          0))

(define (menu-breadcrumbs menu)
        "Return the list of menu titles from root down to MENU."
        (if (menu? menu)
          (let loop
            ((cur menu) (acc '()))
            (if cur
              (loop (menu-parent cur) (cons (menu-title cur) acc))
              acc))
          '()))

(define* (menu-breadcrumb-title menu #:optional (sep " › "))
         "Format MENU's breadcrumb path joined by SEP (e.g. \"Configuration › Theme\")."
         (string-join (menu-breadcrumbs menu) sep))

(define (menu-selected-item menu)
        "Return the currently selected item of MENU, or #f when it is empty."
        (let ((count (menu-count menu)))
          (if (= count 0)
            #f
            (list-ref (menu-items menu)
                      (min (max 0 (menu-selected-index menu)) (- count 1))))))

(define (menu-visible-slice menu max-visible)
        "Return (values visible-items offset): the window of at most MAX-VISIBLE
items of MENU positioned so the selected item stays on screen."
        (let* ((items (menu-items menu))
               (count (length items))
               (cap (max 1 max-visible))
               (sel (min (max 0 (menu-selected-index menu)) (max 0 (- count 1)))))
          (if (<= count cap)
            (values items 0)
            (let ((start (min (max 0 (- (+ sel 1) cap)) (- count cap))))
              (values (take (drop items start) cap) start)))))

(define (menu-item-label item)
        "Return the label string of menu ITEM."
        (if (and (pair? item) (pair? (car item))) (ensure-string (caar item)) ""))

(define (menu-item-description item)
        "Return the description string of menu ITEM."
        (if (and (pair? item) (pair? (car item))) (ensure-string (cdar item)) ""))

(define (menu-item-action item)
        "Return the action data carried by menu ITEM; #f when the item carries none."
        (if (pair? item) (cdr item) #f))

(define %theme-descriptions
        '(("badb-dark" . "Cyan & green on black (default)")
          ("midnight" . "Deep blue night palette")
          ("nord" . "Nord-inspired cool blues")
          ("solarized-dark" . "Solarized dark warm tones")
          ("paper" . "Light paper-like scheme")
          ("mono" . "Monochrome, no colors")
          ("tokyo-night" . "Blue-heavy modern night theme")
          ("tokyo-night-storm" . "Tokyo Night, deeper and muted")
          ("tokyo-night-day" . "Tokyo Night light variant")
          ("catppuccin-mocha" . "Pastel dark standard")
          ("catppuccin-latte" . "Pastel light counterpart")
          ("gruvbox-dark" . "Warm retro groove")
          ("gruvbox-light" . "Daytime groove")
          ("dracula" . "The famous purple-dark theme")
          ("kanagawa" . "Japanese wave palette")
          ("rose-pine" . "Soft sunset pastels")
          ("everforest-dark" . "Calm forest greens")
          ("monokai" . "Classic editor dark")
          ("onedark" . "Atom editor classic")))

(define %guardrail-levels
        '((disabled . "No guardrail restrictions")
          (permissive . "Ask only on high-risk commands")
          (cautious . "Deny destructive, ask mutations (default)")
          (strict . "Deny dangerous, ask for everything")
          (paranoid . "Deny all state mutations")))

;;;
;;; Model Auto-Detection & Live Model Menu Helpers
;;;
(define (detect-model-decision cur-model detected-model error)
        "Pure adopt/detect decision policy for startup model detection."
        (cond
          ((not (generic-model-name? cur-model)) (values cur-model 'skipped))
          (error (values #f 'failed))
          (detected-model (values detected-model 'adopted))
          (else (values #f 'unavailable))))

(define (build-model-menu-items model-ids active-model)
        "Pure construction of live model menu items: one item per MODEL-ID in
order, each carrying a (set-model . id) action."
        (append
          (map
            (lambda (id)
              (cons (cons id
                          (if (and (string? active-model)
                                   (string=? id active-model))
                            "active"
                            "available"))
                    (cons 'set-model id)))
            (if (list? model-ids) model-ids '()))
          (list (cons (cons "Set model..." "Type a model name manually")
                      'prefill-model))))

(define (config-recorded-models cfg)
        "Collect the model names recorded in CFG: the models of every provider
preset, deduplicated in declaration order."
        (delete-duplicates
          (append-map
            (lambda (p)
              (let ((raw (get-prop (cdr p) 'models #f)))
                (cond
                  ((string? raw) (list raw))
                  ((list? raw)
                    (filter-map
                      (lambda (m)
                        (cond
                          ((string? m) m)
                          ((and (pair? m) (string? (car m))) (car m))
                          ((and (pair? m) (string? (cdr m))) (cdr m))
                          (else #f)))
                      raw))
                  (else '()))))
            (badb-config-providers cfg))
          equal?))

(define (fetch-live-models client)
        "Query CLIENT's /models listing. Returns (values ids error)."
        (catch #t
          (lambda ()
            (call-with-values
              (lambda () (list-models client))
              (lambda (ids err)
                (if (or err (null? ids))
                  (values #f (or err "no models listed on endpoint"))
                  (values ids #f)))))
          (lambda (key . args)
            (values #f (format #f "model list request failed (~a): ~s" key args)))))

(define (build-live-or-recorded-model-menu-items client cfg cur-model)
        "Build the model menu items: live models from CLIENT when available,
otherwise CUR-MODEL plus models recorded in CFG provider presets, plus
the trailing manual entry item."
        (call-with-values
          (lambda () (fetch-live-models client))
          (lambda (live-ids _live-err)
            (if live-ids
              (build-model-menu-items live-ids cur-model)
              (let ((known (delete-duplicates (cons cur-model (config-recorded-models cfg)) equal?)))
                (append
                  (map (lambda (m)
                         (cons (cons m (if (string=? m cur-model)
                                         "current model"
                                         "recorded in config"))
                               (cons 'set-model m)))
                       known)
                  (list (cons (cons "Set model..." "Type a model name manually") 'prefill-model))))))))

(define (endpoint-menu-items-for-config cfg)
        "Build the endpoint menu items: configured provider presets in CFG plus
a manual entry item."
        (append
          (map (lambda (p)
                 (let ((name (ensure-string (car p))))
                   (cons (cons name (or (get-prop (cdr p) 'endpoint #f) "provider preset"))
                         (cons 'switch-provider name))))
               (badb-config-providers cfg))
          (list (cons (cons "Set endpoint..." "Type an endpoint URL manually")
                      'prefill-endpoint))))

(define (%provider-first-model props)
        "Return the primary model name listed in provider PROPS, or #f."
        (let ((raw (get-prop props 'models #f)))
          (cond
            ((string? raw) raw)
            ((and (pair? raw) (string? (car raw))) (car raw))
            ((and (pair? raw) (pair? (car raw)) (string? (caar raw))) (caar raw))
            (else #f))))

(define (%chat-providers-from-config cfg)
        "Return chat-kind provider presets from CFG, treating presets without
an explicit 'kind property as \"chat\"."
        (filter (lambda (entry)
                  (let ((prov (cdr entry)))
                    (equal? (or (get-prop prov 'kind #f) "chat") "chat")))
                (badb-config-providers cfg)))

(define* (provider-menu-items cfg #:optional (cur-endpoint #f))
         "Build interactive provider preset menu items from CFG, highlighting the
preset matching CUR-ENDPOINT (or CFG's model endpoint)."
         (let ((active-ep (or cur-endpoint (badb-config-model-endpoint cfg)))
               (provs (%chat-providers-from-config cfg)))
           (append
             (map (lambda (p)
                    (let* ((name (ensure-string (car p)))
                           (props (cdr p))
                           (ep (or (get-prop props 'endpoint #f) "(no endpoint)"))
                           (m (%provider-first-model props))
                           (active? (and (string? active-ep) (string=? ep active-ep)))
                           (badge (if active? "[active] " ""))
                           (desc (if m
                                   (format #f "~a~a · ~a" badge ep m)
                                   (format #f "~a~a" badge ep))))
                      (cons (cons name desc)
                            (cons 'switch-provider name))))
                  provs)
             (list (cons (cons "Set endpoint..." "Type an endpoint URL manually")
                         'prefill-endpoint)))))

(define (%synthesis-agent-provider cfg)
        "Return the configured synthesis agent provider name from CFG, or #f."
        (let ((role (badb-config-get-agent cfg "synthesis")))
          (and role (get-prop role 'provider #f))))

(define (apply-synthesis-provider-config! cfg provider-name)
        "Update or clear the 'provider property of the 'synthesis agent role in
CFG options while preserving its model, persona, and sibling agent roles."
        (let* ((opts (badb-config-options cfg))
               (agents-entry (or (assq 'agents opts) '(agents)))
               (roles (cdr agents-entry))
               (synth-entry (or (assq 'synthesis roles) '(synthesis)))
               (synth-body (filter (lambda (prop)
                                     (not (and (pair? prop) (eq? (car prop) 'provider))))
                                   (cdr synth-entry)))
               (new-synth-body (if provider-name
                                 (cons `(provider ,(ensure-string provider-name)) synth-body)
                                 synth-body))
               (new-synth-entry (cons 'synthesis new-synth-body))
               (new-roles (cons new-synth-entry
                                (filter (lambda (r)
                                          (not (and (pair? r) (eq? (car r) 'synthesis))))
                                        roles)))
               (new-agents-entry (cons 'agents new-roles))
               (new-opts (cons new-agents-entry
                               (filter (lambda (opt)
                                         (not (and (pair? opt) (eq? (car opt) 'agents))))
                                       opts))))
          (set-badb-config-options! cfg new-opts)
          cfg))

(define (synthesizer-config-menu-items cfg)
        "Build top-level menu items for configuring the default synthesiser
provider and model."
        (let ((synth-prov (%synthesis-agent-provider cfg))
              (synth-model (badb-config-synthesis-model cfg)))
          (list
            (cons (cons "Synthesis Provider"
                        (format #f "Default synthesiser provider (~a)"
                                (or synth-prov "inherit chat provider")))
                  'open-synth-provider-menu)
            (cons (cons "Synthesis Model"
                        (format #f "Default synthesiser model (~a)" synth-model))
                  'open-synth-model-menu))))

(define (synthesizer-provider-menu-items cfg)
        "Build menu items for choosing the default synthesiser provider preset."
        (let ((cur-prov (%synthesis-agent-provider cfg))
              (provs (badb-config-providers cfg)))
          (append
            (map (lambda (p)
                   (let* ((name (ensure-string (car p)))
                          (ep (or (get-prop (cdr p) 'endpoint #f) "provider preset"))
                          (active? (and cur-prov (string=? (ensure-string cur-prov) name)))
                          (desc (if active?
                                  (format #f "[active] ~a" ep)
                                  ep)))
                     (cons (cons name desc)
                           (cons 'set-synth-provider name))))
                 provs)
            (list (cons (cons "Inherit chat provider"
                              (if cur-prov
                                "Clear synthesis provider override"
                                "[active] Uses active chat provider"))
                        (cons 'set-synth-provider #f))))))

(define* (synthesizer-model-menu-items cfg #:optional (live-ids #f))
         "Build menu items for choosing the default synthesiser model from
LIVE-IDS (when available) and models recorded in CFG."
         (let* ((cur-synth (badb-config-synthesis-model cfg))
                (known (delete-duplicates
                         (cons cur-synth
                               (append (if (list? live-ids) live-ids '())
                                       (config-recorded-models cfg)))
                         equal?)))
           (append
             (map (lambda (m)
                    (cons (cons m (if (string=? m cur-synth)
                                    "[active] synthesis model"
                                    "available model"))
                          (cons 'set-synth-model m)))
                  known)
             (list (cons (cons "Set synthesis model..."
                               "Type a synthesis model name manually")
                         'prefill-synth-model)))))

(define (%session-first-user-snippet turns)
        "Extract a single-line preview of the first user turn in TURNS, or #f."
        (let ((user-turn (find (lambda (t)
                                 (and (pair? t)
                                      (equal? (assoc-ref t 'role) "user")
                                      (string? (assoc-ref t 'content))
                                      (not (string-null? (string-trim-both (assoc-ref t 'content))))))
                               turns)))
          (and user-turn
               (let* ((raw (assoc-ref user-turn 'content))
                      (one-line (string-join (string-tokenize (string-trim-both raw)) " ")))
                 (truncate-string one-line 36)))))

(define (session-resurrection-menu-items sessions-or-dir)
        "Build menu items for /resume session resurrection from SESSIONS-OR-DIR
(either a sessions directory path or a list of session JSONL paths).
Includes timestamp, turn count, and first user prompt snippet."
        (let ((paths (cond
                       ((string? sessions-or-dir) (recent-session-files sessions-or-dir))
                       ((list? sessions-or-dir) sessions-or-dir)
                       (else '()))))
          (map (lambda (path)
                 (let* ((turns (load-session-file path))
                        (turn-count (length (filter (lambda (m)
                                                      (not (equal? (assoc-ref m 'role) "system")))
                                                    turns)))
                        (mtime (session-mtime-string path))
                        (snippet (%session-first-user-snippet turns))
                        (label (truncate-string (basename path) %session-filename-max-length))
                        (desc (if snippet
                                (format #f "~a · ~a turn(s) · \"~a\"" mtime turn-count snippet)
                                (format #f "~a · ~a turn(s)" mtime turn-count))))
                   (cons (cons label desc)
                         (cons 'resume-session path))))
               paths)))

(define* (theme-menu-items #:optional (active-name #f))
         "Build the theme menu items from the built-in theme list, optionally
marking ACTIVE-NAME."
         (map (lambda (name)
                (let* ((base (or (assoc-ref %theme-descriptions name) "Built-in color theme"))
                       (desc (if (and (string? active-name) (string=? name active-name))
                               (format #f "[active] ~a" base)
                               base)))
                  (cons (cons name desc)
                        (cons 'set-theme name))))
              (all-theme-names)))

(define* (make-theme-preview-menu current-theme-name apply-theme!
                                  #:key (parent #f) (on-cancel #f) (title "Theme"))
         "Create a <menu> for theme selection with live on-highlight preview.
Moving across items invokes (APPLY-THEME! name) immediately; canceling via
menu-pop restores CURRENT-THEME-NAME and invokes ON-CANCEL if provided."
         (let* ((names (all-theme-names))
                (idx (or (and (string? current-theme-name)
                              (list-index (lambda (n) (string=? n current-theme-name)) names))
                         0))
                (items (theme-menu-items current-theme-name)))
           (make-menu title
                      items
                      #:selected-index idx
                      #:parent parent
                      #:on-highlight
                      (lambda (item)
                        (let ((act (menu-item-action item)))
                          (when (and (pair? act)
                                     (eq? (car act) 'set-theme)
                                     (procedure? apply-theme!))
                            (apply-theme! (cdr act)))))
                      #:on-cancel
                      (lambda ()
                        (when (and (string? current-theme-name)
                                   (procedure? apply-theme!))
                          (apply-theme! current-theme-name))
                        (when (procedure? on-cancel)
                          (on-cancel))))))

(define (guardrail-menu-items)
        "Build the guardrail level menu items."
        (map (lambda (entry)
               (cons (cons (symbol->string (car entry)) (cdr entry))
                     (cons 'set-guardrail (car entry))))
             %guardrail-levels))

(define (config-menu-items-for-state cfg cur-model cur-endpoint policy)
        "Build the top-level config menu items with live state woven into descriptions."
        (list
          (cons (cons "Model" (format #f "Switch active model (~a)" cur-model))
                'open-model-menu)
          (cons (cons "Provider" (format #f "Switch provider preset (~a)" cur-endpoint))
                'open-provider-menu)
          (cons (cons "Endpoint" (format #f "Switch endpoint (~a)" cur-endpoint))
                'open-endpoint-menu)
          (cons (cons "Synthesiser"
                      (format #f "Default synthesiser (~a)" (badb-config-synthesis-model cfg)))
                'open-synthesizer-menu)
          (cons (cons "Temperature"
                      (format #f "Set sampling temperature (~a)" (badb-config-model-temperature cfg)))
                'prefill-temperature)
          (cons (cons "Theme"
                      (format #f "Choose UI color theme (~a)" (or (badb-config-ui-theme cfg) "default")))
                'open-theme-menu)
          (cons (cons "Guardrail"
                      (format #f "Set policy level (~a)" (guardrail-policy-level policy)))
                'open-guardrail-menu)
          (cons (cons "Reload" "Reload configuration from disk") 'config-reload)
          (cons (cons "Save" "Save configuration to disk") 'config-save)))

;;;
;;; Categorized & Nested Slash Command Menus
;;;
(define %command-categories
        '(("Session & History"
           "Resume, compact, checkpoint, rollback, fork, export"
           (("resume" "Resume a previous chat session from disk" open-resume-menu)
            ("compact" "Compact conversation history context" (run-command . "compact"))
            ("checkpoint"
             "Create a workspace and registry checkpoint"
             (prefill-command . "/checkpoint "))
            ("rollback"
             "Revert to latest or specified checkpoint"
             (prefill-command . "/rollback "))
            ("fork"
             "Create a conversation branch at checkpoint"
             (prefill-command . "/fork "))
            ("dump"
             "Export conversation transcript to a text file"
             (prefill-command . "/dump "))
            ("clear" "Clear chat viewport history" (run-command . "clear"))))
          ("Model & Config"
           "Model, configuration, guardrails"
           (("model" "Switch or inspect active LLM model" open-model-menu)
            ("config" "Inspect, reload, or save badb configuration" open-config-menu)
            ("guardrail"
             "Inspect or set guardrail security policy level"
             open-guardrail-menu)))
          ("Context & Knowledge"
           "Token metrics (/ctx), rules, skills, memories"
           (("context"
             "Token metrics, bindings plot, or BM25 index (alias: /ctx)"
             (run-command . "context"))
            ("rules"
             "Inspect discovered project rules & workspace tree"
             (run-command . "rules"))
            ("skills" "List active agent skills" (run-command . "skills"))
            ("memories" "Inspect persistent project memories"
             (prefill-command . "/memories "))))
          ("Geasa & Synthesis" "Contract-gated synthesis"
           (("geis"
             "Contract-gated synthesis (/geis add|model|list|verify|bindings)"
             (prefill-command . "/geis "))))
          ("Tools & Execution"
           "MCP servers, shell passthrough, Scheme REPL, steering"
           (("mcp"
             "Manage MCP servers (list, start, stop, add, remove, tools)"
             (prefill-command . "/mcp "))
            ("sh"
             "Execute shell command (!<cmd>, alias: /shell)"
             (prefill-command . "/sh "))
            ("eval"
             "Evaluate Scheme code in REPL (,<expr>, alias: /scheme)"
             (prefill-command . "/eval "))
            ("steer"
             "Queue steering text for the running agent"
             (prefill-command . "/steer "))
            ("interrupt"
             "Halt the running agent at its next boundary"
             (run-command . "interrupt"))))
          ("Navigation & Help" "Help manual and exit"
           (("help"
             "Show categorized help manual & shortcuts (alias: /?)"
             (run-command . "help"))
            ("quit" "Exit badb microharness (alias: /exit)" (run-command . "quit"))))))

(define (command-category-menu-items)
        "Build top-level category menu items from %command-categories."
        (map (lambda (cat)
               (let* ((name (car cat))
                      (summary (cadr cat))
                      (cmds (caddr cat))
                      (desc (format #f "~a (~a)" summary (length cmds))))
                 (cons (cons name desc)
                       (cons 'open-command-category name))))
             %command-categories))

(define (command-items-for-category cat-name)
        "Return normalized menu items for the command category CAT-NAME, or '()."
        (let
            ((entry (assoc cat-name %command-categories)))
          (if entry
            (map
              (lambda (spec)
                (cons (cons (string-append "/" (car spec)) (cadr spec)) (caddr spec)))
              (caddr entry))
            '())))

(define* (make-commands-root-menu #:key (parent #f))
         "Create the root categorized commands menu."
         (make-menu "Commands" (command-category-menu-items) #:parent parent))

(define (open-command-category-menu parent-menu cat-name)
        "Push the command submenu for CAT-NAME onto PARENT-MENU's breadcrumb stack."
        (menu-push parent-menu cat-name (command-items-for-category cat-name)))
