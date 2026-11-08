;;; (geasa surfaces-tui menus) — Interactive Menu Model & Configuration Menus
;;;
;;; Extracted from surfaces-tui plugin.scm. Provides pure menu data structures,
;;; item normalization, navigation, live model menus, and configuration flows.

(define-module (geasa surfaces-tui menus)
  #:use-module (geasa foundation-config plugin)
  #:use-module (geasa surfaces-theme plugin)
  #:use-module (geasa providers-model common)
  #:use-module (geasa providers-model openai)
  #:use-module (geasa policies-guardrail plugin)
  #:use-module (geasa foundation-util plugin)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (%menu-max-visible
            <menu>
            menu?
            menu-title
            menu-items
            menu-selected-index
            normalize-menu-item
            make-menu
            menu-count
            menu-move
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
            theme-menu-items
            guardrail-menu-items
            config-menu-items-for-state))

;;;
;;; Interactive Menu Model
;;;

(define %menu-max-visible 12)

(define-record-type <menu>
  (%make-menu title items selected-index)
  menu?
  (title menu-title)
  (items menu-items)
  (selected-index menu-selected-index))

(define (normalize-menu-item item)
  "Coerce raw ITEM into canonical ((label . description) . action) form."
  (let* ((head (if (pair? item) (car item) item))
         (label (if (pair? head) (car head) head))
         (desc (if (pair? head) (cdr head) #f))
         (action (if (pair? item) (cdr item) #f)))
    (cons (cons (ensure-string label) (ensure-string (or desc ""))) action)))

(define* (make-menu title items #:key (selected-index 0))
  "Create a menu titled TITLE from ITEMS, each of the form
((label . description) . action), with SELECTED-INDEX preselected."
  (let* ((norm-items (map normalize-menu-item (if (list? items) items '())))
         (count (length norm-items))
         (idx (min (max 0 selected-index) (max 0 (- count 1)))))
    (%make-menu (ensure-string (or title "Menu")) norm-items idx)))

(define (menu-count menu)
  "Return the number of items in MENU."
  (length (menu-items menu)))

(define (menu-move menu delta)
  "Return a new MENU with the selection moved by DELTA positions, wrapping
around at both ends. Empty menus are returned unchanged."
  (let ((count (menu-count menu)))
    (if (= count 0)
        menu
        (let ((m (remainder (+ (menu-selected-index menu) delta) count)))
          (%make-menu (menu-title menu)
                      (menu-items menu)
                      (if (< m 0) (+ m count) m))))))

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

(define (theme-menu-items)
  "Build the theme menu items from the built-in theme list."
  (map (lambda (name)
         (cons (cons name (or (assoc-ref %theme-descriptions name) "Built-in color theme"))
               (cons 'set-theme name)))
       (all-theme-names)))

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
   (cons (cons "Endpoint" (format #f "Switch endpoint (~a)" cur-endpoint))
         'open-endpoint-menu)
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
