;;; (geasa surfaces-context-inspector plugin) — /context & /usage modal

(define-module (geasa surfaces-context-inspector plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (context-inspector-state?
                          context-inspector-budget
                          context-inspector-items
                          context-inspector-usage
                          context-inspector-filter-query
                          context-inspector-status-message
                          make-context-inspector-overlay
                          register-context-inspector-tui!))

(define-record-type <context-inspector-state>
                    (%make-context-inspector-state
                      budget items usage items-list filter-query filter-active?
                      on-compact status-message)
                    context-inspector-state?
                    (budget context-inspector-budget set-context-inspector-budget!)
                    (items context-inspector-items set-context-inspector-items!)
                    (usage context-inspector-usage set-context-inspector-usage!)
                    (items-list context-inspector-items-list set-context-inspector-items-list!)
                    (filter-query context-inspector-filter-query set-context-inspector-filter-query!)
                    (filter-active? context-inspector-filter-active? set-context-inspector-filter-active?!)
                    (on-compact context-inspector-on-compact)
                    (status-message context-inspector-status-message set-context-inspector-status-message!))

(define (alist-get alist key default)
        (let ((pair (assq key alist)))
          (if pair (cdr pair) default)))

(define (kind->str k)
        (cond
          ((string? k) k)
          ((symbol? k) (symbol->string k))
          (else (format #f "~a" (or k "item")))))

(define (string-contains-ci? haystack needle)
        (let ((hlen (string-length haystack))
              (nlen (string-length needle)))
          (if (= nlen 0)
            #t
            (let ((h (string-downcase haystack))
                  (n (string-downcase needle)))
              (let loop ((i 0))
                        (cond
                          ((> (+ i nlen) hlen) #f)
                          ((string= (substring h i (+ i nlen)) n) #t)
                          (else (loop (+ i 1)))))))))

(define (filter-items items query)
        (if (string=? query "")
          items
          (filter (lambda (item)
                    (let ((name (alist-get item 'name ""))
                          (kind (kind->str (alist-get item 'kind 'item))))
                      (or (string-contains-ci? name query)
                          (string-contains-ci? kind query))))
                  items)))

(define (refresh-items-list! st)
        (let ((filtered (filter-items (context-inspector-items st)
                                      (context-inspector-filter-query st))))
          (set-context-inspector-items-list!
            st
            (make-selectable-list filtered #:selected-index 0 #:max-visible 10))))

(define (meter-role ratio)
        (cond
          ((>= ratio 0.90) 'error)
          ((>= ratio 0.75) 'warn)
          (else 'ok)))

(define (render-meter-row label used total width)
        (let* ((safe-total (max 1 total))
               (ratio (min 1.0 (max 0.0 (/ (* 1.0 used) safe-total))))
               (bar-width (max 10 (min 24 (- width 36))))
               (filled (inexact->exact (round (* ratio bar-width))))
               (empty (- bar-width filled))
               (bar (string-append "["
                                   (make-string filled #\#)
                                   (make-string empty #\-)
                                   "]"))
               (pct (inexact->exact (round (* ratio 100)))))
          (hstack-widget
            (list (text-widget (pad-str label 18) #:bold? #t)
                  (text-widget bar #:role (meter-role ratio))
                  (text-widget (format #f " ~3d% (~a/~a)" pct used total) #:dim? #t))
            #:gap 1)))

(define (render-budget-tab st width)
        (let* ((budget (context-inspector-budget st))
               (total (alist-get budget 'total 128000))
               (used (alist-get budget 'used 0))
               (breakdown (alist-get budget 'breakdown '()))
               (rows (map (lambda (entry)
                            (render-meter-row (car entry) (cdr entry) total width))
                          breakdown)))
          (vstack-widget
            (append (list (render-meter-row "Total Window" used total width)
                          (rule-widget))
                    (if (null? rows)
                      (list (text-widget "No breakdown data available." #:dim? #t))
                      rows))
            #:gap 1)))

(define (render-items-tab st width)
        (let* ((filter-bar
                 (if (context-inspector-filter-active? st)
                   (text-widget (format #f "Filter: /~a_" (context-inspector-filter-query st))
                                #:role 'accent #:bold? #t)
                   (text-widget "Press / to filter active rules, memories, and tools" #:dim? #t)))
               (slist (context-inspector-items-list st))
               (body
                 (if (null? (selectable-list-items slist))
                   (text-widget "No matching active items." #:dim? #t)
                   (selectable-list-widget
                     slist
                     #:item-renderer
                     (lambda (item selected?)
                       (let ((kind (kind->str (alist-get item 'kind 'item)))
                             (name (alist-get item 'name ""))
                             (toks (alist-get item 'tokens 0)))
                         (hstack-widget
                           (list (text-widget (if selected? ">" " ") #:bold? selected?)
                                 (pill-widget kind #:role 'accent)
                                 (text-widget (clip-str name (max 12 (- width 26))) #:bold? selected?)
                                 (text-widget (format #f "~a tok" toks) #:dim? #t))
                           #:gap 1)))))))
          (vstack-widget (list filter-bar body) #:gap 1)))

(define (render-usage-tab st)
        (let* ((usage (context-inspector-usage st))
               (prompt-tok (alist-get usage 'prompt-tokens 0))
               (comp-tok (alist-get usage 'completion-tokens 0))
               (cached-tok (alist-get usage 'cached-tokens 0))
               (turns (alist-get usage 'turns 0)))
          (vstack-widget
            (list (text-widget (format #f "Prompt tokens:     ~a" prompt-tok))
                  (text-widget (format #f "Completion tokens: ~a" comp-tok))
                  (text-widget (format #f "Cached tokens:     ~a" cached-tok))
                  (text-widget (format #f "Turns:             ~a" turns)))
            #:gap 1)))

(define (render-context-body ov st rect)
        (let* ((tab (modal-active-tab ov))
               (width (rect-width rect))
               (status (context-inspector-status-message st))
               (content
                 (case tab
                   ((budget) (render-budget-tab st width))
                   ((items) (render-items-tab st width))
                   ((usage) (render-usage-tab st))
                   (else (text-widget "Unknown tab" #:dim? #t)))))
          (if status
            (vstack-widget (list content (text-widget status #:role 'ok #:bold? #t)) #:gap 1)
            content)))

(define (render-context-detail ov st _rect)
        (let ((tab (modal-active-tab ov)))
          (if (eq? tab 'items)
            (let ((item (list-selected-item (context-inspector-items-list st))))
              (if item
                (vstack-widget
                  (list (text-widget (alist-get item 'name "") #:bold? #t)
                        (text-widget (format #f "Kind: ~a | Tokens: ~a"
                                             (alist-get item 'kind 'item)
                                             (alist-get item 'tokens 0))
                                     #:dim? #t)
                        (rule-widget)
                        (text-widget (alist-get item 'detail "No detail available.")))
                  #:gap 1)
                (text-widget "No item selected." #:dim? #t)))
            (text-widget "Detail view available on Active Items tab." #:dim? #t))))

(define (handle-filter-key st key)
        (cond
          ((eq? key 'escape)
            (set-context-inspector-filter-active?! st #f)
            (set-context-inspector-filter-query! st "")
            (refresh-items-list! st)
            #t)
          ((eq? key 'enter)
            (set-context-inspector-filter-active?! st #f)
            #t)
          ((or (eq? key 'backspace) (equal? key #\backspace) (equal? key #\del))
            (let ((q (context-inspector-filter-query st)))
              (when (> (string-length q) 0)
                (set-context-inspector-filter-query! st (substring q 0 (- (string-length q) 1)))
                (refresh-items-list! st)))
            #t)
          ((and (char? key) (>= (char->integer key) 32))
            (set-context-inspector-filter-query!
              st
              (string-append (context-inspector-filter-query st) (string key)))
            (refresh-items-list! st)
            #t)
          (else #f)))

(define (move-items-selection! st delta)
        (set-context-inspector-items-list!
          st
          (list-move (context-inspector-items-list st) delta)))

(define* (make-context-inspector-overlay
           #:key
           (budget '((total . 128000) (used . 0) (breakdown . ())))
           (items '())
           (usage '((prompt-tokens . 0) (completion-tokens . 0)
                    (cached-tokens . 0) (turns . 0)))
           (on-compact #f)
           (active-tab 'budget))
         "Create the /context & /usage modal <overlay>."
         (let ((st (%make-context-inspector-state
                     budget
                     items
                     usage
                     (make-selectable-list items #:selected-index 0 #:max-visible 10)
                     ""
                     #f
                     on-compact
                     #f)))
           (make-modal-overlay
             #:title "Context & Usage Inspector"
             #:tabs '((budget . "Budget") (items . "Active Items") (usage . "Usage"))
             #:active-tab active-tab
             #:state st
             #:body-renderer render-context-body
             #:detail-renderer render-context-detail
             #:footer-hints '(("Tab" . "switch") ("/" . "filter") ("Enter" . "detail")
                              ("c" . "compact") ("Esc" . "close"))
             #:key-bindings
             (list
               (cons #\/ (lambda (ov _k _ctx)
                           (let ((s (modal-overlay-state ov)))
                             (set-context-inspector-filter-active?! s #t)
                             #t)))
               (cons #\c (lambda (ov _k _ctx)
                           (let* ((s (modal-overlay-state ov))
                                  (cb (context-inspector-on-compact s)))
                             (when cb (cb s))
                             (set-context-inspector-status-message! s "Compaction triggered")
                             #t)))
               (cons #\j (lambda (ov _k _ctx) (move-items-selection! (modal-overlay-state ov) 1) #t))
               (cons #\k (lambda (ov _k _ctx) (move-items-selection! (modal-overlay-state ov) -1) #t))
               (cons 'down (lambda (ov _k _ctx) (move-items-selection! (modal-overlay-state ov) 1) #t))
               (cons 'up (lambda (ov _k _ctx) (move-items-selection! (modal-overlay-state ov) -1) #t))
               (cons 'enter (lambda (ov _k _ctx)
                              (set-modal-show-detail?! ov (not (modal-show-detail? ov)))
                              #t)))
             #:on-unhandled-key
             (lambda (_ov s k _ctx)
               (if (context-inspector-filter-active? s)
                 (handle-filter-key s k)
                 #f)))))

(define* (register-context-inspector-tui!
           #:key
           (register-command! #f)
           (push-overlay! #f)
           (budget-provider (lambda () '((total . 128000) (used . 0) (breakdown . ()))))
           (items-provider (lambda () '()))
           (usage-provider (lambda () '((prompt-tokens . 0) (completion-tokens . 0)
                                        (cached-tokens . 0) (turns . 0))))
           (on-compact #f))
         "Register /context and /usage commands in the TUI."
         (let ((open-tab
                 (lambda (tab)
                   (when push-overlay!
                     (push-overlay!
                       (make-context-inspector-overlay
                         #:budget (budget-provider)
                         #:items (items-provider)
                         #:usage (usage-provider)
                         #:on-compact on-compact
                         #:active-tab tab))))))
           (when register-command!
             (register-command! "context" "Inspect context window budget & active items"
                                (lambda (_args) (open-tab 'budget)))
             (register-command! "usage" "Inspect token usage statistics"
                                (lambda (_args) (open-tab 'usage))))
           #t))
