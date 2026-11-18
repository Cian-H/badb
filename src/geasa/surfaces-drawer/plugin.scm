;;; (geasa surfaces-drawer plugin) — Wide-Terminal Side Drawer & Completion Preview

(define-module (geasa surfaces-drawer plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (compute-drawer-layout
                          register-drawer-panel!
                          unregister-drawer-panel!
                          list-drawer-panels
                          clear-drawer-panels!
                          register-builtin-drawer-panels!
                          <drawer-state>
                          make-drawer-state
                          drawer-state?
                          drawer-open?
                          set-drawer-open?!
                          drawer-active-panel
                          set-drawer-active-panel!
                          toggle-drawer!
                          cycle-drawer-panel!
                          render-drawer-widget
                          make-completion-preview-overlay
                          register-drawer-tui!))

;;;
;;; Pure Layout Splitter
;;;
(define* (compute-drawer-layout total-rect #:key (open? #t) (min-width 100) (drawer-ratio 0.30))
         "Split TOTAL-RECT into (values main-rect drawer-rect-or-#f).
Auto-collapses when OPEN? is #f or width < MIN-WIDTH."
         (let ((w (rect-width total-rect))
               (h (rect-height total-rect))
               (x (rect-x total-rect))
               (y (rect-y total-rect)))
           (if (and open? (>= w min-width))
             (let* ((dw (max 20 (inexact->exact (round (* w drawer-ratio)))))
                    (mw (max 1 (- w dw))))
               (values (make-rect x y mw h)
                       (make-rect (+ x mw) y dw h)))
             (values total-rect #f))))

;;;
;;; Drawer Panel Registry
;;;
(define %drawer-panels '())

(define* (register-drawer-panel! id #:key (title "") render)
         "Register a drawer panel ID with TITLE and RENDER (rect state) -> <widget>."
         (set! %drawer-panels
               (append (alist-delete id %drawer-panels eq?)
                       (list (cons id `((title . ,title)
                                        (render . ,render)))))))

(define (unregister-drawer-panel! id)
        (set! %drawer-panels (alist-delete id %drawer-panels eq?)))

(define (list-drawer-panels)
        %drawer-panels)

(define (clear-drawer-panels!)
        (set! %drawer-panels '()))

(define (register-builtin-drawer-panels!)
        "Register built-in todos, subagents, and files drawer panels."
        (register-drawer-panel!
          'todos
          #:title "Todos"
          #:render
          (lambda (_rect state)
            (let ((items (or (assq-ref state 'todos) '())))
              (if (null? items)
                (text-widget "No active todos." #:dim? #t)
                (vstack-widget
                  (map (lambda (it)
                         (text-widget (format #f "- ~a" it)))
                       items))))))
        (register-drawer-panel!
          'subagents
          #:title "Subagents"
          #:render
          (lambda (_rect state)
            (let ((items (or (assq-ref state 'subagents) '())))
              (if (null? items)
                (text-widget "No running subagents." #:dim? #t)
                (vstack-widget
                  (map (lambda (it)
                         (text-widget (format #f "* ~a" it)))
                       items))))))
        (register-drawer-panel!
          'files
          #:title "Files"
          #:render
          (lambda (_rect state)
            (let ((items (or (assq-ref state 'files) '())))
              (if (null? items)
                (text-widget "No touched files." #:dim? #t)
                (vstack-widget
                  (map (lambda (it)
                         (text-widget (format #f "  ~a" it)))
                       items)))))))

(register-builtin-drawer-panels!)

;;;
;;; Drawer State & Rendering
;;;
(define-record-type <drawer-state>
                    (%make-drawer-state open? active-panel)
                    drawer-state?
                    (open? drawer-open? set-drawer-open?!)
                    (active-panel drawer-active-panel set-drawer-active-panel!))

(define* (make-drawer-state #:key (open? #t) (active-panel 'todos))
         (%make-drawer-state open? active-panel))

(define (toggle-drawer! st)
        (set-drawer-open?! st (not (drawer-open? st)))
        (drawer-open? st))

(define (cycle-drawer-panel! st)
        (let* ((ids (map car %drawer-panels))
               (cur (drawer-active-panel st))
               (tail (memq cur ids))
               (next (cond
                       ((null? ids) #f)
                       ((and tail (pair? (cdr tail))) (cadr tail))
                       (else (car ids)))))
          (set-drawer-active-panel! st next)
          next))

(define* (render-drawer-widget drawer-state rect #:key (context '()))
         "Render the active drawer panel into a framed box <widget>."
         (let* ((panel-id (drawer-active-panel drawer-state))
                (entry (assq-ref %drawer-panels panel-id))
                (title (if entry (assq-ref entry 'title) "Drawer"))
                (render-fn (and entry (assq-ref entry 'render)))
                (body (if (procedure? render-fn)
                        (render-fn rect context)
                        (text-widget "No panel registered." #:dim? #t))))
           (box-widget body #:title title)))

;;;
;;; Side-by-Side Completion Preview Overlay
;;;
(define* (make-completion-preview-overlay
           list-state
           preview-fn
           #:key
           (anchor-y 0)
           (max-width 72))
         "Create a completion preview <overlay> pairing a <selectable-list> with a preview pane."
         (let ((state-box (list list-state)))
           (make-overlay
             #:kind 'completion-preview
             #:data state-box
             #:render
             (lambda (_ov draw-ctx maxy maxx _ay)
               (let* ((slist (car state-box))
                      (w (min max-width (max 30 (- maxx 4))))
                      (h (min 12 (max 6 (- maxy 4))))
                      (y (max 0 (min anchor-y (- maxy h))))
                      (rect (make-rect 2 y w h))
                      (sel (list-selected-item slist))
                      (preview-text (if (and sel (procedure? preview-fn))
                                      (or (preview-fn sel) "")
                                      ""))
                      (list-w (box-widget
                                (selectable-list-widget slist)
                                #:title "Completions"))
                      (prev-w (box-widget
                                (text-widget (if (string-null? preview-text)
                                               "No preview."
                                               preview-text))
                                #:title "Preview"))
                      (combined (hstack-widget (list list-w prev-w)))
                      (ops (widget-render-rect combined rect))
                      (draw-ops! (assq-ref draw-ctx 'draw-ops!)))
                 (when draw-ops!
                   (draw-ops! ops rect))
                 #t))
             #:on-key
             (lambda (_ov ch _ctx)
               (cond
                 ((or (eqv? ch (char->integer #\j)) (eq? ch 'down))
                   (set-car! state-box (list-move (car state-box) 1))
                   'handled)
                 ((or (eqv? ch (char->integer #\k)) (eq? ch 'up))
                   (set-car! state-box (list-move (car state-box) -1))
                   'handled)
                 ((or (eqv? ch 27) (eqv? ch (char->integer #\q)))
                   'close)
                 (else 'pass))))))

(define* (register-drawer-tui!
           #:key
           (register-category! (lambda (_p _d) #f))
           (register-command! (lambda (_n _d _a . _r) #f))
           (on-toggle-drawer! (lambda () #f)))
         "Register /drawer command and View category in TUI."
         (register-category! '("View") "Layout and side drawer controls")
         (register-command!
           "drawer"
           "Toggle the wide-terminal side drawer"
           (lambda _args
             (on-toggle-drawer!))))
