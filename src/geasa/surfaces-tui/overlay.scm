;;; (geasa surfaces-tui overlay) — Pluggable TUI Overlay & Menu Adapter
;;;
;;; Provides a generic <overlay> protocol (render, on-key, on-click) so modal
;;; UIs (floating menus, pickers, dialogs, fuzzy finders) can be swapped
;;; without modifying the main render loop or event router.

(define-module (geasa surfaces-tui overlay)
               #:use-module
               (geasa surfaces-tui menus)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:export
               (<overlay> make-overlay
                          overlay?
                          overlay-kind
                          overlay-data
                          set-overlay-data!
                          overlay-render
                          overlay-on-key
                          overlay-on-click
                          make-menu-overlay
                          menu-overlay?
                          menu-overlay-menu
                          set-menu-overlay-menu!))

(define-record-type <overlay>
                    (%make-overlay kind data render on-key on-click)
                    overlay?
                    (kind overlay-kind)
                    (data overlay-data set-overlay-data!)
                    (render overlay-render)
                    (on-key overlay-on-key)
                    (on-click overlay-on-click))

(define*
  (make-overlay #:key
                (kind 'custom)
                (data #f)
                (render
                  (lambda (_ov _ctx _maxy _maxx _anchor-y)
                    #f))
                (on-key
                  (lambda (_ov _ch _ctx)
                    'pass))
                (on-click
                  (lambda (_ov _target _ctx)
                    #f)))
  "Create a pluggable TUI overlay with KIND, mutable DATA, and
RENDER / ON-KEY / ON-CLICK handlers."
  (%make-overlay kind data render on-key on-click))

(define (menu-overlay? ov)
        "Return #t if OV is a menu overlay."
        (and (overlay? ov) (eq? (overlay-kind ov) 'menu) (menu? (overlay-data ov))))

(define (menu-overlay-menu ov)
        "Return the <menu> record wrapped by OV, or #f."
        (and (menu-overlay? ov) (overlay-data ov)))

(define (set-menu-overlay-menu! ov m)
        "Update the <menu> record inside menu overlay OV."
        (when
            (and (overlay? ov) (eq? (overlay-kind ov) 'menu))
          (set-overlay-data! ov m)))

(define
  (render-menu-overlay ov draw-ctx maxy maxx popup-anchor-y)
  "Render the <menu> inside OV using DRAW-CTX (alist with 'draw-popup! and
'register-click! callbacks)."
  (let
      ((m (overlay-data ov))
       (draw-popup! (assq-ref draw-ctx 'draw-popup!))
       (register-click! (assq-ref draw-ctx 'register-click!)))
    (when (and (menu? m) draw-popup!)
      (let ((m-count (menu-count m)))
        (when (> m-count 0)
          (call-with-values
            (lambda ()
              (menu-visible-slice m (min %menu-max-visible (max 1 (- maxy 6)))))
            (lambda (vis-items offset)
              (let*
                  ((vis-count (length vis-items))
                   (label-w
                     (fold
                       (lambda (it acc)
                         (max acc (string-length (menu-item-label it))))
                       0
                       vis-items))
                   (desc-w
                     (fold
                       (lambda (it acc)
                         (max acc (string-length (menu-item-description it))))
                       0
                       vis-items))
                   (box-w (min (max 24 (+ 8 label-w desc-w)) (max 20 (- maxx 4))))
                   (box-h (+ vis-count 2))
                   (start-y (- popup-anchor-y 1 box-h))
                   (rows
                     (map
                       (lambda (it)
                         (cons (menu-item-label it) (menu-item-description it)))
                       vis-items)))
                (when (>= start-y 2)
                  (draw-popup! (menu-breadcrumb-title m)
                               rows
                               (- (menu-selected-index m) offset)
                               box-w
                               start-y
                               2
                               offset
                               (- m-count offset vis-count))
                  (when register-click!
                    (let mi
                      ((i 0))
                      (when (< i vis-count)
                        (register-click! (+ start-y 1 i) (cons 'menu-item (+ offset i)))
                        (mi (+ i 1)))))
                  #t)))))))))

(define (handle-menu-overlay-key ov ch key-ctx)
        "Handle key CH for menu overlay OV using KEY-CTX (alist with 'key-up,
'key-down, 'key-left, 'key-backspace, 'key-enter, 'on-select!, 'on-close!).
Returns 'handled, 'close, or 'pass."
        (let
            ((m (overlay-data ov))
             (key-up (assq-ref key-ctx 'key-up))
             (key-down (assq-ref key-ctx 'key-down))
             (key-left (assq-ref key-ctx 'key-left))
             (key-backspace (assq-ref key-ctx 'key-backspace))
             (key-enter (assq-ref key-ctx 'key-enter))
             (on-select! (assq-ref key-ctx 'on-select!))
             (on-close! (assq-ref key-ctx 'on-close!)))
          (cond
            ((not (menu? m)) 'pass)
            ((eqv? ch key-up)
              (let ((next-m (menu-move m -1)))
                (set-overlay-data! ov next-m)
                (menu-trigger-highlight! next-m)
                'handled))
            ((eqv? ch key-down)
              (let ((next-m (menu-move m 1)))
                (set-overlay-data! ov next-m)
                (menu-trigger-highlight! next-m)
                'handled))
            ((eqv? ch 27)
              (let ((parent (menu-pop m)))
                (if parent
                  (begin (set-overlay-data! ov parent) (menu-trigger-highlight! parent) 'handled)
                  (begin
                    (when (procedure? (menu-on-cancel m)) ((menu-on-cancel m)))
                    (when on-close! (on-close!))
                    'close))))
            ((or (eqv? ch key-left)
                 (and key-backspace (eqv? ch key-backspace))
                 (eqv? ch 8)
                 (eqv? ch 127))
              (let ((parent (menu-pop m)))
                (if parent
                  (begin (set-overlay-data! ov parent) (menu-trigger-highlight! parent) 'handled)
                  'pass)))
            ((or (eqv? ch key-enter) (eqv? ch 10) (eqv? ch 13))
              (let ((item (menu-selected-item m)))
                (cond
                  ((and item (eq? (menu-item-action item) 'menu-back))
                    (let ((parent (menu-pop m)))
                      (if parent
                        (begin (set-overlay-data! ov parent) (menu-trigger-highlight! parent) 'handled)
                        (begin (when on-close! (on-close!)) 'close))))
                  (else (when (and item on-select!) (on-select! item)) 'handled))))
            (else 'pass))))

(define
  (handle-menu-overlay-click ov target click-ctx)
  "Handle mouse TARGET on menu overlay OV. Returns #t when handled."
  (let
      ((m (overlay-data ov))
       (on-select! (assq-ref click-ctx 'on-select!))
       (on-close! (assq-ref click-ctx 'on-close!)))
    (if
        (and (menu? m) (pair? target) (eq? (car target) 'menu-item))
      (let*
          ((moved (menu-move m (- (cdr target) (menu-selected-index m))))
           (item (menu-selected-item moved)))
        (set-overlay-data! ov moved)
        (if
            (and item (eq? (menu-item-action item) 'menu-back))
          (let ((parent (menu-pop moved)))
            (if parent
              (begin (set-overlay-data! ov parent) (menu-trigger-highlight! parent))
              (when on-close! (on-close!))))
          (when (and item on-select!) (on-select! item)))
        #t)
      #f)))

(define (make-menu-overlay m)
        "Wrap a <menu> M in a pluggable <overlay>."
        (make-overlay #:kind
                      'menu
                      #:data
                      m
                      #:render
                      render-menu-overlay
                      #:on-key
                      handle-menu-overlay-key
                      #:on-click
                      handle-menu-overlay-click))