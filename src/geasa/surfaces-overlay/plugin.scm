;;; (geasa surfaces-overlay plugin) — Pluggable TUI Overlay & Modular Modal Framework
;;;
;;; Provides the generic <overlay> protocol (render, on-key, on-click),
;;; make-menu-overlay, and make-modal-overlay built over (geasa surfaces-widgets plugin)
;;; with centered/anchored framing, tab-bar header, body/detail host, key-hints
;;; footer, declarative keymap dispatch, click hit-testing, and extension slots.

(define-module (geasa surfaces-overlay plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (<overlay>
                          make-overlay
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
                          set-menu-overlay-menu!
                          <modal-state>
                          modal-overlay?
                          modal-overlay-state
                          set-modal-overlay-state!
                          modal-active-tab
                          set-modal-active-tab!
                          modal-show-detail?
                          set-modal-show-detail?!
                          modal-extension-slots
                          modal-slot-providers
                          modal-register-slot!
                          modal-filter-state
                          modal-filter-query
                          set-modal-filter-state!
                          compute-modal-rect
                          make-modal-overlay
                          <overlay-stack>
                          make-overlay-stack
                          overlay-stack?
                          overlay-stack-modals
                          overlay-stack-floatings
                          overlay-stack-push-modal!
                          overlay-stack-pop-modal!
                          overlay-stack-top-modal
                          overlay-stack-add-floating!
                          overlay-stack-remove-floating!
                          overlay-stack-render
                          overlay-stack-handle-key
                          overlay-stack-handle-click
                          make-confirm-dialog
                          make-input-dialog))

;;;
;;; Core <overlay> Protocol
;;;
(define-record-type <overlay>
                    (%make-overlay kind data render on-key on-click)
                    overlay?
                    (kind overlay-kind)
                    (data overlay-data set-overlay-data!)
                    (render overlay-render)
                    (on-key overlay-on-key)
                    (on-click overlay-on-click))

(define* (make-overlay #:key
                       (kind 'custom)
                       (data #f)
                       (render (lambda (_ov _ctx _maxy _maxx _anchor-y) #f))
                       (on-key (lambda (_ov _ch _ctx) 'pass))
                       (on-click (lambda (_ov _target _ctx) #f)))
         "Create a pluggable TUI overlay with KIND, mutable DATA, and
RENDER / ON-KEY / ON-CLICK handlers."
         (%make-overlay kind data render on-key on-click))

;;;
;;; Menu Overlay (generic over menu-ops or <selectable-list>)
;;;
(define (default-list-ops)
        `((is-menu? . ,selectable-list?)
          (count . ,(lambda (m) (length (selectable-list-items m))))
          (visible-slice . ,list-visible-slice)
          (item-label . ,(lambda (it) (if (pair? it) (ensure-string (car it)) (ensure-string it))))
          (item-desc . ,(lambda (it) (if (pair? it) (ensure-string (cdr it)) "")))
          (title . ,(lambda (_m) "Menu"))
          (selected-index . ,selectable-list-selected-index)
          (move . ,list-move)
          (trigger-highlight! . ,(lambda (m)
                                   (let ((cb (selectable-list-on-highlight m)))
                                     (when (procedure? cb) (cb (list-selected-item m))))))
          (pop . ,(lambda (_m) #f))
          (on-cancel . ,(lambda (_m) #f))
          (items . ,selectable-list-items)
          (item-action . ,(lambda (_it) #f))
          (selected-item . ,list-selected-item)
          (max-visible . 10)))

(define (menu-overlay? ov)
        "Return #t if OV is a menu overlay."
        (and (overlay? ov) (eq? (overlay-kind ov) 'menu) (overlay-data ov) #t))

(define (menu-overlay-menu ov)
        "Return the menu record wrapped by OV, or #f."
        (and (menu-overlay? ov) (overlay-data ov)))

(define (set-menu-overlay-menu! ov m)
        "Update the menu record inside menu overlay OV."
        (when (and (overlay? ov) (eq? (overlay-kind ov) 'menu))
          (set-overlay-data! ov m)))

(define (make-menu-render ops)
        (let ((is-menu? (assq-ref ops 'is-menu?))
              (m-count-fn (assq-ref ops 'count))
              (vis-slice-fn (assq-ref ops 'visible-slice))
              (item-label-fn (assq-ref ops 'item-label))
              (item-desc-fn (assq-ref ops 'item-desc))
              (title-fn (assq-ref ops 'title))
              (sel-idx-fn (assq-ref ops 'selected-index))
              (max-vis (or (assq-ref ops 'max-visible) 10)))
          (lambda (ov draw-ctx maxy maxx popup-anchor-y)
            (let ((m (overlay-data ov))
                  (draw-popup! (assq-ref draw-ctx 'draw-popup!))
                  (register-click! (assq-ref draw-ctx 'register-click!)))
              (when (and (is-menu? m) draw-popup!)
                (let ((m-count (m-count-fn m)))
                  (when (> m-count 0)
                    (call-with-values
                      (lambda () (vis-slice-fn m (min max-vis (max 1 (- maxy 6)))))
                      (lambda (vis-items offset)
                        (let* ((vis-count (length vis-items))
                               (label-w (fold (lambda (it acc) (max acc (string-length (item-label-fn it))))
                                              0 vis-items))
                               (desc-w (fold (lambda (it acc) (max acc (string-length (item-desc-fn it))))
                                             0 vis-items))
                               (box-w (min (max 24 (+ 8 label-w desc-w)) (max 20 (- maxx 4))))
                               (box-h (+ vis-count 2))
                               (start-y (- popup-anchor-y 1 box-h))
                               (rows (map (lambda (it)
                                            (cons (item-label-fn it) (item-desc-fn it)))
                                          vis-items)))
                          (when (>= start-y 2)
                            (draw-popup! (title-fn m)
                                         rows
                                         (- (sel-idx-fn m) offset)
                                         box-w
                                         start-y
                                         2
                                         offset
                                         (- m-count offset vis-count))
                            (when register-click!
                              (let mi ((i 0))
                                      (when (< i vis-count)
                                        (register-click! (+ start-y 1 i) (cons 'menu-item (+ offset i)))
                                        (mi (+ i 1)))))
                            #t)))))))))))

(define (make-menu-key-handler ops)
        (let ((is-menu? (assq-ref ops 'is-menu?))
              (move-fn (assq-ref ops 'move))
              (hl-fn (assq-ref ops 'trigger-highlight!))
              (pop-fn (assq-ref ops 'pop))
              (on-cancel-fn (assq-ref ops 'on-cancel))
              (items-fn (assq-ref ops 'items))
              (action-fn (assq-ref ops 'item-action))
              (sel-item-fn (assq-ref ops 'selected-item)))
          (lambda (ov ch key-ctx)
            (let ((m (overlay-data ov))
                  (key-up (assq-ref key-ctx 'key-up))
                  (key-down (assq-ref key-ctx 'key-down))
                  (key-left (assq-ref key-ctx 'key-left))
                  (key-backspace (assq-ref key-ctx 'key-backspace))
                  (key-enter (assq-ref key-ctx 'key-enter))
                  (on-select! (assq-ref key-ctx 'on-select!))
                  (on-close! (assq-ref key-ctx 'on-close!)))
              (cond
                ((not (is-menu? m)) 'pass)
                ((eqv? ch key-up)
                  (let ((next-m (move-fn m -1)))
                    (set-overlay-data! ov next-m)
                    (when hl-fn (hl-fn next-m))
                    'handled))
                ((eqv? ch key-down)
                  (let ((next-m (move-fn m 1)))
                    (set-overlay-data! ov next-m)
                    (when hl-fn (hl-fn next-m))
                    'handled))
                ((eqv? ch 27)
                  (let ((parent (pop-fn m)))
                    (if parent
                      (begin (set-overlay-data! ov parent) (when hl-fn (hl-fn parent)) 'handled)
                      (begin
                        (when (and on-cancel-fn (procedure? (on-cancel-fn m))) ((on-cancel-fn m)))
                        (when on-close! (on-close!))
                        'close))))
                ((or (eqv? ch key-left)
                     (and key-backspace (eqv? ch key-backspace))
                     (eqv? ch 8)
                     (eqv? ch 127))
                  (let ((parent (pop-fn m)))
                    (if parent
                      (begin (set-overlay-data! ov parent) (when hl-fn (hl-fn parent)) 'handled)
                      (let ((slash-item (find (lambda (it) (eq? (action-fn it) 'back-to-slash))
                                              (items-fn m))))
                        (if slash-item
                          (begin
                            (when (and on-cancel-fn (procedure? (on-cancel-fn m))) ((on-cancel-fn m)))
                            (when on-select! (on-select! slash-item))
                            'close)
                          'pass)))))
                ((or (eqv? ch key-enter) (eqv? ch 10) (eqv? ch 13))
                  (let ((item (sel-item-fn m)))
                    (cond
                      ((and item (eq? (action-fn item) 'menu-back))
                        (let ((parent (pop-fn m)))
                          (if parent
                            (begin (set-overlay-data! ov parent) (when hl-fn (hl-fn parent)) 'handled)
                            (begin (when on-close! (on-close!)) 'close))))
                      (else (when (and item on-select!) (on-select! item)) 'handled))))
                (else 'pass))))))

(define (make-menu-click-handler ops)
        (let ((is-menu? (assq-ref ops 'is-menu?))
              (move-fn (assq-ref ops 'move))
              (sel-idx-fn (assq-ref ops 'selected-index))
              (sel-item-fn (assq-ref ops 'selected-item))
              (action-fn (assq-ref ops 'item-action))
              (pop-fn (assq-ref ops 'pop))
              (hl-fn (assq-ref ops 'trigger-highlight!)))
          (lambda (ov target click-ctx)
            (let ((m (overlay-data ov))
                  (on-select! (assq-ref click-ctx 'on-select!))
                  (on-close! (assq-ref click-ctx 'on-close!)))
              (if (and (is-menu? m) (pair? target) (eq? (car target) 'menu-item))
                (let* ((moved (move-fn m (- (cdr target) (sel-idx-fn m))))
                       (item (sel-item-fn moved)))
                  (set-overlay-data! ov moved)
                  (if (and item (eq? (action-fn item) 'menu-back))
                    (let ((parent (pop-fn moved)))
                      (if parent
                        (begin (set-overlay-data! ov parent) (when hl-fn (hl-fn parent)))
                        (when on-close! (on-close!))))
                    (when (and item on-select!) (on-select! item)))
                  #t)
                #f)))))

(define* (make-menu-overlay m #:key (menu-ops #f))
         "Wrap a menu M in a pluggable <overlay>. Uses MENU-OPS alist if provided,
otherwise defaults to <selectable-list> operations."
         (let ((ops (or menu-ops (default-list-ops))))
           (make-overlay #:kind 'menu
                         #:data m
                         #:render (make-menu-render ops)
                         #:on-key (make-menu-key-handler ops)
                         #:on-click (make-menu-click-handler ops))))

;;;
;;; Modular Modal Overlay Framework
;;;
(define-record-type <modal-state>
                    (%make-modal-state user-state active-tab show-detail? extension-slots last-rect filter-state)
                    modal-state?
                    (user-state modal-state-user set-modal-state-user!)
                    (active-tab modal-state-active-tab set-modal-state-active-tab!)
                    (show-detail? modal-state-show-detail? set-modal-state-show-detail?!)
                    (extension-slots modal-state-extension-slots set-modal-state-extension-slots!)
                    (last-rect modal-state-last-rect set-modal-state-last-rect!)
                    (filter-state modal-state-filter set-modal-state-filter!))

(define (modal-overlay? ov)
        "Return #t if OV is a modal overlay."
        (and (overlay? ov) (eq? (overlay-kind ov) 'modal) (modal-state? (overlay-data ov))))

(define (modal-overlay-state ov)
        "Return the user state stored inside modal overlay OV."
        (and (modal-overlay? ov) (modal-state-user (overlay-data ov))))

(define (set-modal-overlay-state! ov new-state)
        "Update the user state stored inside modal overlay OV."
        (when (modal-overlay? ov)
          (set-modal-state-user! (overlay-data ov) new-state)))

(define (modal-active-tab ov)
        "Return the active tab id symbol of modal overlay OV."
        (and (modal-overlay? ov) (modal-state-active-tab (overlay-data ov))))

(define (set-modal-active-tab! ov tab-id)
        "Set the active tab id symbol of modal overlay OV."
        (when (modal-overlay? ov)
          (set-modal-state-active-tab! (overlay-data ov) tab-id)))

(define (modal-show-detail? ov)
        "Return #t if the detail pane of modal overlay OV is expanded."
        (and (modal-overlay? ov) (modal-state-show-detail? (overlay-data ov))))

(define (set-modal-show-detail?! ov flag)
        "Set whether the detail pane of modal overlay OV is expanded."
        (when (modal-overlay? ov)
          (set-modal-state-show-detail?! (overlay-data ov) (and flag #t))))

(define (modal-extension-slots ov)
        "Return the extension-slots alist of modal overlay OV."
        (if (modal-overlay? ov)
          (modal-state-extension-slots (overlay-data ov))
          '()))

(define (modal-slot-providers ov slot-name)
        "Return the list of providers registered for SLOT-NAME on modal overlay OV."
        (or (assq-ref (modal-extension-slots ov) slot-name) '()))

(define (modal-register-slot! ov slot-name provider)
        "Append PROVIDER to SLOT-NAME in modal overlay OV's extension slots."
        (when (modal-overlay? ov)
          (let* ((ms (overlay-data ov))
                 (slots (modal-state-extension-slots ms))
                 (existing (or (assq-ref slots slot-name) '()))
                 (updated (cons (cons slot-name (append existing (list provider)))
                                (alist-delete slot-name slots eq?))))
            (set-modal-state-extension-slots! ms updated))))

(define (modal-filter-state ov)
        "Return the <filter-state> of modal overlay OV, or #f."
        (and (modal-overlay? ov) (modal-state-filter (overlay-data ov))))

(define (modal-filter-query ov)
        "Return the current filter query string of modal overlay OV (\"\" if none)."
        (let ((fs (modal-filter-state ov)))
          (if (filter-state? fs) (filter-state-query fs) "")))

(define (set-modal-filter-state! ov fs)
        "Set the <filter-state> of modal overlay OV."
        (when (modal-overlay? ov)
          (set-modal-state-filter! (overlay-data ov) fs)))

(define* (compute-modal-rect maxy maxx #:key (anchor 'center) (width 0.8) (height 0.75) (anchor-y #f))
         "Compute a <rect> for a modal overlay within MAXY x MAXX.
WIDTH and HEIGHT may be integers or fractions in (0, 1]. ANCHOR may be 'center,
'bottom, a (x . y) pair, or a procedure (maxy maxx) -> <rect>."
         (if (procedure? anchor)
           (anchor maxy maxx)
           (let* ((w (max 20 (min (- maxx 2)
                                  (if (and (real? width) (<= width 1.0) (> width 0))
                                    (inexact->exact (floor (* maxx width)))
                                    (inexact->exact width)))))
                  (h (max 8 (min (- maxy 2)
                                 (if (and (real? height) (<= height 1.0) (> height 0))
                                   (inexact->exact (floor (* maxy height)))
                                   (inexact->exact height)))))
                  (x (cond
                       ((pair? anchor) (max 0 (min (- maxx w) (car anchor))))
                       (else (max 0 (quotient (- maxx w) 2)))))
                  (y (cond
                       ((pair? anchor) (max 0 (min (- maxy h) (cdr anchor))))
                       ((eq? anchor 'bottom)
                         (max 1 (- (or anchor-y (- maxy 2)) h)))
                       (else (max 1 (quotient (- maxy h) 2))))))
             (make-rect x y w h))))

(define (resolve-value val arg)
        (if (procedure? val) (val arg) val))

(define (key-matches? spec ch key-ctx)
        (cond
          ((list? spec) (any (lambda (s) (key-matches? s ch key-ctx)) spec))
          ((char? spec) (eqv? ch (char->integer spec)))
          ((integer? spec) (eqv? ch spec))
          ((symbol? spec)
            (case spec
              ((up) (eqv? ch (assq-ref key-ctx 'key-up)))
              ((down) (eqv? ch (assq-ref key-ctx 'key-down)))
              ((left) (eqv? ch (assq-ref key-ctx 'key-left)))
              ((right) (eqv? ch (assq-ref key-ctx 'key-right)))
              ((backspace) (or (eqv? ch (assq-ref key-ctx 'key-backspace))
                               (eqv? ch 8)
                               (eqv? ch 127)))
              ((enter) (or (eqv? ch (assq-ref key-ctx 'key-enter))
                           (eqv? ch 10)
                           (eqv? ch 13)))
              ((tab) (or (eqv? ch (assq-ref key-ctx 'key-tab)) (eqv? ch 9)))
              ((esc) (eqv? ch 27))
              (else #f)))
          (else #f)))

(define
  (build-modal-widget ov
                      ms
                      rect
                      title
                      tabs
                      body-renderer
                      detail-renderer
                      footer-hints)
  (let*
      ((ustate (modal-state-user ms))
       (active-tab (modal-state-active-tab ms))
       (show-detail? (modal-state-show-detail? ms))
       (inner-w (max 1 (- (rect-width rect) 2)))
       (inner-h (max 1 (- (rect-height rect) 2)))
       (banner-providers (modal-slot-providers ov 'top-banner))
       (banner-widgets
         (filter-map
           (lambda (p)
             (if (procedure? p)
               (p ov ustate)
               p))
           banner-providers))
       (tab-w
         (if (and (pair? tabs) (not (null? tabs)))
           (let
               ((idx
                  (or
                    (list-index
                      (lambda (t)
                        (eq? (car t) active-tab))
                      tabs)
                    0)))
             (list (tab-bar-widget (map cdr tabs) #:active idx)))
           '()))
       (footer-base (or (resolve-value footer-hints ustate) '()))
       (footer-extra
         (append-map
           (lambda (p)
             (let
                 ((res
                    (if (procedure? p)
                      (p ov ustate)
                      p)))
               (if (list? res)
                 res
                 '())))
           (modal-slot-providers ov 'footer-actions)))
       (all-hints (append footer-base footer-extra))
       (footer-w
         (if (null? all-hints)
           '()
           (list (key-hints-widget all-hints))))
       (footer-gap
         (if (null? footer-w)
           0
           1))
       (fs (modal-state-filter ms))
       (show-filter? (and fs (or (filter-state-active? fs) (not (string-null? (filter-state-query fs))))))
       (filter-w (if show-filter? (list (filter-input-widget fs)) '()))
       (overhead
         (+ (length tab-w) (length banner-widgets) (length filter-w) (length footer-w) footer-gap))
       (avail-h (max 1 (- inner-h overhead)))
       (detail-h
         (if (and show-detail? detail-renderer)
           (max 3 (quotient avail-h 3))
           0))
       (body-h (max 1 (- avail-h detail-h)))
       (body-rect (make-rect 0 0 inner-w body-h))
       (raw-body-w
         (if body-renderer
           (body-renderer ov ustate body-rect)
           (text-widget "")))
       (body-w
         (if (and fs
                  (not (string-null? (filter-state-query fs)))
                  (widget? raw-body-w)
                  (eq? (widget-kind raw-body-w) 'selectable-list))
           (let* ((sl (assq-ref (widget-props raw-body-w) 'list))
                  (filtered (filter-selectable-list sl (filter-state-query fs)
                                                    (or (assq-ref (modal-state-extension-slots ms) 'filter-key-fn)
                                                        ensure-string))))
             (selectable-list-widget filtered))
           raw-body-w))
       (detail-w
         (if
             (and show-detail? detail-renderer (> detail-h 0))
           (list (rule-widget)
                 (detail-renderer ov ustate (make-rect 0 0 inner-w (max 1 (- detail-h 1)))))
           '()))
       (top-children (append tab-w banner-widgets filter-w (list body-w) detail-w))
       (top-stack (vstack-widget top-children))
       (top-h (cdr (widget-measure-size top-stack inner-w avail-h)))
       (spacer-h
         (if (null? footer-w)
           0
           (max 1 (- inner-h top-h (length footer-w)))))
       (spacer
         (if (> spacer-h 0)
           (list
             (make-widget 'spacer
                          #:measure
                          (lambda (_s aw ah)
                            (cons (min aw 1) (min ah spacer-h)))
                          #:render
                          (lambda (_s _r)
                            '())))
           '()))
       (children (append top-children spacer footer-w)))
    (box-widget (vstack-widget children) #:title (resolve-value title ustate))))

(define* (make-modal-overlay #:key
                             (title "Modal")
                             (tabs #f)
                             (active-tab #f)
                             (state #f)
                             (anchor 'center)
                             (width 0.8)
                             (height 0.75)
                             (body-renderer #f)
                             (detail-renderer #f)
                             (show-detail? #f)
                             (key-bindings '())
                             (footer-hints '())
                             (extension-slots '())
                             (filterable? #f)
                             (filter-key-fn #f)
                             (filter-placeholder "Type to filter...")
                             (on-tab-change #f)
                             (on-close #f)
                             (on-unhandled-key #f))
         "Create a modular modal <overlay> built over surfaces-widgets."
         (let* ((init-tab (or active-tab (and (pair? tabs) (caar tabs))))
                (init-slots (if filter-key-fn
                              (cons (cons 'filter-key-fn filter-key-fn) extension-slots)
                              extension-slots))
                (init-filter (and filterable? (make-filter-state #:placeholder filter-placeholder)))
                (ms (%make-modal-state state init-tab (and show-detail? #t) init-slots #f init-filter)))
           (make-overlay
             #:kind 'modal
             #:data ms
             #:render
             (lambda (ov draw-ctx maxy maxx anchor-y)
               (let* ((rect (compute-modal-rect maxy maxx
                                                #:anchor anchor
                                                #:width width
                                                #:height height
                                                #:anchor-y anchor-y))
                      (_ (set-modal-state-last-rect! ms rect))
                      (w (build-modal-widget ov ms rect title tabs body-renderer detail-renderer footer-hints))
                      (ops (widget-render-rect w rect))
                      (draw-ops! (assq-ref draw-ctx 'draw-ops!))
                      (register-click! (assq-ref draw-ctx 'register-click!)))
                 (when draw-ops!
                   (draw-ops! ops rect))
                 (when register-click!
                   (let ((y0 (rect-y rect))
                         (y1 (+ (rect-y rect) (rect-height rect))))
                     (let loop ((y y0))
                               (when (< y y1)
                                 (register-click! y (cons 'modal-row (- y y0)))
                                 (loop (+ y 1))))))
                 #t))
             #:on-key
             (lambda (ov ch key-ctx)
               (let ((on-close! (assq-ref key-ctx 'on-close!))
                     (fs (modal-state-filter ms)))
                 (cond
                   ((and filterable? fs (filter-state-active? fs)
                         (filter-handle-key fs ch #:key-ctx key-ctx))
                     => (lambda (next-fs)
                          (set-modal-state-filter! ms next-fs)
                          'handled))
                   ((and filterable? fs (not (filter-state-active? fs))
                         (key-matches? #\/ ch key-ctx))
                     (set-modal-state-filter!
                       ms
                       (make-filter-state #:query (filter-state-query fs)
                                          #:active? #t
                                          #:placeholder filter-placeholder))
                     'handled)
                   ((and filterable? fs (not (filter-state-active? fs))
                         (not (string-null? (filter-state-query fs)))
                         (key-matches? 'esc ch key-ctx))
                     (set-modal-state-filter!
                       ms
                       (make-filter-state #:query "" #:active? #f #:placeholder filter-placeholder))
                     'handled)
                   (else
                     (let loop ((bindings key-bindings))
                               (cond
                                 ((null? bindings)
                                   (cond
                                     ((key-matches? 'esc ch key-ctx)
                                       (when on-close (on-close ov (modal-state-user ms)))
                                       (when on-close! (on-close!))
                                       'close)
                                     ((and (pair? tabs) (key-matches? 'tab ch key-ctx))
                                       (let* ((ids (map car tabs))
                                              (cur (modal-state-active-tab ms))
                                              (tail (memq cur ids))
                                              (next-id (if (and tail (pair? (cdr tail)))
                                                         (cadr tail)
                                                         (car ids))))
                                         (set-modal-state-active-tab! ms next-id)
                                         (when on-tab-change
                                           (set-modal-state-user! ms (on-tab-change ov (modal-state-user ms) next-id)))
                                         'handled))
                                     ((procedure? on-unhandled-key)
                                       (let ((res (on-unhandled-key ov (modal-state-user ms) ch key-ctx)))
                                         (when (eq? res 'close)
                                           (when on-close (on-close ov (modal-state-user ms)))
                                           (when on-close! (on-close!)))
                                         (or res 'pass)))
                                     (else 'pass)))
                                 ((key-matches? (caar bindings) ch key-ctx)
                                   (let ((res ((cdar bindings) ov (modal-state-user ms) key-ctx)))
                                     (when (eq? res 'close)
                                       (when on-close (on-close ov (modal-state-user ms)))
                                       (when on-close! (on-close!)))
                                     (or res 'handled)))
                                 (else (loop (cdr bindings)))))))))
             #:on-click
             (lambda (ov target _click-ctx)
               (if (and (pair? target) (eq? (car target) 'modal-row))
                 #t
                 #f)))))

;;;
;;; Layered Overlay Stack (<overlay-stack>)
;;;
(define-record-type <overlay-stack>
                    (%make-overlay-stack modals floatings)
                    overlay-stack?
                    (modals overlay-stack-modals set-overlay-stack-modals!)
                    (floatings overlay-stack-floatings set-overlay-stack-floatings!))

(define (make-overlay-stack)
        "Create an empty <overlay-stack> with LIFO 'modal stack and passive 'floating list."
        (%make-overlay-stack '() '()))

(define (overlay-stack-push-modal! stack ov)
        "Push modal overlay OV onto the top of STACK's modal stack."
        (when (and (overlay-stack? stack) (overlay? ov))
          (set-overlay-stack-modals! stack (cons ov (overlay-stack-modals stack)))))

(define (overlay-stack-pop-modal! stack)
        "Pop and return the top modal overlay from STACK, or #f if empty."
        (if (and (overlay-stack? stack) (pair? (overlay-stack-modals stack)))
          (let ((top (car (overlay-stack-modals stack))))
            (set-overlay-stack-modals! stack (cdr (overlay-stack-modals stack)))
            top)
          #f))

(define (overlay-stack-top-modal stack)
        "Return the top modal overlay on STACK, or #f if empty."
        (and (overlay-stack? stack)
             (pair? (overlay-stack-modals stack))
             (car (overlay-stack-modals stack))))

(define (overlay-stack-add-floating! stack ov)
        "Add passive floating overlay OV to STACK (no-op if already present)."
        (when (and (overlay-stack? stack) (overlay? ov)
                   (not (memq ov (overlay-stack-floatings stack))))
          (set-overlay-stack-floatings! stack (append (overlay-stack-floatings stack) (list ov)))))

(define (overlay-stack-remove-floating! stack ov)
        "Remove passive floating overlay OV from STACK."
        (when (overlay-stack? stack)
          (set-overlay-stack-floatings! stack (delq ov (overlay-stack-floatings stack)))))

(define (overlay-stack-render stack draw-ctx maxy maxx anchor-y)
        "Render all modals (bottom-to-top) and then all floating overlays (on top)."
        (if (not (overlay-stack? stack))
          #f
          (let ((rendered? #f))
            (for-each
              (lambda (ov)
                (when ((overlay-render ov) ov draw-ctx maxy maxx anchor-y)
                  (set! rendered? #t)))
              (reverse (overlay-stack-modals stack)))
            (for-each
              (lambda (ov)
                (when ((overlay-render ov) ov draw-ctx maxy maxx anchor-y)
                  (set! rendered? #t)))
              (overlay-stack-floatings stack))
            rendered?)))

(define (overlay-stack-handle-key stack ch key-ctx)
        "Dispatch key CH to the top modal on STACK. Floatings never capture keyboard input.
Automatically pops the top modal when its handler returns 'close."
        (let ((top (overlay-stack-top-modal stack)))
          (if (not top)
            'pass
            (let* ((ctx (if (assq 'overlay-stack key-ctx)
                          key-ctx
                          (append (list (cons 'overlay-stack stack)
                                        (cons 'push-modal! (lambda (m) (overlay-stack-push-modal! stack m))))
                                  key-ctx)))
                   (res ((overlay-on-key top) top ch ctx)))
              (when (and (eq? res 'close) (eq? (overlay-stack-top-modal stack) top))
                (overlay-stack-pop-modal! stack))
              res))))

(define (overlay-stack-handle-click stack target click-ctx)
        "Dispatch click TARGET to the top modal on STACK. Returns #f if no modal is active."
        (let ((top (overlay-stack-top-modal stack)))
          (if top
            ((overlay-on-click top) top target click-ctx)
            #f)))

;;;
;;; Reusable Micro-Dialogs
;;;
(define* (make-confirm-dialog #:key
                              (title "Confirm")
                              (message "")
                              (confirm-label "Confirm")
                              (cancel-label "Cancel")
                              (danger? #f)
                              (on-confirm #f)
                              (on-cancel #f))
         "Create a centered confirmation modal <overlay>.
y/Enter confirms & closes; n/Esc cancels & closes."
         (let ((resolved? #f))
           (make-modal-overlay
             #:title title
             #:width 0.5
             #:height 0.3
             #:body-renderer
             (lambda (_ov _st _rect)
               (vstack-widget
                 (list (text-widget (ensure-string (or message "")) #:wrap? #t)
                       (hstack-widget
                         (list (pill-widget (string-append "y/Enter: " confirm-label)
                                            #:role (if danger? 'error 'accent))
                               (pill-widget (string-append "n/Esc: " cancel-label)
                                            #:role 'dim))
                         #:gap 2))
                 #:gap 1))
             #:footer-hints `(("y/Enter" . ,confirm-label) ("n/Esc" . ,cancel-label))
             #:key-bindings
             `(((#\y #\Y enter)
                . ,(lambda (_ov _st _ctx)
                     (unless resolved?
                       (set! resolved? #t)
                       (when (procedure? on-confirm) (on-confirm)))
                     'close))
               ((#\n #\N esc)
                . ,(lambda (_ov _st _ctx)
                     (unless resolved?
                       (set! resolved? #t)
                       (when (procedure? on-cancel) (on-cancel)))
                     'close)))
             #:on-close
             (lambda (_ov _st)
               (unless resolved?
                 (set! resolved? #t)
                 (when (procedure? on-cancel) (on-cancel)))))))

(define* (make-input-dialog #:key
                            (title "Input")
                            (prompt #f)
                            (initial "")
                            (placeholder "")
                            (validator #f)
                            (on-submit #f)
                            (on-cancel #f))
         "Create a centered single-line input modal <overlay> with optional VALIDATOR.
Enter validates & submits; Esc cancels."
         (let* ((init-str (ensure-string (or initial "")))
                (init-state (list (cons 'buffer init-str)
                                  (cons 'cursor (string-length init-str))
                                  (cons 'error #f)))
                (resolved? #f))
           (make-modal-overlay
             #:title title
             #:width 0.6
             #:height 0.35
             #:state init-state
             #:body-renderer
             (lambda (_ov st _rect)
               (let* ((buf (or (assq-ref st 'buffer) ""))
                      (cur (or (assq-ref st 'cursor) (string-length buf)))
                      (err (assq-ref st 'error))
                      (input-w
                        (if (and (string-null? buf) (not (string-null? placeholder)))
                          (hstack-widget
                            (list (text-widget "> ▏" #:role 'accent #:bold? #t)
                                  (text-widget placeholder #:role 'dim #:dim? #t))
                            #:gap 0)
                          (text-widget
                            (string-append "> " (substring buf 0 cur) "▏" (substring buf cur))
                            #:bold? #t)))
                      (children
                        (filter identity
                                (list (and prompt (text-widget (ensure-string prompt) #:wrap? #t))
                                      input-w
                                      (and err (text-widget err #:role 'error))))))
                 (vstack-widget children #:gap 1)))
             #:footer-hints '(("Enter" . "submit") ("Esc" . "cancel"))
             #:on-unhandled-key
             (lambda (ov st ch key-ctx)
               (let ((buf (or (assq-ref st 'buffer) ""))
                     (cur (or (assq-ref st 'cursor) 0)))
                 (cond
                   ((key-matches? 'enter ch key-ctx)
                     (let ((err (and (procedure? validator) (validator buf))))
                       (if (string? err)
                         (begin
                           (set-modal-overlay-state!
                             ov
                             (list (cons 'buffer buf) (cons 'cursor cur) (cons 'error err)))
                           'handled)
                         (begin
                           (unless resolved?
                             (set! resolved? #t)
                             (when (procedure? on-submit) (on-submit buf)))
                           'close))))
                   ((key-matches? 'backspace ch key-ctx)
                     (when (> cur 0)
                       (let ((next-buf (string-append (substring buf 0 (- cur 1)) (substring buf cur))))
                         (set-modal-overlay-state!
                           ov
                           (list (cons 'buffer next-buf) (cons 'cursor (- cur 1)) (cons 'error #f)))))
                     'handled)
                   ((key-matches? 'left ch key-ctx)
                     (set-modal-overlay-state!
                       ov
                       (list (cons 'buffer buf) (cons 'cursor (max 0 (- cur 1))) (cons 'error #f)))
                     'handled)
                   ((key-matches? 'right ch key-ctx)
                     (set-modal-overlay-state!
                       ov
                       (list (cons 'buffer buf) (cons 'cursor (min (string-length buf) (+ cur 1))) (cons 'error #f)))
                     'handled)
                   ((or (and (char? ch) (char>=? ch #\space) (not (char=? ch #\del)))
                        (and (integer? ch) (>= ch 32) (not (= ch 127)) (<= ch #x10FFFF)))
                     (let* ((c (if (char? ch) ch (integer->char ch)))
                            (next-buf (string-append (substring buf 0 cur) (string c) (substring buf cur))))
                       (set-modal-overlay-state!
                         ov
                         (list (cons 'buffer next-buf) (cons 'cursor (+ cur 1)) (cons 'error #f)))
                       'handled))
                   (else 'handled))))
             #:on-close
             (lambda (_ov _st)
               (unless resolved?
                 (set! resolved? #t)
                 (when (procedure? on-cancel) (on-cancel)))))))
