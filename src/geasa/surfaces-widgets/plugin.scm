;;; (geasa surfaces-widgets plugin) — Pure-Data Composable Widget Engine
;;;
;;; Provides the core pure-data composable widget protocol (<rect>, <draw-op>,
;;; <widget>, layout/render/event dispatch), layout & chrome primitives
;;; (text, rule, pill, box, scroll-view, vstack, hstack), and reusable UI
;;; components (selectable-list, tab-bar, status-badge, toggle, key-hints).

(define-module (geasa surfaces-widgets plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (clip-str
                          pad-str
                          <rect>
                          make-rect
                          rect?
                          rect-x
                          rect-y
                          rect-width
                          rect-height
                          <draw-op>
                          make-draw-op
                          draw-op?
                          draw-op-x
                          draw-op-y
                          draw-op-text
                          draw-op-role
                          draw-op-bold?
                          draw-op-dim?
                          draw-op-reverse?
                          render-ops->lines
                          draw-ops->segments-by-row
                          <widget>
                          make-widget
                          widget?
                          widget-kind
                          widget-props
                          widget-children
                          widget-measure
                          widget-render
                          widget-on-event
                          widget-measure-size
                          widget-render-rect
                          widget-dispatch-event
                          render-widget-to-lines
                          text-widget
                          rule-widget
                          pill-widget
                          box-widget
                          scroll-view-widget
                          vstack-widget
                          hstack-widget
                          <selectable-list>
                          make-selectable-list
                          selectable-list?
                          selectable-list-items
                          selectable-list-selected-index
                          selectable-list-scroll-offset
                          selectable-list-max-visible
                          selectable-list-on-highlight
                          list-move
                          list-visible-slice
                          list-selected-item
                          selectable-list-widget
                          tab-bar-widget
                          status-badge-widget
                          toggle-widget
                          key-hints-widget))

;;;
;;; String & Geometry Helpers
;;;

(define (clip-str s max-len)
        "Clip S to at most MAX-LEN characters."
        (let ((str (ensure-string (or s "")))
              (n (max 0 max-len)))
          (if (> (string-length str) n)
            (substring str 0 n)
            str)))

(define (pad-str s width)
        "Clip or right-pad S with spaces to exact WIDTH."
        (let* ((w (max 0 width))
               (clipped (clip-str s w))
               (pad (max 0 (- w (string-length clipped)))))
          (if (> pad 0)
            (string-append clipped (make-string pad #\space))
            clipped)))

(define-record-type <rect>
                    (%make-rect x y width height)
                    rect?
                    (x rect-x)
                    (y rect-y)
                    (width rect-width)
                    (height rect-height))

(define* (make-rect x y width height)
         "Construct a <rect> bounding box clamped to non-negative dimensions."
         (%make-rect x y (max 0 width) (max 0 height)))

(define-record-type <draw-op>
                    (%make-draw-op x y text role bold? dim? reverse?)
                    draw-op?
                    (x draw-op-x)
                    (y draw-op-y)
                    (text draw-op-text)
                    (role draw-op-role)
                    (bold? draw-op-bold?)
                    (dim? draw-op-dim?)
                    (reverse? draw-op-reverse?))

(define* (make-draw-op x y text #:key (role 'default) (bold? #f) (dim? #f) (reverse? #f))
         "Construct a <draw-op> at (X, Y) with TEXT and optional styling."
         (%make-draw-op x y (ensure-string (or text "")) role bold? dim? reverse?))

(define (render-ops->lines ops width height)
        "Rasterize <draw-op> list OPS into HEIGHT plain-text lines of WIDTH columns."
        (let* ((w (max 0 width))
               (h (max 0 height))
               (rows (make-vector h #f)))
          (let loop ((i 0))
                    (when (< i h)
                      (vector-set! rows i (make-string w #\space))
                      (loop (+ i 1))))
          (for-each
            (lambda (op)
              (when (draw-op? op)
                (let ((x (draw-op-x op))
                      (y (draw-op-y op))
                      (txt (draw-op-text op)))
                  (when (and (>= y 0) (< y h) (< x w))
                    (let* ((row (vector-ref rows y))
                           (start-col (max 0 x))
                           (src-start (if (< x 0) (- x) 0))
                           (avail (- w start-col))
                           (len (min avail (max 0 (- (string-length txt) src-start)))))
                      (when (> len 0)
                        (string-copy! row start-col txt src-start (+ src-start len))))))))
            ops)
          (vector->list rows)))

(define (draw-ops->segments-by-row ops width height)
        "Group <draw-op> list OPS into HEIGHT rows of styled segments:
each row is a list of (TEXT ROLE BOLD? DIM? [REVERSE?]) tuples sorted by X."
        (let* ((w (max 0 width))
               (h (max 0 height))
               (buckets (make-vector h '())))
          (for-each
            (lambda (op)
              (when (draw-op? op)
                (let ((x (draw-op-x op))
                      (y (draw-op-y op))
                      (txt (draw-op-text op)))
                  (when (and (>= y 0) (< y h) (< x w) (> (string-length txt) 0))
                    (let* ((start-col (max 0 x))
                           (src-start (if (< x 0) (- x) 0))
                           (avail (- w start-col))
                           (len (min avail (max 0 (- (string-length txt) src-start)))))
                      (when (> len 0)
                        (let ((seg (list start-col
                                         (substring txt src-start (+ src-start len))
                                         (draw-op-role op)
                                         (draw-op-bold? op)
                                         (draw-op-dim? op)
                                         (draw-op-reverse? op))))
                          (vector-set! buckets y (cons seg (vector-ref buckets y))))))))))
            ops)
          (let loop ((i 0) (acc '()))
                    (if (>= i h)
                      (reverse acc)
                      (let* ((segs (sort (vector-ref buckets i)
                                         (lambda (a b) (< (car a) (car b)))))
                             (filled
                               (let fill-loop ((rest segs) (col 0) (out '()))
                                              (if (null? rest)
                                                (reverse out)
                                                (let* ((s (car rest))
                                                       (sx (car s))
                                                       (stxt (cadr s))
                                                       (srole (caddr s))
                                                       (sbold (cadddr s))
                                                       (sdim (car (cddddr s)))
                                                       (srev (cadr (cddddr s)))
                                                       (gap (max 0 (- sx col)))
                                                       (out* (if (> gap 0)
                                                               (cons (list (make-string gap #\space)
                                                                           'default #f #f #f)
                                                                     out)
                                                               out))
                                                       (next-col (+ (max col sx) (string-length stxt))))
                                                  (fill-loop (cdr rest)
                                                             next-col
                                                             (cons (list stxt srole sbold sdim srev) out*)))))))
                        (loop (+ i 1) (cons filled acc)))))))

;;;
;;; Core Pure-Data Widget Protocol (<widget>)
;;;

(define-record-type <widget>
                    (%make-widget kind props children measure-fn render-fn on-event-fn)
                    widget?
                    (kind widget-kind)
                    (props widget-props)
                    (children widget-children)
                    (measure-fn widget-measure)
                    (render-fn widget-render)
                    (on-event-fn widget-on-event))

(define* (make-widget kind
                      #:key
                      (props '())
                      (children '())
                      (measure #f)
                      (render #f)
                      (on-event #f))
         "Construct a composable <widget> node."
         (%make-widget kind props children measure render on-event))

(define (widget-measure-size widget avail-w avail-h)
        "Return (width . height) preferred size for WIDGET within AVAIL-W x AVAIL-H."
        (let ((w (max 0 avail-w))
              (h (max 0 avail-h)))
          (if (and (widget? widget) (procedure? (widget-measure widget)))
            ((widget-measure widget) widget w h)
            (cons w h))))

(define (widget-render-rect widget rect)
        "Render WIDGET into RECT, returning a flat list of <draw-op> records."
        (if (and (widget? widget) (rect? rect) (procedure? (widget-render widget)))
          ((widget-render widget) widget rect)
          '()))

(define (widget-dispatch-event widget event)
        "Dispatch EVENT to WIDGET and its children; returns list of emitted results."
        (if (not (widget? widget))
          '()
          (let* ((self-res (if (procedure? (widget-on-event widget))
                             (let ((r ((widget-on-event widget) widget event)))
                               (if r (list r) '()))
                             '()))
                 (child-res (append-map (lambda (c) (widget-dispatch-event c event))
                                        (widget-children widget))))
            (append self-res child-res))))

(define (render-widget-to-lines widget width height)
        "Convenience helper: render WIDGET into WIDTH x HEIGHT plain-text lines."
        (render-ops->lines (widget-render-rect widget (make-rect 0 0 width height))
                           width height))

;;;
;;; Standard Layout & Chrome Combinators
;;;

(define* (text-widget text #:key (role 'default) (bold? #f) (dim? #f) (reverse? #f) (wrap? #f))
         "Single-line or wrapped text widget."
         (let ((str (ensure-string (or text ""))))
           (make-widget
             'text
             #:props `((text . ,str) (role . ,role) (bold? . ,bold?)
                       (dim? . ,dim?) (reverse? . ,reverse?) (wrap? . ,wrap?))
             #:measure
             (lambda (_self avail-w _avail-h)
               (if (not wrap?)
                 (cons (min avail-w (string-length str)) 1)
                 (let ((w (max 1 avail-w)))
                   (cons w (max 1 (ceiling (/ (max 1 (string-length str)) w)))))))
             #:render
             (lambda (_self rect)
               (let ((x (rect-x rect))
                     (y (rect-y rect))
                     (w (rect-width rect))
                     (h (rect-height rect)))
                 (if (or (<= w 0) (<= h 0))
                   '()
                   (if (not wrap?)
                     (list (make-draw-op x y (clip-str str w)
                                         #:role role #:bold? bold?
                                         #:dim? dim? #:reverse? reverse?))
                     (let loop ((pos 0) (row 0) (ops '()))
                               (if (or (>= row h) (>= pos (string-length str)))
                                 (reverse ops)
                                 (let* ((end (min (string-length str) (+ pos w)))
                                        (chunk (substring str pos end)))
                                   (loop end (+ row 1)
                                         (cons (make-draw-op x (+ y row) chunk
                                                             #:role role #:bold? bold?
                                                             #:dim? dim? #:reverse? reverse?)
                                               ops))))))))))))

(define* (rule-widget #:key (char #\─) (role 'dim) (title #f))
         "Horizontal rule widget, optionally with an inline title."
         (make-widget
           'rule
           #:props `((char . ,char) (role . ,role) (title . ,title))
           #:measure (lambda (_self avail-w _avail-h) (cons avail-w 1))
           #:render
           (lambda (_self rect)
             (let ((x (rect-x rect))
                   (y (rect-y rect))
                   (w (rect-width rect))
                   (h (rect-height rect)))
               (if (or (<= w 0) (<= h 0))
                 '()
                 (let ((line
                         (if (and (string? title) (> (string-length title) 0) (>= w 4))
                           (let* ((tag (clip-str (string-append " " title " ") (- w 2)))
                                  (rem (max 0 (- w 2 (string-length tag)))))
                             (string-append (make-string 2 char)
                                            tag
                                            (make-string rem char)))
                           (make-string w char))))
                   (list (make-draw-op x y line #:role role #:dim? (eq? role 'dim)))))))))

(define* (pill-widget label #:key (role 'accent) (bold? #t))
         "Compact badge/pill widget: [LABEL]."
         (let ((txt (string-append "[" (ensure-string (or label "")) "]")))
           (make-widget
             'pill
             #:props `((label . ,label) (role . ,role) (bold? . ,bold?))
             #:measure (lambda (_self avail-w _avail-h) (cons (min avail-w (string-length txt)) 1))
             #:render
             (lambda (_self rect)
               (if (or (<= (rect-width rect) 0) (<= (rect-height rect) 0))
                 '()
                 (list (make-draw-op (rect-x rect) (rect-y rect)
                                     (clip-str txt (rect-width rect))
                                     #:role role #:bold? bold?)))))))

(define* (box-widget child #:key (title #f) (border-role 'dim) (role #f) (padding 0))
         "Bordered box container around CHILD with optional TITLE."
         (let ((border-role (or role border-role)))
           (make-widget
             'box
             #:props `((title . ,title) (border-role . ,border-role) (padding . ,padding))
             #:children (if child (list child) '())
             #:measure
             (lambda (_self avail-w avail-h)
               (let* ((pad (max 0 padding))
                      (inner-w (max 0 (- avail-w 2 (* 2 pad))))
                      (inner-h (max 0 (- avail-h 2 (* 2 pad))))
                      (csz (if child (widget-measure-size child inner-w inner-h) '(0 . 0))))
                 (cons (min avail-w (+ (car csz) 2 (* 2 pad)))
                       (min avail-h (+ (cdr csz) 2 (* 2 pad))))))
             #:render
             (lambda (_self rect)
               (let ((x (rect-x rect))
                     (y (rect-y rect))
                     (w (rect-width rect))
                     (h (rect-height rect)))
                 (if (or (< w 2) (< h 2))
                   '()
                   (let* ((inner-w (- w 2))
                          (top-bar
                            (if (and (string? title) (> (string-length title) 0) (>= inner-w 2))
                              (let* ((tag (clip-str (string-append " " title " ") inner-w))
                                     (rem (max 0 (- inner-w (string-length tag)))))
                                (string-append "┌" tag (make-string rem #\─) "┐"))
                              (string-append "┌" (make-string inner-w #\─) "┐")))
                          (bot-bar (string-append "└" (make-string inner-w #\─) "┘"))
                          (side-ops
                            (let loop ((row 1) (acc '()))
                                      (if (>= row (- h 1))
                                        acc
                                        (loop (+ row 1)
                                              (cons (make-draw-op x (+ y row) "│" #:role border-role)
                                                    (cons (make-draw-op (+ x (- w 1)) (+ y row) "│" #:role border-role)
                                                          acc))))))
                          (pad (max 0 padding))
                          (child-rect (make-rect (+ x 1 pad)
                                                 (+ y 1 pad)
                                                 (max 0 (- inner-w (* 2 pad)))
                                                 (max 0 (- h 2 (* 2 pad)))))
                          (child-ops (if (and child (> (rect-width child-rect) 0) (> (rect-height child-rect) 0))
                                       (widget-render-rect child child-rect)
                                       '())))
                     (append (list (make-draw-op x y top-bar #:role border-role)
                                   (make-draw-op x (+ y (- h 1)) bot-bar #:role border-role))
                             side-ops
                             child-ops))))))))

(define* (scroll-view-widget child #:key (offset 0))
         "Scrollable viewport around CHILD starting at vertical line OFFSET."
         (make-widget
           'scroll-view
           #:props `((offset . ,offset))
           #:children (if child (list child) '())
           #:measure
           (lambda (_self avail-w avail-h)
             (if child (widget-measure-size child avail-w avail-h) (cons avail-w avail-h)))
           #:render
           (lambda (_self rect)
             (let ((x (rect-x rect))
                   (y (rect-y rect))
                   (w (rect-width rect))
                   (h (rect-height rect))
                   (off (max 0 offset)))
               (if (or (not child) (<= w 0) (<= h 0))
                 '()
                 (let* ((csz (widget-measure-size child w 10000))
                        (total-h (max h (cdr csz)))
                        (raw-ops (widget-render-rect child (make-rect x 0 w total-h))))
                   (filter-map
                     (lambda (op)
                       (let ((ny (+ y (- (draw-op-y op) off))))
                         (and (>= ny y) (< ny (+ y h))
                              (make-draw-op (draw-op-x op) ny (draw-op-text op)
                                            #:role (draw-op-role op)
                                            #:bold? (draw-op-bold? op)
                                            #:dim? (draw-op-dim? op)
                                            #:reverse? (draw-op-reverse? op)))))
                     raw-ops)))))))

(define* (vstack-widget children #:key (gap 0))
         "Vertical stack of CHILDREN widgets separated by GAP rows."
         (make-widget
           'vstack
           #:props `((gap . ,gap))
           #:children children
           #:measure
           (lambda (_self avail-w avail-h)
             (let loop ((rest children) (max-w 0) (tot-h 0) (first? #t))
                       (if (null? rest)
                         (cons (min avail-w max-w) (min avail-h tot-h))
                         (let* ((g (if first? 0 gap))
                                (rem-h (max 0 (- avail-h tot-h g)))
                                (sz (widget-measure-size (car rest) avail-w rem-h)))
                           (loop (cdr rest)
                                 (max max-w (car sz))
                                 (+ tot-h g (cdr sz))
                                 #f)))))
           #:render
           (lambda (_self rect)
             (let ((x (rect-x rect))
                   (y (rect-y rect))
                   (w (rect-width rect))
                   (h (rect-height rect)))
               (let loop ((rest children) (cy y) (rem-h h) (ops '()))
                         (if (or (null? rest) (<= rem-h 0))
                           ops
                           (let* ((c (car rest))
                                  (sz (widget-measure-size c w rem-h))
                                  (ch (min rem-h (max 0 (cdr sz))))
                                  (c-ops (if (> ch 0)
                                           (widget-render-rect c (make-rect x cy w ch))
                                           '()))
                                  (step (+ ch gap)))
                             (loop (cdr rest) (+ cy step) (max 0 (- rem-h step))
                                   (append ops c-ops)))))))))

(define* (hstack-widget children #:key (gap 1))
         "Horizontal stack of CHILDREN widgets separated by GAP columns."
         (make-widget
           'hstack
           #:props `((gap . ,gap))
           #:children children
           #:measure
           (lambda (_self avail-w avail-h)
             (let loop ((rest children) (tot-w 0) (max-h 0) (first? #t))
                       (if (null? rest)
                         (cons (min avail-w tot-w) (min avail-h max-h))
                         (let* ((g (if first? 0 gap))
                                (rem-w (max 0 (- avail-w tot-w g)))
                                (sz (widget-measure-size (car rest) rem-w avail-h)))
                           (loop (cdr rest)
                                 (+ tot-w g (car sz))
                                 (max max-h (cdr sz))
                                 #f)))))
           #:render
           (lambda (_self rect)
             (let ((x (rect-x rect))
                   (y (rect-y rect))
                   (w (rect-width rect))
                   (h (rect-height rect)))
               (let loop ((rest children) (cx x) (rem-w w) (ops '()))
                         (if (or (null? rest) (<= rem-w 0))
                           ops
                           (let* ((c (car rest))
                                  (sz (widget-measure-size c rem-w h))
                                  (cw (min rem-w (max 0 (car sz))))
                                  (c-ops (if (> cw 0)
                                           (widget-render-rect c (make-rect cx y cw h))
                                           '()))
                                  (step (+ cw gap)))
                             (loop (cdr rest) (+ cx step) (max 0 (- rem-w step))
                                   (append ops c-ops)))))))))

;;;
;;; Reusable UI Components
;;;

(define-record-type <selectable-list>
                    (%make-selectable-list items selected-index scroll-offset max-visible on-highlight)
                    selectable-list?
                    (items selectable-list-items)
                    (selected-index selectable-list-selected-index)
                    (scroll-offset selectable-list-scroll-offset)
                    (max-visible selectable-list-max-visible)
                    (on-highlight selectable-list-on-highlight))

(define (clamp-list-index idx count)
        (if (<= count 0)
          0
          (min (max 0 idx) (- count 1))))

(define (clamp-scroll-offset sel off cap count)
        (if (<= count cap)
          0
          (let* ((max-off (- count cap))
                 (off1 (min (max 0 off) max-off))
                 (off2 (if (< sel off1) sel off1))
                 (off3 (if (>= sel (+ off2 cap)) (- (+ sel 1) cap) off2)))
            (min (max 0 off3) max-off))))

(define* (make-selectable-list items
                               #:key
                               (selected-index 0)
                               (scroll-offset 0)
                               (max-visible 10)
                               (on-highlight #f))
         "Construct a <selectable-list> state with clamped index and scroll offset."
         (let* ((lst (if (list? items) items '()))
                (count (length lst))
                (cap (max 1 max-visible))
                (sel (clamp-list-index selected-index count))
                (off (clamp-scroll-offset sel scroll-offset cap count)))
           (%make-selectable-list lst sel off cap on-highlight)))

(define (list-selected-item sl)
        "Return the currently selected item of SL, or #f when SL is empty."
        (if (not (selectable-list? sl))
          #f
          (let* ((items (selectable-list-items sl))
                 (count (length items)))
            (if (= count 0)
              #f
              (list-ref items (clamp-list-index (selectable-list-selected-index sl) count))))))

(define* (list-visible-slice sl #:optional max-visible-override)
         "Return (values visible-items offset): the window of at most MAX-VISIBLE
items of SL positioned so the selected item stays on screen."
         (if (not (selectable-list? sl))
           (values '() 0)
           (let* ((items (selectable-list-items sl))
                  (count (length items))
                  (cap (max 1 (or max-visible-override (selectable-list-max-visible sl))))
                  (sel (clamp-list-index (selectable-list-selected-index sl) count))
                  (off (clamp-scroll-offset sel (selectable-list-scroll-offset sl) cap count)))
             (if (<= count cap)
               (values items 0)
               (values (take (drop items off) cap) off)))))

(define (list-move sl delta)
        "Move selection in SL by DELTA, clamping to bounds and updating scroll offset.
Invokes on-highlight hook if present and selection changed. Returns new <selectable-list>."
        (if (not (selectable-list? sl))
          sl
          (let* ((items (selectable-list-items sl))
                 (count (length items))
                 (old-sel (selectable-list-selected-index sl))
                 (new-sel (clamp-list-index (+ old-sel delta) count))
                 (cap (selectable-list-max-visible sl))
                 (new-off (clamp-scroll-offset new-sel (selectable-list-scroll-offset sl) cap count))
                 (hook (selectable-list-on-highlight sl)))
            (when (and (procedure? hook) (> count 0) (not (= old-sel new-sel)))
              (hook new-sel (list-ref items new-sel)))
            (%make-selectable-list items new-sel new-off cap hook))))

(define* (selectable-list-widget sl
                                 #:key
                                 (item-renderer (lambda (item _selected?) (ensure-string item)))
                                 (role 'default)
                                 (selected-role 'accent)
                                 (indicator "▸ ")
                                 (empty-text "(empty)"))
         "Renderable widget for <selectable-list> SL."
         (make-widget
           'selectable-list
           #:props `((list . ,sl) (role . ,role) (selected-role . ,selected-role))
           #:measure
           (lambda (_self avail-w avail-h)
             (let* ((items (if (selectable-list? sl) (selectable-list-items sl) '()))
                    (count (length items))
                    (cap (if (selectable-list? sl) (selectable-list-max-visible sl) 10))
                    (h (if (= count 0) 1 (min avail-h (min count cap))))
                    (ind-w (string-length indicator))
                    (max-w (if (= count 0)
                             (string-length empty-text)
                             (fold (lambda (it acc)
                                     (max acc (+ ind-w (string-length (ensure-string (item-renderer it #f))))))
                                   0 items))))
               (cons (min avail-w max-w) h)))
           #:render
           (lambda (_self rect)
             (let ((x (rect-x rect))
                   (y (rect-y rect))
                   (w (rect-width rect))
                   (h (rect-height rect)))
               (if (or (<= w 0) (<= h 0))
                 '()
                 (if (or (not (selectable-list? sl)) (null? (selectable-list-items sl)))
                   (list (make-draw-op x y (clip-str empty-text w) #:role 'dim #:dim? #t))
                   (call-with-values
                     (lambda () (list-visible-slice sl h))
                     (lambda (visible offset)
                       (let ((sel (selectable-list-selected-index sl))
                             (blank-ind (make-string (string-length indicator) #\space)))
                         (let loop ((rest visible) (idx offset) (row 0) (ops '()))
                                   (if (or (null? rest) (>= row h))
                                     (reverse ops)
                                     (let* ((selected? (= idx sel))
                                            (prefix (if selected? indicator blank-ind))
                                            (txt (ensure-string (item-renderer (car rest) selected?)))
                                            (line (clip-str (string-append prefix txt) w)))
                                       (loop (cdr rest)
                                             (+ idx 1)
                                             (+ row 1)
                                             (cons (make-draw-op x (+ y row) line
                                                                 #:role (if selected? selected-role role)
                                                                 #:bold? selected?
                                                                 #:reverse? selected?)
                                                   ops))))))))))))))

(define (tab-label-text tab counts idx)
        (let* ((base (if (pair? tab) (ensure-string (cdr tab)) (ensure-string tab)))
               (cnt (cond
                      ((and (list? counts) (pair? tab) (assoc (car tab) counts))
                        => cdr)
                      ((and (list? counts) (< idx (length counts)))
                        (list-ref counts idx))
                      (else #f))))
          (if cnt
            (format #f " ~a (~a) " base cnt)
            (format #f " ~a " base))))

(define* (tab-bar-widget tabs
                         #:key
                         (active 0)
                         (counts '())
                         (role 'default)
                         (active-role 'accent)
                         (gap 1)
                         (on-select #f))
         "Horizontal tab bar with optional COUNTS badges and click hit-testing."
         (let ((tab-list (if (list? tabs) tabs '())))
           (make-widget
             'tab-bar
             #:props `((tabs . ,tab-list) (active . ,active) (counts . ,counts))
             #:measure
             (lambda (_self avail-w _avail-h)
               (let loop ((rest tab-list) (idx 0) (tot 0))
                         (if (null? rest)
                           (cons (min avail-w tot) 1)
                           (let ((lbl (tab-label-text (car rest) counts idx))
                                 (g (if (= idx 0) 0 gap)))
                             (loop (cdr rest) (+ idx 1) (+ tot g (string-length lbl)))))))
             #:render
             (lambda (_self rect)
               (let ((x (rect-x rect))
                     (y (rect-y rect))
                     (w (rect-width rect))
                     (h (rect-height rect)))
                 (if (or (<= w 0) (<= h 0) (null? tab-list))
                   '()
                   (let loop ((rest tab-list) (idx 0) (cx x) (ops '()))
                             (if (or (null? rest) (>= cx (+ x w)))
                               (reverse ops)
                               (let* ((lbl (tab-label-text (car rest) counts idx))
                                      (avail (- (+ x w) cx))
                                      (clipped (clip-str lbl avail))
                                      (active? (= idx active)))
                                 (loop (cdr rest)
                                       (+ idx 1)
                                       (+ cx (string-length clipped) gap)
                                       (cons (make-draw-op cx y clipped
                                                           #:role (if active? active-role role)
                                                           #:bold? active?
                                                           #:reverse? active?)
                                             ops))))))))
             #:on-event
             (lambda (_self event)
               (if (and (procedure? on-select)
                        (pair? event)
                        (eq? (car event) 'click))
                 (let ((ex (cadr event)))
                   (let loop ((rest tab-list) (idx 0) (cx 0))
                             (if (null? rest)
                               #f
                               (let* ((lbl (tab-label-text (car rest) counts idx))
                                      (len (string-length lbl)))
                                 (if (and (>= ex cx) (< ex (+ cx len)))
                                   (begin (on-select idx) idx)
                                   (loop (cdr rest) (+ idx 1) (+ cx len gap)))))))
                 #f)))))

(define (status-badge-defaults status)
        (case status
          ((ok) (values "✓" 'status #t #f))
          ((info) (values "●" 'accent #t #f))
          ((warn) (values "▲" 'warn #t #f))
          ((error) (values "✗" 'error #t #f))
          ((muted) (values "○" 'dim #f #t))
          (else (values "·" 'default #f #f))))

(define* (status-badge-widget status label #:key (glyph #f))
         "Semantic status badge ('ok, 'info, 'warn, 'error, 'muted) with themed glyph and label."
         (call-with-values
           (lambda () (status-badge-defaults status))
           (lambda (def-glyph role bold? dim?)
             (let* ((g (or glyph def-glyph))
                    (txt (format #f "~a ~a" g (ensure-string (or label "")))))
               (make-widget
                 'status-badge
                 #:props `((status . ,status) (label . ,label) (glyph . ,g))
                 #:measure (lambda (_self avail-w _avail-h) (cons (min avail-w (string-length txt)) 1))
                 #:render
                 (lambda (_self rect)
                   (if (or (<= (rect-width rect) 0) (<= (rect-height rect) 0))
                     '()
                     (list (make-draw-op (rect-x rect) (rect-y rect)
                                         (clip-str txt (rect-width rect))
                                         #:role role #:bold? bold? #:dim? dim?)))))))))

(define* (toggle-widget active?
                        #:key
                        (locked? #f)
                        (on-label "ON")
                        (off-label "OFF")
                        (locked-label "LOCKED")
                        (on-toggle #f))
         "Boolean toggle pill with optional locked/protected indicator."
         (let* ((base (if active?
                        (format #f "[● ~a]" on-label)
                        (format #f "[○ ~a]" off-label)))
                (txt (if locked?
                       (format #f "~a ~a" base locked-label)
                       base))
                (role (cond (locked? 'warn)
                            (active? 'status)
                            (else 'dim))))
           (make-widget
             'toggle
             #:props `((active? . ,active?) (locked? . ,locked?))
             #:measure (lambda (_self avail-w _avail-h) (cons (min avail-w (string-length txt)) 1))
             #:render
             (lambda (_self rect)
               (if (or (<= (rect-width rect) 0) (<= (rect-height rect) 0))
                 '()
                 (list (make-draw-op (rect-x rect) (rect-y rect)
                                     (clip-str txt (rect-width rect))
                                     #:role role #:bold? active? #:dim? (and (not active?) (not locked?))))))
             #:on-event
             (lambda (_self event)
               (if (and (not locked?)
                        (procedure? on-toggle)
                        (or (eq? event 'toggle)
                            (and (pair? event) (memq (car event) '(toggle click)))))
                 (begin (on-toggle (not active?)) (not active?))
                 #f)))))

(define* (key-hints-widget hints #:key (gap 2) (key-role 'accent) (label-role 'dim))
         "Aligned footer strip of [key] label pairs from HINTS alist."
         (let ((hint-list (if (list? hints) hints '())))
           (make-widget
             'key-hints
             #:props `((hints . ,hint-list) (gap . ,gap))
             #:measure
             (lambda (_self avail-w _avail-h)
               (let loop ((rest hint-list) (idx 0) (tot 0))
                         (if (null? rest)
                           (cons (min avail-w tot) 1)
                           (let* ((h (car rest))
                                  (k (format #f "[~a]" (ensure-string (car h))))
                                  (v (format #f " ~a" (ensure-string (cdr h))))
                                  (g (if (= idx 0) 0 gap)))
                             (loop (cdr rest) (+ idx 1) (+ tot g (string-length k) (string-length v)))))))
             #:render
             (lambda (_self rect)
               (let ((x (rect-x rect))
                     (y (rect-y rect))
                     (w (rect-width rect))
                     (h (rect-height rect)))
                 (if (or (<= w 0) (<= h 0) (null? hint-list))
                   '()
                   (let loop ((rest hint-list) (cx x) (ops '()))
                             (if (or (null? rest) (>= cx (+ x w)))
                               (reverse ops)
                               (let* ((pair (car rest))
                                      (k-str (format #f "[~a]" (ensure-string (car pair))))
                                      (v-str (format #f " ~a" (ensure-string (cdr pair))))
                                      (avail1 (- (+ x w) cx))
                                      (k-clip (clip-str k-str avail1))
                                      (cx1 (+ cx (string-length k-clip)))
                                      (avail2 (- (+ x w) cx1))
                                      (v-clip (clip-str v-str avail2))
                                      (cx2 (+ cx1 (string-length v-clip) gap))
                                      (ops1 (cons (make-draw-op cx y k-clip #:role key-role #:bold? #t) ops))
                                      (ops2 (if (> (string-length v-clip) 0)
                                              (cons (make-draw-op cx1 y v-clip #:role label-role #:dim? (eq? label-role 'dim))
                                                    ops1)
                                              ops1)))
                                 (loop (cdr rest) cx2 ops2))))))))))
