;;; (geasa surfaces-widgets plugin) — Pure-Data Composable Widget Engine
;;;
;;; Provides the core pure-data composable widget protocol (<rect>, <draw-op>,
;;; <widget>, layout/render/event dispatch), layout & chrome primitives
;;; (text, rule, pill, box, scroll-view, vstack, hstack), and reusable UI
;;; components (selectable-list, tab-bar, status-badge, toggle, key-hints).

(define-module (geasa surfaces-widgets plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (ice-9 format)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:export
               (clip-str pad-str
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
                         key-hints-widget
                         table-widget
                         <filter-state>
                         make-filter-state
                         filter-state?
                         filter-state-query
                         filter-state-cursor
                         filter-state-active?
                         set-filter-state-active!
                         filter-state-placeholder
                         filter-handle-key
                         fuzzy-subsequence-score
                         filter-selectable-list
                         filter-input-widget
                         %braille-spinner-frames
                         %ascii-spinner-frames
                         spinner-widget
                         progress-bar-widget
                         meter-widget))

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
        "Dispatch EVENT to WIDGET's on-event handler or children. Returns first non-#f result or #f."
        (if (widget? widget)
          (let ((handler (widget-on-event widget)))
            (or
              (and (procedure? handler) (handler widget event))
              (any
                (lambda (c)
                  (widget-dispatch-event c event))
                (widget-children widget))))
          #f))

(define (render-widget-to-lines widget width height)
        "Convenience helper: render WIDGET into WIDTH x HEIGHT plain-text lines."
        (render-ops->lines (widget-render-rect widget (make-rect 0 0 width height))
                           width height))

;;;
;;; Standard Layout & Chrome Combinators
;;;
(define*
  (text-widget text
               #:key
               (role 'default)
               (bold? #f)
               (dim? #f)
               (reverse? #f)
               (wrap? #f)
               (align 'left))
  "Single-line or wrapped text widget with optional horizontal alignment."
  (let ((str (ensure-string (or text ""))))
    (make-widget 'text
                 #:props
                 `((text . ,str) (role . ,role)
                   (bold? . ,bold?)
                   (dim? . ,dim?)
                   (reverse? . ,reverse?)
                   (wrap? . ,wrap?)
                   (align . ,align))
                 #:measure
                 (lambda (_self avail-w avail-h)
                   (if (not wrap?)
                     (cons (min avail-w (string-length str)) 1)
                     (let ((w (max 1 avail-w)))
                       (cons w (min avail-h (max 1 (ceiling (/ (max 1 (string-length str)) w))))))))
                 #:render
                 (lambda (_self rect)
                   (let
                       ((x (rect-x rect)) (y (rect-y rect))
                        (w (rect-width rect))
                        (h (rect-height rect)))
                     (if (or (<= w 0) (<= h 0))
                       '()
                       (let
                           ((align-x
                              (lambda (chunk)
                                (let ((len (string-length chunk)))
                                  (+ x
                                     (case align
                                       ((center) (max 0 (quotient (- w len) 2)))
                                       ((right) (max 0 (- w len)))
                                       (else 0)))))))
                         (if (not wrap?)
                           (let ((clipped (clip-str str w)))
                             (list
                               (make-draw-op (align-x clipped)
                                             y
                                             clipped
                                             #:role
                                             role
                                             #:bold?
                                             bold?
                                             #:dim?
                                             dim?
                                             #:reverse?
                                             reverse?)))
                           (let loop
                             ((pos 0) (row 0) (ops '()))
                             (if (or (>= row h) (>= pos (string-length str)))
                               (reverse ops)
                               (let*
                                   ((end (min (string-length str) (+ pos w))) (chunk (substring str pos end)))
                                 (loop end (+ row 1)
                                       (cons
                                         (make-draw-op (align-x chunk)
                                                       (+ y row)
                                                       chunk
                                                       #:role
                                                       role
                                                       #:bold?
                                                       bold?
                                                       #:dim?
                                                       dim?
                                                       #:reverse?
                                                       reverse?)
                                         ops)))))))))))))

(define*
  (rule-widget #:key (char #\─) (role 'dim) (title #f) (label #f) (dim? 'auto))
  "Horizontal rule widget, optionally with an inline title or label."
  (let
      ((lbl (or title label))
       (dim-flag?
         (if (eq? dim? 'auto)
           (eq? role 'dim)
           (and dim? #t))))
    (make-widget 'rule
                 #:props
                 `((char . ,char) (role . ,role)
                   (title . ,lbl)
                   (label . ,lbl)
                   (dim? . ,dim-flag?))
                 #:measure
                 (lambda (_self avail-w _avail-h)
                   (cons avail-w 1))
                 #:render
                 (lambda (_self rect)
                   (let
                       ((x (rect-x rect)) (y (rect-y rect))
                        (w (rect-width rect))
                        (h (rect-height rect)))
                     (if (or (<= w 0) (<= h 0))
                       '()
                       (let
                           ((line
                              (if
                                  (and (string? lbl) (> (string-length lbl) 0) (>= w 4))
                                (let*
                                    ((tag (clip-str (string-append " " lbl " ") (max 0 (- w 2))))
                                     (rem (max 0 (- w (string-length tag))))
                                     (left-n (min 2 rem))
                                     (right-n (max 0 (- rem left-n))))
                                  (string-append (make-string left-n char) tag (make-string right-n char)))
                                (make-string w char))))
                         (list (make-draw-op x y line #:role role #:dim? dim-flag?)))))))))

(define*
  (pill-widget label #:key (role 'accent) (bold? #t) (prefix "[") (suffix "]"))
  "Compact badge/pill widget: [LABEL]."
  (let
      ((txt
         (string-append (ensure-string prefix)
                        (ensure-string (or label ""))
                        (ensure-string suffix))))
    (make-widget 'pill
                 #:props
                 `((label . ,label) (role . ,role)
                   (bold? . ,bold?)
                   (prefix . ,prefix)
                   (suffix . ,suffix))
                 #:measure
                 (lambda (_self avail-w _avail-h)
                   (cons (min avail-w (string-length txt)) 1))
                 #:render
                 (lambda (_self rect)
                   (if
                       (or (<= (rect-width rect) 0) (<= (rect-height rect) 0))
                     '()
                     (list
                       (make-draw-op (rect-x rect)
                                     (rect-y rect)
                                     (clip-str txt (rect-width rect))
                                     #:role
                                     role
                                     #:bold?
                                     bold?)))))))

(define*
  (box-widget child
              #:key
              (title #f)
              (border-role 'dim)
              (role #f)
              (padding 0)
              (rounded? #t))
  "Bordered box container around CHILD with optional TITLE."
  (let*
      ((border-role (or role border-role))
       (pad (max 0 padding))
       (tl
         (if rounded?
           "╭"
           "┌"))
       (tr
         (if rounded?
           "╮"
           "┐"))
       (bl
         (if rounded?
           "╰"
           "└"))
       (br
         (if rounded?
           "╯"
           "┘")))
    (make-widget 'box
                 #:props
                 `((title . ,title) (border-role . ,border-role)
                   (padding . ,pad)
                   (rounded? . ,rounded?))
                 #:children
                 (if child
                   (list child)
                   '())
                 #:measure
                 (lambda (_self avail-w avail-h)
                   (let*
                       ((overhead (+ 2 (* 2 pad)))
                        (inner-w (max 0 (- avail-w overhead)))
                        (inner-h (max 0 (- avail-h overhead)))
                        (csz
                          (if child
                            (widget-measure-size child inner-w inner-h)
                            '(0 . 0)))
                        (title-w
                          (if
                              (and (string? title) (not (string-null? title)))
                            (+ (string-length title) 4)
                            0)))
                     (cons
                       (min avail-w (max title-w (+ (car csz) overhead)))
                       (min avail-h (+ (cdr csz) overhead)))))
                 #:render
                 (lambda (_self rect)
                   (let
                       ((x (rect-x rect)) (y (rect-y rect))
                        (w (rect-width rect))
                        (h (rect-height rect)))
                     (if (or (< w 2) (< h 2))
                       '()
                       (let*
                           ((inner-w (- w 2))
                            (top-bar
                              (if
                                  (and (string? title) (> (string-length title) 0) (>= inner-w 3))
                                (let*
                                    ((tag (clip-str (string-append "─ " title " ") inner-w))
                                     (rem (max 0 (- inner-w (string-length tag)))))
                                  (string-append tl tag (make-string rem #\─) tr))
                                (string-append tl (make-string inner-w #\─) tr)))
                            (bot-bar (string-append bl (make-string inner-w #\─) br))
                            (side-ops
                              (let loop
                                ((row 1) (acc '()))
                                (if (>= row (- h 1))
                                  (reverse acc)
                                  (loop (+ row 1)
                                        (cons
                                          (make-draw-op (+ x (- w 1)) (+ y row) "│" #:role border-role)
                                          (cons (make-draw-op x (+ y row) "│" #:role border-role) acc))))))
                            (child-rect
                              (make-rect (+ x 1 pad)
                                         (+ y 1 pad)
                                         (max 0 (- inner-w (* 2 pad)))
                                         (max 0 (- h 2 (* 2 pad)))))
                            (child-ops
                              (if
                                  (and child (> (rect-width child-rect) 0) (> (rect-height child-rect) 0))
                                (widget-render-rect child child-rect)
                                '())))
                         (append
                           (list (make-draw-op x y top-bar #:role border-role))
                           side-ops
                           (list (make-draw-op x (+ y (- h 1)) bot-bar #:role border-role))
                           child-ops))))))))

(define*
  (scroll-view-widget child-or-lines
                      #:key
                      (offset #f)
                      (scroll-offset #f)
                      (role 'default)
                      (show-scrollbar? #t))
  "Scrollable viewport around a <widget> CHILD or a list of lines/span-rows."
  (let
      ((eff-off (max 0 (or scroll-offset offset 0))))
    (if (widget? child-or-lines)
      (make-widget 'scroll-view
                   #:props
                   `((offset . ,eff-off) (scroll-offset . ,eff-off))
                   #:children
                   (list child-or-lines)
                   #:measure
                   (lambda (_self avail-w avail-h)
                     (widget-measure-size child-or-lines avail-w avail-h))
                   #:render
                   (lambda (_self rect)
                     (let
                         ((x (rect-x rect)) (y (rect-y rect))
                          (w (rect-width rect))
                          (h (rect-height rect)))
                       (if (or (<= w 0) (<= h 0))
                         '()
                         (let*
                             ((csz (widget-measure-size child-or-lines w 10000))
                              (total-h (max h (cdr csz)))
                              (raw-ops (widget-render-rect child-or-lines (make-rect x 0 w total-h))))
                           (filter-map
                             (lambda (op)
                               (let ((ny (+ y (- (draw-op-y op) eff-off))))
                                 (and (>= ny y)
                                      (< ny (+ y h))
                                      (make-draw-op (draw-op-x op)
                                                    ny
                                                    (draw-op-text op)
                                                    #:role
                                                    (draw-op-role op)
                                                    #:bold?
                                                    (draw-op-bold? op)
                                                    #:dim?
                                                    (draw-op-dim? op)
                                                    #:reverse?
                                                    (draw-op-reverse? op)))))
                             raw-ops))))))
      (let*
          ((entries
             (if (list? child-or-lines)
               child-or-lines
               '()))
           (total (length entries)))
        (make-widget 'scroll-view
                     #:props
                     `((offset . ,eff-off) (scroll-offset . ,eff-off)
                       (total-lines . ,total)
                       (show-scrollbar? . ,show-scrollbar?))
                     #:measure
                     (lambda (_self avail-w avail-h)
                       (cons avail-w (min avail-h (max 1 total))))
                     #:render
                     (lambda (_self rect)
                       (let*
                           ((x (rect-x rect)) (y (rect-y rect))
                            (w (rect-width rect))
                            (h (rect-height rect))
                            (bar? (and show-scrollbar? (> total h) (>= w 4)))
                            (content-w
                              (if bar?
                                (- w 1)
                                w))
                            (max-scroll (max 0 (- total h)))
                            (clamped (max 0 (min eff-off max-scroll)))
                            (start (max 0 (- total h clamped)))
                            (vis (take (drop entries start) (min h (max 0 (- total start)))))
                            (text-ops
                              (let loop
                                ((rest vis) (dy 0) (acc '()))
                                (if (or (null? rest) (>= dy h))
                                  (reverse acc)
                                  (let ((item (car rest)))
                                    (cond
                                      ((string? item)
                                        (loop (cdr rest) (+ dy 1)
                                              (cons (make-draw-op x (+ y dy) (clip-str item content-w) #:role role) acc)))
                                      ((and (pair? item) (every pair? item))
                                        (let sloop
                                          ((spans item) (cx x) (sacc acc))
                                          (if (or (null? spans) (>= cx (+ x content-w)))
                                            (loop (cdr rest) (+ dy 1) sacc)
                                            (let*
                                                ((sp (car spans))
                                                 (stxt (clip-str (car sp) (- (+ x content-w) cx)))
                                                 (srole
                                                   (if (pair? (cdr sp))
                                                     (cadr sp)
                                                     role))
                                                 (sbold? (and (>= (length sp) 3) (caddr sp)))
                                                 (sdim? (and (>= (length sp) 4) (cadddr sp))))
                                              (sloop (cdr spans)
                                                     (+ cx (string-length stxt))
                                                     (if (string-null? stxt)
                                                       sacc
                                                       (cons
                                                         (make-draw-op cx (+ y dy) stxt #:role srole #:bold? sbold? #:dim? sdim?)
                                                         sacc)))))))
                                      (else
                                        (loop (cdr rest) (+ dy 1)
                                              (cons
                                                (make-draw-op x (+ y dy) (clip-str (format #f "~a" item) content-w) #:role role)
                                                acc))))))))
                            (bar-ops
                              (if bar?
                                (let*
                                    ((thumb-h (max 1 (inexact->exact (floor (* h (/ h total))))))
                                     (thumb-top
                                       (if (zero? max-scroll)
                                         0
                                         (inexact->exact (round (* (- h thumb-h) (/ start max-scroll)))))))
                                  (let bloop
                                    ((dy 0) (acc '()))
                                    (if (>= dy h)
                                      (reverse acc)
                                      (let
                                          ((in-thumb? (and (>= dy thumb-top) (< dy (+ thumb-top thumb-h)))))
                                        (bloop (+ dy 1)
                                               (cons
                                                 (make-draw-op (+ x w -1)
                                                               (+ y dy)
                                                               (if in-thumb?
                                                                 "┃"
                                                                 "│")
                                                               #:role
                                                               (if in-thumb?
                                                                 'accent
                                                                 'dim)
                                                               #:dim?
                                                               (not in-thumb?))
                                                 acc))))))
                                '())))
                         (append text-ops bar-ops))))))))

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

(define*
  (selectable-list-widget sl
                          #:key
                          (item-renderer
                            (lambda (item _selected?)
                              (ensure-string item)))
                          (role 'default)
                          (selected-role 'accent)
                          (indicator "▸ ")
                          (empty-text "(empty)"))
  "Renderable widget for <selectable-list> SL. ITEM-RENDERER may return a string or <widget>."
  (make-widget 'selectable-list
               #:props
               `((list . ,sl) (role . ,role) (selected-role . ,selected-role))
               #:measure
               (lambda (_self avail-w avail-h)
                 (let*
                     ((items
                        (if (selectable-list? sl)
                          (selectable-list-items sl)
                          '()))
                      (count (length items))
                      (cap
                        (if (selectable-list? sl)
                          (selectable-list-max-visible sl)
                          10))
                      (h
                        (if (= count 0)
                          1
                          (min avail-h (min count cap))))
                      (ind-w (string-length indicator))
                      (inner-avail (max 0 (- avail-w ind-w)))
                      (max-w
                        (if (= count 0)
                          (string-length empty-text)
                          (fold
                            (lambda (it acc)
                              (let*
                                  ((r (item-renderer it #f))
                                   (rw
                                     (if (widget? r)
                                       (car (widget-measure-size r inner-avail 1))
                                       (string-length (ensure-string r)))))
                                (max acc (+ ind-w rw))))
                            0
                            items))))
                   (cons (min avail-w max-w) h)))
               #:render
               (lambda (_self rect)
                 (let
                     ((x (rect-x rect)) (y (rect-y rect))
                      (w (rect-width rect))
                      (h (rect-height rect))
                      (ind-w (string-length indicator)))
                   (if (or (<= w 0) (<= h 0))
                     '()
                     (if
                         (or (not (selectable-list? sl)) (null? (selectable-list-items sl)))
                       (list (make-draw-op x y (clip-str empty-text w) #:role 'dim #:dim? #t))
                       (call-with-values
                         (lambda ()
                           (list-visible-slice sl h))
                         (lambda (visible offset)
                           (let
                               ((sel (selectable-list-selected-index sl))
                                (blank-ind (make-string ind-w #\space)))
                             (let loop
                               ((rest visible) (idx offset) (row 0) (ops '()))
                               (if (or (null? rest) (>= row h))
                                 ops
                                 (let*
                                     ((selected? (= idx sel))
                                      (prefix
                                        (if selected?
                                          indicator
                                          blank-ind))
                                      (rendered (item-renderer (car rest) selected?))
                                      (row-ops
                                        (if (widget? rendered)
                                          (let*
                                              ((ind-op
                                                 (make-draw-op x
                                                               (+ y row)
                                                               (clip-str prefix w)
                                                               #:role
                                                               (if selected?
                                                                 selected-role
                                                                 role)
                                                               #:bold?
                                                               selected?))
                                               (inner-w (max 0 (- w ind-w)))
                                               (child-ops
                                                 (if (> inner-w 0)
                                                   (widget-render-rect rendered (make-rect (+ x ind-w) (+ y row) inner-w 1))
                                                   '())))
                                            (cons ind-op child-ops))
                                          (let
                                              ((line (clip-str (string-append prefix (ensure-string rendered)) w)))
                                            (list
                                              (make-draw-op x
                                                            (+ y row)
                                                            line
                                                            #:role
                                                            (if selected?
                                                              selected-role
                                                              role)
                                                            #:bold?
                                                            selected?
                                                            #:reverse?
                                                            selected?))))))
                                   (loop (cdr rest) (+ idx 1) (+ row 1) (append ops row-ops))))))))))))))

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
(define (table-col-header col)
        (cond
          ((and (pair? col) (string? (car col))) (car col))
          ((list? col) (or (assoc-ref col 'header) ""))
          (else "")))

(define (table-col-width col)
        (cond
          ((and (pair? col) (number? (cdr col))) (max 1 (cdr col)))
          ((list? col) (max 1 (or (assoc-ref col 'width) 12)))
          (else 12)))

(define*
  (table-widget columns
                rows
                #:key
                (selected-index #f)
                (scroll-offset 0)
                (max-visible #f)
                (gap 1)
                (indicator "▸ ")
                (empty-text "(empty)")
                (show-header? #t))
  "Aligned multi-column table widget. COLUMNS is a list of ((header . str) (width . n))
or (header . width) pairs. ROWS is a list of cell lists (strings or <widget>s)."
  (let*
      ((cols (or columns '())) (all-rows (or rows '()))
       (total (length all-rows))
       (ind-w (string-length indicator))
       (blank-ind (make-string ind-w #\space))
       (g (max 0 gap))
       (header-rows
         (if (and show-header? (not (null? cols)))
           1
           0)))
    (make-widget 'table
                 #:props
                 `((columns . ,cols) (total-rows . ,total))
                 #:measure
                 (lambda (_self avail-w avail-h)
                   (let*
                       ((col-sum
                          (fold
                            (lambda (c acc)
                              (+ acc (table-col-width c)))
                            0
                            cols))
                        (gap-sum
                          (if (null? cols)
                            0
                            (* g (max 0 (- (length cols) 1)))))
                        (tot-w (+ ind-w col-sum gap-sum))
                        (body-h
                          (if (= total 0)
                            1
                            (min total (or max-visible total))))
                        (tot-h (+ header-rows body-h)))
                     (cons (min avail-w tot-w) (min avail-h tot-h))))
                 #:render
                 (lambda (_self rect)
                   (let
                       ((x (rect-x rect)) (y (rect-y rect))
                        (w (rect-width rect))
                        (h (rect-height rect)))
                     (if (or (<= w 0) (<= h 0))
                       '()
                       (let*
                           ((render-row
                              (lambda (ry prefix cells role bold? dim?)
                                (let
                                    ((ind-op
                                       (make-draw-op x ry (clip-str prefix w) #:role role #:bold? bold? #:dim? dim?)))
                                  (let loop
                                    ((cs cols) (vs cells) (cx (+ x ind-w)) (ops (list ind-op)))
                                    (if (or (null? cs) (>= cx (+ x w)))
                                      (reverse ops)
                                      (let*
                                          ((cw (min (table-col-width (car cs)) (max 0 (- (+ x w) cx))))
                                           (cell
                                             (if (pair? vs)
                                               (car vs)
                                               ""))
                                           (cell-ops
                                             (cond
                                               ((<= cw 0) '())
                                               ((widget? cell) (widget-render-rect cell (make-rect cx ry cw 1)))
                                               (else
                                                 (list
                                                   (make-draw-op cx
                                                                 ry
                                                                 (clip-str (ensure-string cell) cw)
                                                                 #:role
                                                                 role
                                                                 #:bold?
                                                                 bold?
                                                                 #:dim?
                                                                 dim?))))))
                                        (loop (cdr cs)
                                              (if (pair? vs)
                                                (cdr vs)
                                                '()) (+ cx cw g) (append (reverse cell-ops) ops))))))))
                            (hdr-ops
                              (if (and show-header? (not (null? cols)))
                                (render-row y blank-ind (map table-col-header cols) 'dim #f #t)
                                '()))
                            (body-y (+ y header-rows))
                            (body-h (max 0 (- h header-rows))))
                         (cond
                           ((<= body-h 0) hdr-ops)
                           ((= total 0)
                             (append hdr-ops
                                     (list (make-draw-op x body-y (clip-str empty-text w) #:role 'dim #:dim? #t))))
                           (else
                             (let*
                                 ((cap (min body-h (or max-visible body-h)))
                                  (off (max 0 (min scroll-offset (max 0 (- total cap)))))
                                  (vis (take (drop all-rows off) (min cap (max 0 (- total off))))))
                               (let loop
                                 ((rest vis) (idx off) (dy 0) (acc hdr-ops))
                                 (if (or (null? rest) (>= dy body-h))
                                   acc
                                   (let*
                                       ((sel? (and (integer? selected-index) (= idx selected-index)))
                                        (prefix
                                          (if sel?
                                            indicator
                                            blank-ind))
                                        (row-ops
                                          (render-row (+ body-y dy)
                                                      prefix
                                                      (car rest)
                                                      (if sel?
                                                        'accent
                                                        'default)
                                                      sel?
                                                      #f)))
                                     (loop (cdr rest) (+ idx 1) (+ dy 1) (append acc row-ops)))))))))))))))

;;;
;;; Filter Input & Fuzzy Matcher
;;;
(define-record-type <filter-state>
                    (%make-filter-state query cursor active? placeholder)
                    filter-state?
                    (query filter-state-query)
                    (cursor filter-state-cursor)
                    (active? filter-state-active? set-filter-state-active!)
                    (placeholder filter-state-placeholder))

(define* (make-filter-state #:key
                            (query "")
                            (cursor #f)
                            (active? #f)
                            (placeholder "Type to filter..."))
         "Construct a <filter-state> for interactive list filtering."
         (let* ((q (ensure-string (or query "")))
                (len (string-length q))
                (cur (if (integer? cursor)
                       (max 0 (min len cursor))
                       len)))
           (%make-filter-state q cur (and active? #t) (ensure-string (or placeholder "")))))

(define (filter-insert-char fs ch-char)
        (let* ((q (filter-state-query fs))
               (cur (filter-state-cursor fs))
               (new-q (string-append (substring q 0 cur)
                                     (string ch-char)
                                     (substring q cur))))
          (%make-filter-state new-q (+ cur 1) #t (filter-state-placeholder fs))))

(define* (filter-handle-key fs ch #:key (key-ctx '()))
         "Handle key CH on <filter-state> FS.
Returns a new <filter-state>, or #f if CH is not a filter-input key."
         (let* ((q (filter-state-query fs))
                (cur (filter-state-cursor fs))
                (len (string-length q))
                (ph (filter-state-placeholder fs))
                (k-bs (assq-ref key-ctx 'key-backspace))
                (k-ent (assq-ref key-ctx 'key-enter))
                (k-left (assq-ref key-ctx 'key-left))
                (k-right (assq-ref key-ctx 'key-right)))
           (cond
             ((or (eq? ch 'esc) (eqv? ch #\esc) (eqv? ch #\escape) (eqv? ch 27))
               (%make-filter-state "" 0 #f ph))
             ((or (eq? ch 'enter) (eqv? ch #\newline) (eqv? ch #\return)
                  (eqv? ch 10) (eqv? ch 13) (and k-ent (eqv? ch k-ent)))
               (%make-filter-state q cur #f ph))
             ((or (eq? ch 'backspace) (eqv? ch #\backspace) (eqv? ch #\del)
                  (eqv? ch 8) (eqv? ch 127) (and k-bs (eqv? ch k-bs)))
               (if (> cur 0)
                 (let ((new-q (string-append (substring q 0 (- cur 1)) (substring q cur))))
                   (%make-filter-state new-q (- cur 1) (filter-state-active? fs) ph))
                 fs))
             ((or (eq? ch 'left) (and k-left (eqv? ch k-left)))
               (%make-filter-state q (max 0 (- cur 1)) (filter-state-active? fs) ph))
             ((or (eq? ch 'right) (and k-right (eqv? ch k-right)))
               (%make-filter-state q (min len (+ cur 1)) (filter-state-active? fs) ph))
             ((and (char? ch) (char>=? ch #\space) (not (char=? ch #\del)))
               (filter-insert-char fs ch))
             ((and (integer? ch) (>= ch 32) (not (= ch 127)) (<= ch #x10FFFF))
               (filter-insert-char fs (integer->char ch)))
             (else #f))))

(define (word-boundary-char? c)
        (or (char=? c #\-) (char=? c #\_) (char=? c #\space)
            (char=? c #\/) (char=? c #\.) (char=? c #\:)))

(define (fuzzy-subsequence-score query text)
        "Score case-insensitive subsequence match of QUERY in TEXT.
Returns an integer score (higher is better), or #f if QUERY is not a subsequence."
        (let ((q (ensure-string (or query "")))
              (t (ensure-string (or text ""))))
          (cond
            ((string-null? q) 0)
            ((string-null? t) #f)
            (else
              (let ((ql (string-downcase q))
                    (tl (string-downcase t))
                    (qlen (string-length q))
                    (tlen (string-length t)))
                (let loop ((qi 0) (ti 0) (prev-ti -2) (score 0))
                          (cond
                            ((>= qi qlen)
                              (max 1 (- score (quotient tlen 4))))
                            ((>= ti tlen) #f)
                            ((char=? (string-ref ql qi) (string-ref tl ti))
                              (let* ((contig-bonus (if (= ti (+ prev-ti 1)) 15 0))
                                     (boundary-bonus (if (or (= ti 0)
                                                             (word-boundary-char? (string-ref tl (- ti 1))))
                                                       12
                                                       0))
                                     (prefix-bonus (if (and (= qi 0) (= ti 0)) 10 0))
                                     (case-bonus (if (char=? (string-ref q qi) (string-ref t ti)) 2 0)))
                                (loop (+ qi 1)
                                      (+ ti 1)
                                      ti
                                      (+ score 10 contig-bonus boundary-bonus prefix-bonus case-bonus))))
                            (else
                              (loop qi (+ ti 1) prev-ti score)))))))))

(define* (filter-selectable-list slist query #:optional (key-fn ensure-string))
         "Return a new <selectable-list> containing items matching QUERY sorted by score.
Preserves original order when QUERY is empty, and clamps selection safely."
         (let* ((items (if (selectable-list? slist) (selectable-list-items slist) '()))
                (cap (if (selectable-list? slist) (selectable-list-max-visible slist) 10))
                (hook (if (selectable-list? slist) (selectable-list-on-highlight slist) #f))
                (q (ensure-string (or query "")))
                (kfn (or key-fn ensure-string)))
           (if (string-null? q)
             (let ((idx (if (selectable-list? slist) (selectable-list-selected-index slist) 0)))
               (make-selectable-list items #:selected-index idx #:max-visible cap #:on-highlight hook))
             (let* ((scored (filter-map
                              (lambda (item)
                                (let ((s (fuzzy-subsequence-score q (ensure-string (kfn item)))))
                                  (and s (cons s item))))
                              items))
                    (sorted (stable-sort scored (lambda (a b) (> (car a) (car b))))))
               (make-selectable-list (map cdr sorted)
                                     #:selected-index 0
                                     #:max-visible cap
                                     #:on-highlight hook)))))

(define* (filter-input-widget fs #:key (width #f) (prefix "/ "))
         "Renderable widget for <filter-state> FS showing '/ <query>▏' or dimmed placeholder."
         (let* ((pfx (ensure-string (or prefix "/ ")))
                (q (if (filter-state? fs) (filter-state-query fs) ""))
                (cur (if (filter-state? fs) (filter-state-cursor fs) (string-length q)))
                (active? (and (filter-state? fs) (filter-state-active? fs)))
                (ph (if (filter-state? fs) (filter-state-placeholder fs) ""))
                (body-txt (cond
                            (active?
                              (string-append (substring q 0 cur) "▏" (substring q cur)))
                            ((not (string-null? q)) q)
                            (else ph)))
                (total-len (+ (string-length pfx) (string-length body-txt)))
                (dim-body? (and (not active?) (string-null? q))))
           (make-widget
             'filter-input
             #:props `((query . ,q) (active? . ,active?) (cursor . ,cur))
             #:measure
             (lambda (_self avail-w _avail-h)
               (cons (min avail-w (or width total-len)) 1))
             #:render
             (lambda (_self rect)
               (let* ((x (rect-x rect))
                      (y (rect-y rect))
                      (w (min (rect-width rect) (or width (rect-width rect))))
                      (h (rect-height rect)))
                 (if (or (<= w 0) (<= h 0))
                   '()
                   (let* ((pfx-clip (clip-str pfx w))
                          (rem (max 0 (- w (string-length pfx-clip))))
                          (body-clip (clip-str body-txt rem))
                          (op1 (make-draw-op x y pfx-clip
                                             #:role (if active? 'accent 'dim)
                                             #:bold? active?
                                             #:dim? (not active?))))
                     (if (> (string-length body-clip) 0)
                       (list op1
                             (make-draw-op (+ x (string-length pfx-clip)) y body-clip
                                           #:role (if dim-body? 'dim 'default)
                                           #:dim? dim-body?))
                       (list op1)))))))))

;;;
;;; Spinner, Progress Bar & Meter
;;;
(define %braille-spinner-frames
        '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))

(define %ascii-spinner-frames
        '("|" "/" "-" "\\"))

(define* (spinner-widget frame-index #:key (label #f) (ascii? #f) (role 'accent))
         "Animated spinner widget selecting frame FRAME-INDEX from braille or ASCII frames."
         (let* ((frames (if ascii? %ascii-spinner-frames %braille-spinner-frames))
                (idx (modulo (max 0 (if (integer? frame-index) frame-index 0)) (length frames)))
                (glyph (list-ref frames idx))
                (lbl (and label (ensure-string label)))
                (txt (if (and lbl (not (string-null? lbl)))
                       (string-append glyph " " lbl)
                       glyph)))
           (make-widget
             'spinner
             #:props `((frame-index . ,idx) (ascii? . ,(and ascii? #t)) (role . ,role))
             #:measure
             (lambda (_self avail-w _avail-h)
               (cons (min avail-w (string-length txt)) 1))
             #:render
             (lambda (_self rect)
               (if (or (<= (rect-width rect) 0) (<= (rect-height rect) 0))
                 '()
                 (list (make-draw-op (rect-x rect) (rect-y rect)
                                     (clip-str txt (rect-width rect))
                                     #:role role #:bold? #t)))))))

(define (clamp-ratio r)
        (if (number? r)
          (max 0.0 (min 1.0 (exact->inexact r)))
          0.0))

(define (repeat-str s n)
        (if (<= n 0)
          ""
          (let loop ((i 0) (acc '()))
                    (if (>= i n)
                      (apply string-append acc)
                      (loop (+ i 1) (cons s acc))))))

(define* (progress-bar-widget ratio
                              #:key
                              (width 20)
                              (label #f)
                              (filled-char "█")
                              (empty-char "░")
                              (role 'accent))
         "Horizontal progress bar clamping RATIO to [0.0, 1.0]."
         (let* ((r (clamp-ratio ratio))
                (bar-w (max 1 width))
                (lbl (and label (ensure-string label)))
                (lbl-part (if (and lbl (not (string-null? lbl)))
                            (string-append " " lbl)
                            "")))
           (make-widget
             'progress-bar
             #:props `((ratio . ,r) (width . ,bar-w) (role . ,role))
             #:measure
             (lambda (_self avail-w _avail-h)
               (cons (min avail-w (+ bar-w (string-length lbl-part))) 1))
             #:render
             (lambda (_self rect)
               (let ((x (rect-x rect))
                     (y (rect-y rect))
                     (w (rect-width rect))
                     (h (rect-height rect)))
                 (if (or (<= w 0) (<= h 0))
                   '()
                   (let* ((actual-bar-w (min bar-w w))
                          (filled-n (inexact->exact (round (* r actual-bar-w))))
                          (empty-n (max 0 (- actual-bar-w filled-n)))
                          (filled-str (repeat-str filled-char filled-n))
                          (empty-str (repeat-str empty-char empty-n))
                          (rem (max 0 (- w actual-bar-w)))
                          (lbl-clip (clip-str lbl-part rem))
                          (ops '()))
                     (when (> filled-n 0)
                       (set! ops (cons (make-draw-op x y filled-str #:role role) ops)))
                     (when (> empty-n 0)
                       (set! ops (cons (make-draw-op (+ x filled-n) y empty-str #:role 'dim #:dim? #t) ops)))
                     (when (> (string-length lbl-clip) 0)
                       (set! ops (cons (make-draw-op (+ x actual-bar-w) y lbl-clip #:role 'default) ops)))
                     (reverse ops))))))))

(define* (meter-widget used
                       total
                       #:key
                       (label #f)
                       (width 20)
                       (warn-ratio 0.75)
                       (danger-ratio 0.90)
                       (show-percent? #t)
                       (unit #f))
         "Resource meter bar choosing 'ok, 'warn, or 'error role automatically."
         (let* ((ratio (if (and (number? total) (> total 0) (number? used))
                         (clamp-ratio (/ used total))
                         0.0))
                (role (cond
                        ((>= ratio danger-ratio) 'error)
                        ((>= ratio warn-ratio) 'warn)
                        (else 'ok)))
                (pct (inexact->exact (round (* ratio 100))))
                (parts (filter
                         identity
                         (list (and show-percent? (format #f "~a%" pct))
                               (and unit (format #f "(~a/~a ~a)" used total unit))
                               (and label (ensure-string label)))))
                (suffix (if (null? parts) #f (string-join parts " ")))
                (bar (progress-bar-widget ratio #:width width #:label suffix #:role role)))
           (make-widget
             'meter
             #:props `((ratio . ,ratio) (role . ,role) (used . ,used) (total . ,total))
             #:children (list bar)
             #:measure (lambda (_self avail-w avail-h) (widget-measure-size bar avail-w avail-h))
             #:render (lambda (_self rect) (widget-render-rect bar rect)))))