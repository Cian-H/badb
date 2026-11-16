;;; (geasa surfaces-tui widgets) — Composable TUI Widgets & Visual Chrome
;;;
;;; Provides a pure-data composable widget system (<widget>, layout/render
;;; protocol, combinators), adaptive input box demarcation, bottom status bar,
;;; animated Hooded Crow startup banner, unified git diff viewer, interactive
;;; <todo-list> widget, and safe declarative Scheme response widgets.

(define-module (geasa surfaces-tui widgets)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa surfaces-tui editor)
               #:use-module (geasa surfaces-theme plugin)
               #:use-module (ice-9 format)
               #:use-module (ice-9 match)
               #:use-module (ice-9 regex)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (<rect>
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
                          format-input-box-frame
                          input-box-widget
                          format-bottom-status-bar
                          status-bar-widget
                          %crow-banner-frames
                          crow-banner-frame-count
                          crow-banner-frame
                          format-startup-banner
                          format-startup-banner-text
                          startup-banner-widget
                          parse-git-diff-lines
                          format-git-diff-view
                          git-diff-widget
                          <todo-item>
                          todo-item?
                          todo-item-id
                          todo-item-text
                          todo-item-done?
                          todo-item-source
                          todo-item-priority
                          <todo-list>
                          todo-list?
                          todo-list-title
                          todo-list-items
                          make-todo-list
                          todo-add
                          todo-toggle
                          todo-remove
                          todo-clear-done
                          todo-counts
                          todo-list->alist
                          alist->todo-list
                          format-todo-widget
                          todo-widget
                          safe-eval-widget-datum
                          extract-response-widgets
                          render-scheme-response-widget))

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

(define* (make-draw-op x y text
                       #:key (role 'default) (bold? #f) (dim? #f) (reverse? #f))
         "Create a positioned styled text draw operation."
         (%make-draw-op x y (ensure-string (or text "")) role bold? dim? reverse?))

(define (render-ops->lines ops width height)
        "Rasterize a list of <draw-op> records onto a plain-text HEIGHT x WIDTH grid
and return a list of HEIGHT strings (useful for headless tests and plain text)."
        (let* ((w (max 0 width))
               (h (max 0 height))
               (rows (make-vector h #f)))
          (let init ((i 0))
                    (when (< i h)
                      (vector-set! rows i (make-string w #\space))
                      (init (+ i 1))))
          (for-each
            (lambda (op)
              (let ((y (draw-op-y op))
                    (x (draw-op-x op))
                    (txt (draw-op-text op)))
                (when (and (>= y 0) (< y h) (< x w) (not (string-null? txt)))
                  (let* ((row-str (vector-ref rows y))
                         (start-col (max 0 x))
                         (skip (max 0 (- start-col x)))
                         (avail (max 0 (- w start-col)))
                         (src-len (max 0 (- (string-length txt) skip)))
                         (copy-len (min avail src-len)))
                    (when (> copy-len 0)
                      (let ((prefix (substring row-str 0 start-col))
                            (mid (substring txt skip (+ skip copy-len)))
                            (suffix (substring row-str (+ start-col copy-len) w)))
                        (vector-set! rows y (string-append prefix mid suffix))))))))
            ops)
          (vector->list rows)))

(define (draw-ops->segments-by-row ops)
        "Group <draw-op> records into an alist of (row . ((x text role bold? dim? reverse?) ...))
sorted by row and x coordinate."
        (let ((table (make-hash-table 31)))
          (for-each
            (lambda (op)
              (let ((y (draw-op-y op))
                    (seg (list (draw-op-x op)
                               (draw-op-text op)
                               (draw-op-role op)
                               (draw-op-bold? op)
                               (draw-op-dim? op)
                               (draw-op-reverse? op))))
                (hashv-set! table y (cons seg (or (hashv-ref table y) '())))))
            ops)
          (let ((rows (sort (hash-map->list cons table)
                            (lambda (a b) (< (car a) (car b))))))
            (map (lambda (entry)
                   (cons (car entry)
                         (sort (cdr entry)
                               (lambda (a b) (< (car a) (car b))))))
                 rows))))

;;;
;;; Core Composable Widget Protocol (ui-wp1.1)
;;;
(define-record-type <widget>
                    (%make-widget kind props children measure render on-event)
                    widget?
                    (kind widget-kind)
                    (props widget-props)
                    (children widget-children)
                    (measure widget-measure)
                    (render widget-render)
                    (on-event widget-on-event))

(define* (make-widget kind
                      #:key (props '())
                      (children '())
                      (measure (lambda (_w avail-w avail-h) (cons avail-w 1)))
                      (render (lambda (_w _rect) '()))
                      (on-event #f))
         "Create a composable <widget> with measure/render/event callbacks."
         (%make-widget kind props children measure render on-event))

(define (widget-measure-size w avail-width avail-height)
        "Compute desired (width . height) for widget W inside AVAIL-WIDTH x AVAIL-HEIGHT."
        (let* ((fn (widget-measure w))
               (res (fn w (max 0 avail-width) (max 0 avail-height))))
          (cons (max 0 (min avail-width (car res)))
                (max 0 (min avail-height (cdr res))))))

(define (widget-render-rect w rect)
        "Render widget W within RECT, returning a flat list of <draw-op> values."
        (if (or (<= (rect-width rect) 0) (<= (rect-height rect) 0))
          '()
          ((widget-render w) w rect)))

(define (widget-dispatch-event w event)
        "Dispatch EVENT to widget W's on-event handler or children. Returns result or #f."
        (let ((handler (widget-on-event w)))
          (or (and handler (handler w event))
              (any (lambda (child) (widget-dispatch-event child event))
                   (widget-children w)))))

(define (render-widget-to-lines w width height)
        "Measure and render widget W into a list of HEIGHT plain-text strings of WIDTH."
        (let ((ops (widget-render-rect w (make-rect 0 0 width height))))
          (render-ops->lines ops width height)))

;;;
;;; Reusable Widget Combinators
;;;
(define* (text-widget text
                      #:key (role 'default) (bold? #f) (dim? #f) (align 'left))
         "Leaf widget displaying wrapped or single-line TEXT with semantic ROLE."
         (let ((str (ensure-string (or text ""))))
           (make-widget
             'text
             #:props `((text . ,str) (role . ,role) (bold? . ,bold?)
                       (dim? . ,dim?) (align . ,align))
             #:measure
             (lambda (_w avail-w avail-h)
               (let* ((lines (wrap-text str (max 1 avail-w)))
                      (max-w (fold (lambda (l acc) (max acc (string-length l))) 0 lines)))
                 (cons (min avail-w max-w) (min avail-h (length lines)))))
             #:render
             (lambda (_w rect)
               (let* ((rw (rect-width rect))
                      (rh (rect-height rect))
                      (lines (take (wrap-text str (max 1 rw))
                                   (min rh (length (wrap-text str (max 1 rw)))))))
                 (let loop ((rest lines) (dy 0) (acc '()))
                           (if (or (null? rest) (>= dy rh))
                             (reverse acc)
                             (let* ((line (clip-str (car rest) rw))
                                    (len (string-length line))
                                    (dx (case align
                                          ((center) (max 0 (quotient (- rw len) 2)))
                                          ((right) (max 0 (- rw len)))
                                          (else 0))))
                               (loop (cdr rest)
                                     (+ dy 1)
                                     (cons (make-draw-op (+ (rect-x rect) dx)
                                                         (+ (rect-y rect) dy)
                                                         line
                                                         #:role role
                                                         #:bold? bold?
                                                         #:dim? dim?)
                                           acc))))))))))

(define* (rule-widget #:key (char #\─) (role 'dim) (label #f) (dim? #t))
         "Horizontal divider widget, optionally centered/prefixed with LABEL."
         (make-widget
           'rule
           #:props `((char . ,char) (role . ,role) (label . ,label) (dim? . ,dim?))
           #:measure (lambda (_w avail-w _avail-h) (cons avail-w 1))
           #:render
           (lambda (_w rect)
             (let* ((w (rect-width rect))
                    (x (rect-x rect))
                    (y (rect-y rect)))
               (if (and (string? label) (not (string-null? label)) (>= w 6))
                 (let* ((lbl (clip-str (string-append " " label " ") (max 0 (- w 2))))
                        (rem (max 0 (- w (string-length lbl))))
                        (left-n (min 2 rem))
                        (right-n (max 0 (- rem left-n))))
                   (list (make-draw-op x y
                                       (string-append (make-string left-n char)
                                                      lbl
                                                      (make-string right-n char))
                                       #:role role
                                       #:dim? dim?)))
                 (list (make-draw-op x y (make-string w char)
                                     #:role role #:dim? dim?)))))))

(define* (pill-widget text
                      #:key (role 'accent) (bold? #t) (prefix "[") (suffix "]"))
         "Compact inline status pill widget."
         (let ((rendered (string-append (ensure-string prefix)
                                        (ensure-string (or text ""))
                                        (ensure-string suffix))))
           (make-widget
             'pill
             #:props `((text . ,text) (role . ,role) (bold? . ,bold?))
             #:measure
             (lambda (_w avail-w _avail-h)
               (cons (min avail-w (string-length rendered)) 1))
             #:render
             (lambda (_w rect)
               (list (make-draw-op (rect-x rect)
                                   (rect-y rect)
                                   (clip-str rendered (rect-width rect))
                                   #:role role
                                   #:bold? bold?))))))

(define* (box-widget child
                     #:key (title #f) (role 'frame) (rounded? #t) (padding 0))
         "Container widget drawing a single-line border around CHILD."
         (let* ((pad (max 0 padding))
                (tl (if rounded? "╭" "┌"))
                (tr (if rounded? "╮" "┐"))
                (bl (if rounded? "╰" "└"))
                (br (if rounded? "╯" "┘")))
           (make-widget
             'box
             #:props `((title . ,title) (role . ,role) (rounded? . ,rounded?) (padding . ,pad))
             #:children (if child (list child) '())
             #:measure
             (lambda (_w avail-w avail-h)
               (let* ((overhead-w (+ 2 (* 2 pad)))
                      (overhead-h (+ 2 (* 2 pad)))
                      (inner-avail-w (max 0 (- avail-w overhead-w)))
                      (inner-avail-h (max 0 (- avail-h overhead-h)))
                      (child-sz (if child
                                  (widget-measure-size child inner-avail-w inner-avail-h)
                                  '(0 . 0)))
                      (title-w (if (and (string? title) (not (string-null? title)))
                                 (+ (string-length title) 4)
                                 0)))
                 (cons (min avail-w (max title-w (+ (car child-sz) overhead-w)))
                       (min avail-h (+ (cdr child-sz) overhead-h)))))
             #:render
             (lambda (_w rect)
               (let* ((x (rect-x rect))
                      (y (rect-y rect))
                      (w (rect-width rect))
                      (h (rect-height rect)))
                 (if (or (< w 2) (< h 2))
                   '()
                   (let* ((inner-w (- w 2))
                          (top-line
                            (if (and (string? title) (not (string-null? title)) (>= inner-w 3))
                              (let* ((tag (clip-str (string-append "─ " title " ") inner-w))
                                     (rem (max 0 (- inner-w (string-length tag)))))
                                (string-append tl tag (make-string rem #\─) tr))
                              (string-append tl (make-string inner-w #\─) tr)))
                          (bot-line (string-append bl (make-string inner-w #\─) br))
                          (side-ops
                            (let loop ((row 1) (acc '()))
                                      (if (>= row (- h 1))
                                        (reverse acc)
                                        (loop (+ row 1)
                                              (cons (make-draw-op (+ x w -1) (+ y row) "│" #:role role)
                                                    (cons (make-draw-op x (+ y row) "│" #:role role)
                                                          acc))))))
                          (child-rect (make-rect (+ x 1 pad)
                                                 (+ y 1 pad)
                                                 (max 0 (- inner-w (* 2 pad)))
                                                 (max 0 (- h 2 (* 2 pad)))))
                          (child-ops (if child (widget-render-rect child child-rect) '())))
                     (append (list (make-draw-op x y top-line #:role role))
                             side-ops
                             (list (make-draw-op x (+ y h -1) bot-line #:role role))
                             child-ops))))))))

(define* (scroll-view-widget lines
                             #:key (scroll-offset 0)
                             (role 'default)
                             (show-scrollbar? #t))
         "Scrollable line/segment viewport with optional right-edge scrollbar."
         (let* ((entries (or lines '()))
                (total (length entries)))
           (make-widget
             'scroll-view
             #:props `((scroll-offset . ,scroll-offset)
                       (total-lines . ,total)
                       (show-scrollbar? . ,show-scrollbar?))
             #:measure
             (lambda (_w avail-w avail-h)
               (cons avail-w (min avail-h (max 1 total))))
             #:render
             (lambda (_w rect)
               (let* ((x (rect-x rect))
                      (y (rect-y rect))
                      (w (rect-width rect))
                      (h (rect-height rect))
                      (bar? (and show-scrollbar? (> total h) (>= w 4)))
                      (content-w (if bar? (- w 1) w))
                      (max-scroll (max 0 (- total h)))
                      (clamped-scroll (max 0 (min scroll-offset max-scroll)))
                      (start (max 0 (- total h clamped-scroll)))
                      (vis (take (drop entries start) (min h (max 0 (- total start)))))
                      (text-ops
                        (let loop ((rest vis) (dy 0) (acc '()))
                                  (if (or (null? rest) (>= dy h))
                                    (reverse acc)
                                    (let ((item (car rest)))
                                      (cond
                                        ((string? item)
                                          (loop (cdr rest)
                                                (+ dy 1)
                                                (cons (make-draw-op x (+ y dy)
                                                                    (clip-str item content-w)
                                                                    #:role role)
                                                      acc)))
                                        ((and (pair? item) (every pair? item))
                                          ;; List of (text role bold? dim?) spans
                                          (let sloop ((spans item) (cx x) (sacc acc))
                                                     (if (or (null? spans) (>= cx (+ x content-w)))
                                                       (loop (cdr rest) (+ dy 1) sacc)
                                                       (let* ((sp (car spans))
                                                              (stxt (clip-str (car sp) (- (+ x content-w) cx)))
                                                              (srole (if (pair? (cdr sp)) (cadr sp) role))
                                                              (sbold? (and (>= (length sp) 3) (caddr sp)))
                                                              (sdim? (and (>= (length sp) 4) (cadddr sp))))
                                                         (sloop (cdr spans)
                                                                (+ cx (string-length stxt))
                                                                (if (string-null? stxt)
                                                                  sacc
                                                                  (cons (make-draw-op cx (+ y dy) stxt
                                                                                      #:role srole
                                                                                      #:bold? sbold?
                                                                                      #:dim? sdim?)
                                                                        sacc)))))))
                                        (else
                                          (loop (cdr rest)
                                                (+ dy 1)
                                                (cons (make-draw-op x (+ y dy)
                                                                    (clip-str (format #f "~a" item) content-w)
                                                                    #:role role)
                                                      acc))))))))
                      (bar-ops
                        (if (not bar?)
                          '()
                          (let* ((thumb-h (max 1 (inexact->exact (floor (* h (/ h total))))))
                                 (thumb-top
                                   (if (zero? max-scroll)
                                     0
                                     (inexact->exact
                                       (round (* (- h thumb-h) (/ start max-scroll)))))))
                            (let bloop ((dy 0) (acc '()))
                                       (if (>= dy h)
                                         (reverse acc)
                                         (let ((in-thumb? (and (>= dy thumb-top)
                                                               (< dy (+ thumb-top thumb-h)))))
                                           (bloop (+ dy 1)
                                                  (cons (make-draw-op (+ x w -1) (+ y dy)
                                                                      (if in-thumb? "┃" "│")
                                                                      #:role (if in-thumb? 'accent 'dim)
                                                                      #:dim? (not in-thumb?))
                                                        acc)))))))))
                 (append text-ops bar-ops))))))

(define* (vstack-widget children #:key (gap 0))
         "Vertical stack layout combinator."
         (let ((g (max 0 gap)))
           (make-widget
             'vstack
             #:props `((gap . ,g))
             #:children children
             #:measure
             (lambda (_w avail-w avail-h)
               (let loop ((rest children) (total-h 0) (max-w 0))
                         (if (null? rest)
                           (cons (min avail-w max-w) (min avail-h total-h))
                           (let* ((rem-h (max 0 (- avail-h total-h)))
                                  (sz (widget-measure-size (car rest) avail-w rem-h))
                                  (next-h (+ total-h (cdr sz) (if (null? (cdr rest)) 0 g))))
                             (loop (cdr rest) next-h (max max-w (car sz)))))))
             #:render
             (lambda (_w rect)
               (let ((x (rect-x rect))
                     (y (rect-y rect))
                     (w (rect-width rect))
                     (h (rect-height rect)))
                 (let loop ((rest children) (cy y) (acc '()))
                           (if (or (null? rest) (>= cy (+ y h)))
                             acc
                             (let* ((rem-h (- (+ y h) cy))
                                    (sz (widget-measure-size (car rest) w rem-h))
                                    (ch (min rem-h (cdr sz)))
                                    (ops (widget-render-rect (car rest) (make-rect x cy w ch))))
                               (loop (cdr rest) (+ cy ch g) (append acc ops))))))))))

(define* (hstack-widget children #:key (gap 1))
         "Horizontal stack layout combinator."
         (let ((g (max 0 gap)))
           (make-widget
             'hstack
             #:props `((gap . ,g))
             #:children children
             #:measure
             (lambda (_w avail-w avail-h)
               (let loop ((rest children) (total-w 0) (max-h 0))
                         (if (null? rest)
                           (cons (min avail-w total-w) (min avail-h max-h))
                           (let* ((rem-w (max 0 (- avail-w total-w)))
                                  (sz (widget-measure-size (car rest) rem-w avail-h))
                                  (next-w (+ total-w (car sz) (if (null? (cdr rest)) 0 g))))
                             (loop (cdr rest) next-w (max max-h (cdr sz)))))))
             #:render
             (lambda (_w rect)
               (let ((x (rect-x rect))
                     (y (rect-y rect))
                     (w (rect-width rect))
                     (h (rect-height rect)))
                 (let loop ((rest children) (cx x) (acc '()))
                           (if (or (null? rest) (>= cx (+ x w)))
                             acc
                             (let* ((rem-w (- (+ x w) cx))
                                    (sz (widget-measure-size (car rest) rem-w h))
                                    (cw (min rem-w (car sz)))
                                    (ops (widget-render-rect (car rest) (make-rect cx y cw h))))
                               (loop (cdr rest) (+ cx cw g) (append acc ops))))))))))

;;;
;;; Input Box Demarcation (ui-wp1.3)
;;;
(define (mode->badge-label mode)
        (case mode
          ((shell) "shell")
          ((scheme) "scheme")
          ((slash) "slash")
          ((steering) "steer")
          (else "chat")))

(define (effective-demarcation-style style maxx)
        "Select a valid input demarcation style; fall back from rounded-box to lines
on ultra-narrow widths (< 16 cols)."
        (cond
          ((not (memq style '(rounded-box lines tint))) 'rounded-box)
          ((and (eq? style 'rounded-box) (< maxx 16)) 'lines)
          (else style)))

(define* (format-input-box-frame input-lines maxx
                                 #:key (prompt-glyph "❯")
                                 (prompt-role 'accent)
                                 (prompt-mode 'chat)
                                 (style 'rounded-box)
                                 (focused? #t)
                                 (placeholder #f)
                                 (cursor-row 0)
                                 (cursor-col 0))
         "Format the prompt input box frame using psychological demarcation STYLE
('rounded-box, 'lines, or 'tint). Returns an alist with:
  rows             ((row-offset . ((x text role bold? dim? reverse?) ...)) ...)
  height           total rows occupied
  text-origin-x    screen column where input text starts
  text-origin-y    row offset where input line 0 starts
  cursor-screen-row / cursor-screen-col
  style            effective style symbol."
         (let* ((w (max 6 maxx))
                (eff-style (effective-demarcation-style style w))
                (border-role (if focused? prompt-role 'dim))
                (border-dim? (not focused?))
                (lines (if (or (not input-lines) (null? input-lines)) '("") input-lines))
                (empty-input? (and (= (length lines) 1) (string-null? (car lines))))
                (badge (format #f "~a ~a" prompt-glyph (mode->badge-label prompt-mode)))
                (safe-cur-row (max 0 (min cursor-row (- (length lines) 1))))
                (safe-cur-col (max 0 cursor-col)))
           (case eff-style
             ((rounded-box)
               (let* ((inner-w (max 1 (- w 4)))
                      (top-tag (clip-str (string-append "─ " badge " ") (- w 2)))
                      (top-rem (max 0 (- w 2 (string-length top-tag))))
                      (top-str (string-append "╭" top-tag (make-string top-rem #\─) "╮"))
                      (bot-str (string-append "╰" (make-string (max 0 (- w 2)) #\─) "╯"))
                      (body-rows
                        (let loop ((rest lines) (idx 0) (acc '()))
                                  (if (null? rest)
                                    (reverse acc)
                                    (let* ((line (car rest))
                                           (use-ph? (and empty-input? (zero? idx)
                                                         (string? placeholder)
                                                         (not (string-null? placeholder))))
                                           (shown (clip-str (if use-ph? placeholder line) inner-w))
                                           (pad-n (max 0 (- inner-w (string-length shown))))
                                           (row-y (+ idx 1))
                                           (segs
                                             (list (list 0 "│ " border-role #f border-dim? #f)
                                                   (list 2 shown
                                                         (if use-ph? 'dim 'default)
                                                         #f
                                                         use-ph?
                                                         #f)
                                                   (list (+ 2 (string-length shown))
                                                         (string-append (make-string pad-n #\space) " │")
                                                         border-role
                                                         #f
                                                         border-dim?
                                                         #f))))
                                      (loop (cdr rest) (+ idx 1) (cons (cons row-y segs) acc))))))
                      (total-h (+ 2 (length lines))))
                 `((style . rounded-box)
                   (height . ,total-h)
                   (text-origin-x . 2)
                   (text-origin-y . 1)
                   (cursor-screen-row . ,(+ 1 safe-cur-row))
                   (cursor-screen-col . ,(min (+ 2 inner-w) (+ 2 safe-cur-col)))
                   (rows . ,(append (list (cons 0 (list (list 0 top-str border-role #f border-dim? #f))))
                                    body-rows
                                    (list (cons (- total-h 1)
                                                (list (list 0 bot-str border-role #f border-dim? #f)))))))))
             ((lines)
               (let* ((inner-w (max 1 (- w 3)))
                      (top-tag (clip-str (string-append "─ " (mode->badge-label prompt-mode) " ") w))
                      (top-rem (max 0 (- w (string-length top-tag))))
                      (top-str (string-append top-tag (make-string top-rem #\─)))
                      (bot-str (make-string w #\─))
                      (body-rows
                        (let loop ((rest lines) (idx 0) (acc '()))
                                  (if (null? rest)
                                    (reverse acc)
                                    (let* ((line (car rest))
                                           (use-ph? (and empty-input? (zero? idx)
                                                         (string? placeholder)
                                                         (not (string-null? placeholder))))
                                           (shown (clip-str (if use-ph? placeholder line) inner-w))
                                           (lead (if (zero? idx) prompt-glyph "┃"))
                                           (row-y (+ idx 1))
                                           (segs
                                             (list (list 0 lead prompt-role (zero? idx) #f #f)
                                                   (list 1 " " 'default #f #f #f)
                                                   (list 2 shown
                                                         (if use-ph? 'dim 'default)
                                                         #f
                                                         use-ph?
                                                         #f))))
                                      (loop (cdr rest) (+ idx 1) (cons (cons row-y segs) acc))))))
                      (total-h (+ 2 (length lines))))
                 `((style . lines)
                   (height . ,total-h)
                   (text-origin-x . 2)
                   (text-origin-y . 1)
                   (cursor-screen-row . ,(+ 1 safe-cur-row))
                   (cursor-screen-col . ,(min (+ 2 inner-w) (+ 2 safe-cur-col)))
                   (rows . ,(append (list (cons 0 (list (list 0 top-str border-role #f #t #f))))
                                    body-rows
                                    (list (cons (- total-h 1)
                                                (list (list 0 bot-str border-role #f #t #f)))))))))
             ((tint)
               (let* ((inner-w (max 1 (- w 3)))
                      (body-rows
                        (let loop ((rest lines) (idx 0) (acc '()))
                                  (if (null? rest)
                                    (reverse acc)
                                    (let* ((line (car rest))
                                           (use-ph? (and empty-input? (zero? idx)
                                                         (string? placeholder)
                                                         (not (string-null? placeholder))))
                                           (shown (pad-str (if use-ph? placeholder line) inner-w))
                                           (lead (if (zero? idx) (string-append "▌" prompt-glyph " ") "▌  "))
                                           (segs
                                             (list (list 0 lead prompt-role #t #f #f)
                                                   (list 3 shown 'selection #f use-ph? #f))))
                                      (loop (cdr rest) (+ idx 1) (cons (cons idx segs) acc))))))
                      (total-h (length lines)))
                 `((style . tint)
                   (height . ,total-h)
                   (text-origin-x . 3)
                   (text-origin-y . 0)
                   (cursor-screen-row . ,safe-cur-row)
                   (cursor-screen-col . ,(min (+ 3 inner-w) (+ 3 safe-cur-col)))
                   (rows . ,body-rows)))))))

(define* (input-box-widget input-lines
                           #:key (prompt-glyph "❯")
                           (prompt-role 'accent)
                           (prompt-mode 'chat)
                           (style 'rounded-box)
                           (focused? #t)
                           (placeholder #f)
                           (cursor-row 0)
                           (cursor-col 0))
         "Wrap format-input-box-frame as a composable <widget>."
         (make-widget
           'input-box
           #:props `((style . ,style) (prompt-mode . ,prompt-mode) (focused? . ,focused?))
           #:measure
           (lambda (_w avail-w avail-h)
             (let* ((frame (format-input-box-frame input-lines avail-w
                                                   #:prompt-glyph prompt-glyph
                                                   #:prompt-role prompt-role
                                                   #:prompt-mode prompt-mode
                                                   #:style style
                                                   #:focused? focused?
                                                   #:placeholder placeholder
                                                   #:cursor-row cursor-row
                                                   #:cursor-col cursor-col))
                    (h (assoc-ref frame 'height)))
               (cons avail-w (min avail-h h))))
           #:render
           (lambda (_w rect)
             (let* ((frame (format-input-box-frame input-lines (rect-width rect)
                                                   #:prompt-glyph prompt-glyph
                                                   #:prompt-role prompt-role
                                                   #:prompt-mode prompt-mode
                                                   #:style style
                                                   #:focused? focused?
                                                   #:placeholder placeholder
                                                   #:cursor-row cursor-row
                                                   #:cursor-col cursor-col))
                    (rows (assoc-ref frame 'rows))
                    (rx (rect-x rect))
                    (ry (rect-y rect))
                    (rh (rect-height rect)))
               (append-map
                 (lambda (row-pair)
                   (let ((dy (car row-pair))
                         (segs (cdr row-pair)))
                     (if (>= dy rh)
                       '()
                       (map (lambda (seg)
                              (make-draw-op (+ rx (car seg))
                                            (+ ry dy)
                                            (cadr seg)
                                            #:role (caddr seg)
                                            #:bold? (cadddr seg)
                                            #:dim? (list-ref seg 4)
                                            #:reverse? (list-ref seg 5)))
                            segs))))
                 rows)))))

;;;
;;; Bottom Status Bar & Animated Hooded Crow Startup Banner (ui-wp1.4)
;;;
(define (shorten-cwd cwd max-len)
        (let ((s (ensure-string (or cwd ""))))
          (cond
            ((<= (string-length s) max-len) s)
            ((<= max-len 3) (clip-str s max-len))
            (else
              (let* ((parts (filter (lambda (p) (not (string-null? p))) (string-split s #\/)))
                     (tail (if (>= (length parts) 2)
                             (string-append "…/" (cadr (reverse parts)) "/" (car (reverse parts)))
                             (string-append "…" (substring s (- (string-length s) (- max-len 1)))))))
                (if (<= (string-length tail) max-len)
                  tail
                  (string-append "…" (substring s (- (string-length s) (- max-len 1))))))))))

(define (mode->status-pill mode)
        (case mode
          ((shell) (cons "SHELL" 'shell))
          ((scheme) (cons "SCHEME" 'scheme))
          ((slash) (cons "SLASH" 'warning))
          ((steering) (cons "STEER" 'warning))
          ((menu) (cons "MENU" 'accent))
          ((selector) (cons "CMDS" 'accent))
          (else (cons "CHAT" 'accent))))

(define (status-word->role status)
        (let ((s (ensure-string (or status ""))))
          (cond
            ((string=? s "READY") 'success)
            ((or (string-prefix? "ERR" s) (string-prefix? "FAIL" s)) 'error)
            ((or (string-prefix? "REACT" s)
                 (string-prefix? "TOOL" s)
                 (string-prefix? "STREAM" s)
                 (string-prefix? "EXEC" s)
                 (string-prefix? "EVAL" s))
              'accent)
            (else 'status))))

(define* (format-bottom-status-bar maxx
                                   #:key (mode 'chat)
                                   (model "")
                                   (status "READY")
                                   (cwd "")
                                   (git-branch #f)
                                   (tokens-used 0)
                                   (tokens-limit 0)
                                   (keymap-hint #f)
                                   (subagent-count 0)
                                   (todo-summary #f)
                                   (feat-count 0)
                                   (journal-count 0)
                                   (mcp-info #f))
         "Build a single-line bottom status bar as positioned segments
((x text role bold? dim?) ...) fitting within MAXX columns. Drops lower-priority
middle items gracefully on narrow terminals."
         (let* ((w (max 1 maxx))
                (mode-pair (mode->status-pill mode))
                (mode-text (format #f " ~a " (car mode-pair)))
                (mode-role (cdr mode-pair))
                (left-base (list (list mode-text mode-role #t #f)))
                (left-opt
                  (filter-map
                    identity
                    (list
                      (and (string? model) (not (string-null? model))
                           (list (clip-str model 24) 'default #f #f))
                      (and (string? git-branch) (not (string-null? git-branch))
                           (list (format #f "⎇ ~a" (clip-str git-branch 16)) 'accent #f #f))
                      (and (string? cwd) (not (string-null? cwd))
                           (list (shorten-cwd cwd 22) 'dim #f #t))
                      (and (string? keymap-hint) (not (string-null? keymap-hint))
                           (list keymap-hint 'dim #f #t)))))
                (tok-str
                  (cond
                    ((and (> tokens-limit 0) (>= tokens-used 0))
                      (let ((pct (min 100 (inexact->exact (round (* 100 (/ tokens-used tokens-limit)))))))
                        (format #f "ctx ~a/~a (~a%)"
                                (format-token-kilo tokens-used)
                                (format-token-kilo tokens-limit)
                                pct)))
                    ((> tokens-used 0)
                      (format #f "~a tok" (format-token-kilo tokens-used)))
                    (else #f)))
                (right-items
                  (filter-map
                    identity
                    (list
                      (and (string? todo-summary) (not (string-null? todo-summary))
                           (list todo-summary 'status #f #f))
                      (and (> subagent-count 0)
                           (list (format #f "⑂ ~a" subagent-count) 'accent #f #f))
                      (and (string? mcp-info) (not (string-null? mcp-info))
                           (list (format #f "mcp ~a" mcp-info) 'dim #f #t))
                      (and (> feat-count 0)
                           (list (format #f "~a feats" feat-count) 'dim #f #t))
                      (and (> journal-count 0)
                           (list (format #f "~a chkpts" journal-count) 'dim #f #t))
                      (and tok-str
                           (list tok-str 'dim #f #t))
                      (and (string? status) (not (string-null? status))
                           (list status (status-word->role status) #t #f)))))
                (items-width
                  (lambda (items sep-w)
                    (if (null? items)
                      0
                      (+ (* sep-w (- (length items) 1))
                         (fold (lambda (it acc) (+ acc (string-length (car it)))) 0 items)))))
                (place-left
                  (lambda (items)
                    (let loop ((rest items) (x 0) (acc '()))
                              (if (null? rest)
                                (reverse acc)
                                (let ((it (car rest)))
                                  (loop (cdr rest)
                                        (+ x (string-length (car it)) 1)
                                        (cons (cons x it) acc)))))))
                (place-right
                  (lambda (items)
                    (let* ((rw (items-width items 2))
                           (start-x (max 0 (- w rw))))
                      (let loop ((rest items) (x start-x) (acc '()))
                                (if (null? rest)
                                  (reverse acc)
                                  (let ((it (car rest)))
                                    (loop (cdr rest)
                                          (+ x (string-length (car it)) 2)
                                          (cons (cons x it) acc)))))))))
           (let fit ((l-opts left-opt) (r-items right-items))
                    (let* ((l-all (append left-base l-opts))
                           (lw (items-width l-all 1))
                           (rw (items-width r-items 2)))
                      (cond
                        ((<= (+ lw (if (null? r-items) 0 2) rw) w)
                          (append (place-left l-all) (place-right r-items)))
                        ((pair? l-opts)
                          (fit (drop-right l-opts 1) r-items))
                        ((> (length r-items) 1)
                          (fit '() (cdr r-items)))
                        (else
                          (let ((clipped-mode (clip-str mode-text w)))
                            (list (list 0 clipped-mode mode-role #t #f)))))))))

(define* (status-bar-widget #:key (mode 'chat)
                            (model "")
                            (status "READY")
                            (cwd "")
                            (git-branch #f)
                            (tokens-used 0)
                            (tokens-limit 0)
                            (keymap-hint #f)
                            (subagent-count 0)
                            (todo-summary #f))
         "Composable <widget> wrapping format-bottom-status-bar."
         (make-widget
           'status-bar
           #:props `((mode . ,mode) (model . ,model) (status . ,status))
           #:measure (lambda (_w avail-w _avail-h) (cons avail-w 1))
           #:render
           (lambda (_w rect)
             (let ((segs (format-bottom-status-bar (rect-width rect)
                                                   #:mode mode
                                                   #:model model
                                                   #:status status
                                                   #:cwd cwd
                                                   #:git-branch git-branch
                                                   #:tokens-used tokens-used
                                                   #:tokens-limit tokens-limit
                                                   #:keymap-hint keymap-hint
                                                   #:subagent-count subagent-count
                                                   #:todo-summary todo-summary)))
               (map (lambda (seg)
                      (make-draw-op (+ (rect-x rect) (car seg))
                                    (rect-y rect)
                                    (cadr seg)
                                    #:role (caddr seg)
                                    #:bold? (cadddr seg)
                                    #:dim? (list-ref seg 4)))
                    segs)))))

;; Animated Hooded Crow (Corvus cornix — Badb) frames.
;; Each frame is a list of lines; each line is a list of (text role bold? dim?) spans
;; capturing the dark hood/wings vs pale ash-grey hooded-crow mantle.
(define %crow-banner-frames
        (vector
          ;; Frame 0
          (list
            (list (list "     " 'default #f #f)
                  (list "⣠⣴⣶⣶⣶⣦⣄" 'dim #f #t)
                  (list "      " 'default #f #f))
            (list (list "⠠⢴⣶⣶⣾⣿⣯" 'dim #f #t)
                  (list "⠂" 'default #t #f)
                  (list "⣿⣿⣿⣿⣇" 'dim #f #t)
                  (list "     " 'default #f #f))
            (list (list "   " 'default #f #f)
                  (list "⣨⣿⣿" 'default #f #f)
                  (list "⣿⣿⣿⣿⣿⣿" 'dim #f #t)
                  (list "⣿⣷⡄" 'default #f #f)
                  (list "   " 'default #f #f))
            (list (list " " 'default #f #f)
                  (list "⣼" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷" 'default #f #f)
                  (list "⡄" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list "⢸⣿⣿" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿" 'default #f #f)
                  (list "⣿⣿" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list " " 'default #f #f)
                  (list "⢻⣿⣿" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿" 'default #f #f)
                  (list "⡿⠃" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list "  " 'default #f #f)
                  (list "⢻⣿⣿⡿⠛⢻⠛⠛⡟⠛⠋" 'dim #f #t)
                  (list "     " 'default #f #f))
            (list (list "⢀⣴⣿⡿⠋" 'dim #f #t)
                  (list "⣀⣤" 'frame #f #f)
                  (list "⣾⣄⣴⣧" 'dim #f #t)
                  (list "⣤⣀" 'frame #f #f)
                  (list "     " 'default #f #f))
            (list (list "⠿⠿⠋" 'dim #f #t) (list "               " 'default #f #f)))
          ;; Frame 1
          (list
            (list (list "    " 'default #f #f)
                  (list "⢀⣤⣶⣶⣶⣶⣤⡀" 'dim #f #t)
                  (list "      " 'default #f #f))
            (list (list "    " 'default #f #f)
                  (list "⣾⣿⣿⣿⣿⣿⣿⣷⣶⠤" 'dim #f #t)
                  (list "    " 'default #f #f))
            (list (list "  " 'default #f #f)
                  (list "⣴⣿⣿" 'default #f #f)
                  (list "⣿⣿⣿⣿⣿⣿" 'dim #f #t)
                  (list "⣿⣧⡀" 'default #f #f)
                  (list "    " 'default #f #f))
            (list (list " " 'default #f #f)
                  (list "⣼" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷" 'default #f #f)
                  (list "⡄" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list "⢸⣿⣿" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿" 'default #f #f)
                  (list "⣿⣿" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list " " 'default #f #f)
                  (list "⢻⣿⣿" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿" 'default #f #f)
                  (list "⡿⠃" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list "  " 'default #f #f)
                  (list "⢻⣿⣿⡿⠛⢻⠛⠛⡟⠛⠋" 'dim #f #t)
                  (list "     " 'default #f #f))
            (list (list "⢀⣴⣿⡿⠋" 'dim #f #t)
                  (list "⣀⣤" 'frame #f #f)
                  (list "⣾⣄⣴⣧" 'dim #f #t)
                  (list "⣤⣀" 'frame #f #f)
                  (list "     " 'default #f #f))
            (list (list "⠿⠿⠋" 'dim #f #t) (list "               " 'default #f #f)))
          ;; Frame 2
          (list
            (list (list "   " 'default #f #f)
                  (list "⣠⣶⣶⣶⣶⣤⣠⣴⣶⣿⣿⠿⠂" 'dim #f #t)
                  (list "  " 'default #f #f))
            (list (list "   " 'default #f #f)
                  (list "⣿⣿⣿" 'dim #f #t)
                  (list "⠈" 'default #t #f)
                  (list "⣿⣿⣿⣿⠟⠉" 'dim #f #t)
                  (list "     " 'default #f #f))
            (list (list " " 'default #f #f)
                  (list "⢀⣾⣿⣿⣿" 'default #f #f)
                  (list "⣿⣿⣿⣿" 'dim #f #t)
                  (list "⣿⣿⣦⡀" 'default #f #f)
                  (list "    " 'default #f #f))
            (list (list "⢀⣾" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿" 'default #f #f)
                  (list "⣿⣆" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list "⠸⣿⣿" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿" 'default #f #f)
                  (list "⣿⡿" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list " " 'default #f #f)
                  (list "⠹⣿⣿" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿" 'default #f #f)
                  (list "⣿⠟⠁" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list "  " 'default #f #f)
                  (list "⣹⣿⣿⠟⠉⢹⠉⠉⡏⠉⠁" 'dim #f #t)
                  (list "     " 'default #f #f))
            (list (list "⢠⣾⣿⠟⠁" 'dim #f #t)
                  (list "⠤⠶" 'frame #f #f)
                  (list "⠿⠦⠾⠷" 'dim #f #t)
                  (list "⠶⠤" 'frame #f #f)
                  (list "     " 'default #f #f))
            (list (list "⠚⠛⠁" 'dim #f #t) (list "               " 'default #f #f)))
          ;; Frame 3
          (list
            (list (list "    " 'default #f #f)
                  (list "⢀⣤⣶⣶⣶⣶⣤⡀" 'dim #f #t)
                  (list "      " 'default #f #f))
            (list (list "   " 'default #f #f)
                  (list "⢀⣿⣿⣿⣿⣿⣿⣿⣿⣶⣶⣶⠤" 'dim #f #t)
                  (list "  " 'default #f #f))
            (list (list "  " 'default #f #f)
                  (list "⣴⣿⣿" 'default #f #f)
                  (list "⣿⣿⣿⣿⣿⣿" 'dim #f #t)
                  (list "⣿⣯⡀" 'default #f #f)
                  (list "    " 'default #f #f))
            (list (list " " 'default #f #f)
                  (list "⣼" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷" 'default #f #f)
                  (list "⡄" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list "⢸⣿⣿" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿" 'default #f #f)
                  (list "⣿⣿" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list " " 'default #f #f)
                  (list "⢻⣿⣿" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿" 'default #f #f)
                  (list "⡿⠃" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list "  " 'default #f #f)
                  (list "⢻⣿⣿⡿⠛⢻⠛⠛⡟⠛⠋" 'dim #f #t)
                  (list "     " 'default #f #f))
            (list (list "⢀⣴⣿⡿⠋" 'dim #f #t)
                  (list "⣀⣤" 'frame #f #f)
                  (list "⣾⣄⣴⣧" 'dim #f #t)
                  (list "⣤⣀" 'frame #f #f)
                  (list "     " 'default #f #f))
            (list (list "⠿⠿⠋" 'dim #f #t) (list "               " 'default #f #f)))
          ;; Frame 4
          (list
            (list (list "    " 'default #f #f)
                  (list "⢀⣤⣶⣶⣶⣶⣤⡀" 'dim #f #t)
                  (list "      " 'default #f #f))
            (list (list "   " 'default #f #f)
                  (list "⢀⣿⣿⣿⣿" 'dim #f #t)
                  (list "⠐" 'default #t #f)
                  (list "⣿⣿⣿⣶⣶⣶⠤" 'dim #f #t)
                  (list "  " 'default #f #f))
            (list (list "  " 'default #f #f)
                  (list "⣴⣿⣿" 'default #f #f)
                  (list "⣿⣿⣿⣿⣿⣿" 'dim #f #t)
                  (list "⣿⣯⡀" 'default #f #f)
                  (list "    " 'default #f #f))
            (list (list " " 'default #f #f)
                  (list "⣼" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷" 'default #f #f)
                  (list "⡄" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list "⢸⣿⣿" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿" 'default #f #f)
                  (list "⣿⣿" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list " " 'default #f #f)
                  (list "⢻⣿⣿" 'dim #f #t)
                  (list "⣿⣿⣿⣿⣿⣿⣿⣿⣿" 'default #f #f)
                  (list "⡿⠃" 'dim #f #t)
                  (list "   " 'default #f #f))
            (list (list "  " 'default #f #f)
                  (list "⢻⣿⣿⡿⠛⢻⠛⠛⡟⠛⠋" 'dim #f #t)
                  (list "     " 'default #f #f))
            (list (list "⢀⣴⣿⡿⠋" 'dim #f #t)
                  (list "⣀⣤" 'frame #f #f)
                  (list "⣾⣄⣴⣧" 'dim #f #t)
                  (list "⣤⣀" 'frame #f #f)
                  (list "     " 'default #f #f))
            (list (list "⠿⠿⠋" 'dim #f #t) (list "               " 'default #f #f)))))

(define (crow-banner-frame-count)
        "Return the number of animation frames in %crow-banner-frames."
        (vector-length %crow-banner-frames))

(define (crow-banner-frame idx)
        "Return the Hooded Crow art frame at IDX (wrapped modulo frame count)."
        (vector-ref %crow-banner-frames
                    (modulo (max 0 idx) (vector-length %crow-banner-frames))))

(define (spans-plain-length spans)
        (fold (lambda (sp acc) (+ acc (string-length (car sp)))) 0 spans))

(define (clip-spans spans max-len)
        (let loop ((rest spans) (rem (max 0 max-len)) (acc '()))
                  (if (or (null? rest) (<= rem 0))
                    (reverse acc)
                    (let* ((sp (car rest))
                           (txt (clip-str (car sp) rem))
                           (len (string-length txt)))
                      (if (zero? len)
                        (reverse acc)
                        (loop (cdr rest)
                              (- rem len)
                              (cons (cons txt (cdr sp)) acc)))))))

(define*
  (format-startup-banner maxx
                         #:key
                         (frame 0)
                         (version "0.1.0")
                         (model #f)
                         (workspace #f)
                         (tips '()))
  "Format the animated Hooded Crow startup banner as a list of styled lines,
where each line is a list of (text role bold? dim?) spans fitting within MAXX."
  (let*
      ((w (max 12 maxx))
       (art (crow-banner-frame frame))
       (art-w-max
         (fold
           (lambda (line acc)
             (max acc (spans-plain-length line)))
           0
           art))
       (art-col-w (+ art-w-max 2))
       (info-w (max 12 (- w art-col-w)))
       (default-tips
         (if (null? tips)
           (list "! shell · ( scheme · / commands · /help"
                 "ctrl+r history · @ files · ctrl+x $EDITOR")
           tips))
       (info-lines
         (filter-map identity
                     (list (list (list "" 'default #f #f))
                           (list (list "badb" 'accent #t #f)
                                 (list (format #f " v~a" (ensure-string (or version "0.1.0"))) 'dim #f #t)
                                 (list " · badb microharness" 'default #f #f))
                           (list (list (make-string (min 44 info-w) #\─) 'dim #f #t))
                           (and (string? model)
                                (not (string-null? model))
                                (list (list "model  " 'dim #f #t)
                                      (list (clip-str model (max 8 (- info-w 7))) 'status #t #f)))
                           (and (string? workspace)
                                (not (string-null? workspace))
                                (list (list "cwd    " 'dim #f #t)
                                      (list (shorten-cwd workspace (max 8 (- info-w 7))) 'default #f #f)))
                           (list (list "" 'default #f #f))
                           (list (list (clip-str (car default-tips) info-w) 'dim #f #t))
                           (and (pair? (cdr default-tips))
                                (list (list (clip-str (cadr default-tips) info-w) 'dim #f #t)))
                           (and (pair? (cdr default-tips))
                                (pair? (cddr default-tips))
                                (list (list (clip-str (caddr default-tips) info-w) 'accent #f #f))))))
       (compact? (< w 52))
       (row-count (max (length art) (length info-lines))))
    (if compact?
      (map
        (lambda (line)
          (clip-spans line w))
        (filter
          (lambda (ln)
            (not (and (pair? ln) (null? (cdr ln)) (string-null? (caar ln)))))
          info-lines))
      (let loop
        ((i 0) (acc '()))
        (if (>= i row-count)
          (reverse acc)
          (let*
              ((art-spans
                 (if (< i (length art))
                   (list-ref art i)
                   '()))
               (info-spans
                 (if (< i (length info-lines))
                   (list-ref info-lines i)
                   '()))
               (cur-w (spans-plain-length art-spans))
               (gap-spans
                 (list (list (make-string (max 1 (- art-col-w cur-w)) #\space) 'default #f #f))))
            (loop (+ i 1) (cons (clip-spans (append art-spans gap-spans info-spans) w) acc))))))))

(define* (format-startup-banner-text maxx
                                     #:key (frame 0)
                                     (version "0.1.0")
                                     (model #f)
                                     (workspace #f)
                                     (tips '()))
         "Render format-startup-banner into a plain multi-line string."
         (string-join
           (map (lambda (spans) (string-concatenate (map car spans)))
                (format-startup-banner maxx
                                       #:frame frame
                                       #:version version
                                       #:model model
                                       #:workspace workspace
                                       #:tips tips))
           "\n"))

(define* (startup-banner-widget #:key (frame 0)
                                (version "0.1.0")
                                (model #f)
                                (workspace #f)
                                (tips '()))
         "Composable <widget> rendering the animated Hooded Crow startup banner."
         (make-widget
           'startup-banner
           #:props `((frame . ,frame) (version . ,version) (model . ,model))
           #:measure
           (lambda (_w avail-w avail-h)
             (let ((rows (format-startup-banner avail-w
                                                #:frame frame
                                                #:version version
                                                #:model model
                                                #:workspace workspace
                                                #:tips tips)))
               (cons avail-w (min avail-h (length rows)))))
           #:render
           (lambda (_w rect)
             (let ((rows (format-startup-banner (rect-width rect)
                                                #:frame frame
                                                #:version version
                                                #:model model
                                                #:workspace workspace
                                                #:tips tips))
                   (rx (rect-x rect))
                   (ry (rect-y rect))
                   (rh (rect-height rect)))
               (let loop ((rest rows) (dy 0) (acc '()))
                         (if (or (null? rest) (>= dy rh))
                           (reverse acc)
                           (let sloop ((spans (car rest)) (cx rx) (sacc acc))
                                      (if (null? spans)
                                        (loop (cdr rest) (+ dy 1) sacc)
                                        (let* ((sp (car spans))
                                               (txt (car sp)))
                                          (sloop (cdr spans)
                                                 (+ cx (string-length txt))
                                                 (cons (make-draw-op cx (+ ry dy) txt
                                                                     #:role (cadr sp)
                                                                     #:bold? (caddr sp)
                                                                     #:dim? (cadddr sp))
                                                       sacc)))))))))))

;;;
;;; Git Diff Viewer (ui-wp1.5)
;;;
(define %hunk-header-rx
        (make-regexp "^@@ -([0-9]+)(,[0-9]+)? \\+([0-9]+)(,[0-9]+)? @@"))

(define (parse-git-diff-lines diff-text)
        "Parse unified DIFF-TEXT into structured entries:
((kind old-line new-line text) ...) where kind is 'file, 'meta, 'hunk, 'add, 'del, or 'ctx."
        (let ((raw-lines (string-split (ensure-string (or diff-text "")) #\newline)))
          (let loop ((rest raw-lines) (old-ln 0) (new-ln 0) (acc '()))
                    (if (null? rest)
                      (reverse acc)
                      (let ((line (car rest)))
                        (cond
                          ((or (string-prefix? "diff --git " line)
                               (string-prefix? "--- " line)
                               (string-prefix? "+++ " line))
                            (loop (cdr rest) old-ln new-ln
                                  (cons (list 'file #f #f line) acc)))
                          ((or (string-prefix? "index " line)
                               (string-prefix? "new file mode" line)
                               (string-prefix? "deleted file mode" line)
                               (string-prefix? "\\ No newline" line))
                            (loop (cdr rest) old-ln new-ln
                                  (cons (list 'meta #f #f line) acc)))
                          ((regexp-exec %hunk-header-rx line)
                            => (lambda (m)
                                 (let ((o (string->number (match:substring m 1)))
                                       (n (string->number (match:substring m 3))))
                                   (loop (cdr rest) o n
                                         (cons (list 'hunk #f #f line) acc)))))
                          ((string-prefix? "+" line)
                            (loop (cdr rest) old-ln (+ new-ln 1)
                                  (cons (list 'add #f new-ln (substring line 1)) acc)))
                          ((string-prefix? "-" line)
                            (loop (cdr rest) (+ old-ln 1) new-ln
                                  (cons (list 'del old-ln #f (substring line 1)) acc)))
                          ((string-prefix? " " line)
                            (loop (cdr rest) (+ old-ln 1) (+ new-ln 1)
                                  (cons (list 'ctx old-ln new-ln (substring line 1)) acc)))
                          ((string-null? line)
                            (loop (cdr rest) old-ln new-ln acc))
                          (else
                            (loop (cdr rest) old-ln new-ln
                                  (cons (list 'meta #f #f line) acc)))))))))

(define (format-diff-ln n)
        (if (integer? n)
          (format #f "~4d" n)
          "    "))

(define* (format-git-diff-view diff-text maxx
                               #:key (title #f)
                               (show-line-numbers? #t)
                               (max-lines #f))
         "Format unified DIFF-TEXT into styled rows (list of span lists
((text role bold? dim?) ...)) ready for rendering or scroll-view-widget."
         (let* ((w (max 12 maxx))
                (parsed (parse-git-diff-lines diff-text))
                (limited (if (and (integer? max-lines) (> (length parsed) max-lines))
                           (append (take parsed (max 0 (- max-lines 1)))
                                   (list (list 'meta #f #f
                                               (format #f "... (~a more diff lines)"
                                                       (- (length parsed) (max 0 (- max-lines 1)))))))
                           parsed))
                (header-row
                  (if (and (string? title) (not (string-null? title)))
                    (list (list (list (clip-str (format #f "diff · ~a" title) w) 'header #t #f)))
                    '()))
                (body-rows
                  (map (lambda (entry)
                         (let ((kind (car entry))
                               (o (cadr entry))
                               (n (caddr entry))
                               (txt (cadddr entry)))
                           (case kind
                             ((file)
                               (list (list (clip-str txt w) 'header #t #f)))
                             ((hunk)
                               (list (list (clip-str txt w) 'accent #t #f)))
                             ((meta)
                               (list (list (clip-str txt w) 'dim #f #t)))
                             ((add)
                               (if show-line-numbers?
                                 (let ((prefix (format #f "     ~a + " (format-diff-ln n))))
                                   (clip-spans
                                     (list (list prefix 'success #t #f)
                                           (list txt 'success #f #f))
                                     w))
                                 (clip-spans (list (list "+ " 'success #t #f)
                                                   (list txt 'success #f #f))
                                             w)))
                             ((del)
                               (if show-line-numbers?
                                 (let ((prefix (format #f "~a      - " (format-diff-ln o))))
                                   (clip-spans
                                     (list (list prefix 'error #t #f)
                                           (list txt 'error #f #f))
                                     w))
                                 (clip-spans (list (list "- " 'error #t #f)
                                                   (list txt 'error #f #f))
                                             w)))
                             (else
                               (if show-line-numbers?
                                 (let ((prefix (format #f "~a ~a   "
                                                       (format-diff-ln o)
                                                       (format-diff-ln n))))
                                   (clip-spans
                                     (list (list prefix 'dim #f #t)
                                           (list txt 'default #f #f))
                                     w))
                                 (clip-spans (list (list "  " 'dim #f #t)
                                                   (list txt 'default #f #f))
                                             w))))))
                       limited)))
           (append header-row body-rows)))

(define* (git-diff-widget diff-text
                          #:key (title #f)
                          (show-line-numbers? #t)
                          (scroll-offset 0)
                          (show-scrollbar? #t))
         "Composable scrollable widget rendering a unified git diff."
         (make-widget
           'git-diff
           #:props `((title . ,title) (show-line-numbers? . ,show-line-numbers?))
           #:measure
           (lambda (_w avail-w avail-h)
             (let ((rows (format-git-diff-view diff-text avail-w
                                               #:title title
                                               #:show-line-numbers? show-line-numbers?)))
               (cons avail-w (min avail-h (max 1 (length rows))))))
           #:render
           (lambda (_w rect)
             (let* ((rows (format-git-diff-view diff-text (rect-width rect)
                                                #:title title
                                                #:show-line-numbers? show-line-numbers?))
                    (sv (scroll-view-widget rows
                                            #:scroll-offset scroll-offset
                                            #:show-scrollbar? show-scrollbar?)))
               (widget-render-rect sv rect)))))

;;;
;;; Interactive Todo List Widget (ui-wp1.5)
;;;
(define-record-type <todo-item>
                    (%make-todo-item id text done? source priority)
                    todo-item?
                    (id todo-item-id)
                    (text todo-item-text)
                    (done? todo-item-done?)
                    (source todo-item-source)
                    (priority todo-item-priority))

(define-record-type <todo-list>
                    (%make-todo-list title items next-id)
                    todo-list?
                    (title todo-list-title)
                    (items todo-list-items)
                    (next-id todo-list-next-id))

(define* (make-todo-list #:key (title "Tasks") (items '()))
         "Create an immutable <todo-list> record with optional initial items."
         (let loop ((rest items) (nid 1) (acc '()))
                   (if (null? rest)
                     (%make-todo-list (ensure-string (or title "Tasks")) (reverse acc) nid)
                     (let ((raw (car rest)))
                       (cond
                         ((todo-item? raw)
                           (loop (cdr rest)
                                 (max (+ nid 1) (+ (todo-item-id raw) 1))
                                 (cons raw acc)))
                         ((string? raw)
                           (loop (cdr rest)
                                 (+ nid 1)
                                 (cons (%make-todo-item nid raw #f 'user 'normal) acc)))
                         ((pair? raw)
                           (let* ((txt (ensure-string (or (assoc-ref raw 'text) "")))
                                  (done? (and (assoc-ref raw 'done?) #t))
                                  (src (ensure-symbol (or (assoc-ref raw 'source) 'user)))
                                  (prio (ensure-symbol (or (assoc-ref raw 'priority) 'normal)))
                                  (id (or (assoc-ref raw 'id) nid)))
                             (loop (cdr rest)
                                   (max (+ nid 1) (+ id 1))
                                   (cons (%make-todo-item id txt done? src prio) acc))))
                         (else (loop (cdr rest) nid acc)))))))

(define* (todo-add tl text #:key (source 'user) (done? #f) (priority 'normal))
         "Return a new <todo-list> with TEXT appended as a task from SOURCE ('user or 'agent)."
         (let* ((nid (todo-list-next-id tl))
                (item (%make-todo-item nid
                                       (ensure-string (or text ""))
                                       (and done? #t)
                                       (if (memq source '(user agent)) source 'user)
                                       (if (memq priority '(low normal high)) priority 'normal))))
           (%make-todo-list (todo-list-title tl)
                            (append (todo-list-items tl) (list item))
                            (+ nid 1))))

(define (todo-match-item? item id-or-index idx)
        (or (eqv? (todo-item-id item) id-or-index)
            (and (symbol? id-or-index) (eqv? idx -999)) ; symbols never match
            (and (pair? id-or-index)
                 (eq? (car id-or-index) 'index)
                 (eqv? (cdr id-or-index) idx))))

(define (todo-toggle tl id-or-index)
        "Return a new <todo-list> with the item matching ID-OR-INDEX (integer id or
'(index . idx)) toggled between done and open."
        (let ((new-items
                (let loop ((rest (todo-list-items tl)) (idx 0) (acc '()))
                          (if (null? rest)
                            (reverse acc)
                            (let ((it (car rest)))
                              (if (todo-match-item? it id-or-index idx)
                                (loop (cdr rest) (+ idx 1)
                                      (cons (%make-todo-item (todo-item-id it)
                                                             (todo-item-text it)
                                                             (not (todo-item-done? it))
                                                             (todo-item-source it)
                                                             (todo-item-priority it))
                                            acc))
                                (loop (cdr rest) (+ idx 1) (cons it acc))))))))
          (%make-todo-list (todo-list-title tl) new-items (todo-list-next-id tl))))

(define (todo-remove tl id-or-index)
        "Return a new <todo-list> without the item matching ID-OR-INDEX."
        (let ((new-items
                (let loop ((rest (todo-list-items tl)) (idx 0) (acc '()))
                          (if (null? rest)
                            (reverse acc)
                            (let ((it (car rest)))
                              (if (todo-match-item? it id-or-index idx)
                                (loop (cdr rest) (+ idx 1) acc)
                                (loop (cdr rest) (+ idx 1) (cons it acc))))))))
          (%make-todo-list (todo-list-title tl) new-items (todo-list-next-id tl))))

(define (todo-clear-done tl)
        "Return a new <todo-list> with all completed items removed."
        (%make-todo-list (todo-list-title tl)
                         (filter (lambda (it) (not (todo-item-done? it)))
                                 (todo-list-items tl))
                         (todo-list-next-id tl)))

(define (todo-counts tl)
        "Return (done-count . total-count) for TL."
        (let ((items (todo-list-items tl)))
          (cons (count todo-item-done? items) (length items))))

(define (todo-list->alist tl)
        "Serialize <todo-list> into a JSON/S-expr safe alist."
        `((title . ,(todo-list-title tl))
          (items . ,(map (lambda (it)
                           `((id . ,(todo-item-id it))
                             (text . ,(todo-item-text it))
                             (done? . ,(todo-item-done? it))
                             (source . ,(todo-item-source it))
                             (priority . ,(todo-item-priority it))))
                         (todo-list-items tl)))))

(define (alist->todo-list alist)
        "Deserialize an alist produced by todo-list->alist back into a <todo-list>."
        (make-todo-list #:title (or (assoc-ref alist 'title) "Tasks")
                        #:items (or (assoc-ref alist 'items) '())))

(define* (format-todo-widget tl maxx
                             #:key (selected-index #f) (show-source? #t))
         "Format <todo-list> into styled span rows ((text role bold? dim?) ...)."
         (let* ((w (max 10 maxx))
                (counts (todo-counts tl))
                (done-n (car counts))
                (total-n (cdr counts))
                (header (list (list (clip-str (format #f "~a (~a/~a)"
                                                      (todo-list-title tl)
                                                      done-n total-n)
                                              w)
                                    'header #t #f)))
                (items (todo-list-items tl)))
           (if (null? items)
             (list header (list (list (clip-str "  (no tasks)" w) 'dim #f #t)))
             (cons header
                   (let loop ((rest items) (idx 0) (acc '()))
                             (if (null? rest)
                               (reverse acc)
                               (let* ((it (car rest))
                                      (sel? (and (integer? selected-index) (= idx selected-index)))
                                      (done? (todo-item-done? it))
                                      (cursor (if sel? "❯ " "  "))
                                      (box (if done? "[✓] " "[ ] "))
                                      (box-role (if done? 'success 'accent))
                                      (src-tag (if show-source?
                                                 (format #f " [~a]" (todo-item-source it))
                                                 ""))
                                      (txt-role (cond
                                                  (sel? 'accent)
                                                  (done? 'dim)
                                                  ((eq? (todo-item-priority it) 'high) 'warning)
                                                  (else 'default)))
                                      (row (clip-spans
                                             (append
                                               (list (list cursor (if sel? 'accent 'default) sel? #f)
                                                     (list box box-role #t #f)
                                                     (list (todo-item-text it) txt-role sel? done?))
                                               (if (string-null? src-tag)
                                                 '()
                                                 (list (list src-tag 'dim #f #t))))
                                             w)))
                                 (loop (cdr rest) (+ idx 1) (cons row acc)))))))))

(define* (todo-widget tl #:key (selected-index #f) (show-source? #t))
         "Wrap <todo-list> in an interactive composable <widget>."
         (make-widget
           'todo
           #:props `((list . ,tl) (selected-index . ,selected-index))
           #:measure
           (lambda (_w avail-w avail-h)
             (let ((rows (format-todo-widget tl avail-w
                                             #:selected-index selected-index
                                             #:show-source? show-source?)))
               (cons avail-w (min avail-h (length rows)))))
           #:render
           (lambda (_w rect)
             (let ((rows (format-todo-widget tl (rect-width rect)
                                             #:selected-index selected-index
                                             #:show-source? show-source?)))
               (let loop ((rest rows) (dy 0) (acc '()))
                         (if (or (null? rest) (>= dy (rect-height rect)))
                           (reverse acc)
                           (let sloop ((spans (car rest)) (cx (rect-x rect)) (sacc acc))
                                      (if (null? spans)
                                        (loop (cdr rest) (+ dy 1) sacc)
                                        (let* ((sp (car spans))
                                               (txt (car sp)))
                                          (sloop (cdr spans)
                                                 (+ cx (string-length txt))
                                                 (cons (make-draw-op cx (+ (rect-y rect) dy) txt
                                                                     #:role (cadr sp)
                                                                     #:bold? (caddr sp)
                                                                     #:dim? (cadddr sp))
                                                       sacc)))))))))))

;;;
;;; Safe Live Scheme Response Widgets (ui-wp1.5)
;;;
(define (parse-kw-args args)
        "Split ARGS into (values positional keyword-alist)."
        (let loop ((rest args) (pos '()) (kws '()))
                  (cond
                    ((null? rest) (values (reverse pos) (reverse kws)))
                    ((and (keyword? (car rest)) (pair? (cdr rest)))
                      (loop (cddr rest) pos (cons (cons (keyword->symbol (car rest)) (cadr rest)) kws)))
                    (else
                      (loop (cdr rest) (cons (car rest) pos) kws)))))

(define (unquote-datum x)
        (if (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x)) (null? (cddr x)))
          (cadr x)
          x))

(define* (progress-bar-widget done total #:key (label #f) (role 'accent))
         (let* ((t (max 1 total))
                (d (max 0 (min done t)))
                (pct (inexact->exact (round (* 100 (/ d t))))))
           (make-widget
             'progress
             #:props `((done . ,d) (total . ,t) (label . ,label))
             #:measure (lambda (_w avail-w _avail-h) (cons avail-w 1))
             #:render
             (lambda (_w rect)
               (let* ((w (rect-width rect))
                      (lbl (if (and (string? label) (not (string-null? label)))
                             (string-append label " ")
                             ""))
                      (suffix (format #f " ~a/~a (~a%)" d t pct))
                      (bar-w (max 4 (- w (string-length lbl) (string-length suffix) 2)))
                      (fill-w (inexact->exact (round (* bar-w (/ d t)))))
                      (empty-w (max 0 (- bar-w fill-w)))
                      (bar (string-append "["
                                          (make-string fill-w #\█)
                                          (make-string empty-w #\░)
                                          "]"))
                      (full (clip-str (string-append lbl bar suffix) w)))
                 (list (make-draw-op (rect-x rect) (rect-y rect) full #:role role)))))))

(define (kv-widget pairs)
        (let ((rows (map (lambda (p)
                           (let ((k (ensure-string (if (pair? p) (car p) p)))
                                 (v (ensure-string (cond
                                                     ((and (pair? p) (pair? (cdr p))) (cadr p))
                                                     ((pair? p) (cdr p))
                                                     (else "")))))
                             (hstack-widget (list (text-widget (string-append k ":") #:role 'dim #:dim? #t)
                                                  (text-widget v #:role 'default))
                                            #:gap 1)))
                         (if (list? pairs) pairs '()))))
          (vstack-widget rows)))

(define (safe-eval-widget-datum datum)
        "Safely interpret a pure declarative widget S-expression DATUM into a <widget>.
Never invokes Guile eval; only accepts the whitelisted declarative constructors:
text, pill, rule, box, vstack, hstack, scroll-view, todo, diff, progress, kv."
        (unless (pair? datum)
          (error "widget datum must be a list expression" datum))
        (let ((head (car datum)))
          (call-with-values
            (lambda () (parse-kw-args (cdr datum)))
            (lambda (pos kws)
              (let ((kw (lambda (sym default)
                          (let ((p (assq sym kws)))
                            (if p (unquote-datum (cdr p)) default)))))
                (case head
                  ((text)
                    (text-widget (if (pair? pos) (unquote-datum (car pos)) "")
                                 #:role (kw 'role 'default)
                                 #:bold? (kw 'bold? #f)
                                 #:dim? (kw 'dim? #f)
                                 #:align (kw 'align 'left)))
                  ((pill)
                    (pill-widget (if (pair? pos) (unquote-datum (car pos)) "")
                                 #:role (kw 'role 'accent)
                                 #:bold? (kw 'bold? #t)
                                 #:prefix (kw 'prefix "[")
                                 #:suffix (kw 'suffix "]")))
                  ((rule)
                    (rule-widget #:role (kw 'role 'dim)
                                 #:label (kw 'label #f)
                                 #:dim? (kw 'dim? #t)))
                  ((box)
                    (let ((child-expr (cond
                                        ((>= (length pos) 2) (cadr pos))
                                        ((= (length pos) 1) (car pos))
                                        (else #f)))
                          (title (or (kw 'title #f)
                                     (and (>= (length pos) 2) (unquote-datum (car pos))))))
                      (box-widget (and child-expr (safe-eval-widget-datum child-expr))
                                  #:title title
                                  #:role (kw 'role 'frame)
                                  #:rounded? (kw 'rounded? #t)
                                  #:padding (kw 'padding 0))))
                  ((vstack)
                    (vstack-widget (map safe-eval-widget-datum pos)
                                   #:gap (kw 'gap 0)))
                  ((hstack)
                    (hstack-widget (map safe-eval-widget-datum pos)
                                   #:gap (kw 'gap 1)))
                  ((scroll-view)
                    (scroll-view-widget (map ensure-string (map unquote-datum pos))
                                        #:scroll-offset (kw 'scroll-offset 0)
                                        #:show-scrollbar? (kw 'show-scrollbar? #t)))
                  ((todo)
                    (todo-widget (make-todo-list #:title (kw 'title "Tasks")
                                                 #:items (map unquote-datum pos))
                                 #:selected-index (kw 'selected-index #f)
                                 #:show-source? (kw 'show-source? #t)))
                  ((diff)
                    (git-diff-widget (if (pair? pos) (unquote-datum (car pos)) "")
                                     #:title (kw 'title #f)
                                     #:show-line-numbers? (kw 'show-line-numbers? #t)))
                  ((progress)
                    (progress-bar-widget (if (>= (length pos) 1) (unquote-datum (car pos)) 0)
                                         (if (>= (length pos) 2) (unquote-datum (cadr pos)) 1)
                                         #:label (kw 'label #f)
                                         #:role (kw 'role 'accent)))
                  ((kv)
                    (kv-widget (map unquote-datum pos)))
                  (else
                    (error "unsupported widget constructor" head))))))))

(define (widget-fence-opener? line)
        (let ((trimmed (string-trim-both line)))
          (or (string=? trimmed "```scheme-widget")
              (string=? trimmed "```widget"))))

(define (extract-response-widgets text)
        "Extract fenced ```scheme-widget or ```widget blocks from agent response TEXT."
        (let ((lines (string-split (ensure-string (or text "")) #\newline)))
          (let loop ((rest lines) (in-fence? #f) (cur '()) (acc '()))
                    (if (null? rest)
                      (reverse acc)
                      (let ((line (string-trim-right (car rest) #\return)))
                        (cond
                          ((and (not in-fence?) (widget-fence-opener? line))
                            (loop (cdr rest) #t '() acc))
                          ((and in-fence? (string=? (string-trim-both line) "```"))
                            (loop (cdr rest) #f '()
                                  (cons (string-join (reverse cur) "\n") acc)))
                          (in-fence?
                            (loop (cdr rest) #t (cons line cur) acc))
                          (else
                            (loop (cdr rest) #f '() acc))))))))

(define* (render-scheme-response-widget expr-or-string maxx
                                        #:key (max-height 24))
         "Safely parse/interpret EXPR-OR-STRING as a live declarative Scheme widget
and render it for width MAXX (capped at MAX-HEIGHT). Returns an alist:
  ok?     #t or #f
  widget  <widget> (fallback error box when ok? is #f)
  ops     list of <draw-op>
  lines   list of plain-text rendered strings
  height  integer row count
  error   error string or #f."
         (let* ((w (max 10 maxx))
                (mh (max 1 max-height)))
           (catch #t
             (lambda ()
               (let* ((datum (if (string? expr-or-string)
                               (call-with-input-string expr-or-string read)
                               expr-or-string))
                      (_ (when (eof-object? datum)
                           (error "empty widget expression")))
                      (widget (safe-eval-widget-datum datum))
                      (sz (widget-measure-size widget w mh))
                      (h (max 1 (min mh (cdr sz))))
                      (rect (make-rect 0 0 w h))
                      (ops (widget-render-rect widget rect))
                      (lines (render-ops->lines ops w h)))
                 `((ok? . #t)
                   (widget . ,widget)
                   (ops . ,ops)
                   (lines . ,lines)
                   (height . ,h)
                   (error . #f))))
             (lambda (key . args)
               (let* ((err-msg (format-exception key args))
                      (fallback (box-widget (text-widget err-msg #:role 'error)
                                            #:title "widget error"
                                            #:role 'error))
                      (sz (widget-measure-size fallback w mh))
                      (h (max 3 (min mh (cdr sz))))
                      (ops (widget-render-rect fallback (make-rect 0 0 w h)))
                      (lines (render-ops->lines ops w h)))
                 `((ok? . #f)
                   (widget . ,fallback)
                   (ops . ,ops)
                   (lines . ,lines)
                   (height . ,h)
                   (error . ,err-msg)))))))
