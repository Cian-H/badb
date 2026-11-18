;;; (geasa surfaces-tui renderer) — TUI Screen & Overlay Renderer
;;;
;;; Provides notcurses/ncurses drawing primitives, theme color-pair memoization,
;;; incremental transcript rendering, input-box & status-bar painting, floating
;;; popup drawing, and generic <overlay> rendering on top of <tui-state>.

(define-module (geasa surfaces-tui renderer)
               #:use-module
               (geasa foundation-config plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (badb kernel journal)
               #:use-module
               (badb kernel registry)
               #:use-module
               (geasa capabilities-mcp plugin)
               #:use-module
               (geasa runtimes-context plugin)
               #:use-module
               (geasa runtimes-react plugin)
               #:use-module
               (geasa agents-lifecycle plugin)
               #:use-module
               (geasa surfaces-theme plugin)
               #:use-module
               (geasa surfaces-tui editor)
               #:use-module
               (geasa surfaces-tui commands)
               #:use-module
               (geasa surfaces-tui steering)
               #:use-module
               (geasa surfaces-tui transcript)
               #:use-module
               (geasa surfaces-tui markdown)
               #:use-module
               (geasa surfaces-tui widgets)
               #:use-module
               (geasa surfaces-tui overlay)
               #:use-module
               (geasa surfaces-tui state)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 threads)
               #:use-module
               (srfi srfi-1)
               #:export
               (tui-theme-attr-constant
                 tui-theme-color->backend-value
                 tui-safe-write-osc-title!
                 tui-safe-curs-set!
                 tui-suspend-tty!
                 tui-resume-tty!
                 tui-draw-role!
                 tui-safe-addstr
                 tui-draw-popup!
                 tui-journal-count-cached
                 tui-context-gauge-info
                 tui-render!
                 tui-request-render!))

(define %max-color-pairs 64)

(define (tui-theme-attr-constant attr nref)
        "Resolve theme attribute symbol ATTR using backend NREF."
        (case attr
          ((bold) (nref 'A_BOLD))
          ((dim) (nref 'A_DIM))
          ((reverse) (nref 'A_REVERSE))
          (else #f)))

(define
  (tui-theme-color->backend-value color nref)
  "Resolve theme COLOR (hex #rrggbb or curses symbol) via backend NREF."
  (if (string? color)
    (string->number (substring color 1) 16)
    (nref (color-symbol->ncurses-constant color))))

(define (tui-safe-write-osc-title! seq)
        "Write OSC title sequence SEQ to the notcurses tty control port when available."
        (when
            (and (string? seq) (not (string-null? seq)))
          (catch #t
            (lambda ()
              (let*
                  ((term-mod (false-if-exception (resolve-module '(notcurses terminal) #f)))
                   (port-fn (and term-mod (module-ref term-mod 'tty-control-port #f)))
                   (port (and (procedure? port-fn) (port-fn))))
                (when
                    (and (output-port? port) (not (port-closed? port)))
                  (display seq port)
                  (force-output port))))
            (lambda _
              #f))))

(define (tui-suspend-tty! st)
        "Stop the terminal backend so a child process (e.g. $EDITOR) owns the tty."
        (catch #t
          (lambda ()
            (((tui-nref st) 'endwin)))
          (lambda _
            #f)))

(define (tui-resume-tty! st)
        "Re-initialize the terminal backend after TUI-SUSPEND-TTY! and force a redraw.
The notcurses curses shim's endwin stops notcurses entirely, so a bare refresh
cannot restore the screen; initscr must run again."
        (let* ((nref (tui-nref st)) (initscr (nref 'initscr)))
          (set-tui-stdscr! st (initscr))
          (tui-safe-curs-set! st 1)
          (set-tui-render-dirty?! st #t)))

(define (tui-safe-curs-set! st v)
        "Set cursor visibility V via ST's backend, ignoring errors."
        (let
            ((curs-set (and (tui-nref st) ((tui-nref st) 'curs-set))))
          (when curs-set
            (catch #t
              (lambda ()
                (curs-set v))
              (lambda _
                #f)))))

(define (role-attrs st role)
        (let
            ((a (caddr (theme-ref (tui-ui-theme st) role))))
          (cond
            ((memq a '(bold dim reverse)) (list a))
            ((list? a) a)
            (else '()))))

(define (ensure-color-pair! st fg bg)
        (let
            ((key (cons fg bg))
             (cache (tui-role-pair-cache st))
             (next-id (tui-next-pair-id st))
             (nref (tui-nref st))
             (init-pair! ((tui-nref st) 'init-pair!)))
          (or (assoc-ref cache key)
              (and (< next-id %max-color-pairs)
                   (begin
                     (catch #t
                       (lambda ()
                         (init-pair! next-id
                                     (tui-theme-color->backend-value fg nref)
                                     (tui-theme-color->backend-value bg nref)))
                       (lambda _
                         #f))
                     (set-tui-role-pair-cache! st (acons key next-id cache))
                     (set-tui-next-pair-id! st (+ next-id 1))
                     next-id))
              0)))

(define (tui-draw-role! st role on?)
        "Enable or disable theme ROLE attributes and color pair on ST's stdscr."
        (let*
            ((nref (tui-nref st)) (stdscr (tui-stdscr st))
             (attr-on! (nref 'attr-on!))
             (attr-off! (nref 'attr-off!))
             (color-pair (nref 'color-pair))
             (spec (theme-ref (tui-ui-theme st) role))
             (attrs (role-attrs st role))
             (pair-off?
               (and (tui-colors-enabled? st)
                    (not (and (eq? (car spec) 'default) (eq? (cadr spec) 'default))))))
          (if on?
            (begin
              (when pair-off?
                (attr-on! stdscr (color-pair (ensure-color-pair! st (car spec) (cadr spec)))))
              (for-each
                (lambda (a)
                  (let ((c (tui-theme-attr-constant a nref))) (when c (attr-on! stdscr c))))
                attrs))
            (begin
              (for-each
                (lambda (a)
                  (let ((c (tui-theme-attr-constant a nref))) (when c (attr-off! stdscr c))))
                attrs)
              (when pair-off?
                (attr-off! stdscr (color-pair (ensure-color-pair! st (car spec) (cadr spec)))))))))

(define (tui-safe-addstr st win y x str max-len)
        "Safely draw STR clipped to MAX-LEN at (Y, X) on WIN."
        (when (and (>= y 0) (>= x 0) (> max-len 0))
          (let*
              ((nref (tui-nref st)) (move (nref 'move))
               (addstr (nref 'addstr))
               (safe-str
                 (if (> (string-length str) max-len)
                   (substring str 0 max-len)
                   str)))
            (catch #t
              (lambda ()
                (move win y x)
                (addstr win safe-str))
              (lambda _
                #f)))))

(define
  (draw-segment-list st y start-x segs inner-w)
  (let*
      ((nref (tui-nref st)) (stdscr (tui-stdscr st))
       (attr-on! (nref 'attr-on!))
       (attr-off! (nref 'attr-off!))
       (A_BOLD (nref 'A_BOLD))
       (A_DIM (nref 'A_DIM)))
    (let loop
      ((rest segs) (cur-x start-x))
      (if
          (or (null? rest) (>= cur-x (+ start-x inner-w)))
        (let ((rem (- (+ start-x inner-w) cur-x)))
          (when (> rem 0)
            (tui-safe-addstr st stdscr y cur-x (make-string rem #\space) rem)))
        (let*
            ((seg (car rest)) (str (car seg))
             (seg-role (cadr seg))
             (bold? (caddr seg))
             (dim? (cadddr seg))
             (len (min (string-length str) (max 0 (- (+ start-x inner-w) cur-x)))))
          (when (> len 0)
            (tui-draw-role! st seg-role #t)
            (when bold? (attr-on! stdscr A_BOLD))
            (when dim? (attr-on! stdscr A_DIM))
            (tui-safe-addstr st stdscr y cur-x str len)
            (when dim? (attr-off! stdscr A_DIM))
            (when bold? (attr-off! stdscr A_BOLD))
            (tui-draw-role! st seg-role #f))
          (loop (cdr rest) (+ cur-x len)))))))

(define
  (draw-styled-interior st y start-x text inner-w theme-role)
  (let*
      ((nref (tui-nref st)) (stdscr (tui-stdscr st))
       (attr-on! (nref 'attr-on!))
       (attr-off! (nref 'attr-off!))
       (A_BOLD (nref 'A_BOLD))
       (A_DIM (nref 'A_DIM))
       (text-len (string-length text))
       (pad-len (max 0 (- inner-w text-len)))
       (padded
         (if (> pad-len 0)
           (string-append text (make-string pad-len #\space))
           (if (> text-len inner-w)
             (substring text 0 inner-w)
             text))))
    (if (memq theme-role '(error user thought))
      (begin (tui-draw-role! st theme-role #t)
             (tui-safe-addstr st stdscr y start-x padded inner-w)
             (tui-draw-role! st theme-role #f))
      (let loop
        ((segs (segment-line padded)) (cur-x start-x))
        (unless
            (or (null? segs) (>= cur-x (+ start-x inner-w)))
          (let*
              ((seg (car segs)) (str (car seg))
               (seg-role (cadr seg))
               (bold? (caddr seg))
               (dim? (cadddr seg))
               (len (min (string-length str) (max 0 (- (+ start-x inner-w) cur-x)))))
            (when (> len 0)
              (tui-draw-role! st seg-role #t)
              (when bold? (attr-on! stdscr A_BOLD))
              (when dim? (attr-on! stdscr A_DIM))
              (tui-safe-addstr st stdscr y cur-x str len)
              (when dim? (attr-off! stdscr A_DIM))
              (when bold? (attr-off! stdscr A_BOLD))
              (tui-draw-role! st seg-role #f))
            (loop (cdr segs) (+ cur-x len))))))))

(define (draw-gutter! st y theme-role selected?)
        (let*
            ((nref (tui-nref st)) (stdscr (tui-stdscr st))
             (attr-on! (nref 'attr-on!))
             (attr-off! (nref 'attr-off!))
             (A_BOLD (nref 'A_BOLD)))
          (tui-draw-role! st theme-role #t)
          (when selected? (attr-on! stdscr A_BOLD))
          (tui-safe-addstr st
                           stdscr
                           y
                           0
                           (if selected?
                             "██"
                             "▏ ")
                           2)
          (when selected? (attr-off! stdscr A_BOLD))
          (tui-draw-role! st theme-role #f)))

(define
  (draw-block-line st y entry content-width selected?)
  (let
      ((stdscr (tui-stdscr st)) (kind (car entry)))
    (cond
      ((string=? kind "spacer") #t)
      ((string=? kind "block-label")
        (let*
            ((label (cadr entry)) (theme-role (caddr entry)))
          (draw-gutter! st y theme-role selected?)
          (tui-draw-role! st theme-role #t)
          (let*
              ((label-str
                 (if (> (string-length label) content-width)
                   (substring label 0 content-width)
                   label))
               (pad-len (max 0 (- content-width (string-length label-str))))
               (padded (string-append label-str (make-string pad-len #\space))))
            (tui-safe-addstr st stdscr y 2 padded content-width))
          (tui-draw-role! st theme-role #f)))
      ((string=? kind "block-body")
        (let*
            ((theme-role (cadr entry)) (text (caddr entry))
             (has-precomputed? (and (pair? (cdddr entry)) (list? (cadddr entry)))))
          (draw-gutter! st y theme-role selected?)
          (hashq-set! (tui-row-texts st) y text)
          (if has-precomputed?
            (draw-segment-list st y 2 (cadddr entry) content-width)
            (draw-styled-interior st y 2 text content-width theme-role))))
      ((string=? kind "block-summary")
        (let*
            ((theme-role (cadr entry)) (text (caddr entry)))
          (draw-gutter! st y theme-role selected?)
          (tui-draw-role! st theme-role #t)
          (tui-safe-addstr st stdscr y 2 "› " 2)
          (tui-draw-role! st theme-role #f)
          (tui-draw-role! st 'dim #t)
          (let*
              ((text-len (string-length text))
               (avail (max 0 (- content-width 2)))
               (text-str
                 (if (> text-len avail)
                   (substring text 0 avail)
                   text))
               (pad-len (max 0 (- avail (string-length text-str))))
               (padded (string-append text-str (make-string pad-len #\space))))
            (tui-safe-addstr st stdscr y 4 padded avail))
          (tui-draw-role! st 'dim #f))))))

(define
  (tui-draw-popup! st
                   title
                   rows
                   sel-pos
                   box-w
                   start-y
                   start-x
                   above-count
                   below-count)
  "Draw a floating rounded pop-up on ST's stdscr."
  (let*
      ((nref (tui-nref st)) (stdscr (tui-stdscr st))
       (attr-on! (nref 'attr-on!))
       (attr-off! (nref 'attr-off!))
       (A_BOLD (nref 'A_BOLD))
       (A_REVERSE (nref 'A_REVERSE))
       (colors? (tui-colors-enabled? st))
       (inner-w (- box-w 4))
       (vis-count (length rows))
       (label-w
         (fold
           (lambda (r acc)
             (max acc (string-length (car r))))
           0
           rows))
       (top-tag
         (string-append "╭─ "
                        title
                        (if (> above-count 0)
                          (format #f " ↑~a " above-count)
                          " ")))
       (top-str
         (string-append top-tag
                        (make-string (max 0 (- box-w (string-length top-tag) 1)) #\─)
                        "╮"))
       (bot-tag
         (if (> below-count 0)
           (format #f "╰─ ↓ ~a more " below-count)
           "╰"))
       (bot-str
         (string-append bot-tag
                        (make-string (max 0 (- box-w (string-length bot-tag) 1)) #\─)
                        "╯")))
    (let blank-rows
      ((by start-y) (bi 0))
      (when (< bi (+ vis-count 2))
        (tui-safe-addstr st stdscr by start-x (make-string box-w #\space) box-w)
        (blank-rows (+ by 1) (+ bi 1))))
    (tui-draw-role! st 'accent #t)
    (tui-safe-addstr st stdscr start-y start-x top-str box-w)
    (tui-draw-role! st 'accent #f)
    (let loop
      ((items rows) (idx 0))
      (unless (null? items)
        (let*
            ((label (caar items)) (desc (cdar items))
             (is-sel? (= idx sel-pos))
             (row-y (+ start-y 1 idx))
             (cursor
               (if is-sel?
                 "❯ "
                 "  "))
             (label-pad (make-string (max 1 (- label-w (string-length label))) #\space))
             (content (string-append cursor label label-pad " " desc))
             (clipped
               (if (> (string-length content) inner-w)
                 (substring content 0 inner-w)
                 content))
             (pad (make-string (max 0 (- inner-w (string-length clipped))) #\space))
             (row-text (string-append "│ " clipped pad " │")))
          (if is-sel?
            (begin (tui-draw-role! st 'selection #t)
                   (attr-on! stdscr A_BOLD)
                   (unless colors? (attr-on! stdscr A_REVERSE))
                   (tui-safe-addstr st stdscr row-y start-x row-text box-w)
                   (unless colors? (attr-off! stdscr A_REVERSE))
                   (attr-off! stdscr A_BOLD)
                   (tui-draw-role! st 'selection #f))
            (begin (tui-draw-role! st 'accent #t)
                   (tui-safe-addstr st stdscr row-y start-x "│ " 2)
                   (tui-draw-role! st 'accent #f)
                   (tui-draw-role! st 'default #t)
                   (tui-safe-addstr st
                                    stdscr
                                    row-y
                                    (+ start-x 2)
                                    (string-append cursor label label-pad)
                                    (+ 2 (string-length cursor) (string-length label) (string-length label-pad)))
                   (tui-draw-role! st 'default #f)
                   (tui-draw-role! st 'dim #t)
                   (let*
                       ((desc-x (+ start-x 2 (string-length cursor) label-w 1))
                        (desc-len (min (string-length desc) (max 0 (- (+ start-x box-w -2) desc-x)))))
                     (when (> desc-len 0) (tui-safe-addstr st stdscr row-y desc-x desc desc-len))
                     (tui-draw-role! st 'dim #f))
                   (tui-draw-role! st 'accent #t)
                   (tui-safe-addstr st stdscr row-y (+ start-x box-w -2) " │" 2)
                   (tui-draw-role! st 'accent #f)))
          (loop (cdr items) (+ idx 1)))))
    (tui-draw-role! st 'accent #t)
    (tui-safe-addstr st stdscr (+ start-y 1 vis-count) start-x bot-str box-w)
    (tui-draw-role! st 'accent #f)))

(define (refresh-journal-count-async! st)
        (catch #t
          (lambda ()
            (call-with-new-thread
              (lambda ()
                (let
                    ((cnt
                       (catch #t
                         (lambda ()
                           (journal-count (tui-jrn st)))
                         (lambda _
                           #f))))
                  (with-mutex (tui-jrn-count-mutex st)
                              (when (integer? cnt) (set-tui-jrn-count-cache! st cnt))
                              (set-tui-jrn-count-time! st (get-internal-real-time))
                              (set-tui-jrn-count-refreshing?! st #f))
                  (when (integer? cnt) (set-tui-render-dirty?! st #t))))))
          (lambda _
            (with-mutex (tui-jrn-count-mutex st) (set-tui-jrn-count-refreshing?! st #f)))))

(define (tui-journal-count-cached st)
        "Return cached journal entry count, refreshing asynchronously at most every 2s."
        (let
            ((now (get-internal-real-time)) (should-refresh? #f) (cached -1))
          (with-mutex (tui-jrn-count-mutex st)
                      (set! cached (tui-jrn-count-cache st))
                      (when
                          (and (not (tui-jrn-count-refreshing? st))
                               (>= (- now (tui-jrn-count-time st)) (* 2 internal-time-units-per-second)))
                        (set-tui-jrn-count-refreshing?! st #t)
                        (set-tui-jrn-count-time! st now)
                        (set! should-refresh? #t)))
          (when should-refresh? (refresh-journal-count-async! st))
          cached))

(define (tui-context-gauge-info st)
        "Return formatted context usage string (e.g. 'ctx 12.4k/16k') or #f."
        (let ((key (length (tui-llm-history st))))
          (unless (eq? key (tui-ctx-cache-key st))
            (set-tui-ctx-cache-key! st key)
            (set-tui-ctx-cache-tokens! st
                                       (estimate-conversation-tokens (tui-llm-history st))))
          (let
              ((used (tui-ctx-cache-tokens st))
               (ceiling (context-compactor-effective-ceiling (tui-compactor st))))
            (if (and (number? used) (> ceiling 0))
              (format #f "ctx ~a/~a" (format-token-kilo used) (format-token-kilo ceiling))
              #f))))

(define
  (render-transcript! st view-top view-bottom view-height content-width)
  (let*
      ((messages (tui-messages st))
       (row-cache (tui-row-cache st))
       (asm-valid?
         (and (eq? (tui-asm-messages st) messages)
              (eqv? (tui-asm-width st) content-width)))
       (msg-count
         (if asm-valid?
           (tui-asm-msg-count st)
           (length messages)))
       (sel-idx
         (and (integer? (tui-msgsel-index st))
              (< (tui-msgsel-index st) msg-count)
              (tui-msgsel-index st))))
    (unless asm-valid?
      (when (< msg-count (tui-asm-msg-count st))
        (row-cache-prune! row-cache messages))
      (let
          ((counts
             (map
               (lambda (msg)
                 (length (row-cache-rows row-cache msg content-width)))
               messages)))
        (set-tui-asm-counts! st counts)
        (set-tui-asm-total! st (apply + 0 counts))
        (set-tui-asm-msg-count! st msg-count)
        (set-tui-asm-messages! st messages)
        (set-tui-asm-width! st content-width)))
    (let*
        ((total-lines (tui-asm-total st))
         (max-scroll (max 0 (- total-lines view-height))))
      (set-tui-scroll-offset! st (max 0 (min (tui-scroll-offset st) max-scroll)))
      (set-tui-max-scroll-seen! st max-scroll)
      (when
          (and (integer? sel-idx) (>= sel-idx 0) (< sel-idx (length (tui-asm-counts st))))
        (let*
            ((row-counts (tui-asm-counts st))
             (starts
               (let loop
                 ((counts row-counts) (pos 0))
                 (if (null? counts)
                   '()
                   (cons pos (loop (cdr counts) (+ pos (car counts)))))))
             (sel-start (list-ref starts sel-idx))
             (sel-end (+ sel-start (list-ref row-counts sel-idx)))
             (base-start (max 0 (- total-lines view-height (tui-scroll-offset st))))
             (win-end (+ base-start view-height)))
          (cond
            ((< sel-start base-start)
              (set-tui-scroll-offset! st
                                      (min max-scroll (+ (tui-scroll-offset st) (- base-start sel-start)))))
            ((> sel-end win-end)
              (set-tui-scroll-offset! st
                                      (max 0 (- (tui-scroll-offset st) (- sel-end win-end))))))))
      (let*
          ((safe-scroll (max 0 (min (tui-scroll-offset st) max-scroll)))
           (start-line (max 0 (- total-lines view-height safe-scroll)))
           (end-line (+ start-line (min view-height (max 0 (- total-lines start-line)))))
           (visible-lines
             (let loop
               ((msgs messages) (counts (tui-asm-counts st)) (idx 0) (pos 0) (acc '()))
               (if (or (null? msgs) (>= pos end-line))
                 (reverse acc)
                 (let*
                     ((msg (car msgs)) (cnt (car counts)) (next-pos (+ pos cnt)))
                   (if (<= next-pos start-line)
                     (loop (cdr msgs) (cdr counts) (+ idx 1) next-pos acc)
                     (let*
                         ((skip (max 0 (- start-line pos)))
                          (take-count (min (- cnt skip) (- end-line (+ pos skip))))
                          (rows (row-cache-rows row-cache msg content-width))
                          (sliced (take (drop rows skip) take-count))
                          (tagged
                            (map
                              (lambda (r)
                                (cons idx r))
                              sliced)))
                       (loop (cdr msgs) (cdr counts) (+ idx 1) next-pos (append (reverse tagged) acc)))))))))
        (let loop
          ((lines visible-lines) (y view-top))
          (unless (or (null? lines) (>= y view-bottom))
            (let ((tagged (car lines)))
              (unless
                  (assoc-ref (list-ref messages (car tagged)) 'startup-banner?)
                (hashq-set! (tui-click-targets st) y (car tagged)))
              (draw-block-line st
                               y
                               (cdr tagged)
                               content-width
                               (and sel-idx (= (car tagged) sel-idx))))
            (loop (cdr lines) (+ y 1))))))))

(define
  (render-running-steering-bar! st
                                maxy
                                maxx
                                view-bottom
                                prompt-top
                                avail-width
                                popup-anchor-y)
  (let*
      ((nref (tui-nref st)) (stdscr (tui-stdscr st))
       (attr-on! (nref 'attr-on!))
       (attr-off! (nref 'attr-off!))
       (A_BOLD (nref 'A_BOLD))
       (move (nref 'move))
       (cursor-shape (nref 'cursor-shape))
       (bar
         (steering-bar-layout (tui-steering-buffer st)
                              (steering-count (tui-steering-queue st))
                              maxx
                              #:interrupt-requested?
                              (tui-interrupt-requested? st)
                              #:urgent-pending?
                              (tui-urgent-steer-requested? st)
                              #:status-text
                              (tui-status-text st)))
       (badge (assoc-ref bar 'badge))
       (badge-role (assoc-ref bar 'badge-role))
       (meta-line (assoc-ref bar 'meta-line))
       (in-text (assoc-ref bar 'input-text))
       (placeholder? (assoc-ref bar 'placeholder?))
       (slash-mode? (eq? (assoc-ref bar 'mode) 'slash))
       (glyph
         (if slash-mode?
           "/"
           (streaming-spinner-glyph))))
    (tui-draw-role! st badge-role #t)
    (attr-on! stdscr A_BOLD)
    (tui-safe-addstr st stdscr view-bottom 0 badge maxx)
    (attr-off! stdscr A_BOLD)
    (tui-draw-role! st badge-role #f)
    (let ((rem-x (string-length badge)))
      (when (and (string? meta-line) (< rem-x maxx))
        (tui-draw-role! st 'dim #t)
        (tui-safe-addstr st stdscr view-bottom rem-x meta-line (- maxx rem-x))
        (tui-draw-role! st 'dim #f)))
    (tui-draw-role! st 'accent #t)
    (attr-on! stdscr A_BOLD)
    (tui-safe-addstr st stdscr prompt-top 0 glyph 1)
    (attr-off! stdscr A_BOLD)
    (tui-draw-role! st 'accent #f)
    (tui-draw-role! st
                    (if placeholder?
                      'dim
                      'default)
                    #t)
    (tui-safe-addstr st stdscr prompt-top 2 in-text avail-width)
    (tui-draw-role! st
                    (if placeholder?
                      'dim
                      'default)
                    #f)
    (tui-draw-role! st 'dim #t)
    (tui-safe-addstr st stdscr (- maxy 2) 0 (assoc-ref bar 'hint) maxx)
    (tui-draw-role! st 'dim #f)
    (when (selector-active? (tui-steering-buffer st))
      (let*
          ((matches (matching-mid-run-slash-commands (tui-steering-buffer st)))
           (box-w (min 62 (max 20 (- maxx 4)))))
        (unless (null? matches)
          (call-with-values
            (lambda ()
              (selector-visible-slice matches
                                      (min %selector-max-visible (max 1 (- maxy 5)))
                                      (tui-steering-selector-index st)))
            (lambda
                (vis-items _offset sel-pos above-count below-count)
              (let*
                  ((vis-count (length vis-items))
                   (box-h (+ vis-count 2))
                   (start-y (- popup-anchor-y 1 box-h))
                   (rows
                     (map
                       (lambda (item)
                         (cons (format #f "/~a" (car item)) (cdr item)))
                       vis-items)))
                (when (>= start-y 2)
                  (tui-draw-popup! st
                                   "mid-run commands"
                                   rows
                                   sel-pos
                                   box-w
                                   start-y
                                   2
                                   above-count
                                   below-count))))))))
    (unless (tui-active-overlay st)
      (if placeholder?
        (begin (move stdscr prompt-top 2) (tui-safe-curs-set! st 0))
        (begin
          (move stdscr prompt-top (+ 2 (assoc-ref bar 'cursor-col)))
          (tui-safe-curs-set! st 1)
          (cursor-shape 'bar))))))

(define
  (render-idle-popups! st maxy maxx popup-anchor-y)
  (let
      ((hs (tui-history-search st))
       (buf (tui-input-buffer st))
       (pm (tui-prompt-mode st))
       (cpos (tui-cursor-pos st)))
    (when
        (and (not (history-search-active? hs)) (selector-active? buf pm))
      (let*
          ((matches (deduplicate-slash-commands (matching-slash-commands buf)))
           (box-w (min 62 (max 20 (- maxx 4)))))
        (unless (null? matches)
          (call-with-values
            (lambda ()
              (selector-visible-slice matches
                                      (min %selector-max-visible (max 1 (- maxy 6)))
                                      (tui-selector-index st)))
            (lambda
                (vis-items offset sel-pos above-count below-count)
              (let*
                  ((vis-count (length vis-items))
                   (box-h (+ vis-count 2))
                   (start-y (- popup-anchor-y 1 box-h))
                   (rows
                     (map
                       (lambda (item)
                         (cons (format #f "/~a" (car item)) (cdr item)))
                       vis-items)))
                (when (>= start-y 2)
                  (tui-draw-popup! st
                                   "commands"
                                   rows
                                   sel-pos
                                   box-w
                                   start-y
                                   2
                                   above-count
                                   below-count)
                  (let li
                    ((i 0))
                    (when (< i vis-count)
                      (hashq-set! (tui-click-targets st)
                                  (+ start-y 1 i)
                                  (cons 'palette-item (+ offset i)))
                      (li (+ i 1)))))))))))
    (when (history-search-active? hs)
      (let*
          ((matches (history-search-matches hs))
           (box-w (min 72 (max 24 (- maxx 4))))
           (q (history-search-query hs))
           (title
             (if (string-null? q)
               "history search (ctrl+r)"
               (format #f "history: ~a" q))))
        (if (null? matches)
          (let ((start-y (- popup-anchor-y 1 3)))
            (when (>= start-y 2)
              (tui-draw-popup! st
                               title
                               '(("(no matching history)" . "type to refine, esc to cancel"))
                               0
                               box-w
                               start-y
                               2
                               0
                               0)))
          (call-with-values
            (lambda ()
              (selector-visible-slice matches
                                      (min %selector-max-visible (max 1 (- maxy 6)))
                                      (history-search-selected-index hs)))
            (lambda
                (vis-items _offset sel-pos above-count below-count)
              (let*
                  ((vis-count (length vis-items))
                   (box-h (+ vis-count 2))
                   (start-y (- popup-anchor-y 1 box-h))
                   (rows
                     (map
                       (lambda (s)
                         (cons s ""))
                       vis-items)))
                (when (>= start-y 2)
                  (tui-draw-popup! st title rows sel-pos box-w start-y 2 above-count below-count))))))))
    (when
        (and (not (history-search-active? hs))
             (not (selector-active? buf pm))
             (at-selector-active? buf cpos pm))
      (unless (tui-workspace-files-cache st)
        (set-tui-workspace-files-cache! st (collect-workspace-files)))
      (let*
          ((cands
             (matching-at-candidates buf
                                     #:cursor-pos
                                     cpos
                                     #:candidates
                                     (tui-workspace-files-cache st)
                                     #:limit
                                     25))
           (box-w (min 62 (max 20 (- maxx 4)))))
        (unless (null? cands)
          (call-with-values
            (lambda ()
              (selector-visible-slice cands
                                      (min %selector-max-visible (max 1 (- maxy 6)))
                                      (tui-at-selector-index st)))
            (lambda
                (vis-items _offset sel-pos above-count below-count)
              (let*
                  ((vis-count (length vis-items))
                   (box-h (+ vis-count 2))
                   (start-y (- popup-anchor-y 1 box-h))
                   (rows
                     (map
                       (lambda (it)
                         (cons (format #f "@~a" (car it)) (cdr it)))
                       vis-items)))
                (when (>= start-y 2)
                  (tui-draw-popup! st
                                   "@ mentions"
                                   rows
                                   sel-pos
                                   box-w
                                   start-y
                                   2
                                   above-count
                                   below-count))))))))))

(define
  (render-idle-input! st
                      maxy
                      maxx
                      view-bottom
                      box-top
                      box-style
                      input-lines
                      input-cur-row
                      input-cur-col
                      popup-anchor-y)
  (let*
      ((nref (tui-nref st)) (stdscr (tui-stdscr st))
       (attr-on! (nref 'attr-on!))
       (attr-off! (nref 'attr-off!))
       (A_BOLD (nref 'A_BOLD))
       (A_DIM (nref 'A_DIM))
       (buf (tui-input-buffer st))
       (pm (tui-prompt-mode st))
       (status (tui-status-text st))
       (hs (tui-history-search st))
       (input-rows (length input-lines))
       (pending
         (steering-pending-line (tui-steering-buffer st)
                                (steering-count (tui-steering-queue st))
                                (tui-interrupt-requested? st))))
    (when pending
      (tui-draw-role! st 'dim #t)
      (tui-safe-addstr st stdscr view-bottom 0 pending maxx)
      (tui-draw-role! st 'dim #f))
    (call-with-values
      (lambda ()
        (get-prompt-indicator buf status pm))
      (lambda (glyph theme-role _bold?)
        (when
            (or (string=? status "STREAMING...") (agent-run-active? status))
          (set! glyph (streaming-spinner-glyph)))
        (let*
            ((frame
               (format-input-box-frame input-lines
                                       maxx
                                       #:prompt-glyph
                                       glyph
                                       #:prompt-role
                                       theme-role
                                       #:prompt-mode
                                       pm
                                       #:style
                                       box-style
                                       #:focused?
                                       (not (tui-active-overlay st))
                                       #:cursor-row
                                       input-cur-row
                                       #:cursor-col
                                       input-cur-col))
             (text-orig-y (assoc-ref frame 'text-origin-y))
             (text-orig-x (assoc-ref frame 'text-origin-x))
             (hl-mode
               (if (selector-active? buf pm)
                 'slash
                 pm)))
          (for-each
            (lambda (row-entry)
              (let*
                  ((dy (car row-entry)) (y (+ box-top dy))
                   (is-input-row? (and (>= dy text-orig-y) (< (- dy text-orig-y) input-rows))))
                (for-each
                  (lambda (seg)
                    (let
                        ((sx (car seg)) (stext (cadr seg))
                         (srole (caddr seg))
                         (sbold? (cadddr seg))
                         (sdim? (car (cddddr seg))))
                      (tui-draw-role! st srole #t)
                      (when sbold? (attr-on! stdscr A_BOLD))
                      (when sdim? (attr-on! stdscr A_DIM))
                      (tui-safe-addstr st stdscr y sx stext (max 0 (- maxx sx)))
                      (when sdim? (attr-off! stdscr A_DIM))
                      (when sbold? (attr-off! stdscr A_BOLD))
                      (tui-draw-role! st srole #f)))
                  (cdr row-entry))
                (when is-input-row?
                  (let
                      ((line-txt (list-ref input-lines (- dy text-orig-y))))
                    (unless (string-null? line-txt)
                      (draw-segment-list st
                                         y
                                         text-orig-x
                                         (segment-input-line line-txt #:prompt-mode hl-mode)
                                         (max 1 (- maxx text-orig-x))))))))
            (assoc-ref frame 'rows)))))
    (let
        ((hint
           (format-keymap-line
             (cond
               ((tui-active-overlay st) (overlay-kind (tui-active-overlay st)))
               ((history-search-active? hs) 'selector)
               ((selector-active? buf pm) 'selector)
               ((and (agent-run-active? status) (tui-nodelay-proc st)) 'steering)
               (else pm)))))
      (tui-draw-role! st 'dim #t)
      (tui-safe-addstr st stdscr (- maxy 2) 0 hint maxx)
      (tui-draw-role! st 'dim #f))
    (render-idle-popups! st maxy maxx popup-anchor-y)))

(define
  (render-bottom-status-bar! st maxy maxx running?)
  (let*
      ((nref (tui-nref st)) (stdscr (tui-stdscr st))
       (attr-on! (nref 'attr-on!))
       (attr-off! (nref 'attr-off!))
       (A_BOLD (nref 'A_BOLD))
       (A_DIM (nref 'A_DIM))
       (todo-pair (todo-counts (tui-active-todo-list st)))
       (todo-sum
         (and (> (cdr todo-pair) 0)
              (format #f "✓ ~a/~a" (car todo-pair) (cdr todo-pair))))
       (sub-cnt
         (length
           (filter
             (lambda (l)
               (eq? (subagent-link-status l) 'running))
             (all-subagent-links))))
       (used-tok
         (or (tui-ctx-cache-tokens st)
             (estimate-conversation-tokens (tui-llm-history st))))
       (lim-tok (context-compactor-effective-ceiling (tui-compactor st)))
       (active-mcp
         (length
           (filter
             (lambda (s)
               (assoc-ref s 'running?))
             (mcp-manager-list-servers (tui-mcp-mgr st)))))
       (total-mcp (length (badb-config-mcp-servers (tui-cfg st))))
       (mcp-info (and (> total-mcp 0) (format #f "~a/~a" active-mcp total-mcp))))
    (for-each
      (lambda (seg)
        (let
            ((x (car seg)) (txt (cadr seg))
             (role (caddr seg))
             (b? (cadddr seg))
             (d? (car (cddddr seg))))
          (tui-draw-role! st role #t)
          (when b? (attr-on! stdscr A_BOLD))
          (when d? (attr-on! stdscr A_DIM))
          (tui-safe-addstr st stdscr (- maxy 1) x txt (max 0 (- maxx x)))
          (when d? (attr-off! stdscr A_DIM))
          (when b? (attr-off! stdscr A_BOLD))
          (tui-draw-role! st role #f)))
      (format-bottom-status-bar maxx
                                #:mode
                                (if running?
                                  'steering
                                  (tui-prompt-mode st))
                                #:model
                                (or (tui-cur-model st) "")
                                #:status
                                (tui-status-text st)
                                #:cwd
                                (getcwd)
                                #:tokens-used
                                (if (number? used-tok)
                                  used-tok
                                  0)
                                #:tokens-limit
                                (if (number? lim-tok)
                                  lim-tok
                                  0)
                                #:subagent-count
                                sub-cnt
                                #:todo-summary
                                todo-sum
                                #:feat-count
                                (registry-count (tui-reg st))
                                #:journal-count
                                (tui-journal-count-cached st)
                                #:mcp-info
                                mcp-info))))

(define (tui-render! st)
        "Render the full TUI frame for ST."
        (let*
            ((nref (tui-nref st)) (stdscr (tui-stdscr st))
             (getmaxy (nref 'getmaxy))
             (getmaxx (nref 'getmaxx))
             (erase (nref 'erase))
             (move (nref 'move))
             (refresh (nref 'refresh))
             (cursor-shape (nref 'cursor-shape)))
          (with-mutex (tui-render-mutex st)
                      (catch #t
                        (lambda ()
                          (let*
                              ((maxy (getmaxy stdscr)) (maxx (getmaxx stdscr))
                               (running? (tui-agent-running? st))
                               (avail-width (max 1 (- maxx 3)))
                               (input-layout
                                 (call-with-values
                                   (lambda ()
                                     (input-wrapped-rows (tui-input-buffer st)
                                                         (tui-cursor-pos st)
                                                         avail-width
                                                         %max-input-rows))
                                   list))
                               (input-lines (car input-layout))
                               (input-cur-row (cadr input-layout))
                               (input-cur-col (caddr input-layout))
                               (input-rows (length input-lines))
                               (box-style
                                 (if (< maxy 14)
                                   'tint
                                   'lines))
                               (box-overhead
                                 (cond
                                   (running? 1)
                                   ((eq? box-style 'tint) input-rows)
                                   (else (+ 2 input-rows))))
                               (box-top
                                 (if running?
                                   (- maxy 3)
                                   (- maxy 2 box-overhead)))
                               (prompt-top
                                 (if running?
                                   (- maxy 3)
                                   (+ box-top
                                      (if (eq? box-style 'tint)
                                        0
                                        1))))
                               (view-top 0)
                               (view-bottom (max (+ view-top 1) (- box-top 1)))
                               (view-height (max 1 (- view-bottom view-top)))
                               (content-width (max 10 (- maxx 3)))
                               (popup-anchor-y
                                 (if running?
                                   prompt-top
                                   box-top)))
                            (hash-clear! (tui-click-targets st))
                            (hash-clear! (tui-row-texts st))
                            (let
                                ((tab-sig
                                   (list (tui-status-text st)
                                         (tui-cur-model st)
                                         (and (spinner-active-status? (tui-status-text st))
                                              (streaming-spinner-glyph)))))
                              (unless
                                  (equal? tab-sig (tui-last-tab-title-sig st))
                                (set-tui-last-tab-title-sig! st tab-sig)
                                (tui-safe-write-osc-title!
                                  (format-osc-tab-title (tui-status-text st) #:model (tui-cur-model st)))))
                            (erase stdscr)
                            (render-transcript! st view-top view-bottom view-height content-width)
                            (if running?
                              (render-running-steering-bar! st
                                                            maxy
                                                            maxx
                                                            view-bottom
                                                            prompt-top
                                                            avail-width
                                                            popup-anchor-y)
                              (render-idle-input! st
                                                  maxy
                                                  maxx
                                                  view-bottom
                                                  box-top
                                                  box-style
                                                  input-lines
                                                  input-cur-row
                                                  input-cur-col
                                                  popup-anchor-y))
                            (render-bottom-status-bar! st maxy maxx running?)
                            (let ((ov (tui-active-overlay st)))
                              (when ov
                                (let
                                    ((draw-ctx
                                       `((draw-popup! .
                                                      ,(lambda
                                                           (title rows sel-pos box-w start-y start-x above below)
                                                         (tui-draw-popup! st title rows sel-pos box-w start-y start-x above below)))
                                         (draw-ops! .
                                                    ,(lambda (ops _rect)
                                                       (for-each
                                                         (lambda (op)
                                                           (when (and (>= (draw-op-y op) 0)
                                                                      (< (draw-op-y op) maxy)
                                                                      (>= (draw-op-x op) 0)
                                                                      (< (draw-op-x op) maxx))
                                                             (move stdscr (draw-op-y op) (draw-op-x op))
                                                             (addstr stdscr (draw-op-text op))))
                                                         ops)))
                                         (register-click! .
                                                          ,(lambda (y target)
                                                             (hashq-set! (tui-click-targets st) y target))))))
                                  ((overlay-render ov) ov draw-ctx maxy maxx popup-anchor-y))))
                            (unless running?
                              (move stdscr (+ prompt-top input-cur-row) (+ 2 input-cur-col))
                              (cond
                                ((agent-run-active? (tui-status-text st)) (tui-safe-curs-set! st 0))
                                ((integer? (tui-msgsel-index st))
                                  (tui-safe-curs-set! st 1)
                                  (cursor-shape 'block))
                                ((memq (tui-prompt-mode st) '(shell scheme))
                                  (tui-safe-curs-set! st 1)
                                  (cursor-shape 'underline))
                                (else (tui-safe-curs-set! st 1) (cursor-shape 'bar))))
                            (refresh stdscr)))
                        (lambda (key . args)
                          (let ((sig (format #f "~s ~s" key args)))
                            (unless
                                (equal? sig (tui-last-render-error-sig st))
                              (set-tui-last-render-error-sig! st sig)
                              (tui-add-err-msg! st (format #f "Render error (~a): ~s" key args)))))))))

(define (tui-request-render! st)
        "Render immediately when idle, or mark render-dirty? when the agent thread is running."
        (if (tui-agent-running? st)
          (set-tui-render-dirty?! st #t)
          (tui-render! st)))