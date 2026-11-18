(define-module (test surfaces-widgets-spec)
               #:use-module (badb spec)
               #:use-module (srfi srfi-64)
               #:use-module (geasa surfaces-widgets plugin))

(spec-begin "surfaces-widgets")

(test-group "rect-and-draw-ops"
            (let ((r (make-rect 2 3 -5 4)))
              (test-assert "rect?" (rect? r))
              (test-equal "rect-x" 2 (rect-x r))
              (test-equal "rect-y" 3 (rect-y r))
              (test-equal "rect-width clamped" 0 (rect-width r))
              (test-equal "rect-height" 4 (rect-height r)))
            (let* ((ops (list (make-draw-op 1 0 "hi" #:role 'accent #:bold? #t)
                              (make-draw-op 0 1 "row1")))
                   (lines (render-ops->lines ops 5 2))
                   (segs (draw-ops->segments-by-row ops 5 2)))
              (test-equal "render-ops->lines" '(" hi  " "row1 ") lines)
              (test-equal "segments row count" 2 (length segs))
              (test-equal "row 0 has gap + accent" 2 (length (car segs)))))

(test-group "core-widgets"
            (let ((lines (render-widget-to-lines
                           (box-widget (vstack-widget (list (text-widget "hello")
                                                            (pill-widget "OK")))
                                       #:title "Box")
                           12 4)))
              (test-equal "box-widget 4 lines" 4 (length lines))
              (test-assert "box title present" (string-contains (car lines) "Box"))
              (test-assert "text child present" (string-contains (cadr lines) "hello"))
              (test-assert "pill child present" (string-contains (caddr lines) "[OK]")))
            (let ((hlines (render-widget-to-lines
                            (hstack-widget (list (text-widget "A") (text-widget "B")) #:gap 2)
                            6 1)))
              (test-equal "hstack-widget" '("A  B  ") hlines))
            (let ((slines (render-widget-to-lines
                            (scroll-view-widget
                              (vstack-widget (list (text-widget "L0")
                                                   (text-widget "L1")
                                                   (text-widget "L2")))
                              #:offset 1)
                            4 2)))
              (test-equal "scroll-view-widget" '("L1  " "L2  ") slines)))

(test-group "selectable-list"
            (let* ((highlighted '())
                   (sl (make-selectable-list '("a" "b" "c" "d" "e")
                                             #:max-visible 3
                                             #:on-highlight (lambda (idx item)
                                                              (set! highlighted (cons idx item))))))
              (test-equal "initial selected" "a" (list-selected-item sl))
              (let ((sl1 (list-move sl 3)))
                (test-equal "moved index" 3 (selectable-list-selected-index sl1))
                (test-equal "moved item" "d" (list-selected-item sl1))
                (test-equal "on-highlight fired" '(3 . "d") highlighted)
                (call-with-values (lambda () (list-visible-slice sl1))
                                  (lambda (vis off)
                                    (test-equal "visible slice" '("b" "c" "d") vis)
                                    (test-equal "scroll offset" 1 off)))
                (let ((sl2 (list-move sl1 10)))
                  (test-equal "clamped at end" 4 (selectable-list-selected-index sl2)))
                (let ((sl3 (list-move sl1 -10)))
                  (test-equal "clamped at start" 0 (selectable-list-selected-index sl3)))))
            (let ((wlines (render-widget-to-lines
                            (selectable-list-widget (make-selectable-list '("one" "two") #:selected-index 1))
                            10 2)))
              (test-equal "selectable-list-widget row 0" "  one     " (car wlines))
              (test-equal "selectable-list-widget row 1" "▸ two     " (cadr wlines))))

(test-group "tab-bar-status-badge-toggle-key-hints"
            (let* ((clicked #f)
                   (tb (tab-bar-widget '("All" "Active")
                                       #:active 0
                                       #:counts '(3 1)
                                       #:on-select (lambda (idx) (set! clicked idx))))
                   (tlines (render-widget-to-lines tb 24 1)))
              (test-assert "tab-bar renders counts" (string-contains (car tlines) "All (3)"))
              (widget-dispatch-event tb '(click 12 0))
              (test-equal "tab-bar click hit-tests second tab" 1 clicked))
            (let ((b-ok (render-widget-to-lines (status-badge-widget 'ok "Ready") 10 1))
                  (b-err (render-widget-to-lines (status-badge-widget 'error "Fail") 10 1)))
              (test-assert "status-badge ok" (string-contains (car b-ok) "✓ Ready"))
              (test-assert "status-badge error" (string-contains (car b-err) "✗ Fail")))
            (let* ((toggled #f)
                   (tg-unlocked (toggle-widget #t #:on-toggle (lambda (v) (set! toggled v))))
                   (tg-locked (toggle-widget #t #:locked? #t #:on-toggle (lambda (v) (set! toggled v)))))
              (test-assert "toggle renders ON" (string-contains (car (render-widget-to-lines tg-unlocked 10 1)) "[● ON]"))
              (test-assert "locked toggle renders LOCKED" (string-contains (car (render-widget-to-lines tg-locked 16 1)) "LOCKED"))
              (widget-dispatch-event tg-locked 'toggle)
              (test-equal "locked toggle ignores event" #f toggled)
              (widget-dispatch-event tg-unlocked 'toggle)
              (test-equal "unlocked toggle fires" #f toggled))
            (let ((kh (render-widget-to-lines
                        (key-hints-widget '(("↑↓" . "move") ("Esc" . "close")))
                        28 1)))
              (test-assert "key-hints renders" (string-contains (car kh) "[↑↓] move"))
              (test-assert "key-hints renders second" (string-contains (car kh) "[Esc] close"))))

(test-group "table-widget"
            (let*
                ((cols '(((header . "State") (width . 10)) ((header . "Name") (width . 10))))
                 (rows
                   (list
                     (list (toggle-widget #t #:locked? #t) "core-mod")
                     (list (toggle-widget #f) "user-mod")))
                 (tbl (table-widget cols rows #:selected-index 1 #:max-visible 5))
                 (lines (render-widget-to-lines tbl 24 4)))
              (test-equal "table renders header + 2 rows" 4 (length lines))
              (test-assert "header has aligned State and Name"
                           (and (string-contains (car lines) "State") (string-contains (car lines) "Name")))
              (test-equal
                "columns stay aligned across locked and unlocked rows"
                (string-contains (cadr lines) "core-mod")
                (string-contains (caddr lines) "user-mod"))
              (test-assert "selected row has indicator" (string-prefix? "▸ " (caddr lines)))))

(test-group "filter-state-and-input"
            (let* ((fs0 (make-filter-state #:active? #t #:placeholder "search"))
                   (fs1 (filter-handle-key fs0 #\a))
                   (fs2 (filter-handle-key fs1 #\b))
                   (fs3 (filter-handle-key fs2 'backspace))
                   (fs4 (filter-handle-key fs3 'enter))
                   (fs5 (filter-handle-key fs4 'esc)))
              (test-assert "filter-state?" (filter-state? fs0))
              (test-equal "typed ab" "ab" (filter-state-query fs2))
              (test-equal "cursor at 2" 2 (filter-state-cursor fs2))
              (test-equal "backspace -> a" "a" (filter-state-query fs3))
              (test-equal "enter locks query" "a" (filter-state-query fs4))
              (test-equal "enter deactivates" #f (filter-state-active? fs4))
              (test-equal "esc clears query" "" (filter-state-query fs5))
              (test-equal "esc deactivates" #f (filter-state-active? fs5))
              (set-filter-state-active! fs5 #t)
              (test-assert "set-filter-state-active!" (filter-state-active? fs5))
              (let ((active-line (car (render-widget-to-lines
                                        (filter-input-widget fs2) 20 1)))
                    (ph-line (car (render-widget-to-lines
                                    (filter-input-widget (make-filter-state #:placeholder "search")) 20 1))))
                (test-assert "active filter renders / ab▏" (string-contains active-line "/ ab▏"))
                (test-assert "inactive empty renders placeholder" (string-contains ph-line "search")))))

(test-group "fuzzy-matcher-and-filter-list"
            (let ((s-exact (fuzzy-subsequence-score "tui" "tui"))
                  (s-boundary (fuzzy-subsequence-score "tui" "surfaces-tui"))
                  (s-miss (fuzzy-subsequence-score "xyz" "surfaces-tui"))
                  (s-empty (fuzzy-subsequence-score "" "anything")))
              (test-assert "exact match > boundary" (> s-exact s-boundary))
              (test-equal "non-subsequence is #f" #f s-miss)
              (test-equal "empty query scores 0" 0 s-empty))
            (let* ((sl (make-selectable-list '("alpha-core" "surfaces-tui" "surfaces-widgets" "beta")
                                             #:selected-index 2))
                   (same (filter-selectable-list sl "" (lambda (x) x)))
                   (filtered (filter-selectable-list sl "tui" (lambda (x) x))))
              (test-equal "empty query preserves items"
                          '("alpha-core" "surfaces-tui" "surfaces-widgets" "beta")
                          (selectable-list-items same))
              (test-equal "empty query preserves selection" 2 (selectable-list-selected-index same))
              (test-equal "filtered matches tui" '("surfaces-tui") (selectable-list-items filtered))
              (test-equal "filtered resets selection" 0 (selectable-list-selected-index filtered))))

(test-group "spinner-progress-and-meter"
            (test-equal "braille frames count" 10 (length %braille-spinner-frames))
            (test-equal "ascii frames count" 4 (length %ascii-spinner-frames))
            (let ((sp-b (car (render-widget-to-lines (spinner-widget 0 #:label "Loading") 16 1)))
                  (sp-a (car (render-widget-to-lines (spinner-widget 1 #:ascii? #t) 4 1))))
              (test-assert "braille spinner renders" (string-contains sp-b "⠋ Loading"))
              (test-assert "ascii spinner renders /" (string-contains sp-a "/")))
            (let* ((pb-low (progress-bar-widget -0.5 #:width 10 #:filled-char "#" #:empty-char "."))
                   (pb-high (progress-bar-widget 1.5 #:width 10 #:filled-char "#" #:empty-char "."))
                   (pb-mid (progress-bar-widget 0.5 #:width 10 #:filled-char "#" #:empty-char ".")))
              (test-equal "clamps below 0" ".........." (car (render-widget-to-lines pb-low 10 1)))
              (test-equal "clamps above 1" "##########" (car (render-widget-to-lines pb-high 10 1)))
              (test-equal "50% bar" "#####....." (car (render-widget-to-lines pb-mid 10 1))))
            (let ((m-ok (meter-widget 50 100 #:width 10))
                  (m-warn (meter-widget 80 100 #:width 10))
                  (m-err (meter-widget 95 100 #:width 10 #:unit "tok")))
              (test-equal "meter ok role" 'ok (assq-ref (widget-props m-ok) 'role))
              (test-equal "meter warn role" 'warn (assq-ref (widget-props m-warn) 'role))
              (test-equal "meter error role" 'error (assq-ref (widget-props m-err) 'role))
              (test-assert "meter renders percent and unit"
                           (string-contains (car (render-widget-to-lines m-err 30 1)) "95% (95/100 tok)"))))

(spec-end)
