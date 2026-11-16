;;; test/test-ui-wp1.scm — SRFI-64 unit tests for WP1 (widgets & UI protocol)

(define-module (test test-ui-wp1)
               #:use-module (geasa surfaces-tui widgets)
               #:use-module (geasa surfaces-tui protocol)
               #:use-module (ice-9 match)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-64))

(test-begin "ui-wp1")

;;;
;;; ui-wp1.1: Composable Widget System
;;;
(test-group "wp1.1-widget-system"
            (let* ((txt (text-widget "hello world" #:role 'accent #:bold? #t))
                   (sz (widget-measure-size txt 40 10))
                   (lines (render-widget-to-lines txt 15 2)))
              (test-assert "widget? identifies widget" (widget? txt))
              (test-equal "text-widget measures natural width and 1 line" '(11 . 1) sz)
              (test-equal "text-widget renders first line" "hello world    " (car lines)))

            (let* ((pill (pill-widget "READY" #:role 'success))
                   (rule (rule-widget #:label "sec"))
                   (boxed (box-widget (vstack-widget (list pill rule))
                                      #:title "Panel"
                                      #:rounded? #t))
                   (lines (render-widget-to-lines boxed 20 5)))
              (test-equal "box-widget top border includes title"
                          "╭─ Panel ──────────╮"
                          (list-ref lines 0))
              (test-assert "box-widget renders pill inside"
                           (string-contains (list-ref lines 1) "[READY]"))
              (test-assert "box-widget renders labeled rule inside"
                           (string-contains (list-ref lines 2) "sec"))
              (test-equal "box-widget bottom border"
                          "╰──────────────────╯"
                          (list-ref lines 4)))

            (let* ((hs (hstack-widget (list (pill-widget "A") (pill-widget "B")) #:gap 2))
                   (lines (render-widget-to-lines hs 12 1)))
              (test-equal "hstack-widget places children with gap"
                          "[A]  [B]    "
                          (car lines)))

            (let* ((sv (scroll-view-widget '("L1" "L2" "L3" "L4" "L5")
                                           #:scroll-offset 1
                                           #:show-scrollbar? #t))
                   (lines (render-widget-to-lines sv 10 3)))
              (test-assert "scroll-view shows scrolled window L2..L4"
                           (and (string-prefix? "L2" (list-ref lines 0))
                                (string-prefix? "L3" (list-ref lines 1))
                                (string-prefix? "L4" (list-ref lines 2))))
              (test-assert "scroll-view draws scrollbar on right edge"
                           (every (lambda (l)
                                    (let ((c (string-ref l 9)))
                                      (or (char=? c #\│) (char=? c #\┃))))
                                  lines)))

            (let* ((clicked? #f)
                   (child (make-widget 'btn
                                       #:on-event (lambda (_w ev)
                                                    (when (eq? ev 'click)
                                                      (set! clicked? #t)
                                                      'handled))))
                   (parent (box-widget child)))
              (test-equal "widget-dispatch-event bubbles to child"
                          'handled
                          (widget-dispatch-event parent 'click))
              (test-assert "event handler ran" clicked?)))

;;;
;;; ui-wp1.2: UI Server / Client Protocol
;;;
(test-group "wp1.2-server-client-protocol"
            (let* ((op (make-draw-op 3 1 "hi" #:role 'accent #:bold? #t))
                   (msg (make-ui-message 'frame
                                         #:seq 7
                                         #:source 'server
                                         #:payload `((cols . 80)
                                                     (rows . 24)
                                                     (ops . (,op)))))
                   (wire (encode-ui-message msg))
                   (decoded (decode-ui-message wire)))
              (test-assert "encode-ui-message returns single-line JSON"
                           (and (string? wire) (not (string-index wire #\newline))))
              (test-assert "decode-ui-message reconstructs <ui-message>" (ui-message? decoded))
              (test-equal "round-trip message type" 'frame (ui-message-type decoded))
              (test-equal "round-trip seq" 7 (ui-message-seq decoded))
              (test-equal "decode-ui-message rejects garbage" #f (decode-ui-message "{not-json")))

            ;; Port streaming framing
            (let* ((msg (make-ui-message 'submit #:seq 1 #:source 'ssh-1 #:payload '((text . "/help"))))
                   (wire-str (call-with-output-string (lambda (p) (write-ui-frame! p msg))))
                   (read-back (call-with-input-string wire-str read-ui-frame)))
              (test-assert "read-ui-frame decodes port frame" (ui-message? read-back))
              (test-equal "port frame type preserved" 'submit (ui-message-type read-back)))

            ;; Server + Client pump and subagent event routing
            (let ((srv (make-ui-server))
                  (cli (make-ui-client #:id 'ssh-client #:cols 100 #:rows 30))
                  (submitted '()))
              (ui-server-on-event! srv 'submit
                                   (lambda (s m)
                                     (set! submitted (cons (assoc-ref (ui-message-payload m) 'text)
                                                           submitted))
                                     (ui-server-emit! s 'notify '((level . info) (text . "ack")))))
              (pump-ui-pair! srv cli
                             (list (cons 'attach '((cols . 100) (rows . 30)))
                                   (cons 'submit '((text . "hello")))
                                   (cons 'ping '())))
              (ui-server-route-subagent-event! srv "sub-1" "root-0" 'tool-call "read_file"
                                               #:iteration 2)
              (let ((frames (ui-server-drain-frames! srv)))
                (for-each (lambda (f) (ui-client-apply-frame! cli f)) frames))
              (let ((snap (ui-client-render-snapshot cli)))
                (test-assert "client marked connected" (assoc-ref snap 'connected?))
                (test-equal "server handler received submit" '("hello") submitted)
                (test-equal "client recorded notification"
                            1
                            (length (assoc-ref snap 'notifications)))
                (test-equal "client received routed subagent event"
                            1
                            (length (assoc-ref snap 'subagent-events))))))

;;;
;;; ui-wp1.3: Input Box Demarcation
;;;
(test-group "wp1.3-input-box-demarcation"
            (let ((rb (format-input-box-frame '("hello") 30 #:style 'rounded-box #:cursor-col 5))
                  (ln (format-input-box-frame '("hello") 30 #:style 'lines #:cursor-col 5))
                  (tn (format-input-box-frame '("hello") 30 #:style 'tint #:cursor-col 5))
                  (narrow (format-input-box-frame '("hi") 12 #:style 'rounded-box)))
              (test-equal "rounded-box height for 1 line is 3" 3 (assoc-ref rb 'height))
              (test-equal "rounded-box cursor screen position"
                          '(1 . 7)
                          (cons (assoc-ref rb 'cursor-screen-row)
                                (assoc-ref rb 'cursor-screen-col)))
              (test-equal "lines style height for 1 line is 3" 3 (assoc-ref ln 'height))
              (test-equal "tint style height for 1 line is 1" 1 (assoc-ref tn 'height))
              (test-equal "narrow width <16 falls back to lines" 'lines (assoc-ref narrow 'style))
              (test-equal "input-box-widget renders 3 lines"
                          3
                          (length (render-widget-to-lines (input-box-widget '("abc") #:style 'rounded-box) 24 3)))))

;;;
;;; ui-wp1.4: Bottom Status Bar & Animated Hooded Crow Startup Banner
;;;
(test-group "wp1.4-status-bar-and-crow-banner"
            (let* ((segs (format-bottom-status-bar 90
                                                   #:mode 'chat
                                                   #:model "qwen2.5-coder"
                                                   #:status "READY"
                                                   #:cwd "/home/cianh/Projects/badb"
                                                   #:git-branch "wp1-arch"
                                                   #:tokens-used 12000
                                                   #:tokens-limit 128000
                                                   #:subagent-count 2
                                                   #:todo-summary "2/5"))
                   (narrow (format-bottom-status-bar 18 #:mode 'shell #:model "very-long-model-name")))
              (test-assert "status bar produces segments" (pair? segs))
              (test-assert "all wide status segments fit within 90 cols"
                           (every (lambda (s) (<= (+ (car s) (string-length (cadr s))) 90)) segs))
              (test-assert "all narrow status segments fit within 18 cols"
                           (every (lambda (s) (<= (+ (car s) (string-length (cadr s))) 18)) narrow)))

            (test-assert "%crow-banner-frames is a vector of >= 3 frames"
                         (and (vector? %crow-banner-frames)
                              (>= (crow-banner-frame-count) 3)))
            (let ((f0 (format-startup-banner-text 70 #:frame 0 #:version "0.1.0" #:model "test-model"))
                  (f2 (format-startup-banner-text 70 #:frame 2 #:version "0.1.0" #:model "test-model"))
                  (compact (format-startup-banner-text 30 #:frame 0 #:version "0.1.0")))
              (test-assert "startup banner includes badb version" (string-contains f0 "badb v0.1.0"))
              (test-assert "animated frames differ across animation indices" (not (string=? f0 f2)))
              (test-assert "compact startup banner still includes badb" (string-contains compact "badb"))))

;;;
;;; ui-wp1.5: Git Diff Viewer, Todo Widget & Live Scheme Response Widgets
;;;
(test-group "wp1.5-diff-todo-scheme-widgets"
            (let* ((sample-diff
                     (string-append
                       "diff --git a/foo.scm b/foo.scm\n"
                       "--- a/foo.scm\n"
                       "+++ b/foo.scm\n"
                       "@@ -10,3 +10,3 @@\n"
                       " (define a 1)\n"
                       "-(define b 2)\n"
                       "+(define b 42)\n"))
                   (parsed (parse-git-diff-lines sample-diff))
                   (rows (format-git-diff-view sample-diff 60 #:title "foo.scm"))
                   (del-entry (find (lambda (e) (eq? (car e) 'del)) parsed))
                   (add-entry (find (lambda (e) (eq? (car e) 'add)) parsed)))
              (test-equal "deleted line tracks old line 11" 11 (cadr del-entry))
              (test-equal "added line tracks new line 11" 11 (caddr add-entry))
              (test-assert "git diff view formats non-empty rows" (> (length rows) 4)))

            (let* ((tl0 (make-todo-list #:title "Sprint"))
                   (tl1 (todo-add tl0 "Write widgets" #:source 'user))
                   (tl2 (todo-add tl1 "Run tests" #:source 'agent #:priority 'high))
                   (tl3 (todo-toggle tl2 1))
                   (tl4 (todo-toggle tl3 '(index . 1)))
                   (tl5 (todo-clear-done tl4))
                   (tl6 (todo-remove tl3 2))
                   (counts (todo-counts tl3))
                   (roundtrip (alist->todo-list (todo-list->alist tl3)))
                   (lines (render-widget-to-lines (todo-widget tl3 #:selected-index 1) 48 4)))
              (test-equal "todo-counts tracks 1 done of 2" '(1 . 2) counts)
              (test-equal "todo-toggle by index marks second done" '(2 . 2) (todo-counts tl4))
              (test-equal "todo-clear-done empties all done items" '(0 . 0) (todo-counts tl5))
              (test-equal "todo-remove drops item 2" '(1 . 1) (todo-counts tl6))
              (test-equal "todo-list round-trips via alist" '(1 . 2) (todo-counts roundtrip))
              (test-assert "todo widget renders checkmark and agent tag"
                           (and (string-contains (list-ref lines 1) "[✓]")
                                (string-contains (list-ref lines 2) "[agent]"))))

            (let* ((md (string-append
                         "Here is status:\n"
                         "```scheme-widget\n"
                         "(box \"Build\" (vstack (pill \"PASS\" #:role 'success) (progress 3 4 #:label \"Steps\")))\n"
                         "```\n"
                         "And KV:\n"
                         "```widget\n"
                         "(kv (\"Model\" . \"qwen\") (\"State\" . \"ok\"))\n"
                         "```\nDone."))
                   (blocks (extract-response-widgets md))
                   (ok-res (render-scheme-response-widget (car blocks) 40))
                   (kv-res (render-scheme-response-widget (cadr blocks) 40))
                   (unsafe-res (render-scheme-response-widget "(system \"echo pwned\")" 40)))
              (test-equal "extracts 2 widget blocks" 2 (length blocks))
              (test-assert "valid declarative widget succeeds" (assoc-ref ok-res 'ok?))
              (test-assert "kv declarative widget succeeds"
                           (and (assoc-ref kv-res 'ok?)
                                (string-contains (string-join (assoc-ref kv-res 'lines) "\n") "Model:")))
              (test-assert "rendered lines include Build and PASS"
                           (let ((joined (string-join (assoc-ref ok-res 'lines) "\n")))
                             (and (string-contains joined "Build")
                                  (string-contains joined "[PASS]"))))
              (test-assert "unsafe scheme form is rejected safely"
                           (and (not (assoc-ref unsafe-res 'ok?))
                                (string? (assoc-ref unsafe-res 'error))))))

(test-end "ui-wp1")
