;;; test/test-ui-markdown.scm — SRFI-64 tests for markdown, transcript, thinking collapse, spinner/OSC, protocols

(define-module (test test-ui-markdown)
               #:use-module
               (geasa surfaces-tui markdown)
               #:use-module
               (geasa surfaces-tui transcript)
               #:use-module
               (ice-9 format)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-64))

(define %standalone?
        (not (test-runner-current)))

(when %standalone?
  (let ((m (resolve-module '(srfi srfi-64))))
    (when (module-bound? m 'test-log-to-file) (module-set! m 'test-log-to-file #f)))
  (test-runner-current (test-runner-simple)))

(test-begin "ui-markdown-and-transcript")

;;;
;;; ui-wp2.1: Markdown, Backticks, Live Input Formatting & mdflow Fallback
;;;
(test-group "markdown-and-mdflow"
            ;; mdflow missing in PATH -> pure-Scheme fallback works seamlessly
            (test-assert
              "mdflow-binary-path returns #f for nonexistent binary"
              (not (mdflow-binary-path "/nonexistent/path/to/mdflow-missing")))
            (test-assert
              "mdflow-available? is #f for nonexistent override"
              (not (mdflow-available? "/nonexistent/path/to/mdflow-missing")))
            ;; Mock executable mdflow script verification
            (let*
                ((tmp-bin (format #f "/tmp/badb-mock-mdflow-~a.sh" (getpid))))
              (dynamic-wind
                (lambda ()
                  (call-with-output-file tmp-bin
                                         (lambda (p)
                                           (display "#!/bin/sh\necho 'rendered-by-mdflow'\n" p)))
                  (chmod tmp-bin #o755))
                (lambda ()
                  (test-equal
                    "mdflow-binary-path resolves executable override"
                    tmp-bin
                    (mdflow-binary-path tmp-bin))
                  (test-assert
                    "mdflow-available? returns #t for executable override"
                    (mdflow-available? tmp-bin))
                  (test-equal
                    "render-markdown-with-mdflow captures mock output and strips ANSI"
                    '("rendered-by-mdflow")
                    (render-markdown-with-mdflow "# hello" #:width 60 #:bin tmp-bin)))
                (lambda ()
                  (when (file-exists? tmp-bin) (delete-file tmp-bin)))))
            ;; Block parser: headings, fences (closed + unclosed), quotes, lists, hr
            (let
                ((blocks
                   (parse-markdown-blocks
                     "# Title\n\n> quoted\n\n- [x] done\n- item\n\n---\n\n```scheme\n(+ 1 2)\n```")))
              (test-assert "parses heading block"
                           (any
                             (lambda (b)
                               (and (eq? (car b) 'heading) (= (cadr b) 1)))
                             blocks))
              (test-assert "parses blockquote block"
                           (any
                             (lambda (b)
                               (eq? (car b) 'blockquote))
                             blocks))
              (test-assert "parses list-item blocks"
                           (= 2
                              (length
                                (filter
                                  (lambda (b)
                                    (eq? (car b) 'list-item))
                                  blocks))))
              (test-assert "parses hr block"
                           (any
                             (lambda (b)
                               (eq? (car b) 'hr))
                             blocks))
              (test-assert "parses scheme fenced code block"
                           (any
                             (lambda (b)
                               (and (eq? (car b) 'code-block) (string=? (cadr b) "scheme")))
                             blocks)))
            (let
                ((unclosed (parse-markdown-blocks "```sh\necho hi")))
              (test-assert
                "unclosed streaming fence still yields code-block"
                (any
                  (lambda (b)
                    (and (eq? (car b) 'code-block) (string=? (cadr b) "sh")))
                  unclosed)))
            ;; Inline backticks & formatting
            (let
                ((segs (segment-markdown-inline "Run `git status` and ``a `b` c`` **now**")))
              (test-assert
                "inline single backtick uses accent role"
                (any
                  (lambda (s)
                    (and (string=? (car s) "git status") (eq? (cadr s) 'accent)))
                  segs))
              (test-assert
                "inline double backtick preserves inner single backtick"
                (any
                  (lambda (s)
                    (and (string=? (car s) "a `b` c") (eq? (cadr s) 'accent)))
                  segs))
              (test-assert "bold text sets bold? flag"
                           (any
                             (lambda (s)
                               (and (string=? (car s) "now") (caddr s)))
                             segs)))
            ;; Markdown rendering to lines
            (let
                ((lines
                   (render-markdown-lines "# Heading\nUse `x` here." #:width 50 #:use-mdflow? #f)))
              (test-assert
                "render-markdown-lines produces heading prefix"
                (any
                  (lambda (l)
                    (string-contains l "Heading"))
                  lines))
              (test-assert
                "render-markdown-lines renders inline backtick text"
                (any
                  (lambda (l)
                    (string-contains l "Use x here."))
                  lines)))
            ;; Live input formatting: strict cursor-length preservation invariant
            (for-each
              (lambda (pair)
                (let*
                    ((mode (car pair)) (input (cdr pair))
                     (segs (segment-input-line input #:prompt-mode mode))
                     (rejoined (string-concatenate (map car segs))))
                  (test-equal
                    (format #f "segment-input-line (~a) preserves text verbatim" mode)
                    input
                    rejoined)))
              '((chat . "Check `foo` and **bar** @alice https://example.org")
                (shell . "sudo git commit --amend | grep 'ok' $HOME")
                (scheme . "(define (f x) (+ x 1))")
                (slash . "/theme tokyo-night"))))

;;;
;;; ui-wp2.2: Syntax Highlighting & Message Card Demarcation
;;;
(test-group "syntax-and-demarcation"
            (let
                ((shell-segs (highlight-shell-line "sudo ls -la | grep foo $HOME # end")))
              (test-equal
                "shell highlighter preserves text verbatim"
                "sudo ls -la | grep foo $HOME # end"
                (string-concatenate (map car shell-segs)))
              (test-assert
                "shell highlighter marks command in shell role"
                (any
                  (lambda (s)
                    (and (string=? (car s) "sudo") (eq? (cadr s) 'shell)))
                  shell-segs))
              (test-assert
                "shell highlighter marks pipe in warning role"
                (any
                  (lambda (s)
                    (and (string=? (car s) "|") (eq? (cadr s) 'warning)))
                  shell-segs))
              (test-assert
                "shell highlighter marks env var in notice role"
                (any
                  (lambda (s)
                    (and (string=? (car s) "$HOME") (eq? (cadr s) 'notice)))
                  shell-segs))
              (test-assert
                "shell highlighter marks comment in dim role"
                (any
                  (lambda (s)
                    (and (string-contains (car s) "# end") (eq? (cadr s) 'dim)))
                  shell-segs)))
            (let
                ((scm-segs (highlight-scheme-line "(define x \"hello\") ; note")))
              (test-equal
                "scheme highlighter preserves text verbatim"
                "(define x \"hello\") ; note"
                (string-concatenate (map car scm-segs)))
              (test-assert
                "scheme highlighter marks string in accent role"
                (any
                  (lambda (s)
                    (and (string-contains (car s) "\"hello\"") (eq? (cadr s) 'accent)))
                  scm-segs)))
            ;; format-message-entry attaches syntax segments for shell-cmd & scheme-in
            (let*
                ((shell-rows
                   (format-message-entry
                     '((role . "shell-cmd") (content . "git status --short"))
                     60))
                 (body
                   (find
                     (lambda (r)
                       (string=? (car r) "block-body"))
                     shell-rows))
                 (segs (block-row-segments body)))
              (test-assert
                "shell-cmd body row has pre-computed segments"
                (>= (length body) 4))
              (test-assert
                "shell-cmd segments highlight git command"
                (any
                  (lambda (s)
                    (and (string=? (car s) "git") (eq? (cadr s) 'shell)))
                  segs)))
            (let*
                ((scm-rows
                   (format-message-entry '((role . "scheme-in") (content . "(define a \"b\")")) 60))
                 (body
                   (find
                     (lambda (r)
                       (string=? (car r) "block-body"))
                     scm-rows))
                 (segs (block-row-segments body)))
              (test-assert
                "scheme-in body row has pre-computed segments"
                (>= (length body) 4))
              (test-assert
                "scheme-in segments highlight string literal"
                (any
                  (lambda (s)
                    (and (string-contains (car s) "\"b\"") (eq? (cadr s) 'accent)))
                  segs)))
            ;; segment-line strips [CODE:role]...[/CODE] wrappers cleanly
            (let
                ((segs (segment-line "x [CODE:scheme]define[/CODE] y")))
              (test-assert
                "segment-line omits literal [CODE:] tags"
                (not
                  (any
                    (lambda (s)
                      (string-contains (car s) "[CODE:"))
                    segs)))
              (test-assert
                "segment-line preserves inner code segment with scheme role"
                (any
                  (lambda (s)
                    (and (string=? (car s) "define") (eq? (cadr s) 'scheme)))
                  segs)))
            ;; Card demarcation helpers
            (test-equal "card header width matches target"
                        30
                        (string-length (format-card-header "assistant" 30 #:icon "◆" #:nerd-fonts? #f)))
            (test-equal "card divider length matches width"
                        20
                        (string-length (format-card-divider 20 'dashed)))
            (test-equal "thinking role label normalizes"
                        "thinking"
                        (message-role-label "thinking"))
            (test-equal "thinking role maps to thought theme"
                        'thought
                        (message-role->theme-role "thinking")))

;;;
;;; ui-wp2.3: Expanding Thinking Cards Bug Fix
;;;
(test-group "thinking-collapse"
            (let*
                ((raw
                   '((role . "assistant")
                     (thoughts . "line 1 of reasoning\nline 2 of reasoning")
                     (content . "final answer")))
                 (collapsed (default-collapse-message raw))
                 (expanded (message-toggle-collapsed collapsed))
                 (recollapsed (message-toggle-collapsed expanded))
                 (cache (make-row-cache)))
              (test-assert
                "assistant with thoughts collapses by default"
                (message-collapsed? collapsed))
              (test-assert "toggling collapsed card expands it"
                           (not (message-collapsed? expanded)))
              (test-assert
                "default-collapse-message does NOT re-collapse explicitly expanded card"
                (not (message-collapsed? (default-collapse-message expanded))))
              (test-assert
                "toggling expanded card collapses it again"
                (message-collapsed? recollapsed))
              (let
                  ((c-rows (row-cache-rows cache collapsed 60))
                   (e-rows (row-cache-rows cache expanded 60)))
                (test-assert
                  "collapsed thoughts render block-summary row"
                  (any
                    (lambda (r)
                      (string=? (car r) "block-summary"))
                    c-rows))
                (test-assert
                  "collapsed thoughts hide thought body rows"
                  (not
                    (any
                      (lambda (r)
                        (and (string=? (car r) "block-body") (eq? (cadr r) 'thought)))
                      c-rows)))
                (test-assert
                  "expanded thoughts render all reasoning lines"
                  (and
                    (any
                      (lambda (r)
                        (and (string=? (car r) "block-body")
                             (eq? (cadr r) 'thought)
                             (string-contains (caddr r) "line 1")))
                      e-rows)
                    (any
                      (lambda (r)
                        (and (string=? (car r) "block-body")
                             (eq? (cadr r) 'thought)
                             (string-contains (caddr r) "line 2")))
                      e-rows)))
                (test-assert
                  "expanded thoughts have no summary row"
                  (not
                    (any
                      (lambda (r)
                        (string=? (car r) "block-summary"))
                      e-rows))))
              ;; Verify row-cache invalidates if a message alist is mutated in place
              (let*
                  ((mut-msg
                     (list (cons 'role "assistant")
                           (cons 'thoughts "reason A\nreason B")
                           (cons 'content "done")
                           (cons 'collapsed? #t)))
                   (rows-before (row-cache-rows cache mut-msg 60)))
                (set-cdr! (assq 'collapsed? mut-msg) #f)
                (let
                    ((rows-after (row-cache-rows cache mut-msg 60)))
                  (test-assert
                    "in-place collapse mutation invalidates row-cache"
                    (not (eq? rows-before rows-after)))
                  (test-assert
                    "invalidated cache renders expanded thought lines"
                    (any
                      (lambda (r)
                        (and (string=? (car r) "block-body") (eq? (cadr r) 'thought)))
                      rows-after))))
              ;; Explicit cache invalidation & clear
              (let
                  ((before (row-cache-rows cache expanded 60)))
                (row-cache-invalidate! cache expanded)
                (test-assert
                  "row-cache-invalidate! forces re-format"
                  (not (eq? before (row-cache-rows cache expanded 60)))))))

;;;
;;; ui-wp2.4: Working Spinner Continuity & Ghostty OSC 0/2 Tab Title
;;;
(test-group "spinner-and-osc-title"
            (test-assert "REACT is active" (agent-run-active? "REACT [2/10]"))
            (test-assert "TOOL: is active" (agent-run-active? "TOOL: read_file..."))
            (test-assert "TOOL CALL... is active" (agent-run-active? "TOOL CALL..."))
            (test-assert "CALLING ... is active" (agent-run-active? "CALLING bash..."))
            (test-assert "THINKING... is active" (agent-run-active? "THINKING..."))
            (test-assert "SYSTEMONE: is active" (agent-run-active? "SYSTEMONE: check"))
            (test-assert "RATE LIMITED is active"
                         (agent-run-active? "RATE LIMITED (3s)..."))
            (test-assert "READY is not active" (not (agent-run-active? "READY")))
            (test-assert
              "STREAMING... is not steering-mode agent-run-active?"
              (not (agent-run-active? "STREAMING...")))
            (test-assert
              "spinner-active-status? includes STREAMING..."
              (spinner-active-status? "STREAMING..."))
            (test-assert
              "spinner-active-status? includes CALLING..."
              (spinner-active-status? "CALLING grep"))
            (test-assert
              "spinner-active-status? excludes READY"
              (not (spinner-active-status? "READY")))
            (test-equal
              "OSC 2 working tab title formats cleanly"
              "\x1b]2;⠋ badb — CALLING read_file\x07"
              (format-osc-tab-title "CALLING read_file" #:spinner-glyph "⠋"))
            (test-equal "OSC 0 tab title supported"
                        "\x1b]0;⠙ badb — THINKING... · claude\x07"
                        (format-osc-tab-title "THINKING..."
                                              #:osc-code
                                              0
                                              #:spinner-glyph
                                              "⠙"
                                              #:model
                                              "claude"))
            (test-equal "OSC tab title resets when READY"
                        "\x1b]2;badb\x07"
                        (reset-osc-tab-title))
            (test-equal
              "OSC tab title sanitizes control characters"
              "\x1b]2;⠋ badb — CALLING bad\x07"
              (format-osc-tab-title "CALLING \x1b[31mbad\x07" #:spinner-glyph "⠋"))
            ;; WP3 coordination: 'slash prompt mode indicator & keymap hint
            (call-with-values
              (lambda ()
                (get-prompt-indicator "" "READY" 'slash))
              (lambda (glyph role bold?)
                (test-equal "slash prompt-mode returns / indicator" "/" glyph)
                (test-equal "slash prompt-mode uses accent role" 'accent role)
                (test-assert "slash prompt-mode is bold" bold?)))
            (test-assert
              "slash keymap hint mentions tab:complete"
              (string-contains (format-keymap-line 'slash) "tab:complete")))

;;;
;;; ui-wp2.5: OSC 8 Clickable Links, Nerd Fonts & Kitty/ncvisual Image Support
;;;
(test-group "terminal-protocols"
            ;; OSC 8 links
            (test-equal "OSC 8 wraps target and label"
                        "\x1b]8;;https://example.com\x07Example\x1b]8;;\x07"
                        (format-osc8-link "https://example.com" "Example"))
            (test-equal
              "file-path->file-uri normalizes relative path"
              "file:///work/src/a.scm"
              (file-path->file-uri "src/a.scm" "/work"))
            ;; Link extraction & click column hit-testing
            (let*
                ((line
                   "See [docs](https://example.org/doc) or src/geasa/surfaces-tui/transcript.scm:42.")
                 (links (extract-clickable-links line)))
              (test-equal "extracts 2 clickable links" 2 (length links))
              (test-equal "first link is url" 'url (assoc-ref (car links) 'kind))
              (test-equal "second link is file" 'file (assoc-ref (cadr links) 'kind))
              (test-equal "second link extracts line number 42"
                          42
                          (assoc-ref (cadr links) 'line))
              (let
                  ((hit1 (find-link-at-column line 6))
                   (hit2 (find-link-at-column line 45))
                   (miss (find-link-at-column line 0)))
                (test-equal "click hit-test finds markdown link"
                            "https://example.org/doc"
                            (assoc-ref hit1 'target))
                (test-equal "click hit-test finds file path"
                            "src/geasa/surfaces-tui/transcript.scm"
                            (assoc-ref hit2 'target))
                (test-assert "click outside links returns #f" (not miss))))
            ;; Nerd Font toggling & Unicode fallback
            (let ((orig-mode (%nerd-fonts-mode)))
              (dynamic-wind
                (lambda ()
                  #t)
                (lambda ()
                  (set-nerd-fonts-mode! #f)
                  (test-assert
                    "nerd-fonts-enabled? #f when forced off"
                    (not (nerd-fonts-enabled?)))
                  (test-equal "Unicode fallback icon for thinking" "◇" (nerd-icon 'thinking))
                  (set-nerd-fonts-mode! #t)
                  (test-assert "nerd-fonts-enabled? #t when forced on" (nerd-fonts-enabled?))
                  (test-assert
                    "Nerd Font glyph differs from Unicode fallback"
                    (not (string=? (nerd-icon 'thinking #t) (nerd-icon 'thinking #f)))))
                (lambda ()
                  (%nerd-fonts-mode orig-mode))))
            ;; Kitty / ncvisual helpers
            (test-assert
              "image-file-path? recognizes PNG/JPG/WebP"
              (and (image-file-path? "/tmp/chart.png")
                   (image-file-path? "photo.JPEG")
                   (not (image-file-path? "notes.md"))))
            (test-assert
              "format-kitty-image-escape emits APC Kitty graphics sequence"
              (let
                  ((seq (format-kitty-image-escape "/tmp/img.png" #:cols 20 #:rows 10)))
                (and (string-prefix? "\x1b_Ga=T,f=100,t=f" seq)
                     (string-contains seq ",c=20,r=10;")
                     (string-suffix? "\x1b\\" seq))))
            (test-assert
              "format-image-placeholder includes filename"
              (string-contains
                (format-image-placeholder "/tmp/pic.png" #:protocol 'kitty #:nerd-fonts? #f)
                "pic.png"))
            (test-assert
              "render-image-ncvisual returns #f safely without nc context"
              (not (render-image-ncvisual #f #f "/tmp/nonexistent.png"))))

(let ((runner (test-runner-current)))
  (test-end "ui-markdown-and-transcript")
  (when %standalone?
    (exit
      (if (zero? (test-runner-fail-count runner))
        0
        1))))