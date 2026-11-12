;;; test/adversarial-surfaces.scm — Adversarial Stress Test Harness for Surfaces Subsystem
;;;
;;; Validates hostile inputs, failure modes, boundary conditions, and stress limits across:
;;; 1. Root launcher & CLI parsing: scripts/badb, (badb main), (badb surfaces cli)
;;; 2. Theme Subsystem: (geasa surfaces-theme plugin)
;;; 3. TUI State & Engine: (geasa surfaces-tui plugin), (geasa surfaces-tui nc)

(define-module (test adversarial-surfaces)
               #:use-module (srfi srfi-64)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 format)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 optargs)
               #:use-module (badb main)
               #:use-module (geasa foundation-config plugin)
               #:use-module (badb surfaces cli)
               #:use-module (geasa surfaces-theme plugin)
               #:use-module (geasa surfaces-tui plugin))

(test-begin "adversarial-surfaces")

;; Helper to run a command and capture exit code, stdout, and stderr
(define (run-cmd-capture cmd-str)
        (let* ((tmp-out (string-append "/tmp/badb-challenger-out-" (number->string (getpid)) "-" (number->string (random 1000000))))
               (tmp-err (string-append "/tmp/badb-challenger-err-" (number->string (getpid)) "-" (number->string (random 1000000))))
               (full-cmd (string-append cmd-str " > " tmp-out " 2> " tmp-err))
               (status (status:exit-val (system full-cmd)))
               (stdout (if (file-exists? tmp-out)
                         (call-with-input-file tmp-out read-string)
                         ""))
               (stderr (if (file-exists? tmp-err)
                         (call-with-input-file tmp-err read-string)
                         "")))
          (when (file-exists? tmp-out) (delete-file tmp-out))
          (when (file-exists? tmp-err) (delete-file tmp-err))
          (values status (or stdout "") (or stderr ""))))

;;; =========================================================================
;;; 1. CLI PARSING & ROOT LAUNCHER STRESS TESTING
;;; =========================================================================
(test-group "challenger-cli-launcher"
            ;; --- 1.1 Hostile CLI flags ---
            (test-assert "cli-rejects-unknown-long-flag"
                         (call-with-values
                           (lambda ()
                             (run-cmd-capture "./scripts/badb --unknown-adversarial-flag-12345"))
                           (lambda (status out err)
                             (and (= status 1)
                                  (string-contains err "unrecognized option '--unknown-adversarial-flag-12345'")))))
            (test-assert
              "cli-rejects-hostile-special-characters"
              (call-with-values
                (lambda ()
                  (run-cmd-capture "./scripts/badb '--flag;rm -rf /;echo'"))
                (lambda (status out err)
                  (and (= status 1)
                       (string-contains err "unrecognized option '--flag;rm -rf /;echo'")))))
            (test-assert "cli-rejects-empty-argument"
                         (call-with-values
                           (lambda ()
                             (run-cmd-capture "./scripts/badb ''"))
                           (lambda (status out err)
                             (and (= status 1) (string-contains err "unrecognized option ''")))))
            (test-assert
              "cli-rejects-whitespace-only-argument"
              (call-with-values
                (lambda ()
                  (run-cmd-capture "./scripts/badb '   '"))
                (lambda (status out err)
                  (and (= status 1) (string-contains err "unrecognized option '   '")))))
            (test-assert "cli-rejects-unknown-short-flags"
                         (call-with-values
                           (lambda ()
                             (run-cmd-capture "./scripts/badb -z"))
                           (lambda (status out err)
                             (and (= status 1) (string-contains err "unrecognized option '-z'")))))
            ;; --- 1.2 Help and version flags ---
            (test-assert "cli-help-flag-succeeds"
                         (call-with-values
                           (lambda ()
                             (run-cmd-capture "./scripts/badb --help"))
                           (lambda (status out err)
                             (and (= status 0)
                                  (string-contains out "Usage:")
                                  (string-contains out "Commands & Options:")))))
            (test-assert "cli-short-help-flag-succeeds"
                         (call-with-values
                           (lambda ()
                             (run-cmd-capture "./scripts/badb -h"))
                           (lambda (status out err)
                             (and (= status 0) (string-contains out "Usage:")))))
            (test-assert "cli-version-flag-succeeds"
                         (call-with-values
                           (lambda ()
                             (run-cmd-capture "./scripts/badb --version"))
                           (lambda (status out err)
                             (and (= status 0) (string-contains out "badb 0.1.0")))))
            (test-assert "cli-short-version-flag-succeeds"
                         (call-with-values
                           (lambda ()
                             (run-cmd-capture "./scripts/badb -V"))
                           (lambda (status out err)
                             (and (= status 0) (string-contains out "badb 0.1.0")))))
            ;; --- 1.3 Subcommand: geis handling ---
            (test-assert "cli-geis-bare-prints-usage-exit-0"
                         (call-with-values
                           (lambda ()
                             (run-cmd-capture "./scripts/badb geis"))
                           (lambda (status out err)
                             (and (= status 0) (string-contains out "Usage: badb geis")))))
            (test-assert "cli-geis-help-prints-usage-exit-0"
                         (call-with-values
                           (lambda ()
                             (run-cmd-capture "./scripts/badb geis help"))
                           (lambda (status out err)
                             (and (= status 0) (string-contains out "Usage: badb geis")))))
            (test-assert "cli-geis-status-exits-0"
                         (call-with-values
                           (lambda ()
                             (run-cmd-capture "./scripts/badb geis status"))
                           (lambda (status out err)
                             (and (= status 0) (string-contains out "Locked spec store:")))))
            (test-assert
              "cli-geis-status-nonexistent-feature"
              (call-with-values
                (lambda ()
                  (run-cmd-capture "./scripts/badb geis status non_existent_feature_abc123"))
                (lambda (status out err)
                  (and (= status 0)
                       (string-contains out
                                        "Locked spec for feature 'non_existent_feature_abc123': none")))))
            (test-assert "cli-geis-list-exits-0"
                         (call-with-values
                           (lambda ()
                             (run-cmd-capture "./scripts/badb geis list"))
                           (lambda (status out err)
                             (and (= status 0) (string-contains out "Locked spec store:")))))
            (test-assert
              "cli-geis-verify-missing-argument-fails-closed"
              (call-with-values
                (lambda ()
                  (run-cmd-capture "./scripts/badb geis verify"))
                (lambda (status out err)
                  (and (= status 1)
                       (string-contains err "Error: badb geis verify requires <feature-id>")))))
            (test-assert
              "cli-geis-verify-nonexistent-feature-fails-gracefully"
              (call-with-values
                (lambda ()
                  (run-cmd-capture "./scripts/badb geis verify nonexistent_feature_xyz999"))
                (lambda (status out err)
                  ;; verify should run subprocess replay, which fails closed
                  (and (string-contains out "FAIL")))))
            (test-assert
              "cli-geis-invalid-subcommand-fails-closed"
              (call-with-values
                (lambda ()
                  (run-cmd-capture "./scripts/badb geis invalid_geis_subcommand_99"))
                (lambda (status out err)
                  (and (= status 1)
                       (string-contains err "Unknown geis subcommand: invalid_geis_subcommand_99")))))
            ;; --- 1.4 Non-existent config environment resilience ---
            (test-assert
              "cli-resilient-under-nonexistent-badb-config"
              (call-with-values
                (lambda ()
                  (run-cmd-capture
                    "BADB_CONFIG=/non/existent/path/never/ever.conf ./scripts/badb --version"))
                (lambda (status out err)
                  (and (= status 0) (string-contains out "badb 0.1.0")))))
            ;; --- 1.5 Color presentation & NO_COLOR / FORCE_COLOR conventions ---
            (test-assert
              "cli-color-forced-with-force-color-1"
              (let ((p (open-output-string)))
                (setenv "FORCE_COLOR" "1")
                (let ((res (supports-color? p)))
                  (unsetenv "FORCE_COLOR")
                  res)))
            (test-assert
              "cli-color-disabled-with-force-color-0"
              (let ((p (open-output-string)))
                (setenv "FORCE_COLOR" "0")
                (let ((res (supports-color? p)))
                  (unsetenv "FORCE_COLOR")
                  (not res))))
            (test-assert "cli-color-disabled-with-no-color"
                         (let ((p (open-output-string)))
                           (unsetenv "FORCE_COLOR")
                           (setenv "NO_COLOR" "1")
                           (let ((res (supports-color? p)))
                             (unsetenv "NO_COLOR")
                             (not res))))
            (test-assert
              "cli-force-color-takes-precedence-over-no-color"
              (let ((p (open-output-string)))
                (setenv "FORCE_COLOR" "1")
                (setenv "NO_COLOR" "1")
                (let ((res (supports-color? p)))
                  (unsetenv "FORCE_COLOR")
                  (unsetenv "NO_COLOR")
                  res)))
            (test-assert
              "cli-color-string-formatting-when-disabled"
              (let ((p (open-output-string)))
                (setenv "NO_COLOR" "1")
                (unsetenv "FORCE_COLOR")
                (let
                    ((b (bold p "test")) (c (cyan p "test"))
                     (g (green p "test"))
                     (r (red p "test")))
                  (unsetenv "NO_COLOR")
                  (and (string=? b "test")
                       (string=? c "test")
                       (string=? g "test")
                       (string=? r "test")))))
            (test-assert
              "cli-color-string-formatting-when-forced"
              (let ((p (open-output-string)))
                (setenv "FORCE_COLOR" "1")
                (let
                    ((b (bold p "test")) (c (cyan p "test"))
                     (g (green p "test"))
                     (r (red p "test")))
                  (unsetenv "FORCE_COLOR")
                  (and (string-contains b "\x1b[1m")
                       (string-contains c "\x1b[36m")
                       (string-contains g "\x1b[32m")
                       (string-contains r "\x1b[31m")))))
            )

;;; =========================================================================
;;; 2. THEME & COLOR PROFILE ADVERSARIAL STRESS TESTING
;;; =========================================================================
(test-group "challenger-theme-stress"

            ;; --- 2.1 Custom themes with hex colors and edge-case specs ---
            (test-assert "theme-supports-custom-hex-colors"
                         (let ((th (make-theme #:name "tokyo-custom"
                                               #:roles '((header "#7aa2f7" black bold)
                                                         (accent "#bb9af7" "#1a1b26" none)))))
                           (and (equal? (theme-ref th 'header) '("#7aa2f7" black bold))
                                (equal? (theme-ref th 'accent) '("#bb9af7" "#1a1b26" none)))))

            (test-assert "theme-coerces-short-spec-to-full-triplet"
                         (let ((th (make-theme #:name "short-specs"
                                               #:roles '((header cyan)
                                                         (accent yellow black)
                                                         (notice red)))))
                           (and (equal? (theme-ref th 'header) '(cyan default none))
                                (equal? (theme-ref th 'accent) '(yellow black none))
                                (equal? (theme-ref th 'notice) '(red default none)))))

            (test-assert "theme-normalizes-invalid-colors-to-default"
                         (let ((th (make-theme #:name "bogus-colors"
                                               #:roles '((header bogus-nonexistent-color black bold)
                                                         (accent "#xyz123" black none)
                                                         (status 12345 black none)))))
                           (and (equal? (theme-ref th 'header) '(default black bold))
                                (equal? (theme-ref th 'accent) '(default black none))
                                (equal? (theme-ref th 'status) '(default black none)))))

            (test-assert "theme-normalizes-invalid-attributes-to-none"
                         (let ((th (make-theme #:name "bogus-attrs"
                                               #:roles '((header cyan black invalid-attr)
                                                         (accent cyan black (bold not-an-attr))))))
                           (and (equal? (theme-ref th 'header) '(cyan black none))
                                (equal? (theme-ref th 'accent) '(cyan black none)))))

            (test-assert "theme-accepts-valid-attribute-lists"
                         (let ((th (make-theme #:name "multi-attrs"
                                               #:roles '((header cyan black (bold dim))
                                                         (error red black (bold reverse))))))
                           (and (equal? (theme-ref th 'header) '(cyan black (bold dim)))
                                (equal? (theme-ref th 'error) '(red black (bold reverse))))))

            (test-assert "theme-coerces-string-and-keyword-role-keys"
                         (let ((th (make-theme #:name "key-types"
                                               #:roles '(("header" cyan black bold)
                                                         (#:accent yellow black none)))))
                           (and (equal? (theme-ref th 'header) '(cyan black bold))
                                (equal? (theme-ref th 'accent) '(yellow black none)))))

            ;; --- 2.2 Missing keys & fallback resilience ---
            (test-assert "theme-missing-role-falls-back-to-default-role"
                         (let ((th (make-theme #:name "has-default"
                                               #:roles '((default white black none)
                                                         (header cyan black bold)))))
                           (equal? (theme-ref th 'completely-unknown-role) '(white black none))))

            (test-assert "theme-missing-both-role-and-default-yields-neutral-triplet"
                         (let ((th (make-theme #:name "no-default"
                                               #:roles '((header cyan black bold)))))
                           (equal? (theme-ref th 'completely-unknown-role) '(default default none))))

            (test-assert "theme-ref-handles-null-or-invalid-theme-safely"
                         (and (pair? (theme-ref #f 'header))
                              (pair? (theme-ref "not-a-theme" 'header))
                              (pair? (theme-ref 999 'header))))

            (test-assert "theme-ref-accepts-string-or-keyword-for-role-query"
                         (let ((th (make-theme #:name "test-query"
                                               #:roles '((header cyan black bold)))))
                           (and (equal? (theme-ref th "header") '(cyan black bold))
                                (equal? (theme-ref th #:header) '(cyan black bold)))))

            ;; --- 2.3 Theme lookup and catalog integrity ---
            (test-assert "theme-lookup-is-case-insensitive-and-accepts-symbol"
                         (let ((t1 (lookup-theme "Dracula"))
                               (t2 (lookup-theme 'DRACULA))
                               (t3 (lookup-theme "dRaCuLa")))
                           (and (theme? t1)
                                (eq? t1 t2)
                                (eq? t1 t3))))

            (test-assert "theme-lookup-returns-false-for-unknown"
                         (and (not (lookup-theme "non-existent-theme-xyz"))
                              (not (lookup-theme 'unknown-theme-123))
                              (not (lookup-theme ""))))

            (test-assert "theme-all-19-builtins-define-required-canonical-roles"
                         (every
                           (lambda (name)
                             (let ((th (lookup-theme name)))
                               (and (theme? th)
                                    (every (lambda (role)
                                             (let ((spec (theme-ref th role)))
                                               (and (list? spec)
                                                    (= (length spec) 3)
                                                    (valid-theme-color? (car spec))
                                                    (valid-theme-color? (cadr spec)))))
                                           theme-role-names))))
                           (all-theme-names)))

            ;; --- 2.4 Color conversion to curses constants ---
            (test-assert "color-symbol->ncurses-constant-maps-safely"
                         (and (eq? (color-symbol->ncurses-constant 'black) 'COLOR_BLACK)
                              (eq? (color-symbol->ncurses-constant 'red) 'COLOR_RED)
                              (eq? (color-symbol->ncurses-constant 'green) 'COLOR_GREEN)
                              (eq? (color-symbol->ncurses-constant 'yellow) 'COLOR_YELLOW)
                              (eq? (color-symbol->ncurses-constant 'blue) 'COLOR_BLUE)
                              (eq? (color-symbol->ncurses-constant 'magenta) 'COLOR_MAGENTA)
                              (eq? (color-symbol->ncurses-constant 'cyan) 'COLOR_CYAN)
                              (eq? (color-symbol->ncurses-constant 'white) 'COLOR_WHITE)
                              ;; Hex colors or unknown symbols safely map to COLOR_WHITE fallback
                              (eq? (color-symbol->ncurses-constant "#7aa2f7") 'COLOR_WHITE)
                              (eq? (color-symbol->ncurses-constant 'unknown-color) 'COLOR_WHITE)
                              (eq? (color-symbol->ncurses-constant #f) 'COLOR_WHITE)))

            )

;;; =========================================================================
;;; 3. TUI STATE, RESIZING, MENU SCROLLING & KEYBINDING STRESS TESTING
;;; =========================================================================
(test-group "challenger-tui-state-stress"
            ;; --- 3.1 Terminal Dimensions & Resizing Edge Cases ---
            (test-assert
              "tui-format-header-segments-resilient-on-zero-or-negative-width"
              (let
                  ((segs-0 (format-header-segments 0 "READY" "gpt-4" 5 10 "2/2" "50%"))
                   (segs-neg (format-header-segments -10 "READY" "gpt-4" 5 10 "2/2" "50%")))
                (and (list? segs-0) (list? segs-neg))))
            (test-assert
              "tui-format-header-segments-progressive-truncation-on-narrow-screen"
              ;; Under width 15, right items are progressively dropped and model is truncated
              (let
                  ((segs
                     (format-header-segments 15
                                             "READY"
                                             "very-long-model-name-gpt-4o"
                                             5
                                             10
                                             "2/2"
                                             "50%")))
                (and (list? segs)
                     (not (null? segs))
                     ;; Wordmark is kept
                     (string=? (cadar segs) "badb"))))
            (test-assert
              "tui-format-header-segments-wide-screen"
              (let
                  ((segs (format-header-segments 200 "READY" "gpt-4o" 12 45 "3/3" "75%")))
                (and (list? segs) (>= (length segs) 4))))
            (test-assert
              "tui-format-rule-line-boundary-handling"
              (and (string=? (format-rule-line 0) "")
                   (string=? (format-rule-line -5) "")
                   (= (string-length (format-rule-line 10)) 10)
                   (= (string-length (format-rule-line 80)) 80)))
            (test-assert
              "tui-word-wrapping-boundary-handling"
              (and (equal? (wrap-line "" 10) '(""))
                   (equal? (wrap-line "hello" 0) '("hello"))
                   (equal? (wrap-line "hello" -5) '("hello"))
                   (equal? (wrap-line #f 10) '(""))
                   (equal? (wrap-text "" 10) '(""))
                   (equal? (wrap-text "hello world test" 5) '("hello" "world" "test"))))
            (test-assert
              "tui-input-visible-window-resilient-to-extreme-dimensions"
              (and
                (call-with-values
                  (lambda ()
                    (input-visible-window "hello world" -5 10))
                  (lambda (str cursor-x)
                    (and (string? str) (integer? cursor-x) (>= cursor-x 0))))
                (call-with-values
                  (lambda ()
                    (input-visible-window "hello world" 999 1))
                  (lambda (str cursor-x)
                    (and (string? str) (= cursor-x 0))))
                (call-with-values
                  (lambda ()
                    (input-visible-window "hello world" 5 0))
                  (lambda (str cursor-x)
                    (and (string? str) (integer? cursor-x))))))
            (test-assert
              "tui-input-wrapped-rows-resilient-to-extreme-arguments"
              (call-with-values
                (lambda ()
                  (input-wrapped-rows "a b c d e f" -10 -5 -2))
                (lambda (lines row col)
                  (and (list? lines) (integer? row) (integer? col) (>= row 0) (>= col 0)))))
            ;; --- 3.2 Menu Data Model, Scrolling & Slicing ---
            (test-assert "tui-menu-empty-menu-behavior"
                         (let ((m (make-menu "Empty" '())))
                           (and (= (menu-count m) 0)
                                (not (menu-selected-item m))
                                (eq? (menu-move m 1) m)
                                (eq? (menu-move m -1) m))))
            (test-assert "tui-menu-empty-menu-visible-slice"
                         (let ((m (make-menu "Empty" '())))
                           (call-with-values
                             (lambda ()
                               (menu-visible-slice m 10))
                             (lambda (items offset)
                               (and (null? items) (= offset 0))))))
            (test-assert "tui-menu-single-item-wraparound"
                         (let*
                             ((m (make-menu "Single" '((("only" . "desc") . action1))))
                              (m-fwd (menu-move m 1))
                              (m-bwd (menu-move m -1))
                              (m-big (menu-move m 100)))
                           (and (= (menu-selected-index m-fwd) 0)
                                (= (menu-selected-index m-bwd) 0)
                                (= (menu-selected-index m-big) 0))))
            (test-assert
              "tui-menu-scrolling-wraparound-forward-and-backward"
              (let*
                  ((items
                     (map
                       (lambda (i)
                         (cons (cons (format #f "Item ~a" i) "desc")
                               (string->symbol (format #f "act-~a" i))))
                       (iota 10)))
                   (m (make-menu "Ten" items #:selected-index 0))
                   (m-fwd (menu-move m 1))
                   (m-wrap-bwd (menu-move m -1))
                   (m-large-fwd (menu-move m 25))
                   (m-large-bwd (menu-move m -25)))
                (and (= (menu-selected-index m-fwd) 1)
                     (= (menu-selected-index m-wrap-bwd) 9)
                     (= (menu-selected-index m-large-fwd) 5)
                     (= (menu-selected-index m-large-bwd) 5))))
            (test-assert
              "tui-menu-visible-slice-windowing-and-offset"
              (let*
                  ((items
                     (map
                       (lambda (i)
                         (cons (format #f "Item ~a" i) i))
                       (iota 30)))
                   ;; Menu with selected index 15 and max-visible 10
                   (m (make-menu "Thirty" items #:selected-index 15)))
                (call-with-values
                  (lambda ()
                    (menu-visible-slice m 10))
                  (lambda (slice offset)
                    (and (= (length slice) 10)
                         (<= offset 15)
                         (< 15 (+ offset 10))
                         ;; Visible slice contains the selected item
                         (member (menu-selected-item m) slice))))))
            (test-assert
              "tui-menu-visible-slice-with-zero-or-negative-cap"
              (let*
                  ((items
                     (map
                       (lambda (i)
                         (cons (format #f "Item ~a" i) i))
                       (iota 10)))
                   (m (make-menu "Ten" items #:selected-index 4)))
                (call-with-values
                  (lambda ()
                    (menu-visible-slice m 0))
                  (lambda (slice offset)
                    (and (= (length slice) 1)
                         (= offset 4)
                         (equal? (car slice) (menu-selected-item m)))))))
            (test-assert
              "tui-menu-item-accessors-handle-malformed-items"
              (and
                (string=? (menu-item-label "bare-string") "")
                (string=? (menu-item-description "bare-string") "")
                (not (menu-item-action "bare-string"))
                (string=? (menu-item-label '()) "")
                (string=? (menu-item-description '()) "")
                (not (menu-item-action '()))))
            ;; --- 3.3 Keybinding Events, Normalization & Steering ---
            (test-assert "tui-key-to-code-normalization"
                         (and (eqv? (key->code #\a) 97)
                              (eqv? (key->code #\newline) 10)
                              (eqv? (key->code 27) 27)
                              (eqv? (key->code 410) 410)
                              ; KEY_RESIZE
                              (eq? (key->code #f) #f)))
            (test-assert
              "tui-resolve-modified-key-resolution-order"
              ;; 1. probe succeeds
              (and
                (=
                  (resolve-modified-key "seq1"
                                        (lambda (s)
                                          501)
                                        (lambda _
                                          #f)
                                        601)
                  501)
                ;; 2. probe fails, bind succeeds
                (=
                  (resolve-modified-key "seq2"
                                        (lambda (s)
                                          #f)
                                        (lambda (s f)
                                          #t)
                                        602)
                  602)
                ;; 3. probe and bind both fail -> returns 0
                (=
                  (resolve-modified-key "seq3"
                                        (lambda (s)
                                          #f)
                                        (lambda (s f)
                                          #f)
                                        603)
                  0)))
            (test-assert
              "tui-steering-key-handle-adversarial-events"
              (and ;; Printable character
                (call-with-values
                  (lambda ()
                    (steering-key-handle "hello" #\!))
                  (lambda (buf act)
                    (and (string=? buf "hello!") (eq? act 'append))))
                ;; Enter with empty or whitespace buffer -> no submit
                (call-with-values
                  (lambda ()
                    (steering-key-handle "   " #\newline))
                  (lambda (buf act)
                    (and (string=? buf "   ") (eq? act 'none))))
                ;; Enter with non-empty buffer -> queue
                (call-with-values
                  (lambda ()
                    (steering-key-handle "stop" 10))
                  (lambda (buf act)
                    (and (string=? buf "") (eq? act 'queue))))
                ;; Interrupt (Tab / Ctrl+I / 9)
                (call-with-values
                  (lambda ()
                    (steering-key-handle "in-progress" #\tab))
                  (lambda (buf act)
                    (and (string=? buf "") (eq? act 'interrupt))))
                ;; Escape (27)
                (call-with-values
                  (lambda ()
                    (steering-key-handle "abandon" 27))
                  (lambda (buf act)
                    (and (string=? buf "") (eq? act 'clear))))
                ;; Backspace on non-empty vs empty
                (call-with-values
                  (lambda ()
                    (steering-key-handle "abc" 127))
                  (lambda (buf act)
                    (and (string=? buf "ab") (eq? act 'backspace))))
                (call-with-values
                  (lambda ()
                    (steering-key-handle "" 127))
                  (lambda (buf act)
                    (and (string=? buf "") (eq? act 'none))))
                ;; Unknown curses code (e.g. KEY_UP)
                (call-with-values
                  (lambda ()
                    (steering-key-handle "buf" 259))
                  (lambda (buf act)
                    (and (string=? buf "buf") (eq? act 'none))))))
            ;; --- 3.4 Cursor Editing Robustness ---
            (test-assert
              "tui-cursor-editing-out-of-bounds-resilience"
              (and ;; insert-at with negative pos
                (call-with-values
                  (lambda ()
                    (insert-at "world" -10 "hello "))
                  (lambda (b p)
                    (and (string=? b "hello world") (= p 6))))
                ;; insert-at with pos > length
                (call-with-values
                  (lambda ()
                    (insert-at "hello" 999 " world"))
                  (lambda (b p)
                    (and (string=? b "hello world") (= p 11))))
                ;; backspace-at at pos 0 or negative
                (call-with-values
                  (lambda ()
                    (backspace-at "abc" 0))
                  (lambda (b p)
                    (and (string=? b "abc") (= p 0))))
                (call-with-values
                  (lambda ()
                    (backspace-at "abc" -5))
                  (lambda (b p)
                    (and (string=? b "abc") (= p 0))))
                ;; delete-at at end or beyond
                (call-with-values
                  (lambda ()
                    (delete-at "abc" 3))
                  (lambda (b p)
                    (and (string=? b "abc") (= p 3))))
                (call-with-values
                  (lambda ()
                    (delete-at "abc" 100))
                  (lambda (b p)
                    (and (string=? b "abc") (= p 3))))))
            ;; --- 3.5 Input Classification & Malicious Payloads ---
            (test-assert
              "tui-input-classifier-adversarial-cases"
              (and (not (classify-input ""))
                   (not (classify-input "    \t  \n "))
                   (equal? (classify-input "!") '(shell . ""))
                   (equal? (classify-input "(") '(scheme . "("))
                   (equal? (classify-input "/") '(slash "" ""))
                   (equal?
                     (classify-input "/unknown-slash-command   with  arg  ")
                     '(slash "unknown-slash-command" "with  arg"))
                   (equal? (classify-input "!rm -rf /") '(shell . "rm -rf /"))
                   (equal? (classify-input "(display \"test\")") '(scheme . "(display \"test\")"))
                   (equal? (classify-input "hello from agent") '(chat . "hello from agent"))))
            ;; --- 3.6 Thoughts and Content Separation Stream Resilience ---
            (test-assert
              "tui-thoughts-separation-unclosed-streaming-tags"
              (and
                (call-with-values
                  (lambda ()
                    (split-thoughts-and-content "<think>pondering the problem deeply"))
                  (lambda (th ct)
                    (and (string=? th "pondering the problem deeply") (string=? ct ""))))
                (call-with-values
                  (lambda ()
                    (split-thoughts-and-content
                      "Prefix text <think>internal reasoning</think> Suffix answer"))
                  (lambda (th ct)
                    (and (string=? th "internal reasoning")
                         (string-contains ct "Prefix text")
                         (string-contains ct "Suffix answer"))))
                (call-with-values
                  (lambda ()
                    (split-thoughts-and-content ""))
                  (lambda (th ct)
                    (and (not th) (string=? ct ""))))))
            (test-assert
              ;; DEV-DEBT [env-dependent]: assumes libnotcurses is ABSENT; fails in any env where
              ;; guile-ncurses is installed (pre-existing). Should skip when the module loads.
              "nc-module-fails-gracefully-with-devenv-hint-when-libnotcurses-absent"
              (catch #t
                (lambda ()
                  (resolve-module '(geasa surfaces-tui nc))
                  #t)
                (lambda (key . args)
                  (let ((msg (and (pair? (cdr args)) (cadr args))))
                    (or
                      (string-contains (format #f "~a" msg) "libnotcurses not found")
                      (string-contains (format #f "~a" msg) "devenv")))))))

;;; =========================================================================
;;; 4. SCHEME EVAL & SHELL EXECUTION ADVERSARIAL STRESS TESTING
;;; =========================================================================
(test-group "challenger-scheme-eval-and-shell-stress"
            (let ((repl-mod (make-fresh-user-module)))

              (test-assert "eval-scheme-code-valid-arithmetic-succeeds"
                           (call-with-values
                             (lambda () (eval-scheme-code "(+ 100 200)" repl-mod))
                             (lambda (res err)
                               (and (not err)
                                    (string-contains res "300")))))

              (test-assert "eval-scheme-code-handles-division-by-zero"
                           (call-with-values
                             (lambda () (eval-scheme-code "(/ 1 0)" repl-mod))
                             (lambda (res err)
                               (and (not res)
                                    (string? err)
                                    (string-contains err "numerical-overflow")))))

              (test-assert "eval-scheme-code-handles-unbound-variable"
                           (call-with-values
                             (lambda () (eval-scheme-code "unbound_variable_xyz999" repl-mod))
                             (lambda (res err)
                               (and (not res)
                                    (string? err)
                                    (string-contains err "unbound-variable")))))

              (test-assert "eval-scheme-code-handles-syntax-error"
                           (call-with-values
                             (lambda () (eval-scheme-code "(+ 1 2" repl-mod)) ; unclosed paren
                             (lambda (res err)
                               (and (not res)
                                    (string? err)
                                    (string-contains err "read-error")))))

              (test-assert "execute-shell-command-succeeds-exit-0"
                           (call-with-values
                             (lambda () (execute-shell-command "echo 'badb-test-output'"))
                             (lambda (out code err)
                               (and (not err)
                                    (= code 0)
                                    (string-contains out "badb-test-output")))))

              (test-assert "execute-shell-command-captures-nonzero-exit"
                           (call-with-values
                             (lambda () (execute-shell-command "exit 42"))
                             (lambda (out code err)
                               (and (not err)
                                    (= code 42)))))

              (test-assert "execute-shell-command-captures-stderr"
                           (call-with-values
                             (lambda () (execute-shell-command "sh -c 'echo error-msg >&2; exit 7'"))
                             (lambda (out code err)
                               (and (not err)
                                    (= code 7)
                                    (string-contains out "error-msg")))))
              ))

;;; =========================================================================
;;; 5. SCALABILITY & LARGE PAYLOAD STRESS TESTING
;;; =========================================================================
(test-group "challenger-scalability-and-large-payload-stress"

            ;; Long word wrapping (10,000 characters)
            (test-assert "wrap-line-10k-characters-stress"
                         (let* ((huge-word (make-string 10000 #\x))
                                (chunks (wrap-line huge-word 80)))
                           (and (> (length chunks) 100)
                                (every (lambda (c) (<= (string-length c) 80)) chunks)
                                (string=? (apply string-append chunks) huge-word))))

            ;; Huge menu (10,000 items) navigation and visible slice
            (test-assert "menu-10k-items-scalability-and-windowing"
                         (let* ((huge-items (map (lambda (i)
                                                   (cons (format #f "Entry-~a" i) i))
                                                 (iota 10000)))
                                (m (make-menu "Huge" huge-items #:selected-index 0))
                                (m-moved (menu-move m 5000))
                                (m-wrapped (menu-move m -1)))
                           (and (= (menu-count m) 10000)
                                (= (menu-selected-index m-moved) 5000)
                                (= (menu-selected-index m-wrapped) 9999)
                                ;; Visible slice correctly returns at most 12 items containing the selected item
                                (call-with-values
                                  (lambda () (menu-visible-slice m-moved 12))
                                  (lambda (slice offset)
                                    (and (= (length slice) 12)
                                         (<= offset 5000)
                                         (< 5000 (+ offset 12))
                                         (member (menu-selected-item m-moved) slice)))))))

            ;; Nested and corrupted thought tags
            (test-assert "split-thoughts-nested-and-malformed-tags"
                         (call-with-values
                           (lambda () (split-thoughts-and-content "<think>level 1 <thought>level 2</thought> level 1 rest</think> final answer"))
                           (lambda (th ct)
                             (and (string? th)
                                  (string-contains th "level 1")
                                  (string-contains ct "final answer")))))
            )

;;; =========================================================================
;;; 6. CONFIG THEME INTEGRATION
;;; =========================================================================
(test-group "challenger-config-theme-integration"

            (test-assert "config-ui-theme-resolves-valid-theme"
                         (let* ((cfg (make-badb-config #:ui-theme "tokyo-night"))
                                (cfg-theme (badb-config-ui-theme cfg))
                                (resolved (lookup-theme cfg-theme)))
                           (and (string=? cfg-theme "tokyo-night")
                                (theme? resolved)
                                (string=? (theme-name resolved) "tokyo-night"))))

            (test-assert "config-ui-theme-fallback-when-missing-or-invalid"
                         (let* ((cfg (make-badb-config #:ui-theme "non-existent-theme-xyz"))
                                (cfg-theme (badb-config-ui-theme cfg))
                                (resolved (lookup-theme cfg-theme)))
                           (and (not resolved)
                                ;; default-theme parameter remains safe fallback
                                (theme? (default-theme))
                                (string=? (theme-name (default-theme)) "badb-dark"))))
            )

(test-end "adversarial-surfaces")
