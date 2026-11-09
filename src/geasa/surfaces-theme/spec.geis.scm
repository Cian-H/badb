;;; test-theme.scm — Tests for (geasa surfaces-theme plugin)
;;;
;;; Verifies theme record construction, built-in theme completeness,
;;; graceful role lookup fallback, and theme name resolution.

(define-module (test test-theme)
               #:use-module (srfi srfi-1)
               #:use-module (badb spec)
               #:use-module (srfi srfi-64)
               #:use-module (ice-9 format)
               #:use-module (geasa surfaces-theme plugin))

(spec-begin "surfaces-theme")

;;;
;;; 1. Theme Record Construction
;;;
(test-group "theme-record"
            (let ((th (make-theme #:name "custom" #:roles '((frame red black bold)))))
              (test-assert "theme? recognizes theme record" (theme? th))
              (test-assert "theme? rejects string" (not (theme? "badb-dark")))
              (test-assert "theme? rejects #f" (not (theme? #f)))
              (test-assert "theme? rejects alist" (not (theme? '((frame . red)))))
              (test-equal "theme-name returns string" "custom" (theme-name th))
              (test-equal "theme-name coerces symbol names" "sym-name"
                          (theme-name (make-theme #:name 'sym-name #:roles '())))
              (test-equal "theme-roles returns role alist"
                          '((frame red black bold)) (theme-roles th))
              (test-equal "theme-ref returns spec for defined role"
                          '(red black bold) (theme-ref th 'frame)))

            (let ((th (make-theme #:name "loose"
                                  #:roles '((header cyan)
                                            (bogus purple black wat)
                                            (status green white)
                                            ("string-key" green black none)
                                            (bare . red)))))
              (test-equal "fg-only specs coerce to (fg default none)"
                          '(cyan default none) (theme-ref th 'header))
              (test-equal "invalid fg color coerces to default"
                          '(default black none) (theme-ref th 'bogus))
              (test-equal "two-part specs coerce missing attrs to none"
                          '(green white none) (theme-ref th 'status))
              (test-equal "string role keys coerce to symbols"
                          '(green black none) (theme-ref th 'string-key))
              (test-equal "bare color spec coerces to (fg default none)"
                          '(red default none) (theme-ref th 'bare))))

;;;
;;; 2. Built-in Theme Completeness
;;;
(define %valid-colors
        '(black red green yellow blue magenta cyan white default))

(define (%valid-attrs? attrs)
        (or (eq? attrs 'none)
            (null? attrs)
            (and (symbol? attrs) (memq attrs '(bold dim reverse)))
            (and (list? attrs) (not (null? attrs))
                 (every (lambda (a) (memq a '(bold dim reverse))) attrs))))

(test-group "theme-builtins"
            (let ((names (all-theme-names)))
              (test-assert "all-theme-names is a non-empty list"
                           (and (list? names) (not (null? names))))
              (test-equal "five required built-ins present"
                          '("badb-dark" "midnight" "nord" "solarized-dark" "paper" "mono")
                          (lset-intersection string=? names
                                             '("badb-dark" "midnight" "nord" "solarized-dark" "paper" "mono")))

              (for-each
                (lambda (name)
                  (let ((th (lookup-theme name)))
                    (test-assert (format #f "lookup-theme resolves built-in ~a" name)
                                 (and (theme? th) (string=? (theme-name th) name)))
                    (test-equal (format #f "~a defines all required roles with valid specs" name)
                                '()
                                (filter-map
                                  (lambda (role)
                                    (let ((entry (assq role (theme-roles th)))
                                          (spec (theme-ref th role)))
                                      (cond
                                        ((not entry)
                                          (list role 'missing-role))
                                        ((not (and (list? spec) (= 3 (length spec))))
                                          (list role 'invalid-spec spec))
                                        ((not (valid-theme-color? (car spec)))
                                          (list role 'invalid-fg (car spec)))
                                        ((not (valid-theme-color? (cadr spec)))
                                          (list role 'invalid-bg (cadr spec)))
                                        ((not (%valid-attrs? (caddr spec)))
                                          (list role 'invalid-attrs (caddr spec)))
                                        (else #f))))
                                  (cons 'default theme-role-names)))))
                names)))

;;;
;;; 3. badb-dark Mirrors the Current TUI Color Scheme
;;;
(test-group "theme-badb-dark-scheme"
            (let ((th (lookup-theme "badb-dark")))
              (test-equal "frame is blue" 'blue (car (theme-ref th 'frame)))
              (test-equal "header is cyan bold" '(cyan black bold) (theme-ref th 'header))
              (test-equal "user is green" '(green black none) (theme-ref th 'user))
              (test-equal "assistant is white" '(white black none) (theme-ref th 'assistant))
              (test-equal "system is yellow" '(yellow black none) (theme-ref th 'system))
              (test-equal "error is red" '(red black bold) (theme-ref th 'error))
              (test-equal "shell is magenta" '(magenta black none) (theme-ref th 'shell))
              (test-equal "success is green" '(green black none) (theme-ref th 'success))
              (test-equal "warning is yellow" '(yellow black bold) (theme-ref th 'warning))))

;;;
;;; 4. theme-ref Fallback Behavior
;;;
(test-group "theme-ref-fallback"
            (let ((th (lookup-theme "badb-dark")))
              (test-equal "unknown role falls back to the default role spec"
                          (theme-ref th 'default) (theme-ref th 'no-such-role))
              (test-equal "string role keys are accepted"
                          (theme-ref th 'error) (theme-ref th "error"))
              (test-equal "keyword role keys are accepted"
                          (theme-ref th 'error) (theme-ref th #:error))
              (test-equal "#f theme falls back to the default theme"
                          (theme-ref (default-theme) 'error) (theme-ref #f 'error)))

            (let ((th (make-theme #:name "no-default" #:roles '((frame red black bold)))))
              (test-equal "defined role still resolves without a default role"
                          '(red black bold) (theme-ref th 'frame))
              (test-equal "theme without default role yields neutral spec"
                          '(default default none) (theme-ref th 'missing))))

;;;
;;; 5. Theme Name Lookup
;;;
(test-group "theme-lookup"
            (test-assert "lookup-theme by string" (theme? (lookup-theme "nord")))
            (test-assert "lookup-theme by symbol" (theme? (lookup-theme 'nord)))
            (test-assert "lookup-theme is case-insensitive" (theme? (lookup-theme "Nord")))
            (test-equal "lookup-theme returns theme with matching name"
                        "solarized-dark" (theme-name (lookup-theme "solarized-dark")))
            (test-assert "lookup-theme unknown string returns #f"
                         (not (lookup-theme "nonexistent-theme")))
            (test-assert "lookup-theme unknown symbol returns #f"
                         (not (lookup-theme 'nonexistent-theme))))

;;;
;;; 6. Default Theme Accessor
;;;
(test-group "theme-default"
            (test-assert "default-theme returns a theme" (theme? (default-theme)))
            (test-equal "default theme is badb-dark" "badb-dark" (theme-name (default-theme)))
            (test-equal "default-theme parameter can be set temporarily"
                        "nord"
                        (parameterize ((default-theme (lookup-theme "nord")))
                                      (theme-name (default-theme))))
            (test-equal "default-theme restored after parameterize"
                        "badb-dark" (theme-name (default-theme))))

(spec-end)
(spec-exit)
