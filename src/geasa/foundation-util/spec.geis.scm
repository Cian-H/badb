;;; test-util.scm — Tests for (geasa foundation-util plugin)
;;;
;;; Verifies filesystem helpers, coercion functions, dual-key alist navigation,
;;; safe JSON stringify/parse, and environment formatting.

(define-module (test test-util)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (geasa foundation-util plugin))

(spec-begin "foundation-util")

;;;
;;; 1. Coercion & Identifier Normalization
;;;
(test-group "util-coercion"
  (test-equal "ensure-string from string" "hello" (ensure-string "hello"))
  (test-equal "ensure-string from symbol" "foo-bar" (ensure-string 'foo-bar))
  (test-equal "ensure-string from keyword" "key" (ensure-string #:key))

  (test-equal "ensure-symbol from symbol" 'alpha (ensure-symbol 'alpha))
  (test-equal "ensure-symbol from string" 'beta (ensure-symbol "beta"))

  (test-equal "ensure-keyword from keyword" #:mode (ensure-keyword #:mode))
  (test-equal "ensure-keyword from symbol" #:mode (ensure-keyword 'mode))
  (test-equal "ensure-keyword from string" #:mode (ensure-keyword "mode"))

  (test-equal "normalize-role symbol" "system" (normalize-role 'system))
  (test-equal "normalize-role uppercase string" "user" (normalize-role "USER"))
  (test-equal "normalize-role keyword" "assistant" (normalize-role #:assistant)))

;;;
;;; 2. Property & Alist Accessors
;;;
(test-group "util-properties"
  (let ((al '((name . "badb") ("model" . "gpt-4") (port 8080) (#:verbose #t))))
    (test-equal "prop-ref with symbol key on symbol alist" "badb" (prop-ref al 'name))
    (test-equal "prop-ref with string key on symbol alist" "badb" (prop-ref al "name"))
    (test-equal "prop-ref with symbol key on string alist" "gpt-4" (prop-ref al 'model))
    (test-equal "prop-ref with string key on string alist" "gpt-4" (prop-ref al "model"))
    (test-equal "json-ref alias works with string key" "gpt-4" (json-ref al "model"))
    (test-equal "json-ref alias works with symbol key" "badb" (json-ref al 'name))
    (test-equal "prop-ref missing key returns default" "default-val" (prop-ref al 'missing "default-val"))

    (test-equal "get-prop pair syntax" "badb" (get-prop al 'name "none"))
    (test-equal "get-prop list syntax" 8080 (get-prop al 'port 0))
    (test-equal "get-prop keyword syntax" #t (get-prop al 'verbose #f)))

  (let* ((al '((a . 1) ("b" . 2)))
         (updated (prop-set al 'a 10))
         (added (prop-set updated "c" 3)))
    (test-equal "prop-set updates existing key" 10 (prop-ref updated 'a))
    (test-equal "prop-set adds new key" 3 (prop-ref added 'c)))

  (let ((sample '(("x" . 10) (y . 20) ("z" . 30))))
    (test-equal "alist-remove removes by string" '((y . 20) ("z" . 30)) (alist-remove "x" sample))
    (test-equal "alist-remove removes by symbol" '(("x" . 10) ("z" . 30)) (alist-remove 'y sample))
    (test-equal "alist-remove preserves on non-existent key" sample (alist-remove 'none sample))
    (test-equal "alist-remove handles empty alist" '() (alist-remove 'k '())))

  (let ((base '(("a" . 1) ("b" . 2)))
        (overlay '(("b" . 20) ("c" . 30))))
    (test-equal "alist-merge overlay takes precedence"
      '(("b" . 20) ("c" . 30) ("a" . 1))
      (alist-merge base overlay))
    (test-equal "alist-merge with empty overlay returns base" base (alist-merge base '()))
    (test-equal "alist-merge with empty base returns overlay" overlay (alist-merge '() overlay))))

;;;
;;; 3. Safe JSON Serialization & Parsing
;;;
(test-group "util-json"
  (test-equal "safe-json-string preserves string" "already a string" (safe-json-string "already a string"))
  (test-assert "safe-json-string serializes pair"
    (string-contains (safe-json-string '((key . "val"))) "\"key\""))

  (test-equal "safe-json-parse valid string" '(("a" . 1)) (safe-json-parse "{\"a\": 1}"))
  (test-equal "safe-json-parse invalid string returns fallback"
    'fallback (safe-json-parse "not valid json {{" 'fallback))
  (test-equal "safe-json-parse empty string returns fallback" #f (safe-json-parse ""))
  (test-equal "parse-tool-args from JSON string"
    '(("name" . "test")) (parse-tool-args "{\"name\": \"test\"}"))
  (test-equal "parse-tool-args from alist"
    '((name . "test")) (parse-tool-args '((name . "test"))))
  (test-equal "parse-tool-args from invalid string returns empty list"
    '() (parse-tool-args "invalid-json{")))

;;;
;;; 4. Environment Normalization & Exec Strings
;;;
(test-group "util-environment"
  (let* ((raw '(("API_KEY" . "sec123") (PORT . 3000) "DEBUG=true"))
         (normalized (normalize-env-alist raw))
         (exec-strings (env-alist->exec-strings normalized)))
    (test-equal "normalized length" 3 (length normalized))
    (test-assert "exec strings contains API_KEY" (member "API_KEY=sec123" exec-strings))
    (test-assert "exec strings contains PORT" (member "PORT=3000" exec-strings))
    (test-assert "exec strings contains DEBUG" (member "DEBUG=true" exec-strings))))

;;;
;;; 5. Filesystem Helpers
;;;
(test-group "util-filesystem"
  (let* ((tmp-dir (string-append "/tmp/badb-util-test-" (number->string (current-time))))
         (nested-dir (string-append tmp-dir "/sub1/sub2"))
         (target-file (string-append nested-dir "/file.txt")))
    (test-assert "ensure-parent-directory! creates nested parents"
      (begin
        (ensure-parent-directory! target-file)
        (file-exists? nested-dir)))))

(test-group "util-helpers"
  (test-assert "non-empty-string? valid string" (non-empty-string? "hello"))
  (test-assert "non-empty-string? padded string" (non-empty-string? "  world  "))
  (test-assert "non-empty-string? empty string returns #f" (not (non-empty-string? "")))
  (test-assert "non-empty-string? whitespace returns #f" (not (non-empty-string? "   ")))
  (test-assert "non-empty-string? non-string returns #f" (not (non-empty-string? #f)))
  (test-equal "split-words trims and splits whitespace"
    '("alpha" "beta" "gamma") (split-words "  alpha   beta   gamma  "))
  (test-equal "split-words empty string" '() (split-words ""))
  (test-equal "format-exception misc-error formatted"
    "value is 42" (format-exception 'misc-error (list #f "value is ~a" (list 42))))
  (test-equal "sanitize-for-json nested symbols"
    '(("cmd" . "run") ("flag" . "true")) (sanitize-for-json '((cmd . "run") (flag . "true"))))
  (test-equal "truncate-string keeps short strings intact"
    "short" (truncate-string "short" 10))
  (test-equal "truncate-string truncates and appends ellipsis"
    "hello..." (truncate-string "hello world" 8))
  (test-equal "truncate-display is an alias for truncate-string"
    "hello..." (truncate-display "hello world" 8)))

(test-group "util-tool-args"
  (let ((args "{\"name\":\"alice\",\"count\":42}"))
    (with-tool-args args (name (c count 0) (foo bar "default"))
      (test-equal "with-tool-args binds simple var" "alice" name)
      (test-equal "with-tool-args binds custom key" 42 c)
      (test-equal "with-tool-args fallback default" "default" foo)))
  (let ((args '((prompt . "hello") (timeout . 10))))
    (with-tool-args args ((p prompt "") (t timeout 5))
      (test-equal "with-tool-args alist prompt" "hello" p)
      (test-equal "with-tool-args alist timeout" 10 t))))

(test-group "util-dir-entries"
  (test-assert "valid-dir-entry? normal name" (valid-dir-entry? "src"))
  (test-assert "valid-dir-entry? dot is false" (not (valid-dir-entry? ".")))
  (test-assert "valid-dir-entry? dot-dot is false" (not (valid-dir-entry? "..")))
  (test-assert "scandir-valid-entries excludes dot and dot-dot"
    (let ((entries (scandir-valid-entries ".")))
      (and (pair? entries)
           (not (member "." entries))
           (not (member ".." entries)))))
  (test-equal "scandir-valid-entries nonexistent dir returns empty"
    '() (scandir-valid-entries "/nonexistent-path-for-test-12345")))

(test-group "util-message-accessors"
  (let ((msg '((role . "assistant") (content . "hello world") (reasoning_content . "thinking..."))))
    (test-equal "message-role assistant" "assistant" (message-role msg))
    (test-equal "message-content text" "hello world" (message-content msg))
    (test-equal "message-thoughts reasoning" "thinking..." (message-thoughts msg)))
  (let ((msg '(("role" . "USER") ("content" . "query"))))
    (test-equal "message-role string user normalized" "user" (message-role msg))
    (test-equal "message-content string user" "query" (message-content msg))
    (test-equal "message-thoughts absent returns #f" #f (message-thoughts msg))))

(test-group "util-shell-and-formatting"
  (test-equal "format-token-kilo 1500" "1.5k" (format-token-kilo 1500))
  (test-equal "format-token-kilo 420" "0.4k" (format-token-kilo 420))
  (call-with-values
    (lambda () (exec-shell-command "echo test-out"))
    (lambda (out exit-val err)
      (test-equal "exec-shell-command stdout" "test-out" out)
      (test-equal "exec-shell-command exit code" 0 exit-val)
      (test-equal "exec-shell-command err #f" #f err))))

(test-group "util-let-props"
  (let ((obj '((name . "alice") (role . "admin") (active . #t))))
    (let-props obj (name (r role "guest") (cnt count 0))
      (test-equal "let-props simple key" "alice" name)
      (test-equal "let-props custom key" "admin" r)
      (test-equal "let-props default fallback" 0 cnt))))

(test-group "util-ensure-list"
  (test-equal "ensure-list from list" '(1 2 3) (ensure-list '(1 2 3)))
  (test-equal "ensure-list from vector" '(1 2 3) (ensure-list #(1 2 3)))
  (test-equal "ensure-list from empty list" '() (ensure-list '()))
  (test-equal "ensure-list from #f" '() (ensure-list #f))
  (test-equal "ensure-list from scalar" '("foo") (ensure-list "foo")))

(test-group "util-flow-macros"
  (test-equal "with-default returns value when no error"
    42 (with-default 0 (+ 40 2)))
  (test-equal "with-default returns fallback on error"
    99 (with-default 99 (error "boom")))
  (test-equal "ignore-errors returns #f on error"
    #f (ignore-errors (/ 1 0)))
  (test-equal "string-case exact match"
    "apple" (string-case "apple"
              ("orange" "citrus")
              ("apple" "apple")
              (else "other")))
  (test-equal "string-case multi-target match"
    "matched" (string-case "c"
                (("a" "b" "c") "matched")
                (else "none")))
  (test-equal "string-case fallback else"
    "fallback" (string-case "unknown"
                 (("a" "b") "first")
                 (else "fallback")))
  (receive (x y) (values 10 20)
    (test-equal "receive binds multiple values" '(10 20) (list x y)))
  (let-values* (((a b) (values 1 2))
                ((c) (values (+ a b))))
    (test-equal "let-values* binds cascading multiple values" 3 c))
  (with-env (("BADB_MACRO_TEST_ENV" "active"))
    (test-equal "with-env sets environment variable" "active" (getenv "BADB_MACRO_TEST_ENV")))
  (test-equal "with-env restores environment variable" #f (getenv "BADB_MACRO_TEST_ENV"))
  (let ((cwd (getcwd)))
    (with-directory "/tmp"
      (test-assert "with-directory changes to /tmp" (or (string=? (getcwd) "/tmp") (string-prefix? "/tmp" (getcwd)))))
    (test-equal "with-directory restores original directory" cwd (getcwd))))

(test-group "util-file-io"
  (let* ((tmp (string-append "/tmp/badb-file-io-test-" (number->string (current-time)) ".txt"))
         (content "hello world UTF-8 string\nline 2\n"))
    (string->file tmp content)
    (test-assert "string->file creates file" (file-exists? tmp))
    (test-equal "file->string reads back content exact" content (file->string tmp))
    (test-equal "file->string nonexistent returns #f" #f (file->string "/nonexistent-path-fileio-999"))))

(spec-end)
(spec-exit)
