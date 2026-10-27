;;; test-util.scm — Tests for (badb util)
;;;
;;; Verifies filesystem helpers, coercion functions, dual-key alist navigation,
;;; safe JSON stringify/parse, and environment formatting.

(define-module (test test-util)
  #:use-module (srfi srfi-64)
  #:use-module (badb util))

(test-begin "badb-util")

;;;
;;; 1. Coercion & Identifier Normalization
;;;
(test-group "util-coercion"
  (test-equal "ensure-string from string"
    "hello"
    (ensure-string "hello"))
  (test-equal "ensure-string from symbol"
    "foo-bar"
    (ensure-string 'foo-bar))
  (test-equal "ensure-string from keyword"
    "key"
    (ensure-string #:key))

  (test-equal "ensure-symbol from symbol"
    'alpha
    (ensure-symbol 'alpha))
  (test-equal "ensure-symbol from string"
    'beta
    (ensure-symbol "beta"))

  (test-equal "ensure-keyword from keyword"
    #:mode
    (ensure-keyword #:mode))
  (test-equal "ensure-keyword from symbol"
    #:mode
    (ensure-keyword 'mode))
  (test-equal "ensure-keyword from string"
    #:mode
    (ensure-keyword "mode"))

  (test-equal "normalize-role symbol"
    "system"
    (normalize-role 'system))
  (test-equal "normalize-role uppercase string"
    "user"
    (normalize-role "USER"))
  (test-equal "normalize-role keyword"
    "assistant"
    (normalize-role #:assistant)))

;;;
;;; 2. Property & Alist Accessors
;;;
(test-group "util-properties"
  (let ((al '((name . "badb") ("model" . "gpt-4") (port 8080) (#:verbose #t))))
    (test-equal "prop-ref with symbol key on symbol alist"
      "badb"
      (prop-ref al 'name))
    (test-equal "prop-ref with string key on symbol alist"
      "badb"
      (prop-ref al "name"))
    (test-equal "prop-ref with symbol key on string alist"
      "gpt-4"
      (prop-ref al 'model))
    (test-equal "prop-ref with string key on string alist"
      "gpt-4"
      (prop-ref al "model"))
    (test-equal "prop-ref missing key returns default"
      "default-val"
      (prop-ref al 'missing "default-val"))

    (test-equal "get-prop pair syntax"
      "badb"
      (get-prop al 'name "none"))
    (test-equal "get-prop list syntax"
      8080
      (get-prop al 'port 0))
    (test-equal "get-prop keyword syntax"
      #t
      (get-prop al 'verbose #f)))

  (let* ((al '((a . 1) ("b" . 2)))
         (updated (prop-set al 'a 10))
         (added (prop-set updated "c" 3)))
    (test-equal "prop-set updates existing key"
      10
      (prop-ref updated 'a))
    (test-equal "prop-set adds new key"
      3
      (prop-ref added 'c))))

;;;
;;; 3. Safe JSON Serialization & Parsing
;;;
(test-group "util-json"
  (test-equal "safe-json-string preserves string"
    "already a string"
    (safe-json-string "already a string"))
  (test-assert "safe-json-string serializes pair"
    (string-contains (safe-json-string '((key . "val"))) "\"key\""))

  (test-equal "safe-json-parse valid string"
    '(("a" . 1))
    (safe-json-parse "{\"a\": 1}"))
  (test-equal "safe-json-parse invalid string returns fallback"
    'fallback
    (safe-json-parse "not valid json {{" 'fallback))
  (test-equal "safe-json-parse empty string returns fallback"
    #f
    (safe-json-parse "")))

;;;
;;; 4. Environment Normalization & Exec Strings
;;;
(test-group "util-environment"
  (let* ((raw '(("API_KEY" . "sec123") (PORT . 3000) "DEBUG=true"))
         (normalized (normalize-env-alist raw))
         (exec-strings (env-alist->exec-strings normalized)))
    (test-equal "normalized length"
      3
      (length normalized))
    (test-assert "exec strings contains API_KEY"
      (member "API_KEY=sec123" exec-strings))
    (test-assert "exec strings contains PORT"
      (member "PORT=3000" exec-strings))
    (test-assert "exec strings contains DEBUG"
      (member "DEBUG=true" exec-strings))))

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
            (test-equal
              "split-words trims and splits whitespace"
              '("alpha" "beta" "gamma")
              (split-words "  alpha   beta   gamma  "))
            (test-equal "split-words empty string" '() (split-words ""))
            (test-equal
              "format-exception misc-error formatted"
              "value is 42"
              (format-exception 'misc-error (list #f "value is ~a" (list 42))))
            (test-equal "sanitize-for-json nested symbols"
                        '(("cmd" . "run") ("flag" . "true"))
                        (sanitize-for-json '((cmd . "run") (flag . "true")))))

(test-end "badb-util")
