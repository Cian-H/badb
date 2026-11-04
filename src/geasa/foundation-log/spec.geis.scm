;;; test-log.scm — Tests for (geasa foundation-log plugin)

;;;

;;; Verifies error log line formatting, appending, directory creation,

;;; the never-throws guarantee on unwritable paths, and the BADB_DEBUG

;;; write gate (no filesystem writes while the variable is unset).

(define-module (test test-log) #:use-module
               (srfi srfi-64)
  #:use-module (badb spec)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (geasa foundation-log plugin))

(spec-begin "foundation-log")

(define %saved-badb-debug (getenv "BADB_DEBUG"))

(test-group "log-entry-formatting"
            (test-equal "entry line format"
                        "[2026-10-28T14:00:00Z] [react] boom"
                        (format-error-entry "2026-10-28T14:00:00Z" "react" "boom"))
            (test-assert "default log path lives in .badb"
                         (string-suffix? ".badb/errors.log" (default-error-log-path)))
            (test-assert
              "timestamp is a non-empty ISO-8601 UTC string"
              (let ((ts (current-iso8601-timestamp)))
                (and (string? ts) (> (string-length ts) 10) (string-suffix? "Z" ts)))))

;;; Every group below expects real filesystem writes, so debug logging
;;; must be enabled for their duration and restored afterwards.

(dynamic-wind
  (lambda ()
    (setenv "BADB_DEBUG" "1"))
  (lambda ()
    (test-group "log-error-append"
                (let*
                    ((tmp-dir "/tmp/badb-log-test")
                     (log-path (string-append tmp-dir "/nested/errors.log")))
                  (use-modules (ice-9 ftw))
                  (catch #t
                    (lambda ()
                      (system* "rm" "-rf" tmp-dir))
                    (lambda _
                      #f))
                  (test-equal "first write succeeds and returns #t"
                              #t
                              (log-error! "first failure" "react" log-path))
                  (test-assert "log file created with parent dirs" (file-exists? log-path))
                  (test-equal "second write succeeds"
                              #t
                              (log-error! "second failure" "tui" log-path))
                  (let
                      ((lines
                         (call-with-input-file log-path
                                               (lambda (port)
                                                 (let loop
                                                   ((acc '()))
                                                   (let ((l (read-line port)))
                                                     (if (eof-object? l)
                                                       (reverse acc)
                                                       (loop (cons l acc)))))))))
                    (test-equal "two entries appended" 2 (length lines))
                    (test-assert "entries are timestamped" (string-prefix? "[" (car lines)))
                    (test-assert "context tag recorded" (string-contains (car lines) "[react]"))
                    (test-assert "second context tag recorded"
                                 (string-contains (cadr lines) "[tui]"))
                    (test-assert "message text recorded"
                                 (string-contains (car lines) "first failure")))
                  (catch #t
                    (lambda ()
                      (system* "rm" "-rf" tmp-dir))
                    (lambda _
                      #f))))

    (test-group "log-error-never-throws"
                (test-assert
                  "unwritable path returns #f without throwing"
                  (not (log-error! "doomed" "tui" "/proc/definitely/not/writable/errors.log")))
                (test-assert
                  "non-string message is stringified without throwing"
                  (log-error! (list "weird" 'message 42) "tui" "/tmp/badb-log-weird.log")))

    (test-group "log-exception"
                (let ((path "/tmp/badb-log-exc.log"))
                  (catch #t
                    (lambda ()
                      (delete-file path))
                    (lambda _
                      #f))
                  (test-assert "logs key and args"
                               (log-exception! 'misc-error '(1 2 3) "react" path))
                  (test-assert "exception text recorded"
                               (call-with-input-file path
                                                     (lambda (port)
                                                       (string-contains (read-line port) "misc-error")))))))
  (lambda ()
    (if %saved-badb-debug
        (setenv "BADB_DEBUG" %saved-badb-debug)
        (unsetenv "BADB_DEBUG"))))

;;; With BADB_DEBUG unset, LOG-ERROR! must report success without
;;; creating or touching any file.

(test-group "log-error-debug-gating"
            (let ((path "/tmp/badb-log-gated.log"))
              (catch #t
                (lambda ()
                  (delete-file path))
                (lambda _
                  #f))
              (dynamic-wind
                (lambda ()
                  (unsetenv "BADB_DEBUG"))
                (lambda ()
                  (test-equal "gated call reports success"
                              #t
                              (log-error! "hidden failure" "react" path))
                  (test-assert "no log file created without BADB_DEBUG"
                               (not (file-exists? path))))
                (lambda ()
                  (if %saved-badb-debug
                      (setenv "BADB_DEBUG" %saved-badb-debug)
                      (unsetenv "BADB_DEBUG"))))))

(spec-end)
(spec-exit)
