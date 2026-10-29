;;; badb — Error Logging

;;;

;;; Append-only error log inside the local .badb directory. Every error

;;; surfaced by the harness (TUI error cards, react verdicts, render

;;; failures) is written here with a UTC timestamp and a context tag so

;;; failures can be diagnosed after the fact. Logging never throws: a

;;; failed write returns #f and leaves the caller untouched.

(define-module (badb log) #:use-module
               (badb util)
               #:use-module
               (ice-9 format)
               #:use-module
               (srfi srfi-19)
               #:export
               (default-error-log-path current-iso8601-timestamp
                                       format-error-entry
                                       ensure-directory!
                                       log-error!
                                       log-exception!))

(define (ensure-directory! dir)
  "Create DIR and any missing parent directories. Never throws."
  (catch #t
    (lambda ()
      (mkdir-p dir))
    (lambda _
      #f)))

(define (default-error-log-path)
        "Return the default error log path inside the local .badb directory."
        ".badb/errors.log")

(define (current-iso8601-timestamp)
        "Return the current UTC timestamp formatted as an ISO-8601 string."
        (date->string (current-date 0) "~Y-~m-~dT~H:~M:~SZ"))

(define
  (format-error-entry timestamp context message)
  "Format one log line from TIMESTAMP, CONTEXT and MESSAGE strings."
  (format #f "[~a] [~a] ~a" timestamp context message))

(define*
  (log-error! message #:optional (context "badb") (path (default-error-log-path)))
  "Append a timestamped MESSAGE line for CONTEXT to the log at PATH,
creating the parent directory when needed. Never throws: returns #t on
success and #f on any failure (unwritable path, encoding error, ...)."
  (catch #t
    (lambda ()
      (let ((dir (dirname path)))
        (ensure-directory! dir))
      (let ((port (open-file path "a")))
        (display (format-error-entry (current-iso8601-timestamp) context message) port)
        (newline port)
        (close-port port))
      #t)
    (lambda _
      #f)))

(define*
  (log-exception! key
                  args
                  #:optional
                  (context "badb")
                  (path (default-error-log-path)))
  "Log a caught exception (KEY and ARGS as delivered to a catch handler)
as a single error line. Returns the result of LOG-ERROR!."
  (log-error! (format #f "~a ~s" key args) context path))