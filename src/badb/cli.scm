;;; badb — CLI presentation helpers.
;;;
;;; Reusable colour support for badb commands.  Callers pass the
;;; target port and never touch raw escape codes.
(define-module (badb cli)
  #:export (bold cyan green red supports-color?))

;;;
;;; ANSI colours.  Enabled when writing to a terminal; forced on by
;;; FORCE_COLOR; always disabled by the NO_COLOR convention
;;; (no-color.org).  Precedence: FORCE_COLOR > NO_COLOR > tty.
;;;

(define %reset (string-append (string #\escape) "[0m"))
(define %bold  (string-append (string #\escape) "[1m"))
(define %red   (string-append (string #\escape) "[31m"))
(define %green (string-append (string #\escape) "[32m"))
(define %cyan  (string-append (string #\escape) "[36m"))

(define (color-forced?)
  (let ((force (getenv "FORCE_COLOR")))
    (and force (not (string-null? force)) (not (string=? force "0")))))

(define (supports-color? port)
  (or (color-forced?)
      (and (isatty? port)
           (not (getenv "NO_COLOR")))))

(define (colorize port color str)
  (if (supports-color? port)
      (string-append color str %reset)
      str))

(define (bold port str)  (colorize port %bold str))
(define (cyan port str)  (colorize port %cyan str))
(define (green port str) (colorize port %green str))
(define (red port str)   (colorize port %red str))
