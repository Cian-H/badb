;;; badb/surfaces/cli.scm — CLI presentation helpers.
;;;
;;; Reusable colour support for badb commands.  Callers pass the
;;; target port and never touch raw escape codes.
(define-module (badb surfaces cli)
               #:use-module (term ansi-color)
               #:export (bold cyan green red supports-color?))

;;;
;;; ANSI colours.  Enabled when writing to a terminal; forced on by
;;; FORCE_COLOR; always disabled by the NO_COLOR convention
;;; (no-color.org).  Precedence: FORCE_COLOR > NO_COLOR > tty.
;;;

(define (color-forced?)
        (let ((force (getenv "FORCE_COLOR")))
          (and force (not (string-null? force)) (not (string=? force "0")))))

(define (supports-color? port)
        (or (color-forced?)
            (and (isatty? port)
                 (not (getenv "NO_COLOR")))))

(define (colorize port attr str)
        (if (supports-color? port)
          (colorize-string str attr)
          str))

(define (bold port str)  (colorize port 'BOLD str))
(define (cyan port str)  (colorize port 'CYAN str))
(define (green port str) (colorize port 'GREEN str))
(define (red port str)   (colorize port 'RED str))
