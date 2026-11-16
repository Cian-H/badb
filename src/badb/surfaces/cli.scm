;;; badb/surfaces/cli.scm — CLI presentation helpers.
;;;
;;; Reusable colour support for badb commands.  Callers pass the
;;; target port and never touch raw escape codes.
(define-module (badb surfaces cli)
               #:use-module (term ansi-color)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:export (bold
                          cyan
                          green
                          red
                          supports-color?
                          cli-pad-right
                          format-cli-item
                          format-cli-kv
                          format-cli-table
                          format-cli-error))

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

;;;
;;; Alignment & Item Formatting Helpers
;;;

(define (cli-pad-right str width)
        "Pad STR on the right with spaces to at least WIDTH visible columns."
        (let* ((s (if (string? str) str (format #f "~a" str)))
               (len (string-length s)))
          (if (>= len width)
            s
            (string-append s (make-string (- width len) #\space)))))

(define* (format-cli-item port label desc
                          #:key (width 16) (indent 2) (bullet #f) (color cyan))
         "Format a single aligned CLI entry for PORT. Padding is computed from
the uncolored LABEL length so ANSI escapes never misalign columns."
         (let* ((pad-str (make-string (max 0 indent) #\space))
                (bullet-str (if bullet (string-append bullet " ") ""))
                (lbl (if (string? label) label (format #f "~a" label)))
                (gap (make-string (max 2 (- width (string-length lbl))) #\space))
                (colored-lbl (if (procedure? color) (color port lbl) lbl)))
           (format #f "~a~a~a~a~a" pad-str bullet-str colored-lbl gap desc)))

(define* (format-cli-table port entries
                           #:key (width #f) (indent 2) (gap 2) (bullet #f) (color cyan))
         "Format ENTRIES (alist of (label . description)) as cleanly aligned
lines for PORT."
         (if (null? entries)
           ""
           (let* ((max-len (fold (lambda (e acc)
                                   (max acc (string-length
                                              (if (string? (car e))
                                                (car e)
                                                (format #f "~a" (car e))))))
                                 0
                                 entries))
                  (col-w (or width (+ max-len (max 2 gap)))))
             (string-join
               (map (lambda (e)
                      (format-cli-item port (car e) (cdr e)
                                       #:width col-w
                                       #:indent indent
                                       #:bullet bullet
                                       #:color color))
                    entries)
               "\n"))))

(define* (format-cli-kv port key val #:key (width 16) (indent 2) (color bold))
         "Format an aligned CLI key-value row: '<indent><key>: <val>'."
         (let* ((pad-str (make-string (max 0 indent) #\space))
                (k-str (if (string? key) key (format #f "~a" key)))
                (padded (cli-pad-right (string-append k-str ":") width))
                (styled (if (procedure? color) (color port padded) padded)))
           (format #f "~a~a ~a" pad-str styled val)))

(define (format-cli-error port fmt . args)
        "Write a consistently prefixed 'Error: ...' message to PORT."
        (format port "~a ~a~%" (red port "Error:") (apply format #f fmt args)))
