(define-module (badb main)
  #:use-module (ice-9 format)
  #:use-module (badb cli)
  #:export (%program-name %version main))

(define %program-name "badb")
(define %version "0.1.0")

(define (print-usage port)
  (format port "~a ~a [COMMAND] [OPTION]...

Microharness kernel.

~a
  ~a   launch interactive terminal user interface (default)
  ~a          run bootstrap test suite
  ~a    display this help and exit
  ~a output version information and exit
"
          (bold port "Usage:") %program-name
          (bold port "Commands & Options:")
          (cyan port "tui, (none)")
          (cyan port "test")
          (cyan port "-h, --help")
          (cyan port "-V, --version")))

(define (main args)
  (let ((options (cdr args)))
    (cond ((or (null? options)
               (member (car options) '("tui" "-t" "--tui")))
           (let ((tui-module (resolve-module '(badb tui))))
             ((module-ref tui-module 'run-tui))))
          ((equal? (car options) "test")
           (let ((runner-module (resolve-module '(test runner))))
             ((module-ref runner-module 'run-all-tests))))
          ((member (car options) '("-h" "--help"))
           (print-usage (current-output-port)))
          ((member (car options) '("-V" "--version"))
           (format #t "~a ~a~%"
                   (bold (current-output-port) %program-name)
                   %version))
          (else
           (format (current-error-port) "~a: unrecognized option '~a'~%"
                   (red (current-error-port) %program-name)
                   (car options))
           (format (current-error-port) "Try 'badb --help' for more information.~%")
           (exit 1)))))
