(define-module (badb main) #:use-module
               (ice-9 format)
               #:use-module
               (badb cli)
               #:autoload
               (badb tui)
               (run-tui)
               #:autoload
               (test runner)
               (run-all-tests)
               #:autoload
               (badb filetree-mcp)
               (run-filetree-mcp)
               #:export
               (%program-name %version main))

(define %program-name "badb")

(define %version "0.1.0")

(define (print-usage port)
        (format port
                "~a ~a [COMMAND] [OPTION]...\n\nMicroharness kernel.\n\n~a\n  ~a   launch interactive terminal user interface (default)\n  ~a          run bootstrap test suite\n  ~a  run the filetree MCP server over stdio\n  ~a    display this help and exit\n  ~a output version information and exit\n"
                (bold port "Usage:")
                %program-name
                (bold port "Commands & Options:")
                (cyan port "tui, (none)")
                (cyan port "test")
                (cyan port "mcp-filetree")
                (cyan port "-h, --help")
                (cyan port "-V, --version")))

(define (main args)
        (let ((options (cdr args)))
          (cond
            ((or (null? options) (member (car options) '("tui" "-t" "--tui"))) (run-tui))
            ((equal? (car options) "test") (run-all-tests))
            ((equal? (car options) "mcp-filetree") (run-filetree-mcp))
            ((member (car options) '("-h" "--help")) (print-usage (current-output-port)))
            ((member (car options) '("-V" "--version"))
              (format #t "~a ~a~%" (bold (current-output-port) %program-name) %version))
            (else
              (format (current-error-port)
                      "~a: unrecognized option '~a'~%"
                      (red (current-error-port) %program-name)
                      (car options))
              (format (current-error-port) "Try 'badb --help' for more information.~%")
              (exit 1)))))