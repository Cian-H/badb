(define-module (badb main) #:use-module
               (ice-9 format)
               #:use-module
               (badb surfaces cli)
               #:autoload
               (badb surfaces tui)
               (run-tui spec-tool-status spec-tool-verify)
               #:autoload
               (badb capabilities filetree-mcp)
               (run-filetree-mcp)
               #:autoload
               (badb runtimes synthesis)
               (run-synthesis-cli)
               #:export
               (%program-name %version main))

(define %program-name "badb")

(define %version "0.1.0")

(define (print-usage port)
        (format port
                "~a ~a [COMMAND] [OPTION]...\n\nMicroharness kernel.\n\n~a\n  ~a   launch interactive terminal user interface (default)\n  ~a          run bootstrap test suite\n  ~a  run the filetree MCP server over stdio\n  ~a   run the Sema plugin synthesis workflow\n  ~a         inspect or verify contract specifications (geasa)\n  ~a    display this help and exit\n  ~a output version information and exit\n"
                (bold port "Usage:")
                %program-name
                (bold port "Commands & Options:")
                (cyan port "tui, (none)")
                (cyan port "test")
                (cyan port "mcp-filetree")
                (cyan port "synthesize")
                (cyan port "geis")
                (cyan port "-h, --help")
                (cyan port "-V, --version")))

(define (main args)
        (let ((options (cdr args)))
          (cond
            ((or (null? options) (member (car options) '("tui" "-t" "--tui"))) (run-tui))
            ((equal? (car options) "test")
             (let ((runner (catch #t
                             (lambda () (module-ref (resolve-interface '(test runner)) 'run-all-tests))
                             (lambda _ #f))))
               (if runner
                   (runner)
                   (begin
                     (format (current-error-port) "Error: test runner module (test runner) not found.\n")
                     (exit 1)))))
            ((equal? (car options) "mcp-filetree") (run-filetree-mcp))
            ((equal? (car options) "synthesize") (run-synthesis-cli (cdr options)))
            ((equal? (car options) "geis")
             (let ((sub-args (cdr options)))
               (cond
                 ((or (null? sub-args) (member (car sub-args) '("help" "-h" "--help")))
                  (format #t "Usage: badb geis [status [fid]|list|verify <fid>|synthesize <fid>]\n\nInspect, verify, or synthesize contract specifications (geasa).\nTo dynamically synthesize a feature with an agent, launch the TUI and run:\n  /geis add <feature description>\n"))
                 ((equal? (car sub-args) "list")
                  (display (spec-tool-status)))
                 ((equal? (car sub-args) "status")
                  (display (apply spec-tool-status (cdr sub-args))))
                 ((equal? (car sub-args) "verify")
                  (if (null? (cdr sub-args))
                      (begin
                        (format (current-error-port) "Error: badb geis verify requires <feature-id>\n")
                        (exit 1))
                      (display (spec-tool-verify (cadr sub-args)))))
                 ((equal? (car sub-args) "synthesize")
                  (run-synthesis-cli (cdr sub-args)))
                 (else
                  (format (current-error-port) "Unknown geis subcommand: ~a\n" (car sub-args))
                  (exit 1)))))
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