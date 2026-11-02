(define-module (badb main) #:use-module
               (ice-9 format)
               #:use-module
               (badb surfaces cli)
               #:autoload
               (geasa surfaces-tui plugin)
               (run-tui spec-tool-status spec-tool-verify)
               #:autoload
               (badb capabilities filetree-mcp)
               (run-filetree-mcp)
               #:autoload
               (geasa runtimes-synthesis plugin)
               (run-synthesis-cli synthesis-assemble-hook default-workflow-path)
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

(define (wire-synthesis-assemble!)
        (synthesis-assemble-hook
          (lambda (feature-id cassette target-path)
            (let*
                ((bundle (resolve-interface '(badb geis bundle)))
                 (pending ((module-ref bundle 'geis-take-pending-prompt))))
              ((module-ref bundle 'geis-bundle-assemble!)
               feature-id
               #:workflow-path
               (default-workflow-path)
               #:cassette-path
               cassette
               #:request
               (and pending (assoc-ref pending 'request))
               #:rendered-prompt
               (and pending (assoc-ref pending 'rendered))
               #:target-path
               target-path)))))

(define (main args)
        (let ((options (cdr args)))
          (cond
            ((or (null? options) (member (car options) '("tui" "-t" "--tui"))) (run-tui))
            ((equal? (car options) "test")
              (let
                  ((runner
                     (catch #t
                       (lambda ()
                         (module-ref (resolve-interface '(test runner)) 'run-all-tests))
                       (lambda _
                         #f))))
                (if runner
                  (runner)
                  (begin
                    (format (current-error-port)
                            "Error: test runner module (test runner) not found.\n")
                    (exit 1)))))
            ((equal? (car options) "mcp-filetree") (run-filetree-mcp))
            ((equal? (car options) "synthesize")
              (begin (wire-synthesis-assemble!) (run-synthesis-cli (cdr options))))
            ((equal? (car options) "geis")
              (let ((sub-args (cdr options)))
                (cond
                  ((or (null? sub-args) (member (car sub-args) '("help" "-h" "--help")))
                    (format #t
                            "Usage: badb geis [status [fid]|list|verify <fid>|synthesize <fid>|pack <fid>|install <dir>|replay <fid>]\n\nInspect, verify, or synthesize contract specifications (geasa).\npack builds a shareable tarball of a bundle from BADB_GEIS_PATH; install locks\na bundle (and its geis dependencies) into the kernel gate and verifies it.\nTo dynamically synthesize a feature with an agent, launch the TUI and run:\n  /geis add <feature description>\n"))
                  ((equal? (car sub-args) "list") (display (spec-tool-status)))
                  ((equal? (car sub-args) "status")
                    (display (apply spec-tool-status (cdr sub-args))))
                  ((equal? (car sub-args) "verify")
                    (cond
                      ((null? (cdr sub-args))
                        (format (current-error-port)
                                "Error: badb geis verify requires <feature-id> or --all\n")
                        (exit 1))
                      ((member (cadr sub-args) '("--all" "-a" "all"))
                        (let
                            ((status (system* "./scripts/verify-geasa.sh")))
                          (exit (status:exit-val status))))
                      (else (display (spec-tool-verify (cadr sub-args))))))
                  ((equal? (car sub-args) "synthesize")
                    (begin (wire-synthesis-assemble!) (run-synthesis-cli (cdr sub-args))))
                  ((and (equal? (car sub-args) "pack") (not (null? (cdr sub-args))))
                    (let
                        ((res
                           ((module-ref (resolve-interface '(badb geis bundle)) 'geis-bundle-pack)
                            (cadr sub-args))))
                      (format #t
                              "packed: ~a (bundle-sha256 ~a)~%"
                              (assoc-ref res 'tarball)
                              (assoc-ref res 'bundle-sha256))))
                  ((equal? (car sub-args) "pack")
                    (format (current-error-port) "Error: badb geis pack requires <feature-id>\n")
                    (exit 1))
                  ((and (equal? (car sub-args) "install") (not (null? (cdr sub-args))))
                    (let*
                        ((install
                           (module-ref (resolve-interface '(badb geis bundle)) 'geis-bundle-install))
                         (report (install (cadr sub-args))))
                      (format #t
                              "installed: ~a (bundle-sha256 ~a)\nlocked: ~a\nverify: ~a\n"
                              (assoc-ref report 'feature-id)
                              (assoc-ref report 'bundle-sha256)
                              (assoc-ref report 'locked)
                              (assoc-ref report 'verify-verdict))))
                  ((equal? (car sub-args) "replay")
                    (if (null? (cdr sub-args))
                      (begin
                        (format (current-error-port) "Error: badb geis replay requires <feature-id>\n")
                        (exit 1))
                      (let*
                          ((replay
                             (module-ref (resolve-interface '(badb geis bundle)) 'geis-bundle-replay))
                           (report
                             (catch 'geis-bundle-error
                               (lambda ()
                                 (replay (cadr sub-args)))
                               (lambda (key . args)
                                 (format (current-error-port) "Error: ~a~%" (car args))
                                 (exit 1)))))
                        (format #t
                                "replay: ~a (workflow-verdict ~a)\nplugin-match: ~a\npinned-hash: ~a\nreplayed-hash: ~a\n"
                                (assoc-ref report 'feature-id)
                                (assoc-ref report 'workflow-verdict)
                                (assoc-ref report 'plugin-match)
                                (assoc-ref report 'pinned-hash)
                                (assoc-ref report 'replayed-hash)))))
                  ((equal? (car sub-args) "install")
                    (format (current-error-port) "Error: badb geis install requires <bundle-dir>\n")
                    (exit 1))
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