(define-module (badb main) #:use-module
               (badb version)
               #:use-module
               (ice-9 format)
               #:use-module
               (badb surfaces cli)
               #:use-module
               (srfi srfi-13)
               #:autoload
               (geasa surfaces-tui plugin)
               (run-tui spec-tool-status
                        spec-tool-verify
                        parse-ui-listen-spec
                        run-ui-server!
                        run-remote-tui-client)
               #:autoload
               (badb capabilities filetree-mcp)
               (run-filetree-mcp)
               #:autoload
               (geasa runtimes-synthesis plugin)
               (run-synthesis-cli synthesis-assemble-hook
                                  default-workflow-path
                                  synthesis-bundle-dir)
               #:autoload
               (geasa runtimes-workflows plugin)
               (bootstrap-project-geasa! distill-exec-patterns! onboard-project-geasa!)
               #:autoload
               (geasa capabilities-code-nav plugin)
               (symbols-overview)
               #:autoload
               (geasa capabilities-context-index plugin)
               (make-bm25-index load-bm25-index format-ctx-stats)
               #:autoload
               (geasa surfaces-acp plugin)
               (run-acp-server acp-model-client)
               #:autoload
               (geasa agents-subagent plugin)
               (make-remote-superagent remote-superagent-start!
                                       remote-superagent-wait
                                       remote-superagent-port
                                       remote-superagent-socket-path)
               #:export
               (%program-name %version main))

(define %program-name "badb")

(define %version %badb-version)

(define (print-usage port)
        (format port
                "~a ~a [COMMAND] [OPTION]...\n\nMicroharness kernel.\n\n~a\n~a\n"
                (bold port "Usage:")
                %program-name
                (bold port "Commands & Options:")
                (format-cli-table
                  port
                  '(("tui, (none)"   . "launch interactive terminal user interface (default)")
                    ("init"          . "initialize project geasa, workflows & memories")
                    ("bootstrap"     . "fetch, verify, and lock base geasa into user store")
                    ("onboard"       . "inspect workspace toolchain and synthesize baseline geasa")
                    ("test"          . "run bootstrap test suite")
                    ("mcp-filetree"  . "run the filetree MCP server over stdio")
                    ("synthesize"    . "run the Sema plugin synthesis workflow")
                    ("geis"          . "inspect, verify, or distill contract specifications (geasa)")
                    ("ctx"           . "show BM25 context index token statistics")
                    ("acp"           . "serve the Agent Client Protocol over stdio (editor surface)")
                    ("-h, --help"    . "display this help and exit")
                    ("-V, --version" . "output version information and exit"))
                  #:width 16)))

(define (wire-synthesis-assemble!)
        (synthesis-assemble-hook
          (lambda (feature-id cassette target-path)
            (let*
                ((bundle (resolve-interface '(badb geis bundle)))
                 (pending ((module-ref bundle 'geis-take-pending-prompt))))
              ((module-ref bundle 'geis-bundle-assemble!)
               feature-id
               #:bundle-dir
               (synthesis-bundle-dir feature-id target-path)
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

(define (run-onboard-command sub-args)
        (let*
            ((args (cdr sub-args))
             (ws
               (cond
                 ((and (pair? args) (equal? (car args) "--workspace") (pair? (cdr args)))
                   (cadr args))
                 ((and (pair? args) (not (string-prefix? "-" (car args)))) (car args))
                 (else (getcwd))))
             (res (onboard-project-geasa! ws)))
          (format #t
                  "Onboarded project (~a): ~s~%"
                  (assoc-ref res 'language)
                  (assoc-ref res 'bundles))))

(define (run-geis-replay sub-args)
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
                    "~a\n~a\n~a\n~a\n~a\n"
                    (format-cli-kv (current-output-port) "replay"
                                   (format #f "~a (workflow-verdict ~a)"
                                           (assoc-ref report 'feature-id)
                                           (assoc-ref report 'workflow-verdict))
                                   #:indent 0 #:width 15 #:color #f)
                    (format-cli-kv (current-output-port) "plugin-match"
                                   (assoc-ref report 'plugin-match)
                                   #:indent 0 #:width 15 #:color #f)
                    (format-cli-kv (current-output-port) "pinned-hash"
                                   (assoc-ref report 'pinned-hash)
                                   #:indent 0 #:width 15 #:color #f)
                    (format-cli-kv (current-output-port) "replayed-hash"
                                   (assoc-ref report 'replayed-hash)
                                   #:indent 0 #:width 15 #:color #f)
                    ""))))

(define (run-geis-pin sub-args)
        (let ((dirs (cdr sub-args)))
          (if (null? dirs)
            (begin
              (format (current-error-port)
                      "Error: badb geis pin requires <bundle-dir> [...]\n")
              (exit 1))
            (let
                ((pin! (module-ref (resolve-interface '(badb geis bundle)) 'geis-bundle-pin!)))
              (for-each
                (lambda (d)
                  (catch 'geis-bundle-error
                    (lambda ()
                      (let ((pins (pin! d))) (format #t "Re-pinned ~a (~a files)\n" d (length pins))))
                    (lambda (key . args)
                      (format (current-error-port) "Error: ~a\n" (car args))
                      (exit 1))))
                dirs)))))

(define (run-geis-install sub-args)
        (let*
            ((install
               (module-ref (resolve-interface '(badb geis bundle)) 'geis-bundle-install))
             (report (install (cadr sub-args))))
          (format #t
                  "~a\n~a\n~a\n"
                  (format-cli-kv (current-output-port) "installed"
                                 (format #f "~a (bundle-sha256 ~a)"
                                         (assoc-ref report 'feature-id)
                                         (assoc-ref report 'bundle-sha256))
                                 #:indent 0 #:width 11 #:color #f)
                  (format-cli-kv (current-output-port) "locked"
                                 (assoc-ref report 'locked)
                                 #:indent 0 #:width 11 #:color #f)
                  (format-cli-kv (current-output-port) "verify"
                                 (assoc-ref report 'verify-verdict)
                                 #:indent 0 #:width 11 #:color #f))))

(define (run-geis-pack sub-args)
        (if (member (cadr sub-args) '("--all" "-a" "all"))
          (let ((res ((module-ref (resolve-interface '(badb geis bundle))
                                  'geis-pack-base-collection)
                      #:version %version)))
            (format #t
                    "~a\n"
                    (format-cli-kv (current-output-port) "packed"
                                   (format #f "~a (~a bundles)"
                                           (assoc-ref res 'tarball)
                                           (assoc-ref res 'bundle-count))
                                   #:indent 0 #:width 8 #:color #f)))
          (let
              ((res
                 ((module-ref (resolve-interface '(badb geis bundle)) 'geis-bundle-pack)
                  (cadr sub-args))))
            (format #t
                    "~a\n"
                    (format-cli-kv (current-output-port) "packed"
                                   (format #f "~a (bundle-sha256 ~a)"
                                           (assoc-ref res 'tarball)
                                           (assoc-ref res 'bundle-sha256))
                                   #:indent 0 #:width 8 #:color #f)))))

(define (run-geis-verify sub-args)
        (cond
          ((null? (cdr sub-args))
            (format (current-error-port)
                    "Error: badb geis verify requires <feature-id> or --all\n")
            (exit 1))
          ((member (cadr sub-args) '("--all" "-a" "all"))
            (let
                ((status (system* "./scripts/verify-geasa")))
              (exit (status:exit-val status))))
          (else
            (display (spec-tool-verify (cadr sub-args)))
            (format (current-error-port)
                    "Note: verify replays the locked copy in .badb/ (read-only, generated). If you edited src/geasa/~a, re-pin and re-lock via ./scripts/verify-geasa; never copy files into .badb/ by hand.~%"
                    (cadr sub-args)))))

(define (run-geis-distill sub-args)
        (let*
            ((analyze? (member "--analyze-only" (cdr sub-args)))
             (res
               (distill-exec-patterns! #:workspace (getcwd) #:analyze-only? (and analyze? #t))))
          (format #t "distill: ~s~%" res)))

(define (run-geis-model sub-args)
        (let* ((cfg-mod (resolve-interface '(geasa foundation-config plugin)))
               (load-cfg (module-ref cfg-mod 'load-config))
               (save-cfg! (module-ref cfg-mod 'save-local-config!))
               (get-model (module-ref cfg-mod 'badb-config-synthesis-model))
               (set-model! (module-ref cfg-mod 'badb-config-set-synthesis-model!))
               (cfg (load-cfg))
               (args (cdr sub-args)))
          (if (null? args)
            (let ((configured (get-model cfg #f)))
              (format #t "Geis synthesis model: ~a~a~%"
                      (or configured (get-model cfg))
                      (if configured
                        " (configured in .badb/config.scm)"
                        " (default: qwen3.6-35b-a3b)")))
            (let ((new-model (car args)))
              (set-model! cfg new-model)
              (save-cfg! cfg)
              (format #t "Geis synthesis model set to: ~a (saved to .badb/config.scm)~%" new-model)))))

(define (%parse-geis-scope-args args)
        "Strip --user / --project from ARGS, returning (values scope explicit-scope? rest)."
        (let loop ((rest args) (scope 'user) (explicit? #f) (acc '()))
                  (cond
                    ((null? rest) (values scope explicit? (reverse acc)))
                    ((equal? (car rest) "--user") (loop (cdr rest) 'user #t acc))
                    ((equal? (car rest) "--project") (loop (cdr rest) 'project #t acc))
                    (else (loop (cdr rest) scope explicit? (cons (car rest) acc))))))

(define (%parse-geis-add-spec spec-str args)
        "Parse <url>#<subpath>[@<ref>] and optional --path / --ref flags."
        (let* ((hash-idx (string-index spec-str #\#))
               (url (if hash-idx (substring spec-str 0 hash-idx) spec-str))
               (rest-spec (if hash-idx (substring spec-str (+ hash-idx 1)) #f))
               (at-idx (and rest-spec (string-index rest-spec #\@)))
               (subpath (if at-idx (substring rest-spec 0 at-idx) rest-spec))
               (ref (if at-idx (substring rest-spec (+ at-idx 1)) "main")))
          (let loop ((a args) (p subpath) (r ref))
                    (cond
                      ((null? a) (values url p r))
                      ((and (equal? (car a) "--path") (pair? (cdr a)))
                        (loop (cddr a) (cadr a) r))
                      ((and (equal? (car a) "--ref") (pair? (cdr a)))
                        (loop (cddr a) p (cadr a)))
                      (else (loop (cdr a) p r))))))

(define (run-geis-pkg-subcommand cmd args scope)
        (let* ((pkg-mod (resolve-interface '(geasa runtimes-geis-pkg plugin)))
               (cfg-mod (resolve-interface '(geasa foundation-config plugin))))
          (case (string->symbol cmd)
            ((add)
              (when (null? args)
                (format (current-error-port) "Error: badb geis add requires <url>#<subpath>[@<ref>]\n")
                (exit 1))
              (call-with-values
                (lambda () (%parse-geis-add-spec (car args) (cdr args)))
                (lambda (url subpath ref)
                  (unless (and url subpath (not (string-null? subpath)))
                    (format (current-error-port) "Error: badb geis add requires <subpath> (via #<subpath> or --path)\n")
                    (exit 1))
                  (let ((res ((module-ref pkg-mod 'geis-pkg-install!)
                              url subpath #:ref ref #:scope scope)))
                    (if (assoc-ref res 'ok)
                      (format #t "Installed ~a (~a) in ~a scope\n"
                              (assoc-ref res 'feature-id) (assoc-ref res 'version) scope)
                      (begin
                        (format (current-error-port) "Error: ~a\n" (or (assoc-ref res 'reason) "install failed"))
                        (exit 1)))))))
            ((remove)
              (when (null? args)
                (format (current-error-port) "Error: badb geis remove requires <fid>\n")
                (exit 1))
              (let ((res ((module-ref pkg-mod 'geis-pkg-remove!) (car args) #:scope scope)))
                (if (assoc-ref res 'ok)
                  (format #t "Removed ~a from ~a scope\n" (car args) scope)
                  (begin
                    (format (current-error-port) "Error: ~a\n" (or (assoc-ref res 'reason) "remove failed"))
                    (exit 1)))))
            ((pin unpin)
              (when (null? args)
                (format (current-error-port) "Error: badb geis ~a requires <fid>\n" cmd)
                (exit 1))
              (let* ((pin? (equal? cmd "pin"))
                     (res ((module-ref pkg-mod 'geis-pkg-pin!) (car args) #:scope scope #:pinned? pin?)))
                (if (assoc-ref res 'ok)
                  (format #t "~a ~a in ~a scope\n" (if pin? "Pinned" "Unpinned") (car args) scope)
                  (begin
                    (format (current-error-port) "Error: ~a\n" (or (assoc-ref res 'reason) "pin failed"))
                    (exit 1)))))
            ((rollback)
              (when (null? args)
                (format (current-error-port) "Error: badb geis rollback requires <fid>\n")
                (exit 1))
              (let ((res ((module-ref pkg-mod 'geis-pkg-rollback!) (car args) #:scope scope)))
                (if (assoc-ref res 'ok)
                  (format #t "Rolled back ~a in ~a scope\n" (car args) scope)
                  (begin
                    (format (current-error-port) "Error: ~a\n" (or (assoc-ref res 'reason) "rollback failed"))
                    (exit 1)))))
            ((enable disable)
              (when (null? args)
                (format (current-error-port) "Error: badb geis ~a requires <fid>\n" cmd)
                (exit 1))
              (let* ((fn (module-ref pkg-mod (if (equal? cmd "enable") 'geis-pkg-enable! 'geis-pkg-disable!)))
                     (res (fn (car args) #:scope scope)))
                (if (assoc-ref res 'ok)
                  (format #t "~a ~a in ~a scope\n" (if (equal? cmd "enable") "Enabled" "Disabled") (car args) scope)
                  (begin
                    (format (current-error-port) "Error: ~a\n" (or (assoc-ref res 'reason) "operation failed"))
                    (exit 1)))))
            ((sync)
              (let* ((cfg ((module-ref cfg-mod 'load-config)))
                     (declared ((module-ref cfg-mod 'badb-config-geasa) cfg #:scope scope))
                     (res ((module-ref pkg-mod 'geis-pkg-sync!) declared #:scope scope)))
                (format #t "Synced ~a scope: ~a installed, ~a unchanged, ~a failed\n"
                        scope
                        (length (or (assoc-ref res 'installed) '()))
                        (length (or (assoc-ref res 'unchanged) '()))
                        (length (or (assoc-ref res 'failed) '())))
                (unless (assoc-ref res 'ok) (exit 1))))
            ((update)
              (let* ((force? (and (member "--force" args) #t))
                     (targets (filter (lambda (a) (not (equal? a "--force"))) args))
                     (entries ((module-ref pkg-mod 'geis-pkg-list) scope))
                     (selected (if (null? targets)
                                 entries
                                 (filter (lambda (e) (member (car e) targets)) entries))))
                (for-each
                  (lambda (entry)
                    (let* ((fid (car entry))
                           (props (cdr entry))
                           (src (assoc-ref props 'source))
                           (pinned? (assoc-ref props 'pinned?))
                           (dirty? ((module-ref pkg-mod 'geis-pkg-locally-modified?) scope fid)))
                      (cond
                        ((and (not force?) pinned?)
                          (format #t "Skipped ~a (pinned)\n" fid))
                        ((and (not force?) dirty?)
                          (format #t "Skipped ~a (locally modified)\n" fid))
                        ((and (list? src) (assoc-ref src 'url) (assoc-ref src 'path))
                          (let ((res ((module-ref pkg-mod 'geis-pkg-install!)
                                      (assoc-ref src 'url)
                                      (assoc-ref src 'path)
                                      #:ref (or (assoc-ref src 'ref) "main")
                                      #:scope scope
                                      #:pin? (and pinned? #t))))
                            (if (assoc-ref res 'ok)
                              (format #t "Updated ~a (~a)\n" fid (assoc-ref res 'version))
                              (format (current-error-port) "Failed ~a: ~a\n" fid (assoc-ref res 'reason)))))
                        (else
                          (format #t "Skipped ~a (no git source)\n" fid)))))
                  selected)))
            ((list)
              (let ((entries ((module-ref pkg-mod 'geis-pkg-list) scope)))
                (if (null? entries)
                  (format #t "No geasa in ~a scope.\n" scope)
                  (for-each
                    (lambda (entry)
                      (let* ((fid (car entry))
                             (props (cdr entry))
                             (ver (or (assoc-ref props 'version) "?"))
                             (en? (if (assoc 'enabled? props) (assoc-ref props 'enabled?) #t))
                             (pin? (assoc-ref props 'pinned?))
                             (src (assoc-ref props 'source))
                             (origin (if (list? src) (or (assoc-ref src 'url) "local") "local")))
                        (format #t "~a\t~a\t~a\t~a\t~a\n"
                                fid ver (if en? "enabled" "disabled")
                                (if pin? "pinned" "unpinned") origin)))
                    entries)))))))

(define (run-geis-command options)
        (call-with-values
          (lambda () (%parse-geis-scope-args (cdr options)))
          (lambda (scope explicit-scope? sub-args)
            (cond
              ((or (null? sub-args) (member (car sub-args) '("help" "-h" "--help")))
                (let ((out (current-output-port)))
                  (format out
                          "~a badb geis [COMMAND]\n\nInspect, verify, or synthesize contract specifications (geasa).\n\n~a\n~a\n\n~a\n  Launch the TUI and run: /geis add <feature description>\n"
                          (bold out "Usage:")
                          (bold out "Subcommands:")
                          (format-cli-table
                            out
                            '(("status [fid]"             . "check status/hash of locked spec(s)")
                              ("list"                     . "list all locked feature contracts (geasa)")
                              ("verify <fid|--all>"       . "replay the LOCKED copy in .badb/ (not src/geasa/); run ./scripts/verify-geasa to re-lock first")
                              ("model [name]"             . "inspect or set default model for geis contract synthesis")
                              ("synthesize <fid>"         . "run Sema plugin synthesis for a locked feature")
                              ("pack <fid>"               . "build a shareable bundle tarball from BADB_GEIS_PATH")
                              ("install <dir>"            . "lock a bundle and its geis dependencies, then verify")
                              ("replay <fid>"             . "replay synthesis cassette and compare hashes")
                              ("pin <dir...>"             . "recompute and write sha256 pins in bundle manifest(s)")
                              ("distill [--analyze-only]" . "analyze execution trajectory and distill into a bundle")
                              ("onboard [--workspace d]"  . "inspect workspace toolchain and synthesize baseline geasa"))
                            #:width 27)
                          (bold out "Dynamic Synthesis:"))))
              ((equal? (car sub-args) "onboard") (run-onboard-command sub-args))
              ((equal? (car sub-args) "distill") (run-geis-distill sub-args))
              ((equal? (car sub-args) "model") (run-geis-model sub-args))
              ((equal? (car sub-args) "list")
                (if explicit-scope?
                  (run-geis-pkg-subcommand "list" (cdr sub-args) scope)
                  (display (spec-tool-status))))
              ((equal? (car sub-args) "status")
                (display (apply spec-tool-status (cdr sub-args))))
              ((equal? (car sub-args) "verify") (run-geis-verify sub-args))
              ((equal? (car sub-args) "synthesize")
                (begin (wire-synthesis-assemble!) (run-synthesis-cli (cdr sub-args))))
              ((and (equal? (car sub-args) "pack") (not (null? (cdr sub-args))))
                (run-geis-pack sub-args))
              ((equal? (car sub-args) "pack")
                (format (current-error-port) "Error: badb geis pack requires <feature-id>\n")
                (exit 1))
              ((and (equal? (car sub-args) "install") (not (null? (cdr sub-args))))
                (run-geis-install sub-args))
              ((equal? (car sub-args) "replay") (run-geis-replay sub-args))
              ((equal? (car sub-args) "install")
                (format (current-error-port) "Error: badb geis install requires <bundle-dir>\n")
                (exit 1))
              ((member (car sub-args) '("pin" "repin"))
                (if (or explicit-scope?
                        (and (equal? (car sub-args) "pin")
                             (pair? (cdr sub-args))
                             (not (file-exists? (cadr sub-args)))))
                  (run-geis-pkg-subcommand "pin" (cdr sub-args) scope)
                  (run-geis-pin sub-args)))
              ((member (car sub-args) '("add" "remove" "update" "sync" "unpin" "rollback" "enable" "disable"))
                (run-geis-pkg-subcommand (car sub-args) (cdr sub-args) scope))
              (else
                (format (current-error-port) "Error: unknown geis subcommand '~a'\n" (car sub-args))
                (exit 1))))))

(define (run-init-command)
        (let ((workspace (getcwd)))
          (bootstrap-project-geasa! workspace)
          (catch #t
            (lambda ()
              (let*
                  ((scan-dir
                     (if
                         (file-exists? (string-append workspace "/src"))
                       (string-append workspace "/src")
                       workspace))
                   (outline (symbols-overview scan-dir))
                   (mem-file (string-append workspace "/.badb/memories/symbols.md")))
                (when (and outline (not (string-null? outline)))
                  (call-with-output-file mem-file
                                         (lambda (p)
                                           (format p "# Project Symbols Outline~%~%~a~%" outline))))))
            (lambda _
              #f))
          (let ((out (current-output-port)))
            (format out "Initialized badb project infrastructure in ~a\n~a\n"
                    workspace
                    (format-cli-table
                      out
                      '((".badb/geasa/"               . "dynamic project capabilities")
                        (".badb/workflows/verify.sema" . "declarative workflows")
                        (".badb/memories/"            . "persistent project knowledge")
                        (".badb/rules.scm"            . "project constraints"))
                      #:bullet "•"
                      #:width 30)))))

(define (run-ctx-command options)
        (let ((sub-args (cdr options)))
          (cond
            ((or (null? sub-args) (equal? (car sub-args) "stats"))
              (let* ((index-path ".badb/context-index.scm")
                     (idx (if (file-exists? index-path)
                            (load-bm25-index index-path)
                            (make-bm25-index))))
                (display (format-ctx-stats idx))
                (newline)))
            (else (format (current-error-port) "Usage: badb ctx stats\n") (exit 1)))))

(define (run-test-command)
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

(define (run-acp-command options)
        (let*
            ((args (cdr options)) (permission 'allow)
             (endpoint #f)
             (model #f)
             (listen-addr #f))
          (for-each
            (lambda (arg)
              (cond
                ((string-prefix? "--permission=" arg)
                  (set! permission (string->symbol (substring arg 13))))
                ((string-prefix? "--endpoint=" arg) (set! endpoint (substring arg 11)))
                ((string-prefix? "--model=" arg) (set! model (substring arg 8)))
                ((string-prefix? "--listen=" arg) (set! listen-addr (substring arg 9)))))
            args)
          (if (memq permission '(allow ask))
            (cond
              ((and listen-addr (string-prefix? "tcp://" listen-addr))
                (let*
                    ((spec (substring listen-addr 6))
                     (colon (string-index-right spec #\:))
                     (host
                       (if colon
                         (substring spec 0 colon)
                         "127.0.0.1"))
                     (port
                       (if colon
                         (or (string->number (substring spec (+ colon 1))) 0)
                         0))
                     (mc (acp-model-client #:endpoint endpoint #:model model))
                     (sup
                       (make-remote-superagent #:transport
                                               'tcp
                                               #:host
                                               host
                                               #:port
                                               port
                                               #:model-client
                                               mc
                                               #:permission-policy
                                               permission)))
                  (remote-superagent-start! sup)
                  (format (current-error-port)
                          "badb acp listening on tcp://~a:~a~%"
                          host
                          (remote-superagent-port sup))
                  (remote-superagent-wait sup)))
              ((and listen-addr (string-prefix? "unix://" listen-addr))
                (let*
                    ((sock-path (substring listen-addr 7))
                     (mc (acp-model-client #:endpoint endpoint #:model model))
                     (sup
                       (make-remote-superagent #:transport
                                               'unix
                                               #:socket-path
                                               sock-path
                                               #:model-client
                                               mc
                                               #:permission-policy
                                               permission)))
                  (remote-superagent-start! sup)
                  (format (current-error-port)
                          "badb acp listening on unix://~a~%"
                          (remote-superagent-socket-path sup))
                  (remote-superagent-wait sup)))
              (listen-addr
                (format (current-error-port)
                        "Error: badb acp --listen must be tcp://HOST:PORT or unix://PATH\n")
                (exit 1))
              (else
                (run-acp-server #:permission-policy
                                permission
                                #:endpoint
                                endpoint
                                #:model
                                model)))
            (begin
              (format (current-error-port)
                      "Error: badb acp --permission must be allow or ask\n")
              (exit 1)))))

(define (run-bootstrap-command options)
        (let loop ((args (cdr options)) (from #f) (no-compile? #f))
                  (cond
                    ((null? args)
                      (let* ((bootstrap (module-ref (resolve-interface '(badb geis bundle))
                                                    'geis-bootstrap-base!))
                             (res (bootstrap #:from from #:no-compile? no-compile?))
                             (installed (assoc-ref res 'installed))
                             (failed (assoc-ref res 'failed)))
                        (format #t "Bootstrapped ~a base geasa into ~a (~a failed)~%"
                                (length installed)
                                (assoc-ref res 'store)
                                (length failed))
                        (unless (null? failed)
                          (format (current-error-port) "Failed bundles: ~s~%" failed)
                          (exit 1))))
                    ((and (equal? (car args) "--from") (pair? (cdr args)))
                      (loop (cddr args) (cadr args) no-compile?))
                    ((equal? (car args) "--no-compile")
                      (loop (cdr args) from #t))
                    (else (loop (cdr args) from no-compile?)))))

(define (run-tui-command args)
        (let loop
          ((rest args) (serve #f) (attach #f) (ssh-target #f))
          (cond
            ((null? rest)
              (cond
                (serve
                  (let ((spec (parse-ui-listen-spec serve)))
                    (unless spec
                      (format (current-error-port)
                              "Error: badb tui --serve must be stdio, unix://PATH, or tcp://HOST:PORT\n")
                      (exit 1))
                    (run-ui-server! #:transport
                                    (assoc-ref spec 'transport)
                                    #:host
                                    (or (assoc-ref spec 'host) "127.0.0.1")
                                    #:port
                                    (or (assoc-ref spec 'port) 0)
                                    #:socket-path
                                    (assoc-ref spec 'socket-path))))
                (attach
                  (let ((spec (parse-ui-listen-spec attach)))
                    (unless spec
                      (format (current-error-port)
                              "Error: badb tui --attach must be stdio, unix://PATH, or tcp://HOST:PORT\n")
                      (exit 1))
                    (run-remote-tui-client #:transport
                                           (assoc-ref spec 'transport)
                                           #:host
                                           (or (assoc-ref spec 'host) "127.0.0.1")
                                           #:port
                                           (or (assoc-ref spec 'port) 0)
                                           #:socket-path
                                           (assoc-ref spec 'socket-path))))
                (ssh-target (run-remote-tui-client #:transport 'ssh #:ssh-target ssh-target))
                (else (run-tui))))
            ((and (equal? (car rest) "--serve") (pair? (cdr rest)))
              (loop (cddr rest) (cadr rest) attach ssh-target))
            ((and (equal? (car rest) "--attach") (pair? (cdr rest)))
              (loop (cddr rest) serve (cadr rest) ssh-target))
            ((and (equal? (car rest) "--ssh") (pair? (cdr rest)))
              (loop (cddr rest) serve attach (cadr rest)))
            (else
              (format (current-error-port) "Error: unrecognized tui option '~a'\n" (car rest))
              (exit 1)))))

(define (main args)
        (let ((options (cdr args)))
          (cond
            ((or (null? options) (member (car options) '("tui" "-t" "--tui")))
              (run-tui-command
                (if (null? options)
                  '()
                  (cdr options))))
            ((equal? (car options) "init") (run-init-command))
            ((equal? (car options) "bootstrap") (run-bootstrap-command options))
            ((equal? (car options) "ctx") (run-ctx-command options))
            ((equal? (car options) "test") (run-test-command))
            ((equal? (car options) "mcp-filetree") (run-filetree-mcp))
            ((equal? (car options) "onboard") (run-onboard-command options))
            ((equal? (car options) "synthesize")
              (begin (wire-synthesis-assemble!) (run-synthesis-cli (cdr options))))
            ((equal? (car options) "geis") (run-geis-command options))
            ((equal? (car options) "acp") (run-acp-command options))
            ((member (car options) '("-h" "--help")) (print-usage (current-output-port)))
            ((member (car options) '("-V" "--version"))
              (format #t "~a ~a~%" (bold (current-output-port) %program-name) %version))
            (else
              (format (current-error-port)
                      "~a: unrecognized option '~a'~%"
                      %program-name
                      (car options))
              (print-usage (current-error-port))
              (exit 1)))))