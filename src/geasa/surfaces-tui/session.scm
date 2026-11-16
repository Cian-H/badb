;;; (geasa surfaces-tui session) — Shell/REPL Evaluation & Session Persistence
;;;
;;; Extracted from surfaces-tui plugin.scm. Provides input classification,
;;; Scheme REPL evaluation, shell execution with exec-logging, session persistence
;;; (/resume), and conversation transcript formatting.

(define-module (geasa surfaces-tui session)
               #:use-module
               (geasa capabilities-history-tools plugin)
               #:use-module
               (geasa capabilities-mcp plugin)
               #:use-module
               (geasa foundation-config plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (geasa policies-guardrail plugin)
               #:use-module
               (geasa runtimes-context plugin)
               #:use-module
               (badb kernel journal)
               #:use-module
               (badb kernel registry)
               #:use-module
               (json)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 regex)
               #:use-module
               (ice-9 threads)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-19)
               #:export
               (classify-input normalize-scheme-expr
                               make-repl-mod
                               eval-scheme-code
                               repl-help
                               repl-inspect
                               repl-geasa
                               repl-state
                               %exec-log-path
                               %append-exec-log!
                               resolve-user-shell
                               shell-kind
                               build-shell-command-argv
                               command-invokes-sudo?
                               sudo-password-prompt?
                               redact-shell-secrets
                               inject-sudo-stdin-flag
                               execute-shell-command
                               resolve-external-editor
                               build-editor-command
                               open-in-external-editor
                               read-via-external-editor
                               %session-filename-max-length
                               default-sessions-dir
                               session-file-path
                               turn->jsonl-line
                               persist-session-file!
                               load-session-file
                               recent-session-files
                               session-mtime-string
                               session-turns->transcript-messages
                               session-file-metadata
                               recent-session-summaries
                               format-conversation-transcript))

;;;
;;; Shell & REPL Execution Helpers
;;;
(define (classify-input input)
        "Classify trimmed user INPUT into an action tag and payload:
   '(slash cmd . arg)
   '(shell . cmd-string)
   '(scheme . code-string)
   '(chat . text)
   #f for empty or whitespace-only input"
        (let ((trimmed (string-trim-both input)))
          (cond
            ((string-null? trimmed)
              #f)
            ((string-prefix? "!" trimmed)
              (cons 'shell (string-trim-both (substring trimmed 1))))
            ((string-prefix? "(" trimmed)
              (cons 'scheme trimmed))
            ((string-prefix? "/" trimmed)
              (let* ((without-slash (substring trimmed 1))
                     (space-pos (string-index without-slash #\space)))
                (if space-pos
                  (list 'slash
                        (substring without-slash 0 space-pos)
                        (string-trim-both (substring without-slash (+ space-pos 1))))
                  (list 'slash without-slash ""))))
            (else
              (cons 'chat trimmed)))))

(define (normalize-scheme-expr str)
        "Normalize user Scheme input string, ensuring s-expressions without outer
parentheses (e.g. '+ 1 2' or '+ 1 2)') are wrapped for evaluation."
        (let ((trimmed (string-trim-both str)))
          (cond
            ((string-null? trimmed) trimmed)
            ;; Already properly wrapped
            ((string-prefix? "(" trimmed) trimmed)
            ;; Literal string or quote
            ((or (string-prefix? "\"" trimmed) (string-prefix? "'" trimmed)) trimmed)
            ;; Ends with ')' but missing '(': e.g. "+ 1 2)"
            ((and (string-suffix? ")" trimmed) (not (string-prefix? "(" trimmed)))
              (string-append "(" trimmed))
            ;; Has spaces and no outer parens: e.g. "+ 1 2"
            ((string-contains trimmed " ")
              (string-append "(" trimmed ")"))
            ;; Standalone identifier or literal number: e.g. "reg", "42"
            (else trimmed))))

(define* (repl-help #:optional (topic #f))
         "Return formatted interactive Scheme REPL help text, optionally focused on TOPIC."
         (let ((t (and topic (string-downcase (ensure-string topic)))))
           (cond
             ((equal? t "reg")
               "reg (<registry>): inspect with (registry-list reg), (registry-count reg), (registry-lookup reg 'id), (repl-inspect reg)")
             ((equal? t "jrn")
               "jrn (<journal>): inspect with (journal-count jrn), (journal-entries jrn), (journal-latest jrn), (repl-inspect jrn)")
             ((equal? t "geasa")
               "(repl-geasa [feature-id]): list registered geis entries and workspace bundles under src/geasa/, or inspect one feature-id.")
             ((equal? t "state")
               "(repl-state): return an alist snapshot of registry, journal, model config, MCP servers, guardrail level, and bound handles.")
             (else
               (string-join
                 '("badb Scheme REPL Introspection:"
                   "  (repl-help [topic])   Show REPL help (topics: reg, jrn, geasa, state)"
                   "  (repl-inspect obj)    Inspect any registry, journal, config, list, or procedure"
                   "  (repl-geasa [fid])    List registered geasa & workspace bundles (or inspect fid)"
                   "  (repl-state)          Snapshot live harness bindings & counts"
                   "Bindings: reg jrn mgr client cfg mcp-mgr policy compactor rollback-mgr verifier composer")
                 "\n")))))

(define (repl-inspect obj)
        "Return a human-readable diagnostic summary of Scheme object OBJ."
        (cond
          ((registry? obj)
            (let ((ids (map ensure-string (registry-list obj))))
              (format #f "#<registry count=~a entries=~s>" (registry-count obj) ids)))
          ((journal? obj)
            (let ((latest (journal-latest obj)))
              (format #f "#<journal count=~a path=~s latest=~s>"
                      (journal-count obj)
                      (journal-path obj)
                      (and latest (journal-entry-feature-id latest)))))
          ((badb-config? obj)
            (format #f "#<badb-config model=~s endpoint=~s theme=~s>"
                    (badb-config-model-name obj)
                    (badb-config-model-endpoint obj)
                    (badb-config-ui-theme obj)))
          ((mcp-manager? obj)
            (let ((servers (mcp-manager-list-servers obj)))
              (format #f "#<mcp-manager servers=~a>" (length servers))))
          ((guardrail-policy? obj)
            (format #f "#<guardrail-policy level=~a>" (guardrail-policy-level obj)))
          ((procedure? obj)
            (format #f "#<procedure name=~s>" (or (procedure-name obj) 'anonymous)))
          ((list? obj)
            (format #f "#<list length=~a preview=~s>"
                    (length obj)
                    (if (> (length obj) 6) (take obj 6) obj)))
          ((pair? obj)
            (format #f "#<pair ~s>" obj))
          ((vector? obj)
            (format #f "#<vector length=~a>" (vector-length obj)))
          (else
            (format #f "~s" obj))))

(define (%list-workspace-geasa-dirs)
        "Return sorted feature-id directory names found under src/geasa/."
        (if (file-exists? "src/geasa")
          (sort (scandir-valid-entries "src/geasa"
                                       (lambda (name)
                                         (file-is-directory? (string-append "src/geasa/" name))))
                string<?)
          '()))

(define* (repl-geasa #:optional (feature-id #f) (reg #f))
         "Inspect geis registrations in REG and bundle directories in src/geasa/.
Without FEATURE-ID, return an alist of registered entries and available bundles.
With FEATURE-ID, return an alist describing that feature or #f if unknown."
         (let ((registered (if (registry? reg)
                             (map ensure-string (registry-list reg))
                             '()))
               (bundles (%list-workspace-geasa-dirs)))
           (if feature-id
             (let* ((fid (ensure-string feature-id))
                    (in-reg? (and (registry? reg)
                                  (and (registry-lookup reg (string->symbol fid)) #t)))
                    (in-bundles? (and (member fid bundles) #t))
                    (manifest-path (string-append "src/geasa/" fid "/manifest.scm")))
               (if (or in-reg? in-bundles?)
                 `((feature-id . ,fid)
                   (registered? . ,in-reg?)
                   (bundle-present? . ,in-bundles?)
                   (manifest-exists? . ,(file-exists? manifest-path)))
                 #f))
             `((registered . ,registered)
               (bundles . ,bundles)))))

(define* (repl-state #:key (reg #f) (jrn #f) (mgr #f) (client #f) (cfg #f)
                     (mcp-mgr #f) (policy #f) (compactor #f) (rollback-mgr #f)
                     (verifier #f) (composer #f))
         "Return an alist snapshot of the active harness state and bound handles."
         (let ((bound (filter-map
                        (lambda (pair) (and (cdr pair) (car pair)))
                        `((reg . ,reg)
                          (jrn . ,jrn)
                          (mgr . ,mgr)
                          (client . ,client)
                          (cfg . ,cfg)
                          (mcp-mgr . ,mcp-mgr)
                          (policy . ,policy)
                          (compactor . ,compactor)
                          (rollback-mgr . ,rollback-mgr)
                          (verifier . ,verifier)
                          (composer . ,composer)))))
           `((registry-count . ,(if (registry? reg) (registry-count reg) 0))
             (registry-names . ,(if (registry? reg) (map ensure-string (registry-list reg)) '()))
             (journal-count . ,(if (journal? jrn) (journal-count jrn) 0))
             (model . ,(and (badb-config? cfg) (badb-config-model-name cfg)))
             (endpoint . ,(and (badb-config? cfg) (badb-config-model-endpoint cfg)))
             (theme . ,(and (badb-config? cfg) (badb-config-ui-theme cfg)))
             (mcp-server-count . ,(if (mcp-manager? mcp-mgr)
                                    (length (mcp-manager-list-servers mcp-mgr))
                                    0))
             (guardrail-level . ,(and (guardrail-policy? policy) (guardrail-policy-level policy)))
             (bound-handles . ,bound))))

(define*
  (make-repl-mod reg
                 jrn
                 mgr
                 client
                 #:optional
                 (cfg #f)
                 (mcp-mgr #f)
                 (policy #f)
                 (compactor #f)
                 (rollback-mgr #f)
                 (verifier #f)
                 (composer #f))
  "Create an isolated user module pre-populated with kernel state bindings,
core badb modules, and REPL introspection helpers (repl-help, repl-inspect,
repl-geasa, repl-state)."
  (let ((mod (make-fresh-user-module)))
    (module-use! mod (resolve-interface '(guile)))
    (module-use! mod (resolve-interface '(geasa providers-model plugin)))
    (module-use! mod (resolve-interface '(badb kernel registry)))
    (module-use! mod (resolve-interface '(badb kernel transaction)))
    (module-use! mod (resolve-interface '(badb kernel journal)))
    (module-use! mod (resolve-interface '(geasa foundation-config plugin)))
    (catch #t
      (lambda ()
        (module-use! mod (resolve-interface '(geasa capabilities-mcp plugin))))
      (lambda _
        #f))
    (catch #t
      (lambda ()
        (module-use! mod (resolve-interface '(geasa policies-guardrail plugin))))
      (lambda _
        #f))
    (catch #t
      (lambda ()
        (module-use! mod (resolve-interface '(geasa runtimes-context plugin))))
      (lambda _
        #f))
    (catch #t
      (lambda ()
        (module-use! mod (resolve-interface '(geasa runtimes-prompt plugin))))
      (lambda _
        #f))
    (catch #t
      (lambda ()
        (module-use! mod (resolve-interface '(geasa runtimes-rollback plugin))))
      (lambda _
        #f))
    (catch #t
      (lambda ()
        (module-use! mod (resolve-interface '(geasa runtimes-verify plugin))))
      (lambda _
        #f))
    (catch #t
      (lambda ()
        (module-use! mod (resolve-interface '(geasa runtimes-react plugin))))
      (lambda _
        #f))
    (module-use! mod (resolve-interface '(ice-9 format)))
    (module-use! mod (resolve-interface '(ice-9 rdelim)))
    (module-use! mod (resolve-interface '(ice-9 pretty-print)))
    (catch #t
      (lambda ()
        (module-use! mod (resolve-interface '(ice-9 session))))
      (lambda _
        #f))
    (module-use! mod (resolve-interface '(srfi srfi-1)))
    (module-define! mod 'reg reg)
    (module-define! mod 'jrn jrn)
    (module-define! mod 'mgr mgr)
    (module-define! mod 'client client)
    (when cfg (module-define! mod 'cfg cfg))
    (when mcp-mgr (module-define! mod 'mcp-mgr mcp-mgr))
    (when policy (module-define! mod 'policy policy))
    (when compactor (module-define! mod 'compactor compactor))
    (when rollback-mgr (module-define! mod 'rollback-mgr rollback-mgr))
    (when verifier (module-define! mod 'verifier verifier))
    (when composer (module-define! mod 'composer composer))
    (module-define! mod 'repl-help repl-help)
    (module-define! mod 'repl-inspect repl-inspect)
    (module-define! mod
                    'repl-geasa
                    (lambda* (#:optional (fid #f)) (repl-geasa fid reg)))
    (module-define! mod
                    'repl-state
                    (lambda ()
                      (repl-state #:reg
                                  reg
                                  #:jrn
                                  jrn
                                  #:mgr
                                  mgr
                                  #:client
                                  client
                                  #:cfg
                                  cfg
                                  #:mcp-mgr
                                  mcp-mgr
                                  #:policy
                                  policy
                                  #:compactor
                                  compactor
                                  #:rollback-mgr
                                  rollback-mgr
                                  #:verifier
                                  verifier
                                  #:composer
                                  composer)))
    mod))

(define (eval-scheme-code code-str repl-mod)
        "Evaluate CODE-STR in REPL-MOD and return (values result-string error-or-#f)."
        (let ((normalized (normalize-scheme-expr code-str)))
          (catch #t
            (lambda ()
              (let* ((p (open-input-string normalized))
                     (output-port (open-output-string)))
                (let loop ()
                          (let ((expr (read p)))
                            (unless (eof-object? expr)
                              (let ((printed
                                      (with-output-to-string
                                          (lambda ()
                                                  (call-with-values (lambda () (eval expr repl-mod))
                                                                    (lambda vals
                                                                      (for-each (lambda (v)
                                                                                  (format #t "=> ~s\n" v))
                                                                                vals)))))))
                                (display printed output-port)
                                (loop)))))
                (let ((res (string-trim-right (get-output-string output-port))))
                  (values (if (string-null? res) "=> #<unspecified>" res) #f))))
            (lambda (key . args)
              (values #f (format #f "Scheme error (~a): ~s" key args))))))

(define (%exec-log-path)
        "Exec log destination: BADB_EXEC_LOG when set (empty string disables
logging), else .badb/exec-log.jsonl."
        (let ((env (getenv "BADB_EXEC_LOG")))
          (cond
            ((and (string? env) (string-null? env)) #f)
            ((string? env) env)
            (else ".badb/exec-log.jsonl"))))

(define (%append-exec-log! cmd)
        "Append a byte-exact record of CMD to the exec log just before spawn,
best effort: never throws, silently skips when the log is disabled or
unwritable."
        (catch #t
          (lambda ()
            (let ((path (%exec-log-path)))
              (when path
                (ensure-parent-directory! path)
                (let* ((tv (gettimeofday))
                       (port (open-file path "a")))
                  (format port "{\"ts\":~a,\"us\":~a,\"len\":~a,\"cmd\":~a}\n"
                          (car tv) (cdr tv) (string-length cmd) (scm->json-string cmd))
                  (close-port port)))))
          (lambda _ #f)))

(define (%find-executable-in-path name)
        "Search PATH for executable NAME; return full path or #f."
        (if (or (not (string? name)) (string-null? name))
          #f
          (if (string-index name #\/)
            (and (file-exists? name) name)
            (let ((path-env (or (getenv "PATH") "")))
              (any (lambda (dir)
                     (and (not (string-null? dir))
                          (let ((cand (string-append dir "/" name)))
                            (and (file-exists? cand) cand))))
                   (string-split path-env #\:))))))

(define* (resolve-user-shell #:optional (override #f) #:key (verify-exists? #t))
         "Resolve the user's preferred shell from OVERRIDE or $SHELL (supporting
nu, fish, zsh, bash, sh). When VERIFY-EXISTS? is true, fall back to /bin/sh if
the configured binary cannot be found."
         (let* ((raw (cond
                       ((and (string? override) (not (string-null? (string-trim-both override))))
                         (string-trim-both override))
                       ((and (string? (getenv "SHELL"))
                             (not (string-null? (string-trim-both (getenv "SHELL")))))
                         (string-trim-both (getenv "SHELL")))
                       (else "/bin/sh"))))
           (if (or (not verify-exists?) (%find-executable-in-path raw))
             raw
             "/bin/sh")))

(define (shell-kind shell-path)
        "Return symbolic shell family ('nu, 'fish, 'zsh, 'bash, or 'sh) for SHELL-PATH."
        (let ((base (string-downcase (basename (if (string? shell-path) shell-path "sh")))))
          (cond
            ((string-prefix? "nu" base) 'nu)
            ((string-prefix? "fish" base) 'fish)
            ((string-prefix? "zsh" base) 'zsh)
            ((string-prefix? "bash" base) 'bash)
            (else 'sh))))

(define (build-shell-command-argv shell-path cmd)
        "Build the argv list to run CMD non-interactively via SHELL-PATH (-c)."
        (list (if (and (string? shell-path) (not (string-null? shell-path)))
                shell-path
                "/bin/sh")
              "-c"
              (if (string? cmd) cmd (format #f "~a" (or cmd "")))))

(define %sudo-cmd-rx (make-regexp "(^|[;|&(\t ]+)sudo([ \t]|$)"))

(define %sudo-pw-prompt-rx
        (make-regexp "(^|\n)[^\n]*password:[ \t]*$" regexp/icase))

(define %redact-piped-sudo-rx
        (make-regexp
          "(echo|printf)[ \t]+([^|;&\n]+?)[ \t]*\\|[ \t]*sudo(([ \t]+-[A-Za-z]+)*)[ \t]*-S"))

(define %redact-env-secret-rx
        (make-regexp
          "((SUDO_)?PASS(WORD|WD)?|API_?KEY|TOKEN|SECRET)=('[^']*'|\"[^\"]*\"|[^ \t;|&]+)"))

(define %redact-flag-secret-rx
        (make-regexp
          "--(password|passwd|token|api-key|secret)(=|[ \t]+)('[^']*'|\"[^\"]*\"|[^ \t;|&]+)"))

(define %sudo-inject-rx (make-regexp "(^|[;|&(\t ]+)sudo([ \t]+)"))

(define %sudo-dedup-s-rx
        (make-regexp "(^|[;|&(\t ]+)sudo([ \t]+)(-[A-Za-z]*S|--stdin)"))

(define (command-invokes-sudo? cmd)
        "Return #t if shell command CMD appears to invoke sudo."
        (and (string? cmd) (regexp-exec %sudo-cmd-rx cmd) #t))

(define (sudo-password-prompt? text)
        "Return #t when command output TEXT indicates sudo requires a password
or an interactive terminal / askpass helper."
        (and (string? text)
             (let ((low (string-downcase text)))
               (or (string-contains low "[sudo] password for")
                   (string-contains low "a terminal is required to read the password")
                   (string-contains low "no askpass program specified")
                   (string-contains low "sudo: a password is required")
                   (and (regexp-exec %sudo-pw-prompt-rx text) #t)))))

(define (redact-shell-secrets str)
        "Redact inline passwords, tokens, and piped sudo secrets from STR before
logging or displaying diagnostics."
        (if (string? str)
          (let*
              ((s1
                 (regexp-substitute/global #f
                                           %redact-piped-sudo-rx
                                           str
                                           'pre
                                           1
                                           " [REDACTED] | sudo"
                                           3
                                           " -S"
                                           'post))
               (s2
                 (regexp-substitute/global #f
                                           %redact-env-secret-rx
                                           s1
                                           'pre
                                           1
                                           "=[REDACTED]"
                                           'post))
               (s3
                 (regexp-substitute/global #f
                                           %redact-flag-secret-rx
                                           s2
                                           'pre
                                           "--"
                                           1
                                           2
                                           "[REDACTED]"
                                           'post)))
            s3)
          ""))

(define (inject-sudo-stdin-flag cmd)
        "Ensure sudo invocations in CMD use '-S -p \"\"' so passwords can be fed
safely on stdin without touching /dev/tty."
        (if (string? cmd)
          (regexp-substitute/global #f
                                    %sudo-dedup-s-rx
                                    (regexp-substitute/global #f %sudo-inject-rx cmd 'pre 1 "sudo -S -p '' " 'post)
                                    'pre
                                    1
                                    "sudo"
                                    2
                                    3
                                    'post)
          ""))

(define (%scrub-secret text secret)
        "Remove any accidental occurrence of SECRET from diagnostic TEXT."
        (if
            (or (not (string? text)) (not (string? secret)) (string-null? secret))
          text
          (let ((idx (string-contains text secret)))
            (if idx
              (%scrub-secret
                (string-append (substring text 0 idx)
                               "[REDACTED]"
                               (substring text (+ idx (string-length secret))))
                secret)
              text))))

(define (%spawn-argv-capture argv stdin-data)
        "Run ARGV via open-pipe* with optional STDIN-DATA string, capturing
combined stdout+stderr and tracking *active-subprocess-pid*.
Return (values output-string exit-code error-or-#f)."
        (catch #t
          (lambda ()
            (let* ((in-pipe (and (string? stdin-data) (pipe)))
                   (in-port (if in-pipe
                              (begin
                                (display stdin-data (cdr in-pipe))
                                (force-output (cdr in-pipe))
                                (close-port (cdr in-pipe))
                                (car in-pipe))
                              (open-input-file "/dev/null")))
                   (err-pipe (pipe))
                   (r-err (car err-pipe))
                   (w-err (cdr err-pipe))
                   (pipe-out
                     (with-input-from-port in-port
                                           (lambda ()
                                             (with-error-to-port w-err
                                                                 (lambda ()
                                                                   (apply open-pipe* OPEN_READ argv))))))
                   (pid
                     (catch #t
                       (lambda ()
                         (let* ((popen-mod (resolve-module '(ice-9 popen)))
                                (mutex (module-ref popen-mod 'port/pid-table-mutex))
                                (table (module-ref popen-mod 'port/pid-table)))
                           (with-mutex mutex (hashq-ref table pipe-out))))
                       (lambda _ #f))))
              (close-port w-err)
              (close-port in-port)
              (dynamic-wind
                (lambda ()
                  (when pid (set-active-subprocess-pid! pid)))
                (lambda ()
                  (let* ((err-thread
                           (call-with-new-thread
                             (lambda ()
                               (let ((s (read-string r-err)))
                                 (close-port r-err)
                                 (if (eof-object? s) "" s)))))
                         (raw-out (read-string pipe-out))
                         (raw-err (join-thread err-thread))
                         (status (close-pipe pipe-out))
                         (exit-val (or (status:exit-val status) 0))
                         (out-part (if (or (eof-object? raw-out) (string-null? raw-out)) "" raw-out))
                         (err-part (if (or (eof-object? raw-err) (string-null? raw-err)) "" raw-err))
                         (combined (cond
                                     ((string-null? out-part) err-part)
                                     ((string-null? err-part) out-part)
                                     ((string-suffix? "\n" out-part) (string-append out-part err-part))
                                     (else (string-append out-part "\n" err-part)))))
                    (values (string-trim-right combined) exit-val #f)))
                (lambda ()
                  (when (eqv? (active-subprocess-pid) pid)
                    (set-active-subprocess-pid! #f))))))
          (lambda (key . args)
            (values #f 1 (format #f "Shell error (~a): ~s" key args)))))

(define*
  (execute-shell-command cmd
                         #:key
                         (shell #f)
                         (password-provider #f)
                         (stdin-data #f)
                         (exec-fn #f))
  "Execute shell CMD through the user's $SHELL (nu, fish, zsh, bash, sh),
returning (values output-string exit-code error-or-#f). Records the secret-
redacted command to the exec log (.badb/exec-log.jsonl, BADB_EXEC_LOG override)
before spawn. When sudo requires a password and PASSWORD-PROVIDER is supplied,
prompts securely in memory without logging secrets and pipes via sudo -S."
  (%append-exec-log! (redact-shell-secrets cmd))
  (let*
      ((sh (resolve-user-shell shell))
       (base-argv (build-shell-command-argv sh cmd))
       (setsid-bin
         (and (not exec-fn)
              (not stdin-data)
              (command-invokes-sudo? cmd)
              (%find-executable-in-path "setsid")))
       (first-argv
         (if setsid-bin
           (cons* setsid-bin "-w" base-argv)
           base-argv))
       (runner (or exec-fn %spawn-argv-capture)))
    (call-with-values
      (lambda ()
        (runner first-argv stdin-data))
      (lambda (out exit-val err)
        (if
            (and (not err)
                 (not (= exit-val 0))
                 (sudo-password-prompt? out)
                 (procedure? password-provider))
          (let ((pw (password-provider "sudo password: ")))
            (if (and (string? pw) (not (string-null? pw)))
              (let*
                  ((sudo-cmd (inject-sudo-stdin-flag cmd))
                   (sudo-argv (build-shell-command-argv sh sudo-cmd))
                   (payload (string-append pw "\n")))
                (call-with-values
                  (lambda ()
                    (runner sudo-argv payload))
                  (lambda (s-out s-exit s-err)
                    (values (%scrub-secret s-out pw) s-exit (%scrub-secret s-err pw)))))
              (values out exit-val "sudo authentication cancelled")))
          (values out exit-val err))))))

;;;
;;; External $EDITOR Integration
;;;
(define* (resolve-external-editor #:optional (override #f))
         "Resolve the external editor command from OVERRIDE, $VISUAL, $EDITOR,
or fallback \"vi\"."
         (let ((cand (or (and (string? override) (not (string-null? (string-trim-both override)))
                              (string-trim-both override))
                         (let ((v (getenv "VISUAL")))
                           (and (string? v) (not (string-null? (string-trim-both v)))
                                (string-trim-both v)))
                         (let ((e (getenv "EDITOR")))
                           (and (string? e) (not (string-null? (string-trim-both e)))
                                (string-trim-both e))))))
           (or cand "vi")))

(define* (build-editor-command editor file #:optional (line #f) (column #f))
         "Build an argv list to open FILE at optional LINE/COLUMN in EDITOR."
         (let* ((parts (split-words (if (and (string? editor) (not (string-null? editor)))
                                      editor
                                      "vi")))
                (prog (if (pair? parts) (car parts) "vi"))
                (base (string-downcase (basename prog)))
                (has-line? (and (integer? line) (> line 0)))
                (has-col? (and (integer? column) (> column 0))))
           (cond
             ((member base '("hx" "helix"))
               (append parts
                       (list (if has-line?
                               (format #f "~a:~a~a" file line (if has-col? (format #f ":~a" column) ""))
                               file))))
             ((member base '("code" "codium" "subl"))
               (append parts
                       (if has-line?
                         (list "--goto"
                               (format #f "~a:~a~a" file line (if has-col? (format #f ":~a" column) "")))
                         (list file))))
             (else
               (append parts
                       (if has-line?
                         (list (format #f "+~a" line) file)
                         (list file)))))))

(define* (open-in-external-editor file #:key (line #f) (column #f) (editor #f)
                                  (suspend-tty! #f) (resume-tty! #f) (runner #f))
         "Open FILE (optionally jumping to LINE and COLUMN) in $EDITOR.
Calls SUSPEND-TTY! before spawning and RESUME-TTY! afterward via dynamic-wind.
Return (values ok? exit-code-or-error)."
         (if (or (not (string? file)) (string-null? (string-trim-both file)))
           (values #f "Invalid file path")
           (let* ((ed (resolve-external-editor editor))
                  (argv (build-editor-command ed file line column))
                  (exec (or runner
                            (lambda (args)
                              (or (status:exit-val (apply system* args)) 1)))))
             (catch #t
               (lambda ()
                 (let ((rc (dynamic-wind
                             (lambda () (when (procedure? suspend-tty!) (suspend-tty!)))
                             (lambda () (exec argv))
                             (lambda () (when (procedure? resume-tty!) (resume-tty!))))))
                   (values (and (integer? rc) (zero? rc)) rc)))
               (lambda (key . args)
                 (values #f (format #f "Editor error (~a): ~s" key args)))))))

(define* (read-via-external-editor #:key (initial-text "") (editor #f)
                                   (suspend-tty! #f) (resume-tty! #f) (runner #f))
         "Write INITIAL-TEXT to a temporary file, launch $EDITOR, and return
(values ok? edited-text-or-error)."
         (let* ((port (mkstemp "/tmp/badb-edit-XXXXXX"))
                (path (port-filename port)))
           (dynamic-wind
             (lambda ()
               (set-port-encoding! port "UTF-8")
               (display (or initial-text "") port)
               (close-port port))
             (lambda ()
               (call-with-values
                 (lambda ()
                   (open-in-external-editor path
                                            #:editor editor
                                            #:suspend-tty! suspend-tty!
                                            #:resume-tty! resume-tty!
                                            #:runner runner))
                 (lambda (ok? rc)
                   (if ok?
                     (values #t (or (file->string path) ""))
                     (values #f rc)))))
             (lambda ()
               (catch #t (lambda () (delete-file path)) (lambda _ #f))))))

;;;
;;; Chat Session Persistence (/resume)
;;;
(define %session-filename-max-length 40)

(define (default-sessions-dir)
        "Return the standard directory for persisted chat sessions
(~/.local/share/badb/sessions, honoring XDG_DATA_HOME)."
        (let ((xdg (getenv "XDG_DATA_HOME"))
              (home (getenv "HOME")))
          (cond
            ((and xdg (not (string-null? xdg)))
              (string-append xdg "/badb/sessions"))
            ((and home (not (string-null? home)))
              (string-append home "/.local/share/badb/sessions"))
            (else
              "/tmp/badb/sessions"))))

(define (session-file-path)
        "Return the JSONL path for this TUI process's session log: a
<timestamp>-<pid> name under (default-sessions-dir)."
        (string-append
          (default-sessions-dir) "/"
          (date->string (current-date 0) "~Y~m~dT~H~M~SZ")
          "-"
          (number->string (getpid))
          ".jsonl"))

(define (%assistant-tce->jso tce)
        "Serialize one assistant tool-call entry TCE for a JSONL line:
id, type, and the function's name plus raw JSON arguments string."
        (let ((fn (assoc-ref tce 'function)))
          `(("id" . ,(or (prop-ref tce 'id) ""))
            ("type" . "function")
            ("function" . (("name" . ,(or (prop-ref fn 'name) ""))
                           ("arguments" . ,(or (prop-ref fn 'arguments) "{}")))))))

(define (%turn-extras msg role)
        "Extra JSONL fields beyond role and content: the assistant turn's
reasoning trace and tool_calls vector, and the tool role's tool_call_id
and name."
        (let ((reasoning (assoc-ref msg 'reasoning_content))
              (tcs (assoc-ref msg 'tool_calls))
              (tid (assoc-ref msg 'tool_call_id))
              (nm (assoc-ref msg 'name)))
          (append
            (if (equal? role "assistant")
              (append
                (if (string? reasoning) `(("reasoning_content" . ,reasoning)) '())
                (if (vector? tcs)
                  (list (cons "tool_calls"
                              (list->vector (map %assistant-tce->jso (vector->list tcs)))))
                  '()))
              '())
            (if (equal? role "tool")
              (append
                (if (string? tid) `(("tool_call_id" . ,tid)) '())
                (if (string? nm) `(("name" . ,nm)) '()))
              '()))))

(define (turn->jsonl-line msg)
        "Serialize one conversation turn MSG into a single JSON object
line. Beyond role and content this preserves the assistant turn's
reasoning trace and tool_calls (ids plus name/arguments), and the tool
role's tool_call_id/name, so a resumed session reconstructs the full
assistant/tool structure the model expects."
        (let ((role (message-role msg)))
          (scm->json-string
            (append
              `(("role" . ,role)
                ("content" . ,(message-content msg)))
              (%turn-extras msg role)))))

(define (persist-session-file! path history)
        "Write the whole conversation HISTORY to the session JSONL file at PATH,
one JSON turn object per line (the system prompt first, as-is), creating
the enclosing directory when missing. Return #t on success and #f on any
filesystem failure."
        (catch #t
          (lambda ()
            (ensure-parent-directory! path)
            (call-with-output-file path
                                   (lambda (port)
                                     (for-each
                                       (lambda (msg)
                                         (display (turn->jsonl-line msg) port)
                                         (newline port))
                                       history)))
            #t)
          (lambda _ #f)))

(define (load-session-file path)
        "Load the conversation turns persisted in the session JSONL file at PATH,
oldest first, skipping blank or malformed lines. An absent or unreadable
file yields the empty list."
        (catch #t
          (lambda ()
            (if (and (string? path) (file-exists? path))
              (let ((raw (or (file->string path) "")))
                (filter-map jsonl-line->turn (string-split raw #\newline)))
              '()))
          (lambda _ '())))

(define (recent-session-files dir)
        "Return up to 7 session JSONL file paths in DIR, most recently modified
first. A missing or unreadable directory yields the empty list."
        (catch #t
          (lambda ()
            (let* ((names
                     (catch 'system-error
                       (lambda ()
                         (let ((stream (opendir dir)))
                           (let loop ((accum '()))
                                     (let ((entry (readdir stream)))
                                       (if (eof-object? entry)
                                         (begin (closedir stream) (reverse accum))
                                         (loop (cons entry accum)))))))
                       (lambda _ '())))
                   (paths
                     (filter-map
                       (lambda (name)
                         (and (string-suffix? ".jsonl" name)
                              (let ((path (string-append dir "/" name)))
                                (and (file-exists? path) path))))
                       names)))
              (take
                (sort paths
                      (lambda (a b)
                        (> (stat:mtime (stat a))
                           (stat:mtime (stat b)))))
                (min 7 (length paths)))))
          (lambda _ '())))

(define (session-mtime-string path)
        "Format the modification time of PATH as a local timestamp for the
resume menu."
        (catch #t
          (lambda ()
            (date->string
              (time-utc->date (make-time time-utc 0 (stat:mtime (stat path))))
              "~Y-~m-~d ~H:~M"))
          (lambda _ "unknown")))

(define (%extract-think-blocks text)
        "Lightweight <think>/<thought> tag splitter for resumed assistant turns."
        (let loop
          ((rem (or text "")) (th '()) (ct '()))
          (let*
              ((p1 (string-contains rem "<think>"))
               (p2 (string-contains rem "<thought>"))
               (hit
                 (cond
                   ((and p1 p2)
                     (if (< p1 p2)
                       (cons p1 "<think>")
                       (cons p2 "<thought>")))
                   (p1 (cons p1 "<think>"))
                   (p2 (cons p2 "<thought>"))
                   (else #f))))
            (if hit
              (let*
                  ((pos (car hit)) (tag (cdr hit))
                   (end-tag
                     (if (string=? tag "<think>")
                       "</think>"
                       "</thought>"))
                   (before (string-trim-both (substring rem 0 pos)))
                   (after (substring rem (+ pos (string-length tag))))
                   (end-pos (string-contains after end-tag))
                   (next-ct
                     (if (string-null? before)
                       ct
                       (cons before ct))))
                (if end-pos
                  (loop
                    (substring after (+ end-pos (string-length end-tag)))
                    (cons (string-trim-both (substring after 0 end-pos)) th) next-ct)
                  (let
                      ((final-th
                         (string-trim-both
                           (string-join (reverse (cons (string-trim-both after) th)) "\n\n")))
                       (final-ct (string-trim-both (string-join (reverse next-ct) "\n"))))
                    (values
                      (if (string-null? final-th)
                        #f
                        final-th)
                      final-ct))))
              (let
                  ((final-ct
                     (string-trim-both
                       (string-join
                         (reverse
                           (if (string-null? (string-trim-both rem))
                             ct
                             (cons (string-trim-both rem) ct)))
                         "\n")))
                   (final-th (string-trim-both (string-join (reverse th) "\n\n"))))
                (values
                  (if (string-null? final-th)
                    #f
                    final-th)
                  final-ct))))))

(define (%assistant-turn->cards turn)
        "Convert a single assistant TURN from llm-history into TUI transcript card(s)."
        (let* ((raw-ct (message-content turn))
               (explicit-th (message-thoughts turn))
               (tcs-raw (prop-ref turn 'tool_calls))
               (tcs (cond
                      ((vector? tcs-raw) (vector->list tcs-raw))
                      ((list? tcs-raw) tcs-raw)
                      (else '()))))
          (call-with-values
            (lambda ()
              (if explicit-th
                (values explicit-th raw-ct)
                (%extract-think-blocks raw-ct)))
            (lambda (th ct)
              (let* ((main-card
                       (if (or (non-empty-string? ct) (non-empty-string? th) (null? tcs))
                         (list `((role . "assistant")
                                 ,@(if (non-empty-string? th)
                                     `((thoughts . ,th) (collapsed? . #t))
                                     '())
                                 (content . ,ct)))
                         '()))
                     (tc-cards
                       (map (lambda (tc)
                              (let* ((fn (or (prop-ref tc 'function) tc))
                                     (nm (or (prop-ref fn 'name) "tool"))
                                     (args (or (prop-ref fn 'arguments) "{}")))
                                `((role . "tool-call")
                                  (content . ,(format #f "Invoking tool: ~a (~a)" nm args)))))
                            tcs)))
                (append main-card tc-cards))))))

(define* (session-turns->transcript-messages turns #:key (include-system? #f) (tool-preview-chars 140))
         "Reconstruct visible TUI transcript message cards from loaded llm-history
TURNS for /resume. Skips system turns unless INCLUDE-SYSTEM? is true, separates
assistant thoughts (collapsed by default), and reconstructs tool-call /
tool-result cards."
         (append-map
           (lambda (turn)
             (let ((role (message-role turn))
                   (content (message-content turn)))
               (cond
                 ((string=? role "system")
                   (if include-system?
                     (list `((role . "system") (content . ,content)))
                     '()))
                 ((string=? role "user")
                   (list `((role . "user") (content . ,content))))
                 ((string=? role "assistant")
                   (%assistant-turn->cards turn))
                 ((string=? role "tool")
                   (let ((nm (or (prop-ref turn 'name) "tool"))
                         (preview (truncate-display content tool-preview-chars)))
                     (list `((role . "tool-result")
                             (content . ,(format #f "Tool ~a returned: ~a" nm preview))))))
                 (else
                   (list `((role . ,role) (content . ,content)))))))
           (if (list? turns) turns '())))

(define (session-file-metadata path)
        "Inspect the persisted session JSONL file at PATH and return a metadata
alist for session resurrection menus (path, filename, mtime, turn-count,
user-turn-count, assistant-turn-count, tool-turn-count, estimated-tokens,
first-prompt, last-prompt, topics, summary)."
        (let* ((turns (load-session-file path))
               (non-sys (filter (lambda (t) (not (string=? (message-role t) "system"))) turns))
               (users (filter user-turn? non-sys))
               (assistants (filter (lambda (t) (string=? (message-role t) "assistant")) non-sys))
               (tools (filter (lambda (t) (string=? (message-role t) "tool")) non-sys))
               (turn-count (length non-sys))
               (est-tokens (estimate-conversation-tokens turns))
               (first-prompt (and (pair? users)
                                  (truncate-display (string-trim-both (message-content (car users))) 48)))
               (last-prompt (and (pair? users)
                                 (truncate-display (string-trim-both (message-content (last users))) 48)))
               (topics (map car (top-topic-words turns 5)))
               (mtime (session-mtime-string path))
               (summary (if (= turn-count 0)
                          (format #f "empty session (~a)" mtime)
                          (format #f "~a turn~a~a (~a)"
                                  turn-count
                                  (if (= turn-count 1) "" "s")
                                  (if first-prompt (format #f " · \"~a\"" first-prompt) "")
                                  mtime))))
          `((path . ,path)
            (filename . ,(basename path))
            (mtime . ,mtime)
            (turn-count . ,turn-count)
            (user-turn-count . ,(length users))
            (assistant-turn-count . ,(length assistants))
            (tool-turn-count . ,(length tools))
            (estimated-tokens . ,est-tokens)
            (first-prompt . ,first-prompt)
            (last-prompt . ,last-prompt)
            (topics . ,topics)
            (summary . ,summary))))

(define* (recent-session-summaries #:optional (dir (default-sessions-dir)) (limit 7))
         "Return metadata alists for up to LIMIT most recent session JSONL files
in DIR, newest first."
         (let* ((files (recent-session-files dir))
                (capped (if (and (integer? limit) (>= limit 0) (> (length files) limit))
                          (take files limit)
                          files)))
           (map session-file-metadata capped)))

(define (format-conversation-transcript messages)
        "Render MESSAGES (the TUI conversation alist with role/content/thoughts
entries) as a plain-text transcript: one block per message with a role
header, an optional thinking section, and the content. Pure - no I/O."
        (let ((blocks
                (map
                  (lambda (msg)
                    (let* ((role
                             (let ((r (assoc-ref msg 'role)))
                               (cond
                                 ((string? r) r)
                                 ((symbol? r) (symbol->string r))
                                 (else "system"))))
                           (content
                             (let ((c (assoc-ref msg 'content)))
                               (cond
                                 ((string? c) c)
                                 ((not c) "")
                                 (else (format #f "~a" c)))))
                           (thoughts
                             (let ((t (assoc-ref msg 'thoughts)))
                               (and (string? t) (not (string-null? t)) t))))
                      (string-append "=== "
                                     (string-upcase role)
                                     " ===\n"
                                     (if thoughts
                                       (string-append "[thinking]\n" thoughts "\n\n")
                                       "")
                                     (if (string-null? content)
                                       ""
                                       (string-append content "\n"))
                                     "\n")))
                  messages)))
          (string-append ";; badb conversation transcript\n" (string-join blocks ""))))
