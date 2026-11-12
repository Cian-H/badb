;;; (geasa surfaces-tui tools) — Builtin TUI Tools & Spec Gate Tool Bindings
;;;
;;; Extracted from surfaces-tui plugin.scm. Provides spec gate tools (spec.lock,
;;; spec.verify, spec.status, spec.synthesize), builtin filesystem tools (read_file,
;;; write_file, execute_command, bash, sh), rollback, tests, history, and MCP tools.

(define-module (geasa surfaces-tui tools)
               #:use-module (badb kernel registry)
               #:use-module (badb kernel geis-gate)
               #:use-module (badb kernel digest)
               #:use-module (geasa surfaces-tui session)
               #:use-module (geasa capabilities-mcp plugin)
               #:use-module (geasa capabilities-macro plugin)
               #:use-module (geasa capabilities-history-tools plugin)
               #:use-module (geasa capabilities-fs-tools plugin)
               #:use-module (geasa capabilities-code-nav plugin)
               #:use-module (geasa capabilities-context-index plugin)
               #:use-module (geasa capabilities-skills plugin)
               #:use-module (geasa agents-subagent plugin)
               #:use-module (geasa capabilities-scheme-eval plugin)
               #:use-module (geasa runtimes-geis-loader plugin)
               #:use-module (geasa runtimes-react plugin)
               #:use-module (geasa runtimes-workflows plugin)
               #:use-module (geasa foundation-config plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa runtimes-rollback plugin)
               #:use-module (geasa runtimes-verify plugin)
               #:use-module (json)
               #:use-module (ice-9 format)
               #:use-module (ice-9 rdelim)
               #:use-module (srfi srfi-1)
               #:export (spec-tool-format-failure
                          spec-tool-lock
                          spec-tool-verify
                          spec-tool-list-locked-specs
                          spec-tool-status
                          register-spec-tools!
                          build-geis-add-prompt
                          exec-shell-tool-impl
                          %read-file-default-limit
                          %read-file-max-chars-per-line
                          %read-file-max-chars
                          builtin-read-file
                          register-builtin-tui-tools!
                          auto-start-mcp-servers!))

;;;
;;; Kernel-Owned Spec Gate Tools
;;;
(define (spec-tool-format-failure failure)
        "Render one parsed spec failure record as a single diagnostic line."
        (format #f
                "- ~a [kind: ~a] seed: ~a inputs: ~s exception: ~a\n"
                (or (assoc-ref failure "name") "?")
                (or (assoc-ref failure "kind") "?")
                (or (assoc-ref failure "seed") "?")
                (or (assoc-ref failure "inputs") '?)
                (or (assoc-ref failure "exception") "?")))

(define (spec-tool-lock spec-path feature-id)
        "Kernel tool 'spec.lock': lock the authored spec at SPEC-PATH for
FEATURE-ID into the kernel-owned locked spec store."
        (catch #t
          (lambda ()
            (cond
              ((not
                 (and (string? spec-path)
                      (not (string-null? (string-trim-both spec-path)))
                      (string? feature-id)
                      (not (string-null? (string-trim-both feature-id)))))
                "Error: spec.lock requires non-empty string arguments spec-path and feature-id")
              ((not (file-exists? spec-path))
                (format #f "Error: spec file '~a' does not exist" spec-path))
              ((not
                 (catch #t
                   (lambda ()
                     (call-with-input-file spec-path
                                           (lambda (_port)
                                             #t)))
                   (lambda _
                     #f)))
                (format #f "Error: spec file '~a' is not readable" spec-path))
              (else
                (let* ((hash (lock-spec spec-path feature-id))
                       (locked (locked-spec-path feature-id)))
                  (format #f
                          "Locked spec for feature '~a'.\nLocked copy: ~a\nSHA-256 spec-hash: ~a\nThe kernel re-reads the LOCKED copy at commit time; locking is how this contract became binding."
                          feature-id
                          locked
                          hash)))))
          (lambda (key . args)
            (format #f "Error locking spec (~a): ~s" key args))))

(define (spec-tool-verify feature-id)
        "Kernel tool 'spec.verify': replay the LOCKED spec copy for FEATURE-ID."
        (if (and (string? feature-id) (not (string-null? (string-trim-both feature-id))))
          (let* ((result (verify-locked-spec feature-id))
                 (verdict (ensure-string (assoc-ref result 'verdict)))
                 (failures (assoc-ref result 'failures)))
            (apply string-append
                   (append
                     (list
                       (format #f
                               "Kernel replay verdict: ~a for feature '~a'.\n"
                               (string-upcase verdict)
                               feature-id)
                       (format #f "spec-hash: ~a\n" (or (assoc-ref result 'spec-hash) "none"))
                       (format #f
                               "summary: total=~a passed=~a failed=~a\n"
                               (or (assoc-ref result 'summary-total) "?")
                               (or (assoc-ref result 'summary-passed) "?")
                               (or (assoc-ref result 'summary-failed) "?"))
                       (format #f "seed: ~a\n" (ensure-string (assoc-ref result 'seed)))
                       (format #f
                               "exit-status: ~a signal: ~a timed-out: ~a\n"
                               (or (assoc-ref result 'exit-status) "none")
                               (or (assoc-ref result 'signal) "none")
                               (assoc-ref result 'timed-out?))
                       (format #f "reason: ~a\n" (ensure-string (assoc-ref result 'reason)))
                       "Verdicts come only from kernel subprocess replay of the LOCKED copy; the kernel re-reads it at commit time.\n")
                     (if (or (not failures) (null? failures))
                       (list "failures: none\n")
                       (cons "failures:\n" (map spec-tool-format-failure failures))))))
          "Error: spec.verify requires a non-empty string feature-id"))

(define (spec-tool-list-locked-specs)
        "Return the sorted locked spec file names in the kernel locked-spec store directory."
        (let ((dir (spec-gate-dir)))
          (if (file-exists? dir)
            (let ((d (opendir dir)))
              (let loop ((names '()))
                        (let ((entry (catch #t
                                       (lambda () (readdir d))
                                       (lambda _ #f))))
                          (cond
                            ((or (not entry) (eof-object? entry)) (closedir d) (sort names string<?))
                            ((string-suffix? ".scm" entry) (loop (cons entry names)))
                            (else (loop names))))))
            '())))

(define (spec-tool-status . maybe-feature-id)
        "Kernel tool 'spec.status': list the locked specs in the kernel locked-spec store."
        (let* ((raw-fid (and (pair? maybe-feature-id) (car maybe-feature-id)))
               (feature-id (and (string? raw-fid) (not (string-null? (string-trim-both raw-fid))) raw-fid))
               (dir (spec-gate-dir))
               (names (spec-tool-list-locked-specs)))
          (apply string-append
                 (append
                   (list (format #f "Locked spec store: ~a\n" dir)
                         (if (null? names)
                           "No locked specs yet.\n"
                           (format #f
                                   "~a locked spec~a:\n"
                                   (length names)
                                   (if (= (length names) 1) "" "s"))))
                   (map (lambda (name)
                          (let* ((path (string-append dir "/" name))
                                 (hash (let ((str (file->string path)))
                                         (if str (sha256-hex str) "unreadable"))))
                            (format #f
                                    "  ~a  ~a  (~a)\n"
                                    (substring name 0 (- (string-length name) 4))
                                    hash
                                    path)))
                        names)
                   (if feature-id
                     (let ((locked (locked-spec-path feature-id)))
                       (list
                         (if (file-exists? locked)
                           (format #f "Locked spec for feature '~a': present at ~a\n" feature-id locked)
                           (format #f
                                   "Locked spec for feature '~a': none (no locked copy at ~a)\n"
                                   feature-id
                                   locked))))
                     '())))))

(define (register-spec-tools! reg)
        "Attach the kernel-owned spec gate tools to the kernel feature registry REG."
        (registry-attach! reg
                          "spec.lock"
                          (lambda (spec-path feature-id)
                            (spec-tool-lock spec-path feature-id))
                          '((description .
                             "Lock the authored spec at spec-path for feature-id into the kernel-owned locked spec store; returns the locked copy path and its SHA-256 spec-hash. Locking is how a contract becomes binding: the kernel re-reads the LOCKED copy at commit time, never the agent's working copy.")
                            (_badb_defer . #t)
                            (category . "spec-gate")
                            (parameters .
                             ((type . "object")
                              (properties .
                               ((spec-path .
                                 ((type . "string") (description . "Path to the authored spec file to lock")))
                                (feature-id .
                                 ((type . "string") (description . "Feature identifier the spec gates")))))
                              (required . #("spec-path" "feature-id"))))))
        (registry-attach! reg
                          "spec.verify"
                          (lambda (feature-id)
                            (spec-tool-verify feature-id))
                          '((description .
                             "Ask the kernel to replay the LOCKED spec copy for feature-id in a disposable subprocess; returns the verdict, spec-hash, summary counts, and failure diagnostics. Verdicts come only from kernel subprocess replay of the locked copy, never from agent self-report; the gate fails closed.")
                            (_badb_defer . #t)
                            (category . "spec-gate")
                            (parameters .
                             ((type . "object")
                              (properties .
                               ((feature-id .
                                 ((type . "string")
                                  (description . "Feature identifier whose locked spec to replay")))))
                              (required . #("feature-id"))))))
        (registry-attach! reg
                          "spec.status"
                          (lambda args
                            (apply spec-tool-status args))
                          '((description .
                             "List the locked specs in the kernel locked-spec store with recomputed SHA-256 hashes; with optional feature-id, report whether that feature has a locked spec. The kernel re-reads the LOCKED copies at commit time.")
                            (_badb_defer . #t)
                            (category . "spec-gate")
                            (parameters .
                             ((type . "object")
                              (properties .
                               ((feature-id .
                                 ((type . "string")
                                  (description . "Optional feature identifier to check for a locked spec")))))
                              (required . #())))))
        (registry-attach! reg
                          "spec.synthesize"
                          (lambda*
                            (feature-id #:optional target-path spec-path)
                            (catch #t
                              (lambda ()
                                (let* ((synth-mod
                                         (false-if-exception (resolve-module '(geasa runtimes-synthesis plugin))))
                                       (resolve-role (module-ref synth-mod 'resolve-synthesis-role))
                                       (synth-fn (module-ref synth-mod 'run-synthesis-workflow))
                                       (role (catch #t
                                               (lambda () (resolve-role (load-config)))
                                               (lambda _ #f)))
                                       (res (synth-fn feature-id
                                                      #:target-path target-path
                                                      #:spec-path spec-path
                                                      #:model (and role (assoc-ref role 'model))
                                                      #:persona (and role (assoc-ref role 'persona-text))
                                                      #:env (if role
                                                              ((module-ref synth-mod 'synthesis-env-alist) role)
                                                              '())
                                                      #:llm-endpoint (if role
                                                                       (assoc-ref ((module-ref synth-mod 'synthesis-llm-args) role) 'llm-endpoint)
                                                                       #f)
                                                      #:llm-api-key (if role
                                                                      (assoc-ref ((module-ref synth-mod 'synthesis-llm-args) role) 'llm-api-key)
                                                                      #f)))
                                       (status (assoc-ref res 'status))
                                       (wf-result (assoc-ref res 'result)))
                                  (let* ((target (or target-path ((module-ref synth-mod 'default-synthesis-target) feature-id)))
                                         (install-report
                                           (if (string=? (or status "") "success")
                                             (let ((rep ((module-ref synth-mod 'install-synthesized-bundle!) feature-id target)))
                                               (catch #t
                                                 (lambda ()
                                                   (discover-and-mount-project-geasa! reg #:workspace (getcwd))
                                                   (mount-bundle-workflows! reg #:workspace (getcwd)))
                                                 (lambda _ #f))
                                               rep)
                                             #f)))
                                    (format #f
                                            "Sema plugin synthesis ~a for feature '~a'.\n~a~a~a"
                                            status
                                            feature-id
                                            (if wf-result
                                              (scm->json-string wf-result)
                                              (or (assoc-ref res 'stdout) ""))
                                            (if install-report
                                              (format #f
                                                      "\nInstalled geis for project: ~a verdict: ~a"
                                                      (or (assoc-ref install-report 'feature-id) feature-id)
                                                      (or (assoc-ref install-report 'verify-verdict)
                                                          (assoc-ref install-report 'install-status)
                                                          "?"))
                                              "")
                                            (if (string=? (or status "") "success")
                                              ""
                                              (format #f
                                                      "\n[guidance] synthesis did not converge. Do NOT keep re-running synthesize: after at most ONE more attempt, write the plugin directly with write_file (the contract stays locked), then verify with the spec gates. Re-running synthesize repeatedly burns budget without progress."))))))
                              (lambda (k . a)
                                (format #f "Error during synthesis (~a): ~s" k a))))
                          '((description .
                             "Run the Sema plugin synthesis workflow for feature-id against its locked contract specification; iteratively synthesizes the Guile module from counterexamples and triggers kernel commit + hot-attach.")
                            (_badb_defer . #t)
                            (category . "spec-gate")
                            (parameters .
                             ((type . "object")
                              (properties .
                               ((feature-id .
                                 ((type . "string") (description . "Feature identifier to synthesize")))
                                (target-path .
                                 ((type . "string")
                                  (description . "Optional path to target Guile module implementation file")))
                                (spec-path .
                                 ((type . "string")
                                  (description . "Optional path to contract specification file")))))
                              (required . #("feature-id"))))))
        reg)

(define (build-geis-add-prompt desc)
        "Construct a structured prompt instructing the agent to author a contract
specification, lock it via spec.lock, synthesize it via spec.synthesize, and
verify hot-attachment."
        (format #f
                "Please implement and dynamically attach a new feature to the badb harness: ~a

Follow the contract-gated dynamic synthesis workflow:
1. Design and write a declarative contract specification in Scheme (SRFI-64) defining the feature requirements, exported procedures, invariants, and test cases. Save this to a file under 'specs/' (e.g. 'specs/<feature-id>.spec.scm').
2. Lock the specification into the kernel-owned spec store using the 'spec.lock' tool with feature-id and spec-path. (Locking makes the contract binding; the kernel re-reads the locked copy at commit time).
3. Synthesize the Guile module implementation using the 'spec.synthesize' tool for your feature-id (which iteratively runs the Sema plugin-synthesis workflow with counterexample feedback until all contracts pass, then commits and hot-attaches the module).
4. Verify the newly attached feature is listed in the kernel registry (e.g. via 'spec.status' or kernel inspection) and summarize what capabilities were attached."
                desc))

;;;
;;; Builtin TUI Tools
;;;
(define %bash-output-max-lines 100)

(define %bash-output-max-chars 4096)

(define %bash-unix-token-names '("grep" "sed" "awk" "find"))

(define (%unix-tool-in-command? cmd)
        "Return #t when CMD mentions one of the raw unix text tools as a
whitespace-, pipe-, or semicolon-delimited token (used to target the
redirection tip on truncated output)."
        (and (string? cmd)
             (let*
                 ((raw (string-downcase cmd))
                  (space-tokens
                    (filter
                      (lambda (t)
                        (> (string-length t) 0))
                      (string-split raw #\space)))
                  (tokens
                    (append-map
                      (lambda (t)
                        (string-split t #\|))
                      (append-map
                        (lambda (t)
                          (string-split t #\;))
                        space-tokens))))
               (any
                 (lambda (tok)
                   (member tok %bash-unix-token-names))
                 tokens))))

(define (%bounded-shell-output cmd out)
        "Return OUT bounded to the bash output budget (~100 lines /
~4KB) with an instructive footer.  When CMD mentioned raw unix text
tools, the footer redirects to the structured search tools instead of
only naming the truncation, teaching the loop not to repeat an
unbounded raw search on the next turn."
        (if (string? out)
          (let*
              ((lines (string-split out #\newline))
               (n (length lines))
               (over-lines? (> n %bash-output-max-lines))
               (over-chars? (> (string-length out) %bash-output-max-chars)))
            (if (and (not over-lines?) (not over-chars?))
              out
              (let*
                  ((shown (take lines (min n %bash-output-max-lines)))
                   (joined (string-join shown "\n"))
                   (body
                     (if
                         (> (string-length joined) %bash-output-max-chars)
                       (substring joined 0 %bash-output-max-chars)
                       joined)))
                (format #f
                        "~a~%[Output truncated: ~a of ~a lines / ~a chars shown to protect the context window.]~a"
                        body
                        (min n %bash-output-max-lines)
                        n
                        (string-length body)
                        (if (%unix-tool-in-command? cmd)
                          "~%Tip: raw bash grep/sed/awk/find output ignores ignore-dirs and floods context. Use the file_search tool for workspace search (glob filter, output_mode files_with_matches or count), read_file for line slices, or ctx_search to recall offloaded command output."
                          "")))))
          (object->string out)))

(define (exec-shell-tool-impl cmd-or-arg)
        "Execute a shell command passed as a string or tool-call argument alist."
        (let
            ((cmd
               (cond
                 ((string? cmd-or-arg) cmd-or-arg)
                 ((and (pair? cmd-or-arg) (pair? (car cmd-or-arg)))
                   (or (assoc-ref cmd-or-arg "command")
                       (assoc-ref cmd-or-arg 'command)
                       (assoc-ref cmd-or-arg "cmd")
                       (assoc-ref cmd-or-arg 'cmd)
                       (cdar cmd-or-arg)))
                 (else (object->string cmd-or-arg)))))
          (call-with-values
            (lambda ()
              (execute-shell-command cmd))
            (lambda (out exit-val err)
              (if err
                (format #f "Error: ~a" err)
                (format #f "[exit ~a]\n~a" exit-val (%bounded-shell-output cmd out)))))))

(define %read-file-default-limit 800)
(define %read-file-max-chars-per-line 400)
(define %read-file-max-chars 16000)

(define (read-line-bounded port max-chars)
        (let loop
          ((chars '()) (length 0))
          (let ((ch (read-char port)))
            (cond
              ((eof-object? ch)
                (and (> length 0) (cons (list->string (reverse chars)) length)))
              ((char=? ch #\newline) (cons (list->string (reverse chars)) length))
              (else
                (loop
                  (if (< length max-chars)
                    (cons ch chars)
                    chars) (+ length 1)))))))

(define
  (read-lines-window port start end max-chars)
  (let loop
    ((i 0) (acc '()) (chars 0) (last-shown #f))
    (let
        ((line-info (read-line-bounded port %read-file-max-chars-per-line)))
      (if (not line-info)
        (list i (reverse acc) last-shown)
        (let*
            ((line (car line-info))
             (line-length (cdr line-info))
             (trim
               (if (> line-length %read-file-max-chars-per-line)
                 (string-append line
                                (format #f "... [line ~a truncated: ~a of ~a chars]" i line-length line-length))
                 line))
             (selected? (and (>= i start) (< i end) (< chars max-chars))))
          (loop (+ i 1)
                (if selected?
                  (cons trim acc)
                  acc)
                (if selected?
                  (+ chars (string-length trim))
                  chars)
                (if selected?
                  i
                  last-shown)))))))

(define*
  (builtin-read-file path #:optional (offset 0) (limit #f))
  "Read a bounded, line-based slice of PATH without loading it all into memory."
  (catch #t
    (lambda ()
      (if (not (file-exists? path))
        (format #f "Error: file '~a' does not exist" path)
        (call-with-input-file path
                              (lambda (port)
                                (let*
                                    ((start
                                       (max 0
                                            (if (number? offset)
                                              (inexact->exact (floor offset))
                                              0)))
                                     (lim
                                       (if (and (number? limit) (> limit 0))
                                         (inexact->exact (floor limit))
                                         %read-file-default-limit))
                                     (scan (read-lines-window port start (+ start lim) %read-file-max-chars))
                                     (total (car scan))
                                     (shown (cadr scan))
                                     (last-shown (caddr scan))
                                     (next (+ (or last-shown start) 1)))
                                  (if (>= start total)
                                    (format #f
                                            "[file ~a - ~a total lines - offset ~a is at or past end of file]"
                                            path
                                            total
                                            start)
                                    (string-append
                                      (format #f
                                              "[file ~a - ~a total lines - showing lines ~a..~a]~%"
                                              path
                                              total
                                              start
                                              (or last-shown start))
                                      (string-join shown "\n")
                                      (if (>= next total)
                                        (format #f "~%[end of file at line ~a of ~a]" total total)
                                        (format #f
                                                "~%[~a more lines elided - call read_file with offset=~a to continue (total ~a lines)]"
                                                (- total next)
                                                next
                                                total)))))))))
    (lambda (_k . a)
      (format #f "Error reading file: ~s" a))))

(define* (register-builtin-tui-tools! reg rollback-mgr verifier mcp-mgr get-llm-history
                                      #:key
                                      (ctx-idx (make-bm25-index))
                                      (skill-reg (make-skill-registry))
                                      (client #f)
                                      (config #f)
                                      (policy #f))
         "Register built-in autonomous agent tools, MCP kernel tools, and spec gate tools on REG."
         ;; 1. Core filesystem & editing tools (read_file, write_file, edit_file, bash)
         (register-fs-tools! reg #:rollback-mgr rollback-mgr #:verifier verifier #:shell-executor exec-shell-tool-impl #:ctx-idx ctx-idx)
         ;; 1b. Input macro expansion (@@, @file:PATH, @SHORTCUT directives)
         (register-macro-tools! reg)
         ;; 2. Code navigation & persistent memories
         (register-code-nav-tools! reg #:workspace (getcwd))
         ;; 3. Virtualized output & BM25 context indexing
         (register-context-index-tools! reg ctx-idx #:shell-executor exec-shell-tool-impl)
         ;; 4. Skills registry & loader
         (register-skill-tools! reg skill-reg)
         ;; 5. Delegated subagent investigation
         (register-subagent-tools! reg #:client client)
         ;; 6. Sandboxed Scheme evaluation
         (register-scheme-eval-tools! reg)

         ;; Backward-compatibility shell aliases
         (registry-attach! reg
                           "execute_command"
                           exec-shell-tool-impl
                           '((description . "Execute a shell command and return output and exit status")
                             (parameters .
                              ((type . "object")
                               (properties .
                                ((cmd . ((type . "string") (description . "Shell command to run")))))
                               (required . #("cmd"))))))
         (registry-attach! reg
                           "sh"
                           exec-shell-tool-impl
                           '((description . "Execute a POSIX shell command and return output and exit code")
                             (parameters .
                              ((type . "object")
                               (properties .
                                ((command . ((type . "string") (description . "Shell command line")))))
                               (required . #("command"))))))
         (registry-attach! reg
                           "checkpoint_create"
                           (lambda (description)
                             (let* ((c (create-checkpoint! rollback-mgr (or description "agent-checkpoint")))
                                    (sess (current-react-session)))
                               (when sess
                                 (set-react-session-active-tools! sess '()))
                               (format #f
                                       "Created checkpoint: ~a (~a) covering ~a workspace file(s) and the registry. Active tools reset to baseline."
                                       (checkpoint-id c)
                                       (checkpoint-description c)
                                       (length (checkpoint-files c)))))
                           '((description .
                              "Create a rollback checkpoint for current workspace files and registry. Only files under the workspace directory are captured; paths outside it are silently left out of the checkpoint.")
                             (_badb_defer . #t)
                             (parameters .
                              ((type . "object")
                               (properties .
                                ((description . ((type . "string") (description . "Checkpoint description")))))))))
         (registry-attach! reg
                           "checkpoint_rollback"
                           (lambda* (#:optional id)
                                    (let ((res (if (and id (not (string-null? id)))
                                                 (rollback-to-checkpoint! rollback-mgr id)
                                                 (rollback-latest! rollback-mgr))))
                                      (format #f
                                              "Rollback: ~a (restored ~a file(s), removed ~a file(s)). Only files captured by the checkpoint are restored; files outside the workspace were never captured."
                                              (assoc-ref res 'status)
                                              (length (or (assoc-ref res 'restored-files) '()))
                                              (length (or (assoc-ref res 'removed-files) '())))))
                           '((description .
                              "Rollback to specified or latest checkpoint. Only files captured by the checkpoint (workspace files at creation time) are restored.")
                             (_badb_defer . #t)
                             (parameters .
                              ((type . "object")
                               (properties . ((id . ((type . "string") (description . "Checkpoint ID")))))))))
         (registry-attach! reg
                           "run_tests"
                           (lambda* (#:optional (cmd "./scripts/test"))
                                    (let ((res (verify-test-suite #:command (or cmd "./scripts/test"))))
                                      (format #f
                                              "Tests: ~a (exit ~a)\n~a"
                                              (if (assoc-ref res 'passed?)
                                                "PASS"
                                                "FAIL")
                                              (assoc-ref res 'exit-code)
                                              (assoc-ref res 'output))))
                           '((description . "Run project test suite and return verdict")
                             (_badb_defer . #t)
                             (parameters .
                              ((type . "object")
                               (properties . ((cmd . ((type . "string") (description . "Test command")))))))))
         (registry-attach! reg
                           "history/summarize"
                           (lambda ()
                             (let ((summary (summarize-turns (get-llm-history)))
                                   (hist-file (getenv "BADB_HISTORY_FILE")))
                               (if (and hist-file (not (string-null? hist-file)))
                                 (let ((file-summary (summarize-history-file hist-file)))
                                   (if file-summary
                                     (string-append summary "\n\n--- " hist-file " ---\n" file-summary)
                                     summary))
                                 summary)))
                           '((description .
                              "Summarize the current conversation: turn counts, token estimate, recent topics")
                             (_badb_defer . #t)
                             (parameters . ((type . "object") (properties . ()) (required . #())))))
         ;; Register MCP Kernel Features (deferred)
         (registry-attach! reg
                           "mcp.list"
                           (lambda ()
                             (mcp-manager-list-servers mcp-mgr))
                           '((description . "List all configured MCP servers and their status")
                             (_badb_defer . #t)))
         (registry-attach! reg
                           "mcp.start"
                           (lambda (name)
                             (mcp-manager-start-server! mcp-mgr name))
                           '((description . "Start a configured MCP server by name")
                             (_badb_defer . #t)))
         (registry-attach! reg
                           "mcp.stop"
                           (lambda (name)
                             (mcp-manager-stop-server! mcp-mgr name))
                           '((description . "Stop a running MCP server by name")
                             (_badb_defer . #t)))
         (registry-attach! reg
                           "mcp.add"
                           (lambda (name cmd args)
                             (mcp-manager-add-server! mcp-mgr name cmd args #:auto-start #t))
                           '((description . "Add a new MCP server specification and persist to config")
                             (_badb_defer . #t)))
         (registry-attach! reg
                           "mcp.remove"
                           (lambda (name)
                             (mcp-manager-remove-server! mcp-mgr name))
                           '((description . "Remove an MCP server from configuration and stop it")
                             (_badb_defer . #t)))
         (registry-attach! reg
                           "mcp.tools"
                           (lambda* (#:optional server-name) (mcp-manager-list-tools mcp-mgr server-name))
                           '((description . "List available tools discovered from running MCP servers")
                             (_badb_defer . #t)))
         (registry-attach! reg
                           "mcp.call"
                           (lambda (name-or-server tool-or-args . maybe-args)
                             (if (pair? maybe-args)
                               (mcp-manager-call-tool mcp-mgr name-or-server tool-or-args (car maybe-args))
                               (mcp-manager-call-tool mcp-mgr name-or-server tool-or-args)))
                           '((description . "Call a tool on an MCP server")
                             (_badb_defer . #t)))

         ;; Progressive geis discovery & just-in-time hydration tools
         (registry-attach! reg
                           "geis.index"
                           (lambda* (#:optional category query)
                                    (geis-index-query #:category category #:query query))
                           '((description .
                              "Query the geis capability index for available domain tools, test runners, or workflows. Returns a concise summary table without hydrating schemas into context. To use a discovered tool, activate it with geis.activate(feature_id).")
                             (parameters .
                              ((type . "object")
                               (properties .
                                ((category . ((type . "string") (description . "Optional category filter (e.g. 'test', 'lint', 'spec-gate', 'project')")))
                                 (query . ((type . "string") (description . "Optional search keyword for feature-id or description")))))
                               (required . #())))))

         (registry-attach! reg
                           "geis.activate"
                           (lambda (feature-id)
                             (let* ((fid (ensure-string feature-id))
                                    (sess (current-react-session))
                                    (all-index (geis-catalog-index))
                                    (matched (find (lambda (e)
                                                     (let ((efid (assoc-ref e 'fid))
                                                           (tools (assoc-ref e 'tools)))
                                                       (or (string=? efid fid)
                                                           (member fid (ensure-list tools)))))
                                                   all-index))
                                    (tools-to-add (if matched
                                                    (cons fid (or (assoc-ref matched 'tools) '()))
                                                    (list fid))))
                               (if sess
                                 (let ((cur (react-session-active-tools sess)))
                                   (set-react-session-active-tools! sess (delete-duplicates (append cur tools-to-add)))
                                   (format #f "Activated capability '~a' (~a tool(s) hydrated into active toolset: ~a). The tool parameter schemas are now available for subsequent turns."
                                           fid (length tools-to-add) (string-join tools-to-add ", ")))
                                 (format #f "Capability '~a' marked active (~a)." fid (string-join tools-to-add ", ")))))
                           '((description .
                              "Activate a geis capability by feature-id (from geis.index), hydrating its full tool parameter schema into your active toolset for the current task.")
                             (parameters .
                              ((type . "object")
                               (properties .
                                ((feature-id . ((type . "string") (description . "Feature ID or tool name to activate")))))
                               (required . #("feature-id"))))))

         (registry-attach! reg
                           "geis.deactivate"
                           (lambda (feature-id)
                             (let* ((fid (ensure-string feature-id))
                                    (sess (current-react-session))
                                    (all-index (geis-catalog-index))
                                    (matched (find (lambda (e) (string=? (assoc-ref e 'fid) fid)) all-index))
                                    (tools-to-remove (if matched
                                                       (cons fid (or (assoc-ref matched 'tools) '()))
                                                       (list fid))))
                               (if sess
                                 (let* ((cur (react-session-active-tools sess))
                                        (remaining (filter (lambda (t) (not (member t tools-to-remove))) cur)))
                                   (set-react-session-active-tools! sess remaining)
                                   (format #f "Deactivated capability '~a'. Tools demoted back to deferred status." fid))
                                 (format #f "Capability '~a' deactivated." fid))))
                           '((description .
                              "Deactivate a geis capability, demoting its schema back to deferred status to save context.")
                             (parameters .
                              ((type . "object")
                               (properties .
                                ((feature-id . ((type . "string") (description . "Feature ID or tool name to deactivate")))))
                               (required . #("feature-id"))))))

         ;; Project geasa and workflows auto-mount
         (catch #t
           (lambda ()
             (discover-and-mount-project-geasa! reg #:workspace (getcwd))
             (mount-bundle-workflows! reg #:workspace (getcwd)))
           (lambda _ #f))

         ;; Register kernel-owned spec gate tools
         (register-spec-tools! reg))

(define (auto-start-mcp-servers! cfg mcp-mgr)
        "Auto-start configured MCP servers that specify auto-start: #t."
        (catch #t
          (lambda ()
            (for-each
              (lambda (entry)
                (let* ((name (if (pair? entry)
                               (if (pair? (car entry))
                                 (assoc-ref entry 'name)
                                 (car entry))
                               #f))
                       (spec (if (pair? entry)
                               (if (pair? (car entry))
                                 entry
                                 (cdr entry))
                               '()))
                       (auto? (or (assoc-ref spec 'auto-start) (assoc-ref spec 'auto-start?))))
                  (when (and name auto?)
                    (catch #t
                      (lambda () (mcp-manager-start-server! mcp-mgr name))
                      (lambda _ #f)))))
              (badb-config-mcp-servers cfg)))
          (lambda _ #f)))
