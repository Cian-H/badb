;;; badb — Sema Synthesis Workflows & Dogfood (roadmap Phase 3).
;;;
;;; Coordinates between the badb kernel and the Sema workflow engine
;;; to synthesize Guile modules from declarative contract specifications.
;;;
;;; WORKFLOW CONTRACT
;;; =================
;;; 1. A contract specification (spec.scm) is locked read-only in the
;;;    kernel store (.badb/specs/<feature-id>.scm) via lock-spec.
;;; 2. The Sema plugin-synthesis workflow is executed:
;;;    - An LLM agent (defagent) is provisioned with AST MCP structural
;;;      editing tools (semcp), file editing tools, and the kernel spec_runner tool.
;;;    - Execution is bounded by spending/token caps (llm/with-budget).
;;;    - The agent iteratively mutates the implementation, guided by structured
;;;      counterexample feedback (seeds, failing contract names, input values,
;;;      arity mismatches, and Guile exceptions).
;;;    - The synthesis can be recorded or replayed deterministically via
;;;      llm/with-cassette for hermetic, offline test suites.
;;; 3. Once all contracts pass, the workflow triggers the kernel commit gate:
;;;    - The kernel independently replays the locked spec copy in a disposable
;;;      subprocess (verify-locked-spec).
;;;    - The transaction manager (apply-transaction!) pins identity hashes
;;;      (ast-diff-hash, spec-hash, quickcheck-seed, quickcheck-verdict,
;;;      kernel-replay-verdict), attaches the new module into the feature registry,
;;;      and records the commit checkpoint in the provenance journal.

(define-module (geasa runtimes-synthesis plugin)
               #:use-module
               (badb kernel digest)
               #:use-module
               (badb kernel journal)
               #:use-module
               (badb kernel registry)
               #:use-module
               (badb kernel geis-gate)
               #:use-module
               (badb kernel transaction)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 ftw)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 textual-ports)
               #:use-module
               (json)
               #:use-module
               (srfi srfi-1)
               #:export
               (find-sema-executable lock-feature-spec
                                     commit-synthesized-module!
                                     commit-synthesized-module-cli
                                     default-workflow-path
                                     run-synthesis-workflow
                                     synthesize-feature!
                                     run-synthesis-cli
                                     synthesis-assemble-hook))

;;;
;;; Binary and workflow path discovery
;;;
(define (find-sema-executable)
  "Locate the sema binary: honors $SEMA_BIN, then search PATH,
then inspects GNU Guix store profiles."
  (or (let ((bin (getenv "SEMA_BIN")))
        (and bin (file-exists? bin) bin))
      (search-path (parse-path (or (getenv "PATH") "")) "sema")
      (let ((store-matches
             (catch #t
               (lambda ()
                 (scandir "/gnu/store"
                          (lambda (f)
                            (and (string-contains f "sema-2.0.0")
                                 (not (string-suffix? ".lock" f))
                                 (not (string-suffix? ".drv" f))
                                 (not (string-suffix? "-builder" f))))))
               (lambda _ #f))))
        (and (pair? store-matches)
             (let ((bin (string-append "/gnu/store/" (car store-matches) "/bin/sema")))
               (and (file-exists? bin) bin))))
      (let ((profile-matches
             (catch #t
               (lambda ()
                 (scandir "/gnu/store"
                          (lambda (f)
                            (and (string-suffix? "-profile" f)
                                 (file-exists? (string-append "/gnu/store/" f "/bin/sema"))))))
               (lambda _ #f))))
        (and (pair? profile-matches)
             (string-append "/gnu/store/" (car profile-matches) "/bin/sema")))
      "sema"))

(define (default-workflow-path)
  "Return the default path to the plugin synthesis workflow file."
  (let ((env-path (getenv "BADB_SYNTHESIS_WORKFLOW")))
    (if (and env-path (file-exists? env-path))
        env-path
        (let* ((here (or (current-filename) "src/badb/synthesis.scm"))
               (root (canonicalize-path (string-append (dirname here) "/../..")))
               (candidate (string-append root "/workflows/plugin-synthesis.sema")))
          (if (file-exists? candidate)
              candidate
              "workflows/plugin-synthesis.sema")))))

;;;
;;; Spec locking helper
;;;
(define (lock-feature-spec spec-path feature-id)
  "Lock the authored spec at SPEC-PATH for FEATURE-ID into the
kernel locked spec store. Returns the SHA-256 spec hash."
  (lock-spec spec-path (ensure-string feature-id)))

;;;
;;; Kernel transaction commit for synthesized modules
;;;
(define* (commit-synthesized-module! mgr feature-id target-path #:key (metadata '()))
  "Read the synthesized module from TARGET-PATH and apply an atomic
transaction to commit it into MGR. The kernel spec gate replays the
locked spec copy; on pass, identity pins are recorded into the journal
and the implementation is hot-attached to the registry."
  (unless (transaction-manager? mgr)
    (error "commit-synthesized-module!: expected <transaction-manager>" mgr))
  (unless (file-exists? target-path)
    (error (format #f "commit-synthesized-module!: target file '~a' does not exist" target-path)))
  (let* ((content (call-with-input-file target-path
                    (lambda (port)
                      (set-port-encoding! port "UTF-8")
                      (get-string-all port))))
         (feature-str (ensure-string feature-id))
         (res (apply-transaction! mgr feature-str content (lambda (_mod) #t)
                                 #:metadata metadata)))
    res))

(define (commit-synthesized-module-cli feature-id target-path . args)
  "CLI helper invoked by external processes (e.g. Sema workflow commit phase)
to perform a gated kernel commit. Prints JSON result and exits with 0 on
commit or 1 on revert/error."
  (catch #t
    (lambda ()
      (let* ((feature-str (ensure-string feature-id))
             (target-str (ensure-string target-path))
             (jrn-path (or (getenv "BADB_JOURNAL_PATH") ".badb/journal.jsonl"))
             (reg (make-registry))
             (jrn (make-journal jrn-path))
             (mgr (make-transaction-manager reg jrn))
             (res (commit-synthesized-module! mgr feature-str target-str)))
        (if (string=? (or (assoc-ref res 'status) "") "committed")
            (let* ((latest (or (journal-latest jrn) '()))
                   (output `((status . "committed")
                             (feature-id . ,feature-str)
                             (ast-diff-hash . ,(or (assoc-ref latest "ast-diff-hash") ""))
                             (spec-hash . ,(or (assoc-ref latest "spec-hash") ""))
                             (quickcheck-seed . ,(or (assoc-ref latest "quickcheck-seed") ""))
                             (quickcheck-verdict . ,(or (assoc-ref latest "quickcheck-verdict") ""))
                             (kernel-replay-verdict . ,(or (assoc-ref latest "kernel-replay-verdict") "")))))
              (display (scm->json-string output))
              (newline)
              (exit 0))
            (begin
              (display (scm->json-string res))
              (newline)
              (exit 1)))))
    (lambda (key . err-args)
      (if (eq? key 'quit)
          (apply throw key err-args)
          (begin
            (display (scm->json-string `((status . "error")
                                        (error . ,(format #f "commit-synthesized-module-cli failed (~a): ~s" key err-args)))))
            (newline)
            (exit 1))))))

;;;
;;; Subprocess runner helper
;;;
(define (extract-last-nonempty-line text)
  (let ((lines (filter (lambda (l) (not (string-null? (string-trim-both l))))
                       (string-split text #\newline))))
    (if (pair? lines)
        (last lines)
        "")))

(define (run-command-capture cmd)
  "Run CMD in a subprocess and return (EXIT-CODE STDOUT-STRING STDERR-STRING)."
  (let* ((pipe (open-pipe cmd OPEN_READ))
         (out (get-string-all pipe))
         (status (close-pipe pipe))
         (exit-code (if (status:exit-val status) (status:exit-val status) 127)))
    (list exit-code (or out "") "")))

;;;
;;; Sema synthesis workflow runner
;;;
(define* (run-synthesis-workflow feature-id
                                #:key
                                (spec-path #f)
                                (target-path #f)
                                (workflow-path #f)
                                (model #f)
                                (budget #f)
                                (cassette #f)
                                (cassette-mode #f)
                                (max-turns #f)
                                (ast-mcp-command #f)
                                (run-dir #f))
  "Run the Sema plugin synthesis workflow for FEATURE-ID. Returns an alist
with the parsed result from the workflow execution."
  (let* ((feature-str (ensure-string feature-id))
         (actual-wf (or workflow-path (default-workflow-path)))
         (sema-bin (find-sema-executable))
         (actual-target (or target-path (format #f "src/badb/~a.scm" (sanitize-feature-id feature-str))))
         (gate-root (spec-gate-root))
         (default-locked (locked-spec-path feature-str gate-root))
         ;; Lock spec if provided and not yet locked
         (actual-spec
          (cond
           ((file-exists? default-locked) default-locked)
           ((and spec-path (file-exists? spec-path))
            (lock-spec spec-path feature-str)
            default-locked)
           (spec-path spec-path)
           (else default-locked)))
         ;; Prepare arguments map
         (args-alist
          (filter (lambda (p) (cdr p))
                  `((feature-id . ,feature-str)
                    (target-path . ,actual-target)
                    (spec-path . ,actual-spec)
                    (model . ,model)
                    (budget . ,budget)
                    (cassette . ,cassette)
                    (cassette-mode . ,(and cassette-mode (ensure-string cassette-mode)))
                    (max-turns . ,max-turns)
                    (ast-mcp-command . ,ast-mcp-command))))
         (args-json (scm->json-string args-alist))
         (actual-run-dir (or run-dir (string-append (or (getenv "TMPDIR") "/tmp")
                                                    "/badb-sema-runs-"
                                                    (number->string (current-time)))))
         (cmd (format #f "~a workflow run ~s --args ~s --run-dir ~s"
                      sema-bin actual-wf args-json actual-run-dir))
         (proc-res (run-command-capture cmd))
         (exit-code (list-ref proc-res 0))
         (stdout-str (list-ref proc-res 1))
         ;; Look for result.json in the run directory first
         (result-file
          (catch #t
            (lambda ()
              (and (file-exists? actual-run-dir)
                   (let ((runs (scandir actual-run-dir (lambda (d) (not (member d '("." "..")))))))
                     (and (pair? runs)
                          (let ((rf (string-append actual-run-dir "/" (car runs) "/result.json")))
                            (and (file-exists? rf) rf))))))
            (lambda _ #f)))
         (parsed-result
          (cond
           (result-file
            (catch #t
              (lambda ()
                (call-with-input-file result-file
                  (lambda (port)
                    (json-string->scm (get-string-all port)))))
              (lambda _ #f)))
           (else
            (let ((last-line (extract-last-nonempty-line stdout-str)))
              (catch #t
                (lambda () (json-string->scm last-line))
                (lambda _ #f)))))))
    `((status . ,(if (= exit-code 0) "success" "failed"))
      (exit-code . ,exit-code)
      (feature-id . ,feature-str)
      (result . ,parsed-result)
      (stdout . ,stdout-str))))

;;;
;;; High-level feature synthesis with optional transaction manager integration
;;;
(define* (synthesize-feature! feature-id
                             #:key
                             (spec-path #f)
                             (target-path #f)
                             (mgr #f)
                             (workflow-path #f)
                             (model #f)
                             (budget #f)
                             (cassette #f)
                             (cassette-mode #f)
                             (max-turns #f)
                             (ast-mcp-command #f)
                             (run-dir #f))
  "Synthesize FEATURE-ID against its contract specification using the Sema
workflow engine. When MGR is provided, also verifies that the feature is
hot-attached to MGR's registry on completion."
  (let* ((res (run-synthesis-workflow feature-id
                                     #:spec-path spec-path
                                     #:target-path target-path
                                     #:workflow-path workflow-path
                                     #:model model
                                     #:budget budget
                                     #:cassette cassette
                                     #:cassette-mode cassette-mode
                                     #:max-turns max-turns
                                     #:ast-mcp-command ast-mcp-command
                                     #:run-dir run-dir))
         (success? (string=? (or (assoc-ref res 'status) "") "success"))
         (wf-result (assoc-ref res 'result))
         (actual-target (or target-path (format #f "src/badb/~a.scm" (sanitize-feature-id (ensure-string feature-id))))))
    (when (and success? (transaction-manager? mgr) (file-exists? actual-target))
      ;; Hot-attach in the provided manager's registry if not already attached
      (catch #t
        (lambda ()
          (commit-synthesized-module! mgr feature-id actual-target))
        (lambda _ #f)))
    res))

;;;
;;; CLI presentation and dispatch
;;;
(define (print-synthesis-usage port)
  (format port
          "Usage: badb synthesize <feature-id> [OPTIONS]

Synthesize a Guile module to satisfy a locked contract specification
using the Sema workflow engine.

Options:
  --spec <path>           Path to the contract spec file to lock and verify
  --target <path>         Path to the target implementation file to synthesize
  --model <name>          LLM model name (default: qwen3.6-35b-a3b)
  --budget-tokens <N>     Maximum token budget for the synthesis run
  --budget-usd <N>        Maximum cost budget in USD
  --cassette <path>       Tape file path for cassette recording or replay
  --cassette-mode <mode>  Cassette mode: replay (default if set), record, or auto
  --max-turns <N>         Maximum agent tool-calling turns (default: 10)
  --commit-only           Only execute the kernel commit gate for target-path
  -h, --help              Display this help and exit
"))

(define synthesis-assemble-hook (make-parameter #f))

(define (run-synthesis-cli args)
        "CLI dispatcher for `badb synthesize`."
        (if
            (or (null? args) (member (car args) '("-h" "--help")))
          (begin (print-synthesis-usage (current-output-port)) (exit 0))
          (let loop
            ((rest args) (feature-id #f)
             (spec-path #f)
             (target-path #f)
             (model #f)
             (budget-tokens #f)
             (budget-usd #f)
             (cassette #f)
             (cassette-mode #f)
             (max-turns #f)
             (commit-only? #f))
            (cond
              ((null? rest)
                (if (not feature-id)
                  (begin
                    (format (current-error-port) "Error: missing required feature-id argument.~%")
                    (exit 1))
                  (if commit-only?
                    (commit-synthesized-module-cli feature-id
                                                   (or target-path (format #f "src/badb/~a.scm" feature-id)))
                    (let*
                        ((budget
                           (if (or budget-tokens budget-usd)
                             `((max-tokens . ,budget-tokens) (max-cost-usd . ,budget-usd))
                             #f))
                         (res
                           (run-synthesis-workflow feature-id
                                                   #:spec-path
                                                   spec-path
                                                   #:target-path
                                                   target-path
                                                   #:model
                                                   model
                                                   #:budget
                                                   budget
                                                   #:cassette
                                                   cassette
                                                   #:cassette-mode
                                                   cassette-mode
                                                   #:max-turns
                                                   max-turns))
                         (status (assoc-ref res 'status)))
                      (format #t "Sema synthesis ~a for feature '~a'.~%" status feature-id)
                      (when (assoc-ref res 'result)
                        (format #t "Result: ~a~%" (scm->json-string (assoc-ref res 'result))))
                      (catch #t
                        (lambda ()
                          (let ((hook (synthesis-assemble-hook)))
                            (when hook (hook feature-id cassette target-path))))
                        (lambda _
                          #f))
                      (exit
                        (if (string=? (or status "") "success")
                          0
                          1))))))
              ((equal? (car rest) "--commit-only")
                (loop (cdr rest) feature-id spec-path target-path model budget-tokens
                      budget-usd cassette cassette-mode max-turns #t))
              ((and (equal? (car rest) "--spec") (pair? (cdr rest)))
                (loop (cddr rest) feature-id (cadr rest) target-path model budget-tokens
                      budget-usd cassette cassette-mode max-turns commit-only?))
              ((and (equal? (car rest) "--target") (pair? (cdr rest)))
                (loop (cddr rest) feature-id spec-path (cadr rest) model budget-tokens
                      budget-usd cassette cassette-mode max-turns commit-only?))
              ((and (equal? (car rest) "--model") (pair? (cdr rest)))
                (loop (cddr rest) feature-id spec-path target-path (cadr rest) budget-tokens
                      budget-usd cassette cassette-mode max-turns commit-only?))
              ((and (equal? (car rest) "--budget-tokens") (pair? (cdr rest)))
                (loop (cddr rest) feature-id spec-path target-path model
                      (string->number (cadr rest)) budget-usd cassette cassette-mode max-turns
                      commit-only?))
              ((and (equal? (car rest) "--budget-usd") (pair? (cdr rest)))
                (loop (cddr rest) feature-id spec-path target-path model budget-tokens
                      (string->number (cadr rest)) cassette cassette-mode max-turns commit-only?))
              ((and (equal? (car rest) "--cassette") (pair? (cdr rest)))
                (loop (cddr rest) feature-id spec-path target-path model budget-tokens
                      budget-usd (cadr rest) (or cassette-mode "replay") max-turns commit-only?))
              ((and (equal? (car rest) "--cassette-mode") (pair? (cdr rest)))
                (loop (cddr rest) feature-id spec-path target-path model budget-tokens
                      budget-usd cassette (cadr rest) max-turns commit-only?))
              ((and (equal? (car rest) "--max-turns") (pair? (cdr rest)))
                (loop (cddr rest) feature-id spec-path target-path model budget-tokens
                      budget-usd cassette cassette-mode (string->number (cadr rest)) commit-only?))
              ((not feature-id)
                (loop (cdr rest) (car rest) spec-path target-path model budget-tokens
                      budget-usd cassette cassette-mode max-turns commit-only?))
              (else
                (format (current-error-port) "Error: unrecognized argument '~a'~%" (car rest))
                (exit 1))))))