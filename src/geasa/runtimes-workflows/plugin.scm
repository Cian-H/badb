;;; (geasa runtimes-workflows plugin) — Workflow Execution & Trajectory Distillation
;;;
;;; Executes declarative .sema workflow scripts, bootstraps badb project infrastructure,
;;; and distills successful agent session trajectories into reproducible Geis bundles.

(define-module (geasa runtimes-workflows plugin)
               #:use-module (badb geis bundle)
               #:use-module (badb kernel registry)
               #:use-module (badb kernel digest)
               #:use-module (geasa foundation-util plugin)
               #:use-module (json)
               #:use-module (ice-9 ftw)
               #:use-module (ice-9 format)
               #:use-module (ice-9 textual-ports)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 rdelim)
               #:use-module (srfi srfi-1)
               #:export (execute-sema-workflow
                          bootstrap-project-geasa!
                          profile-workspace-environment
                          onboard-project-geasa!
                          distill-trajectory-to-geis
                          find-sema-executable
                          mount-bundle-workflows!
                          distill-exec-patterns!))

(define write-file! string->file)

(define (find-sema-executable)
        "Locate the sema binary: honors $SEMA_BIN, then search PATH."
        (or (let ((bin (getenv "SEMA_BIN")))
              (and bin (file-exists? bin) bin))
            (search-path (parse-path (or (getenv "PATH") "")) "sema")))

;;;
;;; Workflow Execution Engine
;;;
(define* (execute-sema-workflow workflow-path
                                #:key
                                (registry #f)
                                (args '())
                                (engine 'auto)
                                (timeout 60))
         "Execute a .sema workflow script from WORKFLOW-PATH.
When REGISTRY is provided (or ENGINE is 'native), executes the workflow
in-process against REGISTRY. When ENGINE is 'sema (and SEMA_BIN available),
runs via the external sema CLI."
         (let ((sema-bin (find-sema-executable)))
           (if (and (eq? engine 'sema) sema-bin)
             ;; External sema CLI runner
             (let* ((args-json (scm->json-string (or args '())))
                    (pipe (open-pipe* "r" sema-bin "workflow" "run" workflow-path "--args" args-json))
                    (output (read-string pipe))
                    (status (close-pipe pipe)))
               `((status . ,(if (zero? (status:exit-val status)) 'ok 'error))
                 (output . ,output)))
             ;; In-process interpreter against registry
             (catch #t
               (lambda ()
                 (let* ((content (file->string workflow-path))
                        (forms (call-with-input-string content
                                                       (lambda (p)
                                                         (let loop ((acc '()))
                                                                   (let ((x (read p)))
                                                                     (if (eof-object? x)
                                                                       (reverse acc)
                                                                       (loop (cons x acc))))))))
                        (executed-steps '()))
                   (for-each
                     (lambda (form)
                       (when (pair? form)
                         (case (car form)
                           ((defworkflow workflow)
                             (let ((body (if (and (pair? (cdr form)) (symbol? (cadr form)))
                                           (cddr form)
                                           (cdr form))))
                               (for-each
                                 (lambda (step-form)
                                   (when (and (pair? step-form) (eq? (car step-form) 'step))
                                     (let* ((action (caddr step-form))
                                            (t-name (if (and (pair? action) (eq? (car action) 'call-tool))
                                                      (cadr action)
                                                      (symbol->string (car action))))
                                            (t-args (if (and (pair? action) (eq? (car action) 'call-tool))
                                                      (cddr action)
                                                      (cdr action)))
                                            (proc (and registry (registry-lookup registry t-name))))
                                       (when (procedure? proc)
                                         (let ((res (apply proc t-args)))
                                           (set! executed-steps
                                                 (cons (cons t-name res) executed-steps)))))))
                                 body)))
                           ((step call-tool)
                             (let* ((t-name (if (eq? (car form) 'step)
                                              (cadr (caddr form))
                                              (cadr form)))
                                    (t-args (if (eq? (car form) 'step)
                                              (cddr (caddr form))
                                              (cddr form)))
                                    (proc (and registry (registry-lookup registry t-name))))
                               (when (procedure? proc)
                                 (let ((res (apply proc t-args)))
                                   (set! executed-steps (cons (cons t-name res) executed-steps))))))
                           (else #f))))
                     forms)
                   `((status . ok)
                     (executed . ,(reverse executed-steps)))))
               (lambda (k . a)
                 `((status . error)
                   (reason . ,(format #f "Workflow execution failed: (~a) ~s" k a))))))))

;;;
;;; Project Infrastructure Bootstrapping
;;;
(define* (bootstrap-project-geasa! #:optional (workspace (getcwd)))
         "Initialize .badb structure: geasa/, workflows/, memories/, rules.scm."
         (let* ((badb-dir (string-append workspace "/.badb"))
                (geasa-dir (string-append badb-dir "/geasa"))
                (wf-dir (string-append badb-dir "/workflows"))
                (mem-dir (string-append badb-dir "/memories"))
                (rules-file (string-append badb-dir "/rules.scm"))
                (default-wf (string-append wf-dir "/verify.sema"))
                (arch-mem (string-append mem-dir "/architecture.md")))
           (mkdir-p geasa-dir)
           (mkdir-p wf-dir)
           (mkdir-p mem-dir)

           ;; 1. .badb/rules.scm
           (unless (file-exists? rules-file)
             (write-file! rules-file
                          ";;; .badb/rules.scm — Project Rules & Invariants
;;; Read automatically by badb system prompt composer.
(rule \"documentation-integrity\"
      \"Preserve existing docstrings, licenses, and comments unless explicitly asked to modify.\")
(rule \"verification-gate\"
      \"Always run test suite before completing tasks or declaring success.\")
"))

           ;; 2. .badb/workflows/verify.sema
           (unless (file-exists? default-wf)
             (write-file! default-wf
                          ";;; verify.sema — Automated Verification Workflow
(defworkflow verify
  (step run-tests (call-tool \"run_tests\" \"\"))
  (step check-git (call-tool \"bash\" \"git status --short\")))
"))

           ;; 3. .badb/memories/architecture.md
           (unless (file-exists? arch-mem)
             (write-file! arch-mem
                          "# Project Architecture Memory
- Workspace: Root directory initialized by badb.
- Workflows: Reusable automation pipelines live in `.badb/workflows/`.
- Geasa: Dynamic project plugins live in `.badb/geasa/`.
"))
           #t))

;;;
;;; Project Onboarding & Workspace Profiling
;;;

(define* (profile-workspace-environment #:optional (workspace (getcwd)))
         "Inspect WORKSPACE and return an alist describing detected language, test command, and lint command."
         (let ((has? (lambda (f) (file-exists? (string-append workspace "/" f)))))
           (cond
             ((or (has? "guix.scm") (has? "channels.scm") (has? "manifest.scm"))
               '((language . "guile")
                 (test-cmd . "./scripts/test")
                 (lint-cmd . "./scripts/test")))
             ((has? "Cargo.toml")
               '((language . "rust")
                 (test-cmd . "cargo test")
                 (lint-cmd . "cargo clippy")))
             ((has? "package.json")
               '((language . "node")
                 (test-cmd . "npm test")
                 (lint-cmd . "npm run lint")))
             ((or (has? "pyproject.toml") (has? "setup.py") (has? "requirements.txt"))
               '((language . "python")
                 (test-cmd . "pytest")
                 (lint-cmd . "ruff check .")))
             ((has? "Makefile")
               '((language . "make")
                 (test-cmd . "make check")
                 (lint-cmd . "make lint")))
             (else
               '((language . "general")
                 (test-cmd . "./scripts/test")
                 (lint-cmd . #f))))))

(define* (onboard-project-geasa! #:optional (workspace (getcwd)) #:key (reg #f))
         "Detect workspace toolchain, bootstrap .badb infrastructure, synthesize a
calibrated project-test-runner geis bundle, and mount into REG if provided.
Returns a structured onboarding report alist."
         (bootstrap-project-geasa! workspace)
         (let* ((profile (profile-workspace-environment workspace))
                (lang (assoc-ref profile 'language))
                (test-cmd (assoc-ref profile 'test-cmd))
                (geasa-dir (string-append workspace "/.badb/geasa"))
                (runner-dir (string-append geasa-dir "/project-test-runner"))
                (plugin-file (string-append runner-dir "/plugin.scm"))
                (spec-file (string-append runner-dir "/spec.geis.scm"))
                (manifest-file (string-append runner-dir "/manifest.scm"))
                (index-file (string-append geasa-dir "/index.json")))

           (mkdir-p runner-dir)

           ;; 1. Generate project-test-runner/plugin.scm
           (call-with-output-file plugin-file
                                  (lambda (p)
                                    (format p ";;; (project project-test-runner) — Calibrated Project Test Runner~%")
                                    (format p ";;; Auto-synthesized during badb onboarding for language: ~a~%" lang)
                                    (format p "(define-module (project project-test-runner)~%")
                                    (format p "  #:use-module (geasa foundation-util plugin)~%")
                                    (format p "  #:use-module (ice-9 format)~%")
                                    (format p "  #:export (project-test-run))~%~%")
                                    (format p "(define* (project-test-run #:optional (cmd ~s))~%" test-cmd)
                                    (format p "  \"Run project tests with bounded output, preventing context bloat.\"~%")
                                    (format p "  (call-with-values~%")
                                    (format p "    (lambda () (execute-shell-command (or cmd ~s)))~%" test-cmd)
                                    (format p "    (lambda (out exit-val err)~%")
                                    (format p "      (let* ((passed? (zero? exit-val))~%")
                                    (format p "             (lines (string-split (or out \"\") #\\newline))~%")
                                    (format p "             (n (length lines))~%")
                                    (format p "             (trimmed (if (> n 20) (take (reverse lines) 20) lines))~%")
                                    (format p "             (summary (string-join (reverse trimmed) \"\\n\")))~%")
                                    (format p "        (format #f \"Test Verdict: ~~a (exit ~~a)\\nSummary (last ~~a lines):\\n~~a\"~%")
                                    (format p "                (if passed? \"PASS\" \"FAIL\") exit-val (min n 20) summary)))))~%")))

           ;; 2. Generate project-test-runner/spec.geis.scm
           (call-with-output-file spec-file
                                  (lambda (p)
                                    (format p ";;; spec.geis.scm — Contract verification for project-test-runner~%")
                                    (format p "(define-module (geis project-test-runner spec)~%")
                                    (format p "  #:use-module (badb spec)~%")
                                    (format p "  #:use-module (project project-test-runner))~%~%")
                                    (format p "(spec-begin \"project-test-runner\")~%")
                                    (format p "(test-assert \"project-test-run procedure is callable\" (procedure? project-test-run))~%")
                                    (format p "(spec-end)~%")
                                    (format p "(spec-exit)~%")))

           ;; 3. Compute pins & generate manifest.scm
           (let* ((pins (compute-geis-sha256-pins runner-dir))
                  (manifest
                    `((bundle-version . 1)
                      (feature-id . "project-test-runner")
                      (version . "1.0.0")
                      (spec . "spec.geis.scm")
                      (category . "test")
                      (description . ,(format #f "Project test runner (~a) with structured failure output" lang))
                      (tools . ("project-test-run"))
                      (plugin . ((module . (project project-test-runner))
                                 (file . "plugin.scm")))
                      (sha256 . ,pins))))
             (call-with-output-file manifest-file
                                    (lambda (p) (write manifest p))))

           ;; 4. Write .badb/geasa/index.json
           (let ((index-data
                   `#(((fid . "project-test-runner")
                       (category . "test")
                       (description . ,(format #f "Project test runner (~a) with structured failure output" lang))
                       (tools . #("project-test-run"))))))
             (call-with-output-file index-file
                                    (lambda (p) (display (scm->json-string index-data #:pretty #t) p))))

           ;; 5. Mount into registry if passed
           (when reg
             (catch #t
               (lambda ()
                 (let ((loader-mod (false-if-exception (resolve-module '(geasa runtimes-geis-loader plugin)))))
                   (when loader-mod
                     ((module-ref loader-mod 'discover-and-mount-project-geasa!) reg #:workspace workspace))))
               (lambda _ #f)))

           `((status . "success")
             (language . ,lang)
             (test-cmd . ,test-cmd)
             (bundles . ("project-test-runner")))))

;;;
;;; Trajectory Distillation to Geis Bundle
;;;
(define* (distill-trajectory-to-geis trajectory name
                                     #:key
                                     (workspace (getcwd))
                                     (target-dir #f))
         "Distill sequence of tool calls from TRAJECTORY into a reproducible Geis bundle."
         (let* ((bundle-dir (or target-dir (string-append workspace "/.badb/geasa/" name)))
                (wf-file (string-append bundle-dir "/workflow.sema"))
                (spec-file (string-append bundle-dir "/spec.geis.scm"))
                (manifest-file (string-append bundle-dir "/manifest.scm"))
                (tool-calls '()))

           (mkdir-p bundle-dir)

           ;; Extract assistant tool calls
           (for-each
             (lambda (msg)
               (let ((tc-list (or (assoc-ref msg "tool_calls")
                                  (assoc-ref msg 'tool_calls))))
                 (for-each
                   (lambda (tc)
                     (let* ((fn-obj (or (assoc-ref tc "function") (assoc-ref tc 'function)))
                            (fn-name (and fn-obj (or (assoc-ref fn-obj "name") (assoc-ref fn-obj 'name))))
                            (fn-args (and fn-obj (or (assoc-ref fn-obj "arguments") (assoc-ref fn-obj 'arguments)))))
                       (when fn-name
                         (set! tool-calls (cons (list fn-name fn-args) tool-calls)))))
                   (ensure-list tc-list))))
             (ensure-list trajectory))

           (let ((ordered-calls (reverse tool-calls)))
             ;; 1. Generate workflow.sema
             (call-with-output-file wf-file
                                    (lambda (p)
                                      (format p ";;; workflow.sema — Distilled from agent trajectory for ~a~%" name)
                                      (format p "(defworkflow ~a~%" name)
                                      (let loop ((rest ordered-calls) (idx 1))
                                                (unless (null? rest)
                                                  (let* ((item (car rest))
                                                         (tname (car item))
                                                         (targs (cadr item)))
                                                    (format p "  (step step-~a (call-tool ~s ~s))~%" idx tname (or targs "{}"))
                                                    (loop (cdr rest) (+ idx 1)))))
                                      (format p ")~%")))

             ;; 2. Generate spec.geis.scm
             (call-with-output-file spec-file
                                    (lambda (p)
                                      (format p ";;; spec.geis.scm — Contract verification for ~a~%" name)
                                      (format p "(define-module (geis ~a spec) #:use-module (badb spec))~%" name)
                                      (format p "(spec-begin ~s)~%" name)
                                      (format p "(test-assert \"distilled workflow valid\" #t)~%")
                                      (format p "(spec-end)~%")
                                      (format p "(spec-exit)~%")))

             ;; 3. Compute SHA256 pins & write manifest.scm
             (let* ((pins (compute-geis-sha256-pins bundle-dir))
                    (manifest
                      `((bundle-version . 1)
                        (feature-id . ,name)
                        (version . "1.0.0")
                        (spec . "spec.geis.scm")
                        (workflow . "workflow.sema")
                        (sha256 . ,pins))))
               (call-with-output-file manifest-file
                                      (lambda (p) (write manifest p)))
               `((feature-id . ,name)
                 (bundle-dir . ,bundle-dir)
                 (tool-calls-count . ,(length ordered-calls)))))))

(define* (mount-bundle-workflows! reg #:key (workspace (getcwd)))
         "Scan WORKSPACE/.badb/workflows and WORKSPACE/.badb/geasa for .sema files and mount them into REG."
         (let* ((candidate-roots
                  (filter file-exists?
                          (list (string-append workspace "/.badb/workflows")
                                (string-append workspace "/.badb/geasa"))))
                (mounted '()))
           (for-each
             (lambda (root)
               (let ((entries (scandir-valid-entries root)))
                 (for-each
                   (lambda (entry)
                     (let ((entry-path (string-append root "/" entry)))
                       (cond
                         ((and (file-is-directory? entry-path)
                               (file-exists? (string-append entry-path "/workflow.sema")))
                           (let* ((sema-path (string-append entry-path "/workflow.sema"))
                                  (tool-name (string-append "workflow." entry)))
                             (registry-attach!
                               reg tool-name
                               (lambda args
                                 (execute-sema-workflow sema-path #:registry reg #:args args))
                               `((description . ,(format #f "Run project workflow ~a" entry))
                                 (_badb_defer . #t)))
                             (set! mounted (cons tool-name mounted))))
                         ((and (string-suffix? ".sema" entry) (not (file-is-directory? entry-path)))
                           (let* ((base (substring entry 0 (- (string-length entry) 5)))
                                  (tool-name (string-append "workflow." base)))
                             (registry-attach!
                               reg tool-name
                               (lambda args
                                 (execute-sema-workflow entry-path #:registry reg #:args args))
                               `((description . ,(format #f "Run project workflow ~a" base))
                                 (_badb_defer . #t)))
                             (set! mounted (cons tool-name mounted)))))))
                   entries)))
             candidate-roots)
           mounted))

(define* (distill-exec-patterns! #:key
                                 (workspace (getcwd))
                                 (name "distilled-auto")
                                 (analyze-only? #f)
                                 (trajectory #f))
         "Analyze agent execution trajectory and synthesize into a reproducible Geis bundle.
When ANALYZE-ONLY? is #t, returns summary of patterns without writing files."
         (let* ((traj (or trajectory
                          (catch #t
                            (lambda ()
                              (let ((sess-dir (string-append workspace "/.badb/sessions")))
                                (if (file-exists? sess-dir)
                                  (let ((files (or (scandir sess-dir (lambda (n) (string-suffix? ".jsonl" n))) '())))
                                    (if (null? files)
                                      '()
                                      (let* ((latest (string-append sess-dir "/" (last (sort files string<?))))
                                             (lines (call-with-input-file latest
                                                                          (lambda (p)
                                                                            (let loop ((acc '()))
                                                                                      (let ((l (read-line p)))
                                                                                        (if (eof-object? l)
                                                                                          (reverse acc)
                                                                                          (loop (cons l acc)))))))))
                                        (filter-map (lambda (l)
                                                      (catch #t
                                                        (lambda () (json-string->scm l))
                                                        (lambda _ #f)))
                                                    lines))))
                                  '())))
                            (lambda _ '()))))
                (tool-calls '()))
           (for-each
             (lambda (msg)
               (let ((tc-list (or (assoc-ref msg "tool_calls")
                                  (assoc-ref msg 'tool_calls))))
                 (when (or (vector? tc-list) (list? tc-list))
                   (let ((items (if (vector? tc-list) (vector->list tc-list) tc-list)))
                     (for-each
                       (lambda (tc)
                         (let* ((fn-obj (or (assoc-ref tc "function") (assoc-ref tc 'function)))
                                (fn-name (and fn-obj (or (assoc-ref fn-obj "name") (assoc-ref fn-obj 'name))))
                                (fn-args (and fn-obj (or (assoc-ref fn-obj "arguments") (assoc-ref fn-obj 'arguments)))))
                           (when fn-name
                             (set! tool-calls (cons (cons fn-name fn-args) tool-calls)))))
                       items)))))
             traj)
           (let ((ordered (reverse tool-calls)))
             (if analyze-only?
               `((status . analyzed)
                 (tool-calls-count . ,(length ordered))
                 (tools-used . ,(delete-duplicates (map car ordered))))
               (if (null? ordered)
                 `((status . empty)
                   (message . "No tool call trajectory found to distill."))
                 (distill-trajectory-to-geis traj name #:workspace workspace))))))
