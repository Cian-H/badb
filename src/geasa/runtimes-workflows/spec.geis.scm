;;; (geasa runtimes-workflows spec.geis) — Contract verification for Workflows

(define-module (test test-runtimes-workflows)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (badb geis bundle)
  #:use-module (badb kernel registry)
  #:use-module (geasa runtimes-workflows plugin)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1))

(spec-begin "runtimes-workflows")

(test-group "workflows-bootstrap"
  (let* ((tmp-dir (mkdtemp "/tmp/badb-bootstrap-test-XXXXXX"))
         (_ (bootstrap-project-geasa! tmp-dir)))
    (test-assert ".badb/rules.scm created"
      (file-exists? (string-append tmp-dir "/.badb/rules.scm")))
    (test-assert ".badb/workflows/verify.sema created"
      (file-exists? (string-append tmp-dir "/.badb/workflows/verify.sema")))
    (test-assert ".badb/memories/architecture.md created"
      (file-exists? (string-append tmp-dir "/.badb/memories/architecture.md")))

    (let ((rules-content (call-with-input-file (string-append tmp-dir "/.badb/rules.scm") get-string-all)))
      (test-assert "rules contain documentation-integrity"
        (string-contains rules-content "documentation-integrity")))))

(test-group "workflows-execution"
  (let* ((tmp-dir (mkdtemp "/tmp/badb-workflow-exec-XXXXXX"))
         (wf-path (string-append tmp-dir "/math.sema"))
         (reg (make-registry)))
    (registry-attach! reg "add" (lambda (a b) (+ a b)))
    (registry-attach! reg "mul" (lambda (a b) (* a b)))

    (call-with-output-file wf-path
      (lambda (p)
        (display "(defworkflow test-math\n  (step step-1 (call-tool \"add\" 10 20))\n  (step step-2 (call-tool \"mul\" 5 6)))\n" p)))

    (let ((result (execute-sema-workflow wf-path #:registry reg)))
      (test-equal "workflow status ok" 'ok (assoc-ref result 'status))
      (let ((executed (assoc-ref result 'executed)))
        (test-equal "2 steps executed" 2 (length executed))
        (test-equal "add step result is 30" 30 (assoc-ref executed "add"))
        (test-equal "mul step result is 30" 30 (assoc-ref executed "mul"))))))

(test-group "workflows-distillation"
  (let* ((tmp-dir (mkdtemp "/tmp/badb-distill-test-XXXXXX"))
         (trajectory
          `(((role . "user") (content . "Inspect files"))
            ((role . "assistant")
             (content . "Checking git")
             (tool_calls . #(((id . "c1") (type . "function")
                               (function . ((name . "git_status") (arguments . "{}")))))))
            ((role . "tool") (content . "clean"))
            ((role . "assistant")
             (content . "Running tests")
             (tool_calls . #(((id . "c2") (type . "function")
                               (function . ((name . "run_tests") (arguments . "{}")))))))
            ((role . "tool") (content . "all passed"))))
         (report (distill-trajectory-to-geis trajectory "auto-verify" #:workspace tmp-dir))
         (bundle-dir (assoc-ref report 'bundle-dir)))

    (test-assert "distill reports 2 tool calls" (= 2 (assoc-ref report 'tool-calls-count)))
    (test-assert "workflow.sema was created" (file-exists? (string-append bundle-dir "/workflow.sema")))
    (test-assert "spec.geis.scm was created" (file-exists? (string-append bundle-dir "/spec.geis.scm")))
    (test-assert "manifest.scm was created" (file-exists? (string-append bundle-dir "/manifest.scm")))

    ;; Validate distilled bundle against geis-manifest-valid?
    (let ((errors (geis-manifest-valid? bundle-dir)))
      (test-assert "distilled bundle manifest is valid" (null? errors)))))

(test-group "workflows-onboard"
  (let* ((tmp-dir (mkdtemp "/tmp/badb-onboard-test-XXXXXX"))
         (_ (call-with-output-file (string-append tmp-dir "/Cargo.toml")
              (lambda (p) (display "[package]\nname = \"demo\"\n" p))))
         (profile (profile-workspace-environment tmp-dir))
         (report (onboard-project-geasa! tmp-dir)))
    (test-equal "profile detects rust" "rust" (assoc-ref profile 'language))
    (test-equal "profile assigns cargo test" "cargo test" (assoc-ref profile 'test-cmd))
    (test-equal "onboard status is success" "success" (assoc-ref report 'status))
    (test-assert "project-test-runner bundle created"
      (file-exists? (string-append tmp-dir "/.badb/geasa/project-test-runner/plugin.scm")))
    (test-assert "project-test-runner spec created"
      (file-exists? (string-append tmp-dir "/.badb/geasa/project-test-runner/spec.geis.scm")))
    (test-assert "project-test-runner manifest created"
      (file-exists? (string-append tmp-dir "/.badb/geasa/project-test-runner/manifest.scm")))
    (test-assert "index.json created"
      (file-exists? (string-append tmp-dir "/.badb/geasa/index.json")))))

(spec-end)
(spec-exit)
