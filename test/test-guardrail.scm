;;; test-guardrail.scm — Tests for Pluggable Guardrails & Policy Engine (badb guardrail)
;;;
;;; Verifies raising/lowering levels, pattern matching rules, default evaluator,
;;; algorithmic Scheme predicates, System 1 model evaluation, and journal auditing.

(define-module (test test-guardrail)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (web response)
  #:use-module (badb providers common)
  #:use-module (badb providers openai)
  #:use-module (badb providers mock)
  #:use-module (badb kernel journal)
  #:use-module (badb policies guardrail))

(test-begin "badb-guardrail")

;;;
;;; 1. Levels & Ranking
;;;

(test-group "guardrail-levels"
  (test-assert "paranoid is higher rank than cautious"
    (> (guardrail-level-rank 'paranoid) (guardrail-level-rank 'cautious)))
  (test-assert "cautious is higher rank than permissive"
    (> (guardrail-level-rank 'cautious) (guardrail-level-rank 'permissive)))
  (test-assert "permissive is higher rank than disabled"
    (> (guardrail-level-rank 'permissive) (guardrail-level-rank 'disabled)))

  (let ((policy (make-guardrail-policy #:level 'permissive)))
    (test-equal "initial level is permissive" 'permissive (guardrail-policy-level policy))
    ;; Raise level
    (set-guardrail-policy-level! policy 'strict)
    (test-equal "raised level is strict" 'strict (guardrail-policy-level policy))
    ;; Lower level
    (set-guardrail-policy-level! policy 'disabled)
    (test-equal "lowered level is disabled" 'disabled (guardrail-policy-level policy))))

;;;
;;; 2. Declarative Pattern Rules
;;;

(test-group "guardrail-pattern-rules"
  (let* ((rule-rm (make-pattern-rule #f "rm -rf" 'deny "Refusing dangerous deletion"))
         (rule-git (make-pattern-rule "git_.*" #f 'allow "All git commands approved"))
         (policy (make-guardrail-policy #:level 'cautious
                                       #:rules (list rule-rm rule-git))))

    ;; Matches rule-rm
    (let ((dec (evaluate-guardrail policy "shell_exec" '(("cmd" . "rm -rf /")))))
      (test-assert "matches deny rule" (guardrail-deny? dec))
      (test-assert "matches custom reason" (string-contains (cdr dec) "Refusing dangerous deletion")))

    ;; Matches rule-git
    (let ((dec (evaluate-guardrail policy "git_status" '())))
      (test-assert "matches allow rule" (guardrail-allow? dec))
      (test-assert "matches git reason" (string-contains (cdr dec) "All git commands approved")))))

;;;
;;; 3. Default Cautious Evaluator Behavior
;;;

(test-group "guardrail-default-evaluator"
  (let ((policy (make-guardrail-policy #:level 'cautious)))
    ;; Read-only: allowed
    (let ((dec (evaluate-guardrail policy "read_file" '(("path" . "README.md")))))
      (test-assert "read_file allowed in cautious mode" (guardrail-allow? dec)))

    ;; Mutation: ask
    (let ((dec (evaluate-guardrail policy "write_file" '(("path" . "out.txt")))))
      (test-assert "write_file requires ask in cautious mode" (guardrail-ask? dec)))

    ;; Built-in bash / sh tools are classified as mutating in cautious and paranoid modes
    (let ((dec-bash (evaluate-guardrail policy "bash" '(("command" . "touch foo.txt"))))
          (dec-sh (evaluate-guardrail policy "sh" '(("command" . "echo hi > bar.txt")))))
      (test-assert "bash requires ask in cautious mode" (guardrail-ask? dec-bash))
      (test-assert "sh requires ask in cautious mode" (guardrail-ask? dec-sh)))

    ;; Dangerous: denied (including case-insensitive and rm -fr variants)
    (let ((dec (evaluate-guardrail policy "shell_run" '(("cmd" . "rm -rf ."))))
          (dec-fr (evaluate-guardrail policy "bash" '(("cmd" . "rm -fr /tmp/foo"))))
          (dec-sql (evaluate-guardrail policy "bash" '(("cmd" . "DROP TABLE users;")))))
      (test-assert "destructive shell command denied in cautious mode" (guardrail-deny? dec))
      (test-assert "rm -fr denied in cautious mode" (guardrail-deny? dec-fr))
      (test-assert "uppercase DROP TABLE denied in cautious mode" (guardrail-deny? dec-sql))))

  ;; Paranoid mode denies bash / sh mutations
  (let* ((policy (make-guardrail-policy #:level 'paranoid))
         (dec (evaluate-guardrail policy "bash" '(("command" . "ls")))))
    (test-assert "paranoid level denies bash tool" (guardrail-deny? dec)))

  ;; Disabled mode allows all
  (let* ((policy (make-guardrail-policy #:level 'disabled))
         (dec (evaluate-guardrail policy "shell_run" '(("cmd" . "rm -rf .")))))
    (test-assert "disabled level allows all" (guardrail-allow? dec)))

  ;; Strict mode asks on all
  (let* ((policy (make-guardrail-policy #:level 'strict))
         (dec (evaluate-guardrail policy "read_file" '(("path" . "README.md")))))
    (test-assert "strict level asks on read" (guardrail-ask? dec))))

;;;
;;; 4. Custom Algorithmic Scheme Predicate Evaluator
;;;

(test-group "guardrail-scheme-predicate"
  (let* ((workspace-guard
          (make-scheme-predicate-evaluator
           (lambda (name args level context)
             (let ((path (assoc-ref args "path")))
               (if (and path (string-prefix? "/etc" path))
                   (cons 'deny "System directory access forbidden")
                   (cons 'allow "Workspace path approved"))))))
         (policy (make-guardrail-policy #:evaluator workspace-guard)))

    ;; Safe path
    (let ((dec (evaluate-guardrail policy "read_file" '(("path" . "/home/user/project/file.scm")))))
      (test-assert "safe path allowed" (guardrail-allow? dec))
      (test-equal "safe path reason" "Workspace path approved" (cdr dec)))

    ;; Forbidden path
    (let ((dec (evaluate-guardrail policy "read_file" '(("path" . "/etc/passwd")))))
      (test-assert "forbidden path denied" (guardrail-deny? dec))
      (test-equal "forbidden path reason" "System directory access forbidden" (cdr dec)))))

;;;
;;; 5. Algorithmic System 1 Fast Model Evaluator
;;;

(test-group "guardrail-system1-model"
  (let* ((fast-client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "system1-guard"))
         (mock-poster
          (lambda* (uri #:key body headers streaming?)
            (let ((resp-body (scm->json-string
                              '((choices . #(((message . ((role . "assistant")
                                                          (content . "{\"verdict\": \"allow\", \"reason\": \"Low risk introspection\"}"))))))))))
              (values (build-response #:code 200) resp-body))))
         (sys1-eval (make-system1-model-evaluator fast-client)))

    (parameterize ((current-http-post mock-poster))
      (let* ((policy (make-guardrail-policy #:evaluator sys1-eval))
             (dec (evaluate-guardrail policy "git_diff" '())))
        (test-assert "system 1 model approved" (guardrail-allow? dec))
        (test-equal "system 1 reason extracted" "Low risk introspection" (cdr dec))))))

;;;
;;; 6. Audit Logging & Journal Provenance
;;;

(test-group "guardrail-journal-audit"
  (let* ((jrn-path "/tmp/badb-test-guardrail-jrn.jsonl")
         (_ (when (file-exists? jrn-path) (delete-file jrn-path)))
         (jrn (make-journal jrn-path))
         (policy (make-guardrail-policy #:level 'cautious #:journal jrn)))

    (evaluate-guardrail policy "write_file" '(("path" . "foo.txt")))

    (test-equal "1 audit entry in journal" 1 (journal-count jrn))
    (let ((entry (car (journal-entries jrn))))
      (test-equal "feature-id is guardrail" "guardrail" (assoc-ref entry "feature-id"))
      (test-equal "verdict logged is ask" "ask" (assoc-ref entry "verdict"))
      (test-equal "tool logged is write_file" "write_file" (assoc-ref entry "tool-name"))
      (test-equal "level logged is cautious" "cautious" (assoc-ref entry "level")))))

(test-end "badb-guardrail")
