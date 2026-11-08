;;; (geasa agents-subagent spec.geis) — Contract verification for Sub-Agent Specialization

(define-module (test test-agents-subagent)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (badb kernel registry)
  #:use-module (geasa providers-model common)
  #:use-module (geasa providers-model mock)
  #:use-module (geasa agents-subagent plugin)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1))

(spec-begin "agents-subagent")

(test-group "subagent-investigate-execution"
  (let* ((tmp-dir (mkdtemp "/tmp/badb-subagent-test-XXXXXX"))
         (journal-dir (string-append tmp-dir "/subagent-journal"))
         (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
         (mock-poster
          (lambda* (uri #:key body headers streaming?)
            (let ((resp-body (scm->json-string
                              '((choices . #(((message . ((role . "assistant")
                                                          (content . "Synthesized finding: Symbol X is defined in core.scm."))))))))))
              (values (build-response #:code 200) resp-body))))
         (reg (make-registry))
         (_ (registry-attach! reg "read_file" (lambda (p) "file content") '((description . "read"))))
         (_ (registry-attach! reg "dangerous_delete" (lambda (p) "deleted") '((description . "write")))))

    (parameterize ((current-http-post mock-poster))
      (register-subagent-tools! reg #:client client #:journal-dir journal-dir)

      (let ((investigate-fn (registry-lookup reg "investigate")))
        (test-assert "investigate tool is attached" (procedure? investigate-fn))

        (let ((result (investigate-fn `((prompt . "Inspect Symbol X")))))
          (test-assert "returns synthesized answer string" (string? result))
          (test-assert "answer contains findings"
            (string-contains result "Symbol X is defined in core.scm"))

          ;; Verify trajectory was written to journal-dir
          (test-assert "journal directory exists" (file-exists? journal-dir))
          (let ((files (or (scandir journal-dir (lambda (n) (string-suffix? ".jsonl" n))) '())))
            (test-assert "trajectory jsonl file created" (pair? files))
            (let* ((traj-path (string-append journal-dir "/" (car files)))
                   (content (call-with-input-file traj-path get-string-all)))
              (test-assert "trajectory contains user request" (string-contains content "Inspect Symbol X"))
              (test-assert "trajectory contains assistant response" (string-contains content "Symbol X is defined")))))))
    (system* "rm" "-rf" tmp-dir)))

(test-group "subagent-parallel-execution"
  (let* ((tmp-dir (mkdtemp "/tmp/badb-subagent-par-XXXXXX"))
         (journal-dir (string-append tmp-dir "/subagent-journal"))
         (client (make-model-client #:endpoint "http://127.0.0.1:10100" #:model "mock-llm"))
         (mock-poster
          (lambda* (uri #:key body headers streaming?)
            (let ((resp-body (scm->json-string
                              '((choices . #(((message . ((role . "assistant")
                                                          (content . "Answer for query."))))))))))
              (values (build-response #:code 200) resp-body))))
         (reg (make-registry)))

    (parameterize ((current-http-post mock-poster))
      (register-subagent-tools! reg #:client client #:journal-dir journal-dir)
      (let ((par-fn (registry-lookup reg "investigate_parallel")))
        (test-assert "investigate_parallel tool is attached" (procedure? par-fn))

        (let ((result (par-fn `((queries . #("What is A?" "What is B?"))))))
          (test-assert "contains Query A" (string-contains result "What is A?"))
          (test-assert "contains Query B" (string-contains result "What is B?"))
          (test-assert "contains Answer" (string-contains result "Answer for query.")))))
    (system* "rm" "-rf" tmp-dir)))

(spec-end)
(spec-exit)
