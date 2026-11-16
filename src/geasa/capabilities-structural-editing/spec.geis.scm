;;; spec.geis.scm — Contract spec for (geasa capabilities-structural-editing plugin)

(define-module (test capabilities-structural-editing-spec)
               #:use-module (srfi srfi-64)
               #:use-module (srfi srfi-1)
               #:use-module (badb spec)
               #:use-module (badb kernel registry)
               #:use-module (geasa runtimes-rollback plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa capabilities-structural-editing plugin))

(spec-begin "capabilities-structural-editing")

(define (%rm-rf dir)
        (when (and (string? dir) (file-exists? dir))
          (system* "rm" "-rf" dir)))

(test-group "graceful degradation when binary missing"
            (let ((orig-path (or (getenv "PATH") "")))
              (dynamic-wind
                (lambda () (setenv "PATH" "/nonexistent-bin-dir"))
                (lambda ()
                  (parameterize ((semcp-binary "/nonexistent/semcp-missing"))
                                (test-assert "semcp-available? is #f when missing"
                                             (not (semcp-available?)))
                                (test-equal "semcp-list-tools returns empty list"
                                            '()
                                            (semcp-list-tools))
                                (test-assert "semcp-call returns unavailable message"
                                             (string-contains
                                               (semcp-call "read_node" '())
                                               "unavailable"))
                                (let ((reg (make-registry)))
                                  (test-equal "register-structural-editing-tools! returns 0"
                                              0
                                              (register-structural-editing-tools! reg)))))
                (lambda () (setenv "PATH" orig-path)))))

(when (semcp-available?)
  (test-group
    "semcp tool discovery and session lifecycle"
    (let ((tools (semcp-list-tools)))
      (test-assert "semcp-list-tools returns non-empty list" (pair? tools))
      (test-assert "includes read_node"
                   (any
                     (lambda (m)
                       (equal? (assoc-ref m 'name) "read_node"))
                     tools))
      (test-assert "includes ast_modify"
                   (any
                     (lambda (m)
                       (equal? (assoc-ref m 'name) "ast_modify"))
                     tools))
      (test-assert "includes commit_workspace"
                   (any
                     (lambda (m)
                       (equal? (assoc-ref m 'name) "commit_workspace"))
                     tools))))
  (test-group
    "staged edit, commit_workspace rollback checkpoint, and registry"
    (let*
        ((ws (mkdtemp "/tmp/badb-semcp-spec-XXXXXX"))
         (sample (string-append ws "/sample.scm"))
         (orig-src "(define (add-one x) (+ x 1))\n")
         (rm (make-rollback-manager #:workspace-dir ws #:registry (make-registry))))
      (dynamic-wind
        (lambda ()
          #t)
        (lambda ()
          (string->file sample orig-src)
          (let
              ((init-out (semcp-init! #:workspace ws #:paths (list sample))))
            (test-assert "semcp-init! succeeds"
                         (and (string? init-out) (file-exists? (string-append ws "/.semcp/state.json")))))
          (let ((reg (make-registry)))
            (test-assert
              "register-structural-editing-tools! registers > 0 tools"
              (> (register-structural-editing-tools! reg #:workspace ws #:rollback-mgr rm) 0))
            (let
                ((read-fn (registry-lookup reg "read_node")))
              (test-assert "read_node registered in registry" (procedure? read-fn))
              (let ((out (read-fn `(("path" . #(0 0))))))
                (test-assert "read_node output includes add-one"
                             (and (string? out) (string-contains out "add-one"))))))
          ;; Stage an AST modification and commit with rollback checkpoint
          (semcp-call "ast_modify"
                      `(("path" . #(0 0 0 2)) ("action" . "overwrite") ("new_node" . "(+ x 2)"))
                      #:workspace
                      ws
                      #:rollback-mgr
                      rm)
          (test-equal
            "disk file unchanged before commit_workspace"
            orig-src
            (file->string sample))
          (semcp-call "commit_workspace" '() #:workspace ws #:rollback-mgr rm)
          (test-assert
            "disk file updated after commit_workspace"
            (string-contains (file->string sample) "(+ x 2)"))
          (test-assert
            "pre-commit rollback checkpoint created"
            (= 1 (length (list-checkpoints rm))))
          (rollback-latest! rm)
          (test-equal "rollback restores original file" orig-src (file->string sample)))
        (lambda ()
          (%rm-rf ws)))))
  (test-group "semcp lint validator"
              (let
                  ((ws (mkdtemp "/tmp/badb-semcp-lint-XXXXXX")))
                (dynamic-wind
                  (lambda ()
                    #t)
                  (lambda ()
                    (let
                        ((sample (string-append ws "/clean.scm")) (v (make-semcp-lint-validator)))
                      (string->file "(define (id x) x)\n" sample)
                      (test-assert
                        "clean scheme file passes lint validator"
                        (assoc-ref (v `((file . ,sample) (workspace . ,ws))) 'passed?))))
                  (lambda ()
                    (%rm-rf ws))))))

(spec-end)
(spec-exit)
