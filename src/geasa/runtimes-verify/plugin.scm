;;; badb — Neuro-Symbolic Verification Engine
;;;
;;; Bridges neural LLM code synthesis with deterministic symbolic verification gates:
;;; AST syntax checking, test runner execution, security policy gating,
;;; and transactional rollback on verification failure.

(define-module (geasa runtimes-verify plugin)
  #:use-module (geasa runtimes-rollback plugin)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<verifier>
            verify-scheme-syntax
            verify-test-suite
                                     make-verifier
                                     verifier?
                                     verifier-check-syntax?
                                     set-verifier-check-syntax!
                                     verifier-run-tests?
                                     set-verifier-run-tests!
                                     verifier-test-command
                                     set-verifier-test-command!
                                     verifier-custom-validators
                                     set-verifier-custom-validators!
                                     add-verifier-validator!
                                     verify-action
                                     with-neuro-symbolic-gate))

;;;
;;; Symbolic Checks
;;;
(define (verify-scheme-syntax code-or-path)
        "Verify syntax of Scheme code (passed as string or filepath).
Returns '((passed? . #t)) or '((passed? . #f) (error . reason))."
        (catch #t
          (lambda ()
            (let*
                ((is-file?
                   (and (string? code-or-path)
                        (not
                          (string-contains code-or-path
                                           "
"))
                        (file-exists? code-or-path)))
                 (port
                   (if is-file?
                     (open-input-file code-or-path)
                     (open-input-string code-or-path))))
              (dynamic-wind
                (lambda ()
                  #f)
                (lambda ()
                  (let loop
                    ()
                    (let ((form (read port)))
                      (if (eof-object? form)
                        '((passed? . #t))
                        (loop)))))
                (lambda ()
                  (close-port port)))))
          (lambda (key . args)
            `((passed? . #f) (error . ,(format #f "Syntax error (~a): ~s" key args))))))

(define* (verify-test-suite #:key (command "./scripts/test") (dir #f))
  "Execute test suite COMMAND and capture exit code and output.
Returns '((passed? . #t/#f) (exit-code . code) (output . output-string))."
  (catch #t
    (lambda ()
      (let ((orig-dir (and dir (getcwd))))
        (dynamic-wind
          (lambda ()
            (when dir (chdir dir)))
          (lambda ()
            (let*
                ((pipe (open-input-pipe (string-append command " 2>&1")))
                 (output (read-string pipe))
                 (status (close-pipe pipe))
                 (exit-code (status:exit-val status)))
              `((passed? . ,(zero? exit-code))
                (exit-code . ,exit-code)
                (output . ,(or output "")))))
          (lambda ()
            (when orig-dir (chdir orig-dir))))))
    (lambda (key . args)
      `((passed? . #f) (exit-code . -1)
        (output . ,(format #f "Failed to run test suite (~a): ~s" key args))))))

;;;
;;; Verifier Record
;;;
(define-record-type <verifier>
  (%make-verifier check-syntax? run-tests? test-command custom-validators)
  verifier?
  (check-syntax? verifier-check-syntax? set-verifier-check-syntax!)
  (run-tests? verifier-run-tests? set-verifier-run-tests!)
  (test-command verifier-test-command set-verifier-test-command!)
  (custom-validators verifier-custom-validators set-verifier-custom-validators!))

(define* (make-verifier #:key
                        (check-syntax? #t)
                        (run-tests? #f)
                        (test-command "./scripts/test")
                        (custom-validators '()))
  "Construct a new <verifier> record."
  (%make-verifier check-syntax? run-tests? test-command custom-validators))

(define (add-verifier-validator! verifier validator)
  "Register a custom validator procedure (lambda (context) -> ((passed? . #t/#f) ...))"
  (set-verifier-custom-validators!
   verifier
   (cons validator (verifier-custom-validators verifier))))

;;;
;;; Action Verification
;;;
(define* (verify-action verifier #:key (code #f) (file #f) (workspace-dir #f))
  "Run active verification checks against candidate CODE or FILE."
  (let ((failures '()))
    ;; 1. Syntax check
    (when (verifier-check-syntax? verifier)
      (cond
       ((or code file)
        (when (or (not file)
                  (string-suffix? ".scm" file)
                  (string-suffix? ".sema" file))
          (let* ((target (or code file))
                 (res (verify-scheme-syntax target)))
            (unless (assoc-ref res 'passed?)
              (set! failures (cons (assoc-ref res 'error) failures))))))
       ((and workspace-dir (file-exists? workspace-dir))
        (for-each
         (lambda (rel-path)
           (when (string-suffix? ".scm" rel-path)
             (let* ((full-path (string-append workspace-dir "/" rel-path))
                    (res (verify-scheme-syntax full-path)))
               (unless (assoc-ref res 'passed?)
                 (set! failures (cons (assoc-ref res 'error) failures))))))
         (scan-text-files workspace-dir)))))

    ;; 2. Test suite check
    (when (verifier-run-tests? verifier)
      (let ((res (verify-test-suite #:command (verifier-test-command verifier)
                                    #:dir workspace-dir)))
        (unless (assoc-ref res 'passed?)
          (set! failures (cons (format #f "Test suite failed (exit ~a):\n~a"
                                       (assoc-ref res 'exit-code)
                                       (assoc-ref res 'output))
                               failures)))))

    ;; 3. Custom validators
    (for-each
     (lambda (val-fn)
       (catch #t
         (lambda ()
           (let ((res (val-fn `((code . ,code) (file . ,file) (workspace . ,workspace-dir)))))
             (unless (assoc-ref res 'passed?)
               (set! failures (cons (or (assoc-ref res 'error) "Custom validation failed") failures)))))
         (lambda (k . a)
           (set! failures (cons (format #f "Validator threw exception: ~s" a) failures)))))
     (verifier-custom-validators verifier))

    (if (null? failures)
        '((passed? . #t))
        `((passed? . #f)
          (failures . ,(reverse failures))))))

;;;
;;; Gated Execution with Rollback
;;;
(define* (with-neuro-symbolic-gate verifier rollback-mgr desc thunk
                                  #:key (verify-target #f))
  "Execute THUNK inside a rollback checkpoint and verify the resulting state
using VERIFIER. If symbolic verification fails, the workspace is automatically
rolled back to the pre-action checkpoint and the verification diagnostic is returned."
  (let ((chkpt (create-checkpoint! rollback-mgr (format #f "pre-gate: ~a" desc))))
    (catch #t
      (lambda ()
        (let* ((thunk-result (thunk))
               (v-res (verify-action verifier
                                     #:file verify-target
                                     #:workspace-dir (rollback-manager-workspace-dir rollback-mgr))))
          (if (assoc-ref v-res 'passed?)
              `((status . "verified")
                (result . ,thunk-result))
              (begin
                ;; Verification failed: Rollback
                (rollback-to-checkpoint! rollback-mgr (checkpoint-id chkpt))
                `((status . "verification_failed")
                  (checkpoint-rolled-back . ,(checkpoint-id chkpt))
                  (failures . ,(assoc-ref v-res 'failures)))))))
      (lambda (key . args)
        ;; Exception during execution: Rollback
        (rollback-to-checkpoint! rollback-mgr (checkpoint-id chkpt))
        `((status . "execution_error")
          (checkpoint-rolled-back . ,(checkpoint-id chkpt))
          (error . ,(format #f "~a: ~s" key args)))))))
