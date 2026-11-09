;;; (geasa capabilities-scheme-eval plugin) — Sandboxed Scheme Evaluation
;;;
;;; Evaluates Scheme expressions in an isolated module with tool composition
;;; via (call-tool name . args), timeout enforcement, guardrail inspection,
;;; and output size bounding.

(define-module (geasa capabilities-scheme-eval plugin)
  #:use-module (badb kernel registry)
  #:use-module (geasa policies-guardrail plugin)
  #:use-module (geasa foundation-util plugin)
  #:use-module (json)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (register-scheme-eval-tools!
            eval-scheme-sandboxed
            call-with-timeout
            %scheme-eval-tool-schema))

(define %restricted-sandbox-symbols
  '(system system* execl execlp execle primitive-fork
    pipeline open-pipe open-pipe* open-input-pipe open-output-pipe
    delete-file rmdir mkdir chmod chown setenv putenv
    primitive-exit exit kill sigaction alarm))

(define (call-with-timeout timeout-sec thunk timeout-val)
  "Execute THUNK with a SIGALRM timeout. Return TIMEOUT-VAL if timeout trips."
  (let ((prev-handler (sigaction SIGALRM)))
    (catch 'timeout-tag
      (lambda ()
        (dynamic-wind
          (lambda ()
            (sigaction SIGALRM (lambda (sig) (throw 'timeout-tag)))
            (alarm timeout-sec))
          (lambda ()
            (thunk))
          (lambda ()
            (alarm 0)
            (sigaction SIGALRM (car prev-handler)))))
      (lambda (k . a)
        timeout-val))))

(define* (eval-scheme-sandboxed code-str
                                #:key
                                (registry #f)
                                (guardrail #f)
                                (timeout-seconds 10)
                                (max-output-chars 4000))
  "Evaluate CODE-STR inside an isolated module with (call-tool name . args) support."
  (catch #t
    (lambda ()
      (let* ((mod (make-fresh-feature-module '(sandbox eval-env)
                                            #:imports '((guile)
                                                        (srfi srfi-1)
                                                        (ice-9 format))))
             ;; Restrict unsafe host system operations in the sandbox module
             (_ (for-each
                 (lambda (sym)
                   (feature-module-define!
                    mod sym
                    (lambda _
                      (error (format #f "Security restriction: '~a' is forbidden in sandboxed evaluation" sym)))))
                 %restricted-sandbox-symbols))
             ;; Define call-tool inside the sandbox module
             (_ (feature-module-define!
                 mod
                 'call-tool
                 (lambda (name . args)
                   (let* ((arg-data (cond
                                     ((null? args) '())
                                     ((null? (cdr args)) (car args))
                                     (else args)))
                          (name-str (if (symbol? name) (symbol->string name) name)))
                     ;; Guardrail check
                     (when (and guardrail (guardrail-policy? guardrail))
                       (let ((decision (evaluate-guardrail guardrail name-str arg-data)))
                         (when (eq? (car decision) 'deny)
                           (throw 'guardrail-denied (cdr decision)))))
                     ;; Registry tool lookup
                     (if (and registry (registry? registry))
                         (let ((tool-fn (registry-lookup registry name-str)))
                           (if (procedure? tool-fn)
                               (tool-fn arg-data)
                               (throw 'tool-not-found name-str)))
                         (throw 'no-registry name-str))))))
             ;; Parse the code
             (expr (call-with-input-string code-str read)))
        ;; Execute with timeout
        (let* ((result-thunk
                (lambda ()
                  (let* ((ret #f)
                         (stdout-str
                          (with-output-to-string
                            (lambda ()
                              (set! ret (eval-in-feature-module mod expr))))))
                    (let ((val-str (if (string? ret) ret (object->string ret))))
                      (if (non-empty-string? stdout-str)
                          (format #f "Stdout:\n~a\nResult: ~a" stdout-str val-str)
                          val-str)))))
               (output
                (call-with-timeout (max 1 timeout-seconds)
                                   result-thunk
                                   (format #f "Execution timed out after ~a seconds" timeout-seconds))))
          ;; Bound output size
          (if (and (string? output) (> (string-length output) max-output-chars))
              (string-append (substring output 0 max-output-chars)
                             (format #f "\n[... truncated ~a characters ...]"
                                     (- (string-length output) max-output-chars)))
              output))))
    (lambda (key . args)
      (format #f "Evaluation error (~a): ~s" key args))))

(define %scheme-eval-tool-schema
  `((name . "scheme_eval")
    (description . "Execute Scheme expressions safely. Supports (call-tool name . args) to compose tools in a single turn without LLM roundtrips.")
    (parameters .
     ((type . "object")
      (properties .
       ((code . ((type . "string") (description . "Scheme expression to evaluate")))
        (timeout_seconds . ((type . "integer") (description . "Execution timeout in seconds (default: 10)")))))
      (required . #("code"))))))

(define* (register-scheme-eval-tools! reg #:key (guardrail #f))
  "Register the 'scheme_eval' tool into REG."
  (registry-attach!
   reg
   "scheme_eval"
   (lambda (args)
     (with-tool-args args ((code code "")
                           (timeout timeout_seconds 10))
       (eval-scheme-sandboxed code
                              #:registry reg
                              #:guardrail guardrail
                              #:timeout-seconds timeout)))
   #:metadata %scheme-eval-tool-schema))
