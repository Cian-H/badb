;;; A minimal JSON-RPC worker for untrusted project geis tools.
(define-module (badb geis worker)
               #:use-module (badb geis capabilities)
               #:use-module (ice-9 format)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 textual-ports)
               #:use-module (json)
               #:use-module (srfi srfi-1)
               #:export (run-geis-worker))

(define (read-json-line port)
        (let ((line (read-line port)))
          (if (eof-object? line) line (json->scm (open-input-string line)))))

(define (write-json-line port value)
        (display (scm->json-string value) port)
        (newline port)
        (force-output port))

(define (make-broker-grant output input id name target)
        (lambda args
          (write-json-line output
                           `((type . "capability") (id . ,id)
                             (name . ,(symbol->string name)) (target . ,target)
                             (args . ,(list->vector args))))
          (let ((reply (read-json-line input)))
            (unless (and (pair? reply)
                         (equal? (assoc-ref reply "type") "capability-result")
                         (= (assoc-ref reply "id") id))
              (error "invalid host capability broker response" reply))
            (if (assoc-ref reply "error")
              (error "host capability request denied" (assoc-ref reply "error"))
              (assoc-ref reply "value")))))

(define (run-geis-worker)
        (let* ((input (current-input-port))
               (output (current-output-port))
               (request (read-json-line input)))
          (unless (pair? request)
            (error "missing geis worker request"))
          (let* ((module-name
                   (call-with-input-string (assoc-ref request "module") read))
                 (plugin-file (assoc-ref request "plugin"))
                 (tool-name (string->symbol (assoc-ref request "tool")))
                 (args (vector->list (assoc-ref request "args")))
                 (capability-specs (vector->list (assoc-ref request "capabilities")))
                 (grants
                   (map (lambda (spec id)
                          (let* ((name (string->symbol (assoc-ref spec "name")))
                                 (target (assoc-ref spec "target")))
                            (cons (list name target)
                                  (make-broker-grant output input id name target))))
                        capability-specs
                        (iota (length capability-specs)))))
            (catch #t
              (lambda ()
                (parameterize ((current-output-port (current-error-port)))
                              (primitive-load plugin-file)
                              (let* ((module (resolve-module module-name))
                                     (procedure (module-ref module tool-name))
                                     (value
                                       (parameterize ((current-geis-capabilities grants))
                                                     (apply procedure args))))
                                (write-json-line output `((type . "result") (value . ,value))))))
              (lambda (key . data)
                (write-json-line output
                                 `((type . "result")
                                   (error . ,(format #f "~a: ~s" key data)))))))))

(run-geis-worker)
