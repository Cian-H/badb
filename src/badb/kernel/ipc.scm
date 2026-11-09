;;; (badb kernel ipc) — Transport-agnostic JSON-RPC 2.0 / stdio IPC primitives
(define-module (badb kernel ipc)
               #:use-module (srfi srfi-9)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 format)
               #:use-module (json)
               #:export (<ipc-channel> make-ipc-channel ipc-channel?
                                       ipc-channel-in-port ipc-channel-out-port ipc-channel-pid
                                       ipc-channel-closed? ipc-channel-close!
                                       ipc-send-request! ipc-send-notification! ipc-read-message
                                       ipc-call ipc-result-response ipc-error-response ipc-write-response!))

(define-record-type <ipc-channel>
                    (%make-ipc-channel in-port out-port pid next-id closed?)
                    ipc-channel?
                    (in-port ipc-channel-in-port)
                    (out-port ipc-channel-out-port)
                    (pid ipc-channel-pid)
                    (next-id %ipc-channel-next-id %set-ipc-channel-next-id!)
                    (closed? ipc-channel-closed? %set-ipc-channel-closed?!))

(define* (make-ipc-channel in-port out-port #:optional (pid #f))
         (%make-ipc-channel in-port out-port pid 1 #f))

(define (ipc-channel-close! chan)
        (unless (ipc-channel-closed? chan)
          (%set-ipc-channel-closed?! chan #t)
          (catch #t (lambda () (close-port (ipc-channel-in-port chan))) (lambda _ #f))
          (catch #t (lambda () (close-port (ipc-channel-out-port chan))) (lambda _ #f))
          (let ((pid (ipc-channel-pid chan)))
            (when (and pid (integer? pid) (> pid 0))
              (catch #t
                (lambda ()
                  (let ((reaped (waitpid pid WNOHANG)))
                    (when (and (pair? reaped) (zero? (car reaped)))
                      (kill pid SIGTERM)
                      (usleep 10000)
                      (let ((r2 (waitpid pid WNOHANG)))
                        (when (and (pair? r2) (zero? (car r2)))
                          (kill pid SIGKILL)
                          (waitpid pid))))))
                (lambda _ #f))))
          #t))

(define* (ipc-send-request! chan method #:optional params (id #f))
         (let* ((req-id (or id (%ipc-channel-next-id chan)))
                (out (ipc-channel-out-port chan))
                (method-str (cond ((string? method) method) ((symbol? method) (symbol->string method))
                                  (else (format #f "~a" method))))
                (msg `((jsonrpc . "2.0") (id . ,req-id) (method . ,method-str)
                       ,@(if params `((params . ,params)) '()))))
           (unless id (%set-ipc-channel-next-id! chan (+ req-id 1)))
           (display (scm->json-string msg) out)
           (newline out)
           (force-output out)
           req-id))

(define* (ipc-send-notification! chan method #:optional params)
         (let* ((out (ipc-channel-out-port chan))
                (method-str (cond ((string? method) method) ((symbol? method) (symbol->string method))
                                  (else (format #f "~a" method))))
                (msg `((jsonrpc . "2.0") (method . ,method-str)
                       ,@(if params `((params . ,params)) '()))))
           (display (scm->json-string msg) out)
           (newline out)
           (force-output out)
           *unspecified*))

(define (ipc-read-message chan)
        (let ((in (ipc-channel-in-port chan)))
          (let loop ()
                    (let ((line (read-line in)))
                      (cond
                        ((eof-object? line) line)
                        ((string-null? (string-trim-both line)) (loop))
                        (else
                          (catch #t
                            (lambda () (json-string->scm line))
                            (lambda _ (loop)))))))))

(define* (ipc-call chan method #:optional params)
         (let ((req-id (ipc-send-request! chan method params)))
           (let loop ()
                     (let ((msg (ipc-read-message chan)))
                       (cond
                         ((eof-object? msg) (error "ipc-call: unexpected EOF waiting for response to request" req-id))
                         ((not (list? msg)) (error "ipc-call: expected JSON object" msg))
                         (else
                           (let ((id-pair (or (assoc "id" msg) (assoc 'id msg))))
                             (if id-pair
                               (let ((resp-id (cdr id-pair))
                                     (err-pair (or (assoc "error" msg) (assoc 'error msg)))
                                     (res-pair (or (assoc "result" msg) (assoc 'result msg))))
                                 (unless (equal? resp-id req-id)
                                   (error "ipc-call: response ID mismatch" req-id resp-id msg))
                                 (if err-pair
                                   (scm-error 'ipc-error "ipc-call" "JSON-RPC error: ~s" (list (cdr err-pair)) (list (cdr err-pair)))
                                   (if res-pair (cdr res-pair)
                                                (error "ipc-call: response missing both result and error" msg))))
                               (loop)))))))))

(define (ipc-result-response id result)
        `((jsonrpc . "2.0") (id . ,id) (result . ,result)))

(define (ipc-error-response id code message)
        `((jsonrpc . "2.0") (id . ,id) (error . ((code . ,code) (message . ,message)))))

(define* (ipc-write-response! resp #:optional (port (current-output-port)))
         (catch #t
           (lambda () (display (scm->json-string resp) port) (newline port) (force-output port))
           (lambda _ #f)))
