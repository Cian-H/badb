;;; badb — HTTP Transport & Mock Provider Stubs
;;;
;;; Parameterized HTTP transport primitives, curl fallback pipelines,
;;; and mock test injection hooks.

(define-module (badb providers mock)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (web http)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (%request-timeout-seconds
            curl-header-args
            curl-collect-output!
            curl-output->response
            curl-http-post
            gnutls-unavailable?
            default-http-post
            current-http-post
            curl-http-get
            default-http-get
            current-http-get))

;;;
;;; HTTP Request Execution & Timeout Management
;;;
(define %request-timeout-seconds
  (make-parameter
    (or (and=> (getenv "BADB_REQUEST_TIMEOUT") string->number)
        600)))

(define (curl-header-args headers accept)
  "Build curl -H argument pairs from HEADERS, coercing header values and
substituting ACCEPT for the accept header."
  (append-map
    (lambda (h)
      (let* ((key (car h)) (val (cdr h))
             (str-val
               (cond
                 ((string? val) val)
                 ((and (eq? key 'authorization) (pair? val)) (format #f "Bearer ~a" (cadr val)))
                 ((eq? key 'content-type) "application/json; charset=utf-8")
                 ((eq? key 'accept) accept)
                 (else (format #f "~a" val)))))
        (list "-H" (format #f "~a: ~a" key str-val))))
    (or headers '())))

(define (curl-collect-output! in pids)
  "Read the remaining output of a curl pipeline from IN, close it, reap the
child PIDS, and return the captured output string."
  (let ((out-str (read-string in)))
    (close-port in)
    (for-each
      (lambda (p)
        (catch 'system-error
          (lambda () (waitpid p))
          (lambda _ #f)))
      pids)
    (or out-str "")))

(define (curl-output->response out-str)
  "Split the trailing status code appended by curl's -w option from OUT-STR
and return (values response body-text)."
  (let* ((trimmed (or out-str ""))
         (last-nl (string-rindex trimmed #\newline))
         (code-str
           (if last-nl
             (substring trimmed (+ last-nl 1))
             "200"))
         (body-text
           (if last-nl
             (substring trimmed 0 last-nl)
             trimmed))
         (status-code (or (string->number (string-trim-both code-str)) 200)))
    (values (build-response #:code status-code) body-text)))

(define* (curl-http-post uri #:key body headers (streaming? #f))
  "Execute an HTTP POST using curl subprocess via stdin pipeline. Used as a transparent fallback
when Guile GnuTLS is not available for HTTPS requests.
Streams payload directly over standard input without temporary files, captures real HTTP status codes."
  (let*
      ((url
         (if (string? uri)
           uri
           (uri->string uri)))
       (timeout-args
         (let ((secs (%request-timeout-seconds)))
           (if (and (number? secs) (> secs 0))
             (list "--max-time" (number->string (inexact->exact secs)))
             '())))
       (header-args
         (curl-header-args
           headers
           (if streaming? "text/event-stream" "application/json")))
       (body-str
         (cond
           ((string? body) body)
           ((bytevector? body) (utf8->string body))
           (else ""))))
    (if streaming?
      (let
          ((cmd
             (append
               (list "curl" "-s" "-N" "-X" "POST" url "--data-binary" "@-")
               timeout-args
               header-args)))
        (call-with-values
          (lambda ()
            (pipeline (list cmd)))
          (lambda (in out pids)
            (display body-str out)
            (close-port out)
            (values (build-response #:code 200) in))))
      (let
          ((cmd
             (append
               (list "curl"
                     "-s"
                     "-w"
                     (format #f "~a%{http_code}" #\newline)
                     "-X"
                     "POST"
                     url
                     "--data-binary"
                     "@-")
               timeout-args
               header-args)))
        (call-with-values
          (lambda ()
            (pipeline (list cmd)))
          (lambda (in out pids)
            (display body-str out)
            (close-port out)
            (curl-output->response (curl-collect-output! in pids))))))))

(define (gnutls-unavailable? key args)
  "Return #t when a web client exception (KEY, ARGS) indicates missing GnuTLS
support for HTTPS requests."
  (or (eq? key 'gnutls-not-available)
      (and (pair? args) (string? (car args)) (string-contains (car args) "gnutls"))))

(define* (default-http-post uri #:key body headers (streaming? #f))
  "Dispatch HTTP POST to the bounded curl transport when a request timeout is
configured (BADB_REQUEST_TIMEOUT seconds, default 600), otherwise to the web
client with a curl fallback for missing gnutls: the web client has no timeout
support, so it is only usable when requests are allowed to run unbounded."
  (if (> (%request-timeout-seconds) 0)
    (curl-http-post uri #:body body #:headers headers #:streaming? streaming?)
    (catch #t
      (lambda ()
        (http-post uri #:body body #:headers headers #:streaming? streaming?))
      (lambda (key . args)
        (if (gnutls-unavailable? key args)
            (curl-http-post uri #:body body #:headers headers #:streaming? streaming?)
            (apply throw key args))))))

(define current-http-post
  (make-parameter default-http-post))

(define* (curl-http-get uri #:key headers)
  "Execute an HTTP GET using a curl subprocess. Used as a transparent fallback
when Guile GnuTLS is not available for HTTPS requests.
Captures real HTTP status codes."
  (let* ((url (if (string? uri) uri (uri->string uri)))
         (header-args (curl-header-args headers "application/json"))
         (cmd (append (list "curl"
                            "-s"
                            "-w"
                            (format #f "~a%{http_code}" #\newline)
                            url)
                      header-args)))
    (call-with-values
        (lambda () (pipeline (list cmd)))
      (lambda (in out pids)
        (close-port out)
        (curl-output->response (curl-collect-output! in pids))))))

(define* (default-http-get uri #:key headers)
  "Dispatch HTTP GET to web client, falling back to curl if gnutls is not available."
  (catch #t
    (lambda ()
      (http-get uri #:headers headers))
    (lambda (key . args)
      (if (gnutls-unavailable? key args)
          (curl-http-get uri #:headers headers)
          (apply throw key args)))))

(define current-http-get
  (make-parameter default-http-get))
