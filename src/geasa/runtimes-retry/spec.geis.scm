;;; test-retry.scm — Tests for Error Recovery & Retry (geasa runtimes-retry plugin)
;;;
;;; Verifies policy construction and validation, backoff computation
;;; (exponential growth, cap, jitter bounds), the retry driver (first-try
;;; success, retry-then-success with recorded backoff schedule, immediate
;;; rethrow of non-retryable failures, exhaustion), failure classification,
;;; and the ON-RETRY observability callback.

(define-module (test test-retry)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (srfi srfi-1)
  #:use-module (geasa runtimes-retry plugin))

(spec-begin "runtimes-retry")

(define (recording-sleeper log)
  "Sleeper that never sleeps; records each delay into LOG."
  (lambda (seconds) (set-car! log (cons seconds (car log)))))

(define (counting-thunk failures-remaining value)
  "Thunk that throws 'transient until its counter empties, then returns VALUE.
Counter is a one-element list shared with the caller."
  (lambda ()
    (if (> (car failures-remaining) 0)
        (begin
          (set-car! failures-remaining (- (car failures-remaining) 1))
          (throw 'transient "attempt failed"))
        value)))

(define %jitter-free-policy
  (make-retry-policy #:max-attempts 4
                     #:initial-delay 1.0
                     #:multiplier 2.0
                     #:max-delay 5.0
                     #:jitter 0))

;;;
;;; 1. Policy Construction & Validation
;;;

(test-group "retry-policy-construction"
  (let ((policy (make-retry-policy)))
    (test-assert "retry-policy? recognizes record" (retry-policy? policy))
    (test-equal "default max-attempts is 3" 3 (retry-policy-max-attempts policy))
    (test-equal "default initial-delay is 0.25" 0.25 (retry-policy-initial-delay policy))
    (test-equal "default multiplier is 2.0" 2.0 (retry-policy-multiplier policy))
    (test-equal "default max-delay is 30.0" 30.0 (retry-policy-max-delay policy))
    (test-assert "default sleeper is a procedure"
      (procedure? (retry-policy-sleeper policy)))
    (test-assert "default retryable? is a procedure"
      (procedure? (retry-policy-retryable? policy)))
    (test-assert "default on-retry is #f" (not (retry-policy-on-retry policy))))

  (let ((custom (make-retry-policy #:max-attempts 7
                                   #:initial-delay 0.5
                                   #:multiplier 1.5
                                   #:max-delay 9.0
                                   #:jitter 0.2
                                   #:on-retry (lambda args #f))))
    (test-equal "custom max-attempts preserved" 7 (retry-policy-max-attempts custom))
    (test-equal "custom initial-delay preserved" 0.5 (retry-policy-initial-delay custom))
    (test-equal "custom multiplier preserved" 1.5 (retry-policy-multiplier custom))
    (test-equal "custom max-delay preserved" 9.0 (retry-policy-max-delay custom))
    (test-equal "custom jitter preserved" 0.2 (retry-policy-jitter custom))
    (test-assert "custom on-retry preserved"
      (procedure? (retry-policy-on-retry custom)))))

(test-group "retry-policy-validation"
  (define (rejects? thunk)
    (catch #t (lambda () (thunk) #f) (lambda args #t)))
  (test-assert "max-attempts 0 rejected"
    (rejects? (lambda () (make-retry-policy #:max-attempts 0))))
  (test-assert "non-integer max-attempts rejected"
    (rejects? (lambda () (make-retry-policy #:max-attempts 2.5))))
  (test-assert "negative initial-delay rejected"
    (rejects? (lambda () (make-retry-policy #:initial-delay -1))))
  (test-assert "multiplier below 1 rejected"
    (rejects? (lambda () (make-retry-policy #:multiplier 0.9))))
  (test-assert "max-delay below initial-delay rejected"
    (rejects? (lambda () (make-retry-policy #:initial-delay 2.0 #:max-delay 1.0))))
  (test-assert "jitter above 1 rejected"
    (rejects? (lambda () (make-retry-policy #:jitter 1.5))))
  (test-assert "non-procedure sleeper rejected"
    (rejects? (lambda () (make-retry-policy #:sleeper "nope"))))
  (test-assert "non-procedure retryable? rejected"
    (rejects? (lambda () (make-retry-policy #:retryable? 42)))))

;;;
;;; 2. Backoff Computation
;;;

(test-group "retry-backoff-computation"
  (test-equal "first failure backs off the initial delay"
    1.0 (retry-delay-for-attempt %jitter-free-policy 1))
  (test-equal "second failure grows by the multiplier"
    2.0 (retry-delay-for-attempt %jitter-free-policy 2))
  (test-equal "third failure grows geometrically"
    4.0 (retry-delay-for-attempt %jitter-free-policy 3))
  (test-equal "fourth failure is capped at max-delay"
    5.0 (retry-delay-for-attempt %jitter-free-policy 4))
  (test-equal "far failures stay capped"
    5.0 (retry-delay-for-attempt %jitter-free-policy 40))

  (let ((jittered (make-retry-policy #:initial-delay 1.0
                                     #:multiplier 1.0
                                     #:max-delay 100.0
                                     #:jitter 0.5)))
    (test-assert "jitter keeps the delay within [base-j, base+j]"
      (let loop ((n 0) (ok #t))
        (if (>= n 50)
            ok
            (let ((d (retry-delay-for-attempt jittered 1)))
              (loop (+ n 1) (and ok (<= 0.5 d) (<= d 1.5))))))))

  (let ((still (make-retry-policy #:initial-delay 2.0 #:multiplier 1.0
                                  #:max-delay 2.0 #:jitter 0.9)))
    (test-equal "jitter clamps at max-delay"
      2.0 (retry-delay-for-attempt still 1))))

;;;
;;; 3. Retry Driver
;;;

(test-group "retry-success-paths"
  (let* ((log (list '()))
         (policy (make-retry-policy #:max-attempts 4
                                    #:initial-delay 1.0
                                    #:jitter 0
                                    #:sleeper (recording-sleeper log))))
    (test-equal "first-try success returns the value without sleeping"
      'done
      (call-with-retry policy (lambda () 'done)))
    (test-equal "no sleep happened" '() (car log)))

  (let* ((log (list '()))
         (remaining (list 2))
         (value (call-with-retry
                    (make-retry-policy #:max-attempts 4
                                       #:initial-delay 1.0
                                       #:multiplier 2.0
                                       #:max-delay 5.0
                                       #:jitter 0
                                       #:sleeper (recording-sleeper log))
                  (counting-thunk remaining 'recovered))))
    (test-equal "retry-then-success returns the value" 'recovered value)
    (test-equal "exactly two failures occurred" 0 (car remaining))
    (test-equal "backoff schedule is 1.0 then 2.0"
      '(1.0 2.0) (reverse (car log)))))

(test-group "retry-failure-paths"
  (let* ((log (list '()))
         (policy (make-retry-policy #:max-attempts 3
                                    #:initial-delay 1.0
                                    #:jitter 0
                                    #:sleeper (recording-sleeper log))))
    (test-equal "non-retryable failure rethrows immediately with its own key"
      'misc-error
      (catch #t
        (lambda () (call-with-retry policy (lambda () (throw 'misc-error "boom"))))
        (lambda (key . args) key)))
    (test-equal "no sleep for non-retryable failure" '() (car log))

  (let* ((log (list '()))
         (policy (make-retry-policy #:max-attempts 3
                                    #:initial-delay 1.0
                                    #:jitter 0
                                    #:sleeper (recording-sleeper log))))
    (define exhaust
      (catch 'retry-exhausted
        (lambda ()
          (call-with-retry policy (lambda () (throw 'transient "always"))))
        (lambda (thrown-key thrown-attempt last-key last-args)
          (list thrown-attempt last-key last-args))))
    (test-equal "exhaustion reports the attempt budget" 3 (car exhaust))
    (test-equal "exhaustion carries the last failure key" 'transient (cadr exhaust))
    (test-equal "exhaustion carries the last irritants"
      '("always") (caddr exhaust))
    (test-equal "the full budget of sleeps happened"
      '(1.0 2.0) (reverse (car log))))

  (test-equal "budget of 1 never retries and reports exhaustion"
    'retry-exhausted
    (catch #t
      (lambda ()
        (call-with-retry (make-retry-policy #:max-attempts 1)
          (lambda () (throw 'transient "one-shot"))))
      (lambda (key . args) key)))))

;;;
;;; 4. Failure Classification
;;;

(test-group "retry-classification"
  (test-assert "'transient is retryable" (default-retryable? 'transient "x"))
  (test-assert "connection refused (errno 111) is retryable"
    (default-retryable? 'system-error "recv" "~a x" (list #f) 111))
  (test-assert "timed out (errno 110) is retryable"
    (default-retryable? 'system-error "poll" "~a" (list) 110))
  (test-assert "EAGAIN (errno 11) is retryable"
    (default-retryable? 'system-error "read" "~a" (list) 11))
  (test-assert "a non-transient errno is not retryable"
    (not (default-retryable? 'system-error "open" "~a" (list) 2)))
  (test-assert "a plain error key is not retryable"
    (not (default-retryable? 'misc-error "bad input")))
  (test-assert "a custom predicate overrides classification"
    (let* ((seen '())
           (policy (make-retry-policy
                    #:max-attempts 2
                    #:retryable?
                    (lambda (key . args)
                      (set! seen (cons key seen))
                      (eq? key 'always-retry)))))
      (and (eq? 'ok
                (call-with-retry policy
                  (let ((n 0))
                    (lambda ()
                      (set! n (+ n 1))
                      (if (< n 2) (throw 'always-retry) 'ok)))))
           (equal? '(always-retry) seen)))))

;;;
;;; 5. Observability Callback
;;;

(test-group "retry-on-retry-callback"
  (let* ((events '())
         (policy (make-retry-policy #:max-attempts 4
                                    #:initial-delay 1.0
                                    #:multiplier 1.0
                                    #:jitter 0
                                    #:sleeper (lambda args #f)
                                    #:on-retry
                                    (lambda (attempt delay key args)
                                      (set! events
                                        (cons (list attempt delay key)
                                              events))))))
    (call-with-retry policy
      (let ((n 0))
        (lambda ()
          (set! n (+ n 1))
          (if (< n 3) (throw 'transient "n") 'ok))))
    (test-equal "on-retry fired for each retry"
      '((1 1.0 transient) (2 1.0 transient))
      (reverse events))))

(spec-end)
(spec-exit)
