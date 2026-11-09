;;; badb — Error Recovery & Retry Engine
;;;
;;; Provides a declarative retry policy (attempt budget, exponential
;;; backoff with jitter, injectable sleeper and retryability predicate)
;;; and CALL-WITH-RETRY, which runs a thunk under that policy.  Failures
;;; classified as non-retryable rethrow immediately; retryable failures
;;; back off and retry until the attempt budget is exhausted, at which
;;; point a 'retry-exhausted error is thrown carrying the attempt
;;; count and the last underlying failure.

(define-module (geasa runtimes-retry plugin)
               #:use-module (ice-9 format)
               #:use-module (ice-9 optargs)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (<retry-policy>
                          make-retry-policy
                          retry-policy?
                          retry-policy-max-attempts
                          retry-policy-initial-delay
                          retry-policy-multiplier
                          retry-policy-max-delay
                          retry-policy-jitter
                          retry-policy-sleeper
                          retry-policy-retryable?
                          retry-policy-on-retry
                          retry-delay-for-attempt
                          call-with-retry
                          default-retryable?))

;;;
;;; Policy record
;;;

(define-record-type <retry-policy>
                    (%make-retry-policy max-attempts initial-delay multiplier max-delay
                                        jitter sleeper retryable? on-retry)
                    retry-policy?
                    (max-attempts retry-policy-max-attempts)
                    (initial-delay retry-policy-initial-delay)
                    (multiplier retry-policy-multiplier)
                    (max-delay retry-policy-max-delay)
                    (jitter retry-policy-jitter)
                    (sleeper retry-policy-sleeper)
                    (retryable? retry-policy-retryable?)
                    (on-retry retry-policy-on-retry))

(define (default-sleeper seconds)
        "Sleep SECONDS (a real, may be fractional) via usleep."
        (let ((us (max 0 (inexact->exact (round (* 1000000 seconds))))))
          (if (> us 0) (usleep us) #f)))

(define* (make-retry-policy #:key
                            (max-attempts 3)
                            (initial-delay 0.25)
                            (multiplier 2.0)
                            (max-delay 30.0)
                            (jitter 0.1)
                            (sleeper default-sleeper)
                            (retryable? default-retryable?)
                            (on-retry #f))
         "Build a <retry-policy>.  MAX-ATTEMPTS is the total attempt budget
(at least 1).  The delay before retry N is INITIAL-DELAY * MULTIPLIER^(N-1)
capped at MAX-DELAY, then jittered by +/- JITTER of the capped value.
SLEEPER suspends between attempts (injectable for tests); RETRYABLE?
classifies a caught failure; ON-RETRY, when set, is invoked as
(ON-RETRY ATTEMPT DELAY KEY ARGS) before each sleep for observability."
         (define (fail what)
                 (error (format #f "make-retry-policy: ~a" what)))
         (unless (and (integer? max-attempts) (>= max-attempts 1))
           (fail "max-attempts must be an integer >= 1"))
         (unless (and (real? initial-delay) (>= initial-delay 0))
           (fail "initial-delay must be a real >= 0"))
         (unless (and (real? multiplier) (>= multiplier 1))
           (fail "multiplier must be a real >= 1"))
         (unless (and (real? max-delay) (>= max-delay initial-delay))
           (fail "max-delay must be a real >= initial-delay"))
         (unless (and (real? jitter) (>= jitter 0) (<= jitter 1))
           (fail "jitter must be a real within [0, 1]"))
         (unless (procedure? sleeper) (fail "sleeper must be a procedure"))
         (unless (procedure? retryable?) (fail "retryable? must be a procedure"))
         (unless (or (not on-retry) (procedure? on-retry))
           (fail "on-retry must be a procedure or #f"))
         (%make-retry-policy max-attempts initial-delay multiplier max-delay
                             jitter sleeper retryable? on-retry))

;;;
;;; Failure classification
;;;

;; Errnos treated as transient infrastructure failures by
;; DEFAULT-RETRYABLE?: EAGAIN, EPIPE, ENETUNREACH, ECONNRESET,
;; ETIMEDOUT, ECONNREFUSED, EHOSTUNREACH.
(define %transient-errnos '(11 32 101 104 110 111 113))

(define* (default-retryable? key . args)
         "Classify a caught failure: #t for the synthetic 'transient key and
for system errors carrying a transient errno, #f otherwise."
         (cond
           ((eq? key 'transient) #t)
           ((eq? key 'system-error)
             (any (lambda (arg) (and (integer? arg) (memv arg %transient-errnos)))
                  args))
           (else #f)))

;;;
;;; Backoff computation
;;;

(define (retry-delay-for-attempt policy attempt)
        "Delay (seconds) before the retry following failure number ATTEMPT:
exponential growth from INITIAL-DELAY capped at MAX-DELAY, jittered by
+/- JITTER of the capped base and clamped at MAX-DELAY."
        (let* ((initial (retry-policy-initial-delay policy))
               (multiplier (retry-policy-multiplier policy))
               (cap (retry-policy-max-delay policy))
               (jitter (retry-policy-jitter policy))
               (base (min cap (* initial (expt multiplier (- attempt 1)))))
               (spread (* base jitter (- (* 2 (random 1.0)) 1))))
          (min cap (max 0 (+ base spread)))))

;;;
;;; Retry driver
;;;

(define* (call-with-retry policy thunk)
         "Apply THUNK under POLICY and return its value.  A failure classified
non-retryable by POLICY rethrows immediately.  A retryable failure
sleeps for the backoff delay and retries; once the attempt budget is
exhausted, throws ('retry-exhausted ATTEMPT-COUNT KEY ARGS) where ARGS
is the irritant list of the last underlying failure."
         (let ((budget (retry-policy-max-attempts policy)))
           (let loop ((attempt 1))
                     (catch #t
                       thunk
                       (lambda (key . args)
                         (cond
                           ((not (apply (retry-policy-retryable? policy) key args))
                             (apply throw key args))
                           ((>= attempt budget)
                             (throw 'retry-exhausted attempt key args))
                           (else
                             (let* ((delay (retry-delay-for-attempt policy attempt))
                                    (on-retry (retry-policy-on-retry policy)))
                               (if on-retry (on-retry attempt delay key args))
                               ((retry-policy-sleeper policy) delay)
                               (loop (+ attempt 1))))))))))
