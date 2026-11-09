;;; badb — Anthropic Provider Integration
;;;
;;; Content and delta extraction routines for Anthropic Messages API format.

(define-module (geasa providers-model anthropic)
               #:use-module (geasa foundation-util plugin)
               #:export (extract-anthropic-delta-item
                          extract-anthropic-content))

(define (extract-anthropic-delta-item obj)
        "Extract delta item from Anthropic-format delta payload."
        (let ((delta (json-ref obj "delta")))
          (and (pair? delta)
               (cond
                 ((or (json-ref delta "thinking") (json-ref delta "thought"))
                   => (lambda (th) (cons 'thought th)))
                 ((or (json-ref delta "text") (json-ref delta "content"))
                   => (lambda (c) (cons 'content c)))
                 (else #f)))))

(define (extract-anthropic-content obj)
        "Extract content from Anthropic-format response payload."
        (let ((content (json-ref obj "content")))
          (cond
            ((string? content) content)
            ((and (vector? content) (> (vector-length content) 0))
              (let ((b0 (vector-ref content 0)))
                (and (pair? b0) (json-ref b0 "text"))))
            (else #f))))
