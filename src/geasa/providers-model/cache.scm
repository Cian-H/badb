;;; badb — Ephemeral Prompt Cache Markers
;;;
;;; Anthropic-style cache_control markers: annotate the stable leading
;;; system-message prefix of a chat message list with an ephemeral
;;; cache breakpoint so providers with prompt caching (Anthropic,
;;; OpenRouter, and compatible gateways) reuse recompute-free KV
;;; prefixes across ReAct iterations.  Markers are opt-in per client
;;; (<model-client> #:cache-markers? #t); strict OpenAI-compatible
;;; servers that reject unknown fields simply never receive them.

(define-module (geasa providers-model cache)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (ice-9 optargs)
               #:export
               (ephemeral-cache-control apply-cache-breakpoints strip-cache-markers))

(define (ephemeral-cache-control)
        "Return a fresh Anthropic-style ephemeral cache_control value
((\"type\" . \"ephemeral\"))."
        (list (cons "type" "ephemeral")))

(define (message-role-str msg)
        (and (pair? msg)
             (or (prop-ref msg 'role) (prop-ref msg "role"))
             (ensure-string (or (prop-ref msg 'role) (prop-ref msg "role")))))

(define (system-message? msg)
        (and (message-role-str msg)
             (member (message-role-str msg) '("system" "developer"))
             #t))

(define (system-prefix-length msgs)
        "Number of leading system/developer messages in MSGS."
        (let loop ((rest msgs) (n 0))
                  (cond
                    ((null? rest) n)
                    ((system-message? (car rest)) (loop (cdr rest) (+ n 1)))
                    (else n))))

(define (text-block content) (list (cons "type" "text") (cons "text" content)))

(define (block-alist? x)
        (and (pair? x) (assoc-ref (ensure-list x) "type") #t))

(define (content->blocks content)
        "Coerce message CONTENT into a list of text block alists.
A plain string becomes a single block; a vector or list of block
alists passes through unchanged; anything else becomes one block."
        (cond
          ((string? content) (list (text-block content)))
          ((vector? content) (vector->list content))
          ((and (pair? content) (block-alist? (car content))) content)
          (else (list (text-block (format #f "~a" content))))))

(define (blocks->content blocks)
        "Collapse a list of block alists back into the plain string
formed by their text fields."
        (string-join
          (filter-map
            (lambda (b)
              (let ((t (or (prop-ref b 'text) (prop-ref b "text"))))
                (and (string? t) t)))
            blocks)
          ""))

(define (cache-marked-content content)
        "Wrap CONTENT into text blocks and attach an ephemeral
cache_control marker to the final block (the cache breakpoint)."
        (let* ((blocks (content->blocks content))
               (n (length blocks)))
          (list->vector
            (let loop ((rest blocks) (i 1) (out '()))
                      (if (null? rest)
                        (reverse out)
                        (let ((b (car rest)))
                          (loop (cdr rest)
                                (+ i 1)
                                (cons (if (= i n)
                                        (append (ensure-list b)
                                                (list (cons "cache_control"
                                                            (ephemeral-cache-control))))
                                        b)
                                      out))))))))

(define (cache-blocks->content content)
        "Collapse block CONTENT after dropping any cache_control marks."
        (define (drop-cache-control b)
                (filter
                  (lambda (pair)
                    (not (and (pair? pair) (string=? (ensure-string (car pair)) "cache_control"))))
                  (ensure-list b)))
        (cond
          ((vector? content)
            (blocks->content
              (map
                (lambda (b)
                  (if (pair? b)
                    (drop-cache-control b)
                    b))
                (vector->list content))))
          ((and (pair? content) (block-alist? (car content)))
            (blocks->content
              (map
                (lambda (b)
                  (if (pair? b)
                    (drop-cache-control b)
                    b))
                content)))
          (else content)))

(define (message-alist? x)
        (and (pair? x) (pair? (car x)) (or (prop-ref x 'role) (prop-ref x "role"))))

(define (message-alist-forms messages)
        "Normalize MESSAGES (vector, list, or single message alist)
to a list of message alists."
        (cond
          ((vector? messages) (vector->list messages))
          ((and (pair? messages) (message-alist? messages)) (list messages))
          ((list? messages) (filter pair? messages))
          (else '())))

(define*
  (map-last-system-content messages transformer #:optional (target-index #f))
  "Return MESSAGES with TRANSFORMER applied to the content of one
message in its leading system/developer run: the message at
TARGET-INDEX (clamped to the run length) when given, otherwise the
last message of the run.  Original shape is preserved (vector or
list; a bare alist yields a vector, matching normalize-messages).
Returns #f when MESSAGES has no leading system message."
  (let ((msgs (message-alist-forms messages)))
    (let*
        ((n (system-prefix-length msgs)) (idx (min (or target-index (- n 1)) (- n 1))))
      (and (> n 0)
           (let
               ((marked
                  (let loop
                    ((rest msgs) (i 0) (out '()))
                    (if (null? rest)
                      (reverse out)
                      (let ((m (car rest)))
                        (loop (cdr rest) (+ i 1)
                              (cons
                                (if (= i idx)
                                  (let
                                      ((content (or (prop-ref m 'content) (prop-ref m "content") "")))
                                    (alist-merge (ensure-list m) (list (cons "content" (transformer content)))))
                                  m)
                                out)))))))
             (if (vector? messages)
               (list->vector marked)
               (if
                   (and (pair? messages) (message-alist? messages))
                 (list->vector marked)
                 marked)))))))

(define*
  (apply-cache-breakpoints messages #:optional (prefix-count #f))
  "Annotate MESSAGES with an explicit ephemeral cache breakpoint:
the content of one message in the leading system/developer run is
wrapped into text blocks carrying a cache_control marker on the final
block.  Without PREFIX-COUNT the last message of the run is marked;
with PREFIX-COUNT the message at index (- PREFIX-COUNT 1) is marked
instead (clamped to the run length) — use it to break between a static
cached system prompt and later volatile system sections.  Messages
shape: a message vector, a list, or a single message alist (the alist
form returns a vector, matching normalize-messages).  MESSAGES itself
is not mutated."
  (let
      ((out
         (map-last-system-content messages
                                  (lambda (content)
                                    (cache-marked-content content))
                                  (and prefix-count (- prefix-count 1)))))
    (if out
      out
      (cond
        ((vector? messages) messages)
        ((and (pair? messages) (message-alist? messages)) (vector messages))
        ((list? messages) (list->vector messages))
        (else messages)))))

(define (strip-cache-markers messages)
        "Remove every cache_control marker from MESSAGES and collapse
block-array content back into plain text strings.  Inverse of
apply-cache-breakpoints on the stable prefix; messages already in
plain string form pass through unchanged."
        (let ((strip-one
                (lambda (m)
                  (if (and (pair? m) (pair? (car m)))
                    (let ((content (or (prop-ref m 'content) (prop-ref m "content") #f)))
                      (if content
                        (alist-merge (ensure-list m)
                                     (list (cons "content" (cache-blocks->content content))))
                        m))
                    m))))
          (cond
            ((vector? messages)
              (list->vector (map strip-one (vector->list messages))))
            ((list? messages) (map strip-one messages))
            (else messages))))
