;;; badb — Model Providers Common Types & Utilities
;;;
;;; Implements <model-client> record, message normalization/validation,
;;; and Server-Sent Events (SSE) stream parsing.

(define-module (geasa providers-model common)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (json)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:use-module
               (srfi srfi-158)
               #:export
               (current-send-reasoning? <model-client>
                                        make-model-client
                                        model-client?
                                        model-client-endpoint
                                        model-client-api-key
                                        model-client-model
                                        model-client-allow-missing-thinking?
                                        model-client-cache-markers?
                                        valid-api-message?
                                        validate-conversation-messages
                                        repair-conversation-messages
                                        normalize-messages
                                        parse-sse-line
                                        sse-event-generator
                                        read-sse-events))

;;;
;;; Client Record Type
;;;
(define-record-type <model-client>
                    (%make-model-client endpoint
                                        api-key
                                        model
                                        allow-missing-thinking?
                                        cache-markers?)
                    model-client?
                    (endpoint model-client-endpoint)
                    (api-key model-client-api-key)
                    (model model-client-model)
                    (allow-missing-thinking? model-client-allow-missing-thinking?)
                    (cache-markers? model-client-cache-markers?))

(define (make-model-client . args)
        "Create a model client record.
Can be invoked with keyword arguments:
  #:endpoint - HTTP(S) endpoint URL (string)
  #:api-key  - Optional API key string (or #f)
  #:model    - Optional model name string (or #f)
  #:allow-missing-thinking? - Send the Qwen3-style chat-template
    flag allow_missing_thinking (default #t).  Disable it for strict
    OpenAI-compatible APIs (e.g. api.mistral.ai) that reject extra
    body fields.
  #:cache-markers? - Annotate the stable leading system-message prefix
    with an explicit ephemeral cache_control breakpoint (default #f).
    Enable it only for providers with explicit prompt caching
    (Anthropic and compatible gateways).
or with positional arguments:
  (make-model-client endpoint [api-key [model]])"
        (let loop
          ((rest args)
           (endpoint "http://localhost:11434/v1/chat/completions")
           (api-key #f)
           (model "default")
           (amt? #t)
           (cache? #f))
          (cond
            ((null? rest) (%make-model-client endpoint api-key model amt? cache?))
            ((keyword? (car rest))
              (let
                  ((k (car rest))
                   (v
                     (if (pair? (cdr rest))
                       (cadr rest)
                       #f))
                   (tail
                     (if (pair? (cdr rest))
                       (cddr rest)
                       '())))
                (case k
                  ((#:endpoint) (loop tail v api-key model amt? cache?))
                  ((#:api-key) (loop tail endpoint v model amt? cache?))
                  ((#:model) (loop tail endpoint api-key v amt? cache?))
                  ((#:allow-missing-thinking?)
                    (loop tail endpoint api-key model (and v #t) cache?))
                  ((#:cache-markers?) (loop tail endpoint api-key model amt? (and v #t)))
                  (else (loop tail endpoint api-key model amt? cache?)))))
            (else
              (let
                  ((ep (car args))
                   (ak
                     (if (pair? (cdr args))
                       (cadr args)
                       #f))
                   (md
                     (if (and (pair? (cdr args)) (pair? (cddr args)))
                       (caddr args)
                       "default")))
                (%make-model-client ep ak md #t #f))))))

;;;
;;; Message Validation & Normalization
;;;
(define current-send-reasoning?
        ;; Whether outgoing messages may carry non-protocol fields (e.g. the
        ;; reasoning_content trace).  Local reasoning-model servers want them;
        ;; strict OpenAI-compatible APIs (api.mistral.ai) reject unknown fields.
        ;; model-complete/model-stream bind this to the client's
        ;; allow-missing-thinking? flag: the strict-OpenAI profile.
        (make-parameter #t))

(define (message-alist? x) (and (pair? x) (pair? (car x))))
(define (role-content-pair? x) (and (pair? x) (not (pair? (car x)))))
(define (single-message-alist? x)
        (and (message-alist? x) (or (prop-ref x "role") (prop-ref x "content"))))

(define (valid-api-message? msg)
        "Predicate to check if MSG is a valid OpenAI/Anthropic chat protocol message.
Rejects internal UI logs or tool responses missing required tool_call_id."
        (cond
          ((role-content-pair? msg)
            (let ((r (ensure-string (car msg))))
              (and (member r '("system" "user" "assistant")) #t)))
          ((message-alist? msg)
            (let* ((role (prop-ref msg 'role))
                   (role-str (if role (ensure-string role) "")))
              (cond
                ((member role-str '("system" "user" "assistant")) #t)
                ((string=? role-str "tool") (non-empty-string? (prop-ref msg 'tool_call_id)))
                (else #f))))
          ((string? msg) #t)
          (else #f)))

(define (extract-message-tool-call-ids msg)
        "Return list of (id . name) pairs from MSG's tool_calls field."
        (let
            ((raw (or (prop-ref msg 'tool_calls) (prop-ref msg "tool_calls"))))
          (let
              ((items
                 (cond
                   ((vector? raw) (vector->list raw))
                   ((list? raw) raw)
                   (else '()))))
            (filter-map
              (lambda (tc)
                (and (pair? tc)
                     (let*
                         ((id (or (prop-ref tc 'id) (prop-ref tc "id")))
                          (fn (or (prop-ref tc 'function) (prop-ref tc "function")))
                          (name
                            (or
                              (and (pair? fn) (or (prop-ref fn 'name) (prop-ref fn "name")))
                              (prop-ref tc 'name)
                              (prop-ref tc "name")
                              "")))
                       (and (string? id) (not (string-null? id)) (cons id (ensure-string name))))))
              items))))

(define (coerce-message-list msgs)
        "Coerce MSGS (vector, list, single message alist, or string) into a list
of message alists for validation or repair."
        (cond
          ((vector? msgs) (vector->list msgs))
          ((single-message-alist? msgs) (list msgs))
          ((list? msgs) msgs)
          ((string? msgs) (list (list (cons "role" "user") (cons "content" msgs))))
          (else '())))

(define (validate-conversation-messages msgs)
        "Validate that MSGS forms a structurally valid conversation sequence for
chat completion endpoints. Checks that at least one non-system turn is present,
every assistant tool_calls turn is immediately answered by matching tool
responses, no orphaned tool messages appear, and the final message is not an
assistant/model turn. Returns (values valid? reason-or-#f)."
        (let*
            ((raw-list (coerce-message-list msgs))
             (msg-list
               (filter
                 (lambda (m)
                   (or (valid-api-message? m)
                       (and (message-alist? m)
                            (equal? (and=> (prop-ref m 'role) ensure-string) "tool"))))
                 raw-list)))
          (let loop
            ((rest msg-list) (pending '()) (non-sys 0) (last-role #f))
            (if (null? rest)
              (cond
                ((pair? pending) (values #f "conversation has unanswered tool_calls at end"))
                ((= non-sys 0) (values #f "conversation has no user or tool turns"))
                ((member last-role '("assistant" "model"))
                  (values #f "conversation ends with an assistant/model turn"))
                (else (values #t #f)))
              (let*
                  ((m (car rest))
                   (role
                     (cond
                       ((role-content-pair? m) (ensure-string (car m)))
                       ((message-alist? m) (or (and=> (prop-ref m 'role) ensure-string) ""))
                       ((string? m) "user")
                       (else "")))
                   (next-non-sys
                     (if (member role '("system" "developer"))
                       non-sys
                       (+ non-sys 1))))
                (cond
                  ((string=? role "tool")
                    (let
                        ((tid (and (message-alist? m) (prop-ref m 'tool_call_id))))
                      (cond
                        ((not (non-empty-string? tid))
                          (values #f "tool message missing required tool_call_id"))
                        ((null? pending)
                          (values #f (format #f "orphaned tool message for tool_call_id '~a'" tid)))
                        ((not (assoc tid pending))
                          (values #f
                                  (format #f "unexpected tool_call_id '~a' not matching pending tool_calls" tid)))
                        (else
                          (let
                              ((remaining (alist-delete tid pending string=?)))
                            (loop (cdr rest) remaining next-non-sys role))))))
                  ((pair? pending)
                    (values #f
                            (format #f
                                    "unanswered tool_calls (~a) before '~a' turn"
                                    (string-join (map car pending) ", ")
                                    role)))
                  ((string=? role "assistant")
                    (let
                        ((tc-ids
                           (if (message-alist? m)
                             (extract-message-tool-call-ids m)
                             '())))
                      (loop (cdr rest) tc-ids next-non-sys role)))
                  (else (loop (cdr rest) '() next-non-sys role))))))))

(define (repair-conversation-messages msgs)
        "Repair assistant/tool_calls/tool sequencing in MSGS: drops orphaned tool
messages and appends synthetic aborted tool observations for any unanswered
tool_calls before the next non-tool turn or end of list."
        (define (synthetic-abort-msg id-name)
                `((role . "tool") (tool_call_id . ,(car id-name))
                  (name . ,(cdr id-name))
                  (content . "Tool execution aborted: missing tool observation.")))
        (let loop
          ((rest (coerce-message-list msgs)) (pending '()) (acc '()))
          (if (null? rest)
            (append (reverse acc) (map synthetic-abort-msg pending))
            (let*
                ((m (car rest))
                 (role
                   (cond
                     ((role-content-pair? m) (ensure-string (car m)))
                     ((message-alist? m) (or (and=> (prop-ref m 'role) ensure-string) ""))
                     ((string? m) "user")
                     (else ""))))
              (cond
                ((string=? role "tool")
                  (let
                      ((tid (and (message-alist? m) (prop-ref m 'tool_call_id))))
                    (if
                        (and (non-empty-string? tid) (pair? pending) (assoc tid pending))
                      (loop (cdr rest) (alist-delete tid pending string=?) (cons m acc))
                      ;; Drop orphaned or duplicate tool message
                      (loop (cdr rest) pending acc))))
                ((pair? pending)
                  (let
                      ((aborts (reverse (map synthetic-abort-msg pending))))
                    (loop rest '() (append aborts acc))))
                ((string=? role "assistant")
                  (let
                      ((tc-ids
                         (if (message-alist? m)
                           (extract-message-tool-call-ids m)
                           '())))
                    (loop (cdr rest) tc-ids (cons m acc))))
                (else (loop (cdr rest) '() (cons m acc))))))))

(define (normalize-message msg)
        (cond
          ((message-alist? msg)
            (let*
                ((stringified
                   (map
                     (lambda (pair)
                       (cons (ensure-string (car pair)) (cdr pair)))
                     msg))
                 (role (assoc-ref stringified "role"))
                 (tc (assoc-ref stringified "tool_calls"))
                 (rc (assoc-ref stringified "reasoning_content")))
              (cond
                ((not (current-send-reasoning?))
                  ;; Strict OpenAI-compatible profile: only protocol fields go on
                  ;; the wire; reasoning traces stay internal to the harness.
                  (filter
                    (lambda (p)
                      (member (car p) (quote ("role" "content" "tool_calls" "tool_call_id"))))
                    stringified))
                ((and role
                      (string=? role "assistant")
                      tc
                      (or (not rc) (and (string? rc) (string-null? (string-trim-both rc)))))
                  (cons (cons "reasoning_content" "Thinking...")
                        (filter
                          (lambda (p)
                            (not (string=? (car p) "reasoning_content")))
                          stringified)))
                (else stringified))))
          ((role-content-pair? msg)
            (list (cons "role" (ensure-string (car msg))) (cons "content" (cdr msg))))
          ((string? msg) (list (cons "role" "user") (cons "content" msg)))
          (else msg)))

(define (normalize-messages msgs)
        (cond
          ((vector? msgs)
            (list->vector (map normalize-message (filter valid-api-message? (vector->list msgs)))))
          ((single-message-alist? msgs)
            (if (valid-api-message? msgs)
              (vector (normalize-message msgs))
              (vector)))
          ((list? msgs)
            (list->vector (map normalize-message (filter valid-api-message? msgs))))
          ((string? msgs)
            (vector (list (cons "role" "user") (cons "content" msgs))))
          (else msgs)))

;;;
;;; Server-Sent Events (SSE) Parsing
;;;
(define (clean-sse-line line)
        (if (string? line)
          (string-trim-right line (lambda (c) (or (char=? c #\return) (char=? c #\newline))))
          ""))

(define (parse-sse-line line)
        "Parse a single SSE line.
Returns:
  - 'done if the line indicates stream completion ([DONE])
  - string payload if the line is a data event (\"data: ...\")
  - #f for empty lines, comments (\": ...\"), or non-data lines."
        (let ((cleaned (clean-sse-line line)))
          (cond
            ;; Empty line
            ((string-null? cleaned) #f)
            ;; Comment line (starts with colon per SSE spec)
            ((string-prefix? ":" cleaned) #f)
            ;; Detect bare [DONE]
            ((string=? cleaned "[DONE]") 'done)
            ;; Data lines
            ((string-prefix? "data:" cleaned)
              (let* ((rest (substring cleaned 5))
                     (payload (if (and (> (string-length rest) 0)
                                       (char=? (string-ref rest 0) #\space))
                                (substring rest 1)
                                rest)))
                (if (string=? payload "[DONE]")
                  'done
                  payload)))
            ;; Other SSE fields (event, id, retry)
            (else #f))))

(define (sse-event-generator port)
        "Return a lazy (srfi-158) generator that yields parsed SSE events
from PORT.  Each yield is one of:
  (data . OBJ-OR-STRING)  a 'data: ' SSE event payload
  (body . ALIST)          a non-SSE complete JSON completion response
  done                      '[DONE]' was seen
  eof                       the port reached end of file"
        (make-coroutine-generator
          (lambda (yield)
            (let loop ()
                      (let ((line (read-line port)))
                        (cond
                          ((eof-object? line) (yield 'eof))
                          (else
                            (let* ((cleaned (clean-sse-line line))
                                   (parsed (parse-sse-line line)))
                              (cond
                                ((eq? parsed 'done) (yield 'done))
                                ((string? parsed)
                                  (let ((data (catch #t
                                                (lambda () (json-string->scm parsed))
                                                (lambda _ parsed))))
                                    (yield (cons 'data data))
                                    (loop)))
                                ((and (string-prefix? "{" cleaned)
                                      (or (string-contains cleaned "\"error\"")
                                          (string-contains cleaned "'error'")))
                                  (let ((err-obj (catch #t
                                                   (lambda () (json-string->scm cleaned))
                                                   (lambda _ #f))))
                                    (error "SSE stream received error response"
                                           (or (and (pair? err-obj)
                                                    (let ((err-inner (assoc-ref err-obj "error")))
                                                      (and (pair? err-inner) (assoc-ref err-inner "message"))))
                                               cleaned))))
                                ;; Non-SSE complete JSON response: some endpoints ignore
                                ;; stream:true and answer with a normal completion body.
                                ;; Yield it as a terminal event.
                                ((and (string-prefix? "{" cleaned)
                                      (catch #t
                                        (lambda () (json-string->scm cleaned))
                                        (lambda _ #f)))
                                  => (lambda (obj)
                                       (if (and (pair? obj) (json-ref obj "choices"))
                                         (yield (cons 'body obj))
                                         (loop))))
                                (else (loop)))))))))))

(define (read-sse-events port callback)
        "Read lines from PORT, parse SSE 'data: ' events, and invoke
(CALLBACK data-json-or-string) for each event until EOF or '[DONE]'.
Returns 'done when [DONE] is encountered, 'eof upon reaching EOF, or the
parsed alist when the body is a non-SSE complete JSON response (an
endpoint that ignored stream:true)."
        (let ((gen (sse-event-generator port)))
          (let loop ()
                    (let ((event (gen)))
                      (cond
                        ((eq? event 'eof) 'eof)
                        ((eq? event 'done) 'done)
                        ((and (pair? event) (eq? (car event) 'data))
                          (let ((res (callback (cdr event))))
                            (if (eq? res 'stop)
                              'done
                              (loop))))
                        ((and (pair? event) (eq? (car event) 'body))
                          (cdr event))
                        (else (loop)))))))
