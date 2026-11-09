;;; badb — Pluggable Guardrails & Policy Engine
;;;
;;; Extensible security and governance system for AI tool execution.
;;; Guardrails can be raised, lowered, and determined by static pattern rules,
;;; arbitrary Scheme predicates, interactive human confirmation, or algorithmic
;;; fast "System 1" models evaluating risk in real-time.

(define-module (geasa policies-guardrail plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa providers-model openai)
               #:use-module (badb kernel journal)
               #:use-module (json)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 regex)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:export
               (<guardrail-policy> make-guardrail-policy
                                   guardrail-policy?
                                   guardrail-policy-level
                                   guardrail-policy-rules
                                   guardrail-policy-evaluator
                                   guardrail-policy-journal
                                   set-guardrail-policy-level!
                                   set-guardrail-policy-rules!
                                   set-guardrail-policy-evaluator!
                                   set-guardrail-policy-journal!
                                   ;; Pre-built evaluators & rule constructors
                                   make-pattern-rule
                                   make-scheme-predicate-evaluator
                                   make-system1-model-evaluator
                                   default-cautious-evaluator
                                   dangerous-operation?
                                   mutating-operation?
                                   guile-c-paren-imbalance
                                   scheme-paren-imbalance
                                   guardrail-command-string
                                   ;; Decision evaluation & level inspection
                                   evaluate-guardrail
                                   guardrail-level-rank
                                   guardrail-allow?
                                   guardrail-deny?
                                   guardrail-ask?))

;;;
;;; Guardrail Levels & Ranking
;;;
(define %guardrail-levels
        '((disabled   . 0)
          (permissive . 1)
          (cautious   . 2)
          (strict     . 3)
          (paranoid   . 4)))

(define (guardrail-level-rank level)
        "Return numeric security rank for LEVEL symbol (higher is stricter)."
        (or (assq-ref %guardrail-levels level) 2))

;;;
;;; Rule Constructors
;;;
(define (compile-guardrail-pattern pat)
        (cond
          ((not pat) #f)
          ((string? pat)
            (catch #t
              (lambda ()
                (make-regexp pat regexp/icase))
              (lambda _
                pat)))
          (else pat)))

(define* (make-pattern-rule name-pattern args-pattern verdict #:optional (reason #f))
         "Create a declarative rule matching tool name and argument strings.
VERDICT must be 'allow, 'deny, or 'ask.
NAME-PATTERN and ARGS-PATTERN can be regex strings, literal strings, or #f (wildcard)."
         `((name-pattern . ,(compile-guardrail-pattern name-pattern))
           (raw-name-pattern . ,name-pattern)
           (args-pattern . ,(compile-guardrail-pattern args-pattern))
           (raw-args-pattern . ,args-pattern)
           (verdict . ,verdict)
           (reason .
                   ,(or reason
                        (format #f "Matched pattern rule: ~a / ~a" name-pattern args-pattern)))))

(define (matches-pattern? pattern text)
        (cond
          ((not pattern) #t)
          ((not (string? text)) #f)
          ((regexp? pattern) (and (regexp-exec pattern text) #t))
          ((string? pattern)
            (catch #t
              (lambda ()
                (and (regexp-exec (make-regexp pattern regexp/icase) text) #t))
              (lambda _
                (and (string-contains (string-downcase text) (string-downcase pattern)) #t))))
          ((procedure? pattern) (and (pattern text) #t))
          (else #f)))

;;;
;;; Pre-built Evaluator Strategies
;;;
(define* (dangerous-operation? tool-name #:optional (args '()))
         "Return #t if TOOL-NAME and ARGS constitute a dangerous destructive operation."
         (let ((name-str (string-downcase (ensure-string tool-name))))
           (cond
             ((string-contains name-str "danger") #t)
             ;; Read-only inspection tools are not destructive commands
             ((member name-str '("read_file" "fs-read-file" "ctx_search" "find_symbol" "symbols_overview")) #f)
             (else
               (let ((args-str
                       (string-downcase
                         (if (string? args)
                           args
                           (object->string args)))))
                 (and
                   (or (string-contains args-str "rm -rf")
                       (string-contains args-str "rm  -rf")
                       (string-contains args-str "rm -fr")
                       (string-contains args-str "rm -r -f")
                       (string-contains args-str "rm -f -r")
                       (string-contains args-str "rm --recursive")
                       (string-contains args-str "rm -r --force")
                       (string-contains args-str "rm --force -r")
                       (string-contains args-str "drop table")
                       (string-contains args-str "drop database")
                       (string-contains args-str "truncate table")
                       (string-contains args-str "format disk")
                       (string-contains args-str "mkfs")
                       (string-contains args-str "> /dev/sd")
                       (string-contains args-str "dd if="))
                   #t))))))

(define* (mutating-operation? tool-name #:optional (_args '()))
         "Return #t if TOOL-NAME represents a state-mutating operation."
         (let
             ((name-str (string-downcase (ensure-string tool-name))))
           (and
             (or (and (member name-str '("bash" "sh" "zsh" "ksh" "cmd" "command")) #t)
                 (string-prefix? "write" name-str)
                 (string-prefix? "edit" name-str)
                 (string-prefix? "delete" name-str)
                 (string-prefix? "remove" name-str)
                 (string-prefix? "unlink" name-str)
                 (string-prefix? "create" name-str)
                 (string-prefix? "shell" name-str)
                 (string-prefix? "run_" name-str)
                 (string-prefix? "run-" name-str)
                 (string-prefix? "patch" name-str)
                 (string-prefix? "modify" name-str)
                 (string-contains name-str "exec")
                 (string-contains name-str "mutate")
                 (string-contains name-str "eval"))
             #t)))

;;;
;;; Quoting-Layer Sanity: guile -c Paren Balance
;;;
;;; A shell command embedding `guile ... -c <scheme>' crosses several
;;; quoting layers (model tool-call JSON, harness argument decoding, bash
;;; single-quote strings, the Guile reader).  A single byte lost or added
;;; on the way surfaces as a cryptic reader error far from the corruption
;;; site.  These predicates detect unbalanced payloads BEFORE execution so
;;; the agent receives the exact diagnosis instead of a stack trace.
;;;
(define %guile-c-regexp
        (make-regexp "\\bguile\\b[^;|&\n]*[ 	]-c\\b[ 	]*['\"]"))

(define %shell-like-tool-names
        '("bash" "sh" "zsh" "ksh" "cmd" "command"
          "execute_command" "execute-command" "shell"))

(define (%shell-like-tool? tool-name)
        (let ((name-str (string-downcase (ensure-string tool-name))))
          (or (member name-str %shell-like-tool-names)
              (string-contains name-str "shell")
              (string-contains name-str "exec")
              (string-contains name-str "command"))))

(define (guardrail-command-string args)
        "Extract the shell command string from tool ARGS: ARGS itself when it
is a string, the command/cmd field when it is an alist, else #f."
        (cond
          ((string? args) args)
          ((and (pair? args) (pair? (car args)))
            (or (assoc-ref args "command")
                (assoc-ref args 'command)
                (assoc-ref args "cmd")
                (assoc-ref args 'cmd)))
          (else #f)))

(define (%payload-after cmd start open)
        "Return the -c payload beginning at index START in CMD, delimited by
the OPEN quote character (' or \").  Single-quoted payloads end at the
next single quote (shell single quotes cannot nest); double-quoted
payloads honor backslash escapes.  #f when unterminated."
        (let ((n (string-length cmd)))
          (let loop ((i start))
                    (cond
                      ((>= i n) #f)
                      ((char=? open #\")
                        (cond ((char=? (string-ref cmd i) #\\)
                                (if (>= (+ i 1) n) #f (loop (+ i 2))))
                              ((char=? (string-ref cmd i) #\") (substring cmd start i))
                              (else (loop (1+ i)))))
                      (else (if (char=? (string-ref cmd i) #\')
                              (substring cmd start i)
                              (loop (1+ i))))))))

(define (scheme-paren-imbalance payload)
        "Return #f when PAYLOAD's parentheses balance outside strings,
line/block comments, and character literals; otherwise a string
describing the imbalance."
        (let ((n (string-length payload)))
          (let loop ((i 0) (depth 0) (state 'normal) (block 0))
                    (if (>= i n)
                      (cond
                        ((eq? state 'string) "payload ends inside a string literal")
                        ((> block 0) "payload ends inside a block comment")
                        ((> depth 0)
                          (format #f "~a unclosed opening paren(s) at end of payload" depth))
                        ((< depth 0)
                          (format #f "~a extra closing paren(s)" (- depth)))
                        (else #f))
                      (let ((c (string-ref payload i)))
                        (cond
                          ((eq? state 'line-comment)
                            (if (char=? c #\newline)
                              (loop (1+ i) depth 'normal block)
                              (loop (1+ i) depth 'line-comment block)))
                          ((eq? state 'string)
                            (cond ((char=? c #\\) (if (>= (+ i 1) n)
                                                    (loop (+ i 1) depth 'string block)
                                                    (loop (+ i 2) depth 'string block)))
                                  ((char=? c #\") (loop (1+ i) depth 'normal block))
                                  (else (loop (1+ i) depth 'string block))))
                          ((eq? state 'block-comment)
                            (cond ((and (char=? c #\|) (< (+ i 1) n)
                                        (char=? (string-ref payload (+ i 1)) #\#))
                                    (if (= block 1)
                                      (loop (+ i 2) depth 'normal 0)
                                      (loop (+ i 2) depth 'block-comment (- block 1))))
                                  ((and (char=? c #\#) (< (+ i 1) n)
                                        (char=? (string-ref payload (+ i 1)) #\|))
                                    (loop (+ i 2) depth 'block-comment (+ block 1)))
                                  (else (loop (1+ i) depth 'block-comment block))))
                          (else
                            (cond
                              ((and (char=? c #\#) (< (+ i 1) n)
                                    (char=? (string-ref payload (+ i 1)) #\\))
                                ;; #\X char literal: the character after the backslash is
                                ;; literal data (#\( and #\) must not count as parens).
                                (loop (+ i 3) depth 'normal block))
                              ((and (char=? c #\#) (< (+ i 1) n)
                                    (char=? (string-ref payload (+ i 1)) #\|))
                                (loop (+ i 2) depth 'block-comment 1))
                              ((char=? c #\") (loop (1+ i) depth 'string block))
                              ((char=? c #\;) (loop (1+ i) depth 'line-comment block))
                              ((char=? c #\() (loop (1+ i) (+ depth 1) 'normal block))
                              ((char=? c #\)) (loop (1+ i) (- depth 1) 'normal block))
                              (else (loop (1+ i) depth 'normal block))))))))))

(define (guile-c-paren-imbalance tool-name args)
        "Return a deny reason string when TOOL-NAME executes a shell command
embedding a guile -c payload with unbalanced parentheses; #f otherwise.
Only shell-like tools are checked: file-writing tools may legitimately
carry guile -c text as file content."
        (and (%shell-like-tool? tool-name)
             (let ((cmd (guardrail-command-string args)))
               (and (string? cmd)
                    (let loop ((ms (list-matches %guile-c-regexp cmd)))
                              (if (null? ms)
                                #f
                                (let* ((m (car ms))
                                       (open (string-ref cmd (- (match:end m) 1)))
                                       (payload (%payload-after cmd (match:end m) open)))
                                  (cond
                                    ((not payload)
                                      "unterminated guile -c payload quoting in command")
                                    ((scheme-paren-imbalance payload)
                                      => (lambda (desc)
                                           (format #f
                                                   "guile -c paren imbalance: ~a. The command was NOT executed; a character was likely lost or added crossing quoting layers. Re-issue the command balanced, or write the Scheme to a file and run 'guile <file>' instead of -c."
                                                   desc)))
                                    (else (loop (cdr ms)))))))))))

(define* (default-cautious-evaluator tool-name args level #:optional (context '()))
         "Default built-in evaluator applying standard cautious/permissive/strict policies.
A shell command embedding an unbalanced guile -c payload is denied
before any level-specific decision (except at 'disabled): a
syntactically broken command cannot succeed, and the denial reason
replaces the cryptic reader error the subprocess would produce."
         (if (eq? level 'disabled)
           (cons 'allow "Guardrails disabled")
           (let ((imbalance (guile-c-paren-imbalance tool-name args)))
             (if imbalance
               (cons 'deny imbalance)
               (case level
                 ((permissive)
                   (if (dangerous-operation? tool-name args)
                     (cons 'ask "High-risk command detected in permissive mode")
                     (cons 'allow "Permissive mode auto-approved")))
                 ((cautious)
                   (cond
                     ((dangerous-operation? tool-name args)
                       (cons 'deny "Dangerous destructive operation blocked in cautious mode"))
                     ((mutating-operation? tool-name args)
                       (cons 'ask "State mutation requires confirmation in cautious mode"))
                     (else (cons 'allow "Read-only inspection permitted"))))
                 ((strict)
                   (if (dangerous-operation? tool-name args)
                     (cons 'deny "Dangerous operation blocked in strict mode")
                     (cons 'ask "All operations require confirmation in strict mode")))
                 ((paranoid)
                   (if (mutating-operation? tool-name args)
                     (cons 'deny "All mutations prohibited in paranoid mode")
                     (cons 'ask "Inspection requires confirmation in paranoid mode")))
                 (else (cons 'ask "Unrecognized security level; defaulting to ask")))))))

(define
  (make-scheme-predicate-evaluator predicate-fn)
  "Create an algorithmic evaluator driven by custom Scheme PREDICATE-FN.
PREDICATE-FN receives (tool-name args level context) and returns 'allow, 'deny, or 'ask."
  (lambda*
    (tool-name args level #:optional (context '()))
    (let
        ((res (predicate-fn tool-name args level context)))
      (cond
        ((pair? res) res)
        ((eq? res #t) (cons 'allow "Approved by custom Scheme predicate"))
        ((eq? res #f) (cons 'deny "Denied by custom Scheme predicate"))
        (else (cons res (format #f "Decided ~a by Scheme predicate" res)))))))

(define*
  (make-system1-model-evaluator fast-client
                                #:key
                                (system-prompt
                                  "You are a fast System 1 safety guardrail. Evaluate the proposed tool call. Respond strictly with JSON: {\"verdict\": \"allow\"|\"ask\"|\"deny\", \"reason\": \"...\"}")
                                (fallback-evaluator default-cautious-evaluator))
  "Create a fast neuro-symbolic evaluator using a lightweight 'System 1' model FAST-CLIENT.
Falls back to FALLBACK-EVALUATOR if the model call fails."
  (lambda*
    (tool-name args level #:optional (context '()))
    (catch #t
      (lambda ()
        (let*
            ((prompt
               (format #f
                       "Tool: ~a\nArguments: ~s\nSecurity Level: ~a\nContext: ~s"
                       tool-name
                       args
                       level
                       context))
             (msgs (list (cons "system" system-prompt) (cons "user" prompt)))
             (resp (model-complete fast-client msgs #:temperature 0.0))
             (parsed (safe-json-parse resp #f))
             (verdict (and (pair? parsed) (prop-ref parsed 'verdict)))
             (reason (and (pair? parsed) (prop-ref parsed 'reason))))
          (if
              (and verdict (member (ensure-string verdict) '("allow" "ask" "deny")))
            (cons (ensure-symbol verdict) (or reason "Evaluated by System 1 model"))
            (fallback-evaluator tool-name args level context))))
      (lambda (k . a)
        (fallback-evaluator tool-name args level context)))))

;;;
;;; Guardrail Policy Record
;;;
(define-record-type <guardrail-policy>
                    (%make-guardrail-policy level rules evaluator journal)
                    guardrail-policy?
                    (level guardrail-policy-level set-guardrail-policy-level!)
                    (rules guardrail-policy-rules set-guardrail-policy-rules!)
                    (evaluator guardrail-policy-evaluator set-guardrail-policy-evaluator!)
                    (journal guardrail-policy-journal set-guardrail-policy-journal!))

(define* (make-guardrail-policy #:key
                                (level 'cautious)
                                (rules '())
                                (evaluator default-cautious-evaluator)
                                (journal #f))
         "Construct an extensible <guardrail-policy> instance."
         (%make-guardrail-policy level rules evaluator journal))

;;;
;;; Evaluation Pipeline
;;;
(define* (evaluate-guardrail policy tool-name args #:optional (context '()))
         "Evaluate TOOL-NAME and ARGS against POLICY.
Returns a pair: (verdict-symbol . reason-string), e.g. '(allow . \"...\") or '(deny . \"...\")."
         (let* ((level (guardrail-policy-level policy))
                (rules (guardrail-policy-rules policy))
                (evaluator (guardrail-policy-evaluator policy))
                (jrn (guardrail-policy-journal policy))
                (name-str (if (symbol? tool-name) (symbol->string tool-name) tool-name))
                (args-str (object->string args))

                ;; 1. Check explicit pattern rules first
                (matching-rule
                  (find (lambda (r)
                          (let ((np (assoc-ref r 'name-pattern))
                                (ap (assoc-ref r 'args-pattern)))
                            (and (matches-pattern? np name-str)
                                 (matches-pattern? ap args-str))))
                        rules))

                ;; 2. Determine verdict
                (decision
                  (if matching-rule
                    (cons (assoc-ref matching-rule 'verdict)
                          (or (assoc-ref matching-rule 'reason) "Matched explicit rule"))
                    (if (procedure? evaluator)
                      (evaluator name-str args level context)
                      (cons 'allow "No evaluator configured"))))

                (verdict (car decision))
                (reason (cdr decision)))

           ;; 3. Log provenance to journal if attached
           (safe-journal-append! jrn
                                 `((feature-id . "guardrail")
                                   (checkpoint . ,(format #f "guardrail:~a" name-str))
                                   (tool-name . ,name-str)
                                   (level . ,(symbol->string level))
                                   (verdict . ,(symbol->string verdict))
                                   (reason . ,reason)))

           decision))

(define (guardrail-allow? decision)
        (eq? (car decision) 'allow))

(define (guardrail-deny? decision)
        (eq? (car decision) 'deny))

(define (guardrail-ask? decision)
        (eq? (car decision) 'ask))
