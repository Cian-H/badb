;;; badb — Shared Utility Library
;;;
;;; Reusable primitives and cross-cutting helpers for badb Scheme modules.
;;; Centralizes filesystem creation, coercion, property list & alist lookup,
;;; safe JSON serialization, and environment variable formatting to keep
;;; subsystems DRY and maintainable.

(define-module (geasa foundation-util plugin) #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 ftw)
               #:use-module
               (json)
               #:use-module
               (fslib)
               #:use-module
               (srfi srfi-1)
               #:export
               (mkdir-p ensure-parent-directory!
                        file->string
                        string->file
                        valid-dir-entry?
                        scandir-valid-entries
                        exec-shell-command
                        active-subprocess-pid
                        set-active-subprocess-pid!
                        kill-active-subprocess!
                        ensure-string
                        ensure-symbol
                        ensure-keyword
                        ensure-list
                        non-empty-string?
                        normalize-role
                        message-role
                        message-content
                        message-thoughts
                        get-prop
                        prop-ref
                        json-ref
                        prop-set
                        alist-remove
                        alist-merge
                        safe-json-string
                        safe-json-parse
                        parse-tool-args
                        let-props
                        with-tool-args
                        %with-tool-args-bindings
                        with-default
                        ignore-errors
                        receive
                        let-values*
                        with-directory
                        with-env
                        string-case
                        sanitize-for-json
                        format-exception
                        format-token-kilo
                        split-words
                        truncate-string
                        truncate-display
                        normalize-env-alist
                        env-alist->exec-strings))

;;;
;;; Filesystem Helpers
;;;
(define (canonicalize-fsing path)
        "Return PATH with any `.' and `..' components collapsed, using
guile-fslib. Falls back to PATH unchanged when it cannot be
canonicalized (e.g. when `..' escapes above the root)."
        (catch #t
          (lambda ()
            (if (absolute-fsing? path)
              (absolute-fsing path #:canonicalize #t)
              (absolute-fsing path
                              #:working-directory (getcwd)
                              #:canonicalize #t)))
          (lambda _ path)))

(define (mkdir-p dir)
        "Recursively ensure that directory DIR exists.  The path is
canonicalized with guile-fslib so that `.' and `..' components are
resolved before anything is created on disk."
        (unless
            (or (not (string? dir))
                (string-null? dir)
                (string=? dir ".")
                (string=? dir "/")
                (file-exists? dir))
          (let*
              ((canon-dir (canonicalize-fsing dir)) (parent (dirname canon-dir)))
            (unless
                (or (string=? parent canon-dir) (string-null? parent) (string=? parent "."))
              (mkdir-p parent))
            (catch 'system-error
              (lambda ()
                (mkdir canon-dir))
              (lambda args
                (unless (file-exists? canon-dir) (apply throw args)))))))

(define (ensure-parent-directory! file-path)
        "Ensure that the enclosing parent directory of FILE-PATH exists."
        (when (string? file-path)
          (let ((dir (dirname file-path)))
            (when (and dir (not (string-null? dir)) (not (string=? dir ".")))
              (mkdir-p dir)))))

(define (valid-dir-entry? name)
        "Return #t if NAME is not '.' or '..'."
        (and (string? name) (not (member name '("." "..")))))

(define* (scandir-valid-entries dir #:optional (extra-filter #f))
         "Return list of non-dot directory entries in DIR, optionally filtered by EXTRA-FILTER.
Returns '() if DIR does not exist or cannot be read."
         (or (scandir dir (if extra-filter
                            (lambda (n) (and (valid-dir-entry? n) (extra-filter n)))
                            valid-dir-entry?))
             '()))

(define (file->string path)
        "Read entire contents of PATH as a UTF-8 string, or #f if unreadable."
        (catch #t
          (lambda ()
            (call-with-input-file path
                                  (lambda (p)
                                    (set-port-encoding! p "UTF-8")
                                    (read-string p))))
          (lambda _ #f)))

(define* (string->file path content #:key (ensure-dir? #t))
         "Write CONTENT to PATH in UTF-8, creating parent directories when ENSURE-DIR? is true."
         (when ensure-dir? (ensure-parent-directory! path))
         (call-with-output-file path
                                (lambda (p)
                                  (set-port-encoding! p "UTF-8")
                                  (display (or content "") p))))

;;;
;;; Coercion & Identifier Normalization
;;;
(define (ensure-string val)
        "Convert VAL to string representation."
        (cond
          ((string? val) val)
          ((symbol? val) (symbol->string val))
          ((keyword? val) (symbol->string (keyword->symbol val)))
          (else (object->string val))))

(define (ensure-symbol val)
        "Convert VAL to symbol representation."
        (cond
          ((symbol? val) val)
          ((string? val) (string->symbol val))
          ((keyword? val) (keyword->symbol val))
          (else (string->symbol (object->string val)))))

(define (ensure-keyword val)
        "Convert VAL to Guile keyword representation."
        (cond
          ((keyword? val) val)
          ((symbol? val) (symbol->keyword val))
          ((string? val) (symbol->keyword (string->symbol val)))
          (else (symbol->keyword (string->symbol (object->string val))))))

(define (ensure-list val)
        "Convert VAL to list representation, turning vectors into lists, #f or nil into '(), or wrapping single values."
        (cond
          ((or (not val) (null? val)) '())
          ((list? val) val)
          ((vector? val) (vector->list val))
          (else (list val))))

(define (non-empty-string? val)
        "Return #t if VAL is a string that is non-empty after trimming whitespace."
        (and (string? val) (not (string-null? (string-trim-both val)))))

(define (normalize-role role)
        "Normalize message ROLE into canonical string ('system', 'user', 'assistant', 'tool')."
        (cond
          ((or (eq? role 'user) (equal? role "user")) "user")
          ((or (eq? role 'assistant) (equal? role "assistant")) "assistant")
          ((or (eq? role 'system) (equal? role "system")) "system")
          ((or (eq? role 'tool) (equal? role "tool")) "tool")
          (else
            (let ((s (string-downcase (ensure-string role))))
              (if (string-prefix? ":" s)
                (substring s 1)
                s)))))

(define (message-role msg)
        "Extract normalized role string from MSG ('system', 'user', 'assistant', 'tool')."
        (normalize-role (prop-ref msg 'role "user")))

(define (message-content msg)
        "Extract content field from MSG alist as a string, or empty string if absent."
        (ensure-string (or (prop-ref msg 'content) "")))

(define (message-thoughts msg)
        "Extract thinking or reasoning content from MSG, returning #f if absent."
        (let ((t (or (prop-ref msg 'reasoning_content)
                     (prop-ref msg 'thoughts))))
          (and t (non-empty-string? (ensure-string t)) (ensure-string t))))

;;;
;;; Property & Alist Accessors
;;;
(define (get-prop lst key default)
        "Safely extract KEY from property list or alist LST, supporting both
(key . val), (key val), and #:key val syntax."
        (let* ((sym (ensure-symbol key))
               (str (symbol->string sym))
               (kw (symbol->keyword sym)))
          (let loop ((items lst))
                    (cond
                      ((null? items) default)
                      ((not (pair? items)) default)
                      ((and (pair? (car items))
                            (or (equal? (caar items) sym)
                                (equal? (caar items) str)
                                (equal? (caar items) kw)))
                        (let ((val (cdar items)))
                          (if (and (pair? val) (null? (cdr val)))
                            (car val)
                            val)))
                      ((and (keyword? (car items)) (equal? (car items) kw) (pair? (cdr items)))
                        (cadr items))
                      (else (loop (cdr items)))))))

(define* (prop-ref alist key #:optional (default #f))
         "Safely retrieve value for KEY from ALIST, checking symbol, string, or keyword variants."
         (if (pair? alist)
           (let* ((sym (ensure-symbol key))
                  (str (symbol->string sym))
                  (kw (symbol->keyword sym)))
             (let loop ((rest alist))
                       (if (null? rest)
                         default
                         (let ((pair (car rest)))
                           (if (and (pair? pair)
                                    (let ((k (car pair)))
                                      (or (eq? k sym) (eq? k kw) (and (string? k) (string=? k str)) (equal? k key))))
                             (cdr pair)
                             (loop (cdr rest)))))))
           default))

(define json-ref prop-ref)

(define (alist-remove key alist)
        "Return ALIST with all entries whose car matches KEY removed.
Matches against string, symbol, or keyword forms via ensure-string."
        (if (pair? alist)
          (let ((key-str (ensure-string key)))
            (remove (lambda (pair)
                      (and (pair? pair)
                           (string=? (ensure-string (car pair)) key-str)))
                    alist))
          '()))

(define (alist-merge base overlay)
        "Merge association lists BASE and OVERLAY. Entries in OVERLAY take
precedence over entries in BASE with matching keys (compared via ensure-string)."
        (cond
          ((null? overlay) (or base '()))
          ((null? base) overlay)
          (else
            (let ((tbl (make-hash-table (length overlay))))
              (for-each (lambda (e)
                          (when (pair? e)
                            (hash-set! tbl (ensure-string (car e)) #t)))
                        overlay)
              (append overlay
                      (remove (lambda (e)
                                (and (pair? e)
                                     (hash-ref tbl (ensure-string (car e)))))
                              base))))))

(define (prop-set alist key val)
        "Update or insert KEY with VAL in ALIST, preserving or using symbol representation."
        (let ((sym (ensure-symbol key)))
          (cons (cons sym val) (alist-remove sym alist))))

;;;
;;; Safe JSON Serialization
;;;
(define (safe-json-string val)
        "Return VAL as string. If not already a string, serializes VAL to JSON."
        (cond
          ((string? val) val)
          (else (catch #t
                  (lambda () (scm->json-string val))
                  (lambda (_key . _args) (object->string val))))))

(define* (safe-json-parse str #:optional (fallback #f))
         "Parse JSON string STR, returning FALLBACK if parsing fails."
         (if (or (not (string? str)) (string-null? (string-trim-both str)))
           fallback
           (catch #t
             (lambda () (json-string->scm str))
             (lambda _ fallback))))

(define (parse-tool-args args)
        "Normalize ARGS (string or parsed structure) into Scheme data, returning '() on error."
        (cond
          ((string? args) (safe-json-parse args '()))
          ((pair? args) args)
          ((vector? args) args)
          (else '())))

(define-syntax let-props
               (syntax-rules ()
                             ((_ obj () body ...)
                              (begin body ...))
                             ((_ obj ((var key default) . rest) body ...)
                              (let ((var (prop-ref obj (quote key) default)))
                                (let-props obj rest body ...)))
                             ((_ obj ((var key) . rest) body ...)
                              (let-props obj ((var key #f) . rest) body ...))
                             ((_ obj (var . rest) body ...)
                              (let-props obj ((var var #f) . rest) body ...))))

(define-syntax with-tool-args
               (syntax-rules ()
                             ((_ raw-args (binding ...) body ...)
                              (let ((parsed (parse-tool-args raw-args)))
                                (let-props parsed (binding ...) body ...)))))

(define-syntax %with-tool-args-bindings
               (syntax-rules ()
                             ((_ parsed bindings body ...)
                              (let-props parsed bindings body ...))))

;;;
;;; Flow Control & Dispatch Macros
;;;
(define-syntax with-default
               (syntax-rules ()
                             ((_ default expr1 expr2 ...)
                              (catch #t
                                (lambda () expr1 expr2 ...)
                                (lambda _ default)))))

(define-syntax-rule (ignore-errors expr1 expr2 ...)
                    (with-default #f expr1 expr2 ...))

(define-syntax receive
               (syntax-rules ()
                             ((_ (var ...) expr body ...)
                              (call-with-values (lambda () expr)
                                                (lambda (var ...) body ...)))
                             ((_ (var ... . rest) expr body ...)
                              (call-with-values (lambda () expr)
                                                (lambda (var ... . rest) body ...)))))

(define-syntax let-values*
               (syntax-rules ()
                             ((_ () body ...)
                              (begin body ...))
                             ((_ (((var ...) expr) . rest) body ...)
                              (receive (var ...) expr
                                       (let-values* rest body ...)))))

(define-syntax with-directory
               (syntax-rules ()
                             ((_ dir body ...)
                              (let ((target dir)
                                    (orig (getcwd)))
                                (dynamic-wind
                                  (lambda () (when target (chdir target)))
                                  (lambda () body ...)
                                  (lambda () (when (and target orig) (chdir orig))))))))

(define-syntax with-env
               (syntax-rules ()
                             ((_ ((var val) ...) body ...)
                              (let ((origs (list (cons var (getenv var)) ...)))
                                (dynamic-wind
                                  (lambda () (setenv var val) ...)
                                  (lambda () body ...)
                                  (lambda ()
                                    (for-each (lambda (pair)
                                                (if (cdr pair) (setenv (car pair) (cdr pair)) (unsetenv (car pair))))
                                              origs)))))))

(define-syntax string-case
               (syntax-rules (else)
                             ((_ str (else body ...))
                              (begin body ...))
                             ((_ str ((target ...) body ...) rest ...)
                              (let ((s str))
                                (if (or (string=? s target) ...)
                                  (begin body ...)
                                  (string-case s rest ...))))
                             ((_ str (target body ...) rest ...)
                              (let ((s str))
                                (if (string=? s target)
                                  (begin body ...)
                                  (string-case s rest ...))))))

;;;
;;; Environment Formatting
;;;
(define (normalize-env-alist raw-env)
        "Normalize heterogenous environment specifications into canonical ((str-key . str-val) ...)."
        (cond
          ((null? raw-env) '())
          ((and (pair? raw-env) (or (symbol? (car raw-env)) (string? (car raw-env))))
            (list (cons (ensure-string (car raw-env)) (ensure-string (cdr raw-env)))))
          ((list? raw-env)
            (filter-map
              (lambda (item)
                (cond
                  ((and (pair? item) (or (symbol? (car item)) (string? (car item))))
                    (cons (ensure-string (car item)) (ensure-string (cdr item))))
                  ((string? item)
                    (let ((idx (string-contains item "=")))
                      (if idx
                        (cons (substring item 0 idx) (substring item (+ idx 1)))
                        #f)))
                  (else #f)))
              raw-env))
          (else '())))

(define (env-alist->exec-strings env-alist)
        "Convert canonical environment alist into list of 'KEY=VAL' strings."
        (map (lambda (pair)
               (format #f "~a=~a" (ensure-string (car pair)) (ensure-string (cdr pair))))
             env-alist))
(define (split-words str)
        "Split string STR on whitespace, discarding empty tokens."
        (if (or (not (string? str)) (string-null? str))
          '()
          (filter
            (lambda (s)
              (not (string-null? s)))
            (string-split (string-trim-both str) #\space))))

(define (truncate-string text max-chars)
        "Return TEXT capped at MAX-CHARS characters, the tail replaced by an
ellipsis when it had to be cut."
        (if (> (string-length text) max-chars)
          (string-append (substring text 0 (- max-chars 3)) "...")
          text))

(define truncate-display truncate-string)

(define (format-exception ex-key ex-args)
        "Format an exception key and args into a human-readable diagnostic string."
        (cond
          ((and (eq? ex-key 'misc-error)
                (pair? ex-args)
                (pair? (cdr ex-args))
                (string? (cadr ex-args)))
            (catch #t
              (lambda ()
                (let* ((msg-fmt (cadr ex-args))
                       (msg-args (if (and (pair? (cddr ex-args)) (pair? (caddr ex-args)))
                                   (caddr ex-args)
                                   (cddr ex-args))))
                  (if (list? msg-args)
                    (apply format #f msg-fmt msg-args)
                    (format #f "~a" msg-fmt))))
              (lambda (_k . _a)
                (format #f "~a: ~s" ex-key ex-args))))
          ((and (pair? ex-args) (string? (car ex-args))) (car ex-args))
          (else (format #f "~a: ~s" ex-key ex-args))))

(define (sanitize-for-json val)
        "Sanitize nested data structures so they can be JSON-serialized."
        (cond
          ((or (string? val) (number? val) (boolean? val) (null? val)) val)
          ((symbol? val) (symbol->string val))
          ((pair? val)
            (if
                (and (or (symbol? (car val)) (string? (car val)))
                     (not (pair? (cdr val)))
                     (not (null? (cdr val))))
              (cons
                (if (symbol? (car val))
                  (symbol->string (car val))
                  (car val))
                (sanitize-for-json (cdr val)))
              (map sanitize-for-json val)))
          ((module? val) "#<module>")
          ((procedure? val) "#<procedure>")
          (else (format #f "~a" val))))

(define (format-token-kilo n)
        "Format number N into a compact 1-decimal-place kilo string, e.g. 1500 -> '1.5k'."
        (format #f "~,1fk" (/ (if (number? n) n 0) 1000.0)))

;;;
;;; Subprocess Execution & Signals
;;;
(define *active-subprocess-pid* #f)

(define (active-subprocess-pid)
        "Return the PID of the currently active shell subprocess, or #f if none."
        *active-subprocess-pid*)

(define (set-active-subprocess-pid! pid)
        "Set the PID of the currently active shell subprocess."
        (set! *active-subprocess-pid* pid))

(define (kill-active-subprocess!)
        "Send SIGINT (falling back to SIGTERM) to the active subprocess and its group, if any."
        (let ((pid *active-subprocess-pid*))
          (when (and (number? pid) (> pid 0))
            (catch #t
              (lambda ()
                ;; Signal process group first, then the process
                (catch #t (lambda () (kill (- pid) SIGINT)) (lambda _ #f))
                (kill pid SIGINT))
              (lambda _
                (catch #t
                  (lambda ()
                    (catch #t (lambda () (kill (- pid) SIGTERM)) (lambda _ #f))
                    (kill pid SIGTERM))
                  (lambda _ #f)))))))

(define (exec-shell-command cmd)
        "Execute shell CMD and return (values output-string exit-code error-or-#f)."
        (catch #t
          (lambda ()
            (let*
                ((wrapped-cmd (format #f "( ~a\n) 2>&1 < /dev/null" cmd))
                 (pipe (open-pipe wrapped-cmd OPEN_READ))
                 (pid
                   (catch #t
                     (lambda ()
                       (let*
                           ((popen-mod (resolve-module '(ice-9 popen)))
                            (mutex (module-ref popen-mod 'port/pid-table-mutex))
                            (table (module-ref popen-mod 'port/pid-table)))
                         (with-mutex mutex (hashq-ref table pipe))))
                     (lambda _
                       #f))))
              (dynamic-wind
                (lambda ()
                  (when pid (set-active-subprocess-pid! pid)))
                (lambda ()
                  (let*
                      ((raw-out (read-string pipe))
                       (status (close-pipe pipe))
                       (exit-val
                         (if (status:exit-val status)
                           (status:exit-val status)
                           0))
                       (clean-out
                         (if
                             (or (eof-object? raw-out) (string-null? raw-out))
                           ""
                           (string-trim-right raw-out))))
                    (values clean-out exit-val #f)))
                (lambda ()
                  (when (eqv? *active-subprocess-pid* pid) (set-active-subprocess-pid! #f))))))
          (lambda (key . args)
            (values #f 1 (format #f "Shell error (~a): ~s" key args)))))