;;; (geasa surfaces-tui session) — Shell/REPL Evaluation & Session Persistence
;;;
;;; Extracted from surfaces-tui plugin.scm. Provides input classification,
;;; Scheme REPL evaluation, shell execution with exec-logging, session persistence
;;; (/resume), and conversation transcript formatting.

(define-module (geasa surfaces-tui session)
               #:use-module (geasa capabilities-history-tools plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (json)
               #:use-module (ice-9 format)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 rdelim)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-19)
               #:export (classify-input
                          normalize-scheme-expr
                          make-repl-mod
                          eval-scheme-code
                          %exec-log-path
                          %append-exec-log!
                          execute-shell-command
                          %session-filename-max-length
                          default-sessions-dir
                          session-file-path
                          turn->jsonl-line
                          persist-session-file!
                          load-session-file
                          recent-session-files
                          session-mtime-string
                          format-conversation-transcript))

;;;
;;; Shell & REPL Execution Helpers
;;;

(define (classify-input input)
        "Classify trimmed user INPUT into an action tag and payload:
   '(slash cmd . arg)
   '(shell . cmd-string)
   '(scheme . code-string)
   '(chat . text)
   #f for empty or whitespace-only input"
        (let ((trimmed (string-trim-both input)))
          (cond
            ((string-null? trimmed)
              #f)
            ((string-prefix? "!" trimmed)
              (cons 'shell (string-trim-both (substring trimmed 1))))
            ((string-prefix? "(" trimmed)
              (cons 'scheme trimmed))
            ((string-prefix? "/" trimmed)
              (let* ((without-slash (substring trimmed 1))
                     (space-pos (string-index without-slash #\space)))
                (if space-pos
                  (list 'slash
                        (substring without-slash 0 space-pos)
                        (string-trim-both (substring without-slash (+ space-pos 1))))
                  (list 'slash without-slash ""))))
            (else
              (cons 'chat trimmed)))))

(define (normalize-scheme-expr str)
        "Normalize user Scheme input string, ensuring s-expressions without outer
parentheses (e.g. '+ 1 2' or '+ 1 2)') are wrapped for evaluation."
        (let ((trimmed (string-trim-both str)))
          (cond
            ((string-null? trimmed) trimmed)
            ;; Already properly wrapped
            ((string-prefix? "(" trimmed) trimmed)
            ;; Literal string or quote
            ((or (string-prefix? "\"" trimmed) (string-prefix? "'" trimmed)) trimmed)
            ;; Ends with ')' but missing '(': e.g. "+ 1 2)"
            ((and (string-suffix? ")" trimmed) (not (string-prefix? "(" trimmed)))
              (string-append "(" trimmed))
            ;; Has spaces and no outer parens: e.g. "+ 1 2"
            ((string-contains trimmed " ")
              (string-append "(" trimmed ")"))
            ;; Standalone identifier or literal number: e.g. "reg", "42"
            (else trimmed))))

(define* (make-repl-mod reg jrn mgr client #:optional (cfg #f) (mcp-mgr #f)
                        (policy #f) (compactor #f) (rollback-mgr #f) (verifier #f) (composer #f))
         "Create an isolated user module pre-populated with kernel state bindings
and core badb modules for interactive evaluation."
         (let ((mod (make-fresh-user-module)))
           (module-use! mod (resolve-interface '(guile)))
           (module-use! mod (resolve-interface '(geasa providers-model plugin)))
           (module-use! mod (resolve-interface '(badb registry)))
           (module-use! mod (resolve-interface '(badb transaction)))
           (module-use! mod (resolve-interface '(badb journal)))
           (module-use! mod (resolve-interface '(geasa foundation-config plugin)))
           (catch #t (lambda () (module-use! mod (resolve-interface '(geasa capabilities-mcp plugin)))) (lambda _ #f))
           (catch #t (lambda () (module-use! mod (resolve-interface '(geasa policies-guardrail plugin)))) (lambda _ #f))
           (catch #t (lambda () (module-use! mod (resolve-interface '(geasa runtimes-context plugin)))) (lambda _ #f))
           (catch #t (lambda () (module-use! mod (resolve-interface '(geasa runtimes-prompt plugin)))) (lambda _ #f))
           (catch #t (lambda () (module-use! mod (resolve-interface '(geasa runtimes-rollback plugin)))) (lambda _ #f))
           (catch #t (lambda () (module-use! mod (resolve-interface '(geasa runtimes-verify plugin)))) (lambda _ #f))
           (catch #t (lambda () (module-use! mod (resolve-interface '(geasa runtimes-react plugin)))) (lambda _ #f))
           (module-use! mod (resolve-interface '(ice-9 format)))
           (module-use! mod (resolve-interface '(ice-9 rdelim)))
           (module-use! mod (resolve-interface '(ice-9 pretty-print)))
           (catch #t
             (lambda () (module-use! mod (resolve-interface '(ice-9 session))))
             (lambda _ #f))
           (module-use! mod (resolve-interface '(srfi srfi-1)))
           (module-define! mod 'reg reg)
           (module-define! mod 'jrn jrn)
           (module-define! mod 'mgr mgr)
           (module-define! mod 'client client)
           (when cfg (module-define! mod 'cfg cfg))
           (when mcp-mgr (module-define! mod 'mcp-mgr mcp-mgr))
           (when policy (module-define! mod 'policy policy))
           (when compactor (module-define! mod 'compactor compactor))
           (when rollback-mgr (module-define! mod 'rollback-mgr rollback-mgr))
           (when verifier (module-define! mod 'verifier verifier))
           (when composer (module-define! mod 'composer composer))
           mod))

(define (eval-scheme-code code-str repl-mod)
        "Evaluate CODE-STR in REPL-MOD and return (values result-string error-or-#f)."
        (let ((normalized (normalize-scheme-expr code-str)))
          (catch #t
            (lambda ()
              (let* ((p (open-input-string normalized))
                     (output-port (open-output-string)))
                (let loop ()
                          (let ((expr (read p)))
                            (unless (eof-object? expr)
                              (let ((printed
                                      (with-output-to-string
                                          (lambda ()
                                                  (call-with-values (lambda () (eval expr repl-mod))
                                                                    (lambda vals
                                                                      (for-each (lambda (v)
                                                                                  (format #t "=> ~s\n" v))
                                                                                vals)))))))
                                (display printed output-port)
                                (loop)))))
                (let ((res (string-trim-right (get-output-string output-port))))
                  (values (if (string-null? res) "=> #<unspecified>" res) #f))))
            (lambda (key . args)
              (values #f (format #f "Scheme error (~a): ~s" key args))))))

(define (%exec-log-path)
        "Exec log destination: BADB_EXEC_LOG when set (empty string disables
logging), else .badb/exec-log.jsonl."
        (let ((env (getenv "BADB_EXEC_LOG")))
          (cond
            ((and (string? env) (string-null? env)) #f)
            ((string? env) env)
            (else ".badb/exec-log.jsonl"))))

(define (%append-exec-log! cmd)
        "Append a byte-exact record of CMD to the exec log just before spawn,
best effort: never throws, silently skips when the log is disabled or
unwritable."
        (catch #t
          (lambda ()
            (let ((path (%exec-log-path)))
              (when path
                (ensure-parent-directory! path)
                (let* ((tv (gettimeofday))
                       (port (open-file path "a")))
                  (format port "{\"ts\":~a,\"us\":~a,\"len\":~a,\"cmd\":~a}\n"
                          (car tv) (cdr tv) (string-length cmd) (scm->json-string cmd))
                  (close-port port)))))
          (lambda _ #f)))

(define (execute-shell-command cmd)
        "Execute shell CMD and return (values output-string exit-code
error-or-#f). Records the byte-exact command to the exec log
(.badb/exec-log.jsonl, BADB_EXEC_LOG override; empty disables) just
before spawn."
        (%append-exec-log! cmd)
        (exec-shell-command cmd))

;;;
;;; Chat Session Persistence (/resume)
;;;

(define %session-filename-max-length 40)

(define (default-sessions-dir)
        "Return the standard directory for persisted chat sessions
(~/.local/share/badb/sessions, honoring XDG_DATA_HOME)."
        (let ((xdg (getenv "XDG_DATA_HOME"))
              (home (getenv "HOME")))
          (cond
            ((and xdg (not (string-null? xdg)))
              (string-append xdg "/badb/sessions"))
            ((and home (not (string-null? home)))
              (string-append home "/.local/share/badb/sessions"))
            (else
              "/tmp/badb/sessions"))))

(define (session-file-path)
        "Return the JSONL path for this TUI process's session log: a
<timestamp>-<pid> name under (default-sessions-dir)."
        (string-append
          (default-sessions-dir) "/"
          (date->string (current-date 0) "~Y~m~dT~H~M~SZ")
          "-"
          (number->string (getpid))
          ".jsonl"))

(define (turn->jsonl-line msg)
        "Serialize one conversation turn MSG (an alist with role and content)
into a single JSON object line {\"role\":...,\"content\":...}."
        (scm->json-string
          `(("role" . ,(message-role msg))
            ("content" . ,(message-content msg)))))

(define (persist-session-file! path history)
        "Write the whole conversation HISTORY to the session JSONL file at PATH,
one JSON turn object per line (the system prompt first, as-is), creating
the enclosing directory when missing. Return #t on success and #f on any
filesystem failure."
        (catch #t
          (lambda ()
            (ensure-parent-directory! path)
            (call-with-output-file path
                                   (lambda (port)
                                     (for-each
                                       (lambda (msg)
                                         (display (turn->jsonl-line msg) port)
                                         (newline port))
                                       history)))
            #t)
          (lambda _ #f)))

(define (load-session-file path)
        "Load the conversation turns persisted in the session JSONL file at PATH,
oldest first, skipping blank or malformed lines. An absent or unreadable
file yields the empty list."
        (catch #t
          (lambda ()
            (if (and (string? path) (file-exists? path))
              (let ((raw (or (file->string path) "")))
                (filter-map jsonl-line->turn (string-split raw #\newline)))
              '()))
          (lambda _ '())))

(define (recent-session-files dir)
        "Return up to 7 session JSONL file paths in DIR, most recently modified
first. A missing or unreadable directory yields the empty list."
        (catch #t
          (lambda ()
            (let* ((names
                     (catch 'system-error
                       (lambda ()
                         (let ((stream (opendir dir)))
                           (let loop ((accum '()))
                                     (let ((entry (readdir stream)))
                                       (if (eof-object? entry)
                                         (begin (closedir stream) (reverse accum))
                                         (loop (cons entry accum)))))))
                       (lambda _ '())))
                   (paths
                     (filter-map
                       (lambda (name)
                         (and (string-suffix? ".jsonl" name)
                              (let ((path (string-append dir "/" name)))
                                (and (file-exists? path) path))))
                       names)))
              (take
                (sort paths
                      (lambda (a b)
                        (> (stat:mtime (stat a))
                           (stat:mtime (stat b)))))
                (min 7 (length paths)))))
          (lambda _ '())))

(define (session-mtime-string path)
        "Format the modification time of PATH as a local timestamp for the
resume menu."
        (catch #t
          (lambda ()
            (date->string
              (time-utc->date (make-time time-utc 0 (stat:mtime (stat path))))
              "~Y-~m-~d ~H:~M"))
          (lambda _ "unknown")))

(define (format-conversation-transcript messages)
        "Render MESSAGES (the TUI conversation alist with role/content/thoughts
entries) as a plain-text transcript: one block per message with a role
header, an optional thinking section, and the content. Pure - no I/O."
        (let ((blocks
                (map
                  (lambda (msg)
                    (let* ((role
                             (let ((r (assoc-ref msg 'role)))
                               (cond
                                 ((string? r) r)
                                 ((symbol? r) (symbol->string r))
                                 (else "system"))))
                           (content
                             (let ((c (assoc-ref msg 'content)))
                               (cond
                                 ((string? c) c)
                                 ((not c) "")
                                 (else (format #f "~a" c)))))
                           (thoughts
                             (let ((t (assoc-ref msg 'thoughts)))
                               (and (string? t) (not (string-null? t)) t))))
                      (string-append "=== "
                                     (string-upcase role)
                                     " ===\n"
                                     (if thoughts
                                       (string-append "[thinking]\n" thoughts "\n\n")
                                       "")
                                     (if (string-null? content)
                                       ""
                                       (string-append content "\n"))
                                     "\n")))
                  messages)))
          (string-append ";; badb conversation transcript\n" (string-join blocks ""))))
