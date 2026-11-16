;;; (geasa capabilities-structural-editing plugin) — Structural AST Editing via semcp
;;;
;;; Integrates the `semcp` (structural-editing-mcp) binary as a first-class
;;; geis capability bundle.  Tool calls invoke `semcp --session <dir> call`
;;; directly via `open-pipe*' (no shell interpolation), keeping staged AST
;;; edits and branch workspaces persistent across turns, while hooking
;;; `commit_workspace` into `runtimes-rollback` checkpoints.

(define-module (geasa capabilities-structural-editing plugin)
               #:use-module (badb kernel registry)
               #:use-module (geasa runtimes-rollback plugin)
               #:use-module (json)
               #:use-module (ice-9 format)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 textual-ports)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-13)
               #:export (semcp-binary
                          semcp-available?
                          semcp-init!
                          semcp-call
                          semcp-list-tools
                          register-structural-editing-tools!))

;; ---- binary discovery -----------------------------------------------------

(define (%executable-file? path)
        (and (string? path)
             (not (string-null? path))
             (file-exists? path)
             (not (file-is-directory? path))
             (access? path X_OK)))

(define (%find-in-path name)
        (let ((path-env (or (getenv "PATH") "")))
          (any (lambda (dir)
                 (and (not (string-null? dir))
                      (let ((cand (string-append dir "/" name)))
                        (and (%executable-file? cand) cand))))
               (string-split path-env #\:))))

(define semcp-binary (make-parameter #f))

(define (%resolve-semcp-binary)
        (or (let ((override (semcp-binary)))
              (and (%executable-file? override) override))
            (let ((env-bin (getenv "BADB_SEMCP_BIN")))
              (and (%executable-file? env-bin) env-bin))
            (%find-in-path "semcp")))

(define (semcp-available?)
        "Return #t when the `semcp' binary is resolved and executable."
        (and (%resolve-semcp-binary) #t))

;; ---- subprocess execution -------------------------------------------------

(define (%run-semcp args)
        "Invoke `semcp' with ARGS (list of strings, no shell).
Returns (EXIT-OK? . OUTPUT-STRING)."
        (let ((bin (%resolve-semcp-binary)))
          (if bin
            (catch #t
              (lambda ()
                (let*
                    ((port (apply open-pipe* OPEN_READ bin args))
                     (out (get-string-all port))
                     (status (close-pipe port))
                     (code (status:exit-val status)))
                  (cons (and code (zero? code))
                        (if (eof-object? out)
                          ""
                          out))))
              (lambda (key . err-args)
                (cons #f (format #f "semcp subprocess error (~a): ~s" key err-args))))
            (cons #f "semcp unavailable: binary not found on PATH"))))

(define (%parse-json-safe str)
        (catch #t
          (lambda () (json-string->scm str))
          (lambda _ #f)))

(define (%extract-semcp-output ok? raw)
        (let ((parsed (%parse-json-safe raw)))
          (cond
            ((not (pair? parsed))
              (if ok? raw (string-append "semcp error: " raw)))
            ((assoc-ref parsed "ok")
              (let ((res (assoc-ref parsed "result")))
                (cond
                  ((string? res) res)
                  (res (scm->json-string res))
                  (else raw))))
            (else
              (let ((err (or (assoc-ref parsed "error")
                             (assoc-ref parsed "result")
                             raw)))
                (string-append "semcp error: "
                               (if (string? err) err (scm->json-string err))))))))

;; ---- session & tool API ---------------------------------------------------

(define (%default-session-dir workspace)
        (string-append (string-trim-right workspace #\/) "/.semcp"))

(define*
  (semcp-init! #:key (workspace (getcwd)) (session-dir #f) (paths '()))
  "Initialize a staged .semcp session for WORKSPACE."
  (if (semcp-available?)
    (let*
        ((sdir (or session-dir (%default-session-dir workspace)))
         (args (append (list "--session" sdir "init") paths (list "--json")))
         (res (%run-semcp args)))
      (%extract-semcp-output (car res) (cdr res)))
    "semcp unavailable: binary not found on PATH"))

(define (semcp-list-tools)
        "Query `semcp tools --json' and return a list of tool metadata alists
ready for `registry-attach!'."
        (if (semcp-available?)
          (let*
              ((res (%run-semcp '("tools" "--json")))
               (parsed (and (car res) (%parse-json-safe (cdr res))))
               (tools (and (pair? parsed) (assoc-ref parsed "tools"))))
            (if (vector? tools)
              (filter-map
                (lambda (entry)
                  (let
                      ((name (and (pair? entry) (assoc-ref entry "name")))
                       (desc (and (pair? entry) (assoc-ref entry "description")))
                       (schema (and (pair? entry) (assoc-ref entry "inputSchema"))))
                    (and (string? name)
                         `((name . ,name) (description . ,(or desc ""))
                           (parameters . ,(or schema '((type . "object"))))))))
                (vector->list tools))
              '()))
          '()))

(define (%encode-args args)
        (cond
          ((string? args) args)
          ((or (not args) (null? args)) "{}")
          (else (scm->json-string args))))

(define*
  (semcp-call tool-name
              args
              #:key
              (workspace (getcwd))
              (session-dir #f)
              (rollback-mgr #f))
  "Invoke `semcp' TOOL-NAME with ARGS alist/JSON string.
Creates a `runtimes-rollback' checkpoint before `commit_workspace'."
  (if (semcp-available?)
    (let*
        ((sdir (or session-dir (%default-session-dir workspace)))
         (state-file (string-append sdir "/state.json")))
      (unless (file-exists? state-file)
        (semcp-init! #:workspace workspace #:session-dir sdir))
      (when
          (and rollback-mgr (string=? tool-name "commit_workspace"))
        (create-checkpoint! rollback-mgr "pre-semcp-commit"))
      (let*
          ((json-args (%encode-args args))
           (res (%run-semcp (list "--session" sdir "call" tool-name json-args "--json"))))
        (%extract-semcp-output (car res) (cdr res))))
    "semcp unavailable: binary not found on PATH"))

;; ---- registry wiring ------------------------------------------------------

(define* (register-structural-editing-tools! reg
                                             #:key (workspace (getcwd))
                                             (session-dir #f)
                                             (rollback-mgr #f))
         "Register all `semcp' structural editing and analysis tools in REG.
Returns the number of registered tools, or 0 if `semcp' is unavailable."
         (let ((tools (semcp-list-tools)))
           (for-each
             (lambda (meta)
               (let ((name (assoc-ref meta 'name)))
                 (registry-attach!
                   reg
                   name
                   (lambda (args)
                     (semcp-call name args
                                 #:workspace workspace
                                 #:session-dir session-dir
                                 #:rollback-mgr rollback-mgr))
                   #:metadata meta)))
             tools)
           (length tools)))
