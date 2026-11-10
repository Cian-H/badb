;;; (geasa capabilities-git plugin) — Git Capability Bundle
;;;
;;; Contract-verified Git tooling for agentic workflows: status, diff,
;;; log inspection, and commits. Deliberately read-mostly: the only
;;; mutating operation is `git-commit', which stages nothing on its
;;; own and refuses to run on a clean tree.
;;;
;;; Design notes:
;;;  * All Git interaction goes through `git' CLI subprocesses; every
;;;    argument is shell-quoted, so untrusted strings can never
;;;    interpolate into the command line.
;;;  * Every tool returns a string, so results are always renderable
;;;    and never unwind on Git errors (errors become structured
;;;    "git: ..." messages).
;;;  * `%git' returns (EXIT-OK? . OUTPUT) so callers can distinguish
;;;    "not a repository" from other Git failures.

(define-module (geasa capabilities-git plugin)
               #:use-module (badb kernel registry)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 rdelim)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-13)
               #:export (git-status
                          git-diff
                          git-log
                          git-commit
                          register-git-tools!))

;; ---- subprocess plumbing ---------------------------------------------------

(define (%shell-quote s)
        "Quote string S as a single POSIX shell word."
        (string-append "'"
                       (list->string
                         (append-join
                           (map (lambda (c)
                                  (if (eqv? c #\')
                                    (list #\' #\\ #\' #\')
                                    (list c)))
                                (string->list s))))
                       "'"))

(define (append-join lists)
        (fold-right append '() lists))

(define (%git repo . args)
        "Run `git ARGS' inside REPO; return (EXIT-OK? . OUTPUT-STRING).
Never raises: subprocess spawn failures degrade to (#f . \"\")."
        (catch #t
          (lambda ()
            (let* ((cmd (string-append "git -C " (%shell-quote repo) " "
                                       (string-join
                                         (map %shell-quote
                                              (filter (lambda (a)
                                                        (not (string-null? a)))
                                                      args))
                                         " ")))
                   (port (open-input-pipe cmd))
                   (out (let loop ((acc '()))
                                  (let ((line (read-line port)))
                                    (if (eof-object? line)
                                      (string-join (reverse acc) "\n")
                                      (loop (cons line acc))))))
                   (status (close-pipe port)))
              (cons (zero? (status:exit-val status)) out)))
          (lambda _ (cons #f ""))))

(define (%first-line s)
        (let ((idx (string-index s #\newline)))
          (if idx (substring s 0 idx) s)))

(define (%git-error res)
        "Human-readable degradation message from a %git result pair."
        (let ((err (%first-line (cdr res))))
          (if (string-null? err)
            "git: command failed"
            (string-append "git: " err))))

;; ---- public tool surface ---------------------------------------------------

(define* (git-status repo)
         "Return porcelain status of REPO plus current branch."
         (let ((res (%git repo "status" "--porcelain=v1" "-b")))
           (cond
             ((not (car res)) "git: not a repository")
             ((string-null? (cdr res)) "(clean working tree)")
             (else (cdr res)))))

(define* (git-diff repo #:key (staged #f) (path #f))
         "Return unified diff of unstaged (or staged) changes in REPO."
         (let ((res (%git repo "diff" "--no-color"
                          (if staged "--cached" "")
                          "--" (or path ""))))
           (cond
             ((not (car res)) (%git-error res))
             ((string-null? (cdr res)) "(no changes)")
             (else (cdr res)))))

(define* (git-log repo #:key (limit 10) (path #f))
         "Return the last LIMIT commits as oneline summaries."
         (let* ((n (max 1 (min (or limit 10) 100)))
                (res (%git repo "log" "--oneline"
                           (string-append "--max-count="
                                          (number->string n))
                           "--" (or path ""))))
           (cond
             ((not (car res)) (%git-error res))
             ((string-null? (cdr res)) "(no commits)")
             (else (cdr res)))))

(define* (git-commit repo message #:key (stage-all #f))
         "Commit current changes in REPO with MESSAGE. With STAGE-ALL,
stage every modification first. Refuses empty messages and no-op
commits; returns a human-readable summary."
         (cond
           ((not (string? message)) "error: commit message must be a string")
           ((string-null? (string-trim-both message))
             "error: commit message must not be empty")
           (else
             (let ((repo-check (%git repo "rev-parse" "--git-dir")))
               (cond
                 ((not (car repo-check)) "git: not a repository")
                 (else
                   (when stage-all (%git repo "add" "-A"))
                   (let ((porcelain (%git repo "status" "--porcelain=v1")))
                     (cond
                       ((string-null? (cdr porcelain)) "error: nothing to commit")
                       (else
                         (let ((res (%git repo "commit" "-m" message)))
                           (if (car res)
                             (string-append "committed: " (%first-line (cdr res)))
                             (%git-error res))))))))))))

;; ---- tool registration -----------------------------------------------------

(define (register-git-tools! reg)
        "Register the git capability tools in registry REG."
        (registry-attach! reg "git_status"
                          (lambda (args)
                            (git-status (or (assoc-ref args "repo") ".")))
                          #:metadata %git-status-schema)
        (registry-attach! reg "git_diff"
                          (lambda (args)
                            (git-diff (or (assoc-ref args "repo") ".")
                                      #:staged (assoc-ref args "staged")
                                      #:path (assoc-ref args "path")))
                          #:metadata %git-diff-schema)
        (registry-attach! reg "git_log"
                          (lambda (args)
                            (git-log (or (assoc-ref args "repo") ".")
                                     #:limit (assoc-ref args "limit")
                                     #:path (assoc-ref args "path")))
                          #:metadata %git-log-schema)
        (registry-attach! reg "git_commit"
                          (lambda (args)
                            (git-commit (or (assoc-ref args "repo") ".")
                                        (assoc-ref args "message")
                                        #:stage-all (assoc-ref args "stage-all")))
                          #:metadata %git-commit-schema))

(define %git-status-schema
        `((name . "git_status")
          (description . "Show working-tree status and current branch.")
          (parameters . ((type . "object")
                         (properties . ((repo . ((type . "string")))))))))

(define %git-diff-schema
        `((name . "git_diff")
          (description . "Show unstaged or staged diffs, optionally scoped to PATH.")
          (parameters . ((type . "object")
                         (properties . ((repo . ((type . "string")))
                                        (staged . ((type . "boolean")))
                                        (path . ((type . "string")))))))))

(define %git-log-schema
        `((name . "git_log")
          (description . "Show recent commit summaries (limit <= 100).")
          (parameters . ((type . "object")
                         (properties . ((repo . ((type . "string")))
                                        (limit . ((type . "integer")))))))))

(define %git-commit-schema
        `((name . "git_commit")
          (description . "Commit current changes (optionally staging all first).")
          (parameters . ((type . "object")
                         (properties . ((repo . ((type . "string")))
                                        (message . ((type . "string")))
                                        (stage-all . ((type . "boolean")))))
                         (required . #("message"))))))
