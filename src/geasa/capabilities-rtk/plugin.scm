;;; (geasa capabilities-rtk plugin) — RTK CLI Proxy & Bash Pre-Hook
;;;
;;; Integrates the `rtk' binary as a geis capability bundle. Provides a
;;; toggleable bash pre-hook that rewrites supported shell commands via
;;; `rtk rewrite' (no shell interpolation) so CLI outputs are compressed
;;; before entering context, composing cleanly with context-mode's 5KB
;;; post-hook.

(define-module (geasa capabilities-rtk plugin)
               #:use-module (badb kernel registry)
               #:use-module (ice-9 format)
               #:use-module (ice-9 textual-ports)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-13)
               #:export (rtk-binary
                          rtk-available?
                          rtk-enabled?
                          set-rtk-enabled!
                          rtk-rewrite
                          rtk-run
                          make-rtk-bash-pre-hook
                          register-rtk-tools!
                          %rtk-gain-schema))

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

(define rtk-binary (make-parameter #f))

(define (%resolve-rtk-binary)
        (or (let ((override (rtk-binary)))
              (and override (if (%executable-file? override) override 'missing)))
            (let ((env-bin (getenv "BADB_RTK_BIN")))
              (and (%executable-file? env-bin) env-bin))
            (%find-in-path "rtk")))

(define (rtk-available?)
        "Return #t when the `rtk' binary is resolved and executable."
        (let ((resolved (%resolve-rtk-binary)))
          (and (string? resolved) #t)))

;; ---- toggle state ---------------------------------------------------------

(define %rtk-enabled #t)

(define (rtk-enabled?)
        "Return #t when the RTK bash pre-hook is enabled."
        %rtk-enabled)

(define (set-rtk-enabled! enabled?)
        "Enable or disable the RTK bash pre-hook."
        (set! %rtk-enabled (and enabled? #t)))

;; ---- subprocess execution -------------------------------------------------

(define (%run-rtk args)
        "Invoke `rtk' with ARGS (list of strings, no shell), capturing stdout+stderr.
Returns (EXIT-CODE . OUTPUT-STRING) or (#f . ERROR-MESSAGE)."
        (let ((bin (%resolve-rtk-binary)))
          (if (string? bin)
            (catch #t
              (lambda ()
                (let* ((p (pipe))
                       (r-port (car p))
                       (w-port (cdr p))
                       (pid (primitive-fork)))
                  (if (= pid 0)
                    (begin
                      (close-port r-port)
                      (dup2 (port->fdes w-port) 1)
                      (dup2 (port->fdes w-port) 2)
                      (close-port w-port)
                      (catch #t
                        (lambda () (apply execlp bin bin args))
                        (lambda _ (primitive-exit 127))))
                    (begin
                      (close-port w-port)
                      (let* ((out (get-string-all r-port))
                             (_ (close-port r-port))
                             (st (cdr (waitpid pid)))
                             (code (status:exit-val st))
                             (clean (if (eof-object? out) "" (string-trim-both out))))
                        (cons code clean))))))
              (lambda (key . err-args)
                (cons #f (format #f "rtk subprocess error (~a): ~s" key err-args))))
            (cons #f "rtk unavailable: binary not found on PATH"))))

;; ---- rewrite & bash pre-hook ----------------------------------------------

(define (rtk-rewrite command)
        "Rewrite COMMAND via `rtk rewrite'.
Returns one of:
  ('rewrite . rewritten-command)
  ('passthrough . command)
  ('deny . reason)"
        (if (or (not (string? command)) (string-null? (string-trim-both command)))
          (cons 'passthrough (or command ""))
          (let* ((res (%run-rtk (list "rewrite" command)))
                 (code (car res))
                 (out (cdr res)))
            (cond
              ((not code) (cons 'passthrough command))
              ((or (= code 0) (= code 3))
                (if (string-null? out)
                  (cons 'passthrough command)
                  (cons 'rewrite out)))
              ((= code 2) (cons 'deny out))
              (else (cons 'passthrough command))))))

(define (make-rtk-bash-pre-hook)
        "Return a toggleable bash pre-hook (lambda (command) ...) that rewrites
supported commands through `rtk rewrite'."
        (lambda (command)
          (if (not (rtk-enabled?))
            command
            (let ((res (rtk-rewrite command)))
              (case (car res)
                ((rewrite) (cdr res))
                (else command))))))

(define (rtk-run args)
        "Run `rtk' with ARGS and return its output string."
        (let ((res (%run-rtk args)))
          (cdr res)))

;; ---- optional registry tool -----------------------------------------------

(define %rtk-gain-schema
        `((name . "rtk_gain")
          (description . "Show RTK CLI token savings summary and history.")
          (parameters . ((type . "object") (properties . ())))
          (_badb_defer . #t)
          (_badb_alist_args . #t)))

(define (register-rtk-tools! reg)
        "Register optional RTK inspection tools in REG."
        (registry-attach!
          reg
          "rtk_gain"
          (lambda (_args) (rtk-run '("gain")))
          #:metadata %rtk-gain-schema))
