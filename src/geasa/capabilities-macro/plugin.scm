;;; (geasa capabilities-macro plugin) — Input Macro Expansion
;;;
;;; Expands lightweight directives inside user turns before they reach
;;; the ReAct session:
;;;   @@            → literal @
;;;   @file:PATH    → contents of PATH (bounded)
;;;   @NAME         → stored shortcut snippet (when registered)
;;;
;;; Design notes:
;;;  * Pure text-in/text-out: expansion never raises. Malformed or
;;;    unreadable directives expand to visible `@error:' markers so the
;;;    failure is observable in the delivered turn instead of silently
;;;    swallowed.
;;;  * No filesystem writes, no subprocesses: expansion is read-only.

(define-module (geasa capabilities-macro plugin)
               #:use-module (srfi srfi-13)
               #:use-module (badb kernel registry)
               #:export (expand-macros
                          register-macro-tools!))

;; ---- primitives ------------------------------------------------------------

(define (%starts? s pat i)
        "Return #t if string S contains PAT beginning at index I."
        (and (<= (+ i (string-length pat)) (string-length s))
             (string=? (substring s i (+ i (string-length pat))) pat)))

(define (%take-token s)
        "Return the prefix of S before the first whitespace character."
        (let loop ((i 0))
                  (if (or (= i (string-length s)) (char-whitespace? (string-ref s i)))
                    (substring s 0 i)
                    (loop (+ i 1)))))

(define (%read-file-contents path max-chars)
        "Return the contents of PATH as a string, or #f when unreadable.
Caps the result at MAX-CHARS with an explicit truncation marker."
        (catch #t
          (lambda ()
            (call-with-input-file path
                                  (lambda (port)
                                    (let loop ((chars '()) (n 0))
                                              (cond
                                                ((>= n max-chars)
                                                  (string-append (list->string (reverse chars)) "\n…[truncated]"))
                                                (else
                                                  (let ((c (read-char port)))
                                                    (if (eof-object? c)
                                                      (list->string (reverse chars))
                                                      (loop (cons c chars) (+ n 1))))))))))
          (lambda _ #f)))

;; ---- expansion -------------------------------------------------------------

(define* (expand-macros input #:key (shortcuts '()) (max-file-chars 8192))
         "Expand directives in INPUT. Recognizes @@ (literal @),
@file:PATH (file contents, bounded), and @NAME (shortcut snippet).
Unknown shortcuts pass through verbatim so ordinary text containing
@ is never corrupted."
         (let loop ((i 0) (acc '()))
                   (cond
                     ((>= i (string-length input))
                       (apply string-append (reverse acc)))
                     ((%starts? input "@@" i)
                       (loop (+ i 2) (cons "@" acc)))
                     ((%starts? input "@file:" i)
                       (let* ((rest (substring input (+ i 6)))
                              (token (%take-token rest))
                              (contents (if (string-null? token)
                                          "@error: empty file path"
                                          (or (%read-file-contents token max-file-chars)
                                              "@error: file unreadable"))))
                         (loop (+ i 6 (string-length token))
                               (cons contents acc))))
                     ((%starts? input "@" i)
                       (let* ((rest (substring input (+ i 1)))
                              (token (%take-token rest))
                              (val (assoc-ref shortcuts token)))
                         (loop (+ i 1 (string-length token))
                               (cons (or val (string-append "@" token)) acc))))
                     (else
                       (loop (+ i 1)
                             (cons (substring input i (+ i 1)) acc))))))

;; ---- tool registration -----------------------------------------------------

(define (register-macro-tools! reg)
        "Register the macro-expansion tool in registry REG."
        (registry-attach! reg "macro_expand"
                          (lambda (args)
                            (expand-macros (or (assoc-ref args "input") "")))
                          #:metadata %macro-expand-schema))

(define %macro-expand-schema
        `((name . "macro_expand")
          (description . "Expand @@, @file:PATH, and @SHORTCUT directives in text.")
          (parameters . ((type . "object")
                         (properties . ((input . ((type . "string")))))))))
