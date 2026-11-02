#!/usr/bin/env -S guile -s
!#
;;; scripts/verify-kernel-deps.scm — Verify zero user-space dependencies in kernel modules
;;;
;;; Usage: guile -L src -L . scripts/verify-kernel-deps.scm [kernel-dir]
;;;    or: ./scripts/verify-kernel-deps.scm [kernel-dir]

(when (current-filename)
  (let ((dir (dirname (current-filename))))
    (add-to-load-path (canonicalize-path (string-append dir "/../src")))
    (add-to-load-path (canonicalize-path (string-append dir "/..")))))

(use-modules (ice-9 ftw)
             (ice-9 format)
             (ice-9 rdelim)
             (ice-9 regex))

(define kernel-dir
  (if (> (length (command-line)) 1)
      (cadr (command-line))
      "src/badb/kernel"))

(define forbidden-subsystems
  '(providers runtimes capabilities surfaces policies geis))

(define legacy-userspace-modules
  '(model react synthesis filetree-mcp history-tools cli nc tui guardrail
    mcp mcp-server context theme prompt log rollback verify config util spec))

(define (forbidden-module? mod)
        (cond
          ;; (badb <subsystem> ...)
          ((and (list? mod)
                (>= (length mod) 2)
                (eq? (car mod) 'badb)
                (memq (cadr mod) forbidden-subsystems))
            #t)
          ;; (badb <legacy-userspace> ...)
          ((and (list? mod)
                (>= (length mod) 2)
                (eq? (car mod) 'badb)
                (memq (cadr mod) legacy-userspace-modules))
            #t)
          ((and (list? mod) (>= (length mod) 2) (eq? (car mod) 'geasa)) #t)
          (else #f)))

(define (extract-imports-from-form form)
  (let ((imports '()))
    (define (scan item)
      (when (pair? item)
        (cond
          ;; (define-module (badb kernel ...) #:use-module (foo bar) ...)
          ((eq? (car item) 'define-module)
           (let loop ((rest (cdr item)))
             (cond
               ((not (pair? rest)) #f)
               ((and (eq? (car rest) #:use-module) (pair? (cdr rest)))
                (set! imports (cons (cadr rest) imports))
                (loop (cddr rest)))
               ((and (eq? (car rest) #:autoload) (pair? (cdr rest)))
                (set! imports (cons (cadr rest) imports))
                (loop (if (> (length rest) 2) (cdddr rest) '())))
               (else (loop (cdr rest))))))
          ;; (use-modules (foo bar) ...)
          ((eq? (car item) 'use-modules)
           (let loop ((rest (cdr item)))
             (when (pair? rest)
               (set! imports (cons (car rest) imports))
               (loop (cdr rest)))))
          ;; (load "...") or (primitive-load "...")
          ((memq (car item) '(load primitive-load))
           (set! imports (cons item imports)))
          (else
           (scan (car item))
           (scan (cdr item))))))
    (scan form)
    imports))

(define (check-file-ast filepath)
  (let ((violations '()))
    (catch #t
      (lambda ()
        (call-with-input-file filepath
          (lambda (port)
            (let loop ((form (read port)))
              (unless (eof-object? form)
                (let ((imports (extract-imports-from-form form)))
                  (for-each
                    (lambda (imp)
                      (when (forbidden-module? imp)
                        (set! violations (cons imp violations))))
                    imports))
                (loop (read port)))))))
      (lambda (key . args)
        (format (current-error-port) "Warning: could not parse ~a as Scheme AST: ~a~%" filepath args)))
    violations))

(define (check-file-regex filepath)
        (let
            ((violations '())
             ;; Pattern looks for import/use statements targeting forbidden modules
             (pattern
               (make-regexp
                 "use-(module|modules).*\\((badb[ \t]+(providers|runtimes|capabilities|surfaces|policies|model|react|synthesis|filetree-mcp|history-tools|cli|nc|tui|guardrail|mcp|mcp-server|context|theme|prompt|log|rollback|verify|config|util|spec)|geasa[ \t])")))
          (call-with-input-file filepath
                                (lambda (port)
                                  (let loop
                                    ((line (read-line port)) (line-num 1))
                                    (unless (eof-object? line)
                                      (let ((m (regexp-exec pattern line)))
                                        (when m
                                          (set! violations
                                                (cons (format #f "Line ~a: ~a" line-num (string-trim-both line)) violations))))
                                      (loop (read-line port) (+ line-num 1))))))
          violations))

;; DEV-DEBT [dead-code]: the (geis <fid> ...) builtin-bundle allowance is inert —
;; (geasa ...) imports are now fully forbidden in kernel code above. Remove this
;; check or repurpose it once a (geis ...) namespace actually exists again.
(define (geis-import-feature-id mod)
  "For an import (geis <fid> ...), return <fid> as a string; else #f."
  (and (list? mod)
       (>= (length mod) 2)
       (eq? (car mod) 'geis)
       (symbol->string (cadr mod))))

(define (check-builtin-geis filepath)
  "Kernel modules may only import (geis ...) modules that ship as builtin
bundles under ./src/geasa/<fid>/ in the repo - never user-installed ones."
  (let ((violations '()))
    (catch #t
      (lambda ()
        (call-with-input-file filepath
          (lambda (port)
            (let loop ((form (read port)))
              (unless (eof-object? form)
                (for-each
                 (lambda (imp)
                   (let ((fid (geis-import-feature-id imp)))
                     (when (and fid (not (file-exists?
                                          (string-append "src/geasa/" fid "/manifest.scm"))))
                       (set! violations
                             (cons (format #f "import ~s is not a builtin bundle (no src/geasa/~a/manifest.scm)"
                                           imp fid)
                                   violations)))))
                 (extract-imports-from-form form))
                (loop (read port)))))))
      (lambda _ #f))
    violations))

(define (run-check)
  (format #t "=== Verifying Zero User-Space Dependencies in '~a' ===~%" kernel-dir)
  (unless (file-exists? kernel-dir)
    (format (current-error-port) "FAIL: Kernel directory '~a' does not exist.~%" kernel-dir)
    (exit 1))

  (let* ((files (scandir kernel-dir (lambda (n) (string-suffix? ".scm" n))))
         (total-violations 0))
    (unless files
      (format (current-error-port) "FAIL: No Scheme files found in '~a'.~%" kernel-dir)
      (exit 1))

    (for-each
      (lambda (filename)
        (let* ((filepath (string-append kernel-dir "/" filename))
               (ast-violations (check-file-ast filepath))
               (regex-violations (check-file-regex filepath)))
          (let ((geis-violations (check-builtin-geis filepath)))
            (set! total-violations (+ total-violations (length geis-violations)))
            (for-each (lambda (v)
                        (format (current-error-port) "  Geis Violation: ~a in ~a~%" v filename))
                      geis-violations))
          (format #t "Checking ~a... " filename)
          (if (and (null? ast-violations) (null? regex-violations) (= total-violations 0))
              (format #t "PASS (clean)~%")
              (begin
                (format #t "FAIL!~%")
                (for-each (lambda (v) (format (current-error-port) "  AST Violation: forbidden import ~s in ~a~%" v filename)) ast-violations)
                (for-each (lambda (v) (format (current-error-port) "  Text Violation: ~a in ~a~%" v filename)) regex-violations)
                (set! total-violations (+ total-violations (length ast-violations) (length regex-violations)))))))
      files)

    (if (> total-violations 0)
        (begin
          (format (current-error-port) "FAIL: Found ~a forbidden dependency violation(s) in kernel modules.~%" total-violations)
          (exit 1))
        (begin
          (format #t "SUCCESS: All kernel modules have zero user-space dependencies.~%")
          (exit 0)))))

(run-check)
