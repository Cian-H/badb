#!/usr/bin/env -S guile --no-auto-compile -s
!#
;;; scripts/repin-bundle.scm — Recompute and update sha256 pins in geis bundle manifests
;;;
;;; Usage:
;;;   ./scripts/devenv -- guile --no-auto-compile -L src -L . scripts/repin-bundle.scm <bundle-dir> [...]
;;;   ./scripts/repin-bundle.scm <bundle-dir> [...]

(eval-when (expand load eval)
  (when (current-filename)
    (let ((dir (dirname (current-filename))))
      (add-to-load-path (canonicalize-path (string-append dir "/../src")))
      (add-to-load-path (canonicalize-path (string-append dir "/.."))))))

(use-modules (badb geis bundle)
             (ice-9 format)
             (srfi srfi-1))

(define (update-manifest-pins manifest pins)
  "Return MANIFEST with the 'sha256 entry replaced or appended with PINS."
  (let loop ((rest manifest) (found? #f) (acc '()))
    (cond
     ((null? rest)
      (if found?
          (reverse acc)
          (cons (cons 'sha256 pins) (reverse acc))))
     ((eq? (caar rest) 'sha256)
      (loop (cdr rest) #t (cons (cons 'sha256 pins) acc)))
     (else
      (loop (cdr rest) found? (cons (car rest) acc))))))

(define (repin-bundle! bundle-dir)
  "Recompute sha256 pins for BUNDLE-DIR, update its manifest.scm, and validate."
  (let ((manifest-path (string-append bundle-dir "/manifest.scm")))
    (unless (file-exists? manifest-path)
      (format (current-error-port) "Error: manifest not found at '~a'~%" manifest-path)
      (exit 1))
    (let ((manifest (read-geis-manifest manifest-path)))
      (unless manifest
        (format (current-error-port) "Error: failed to read manifest at '~a'~%" manifest-path)
        (exit 1))
      (let* ((pins (compute-geis-sha256-pins bundle-dir))
             (updated (update-manifest-pins manifest pins)))
        (call-with-output-file manifest-path
          (lambda (port)
            (write updated port)
            (newline port)))
        (let ((errors (geis-manifest-valid? bundle-dir)))
          (unless (null? errors)
            (format (current-error-port) "Error: manifest invalid after re-pinning '~a': ~s~%"
                    bundle-dir errors)
            (exit 1))
          (format #t "Re-pinned ~a (~a files)~%" bundle-dir (length pins)))))))

(define (main args)
  (let ((dirs (cdr args)))
    (if (null? dirs)
        (begin
          (format (current-error-port) "Usage: repin-bundle.scm <bundle-dir> [...]~%")
          (exit 1))
        (for-each repin-bundle! dirs))))

(main (command-line))
