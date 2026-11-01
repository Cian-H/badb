#!/bin/sh
# scripts/migrate-geis.sh — Migrate one flat geis spec into a self-contained bundle.
#
# usage: scripts/migrate-geis.sh <feature-id>
#
# Creates geis/<feature-id>/ with the spec moved via git mv and a validated
# manifest.scm (bundle-version 1, sha256-pinned files, guile-module imports
# extracted from the spec's define-module).  Idempotence guard: refuses when
# the bundle already exists.
set -e

FID="$1"
if [ -z "$FID" ]; then
  echo "usage: $0 <feature-id>" >&2
  exit 1
fi
if [ ! -f "test/specs/$FID.geis.scm" ]; then
  echo "no such flat spec: test/specs/$FID.geis.scm" >&2
  exit 1
fi
if [ -d "geis/$FID" ]; then
  echo "bundle already exists: geis/$FID" >&2
  exit 1
fi

mkdir -p "geis/$FID"
git mv "test/specs/$FID.geis.scm" "geis/$FID/spec.geis.scm"

guile --no-auto-compile -L src -L . -c '
(use-modules (badb geis bundle) (ice-9 format))
(define fid (cadr (program-arguments)))
(define dir (string-append "geis/" fid))
(define imports
  (call-with-input-file (string-append dir "/spec.geis.scm")
    (lambda (p)
      (let loop ((acc (quote ())))
        (let ((form (read p)))
          (if (eof-object? form)
              (reverse acc)
              (if (and (pair? form) (eq? (car form) (quote define-module)))
                    (let scan ((rest (cdr form)) (acc acc))
                      (cond ((not (pair? rest)) (reverse acc))
                            ((eq? (car rest) (quote #:use-module))
                             (scan (cddr rest) (cons (cadr rest) acc)))
                            (else (scan (cdr rest) acc))))
                    (loop acc))))))))
(define manifest
  (append
   (list (cons (quote bundle-version) 1))
   (list (cons (quote feature-id) fid))
   (list (cons (quote version) "1.0.0"))
   (list (cons (quote spec) "spec.geis.scm"))
   (list (cons (quote depends)
           (list (cons (quote guile-modules) imports)
                 (cons (quote badb-version) "0.1.0"))))
   (list (cons (quote provenance)
           (list (cons (quote origin) "migrated-from-flat-spec"))))
   (list (cons (quote sha256) (compute-geis-sha256-pins dir)))))
(call-with-output-file (string-append dir "/manifest.scm")
  (lambda (p) (write manifest p) (newline p)))
(let ((errors (geis-manifest-valid? dir)))
  (if (null? errors)
      (format #t "migrated ~a; manifest valid~%" fid)
      (begin
        (format (current-error-port) "manifest invalid: ~a~%" errors)
        (exit 1))))
' "$FID"
