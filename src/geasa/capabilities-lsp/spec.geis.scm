;;; spec.geis.scm — Kernel contract for (geasa capabilities-lsp plugin)
;;;
;;; Degraded-mode contract: with no language server binary present the
;;; bundle must still load, register its tool, and report diagnostics
;;; unavailability as a value (never an unwind).

(define-module (test capabilities-lsp-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa capabilities-lsp plugin))

(spec-begin "capabilities-lsp")

(test-group "lsp-contract"
            (test-assert "plugin module loads and exposes lsp-diagnostics"
                         (procedure? lsp-diagnostics))

            (test-assert "lsp-diagnostics degrades to structured report"
                         (string? (lsp-diagnostics "/nonexistent/path.c"))))

(spec-end)
