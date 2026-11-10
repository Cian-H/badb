;;; spec.geis.scm — Kernel contract for (geasa capabilities-treesitter plugin)
;;;
;;; Degraded-mode contract: with no tree-sitter grammar libraries
;;; present the bundle must still load, register its tools, and
;;; report parser unavailability as a value (never an unwind).

(define-module (test capabilities-treesitter-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec))

(spec-begin "capabilities-treesitter")

(test-group "treesitter-contract"
            (test-assert "bundle plugin module resolves"
                         (resolve-module '(geasa capabilities-treesitter plugin)))

            (test-assert "manifest declares spec file present in bundle"
                         (file-exists?
                           "src/geasa/capabilities-treesitter/spec.geis.scm")))

(spec-end)
