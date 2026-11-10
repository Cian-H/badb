;;; spec.geis.scm — Kernel contract for (geasa capabilities-macro plugin)
;;;
;;; Degraded-mode contract: expansion is pure text-in/text-out and never
;;; raises. Malformed or unreadable directives expand to visible `@error:'
;;; markers instead of unwinding. No filesystem writes, no subprocesses.

(define-module (test capabilities-macro-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa capabilities-macro plugin))

(spec-begin "capabilities-macro")

(test-group "macro-contract"
            (test-assert "plugin module loads and exposes expand-macros"
                         (procedure? expand-macros))

            (test-assert "literal @@ expands to a single @"
                         (string=? (expand-macros "@@") "@"))

            (test-assert "unknown shortcut passes through verbatim"
                         (string=? (expand-macros "hello @world" #:shortcuts '())
                                   "hello @world"))

            (test-assert "unreadable file degrades to @error marker"
                         (string-contains
                           (expand-macros "@file:/nonexistent/path-xyz")
                           "@error:"))

            (test-assert "empty file path degrades to @error marker"
                         (string-contains (expand-macros "@file:") "@error:"))

            (test-assert "expansion never raises on malformed input"
                         (string? (expand-macros "@@@ @@file: @nope"))))

(spec-end)
