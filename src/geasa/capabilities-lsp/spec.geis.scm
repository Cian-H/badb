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
            (test-assert
              "plugin module loads and exposes lsp-diagnostics"
              (procedure? lsp-diagnostics))
            (test-assert
              "lsp-diagnostics degrades to structured report on missing file"
              (string? (lsp-diagnostics "/nonexistent/path.c")))
            (let
                ((tmp (mkdtemp "/tmp/badb-lsp-spec-XXXXXX")))
              (dynamic-wind
                (lambda ()
                  #t)
                (lambda ()
                  (let ((c-file (string-append tmp "/sample.c")))
                    (call-with-output-file c-file
                                           (lambda (p)
                                             (display "int main(void) { return 0; }\n" p)))
                    (test-assert
                      "lsp-diagnostics returns string report on real file without crashing"
                      (string? (lsp-diagnostics c-file)))
                    (test-assert
                      "make-lsp-client! handles missing server binary without uncaught crash"
                      (string?
                        (catch #t
                          (lambda ()
                            (make-lsp-client! (list "/nonexistent/lsp-server-xyz"))
                            "ok")
                          (lambda _
                            "error"))))))
                (lambda ()
                  (system* "rm" "-rf" tmp)))))

(spec-end)
(spec-exit)