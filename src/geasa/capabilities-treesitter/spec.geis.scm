;;; spec.geis.scm — Kernel contract for (geasa capabilities-treesitter plugin)
;;;
;;; Degraded-mode contract: with no tree-sitter grammar libraries
;;; present the bundle must still load, register its tools, and
;;; report parser unavailability as a value (never an unwind).

(define-module
  (test capabilities-treesitter-spec) #:use-module
  (srfi srfi-64)
  #:use-module
  (badb spec)
  #:use-module
  (geasa capabilities-treesitter plugin))

(spec-begin "capabilities-treesitter")

(test-group "treesitter-contract"
            (test-assert "ts-available? returns boolean" (boolean? (ts-available?)))
            (test-equal
              "list-supported-languages returns expected set"
              '(python rust c typescript)
              (list-supported-languages))
            (test-assert
              "parse-python produces module CST in fallback mode"
              (let
                  ((cst (parse-python "def hello():\n    return 1\n")))
                (and (pair? cst) (eq? (car cst) 'module))))
            (test-assert "parse-rust produces CST pair" (pair? (parse-rust "fn main() {}")))
            (test-assert "parse-c produces CST pair"
                         (pair? (parse-c "int main(void) { return 0; }")))
            (test-assert "parse-typescript produces CST pair"
                         (pair? (parse-typescript "function f(): number { return 1; }")))
            (test-error
              "parse-language rejects unsupported language"
              #t
              (parse-language "brainfuck" "+++"))
            (test-equal
              "cst->sexp preserves nested list structure"
              '(module (function-def "hello"))
              (cst->sexp '(module (function-def "hello")))))

(spec-end)
(spec-exit)