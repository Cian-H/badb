;;; (geasa capabilities-code-nav spec.geis) — Contract verification for Code Nav & Memory

(define-module (test test-capabilities-code-nav)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (badb kernel registry)
  #:use-module (geasa capabilities-code-nav plugin)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1))

(spec-begin "capabilities-code-nav")

(test-group "symbols-overview-extraction"
  (let* ((tmp-dir (mkdtemp "/tmp/badb-syms-test-XXXXXX"))
         (scm-file (string-append tmp-dir "/app.scm"))
         (py-file (string-append tmp-dir "/service.py"))
         (rs-file (string-append tmp-dir "/main.rs")))

    ;; Scheme fixture
    (call-with-output-file scm-file
      (lambda (p)
        (display ";;; Scheme fixture\n(define (calculate-total items)\n  (apply + items))\n\n(define-record-type <order>\n  (make-order id)\n  order?\n  (id order-id))\n" p)))

    ;; Python fixture
    (call-with-output-file py-file
      (lambda (p)
        (display "# Python fixture\nclass UserManager:\n    def get_user(self, uid):\n        return uid\n\ndef helper():\n    pass\n" p)))

    ;; Rust fixture
    (call-with-output-file rs-file
      (lambda (p)
        (display "// Rust fixture\nstruct AppConfig {\n    port: u16,\n}\n\nfn run_server() {\n}\n" p)))

    (let ((scm-syms (extract-symbols-from-file scm-file))
          (py-syms (extract-symbols-from-file py-file))
          (rs-syms (extract-symbols-from-file rs-file)))

      (test-assert "Scheme function calculate-total found"
        (any (lambda (s) (and (equal? (car s) "calculate-total") (eq? (cadr s) 'function))) scm-syms))
      (test-assert "Scheme record <order> found"
        (any (lambda (s) (and (equal? (car s) "<order>") (eq? (cadr s) 'record))) scm-syms))

      (test-assert "Python class UserManager found"
        (any (lambda (s) (and (equal? (car s) "UserManager") (eq? (cadr s) 'class))) py-syms))
      (test-assert "Python def get_user found"
        (any (lambda (s) (and (equal? (car s) "get_user") (eq? (cadr s) 'function))) py-syms))

      (test-assert "Rust struct AppConfig found"
        (any (lambda (s) (and (equal? (car s) "AppConfig") (eq? (cadr s) 'struct))) rs-syms))
      (test-assert "Rust fn run_server found"
        (any (lambda (s) (and (equal? (car s) "run_server") (eq? (cadr s) 'function))) rs-syms))

      (let ((overview (symbols-overview tmp-dir)))
        (test-assert "overview contains calculate-total" (string-contains overview "calculate-total"))
        (test-assert "overview contains UserManager" (string-contains overview "UserManager"))
        (test-assert "overview contains AppConfig" (string-contains overview "AppConfig"))))
    (system* "rm" "-rf" tmp-dir)))

(test-group "find-and-read-symbol"
  (let* ((tmp-dir (mkdtemp "/tmp/badb-find-test-XXXXXX"))
         (scm-file (string-append tmp-dir "/math.scm")))
    (call-with-output-file scm-file
      (lambda (p)
        (display "(define (add a b)\n  (+ a b))\n\n(define (multiply a b)\n  (* a b))\n" p)))

    (let ((find-res (find-symbol-in-workspace "multiply" tmp-dir)))
      (test-assert "find-symbol reports file and line"
        (and (string-contains find-res "math.scm")
             (string-contains find-res "multiply"))))

    (let ((body (read-symbol-from-file scm-file "multiply")))
      (test-assert "read-symbol retrieves exact body"
        (and (string-contains body "(define (multiply a b)")
             (string-contains body "(* a b))"))))
    (system* "rm" "-rf" tmp-dir)))

(test-group "replace-symbol-body"
  (let* ((tmp-dir (mkdtemp "/tmp/badb-replace-test-XXXXXX"))
         (scm-file (string-append tmp-dir "/code.scm")))
    (call-with-output-file scm-file
      (lambda (p)
        (display ";;; Header comment\n(define (greet name)\n  (string-append \"Hello, \" name))\n;;; Footer comment\n" p)))

    (let* ((new-body "(define (greet name)\n  (format #f \"Greetings, ~a!\" name))")
           (res (replace-symbol-in-file scm-file "greet" new-body))
           (updated-content (call-with-input-file scm-file get-string-all)))
      (test-assert "replace reports success" (string-contains res "Successfully updated symbol"))
      (test-assert "file has new body" (string-contains updated-content "Greetings, ~a!"))
      (test-assert "header comment preserved" (string-contains updated-content ";;; Header comment"))
      (test-assert "footer comment preserved" (string-contains updated-content ";;; Footer comment")))
    (system* "rm" "-rf" tmp-dir)))

(test-group "memory-tools"
  (let* ((tmp-dir (mkdtemp "/tmp/badb-mem-test-XXXXXX"))
         (w-res (memory-write "architecture" "# Arch\nUses Guile Scheme" #:workspace tmp-dir)))
    (test-assert "memory-write succeeds" (string-contains w-res "Saved memory 'architecture'"))

    (let ((content (memory-read "architecture" #:workspace tmp-dir)))
      (test-assert "memory-read returns written content" (string-contains content "Uses Guile Scheme")))

    (let ((list-res (memory-read "all" #:workspace tmp-dir)))
      (test-assert "memory-read 'all' lists architecture" (string-contains list-res "architecture")))
    (system* "rm" "-rf" tmp-dir)))

(test-group "code-nav-registration"
  (let ((reg (make-registry)))
    (register-code-nav-tools! reg)
    (test-assert "symbols_overview registered" (procedure? (registry-lookup reg "symbols_overview")))
    (test-assert "find_symbol registered" (procedure? (registry-lookup reg "find_symbol")))
    (test-assert "read_symbol registered" (procedure? (registry-lookup reg "read_symbol")))
    (test-assert "replace_symbol_body registered" (procedure? (registry-lookup reg "replace_symbol_body")))
    (test-assert "memory_read registered" (procedure? (registry-lookup reg "memory_read")))
    (test-assert "memory_write registered" (procedure? (registry-lookup reg "memory_write")))))

(spec-end)
(spec-exit)
