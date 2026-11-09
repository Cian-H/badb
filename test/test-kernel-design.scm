;;; test/test-kernel-design.scm — Architectural Invariants & Microkernel Boundary Tests
;;;
;;; Formally enforces harness-native microkernel design principles:
;;;   1. Epistemic Purity: Zero domain vocabulary leakage (prompts, models, tokens, etc.)
;;;   2. Syntactic Mechanism: Zero macro/DSL definitions in Ring 0
;;;   3. Dependency Purity: Zero user-space or dynamic plugin imports
;;;   4. Frozen ABI Surface: Strict allowlist of permitted system-call exports
;;;   5. Mutation Surface Area: Strict limit on mutating procedures (ending in !)
;;;   6. Implementation Density: Bounded AST nodes per exported capability
;;;   7. Cyclomatic Complexity: Enforced branch density ceilings (McCabe bounds)

(define-module (test test-kernel-design)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim))

;;;
;;; Helpers for S-expression AST analysis
;;;

(define %kernel-dir
  (let* ((script-dir (and (current-filename) (dirname (current-filename))))
         (base (if script-dir
                   (canonicalize-path (string-append script-dir "/../src/badb/kernel"))
                   "src/badb/kernel")))
    (if (file-exists? base)
        base
        (canonicalize-path "src/badb/kernel"))))

(define (read-file-forms path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((acc '()))
        (let ((form (read port)))
          (if (eof-object? form)
              (reverse acc)
              (loop (cons form acc))))))))

(define (count-ast-nodes x)
  (cond
    ((pair? x) (+ 1 (count-ast-nodes (car x)) (count-ast-nodes (cdr x))))
    (else 1)))

(define (ast-tree-depth x)
  (cond
    ((list? x) (+ 1 (apply max (cons 0 (map ast-tree-depth x)))))
    ((pair? x) (+ 1 (max (ast-tree-depth (car x)) (ast-tree-depth (cdr x)))))
    (else 0)))

(define (extract-symbols form)
  (cond
    ((symbol? form) (list form))
    ((pair? form) (append (extract-symbols (car form)) (extract-symbols (cdr form))))
    (else '())))

(define (calc-cyclomatic-complexity form)
  (let ((branches 0))
    (define (walk node)
      (when (pair? node)
        (let ((head (car node)))
          (cond
            ((eq? head 'if)
             (set! branches (+ branches 1)))
            ((eq? head 'cond)
             (set! branches (+ branches (max 0 (- (length (cdr node)) 1)))))
            ((memq head '(and or))
             (set! branches (+ branches (max 0 (- (length (cdr node)) 1)))))
            ((memq head '(case match))
             (set! branches (+ branches (max 0 (- (length (cdr node)) 1)))))
            ((memq head '(catch with-exception-handler))
             (set! branches (+ branches 1)))))
        (walk (car node))
        (walk (cdr node))))
    (walk form)
    (+ 1 branches)))

(define (extract-module-exports forms)
  (let ((mod-form (find (lambda (f) (and (pair? f) (eq? (car f) 'define-module))) forms)))
    (if mod-form
        (let loop ((rest (cdr mod-form)) (acc '()))
          (cond
            ((null? rest) acc)
            ((and (eq? (car rest) #:export) (pair? (cdr rest)))
             (loop (cddr rest) (append acc (cadr rest))))
            (else (loop (cdr rest) acc))))
        '())))

(define (extract-module-imports forms)
  (let ((imports '()))
    (define (scan item)
      (when (pair? item)
        (cond
          ((eq? (car item) 'define-module)
           (let loop ((rest (cdr item)))
             (cond
               ((null? rest) #f)
               ((and (eq? (car rest) #:use-module) (pair? (cdr rest)))
                (set! imports (cons (cadr rest) imports))
                (loop (cddr rest)))
               ((and (eq? (car rest) #:autoload) (pair? (cdr rest)))
                (set! imports (cons (cadr rest) imports))
                (loop (if (> (length rest) 2) (cdddr rest) '())))
               (else (loop (cdr rest))))))
          ((eq? (car item) 'use-modules)
           (for-each (lambda (m) (set! imports (cons m imports))) (cdr item)))
          (else
           (scan (car item))
           (scan (cdr item))))))
    (scan forms)
    imports))

(define %kernel-files
  (map (lambda (f) (string-append %kernel-dir "/" f))
       (sort (scandir %kernel-dir (lambda (n) (string-suffix? ".scm" n))) string<?)))

(define %kernel-file-data
  (map (lambda (path)
         (let* ((forms (read-file-forms path))
                (exports (extract-module-exports forms))
                (imports (extract-module-imports forms))
                (nodes (apply + (map count-ast-nodes forms))))
           (list (basename path) forms exports imports nodes)))
       %kernel-files))

;;;
;;; Frozen Kernel ABI Manifest (Explicit Capability Contract)
;;;
(define %approved-kernel-abi
  '(;; digest.scm
    sha256-hex

    ;; geis-gate.scm
    geis-gate-root geis-gate-dir sanitize-feature-id locked-geis-path
    lock-geis verify-locked-geis geis-gate-transaction-pins geis-gate-ast-diff-hash
    lock-geis-bundle verify-locked-geis-bundle
    ;; legacy aliases in geis-gate.scm
    spec-gate-root spec-gate-dir locked-spec-path lock-spec
    verify-locked-spec spec-gate-transaction-pins spec-gate-ast-diff-hash

    ;; ipc.scm
    <ipc-channel> make-ipc-channel ipc-channel? ipc-channel-in-port
    ipc-channel-out-port ipc-channel-pid ipc-channel-closed? ipc-channel-close!
    ipc-send-request! ipc-send-notification! ipc-read-message ipc-call
    ipc-result-response ipc-error-response ipc-write-response!

    ;; journal.scm
    make-journal journal? journal-path journal-append! journal-entries
    journal-latest journal-count journal-entry->json-string json-string->journal-entry
    journal-verify-integrity journal-entry-ref safe-journal-append!
    kernel-ensure-string kernel-mkdir-p kernel-prop-ref kernel-ensure-parent-directory!
    <journal-entry> make-journal-entry journal-entry? journal-entry-timestamp
    journal-entry-iteration journal-entry-feature-id journal-entry-commit
    journal-entry-test-verdict journal-entry-author journal-entry-details
    journal-entry-ast-diff-hash journal-entry-spec-hash journal-entry-quickcheck-seed
    journal-entry-quickcheck-verdict journal-entry-kernel-replay-verdict

    ;; registry.scm
    <registry> make-registry registry? registry-attach! registry-detach!
    registry-lookup registry-metadata registry-list registry-count
    registry-dispatch make-fresh-feature-module eval-in-feature-module
    feature-module-define! feature-module-ref

    ;; transaction.scm
    <transaction-manager> make-transaction-manager transaction-manager?
    transaction-manager-registry transaction-manager-journal stage-patch
    eval-patch apply-transaction!))

(define %approved-mutating-exports
  '(ipc-channel-close!
    ipc-send-request!
    ipc-send-notification!
    ipc-write-response!
    journal-append!
    safe-journal-append!
    kernel-ensure-parent-directory!
    registry-attach!
    registry-detach!
    feature-module-define!
    apply-transaction!))

;;;
;;; Test Suite: Kernel Architectural Invariants
;;;

(test-begin "kernel-design")

;; ----------------------------------------------------------------------------
;; 1. Whitelist of Compulsory Kernel Modules
;; ----------------------------------------------------------------------------
(test-group "kernel-whitelist"
  (test-equal "kernel directory contains only approved core modules"
    '("digest.scm" "geis-gate.scm" "ipc.scm" "journal.scm" "registry.scm" "transaction.scm")
    (map car %kernel-file-data)))

;; ----------------------------------------------------------------------------
;; 2. Epistemic Purity: Zero Domain Vocabulary in Ring 0
;; ----------------------------------------------------------------------------
(test-group "kernel-epistemic-purity"
  (let* ((domain-lexicon '(prompt agent persona model token chat workflow tui))
         (leaks '()))
    (for-each
      (lambda (entry)
        (let* ((filename (car entry))
               (forms (cadr entry))
               (symbols (delete-duplicates (apply append (map extract-symbols forms)))))
          (for-each
            (lambda (term)
              (let ((term-str (symbol->string term)))
                (when (any (lambda (s) (string-contains (symbol->string s) term-str)) symbols)
                  (set! leaks (cons (cons filename term) leaks)))))
            domain-lexicon)))
      %kernel-file-data)

    (test-equal "zero domain vocabulary leaks into kernel AST"
      '()
      leaks)))

;; ----------------------------------------------------------------------------
;; 3. Syntactic Mechanism Purity: Zero Macro / DSL Definitions in Ring 0
;; ----------------------------------------------------------------------------
(test-group "kernel-macro-purity"
  (let ((macro-defs '()))
    (for-each
      (lambda (entry)
        (let ((filename (car entry))
              (forms (cadr entry)))
          (define (scan form)
            (when (pair? form)
              (when (memq (car form) '(define-syntax defmacro define-macro syntax-rules syntax-case))
                (set! macro-defs (cons (list filename (car form)) macro-defs)))
              (scan (car form))
              (scan (cdr form))))
          (scan forms)))
      %kernel-file-data)

    (test-equal "zero macro or DSL definitions in kernel (pure runtime mechanisms only)"
      '()
      macro-defs)))

;; ----------------------------------------------------------------------------
;; 4. Dependency Boundary: Zero User-Space or Plugin Imports
;; ----------------------------------------------------------------------------
(test-group "kernel-dependency-boundary"
  (let ((forbidden-prefixes '(geasa))
        (forbidden-badb-subsystems '(providers runtimes capabilities surfaces policies geis))
        (violations '()))
    (for-each
      (lambda (entry)
        (let ((filename (car entry))
              (imports (cadddr entry)))
          (for-each
            (lambda (imp)
              (when (pair? imp)
                (let ((root (car imp)))
                  (when (memq root forbidden-prefixes)
                    (set! violations (cons (list filename imp) violations)))
                  (when (and (eq? root 'badb)
                             (> (length imp) 1)
                             (memq (cadr imp) forbidden-badb-subsystems))
                    (set! violations (cons (list filename imp) violations))))))
            imports)))
      %kernel-file-data)

    (test-equal "kernel imports zero user-space or plugin subsystems"
      '()
      violations)))

;; ----------------------------------------------------------------------------
;; 5. Public ABI Surface Area: Frozen Capability Contract
;; ----------------------------------------------------------------------------
(test-group "kernel-public-abi"
  (let* ((actual-exports (apply append (map caddr %kernel-file-data)))
         (unapproved-exports
          (filter (lambda (sym) (not (memq sym %approved-kernel-abi))) actual-exports)))

    (test-equal "all exported kernel procedures belong to approved ABI manifest"
      '()
      unapproved-exports)

    (test-assert "total exported kernel symbols is strictly bounded (<= 86)"
      (<= (length actual-exports) 86))))

;; ----------------------------------------------------------------------------
;; 6. State Mutation Surface: Strictly Bounded Mutation Endpoints
;; ----------------------------------------------------------------------------
(test-group "kernel-mutation-surface"
  (let* ((actual-exports (apply append (map caddr %kernel-file-data)))
         (mutating-exports (filter (lambda (s) (string-suffix? "!" (symbol->string s))) actual-exports))
         (unauthorized-mutators
          (filter (lambda (s) (not (memq s %approved-mutating-exports))) mutating-exports)))

    (test-equal "mutating procedures are strictly confined to authorized state transitions"
      '()
      unauthorized-mutators)

    (test-assert "number of mutating exports does not expand (<= 11)"
      (<= (length mutating-exports) (length %approved-mutating-exports)))))

;; ----------------------------------------------------------------------------
;; 7. Implementation Density: Bounded AST Nodes per Primitive
;; ----------------------------------------------------------------------------
(test-group "kernel-implementation-density"
  (for-each
    (lambda (entry)
      (let* ((filename (car entry))
             (exports (caddr entry))
             (nodes (car (cddddr entry)))
             (num-exports (length exports))
             (density (if (> num-exports 0) (/ nodes (* 1.0 num-exports)) 0)))
        (cond
          ;; digest.scm is an exempt mathematical algorithm with static lookup tables
          ((string=? filename "digest.scm")
           (test-assert "digest.scm exports valid SHA-256 primitives" (> num-exports 0)))
          ;; Lean core mechanism modules: density must stay <= 150 nodes/export
          ((member filename '("ipc.scm" "registry.scm" "journal.scm"))
           (test-assert (format #f "~a implementation density (<= 150 nodes/export)" filename)
             (<= density 150.0)))
          ;; Supervisory & orchestration modules: density must stay <= 350 nodes/export
          ((member filename '("transaction.scm" "geis-gate.scm"))
           (test-assert (format #f "~a implementation density (<= 350 nodes/export)" filename)
             (<= density 350.0))))))
    %kernel-file-data))

;; ----------------------------------------------------------------------------
;; 8. Cyclomatic Complexity: Enforced McCabe Ceilings
;; ----------------------------------------------------------------------------
(test-group "kernel-cyclomatic-complexity"
  (let* ((legacy-high-cc-procs
          '(verify-locked-geis-bundle
            verify-locked-geis
            %drain-ports-with-timeout!
            %cross-check-verdict
            kernel-prop-ref
            journal-verify-integrity
            sanitize-for-json
            format-exception
            resolve-implementation
            apply-transaction!
            ipc-call
            read-last-journal-entry
            kernel-mkdir-p
            ensure-forms
            geis-gate-transaction-pins
            next-iteration
            journal-entries
            %delete-tree!
            %reap-process
            invoke-test-thunk))
         (violations '()))

    (for-each
      (lambda (entry)
        (let ((filename (car entry))
              (forms (cadr entry)))
          (for-each
            (lambda (f)
              (when (and (pair? f) (memq (car f) '(define define* define-public)))
                (let* ((head (cadr f))
                       (name (cond
                               ((pair? head) (car head))
                               ((symbol? head) head)
                               (else 'anonymous)))
                       (cc (calc-cyclomatic-complexity f)))
                  (if (memq name legacy-high-cc-procs)
                      ;; Grandfathered legacy procedures must not exceed CC <= 22
                      (when (> cc 22)
                        (set! violations (cons (list filename name cc 'exceeds-legacy-cap) violations)))
                      ;; All other/new procedures must strictly satisfy CC <= 10
                      (when (> cc 10)
                        (set! violations (cons (list filename name cc 'exceeds-mccabe-cap) violations)))))))
            forms)))
      %kernel-file-data)

    (test-equal "procedure cyclomatic complexity satisfies McCabe bounds"
      '()
      violations)))

(test-end "kernel-design")
