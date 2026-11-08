;;; test/test-token-efficiency.scm — Quantitative Token Efficiency Benchmarks
;;;
;;; Verifies the core thesis: badb's self-modifying, context-managed harness
;;; achieves dramatic, measurable token savings and strict prefix cache invariance.

(define-module (test test-token-efficiency)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (badb kernel registry)
  #:use-module (geasa runtimes-context plugin)
  #:use-module (geasa runtimes-prompt plugin)
  #:use-module (geasa runtimes-react plugin)
  #:use-module (geasa capabilities-context-index plugin)
  #:use-module (geasa capabilities-code-nav plugin)
  #:use-module (geasa capabilities-fs-tools plugin)
  #:use-module (json))

(test-begin "token-efficiency")

(define (estimate-tool-tokens tools)
  (let ((vec (if (vector? tools) tools (list->vector tools))))
    (char-heuristic-estimator (scm->json-string vec))))

;;;
;;; Test 1: Prefix-Cache Invariance
;;; Turns 0..N-1 remain byte-for-byte identical across steps despite ephemeral recitation.
;;;
(test-group "test-1-prefix-cache-invariance"
  (let* ((initial-messages
          (list '((role . "system") (content . "You are badb."))
                '((role . "user") (content . "Fix the failing test in math-utils."))))
         (session
          (make-react-session #:messages initial-messages
                              #:recitation? #t))
         ;; Simulate Step 1 outbound wire payload with ephemeral recitation
         (step-1-recitation (react-recitation-text session))
         (step-1-outbound
          (append (react-session-messages session)
                  (list `((role . "user") (content . ,step-1-recitation)))))
         ;; Step 2: session advances with assistant response and tool feedback
         (_ (begin
              (set-react-session-iteration! session 2)
              (set-react-session-messages!
               session
               (append (react-session-messages session)
                       (list '((role . "assistant") (content . "Investigating test failure..."))
                             '((role . "tool") (content . "PASS")))))))
         ;; Simulate Step 2 outbound wire payload with updated recitation
         (step-2-recitation (react-recitation-text session))
         (step-2-outbound
          (append (react-session-messages session)
                  (list `((role . "user") (content . ,step-2-recitation))))))

    ;; The first two turns (system and user prompt) MUST remain byte-identical
    (test-equal "turns 0..1 byte-identical across steps"
      (take step-1-outbound 2)
      (take step-2-outbound 2))

    ;; Persistent session messages never contain leaked ephemeral recitation turns
    (test-equal "session messages contains exactly 4 persistent turns"
      4
      (length (react-session-messages session)))

    (test-equal "turn 0 role is system"
      "system"
      (assoc-ref (list-ref (react-session-messages session) 0) 'role))
    (test-equal "turn 1 role is user"
      "user"
      (assoc-ref (list-ref (react-session-messages session) 1) 'role))
    (test-equal "turn 2 role is assistant"
      "assistant"
      (assoc-ref (list-ref (react-session-messages session) 2) 'role))
    (test-equal "turn 3 role is tool"
      "tool"
      (assoc-ref (list-ref (react-session-messages session) 3) 'role))))

;;;
;;; Test 2: Deferred Tool Schema Compression
;;; >= 55% reduction in outbound tool tokens when using deferred tool partition.
;;;
(test-group "test-2-deferred-tool-schema-compression"
  (let* ((tools (map (lambda (i)
                       `((name . ,(format #f "tool_~a" i))
                         (description . ,(format #f "Description for tool number ~a with parameter details and schema" i))
                         (parameters .
                          ((type . "object")
                           (properties .
                            ((path . ((type . "string") (description . "Path to input file to inspect")))
                             (mode . ((type . "string") (description . "Operation mode (verbose, quiet, raw)")))
                             (timeout . ((type . "integer") (description . "Timeout in milliseconds for the operation")))))
                           (required . #("path"))))))
                     (iota 25)))
         ;; Uncompressed: all 25 tools serialized in full
         (uncompressed-tokens (estimate-tool-tokens tools))
         ;; Compressed: partitioned with deferred? #t and 4 core tools
         (core-names '("tool_0" "tool_1" "tool_2" "tool_3")))
    (call-with-values
      (lambda ()
        (partition-tools-for-step tools core-names '() #t))
      (lambda (hydrated deferred)
        (let* ((compressed-tokens (estimate-tool-tokens hydrated))
               (reduction (/ (- uncompressed-tokens compressed-tokens)
                             (* 1.0 uncompressed-tokens))))
          (test-assert "deferred list has 21 tools" (= 21 (length deferred)))
          (test-assert "hydrated list has 5 tools (4 core + 1 tool_search)" (= 5 (length hydrated)))
          (test-assert "tool_search exists in hydrated tools"
            (and (find (lambda (t) (string=? "tool_search" (assoc-ref t 'name))) hydrated) #t))
          (test-assert "tool schema compression >= 55%"
            (>= reduction 0.55)))))))

;;;
;;; Test 3: Semantic Navigation Compression
;;; >= 80% reduction vs full file reads/writes on 500-line module.
;;;
(test-group "test-3-semantic-navigation-compression"
  (let* ((tmp-dir (mkdtemp "/tmp/badb-test-semnav-XXXXXX"))
         (file-path (string-append tmp-dir "/module.scm"))
         ;; Generate 500+ lines of Scheme code (~65 functions)
         (code-lines
          (append
           (list "(define-module (sample math-module)"
                 "  #:export (compute-1 compute-25))"
                 "")
           (append-map
            (lambda (i)
              (list (format #f ";;; Function ~a description" i)
                    (format #f "(define (compute-~a x y)" i)
                    "  \"Compute mathematical result for given inputs.\""
                    "  (let* ((step-a (+ x y))"
                    "         (step-b (* step-a 2))"
                    "         (step-c (- step-b x)))"
                    (format #f "    (if (> step-c 100) (+ step-c ~a) (- step-c ~a))))" i i)
                    ""))
            (iota 65 1))))
         (full-content (string-join code-lines "\n")))

    (call-with-output-file file-path
      (lambda (p) (display full-content p)))

    ;; Baseline: agent must read whole file and write whole file back
    (let* ((baseline-read-chars (string-length full-content))
           (baseline-write-chars (string-length full-content))
           (baseline-total-chars (+ baseline-read-chars baseline-write-chars))

           ;; Semantic navigation: symbols overview + targeted read + surgical replacement
           (outline (symbols-overview file-path))
           (target-sym "compute-25")
           (sym-def (read-symbol-from-file file-path target-sym))
           (new-body "(define (compute-25 x y)\n  \"Optimized implementation.\"\n  (+ x y 42))")
           (mod-res (replace-symbol-in-file file-path target-sym new-body))
           (semantic-total-chars (+ (string-length outline)
                                    (string-length sym-def)
                                    (string-length new-body)))
           (reduction (/ (- baseline-total-chars semantic-total-chars)
                         (* 1.0 baseline-total-chars))))

      (test-assert "symbols-overview found target symbol"
        (and (string-contains outline "compute-25") #t))
      (test-assert "read-symbol extracted target function"
        (and (string-contains sym-def "compute-25") #t))
      (test-assert "replace-symbol modified the file"
        (and (string-contains mod-res "Successfully updated")
             (string-contains (call-with-input-file file-path get-string-all) "Optimized implementation.")))
      (test-assert "semantic navigation compression >= 80%"
        (>= reduction 0.80)))

    (system* "rm" "-rf" tmp-dir)))

;;;
;;; Test 4: Virtualized Output Compression
;;; >= 90% reduction on 60 KB test log.
;;;
(test-group "test-4-virtualized-output-compression"
  (let* ((idx (make-bm25-index))
         ;; Generate 60+ KB of test log output (>1,200 lines)
         (log-lines
          (append
           (map (lambda (i)
                  (format #f "PASS [suite-~a] test_case_~a completed in 0.002s (all assertions passed successfully)"
                          (modulo i 10) i))
                (iota 1000))
           (list "FAIL [suite-critical] test_edge_case failed at math.scm:42: assertion (= result 42) failed with 0")
           (map (lambda (i)
                  (format #f "PASS [suite-cleanup-~a] test_cleanup_~a completed in 0.001s (cleanup verified)"
                          (modulo i 5) i))
                (iota 200))))
         (raw-log (string-join log-lines "\n"))
         (raw-bytes (string-length raw-log)))

    (test-assert "test log is at least 60 KB"
      (>= raw-bytes 60000))

    ;; Virtualize output via make-ctx-offload-hook
    (let* ((hook (make-ctx-offload-hook idx))
           (receipt (hook "test.log" "run_tests" raw-log))
           (search-result (ctx-search idx "FAIL assertion"))
           (returned-to-context (+ (string-length receipt) (string-length search-result)))
           (reduction (/ (- raw-bytes returned-to-context)
                         (* 1.0 raw-bytes))))

      (test-assert "receipt indicates output was virtualized"
        (and (or (string-contains receipt "Virtualizing output")
                 (string-contains receipt "virtualized")
                 (string-contains receipt "ctx_search"))
             #t))
      (test-assert "search result finds the failure"
        (and (string-contains search-result "FAIL [suite-critical]") #t))
      (test-assert "virtualized output compression >= 90%"
        (>= reduction 0.90)))))

;;;
;;; Test 5: Cumulative Wire Savings
;;; >= 60% reduction across a 4-turn multi-step coding task.
;;;
(test-group "test-5-cumulative-wire-savings"
  (let* ((tools (map (lambda (i)
                       `((name . ,(format #f "tool_~a" i))
                         (description . ,(format #f "Description for tool number ~a with parameter details" i))
                         (parameters .
                          ((type . "object")
                           (properties .
                            ((path . ((type . "string") (description . "Path to input file")))
                             (mode . ((type . "string") (description . "Operation mode")))))
                           (required . #("path"))))))
                     (iota 25)))
         (core-names '("tool_0" "tool_1" "tool_2" "tool_3"))
         (b-tools-tok (estimate-tool-tokens tools)))
    (call-with-values
      (lambda () (partition-tools-for-step tools core-names '() #t))
      (lambda (hydrated deferred)
        (let* ((o-tools-tok (estimate-tool-tokens hydrated))
               (composer (make-prompt-composer))
               (sys-static (assemble-system-prompt composer #:static-only? #t))
               (sys-full (assemble-system-prompt composer #:static-only? #f))
               (tmp-dir (mkdtemp "/tmp/badb-test-wire-XXXXXX"))
               (file-path (string-append tmp-dir "/module.scm"))
               (code-lines
                (append
                 (list "(define-module (sample math-module)"
                       "  #:export (compute-1 compute-25))"
                       "")
                 (append-map
                  (lambda (i)
                    (list (format #f "(define (compute-~a x y)" i)
                          "  \"Docstring for function.\""
                          (format #f "  (let ((z (+ x y ~a))) (* z 2)))" i)
                          ""))
                  (iota 65 1))))
               (full-code (string-join code-lines "\n"))
               (_ (call-with-output-file file-path (lambda (p) (display full-code p))))
               (log-lines
                (append
                 (map (lambda (i) (format #f "PASS test_~a in 0.002s" i)) (iota 1200))
                 (list "FAIL [suite-critical] test_edge_case failed at math.scm:42")
                 (map (lambda (i) (format #f "PASS cleanup_~a in 0.001s" i)) (iota 200))))
               (raw-log (string-join log-lines "\n"))
               (idx (make-bm25-index))
               (hook (make-ctx-offload-hook idx))
               (receipt (hook "test.log" "run_tests" raw-log))
               (search-res (ctx-search idx "FAIL"))
               (sym-def (read-symbol-from-file file-path "compute-25"))
               (new-sym-def "(define (compute-25 x y)\n  (+ x y 42))")
               (edit-res (replace-symbol-in-file file-path "compute-25" new-sym-def))
               (_ (system* "rm" "-rf" tmp-dir))

               ;; Baseline 4-turn wire messages:
               (b-turn-1 (list `((role . "system") (content . ,sys-full))
                               '((role . "user") (content . "Fix failing test in math-utils."))))
               (b-turn-2 (append b-turn-1
                                 (list '((role . "assistant") (content . "Running test suite"))
                                       `((role . "tool") (content . ,raw-log)))))
               (b-turn-3 (append b-turn-2
                                 (list '((role . "assistant") (content . "Inspecting full file module.scm"))
                                       `((role . "tool") (content . ,full-code)))))
               (b-turn-4 (append b-turn-3
                                 (list `((role . "assistant") (content . ,(string-append "Rewriting entire file module.scm:\n" full-code)))
                                       '((role . "tool") (content . "File written successfully")))))
               (b-wire-1 (+ (estimate-conversation-tokens b-turn-1) b-tools-tok))
               (b-wire-2 (+ (estimate-conversation-tokens b-turn-2) b-tools-tok))
               (b-wire-3 (+ (estimate-conversation-tokens b-turn-3) b-tools-tok))
               (b-wire-4 (+ (estimate-conversation-tokens b-turn-4) b-tools-tok))
               (baseline-total-tokens (+ b-wire-1 b-wire-2 b-wire-3 b-wire-4))

               ;; Badb optimized 4-turn wire messages:
               (o-turn-1 (list `((role . "system") (content . ,sys-static))
                               '((role . "user") (content . "Fix failing test in math-utils."))))
               (o-turn-2 (append o-turn-1
                                 (list '((role . "assistant") (content . "Running test suite"))
                                       `((role . "tool") (content . ,receipt)))))
               (o-turn-3 (append o-turn-2
                                 (list '((role . "assistant") (content . "Search error & inspect symbol"))
                                       `((role . "tool") (content . ,(string-append search-res "\n" sym-def))))))
               (o-turn-4 (append o-turn-3
                                 (list `((role . "assistant") (content . ,(string-append "Updating symbol compute-25:\n" new-sym-def)))
                                       `((role . "tool") (content . ,edit-res)))))
               (o-wire-1 (+ (estimate-conversation-tokens o-turn-1) o-tools-tok))
               (o-wire-2 (+ (estimate-conversation-tokens o-turn-2) o-tools-tok))
               (o-wire-3 (+ (estimate-conversation-tokens o-turn-3) o-tools-tok))
               (o-wire-4 (+ (estimate-conversation-tokens o-turn-4) o-tools-tok))
               (optimized-total-tokens (+ o-wire-1 o-wire-2 o-wire-3 o-wire-4))

               (cumulative-reduction (/ (- baseline-total-tokens optimized-total-tokens)
                                        (* 1.0 baseline-total-tokens))))

          (test-assert "measured cumulative tokens on wire >= 60% reduction"
            (>= cumulative-reduction 0.60)))))))

(test-end "token-efficiency")
