;;; (geasa agents-subagent plugin) — Sub-Agent Task Specialization
;;;
;;; Spawns isolated, disposable ReAct sub-agents with filtered read-only tool
;;; permissions to investigate code, search indexes, or run parallel queries.
;;; Trajectories are offloaded to disk, returning only the synthesized answer
;;; to eliminate context token waste.

(define-module (geasa agents-subagent plugin)
               #:use-module (badb kernel registry)
               #:use-module (geasa runtimes-react plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (json)
               #:use-module (ice-9 threads)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:use-module (uuid generate)
               #:use-module (uuid utils)
               #:export (register-subagent-tools!
                          run-subagent
                          run-subagents-parallel
                          %default-investigate-tools
                          %investigate-tool-schema
                          %investigate-parallel-tool-schema))

(define %default-investigate-tools
        '("read_file" "fs-read-file" "ctx_search" "find_symbol" "symbols_overview"
          "skill_load" "skill_list"))

(define* (run-subagent prompt
                       #:key
                       (client #f)
                       (parent-reg #f)
                       (allowed-tools %default-investigate-tools)
                       (max-iterations 8)
                       (journal-dir ".badb/journal/subagents"))
         "Execute an isolated ReAct sub-agent with ALLOWED-TOOLS and return only the final answer."
         (let* ((sub-id (uuid->string (generate-uuid 'uuidv4)))
                (sub-reg (make-registry))
                (_ (when parent-reg
                     (for-each
                       (lambda (t-name)
                         (let ((impl (registry-lookup parent-reg t-name))
                               (meta (registry-metadata parent-reg t-name)))
                           (when impl
                             (registry-attach! sub-reg t-name impl #:metadata meta))))
                       (or allowed-tools %default-investigate-tools))))
                (sys-prompt
                  "You are a focused research sub-agent. Analyze files and query search tools to answer the user request directly, concisely, and factually. Do not provide fluff.")
                (session (make-react-session client
                                             #:initial-messages
                                             `(((role . "system") (content . ,sys-prompt))
                                               ((role . "user") (content . ,prompt)))
                                             #:reg sub-reg
                                             #:max-iterations max-iterations)))
           (call-with-values
             (lambda () (react-run! session))
             (lambda (status content final-session)
               ;; Dump trajectory to journal-dir
               (catch #t
                 (lambda ()
                   (mkdir-p journal-dir)
                   (let ((traj-file (format #f "~a/~a.jsonl" journal-dir sub-id)))
                     (call-with-output-file traj-file
                                            (lambda (p)
                                              (for-each
                                                (lambda (msg)
                                                  (display (safe-json-string msg) p)
                                                  (newline p))
                                                (react-session-messages final-session))))))
                 (lambda _ #f))
               (or content (format #f "[Sub-agent finished with status: ~a]" status))))))

(define* (run-subagents-parallel queries
                                 #:key
                                 (client #f)
                                 (parent-reg #f)
                                 (allowed-tools %default-investigate-tools)
                                 (max-iterations 8)
                                 (journal-dir ".badb/journal/subagents"))
         "Execute multiple subagents in parallel across QUERIES and collect formatted findings."
         (let ((results
                 (par-map
                   (lambda (q)
                     (let ((ans (run-subagent q
                                              #:client client
                                              #:parent-reg parent-reg
                                              #:allowed-tools allowed-tools
                                              #:max-iterations max-iterations
                                              #:journal-dir journal-dir)))
                       (format #f "### Query: ~a\n~a" q ans)))
                   queries)))
           (string-join results "\n\n")))

(define %investigate-tool-schema
        `((name . "investigate")
          (description . "Spawn a disposable sub-agent to research or inspect files with 0 context pollution. Returns only findings.")
          (parameters .
                      ((type . "object")
                       (properties .
                                   ((prompt . ((type . "string") (description . "Research question for the sub-agent")))
                                    (allowed_tools . ((type . "array") (items . ((type . "string"))) (description . "Tool names to grant")))
                                    (max_iterations . ((type . "integer") (description . "Max iterations (default: 8)")))))
                       (required . #("prompt"))))))

(define %investigate-parallel-tool-schema
        `((name . "investigate_parallel")
          (description . "Run multiple disposable sub-agents concurrently to investigate independent questions.")
          (parameters .
                      ((type . "object")
                       (properties .
                                   ((queries . ((type . "array") (items . ((type . "string"))) (description . "List of questions to investigate")))
                                    (max_iterations . ((type . "integer") (description . "Max iterations per sub-agent (default: 8)")))))
                       (required . #("queries"))))))

(define* (register-subagent-tools! reg
                                   #:key
                                   (client #f)
                                   (journal-dir ".badb/journal/subagents")
                                   (default-allowed-tools %default-investigate-tools))
         "Register 'investigate' and 'investigate_parallel' tools in REG."
         (registry-attach!
           reg
           "investigate"
           (lambda (args)
             (with-tool-args args ((prompt prompt "")
                                   (tools-raw allowed_tools #f)
                                   (max-iter max_iterations 8))
                             (let ((tools (cond
                                            ((vector? tools-raw) (vector->list tools-raw))
                                            ((list? tools-raw) tools-raw)
                                            (else default-allowed-tools))))
                               (run-subagent prompt
                                             #:client client
                                             #:parent-reg reg
                                             #:allowed-tools tools
                                             #:max-iterations max-iter
                                             #:journal-dir journal-dir))))
           #:metadata %investigate-tool-schema)

         (registry-attach!
           reg
           "investigate_parallel"
           (lambda (args)
             (with-tool-args args ((queries-raw queries '())
                                   (max-iter max_iterations 8))
                             (let ((queries (cond
                                              ((vector? queries-raw) (vector->list queries-raw))
                                              ((list? queries-raw) queries-raw)
                                              ((string? queries-raw) (list queries-raw))
                                              (else '()))))
                               (run-subagents-parallel queries
                                                       #:client client
                                                       #:parent-reg reg
                                                       #:allowed-tools default-allowed-tools
                                                       #:max-iterations max-iter
                                                       #:journal-dir journal-dir))))
           #:metadata %investigate-parallel-tool-schema))
