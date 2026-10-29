;;; badb/lema-mcp.scm — Lema Spec Authoring & Diagnostics MCP server.
;;;
;;; Phase 3 Feature #2. Serves Lema contract authoring and diagnostics
;;; over MCP on the shared server core (badb mcp-server): publish a
;;; contract, submit a verification request, replay recorded errors,
;;; list verified contracts, and fetch a contract record. The tools
;;; connect to the same Lema socket as the kernel prove gate
;;; ($BADB_LEMA_SOCKET or ~/.local/share/lema/lema.sock), speaking the
;;; protocol documented in src/badb/lema.scm.
;;;
;;; Authoring/diagnostics ONLY: the kernel retains exclusive control
;;; of the commit gate. badb's transaction commit path runs the
;;; kernel-owned prove gate itself; verdicts fetched through this MCP
;;; server are diagnostics for the agent and are never trusted by the
;;; kernel.
;;;
;;; Registration in badb's config (see (badb config) parse-mcp-server-spec):
;;;   (mcp
;;;    (server "lema"
;;;            (command "scripts/mcp-lema")
;;;            (auto-start #t)))
;;;
;;; Or run the wrapper directly:
;;;   scripts/mcp-lema

(define-module (badb lema-mcp)
  #:use-module (badb lema)
  #:use-module (badb mcp-server)
  #:use-module (json)
  #:export (lema-mcp-server
            lema-mcp-serve))

;;;
;;; Tool definitions (symbol keys; serialized by guile-json)
;;;
(define %lema-tools
  #(((name . "publish-contract")
     (description . "Publish a contract (id, source code, spec text) to the Lema oracle.")
     (inputSchema
      . ((type . "object")
         (properties . ((id . ((type . "string")))
                        (code . ((type . "string")))
                        (spec . ((type . "string")))))
         (required . #("id" "code" "spec")))))
    ((name . "submit")
     (description
      . "Submit a verification request (contract_id, ast_diff_hash, evidence) to the Lema oracle. Diagnostics only: kernel commit verdicts come from the kernel-owned prove gate, never from this tool.")
     (inputSchema
      . ((type . "object")
         (properties . ((contract_id . ((type . "string")))
                        (ast_diff_hash . ((type . "string")))
                        (evidence . ((type . "string")))))
         (required . #("contract_id")))))
    ((name . "replay-errors")
     (description . "Replay the recorded verification errors for a contract id.")
     (inputSchema
      . ((type . "object")
         (properties . ((contract_id . ((type . "string")))))
         (required . #("contract_id")))))
    ((name . "list-verified")
     (description . "List the contract ids that have passed verification by the Lema oracle.")
     (inputSchema
      . ((type . "object")
         (properties . ()))))
    ((name . "get-contract")
     (description . "Fetch the contract record (id, code, spec) for a contract id.")
     (inputSchema
      . ((type . "object")
         (properties . ((id . ((type . "string")))))
         (required . #("id")))))))

;;;
;;; Handlers (inbound tool args arrive as string-keyed alists)
;;;
(define (json-response resp)
  "Render a Lema response alist as JSON text for an MCP tool result."
  (scm->json-string resp))

(define %lema-handlers
  (list
   (cons "publish-contract"
         (lambda (args)
           (json-response
            (lema-publish-contract!
             (or (assoc-ref args "id") "")
             (or (assoc-ref args "code") "")
             (or (assoc-ref args "spec") "")))))
   (cons "submit"
         (lambda (args)
           (json-response
            (lema-submit!
             (or (assoc-ref args "contract_id") "")
             (or (assoc-ref args "ast_diff_hash") "")
             (or (assoc-ref args "evidence") "")))))
   (cons "replay-errors"
         (lambda (args)
           (json-response
            (lema-replay-errors (or (assoc-ref args "contract_id") "")))))
   (cons "list-verified"
         (lambda (args)
           (json-response (lema-list-verified))))
   (cons "get-contract"
         (lambda (args)
           (json-response
            (lema-get-contract (or (assoc-ref args "id") "")))))))

;;;
;;; Server assembly
;;;
(define (lema-mcp-server)
  "Build the lema MCP server definition accepted by (mcp-serve-stdio):
an alist with \"name\", \"version\", symbol-keyed \"tools\" vector, and
a \"handlers\" alist mapping tool names to handler lambdas."
  `(("name" . "lema")
    ("version" . "1.0.0")
    ("tools" . ,%lema-tools)
    ("handlers" . ,%lema-handlers)))

(define (lema-mcp-serve)
  "Serve the lema authoring/diagnostics MCP server over stdio forever
(until stdin EOF), on the (badb mcp-server) core."
  (mcp-serve-stdio (lema-mcp-server)))
