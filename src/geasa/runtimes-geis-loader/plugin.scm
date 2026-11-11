;;; (geasa runtimes-geis-loader plugin) — Dynamic Discovery & Mounting of Project Geasa
;;;
;;; Scans workspace geasa directories (.badb/geasa, .badb/workflows),
;;; verifies bundle integrity against pinned manifests, dynamically mounts
;;; plugin modules into the runtime registry, and synthesizes fallback specs
;;; when unmanaged user plugins are discovered.

(define-module (geasa runtimes-geis-loader plugin)
               #:use-module (badb geis bundle)
               #:use-module (badb geis capabilities)
               #:use-module (badb kernel registry)
               #:use-module (badb kernel digest)
               #:use-module (geasa capabilities-bubblewrap plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (ice-9 ftw)
               #:use-module (ice-9 format)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 textual-ports)
               #:use-module (json)
               #:use-module (srfi srfi-1)
               #:export (discover-and-mount-project-geasa!
                          mount-all-project-geasa!
                          synthesize-fallback-spec!
                          geis-catalog-index
                          geis-index-query))

(define (synthesize-fallback-spec! bundle-dir fid plugin-file module-name)
        "Synthesize a minimal spec.geis.scm and manifest.scm for a raw plugin."
        (let* ((spec-path (string-append bundle-dir "/spec.geis.scm"))
               (manifest-path (string-append bundle-dir "/manifest.scm"))
               (full-plugin (string-append bundle-dir "/" plugin-file)))
          ;; Write a spec that verifies module can be loaded
          (call-with-output-file spec-path
                                 (lambda (port)
                                   (format port ";;; Auto-synthesized fallback spec for ~a~%" fid)
                                   (format port "(define-module (test ~a) #:use-module (badb spec) #:use-module ~s)~%"
                                           fid module-name)
                                   (format port "(spec-begin ~s)~%" fid)
                                   (format port "(test-assert ~s #t)~%" (format #f "~a module loaded" fid))
                                   (format port "(spec-end)~%")
                                   (format port "(spec-exit)~%")))
          ;; Write initial manifest
          (let* ((plugin-sha (sha256-hex (file->string full-plugin)))
                 (spec-sha (sha256-hex (file->string spec-path)))
                 (manifest-data
                   `((bundle-version . 1)
                     (feature-id . ,fid)
                     (version . "1.0.0")
                     (spec . "spec.geis.scm")
                     (plugin . ((module . ,module-name)
                                (file . ,plugin-file)))
                     (sha256 . ((,plugin-file . ,plugin-sha)
                                ("spec.geis.scm" . ,spec-sha))))))
            (call-with-output-file manifest-path
                                   (lambda (port) (write manifest-data port)))
            manifest-data)))

(define (%resolve-capability-grants requests provider)
        (let loop ((remaining requests) (grants '()))
                  (if (null? remaining)
                    (reverse grants)
                    (let* ((request (car remaining))
                           (grant (and (procedure? provider)
                                       (catch #t
                                         (lambda () (provider (car request) (cadr request)))
                                         (lambda _ #f)))))
                      (if (procedure? grant)
                        (loop (cdr remaining) (cons (cons request grant) grants))
                        #f)))))

(define (%json-line->scm line)
        (json->scm (open-input-string line)))

(define (%write-json-line port value)
        (display (scm->json-string value) port)
        (newline port)
        (force-output port))

(define (%broker-capability-call grants name target args)
        (let ((entry (find (lambda (grant)
                             (and (equal? (caar grant) name)
                                  (equal? (cadar grant) target)))
                           grants)))
          (unless entry
            (error "worker requested an undeclared capability" name target))
          (apply (cdr entry) args)))

(define (%declared-tool-name item)
        (cond
          ((string? item) item)
          ((symbol? item) (symbol->string item))
          ((pair? item) (%declared-tool-name (car item)))
          (else #f)))

(define (%run-bubblewrap-tool bundle-dir mod-name plugin-file tool-name args grants _proc)
        "Run one untrusted project tool in a fresh bubblewrap worker."
        (let* ((worker (search-path %load-path "badb/geis/worker.scm"))
               (src-root (and worker (dirname (dirname (dirname worker)))))
               (command (if src-root
                          (bubblewrap-worker-command bundle-dir src-root
                                                     %load-path %load-compiled-path)
                          (error "geis worker source is missing")))
               (pipe (apply open-pipe* OPEN_BOTH command))
               (request `((module . ,(call-with-output-string
                                       (lambda (port) (write mod-name port))))
                          (plugin . ,(string-append "/badb/bundle/" plugin-file))
                          (tool . ,tool-name)
                          (args . ,(list->vector args))
                          (capabilities . ,(list->vector
                                             (map (lambda (grant)
                                                    `((name . ,(symbol->string (caar grant)))
                                                      (target . ,(cadar grant))))
                                                  grants))))))
          (dynamic-wind
            (lambda () #f)
            (lambda ()
              (%write-json-line pipe request)
              (let loop ()
                        (let ((line (read-line pipe)))
                          (when (eof-object? line)
                            (error "bubblewrap worker exited without a result" tool-name))
                          (let* ((message (%json-line->scm line))
                                 (kind (assoc-ref message "type")))
                            (cond
                              ((equal? kind "capability")
                                (let* ((id (assoc-ref message "id"))
                                       (name (string->symbol (assoc-ref message "name")))
                                       (target (assoc-ref message "target"))
                                       (call-args (vector->list (assoc-ref message "args")))
                                       (reply (catch #t
                                                (lambda ()
                                                  `((type . "capability-result") (id . ,id)
                                                    (value . ,(%broker-capability-call
                                                                grants name target call-args))))
                                                (lambda (key . data)
                                                  `((type . "capability-result") (id . ,id)
                                                    (error . ,(format #f "~a: ~s" key data)))))))
                                  (%write-json-line pipe reply)
                                  (loop)))
                              ((equal? kind "result")
                                (if (assoc-ref message "error")
                                  (error "sandboxed geis tool failed" tool-name
                                         (assoc-ref message "error"))
                                  (assoc-ref message "value")))
                              (else (error "invalid bubblewrap worker message" message)))))))
            (lambda () (close-pipe pipe)))))

(define (%warn logger message)
        (when (procedure? logger)
          (logger 'warn message)))

(define (%mount-project-bundle! reg bundle-dir sub capability-provider sandbox-runner logger)
        (let* ((manifest-file (string-append bundle-dir "/manifest.scm"))
               (plugin-file (string-append bundle-dir "/plugin.scm")))
          (unless (file-exists? manifest-file)
            (when (file-exists? plugin-file)
              (catch #t
                (lambda ()
                  (synthesize-fallback-spec! bundle-dir sub "plugin.scm"
                                             (list 'project (string->symbol sub))))
                (lambda (key . data)
                  (%warn logger (format #f "Failed to synthesize fallback spec for ~a: ~s"
                                        sub (cons key data)))))))
          (and (file-exists? manifest-file)
               (catch #t
                 (lambda ()
                   (let ((errors (geis-manifest-valid? bundle-dir)))
                     (if (pair? errors)
                       (begin
                         (%warn logger (format #f "Invalid bundle ~a: ~a"
                                               sub (string-join errors "; ")))
                         #f)
                       (let* ((manifest (read-geis-manifest manifest-file))
                              (fid (manifest-field manifest 'feature-id))
                              (plugin-info (manifest-field manifest 'plugin))
                              (tool-names (manifest-field manifest 'tools))
                              (requests (or (manifest-field manifest 'capabilities) '()))
                              (grants (%resolve-capability-grants requests capability-provider)))
                         (cond
                           ((and (pair? requests) (not grants))
                             (%warn logger (format #f "Bundle ~a denied: a requested capability has no host grant" fid))
                             #f)
                           ((not (and (list? plugin-info)
                                      (manifest-field plugin-info 'file)
                                      (manifest-field plugin-info 'module)
                                      (list? tool-names)
                                      (every %declared-tool-name tool-names)))
                             (%warn logger (format #f "Bundle ~a denied: project plugins must declare tools" fid))
                             #f)
                           (else
                             (let ((rel-file (manifest-field plugin-info 'file))
                                   (mod-name (manifest-field plugin-info 'module)))
                               ;; Plugin code is evaluated only in the isolated worker.
                               (for-each
                                 (lambda (tool)
                                   (let ((name (%declared-tool-name tool)))
                                     (registry-attach! reg name
                                                       (lambda args
                                                         (sandbox-runner bundle-dir mod-name rel-file
                                                                         name args grants #f))
                                                       #:metadata `((source . ,bundle-dir)
                                                                    (bundle-id . ,fid)))))
                                 tool-names)
                               fid)))))))
                 (lambda (key . data)
                   (%warn logger (format #f "Error mounting bundle ~a: ~s"
                                         sub (cons key data)))
                   #f)))))

(define* (discover-and-mount-project-geasa! reg
                                            #:key
                                            (workspace (getcwd))
                                            (extra-search-paths '())
                                            (capability-provider #f)
                                            (sandbox-runner %run-bubblewrap-tool)
                                            (logger #f))
         "Discover and mount project bundles without evaluating plugin code in the host."
         (let ((roots (filter file-exists?
                              (append (list (string-append workspace "/.badb/geasa")
                                            (string-append workspace "/.badb/workflows"))
                                      extra-search-paths)))
               (mounted '()))
           (for-each
             (lambda (root)
               (for-each
                 (lambda (sub)
                   (let* ((bundle-dir (string-append root "/" sub))
                          (fid (and (file-is-directory? bundle-dir)
                                    (%mount-project-bundle! reg bundle-dir sub
                                                            capability-provider
                                                            sandbox-runner logger))))
                     (when fid (set! mounted (cons fid mounted)))))
                 (scandir-valid-entries root)))
             roots)
           (reverse mounted)))

(define mount-all-project-geasa! discover-and-mount-project-geasa!)

;;;
;;; Progressive Geasa Index & Discovery Query
;;;
(define (extract-tool-name item)
        "Extract string tool name from string, symbol, or (name . meta) pair."
        (cond
          ((string? item) item)
          ((symbol? item) (symbol->string item))
          ((pair? item) (extract-tool-name (car item)))
          (else (object->string item))))

(define* (geis-catalog-index #:key
                             (workspace (getcwd))
                             (extra-search-paths '()))
         "Collect lightweight metadata records across all discovered geasa.
Returns a list of alists: ((fid . \"...\") (category . \"...\") (description . \"...\") (tools . (\"...\")))."
         (let* ((candidate-roots
                  (filter file-exists?
                          (append (list (string-append workspace "/.badb/geasa")
                                        (string-append workspace "/.badb/workflows")
                                        (string-append workspace "/geis")
                                        "src/geasa")
                                  extra-search-paths)))
                (seen (make-hash-table))
                (results '()))
           ;; 1. Core built-in spec gate entry
           (let ((spec-gate-entry
                   `((fid . "spec-gate")
                     (category . "spec-gate")
                     (description . "Kernel-owned locked spec gate & Sema plugin synthesis workflow (spec.lock, spec.synthesize, spec.verify, spec.status)")
                     (tools . ("spec.lock" "spec.synthesize" "spec.verify" "spec.status")))))
             (hash-set! seen "spec-gate" #t)
             (set! results (cons spec-gate-entry results)))

           ;; 2. Scan disk bundles
           (for-each
             (lambda (root)
               (let ((subdirs (catch #t
                                (lambda () (scandir-valid-entries root))
                                (lambda _ '()))))
                 (for-each
                   (lambda (sub)
                     (let* ((bundle-dir (string-append root "/" sub))
                            (manifest-file (string-append bundle-dir "/manifest.scm")))
                       (when (and (file-is-directory? bundle-dir)
                                  (file-exists? manifest-file)
                                  (not (hash-ref seen sub)))
                         (catch #t
                           (lambda ()
                             (let* ((manifest (read-geis-manifest manifest-file))
                                    (fid (or (manifest-field manifest 'feature-id) sub))
                                    (desc (or (manifest-field manifest 'description)
                                              (let ((meta (manifest-field manifest 'meta)))
                                                (and (pair? meta) (assoc-ref meta 'description)))
                                              (format #f "Capability bundle ~a" fid)))
                                    (cat (or (manifest-field manifest 'category)
                                             (cond
                                               ((string-prefix? "project." fid) "project")
                                               ((string-prefix? "capabilities-" fid) "capability")
                                               ((string-prefix? "runtimes-" fid) "runtime")
                                               (else "general"))))
                                    (raw-tools (or (manifest-field manifest 'tools)
                                                   (list fid)))
                                    (clean-tools (map extract-tool-name (ensure-list raw-tools))))
                               (hash-set! seen sub #t)
                               (set! results (cons `((fid . ,fid)
                                                     (category . ,cat)
                                                     (description . ,desc)
                                                     (tools . ,clean-tools))
                                                   results))))
                           (lambda _ #f)))))
                   subdirs)))
             candidate-roots)
           (reverse results)))

(define* (geis-index-query #:key
                           (workspace (getcwd))
                           (category #f)
                           (query #f)
                           (extra-search-paths '()))
         "Query the geis catalog index and format a compact markdown summary
suitable for model progressive discovery without context bloat."
         (let* ((all (geis-catalog-index #:workspace workspace #:extra-search-paths extra-search-paths))
                (cat-str (and (string? category) (not (string-null? (string-trim-both category)))
                              (string-downcase (string-trim-both category))))
                (q-str (and (string? query) (not (string-null? (string-trim-both query)))
                            (string-downcase (string-trim-both query))))
                (matches
                  (filter
                    (lambda (entry)
                      (let* ((fid (string-downcase (or (assoc-ref entry 'fid) "")))
                             (cat (string-downcase (or (assoc-ref entry 'category) "")))
                             (desc (string-downcase (or (assoc-ref entry 'description) "")))
                             (tools (map (lambda (t) (string-downcase (extract-tool-name t)))
                                         (or (assoc-ref entry 'tools) '())))
                             (cat-match? (if cat-str (string-contains cat cat-str) #t))
                             (q-match?
                               (if q-str
                                 (or (string-contains fid q-str)
                                     (string-contains desc q-str)
                                     (any (lambda (t) (string-contains t q-str)) tools))
                                 #t)))
                        (and cat-match? q-match?)))
                    all)))
           (if (null? matches)
             (format #f "No geasa found matching category=~s query=~s. Use geis.index without filters to list all."
                     category query)
             (let ((lines
                     (map (lambda (entry)
                            (let* ((fid (assoc-ref entry 'fid))
                                   (cat (assoc-ref entry 'category))
                                   (desc (assoc-ref entry 'description))
                                   (tools (assoc-ref entry 'tools))
                                   (tools-str (if (null? tools) fid (string-join tools ", "))))
                              (format #f "- **~a** [~a] (tools: `~a`): ~a"
                                      fid cat tools-str desc)))
                          matches)))
               (format #f "### Available Geasa Index (~a total)\n~a\n\nTo activate a capability and hydrate its parameter schema into your toolset, call `geis.activate(fid)`."
                       (length matches)
                       (string-join lines "\n"))))))
