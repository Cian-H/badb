;;; (geasa runtimes-geis-loader plugin) — Dynamic Discovery & Mounting of Project Geasa
;;;
;;; Scans workspace geasa directories (.badb/geasa, .badb/workflows),
;;; verifies bundle integrity against pinned manifests, dynamically mounts
;;; plugin modules into the runtime registry, and synthesizes fallback specs
;;; when unmanaged user plugins are discovered.

(define-module (geasa runtimes-geis-loader plugin)
  #:use-module (badb geis bundle)
  #:use-module (badb kernel registry)
  #:use-module (badb kernel digest)
  #:use-module (geasa foundation-util plugin)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
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

(define* (discover-and-mount-project-geasa! reg
                                            #:key
                                            (workspace (getcwd))
                                            (extra-search-paths '())
                                            (logger #f))
  "Discover and mount geis bundles from WORKSPACE/.badb/geasa,
WORKSPACE/.badb/workflows, and EXTRA-SEARCH-PATHS into REG."
  (let* ((candidate-roots
          (filter file-exists?
                  (append (list (string-append workspace "/.badb/geasa")
                                (string-append workspace "/.badb/workflows"))
                          extra-search-paths)))
         (mounted-bundles '()))
    (for-each
     (lambda (root)
       (let ((subdirs (scandir-valid-entries root)))
         (for-each
          (lambda (sub)
            (let* ((bundle-dir (string-append root "/" sub))
                   (manifest-file (string-append bundle-dir "/manifest.scm"))
                   (plugin-file (string-append bundle-dir "/plugin.scm")))
              (when (file-is-directory? bundle-dir)
                ;; If manifest is missing but plugin.scm exists, auto-synthesize contract
                (unless (file-exists? manifest-file)
                  (when (file-exists? plugin-file)
                    (catch #t
                      (lambda ()
                        (synthesize-fallback-spec! bundle-dir sub "plugin.scm"
                                                   (list 'project (string->symbol sub))))
                      (lambda (k . a)
                        (when (procedure? logger)
                          (logger 'warn (format #f "Failed to synthesize fallback spec for ~a: ~s" sub a)))))))
                ;; Now if manifest exists, validate and mount
                (when (file-exists? manifest-file)
                  (let ((errors (geis-manifest-valid? bundle-dir)))
                    (if (null? errors)
                        (let* ((manifest (read-geis-manifest manifest-file))
                               (fid (manifest-field manifest 'feature-id))
                               (plugin-info (manifest-field manifest 'plugin))
                               (tool-names (manifest-field manifest 'tools)))
                          (when (and (list? plugin-info) (manifest-field plugin-info 'file))
                            (let* ((rel-file (manifest-field plugin-info 'file))
                                   (full-plugin (string-append bundle-dir "/" rel-file))
                                   (mod-name (manifest-field plugin-info 'module)))
                              (catch #t
                                (lambda ()
                                  (save-module-excursion
                                    (lambda ()
                                      (primitive-load full-plugin)))
                                  (let* ((mod (resolve-module mod-name))
                                         (iface (module-public-interface mod)))
                                    (if iface
                                        (let ((exports (module-map (lambda (sym var)
                                                                     (cons (symbol->string sym) (variable-ref var)))
                                                                   iface)))
                                          (for-each
                                           (lambda (entry)
                                             (let ((name (car entry))
                                                   (proc (cdr entry)))
                                               (when (and (procedure? proc)
                                                          (or (not tool-names)
                                                              (member name tool-names)))
                                                 (registry-attach! reg name proc
                                                                   #:metadata
                                                                   `((source . ,bundle-dir)
                                                                     (bundle-id . ,fid))))))
                                           exports))
                                        (registry-attach! reg fid mod
                                                          #:metadata `((source . ,bundle-dir)
                                                                      (bundle-id . ,fid)))))
                                  (set! mounted-bundles (cons fid mounted-bundles)))
                                (lambda (k . a)
                                  (when (procedure? logger)
                                    (logger 'error (format #f "Error mounting bundle ~a: ~s" fid a))))))))
                        (when (procedure? logger)
                          (logger 'warn (format #f "Invalid bundle ~a: ~a" sub (string-join errors "; "))))))))))
          subdirs)))
     candidate-roots)
    (reverse mounted-bundles)))

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
