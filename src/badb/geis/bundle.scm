;;; (badb geis bundle) — Geis bundle manifest handling, discovery, validation, hashing, and dependency resolution.
;;;
;;; A geis bundle is a self-contained directory geis/<feature-id>/ holding a
;;; manifest.scm descriptor plus the artifacts it pins: spec.geis.scm, optional
;;; plugin.scm, workflow.sema, prompts/, fixtures/, cassettes/.  Manifests are
;;; plain data alists parsed with (read) and never evaluated.  Bundle identity
;;; is the content hash over every file except manifest.scm, so identical
;;; content yields the same bundle hash wherever it lives.

(define-module (badb geis bundle) #:use-module
               (badb kernel digest)
               #:use-module
               (ice-9 ftw)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 textual-ports)
               #:use-module
               (srfi srfi-1)
               ;; DEV-DEBT [accepted-layering]: bundle tooling imports the synthesis plugin for
;; replay; the reverse direction is broken via synthesis-assemble-hook in the plugin.
#:use-module
               (geasa runtimes-synthesis plugin)
               #:use-module
               (json)
               #:use-module
               (badb kernel geis-gate)
               #:export
               (geis-bundle-search-path find-geis-bundle
                                        read-geis-manifest
                                        manifest-field
                                        geis-manifest-valid?
                                        geis-bundle-files
                                        compute-geis-sha256-pins
                                        geis-bundle-hash
                                        resolve-geis-load-path
                                        geis-bundle-pack
                                        geis-bundle-install
                                        geis-capture-pending-prompt!
                                        geis-take-pending-prompt
                                        geis-bundle-assemble!
                                        geis-bundle-replay))

(define (%string-split-char s delim)
  "Split S on single-character DELIM into a list of substrings."
  (let loop ((rest (string->list s)) (acc '()) (cur '()))
    (cond
     ((null? rest)
      (reverse (cons (list->string (reverse cur)) acc)))
     ((char=? (car rest) delim)
      (loop (cdr rest) (cons (list->string (reverse cur)) acc) '()))
     (else
      (loop (cdr rest) acc (cons (car rest) cur))))))

(define (%read-file-string path)
  "Read all of PATH as a UTF-8 string.  Throws on I/O errors."
  (call-with-input-file path
    (lambda (port)
      (set-port-encoding! port "UTF-8")
      (get-string-all port))))

(define (%directory? path)
  "Return #t when PATH names an existing directory."
  (catch #t
    (lambda () (eq? (stat:type (stat path)) 'directory))
    (lambda _ #f)))

(define (%feature-id-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\. #\_ #\-))))

(define (%valid-feature-id? fid)
  (and (string? fid)
       (not (string-null? fid))
       (let loop ((chars (string->list fid)))
         (cond ((null? chars) #t)
               ((%feature-id-char? (car chars)) (loop (cdr chars)))
               (else #f)))))

(define (%safe-relative-path? path)
  "PATH is a non-empty relative path with no '..' component."
  (and (string? path)
       (not (string-null? path))
       (not (string-prefix? "/" path))
       (not (member ".." (%string-split-char path #\/)))))

(define (%bundle-member-exists? bundle-dir rel-path)
  (file-exists? (string-append bundle-dir "/" rel-path)))

(define (%all-hex? s)
  (let loop ((chars (string->list s)))
    (cond ((null? chars) #t)
          ((memv (char-downcase (car chars))
                 '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                   #\a #\b #\c #\d #\e #\f))
           (loop (cdr chars)))
          (else #f))))

;;;
;;; Search path and discovery
;;;
(define (geis-bundle-search-path)
  "Return the bundle search roots: BADB_GEIS_PATH split on colons, or
'(\"src/geasa\") when unset or empty."
  (let ((path (getenv "BADB_GEIS_PATH")))
    (if (or (not path) (string-null? path))
        '("src/geasa" "geis")
        (%string-split-char path #\:))))

(define (%fid->string fid)
  "Normalize a feature id (symbol from manifest data, or string) to a string."
  (if (symbol? fid) (symbol->string fid) fid))

(define (find-geis-bundle feature-id)
  "Return the directory of FEATURE-ID (symbol or string) from the first
search root holding <root>/<feature-id>/manifest.scm, or #f when no bundle
is found."
  (let ((fid (%fid->string feature-id)))
    (let loop ((roots (geis-bundle-search-path)))
      (cond
       ((null? roots) #f)
       ((file-exists? (string-append (car roots) "/" fid "/manifest.scm"))
        (string-append (car roots) "/" fid))
       (else (loop (cdr roots)))))))

;;;
;;; Manifest reading
;;;
(define (read-geis-manifest path)
  "Read the manifest at PATH as one data sexp.  Return the alist, or #f on
missing file, unreadable content, or a non-list result.  Never evaluates."
  (catch #t
    (lambda ()
      (call-with-input-file path
        (lambda (port)
          (let ((sexp (read port)))
            (and (list? sexp) sexp)))))
    (lambda _ #f)))

(define (manifest-field manifest key)
  "Look up KEY in the manifest alist MANIFEST; #f when absent."
  (assoc-ref manifest key))

;;;
;;; Manifest validation
;;;
;; DEV-DEBT [manual-repin]: pins cover every bundle file, so ANY content edit
;; (even a comment) invalidates the manifest until pins are recomputed. No automated
;; check exists — add one to CI before ./scripts/test.
(define (%validate-sha256-pins! errors-cell bundle-dir pins)
  (define (add! reason)
    (set-car! errors-cell (cons reason (car errors-cell))))
  (for-each
   (lambda (pin)
     (let ((file (and (pair? pin) (car pin)))
           (hash (and (pair? pin) (cdr pin))))
       (cond
        ((not (and (string? file) (string? hash)))
         (add! (format #f "malformed sha256 pin entry: ~s" pin)))
        ((not (= 64 (string-length hash)))
         (add! (format #f "sha256 pin for '~a' is not 64 hex chars" file)))
        ((not (%all-hex? hash))
         (add! (format #f "sha256 pin for '~a' is not hexadecimal" file)))
        ((not (%bundle-member-exists? bundle-dir file))
         (add! (format #f "sha256 pin for '~a' but file does not exist" file)))
        (else
         (catch #t
           (lambda ()
             (unless (string= (sha256-hex (%read-file-string
                                           (string-append bundle-dir "/" file)))
                              hash)
               (add! (format #f "sha256 pin mismatch for file '~a'" file))))
           (lambda _ #f))))))
   pins))

;; DEV-DEBT [complexity]: cyclomatic ~41 — each manifest field is a hand-rolled
;; check; a declarative field schema would shrink this.
(define (geis-manifest-valid? bundle-dir)
  "Validate the bundle at BUNDLE-DIR.  Return '() when the manifest is valid,
otherwise a non-empty list of human-readable reason strings."
  (let ((errors-cell (list '()))
        (manifest (read-geis-manifest (string-append bundle-dir "/manifest.scm"))))
    (define (add! reason)
      (set-car! errors-cell (cons reason (car errors-cell))))
    (cond
     ((not manifest)
      (add! "manifest.scm is missing or unreadable"))
     (else
      (unless (equal? 1 (manifest-field manifest 'bundle-version))
        (add! "bundle-version must be exactly 1"))
      (let ((fid (manifest-field manifest 'feature-id)))
        (unless (%valid-feature-id? fid)
          (add! (format #f "feature-id '~a' is missing or not sanitized" fid)))
        (when (and (string? fid) (not (string= fid (basename bundle-dir))))
          (add! (format #f "feature-id '~a' does not match directory name '~a'"
                        fid (basename bundle-dir)))))
      (let ((ver (manifest-field manifest 'version)))
        (unless (and (string? ver) (not (string-null? ver)))
          (add! "version must be a non-empty string")))
      (let ((spec (manifest-field manifest 'spec)))
        (unless spec (add! "spec field is missing"))
        (when (and (string? spec) (not (%safe-relative-path? spec)))
          (add! (format #f "spec path '~a' is not a safe relative path" spec)))
        (when (and (string? spec) (not (%bundle-member-exists? bundle-dir spec)))
          (add! (format #f "spec file '~a' does not exist in bundle" spec))))
      (let ((plugin (manifest-field manifest 'plugin)))
        (when plugin
          (let ((module (and (list? plugin) (manifest-field plugin 'module)))
                (file (and (list? plugin) (manifest-field plugin 'file))))
            (unless (and (list? module) (every symbol? module))
              (add! "plugin field must carry (module . list-of-symbols)"))
            (unless (string? file)
              (add! "plugin field must carry (file . string)"))
            (when (string? file)
              (unless (%safe-relative-path? file)
                (add! (format #f "plugin file path '~a' is not a safe relative path" file)))
              (unless (%bundle-member-exists? bundle-dir file)
                (add! (format #f "plugin file '~a' does not exist in bundle" file)))))))
      (let ((workflow (manifest-field manifest 'workflow)))
        (when workflow
          (unless (%safe-relative-path? workflow)
            (add! (format #f "workflow path '~a' is not a safe relative path" workflow)))
          (unless (%bundle-member-exists? bundle-dir workflow)
            (add! (format #f "workflow file '~a' does not exist in bundle" workflow)))))
      (let ((prompts (manifest-field manifest 'prompts)))
        (when prompts
          (unless (and (list? prompts) (every string? prompts))
            (add! "prompts field must be a list of file paths"))
          (for-each
           (lambda (p)
             (unless (%safe-relative-path? p)
               (add! (format #f "prompt path '~a' is not a safe relative path" p)))
             (unless (%bundle-member-exists? bundle-dir p)
               (add! (format #f "prompt file '~a' does not exist in bundle" p))))
           (if (list? prompts) prompts '()))))
      (let ((fixtures (manifest-field manifest 'fixtures)))
        (when fixtures
          (unless (%safe-relative-path? fixtures)
            (add! (format #f "fixtures path '~a' is not a safe relative path" fixtures)))
          (unless (%bundle-member-exists? bundle-dir fixtures)
            (add! (format #f "fixtures directory '~a' does not exist in bundle" fixtures)))))
      (let ((cassettes (manifest-field manifest 'cassettes)))
        (when cassettes
          (unless (%safe-relative-path? cassettes)
            (add! (format #f "cassettes path '~a' is not a safe relative path" cassettes)))
          (unless (%bundle-member-exists? bundle-dir cassettes)
            (add! (format #f "cassettes directory '~a' does not exist in bundle" cassettes)))))
      (let ((pins (manifest-field manifest 'sha256)))
        (when pins
          (unless (list? pins)
            (add! "sha256 field must be a list of (file . hash) pins"))
          (when (list? pins)
            (%validate-sha256-pins! errors-cell bundle-dir pins))))))
    (reverse (car errors-cell))))

;;;
;;; Bundle file enumeration and hashing
;;;
(define (%walk-files root rel-dir)
  (let ((entries (scandir (if (string-null? rel-dir)
                               root
                               (string-append root "/" rel-dir))
                          (lambda (n) (not (member n '("." "..")))))))
    (append-map!
     (lambda (entry)
       (let ((rel (if (string-null? rel-dir) entry (string-append rel-dir "/" entry))))
         (if (%directory? (string-append root "/" rel))
             (%walk-files root rel)
             (list rel))))
     (or entries '()))))

(define (%find-files dir)
  "Absolute paths of every file under DIR recursively (all files included)."
  (let ((result '()))
    (define (scan! d)
      (let ((entries (scandir d (lambda (n) (not (member n '("." "..")))))))
        (for-each
         (lambda (entry)
           (let ((full-path (string-append d "/" entry)))
             (if (%directory? full-path)
                 (scan! full-path)
                 (set! result (cons full-path result)))))
         (or entries '()))))
    (scan! dir)
    (reverse result)))

(define (%delete-tree! path)
        "Recursively delete the file or directory tree at PATH (best effort)."
        (let
            ((entries
               (catch #t
                 (lambda ()
                   (scandir path
                            (lambda (n)
                              (not (member n '("." ".."))))))
                 (lambda _
                   #f))))
          (if entries
            (begin
              (for-each
                (lambda (e)
                  (%delete-tree! (string-append path "/" e)))
                (or entries '()))
              (catch 'system-error
                (lambda ()
                  (rmdir path))
                (lambda _
                  #f)))
            (catch 'system-error
              (lambda ()
                (delete-file path))
              (lambda _
                #f)))))

(define (%mkdir-p path)
  "Create directory PATH and any missing parents."
  (let ((parts (%string-split-char path #\/)))
    (let loop ((rest parts)
               (built (if (string-prefix? "/" path) "/" "")))
      (unless (null? rest)
        (let ((cur (cond
                    ((string-null? built) (car rest))
                    ((string= built "/") (string-append "/" (car rest)))
                    (else (string-append built "/" (car rest))))))
          (unless (or (string-null? cur) (file-exists? cur))
            (mkdir cur))
          (loop (cdr rest) cur))))))

(define (geis-bundle-files bundle-dir)
  "Return the sorted relative paths of every file under BUNDLE-DIR
recursively, excluding manifest.scm."
  (sort (delete "manifest.scm" (%walk-files bundle-dir "") string=)
        string<?))

(define (compute-geis-sha256-pins bundle-dir)
  "Return ((file . sha256hex) ...) for every file from geis-bundle-files."
  (map (lambda (rel)
         (cons rel (sha256-hex (%read-file-string
                                 (string-append bundle-dir "/" rel)))))
       (geis-bundle-files bundle-dir)))

(define (geis-bundle-hash bundle-dir)
  "Return the deterministic content identity of BUNDLE-DIR: the sha256 over
sorted \"<file> <hash>\\n\" lines for every pinned file."
  (sha256-hex
   (apply string-append
          (map (lambda (pin) (format #f "~a ~a\n" (car pin) (cdr pin)))
               (compute-geis-sha256-pins bundle-dir)))))

;;;
;;; Dependency resolution
;;;
(define (%geis-deps manifest)
  "Extract the ((fid . ((version . v) (bundle-sha256 . h))) ...) alist from MANIFEST."
  (let ((depends (manifest-field manifest 'depends)))
    (if (list? depends)
        (or (manifest-field depends 'geis) '())
        '())))

(define (%verify-dep-edge! dep-fid spec)
        "Verify that dependency DEP-FID (with pinned SPEC) exists with the pinned
version and bundle hash; throw 'geis-bundle-error otherwise."
        (let*
            ((want-ver (manifest-field spec 'version))
             (want-hash (manifest-field spec 'bundle-sha256))
             (dep-dir (find-geis-bundle dep-fid)))
          (unless dep-dir
            (throw 'geis-bundle-error (format #f "missing dependency '~a'" dep-fid)))
          (let
              ((dep-manifest (read-geis-manifest (string-append dep-dir "/manifest.scm"))))
            (unless dep-manifest
              (throw 'geis-bundle-error
                     (format #f "invalid manifest for bundle '~a'" dep-fid)))
            (let
                ((have-ver (manifest-field dep-manifest 'version)))
              (unless
                  (and (string? want-ver) (string? have-ver) (string= have-ver want-ver))
                (throw 'geis-bundle-error
                       (format #f
                               "version mismatch for dependency '~a': want ~s have ~s"
                               dep-fid
                               want-ver
                               have-ver)))))
          (let ((actual-hash (geis-bundle-hash dep-dir)))
            (unless
                (and (string? want-hash) (string= actual-hash want-hash))
              (throw 'geis-bundle-error
                     (format #f "bundle-sha256 mismatch for dependency '~a'" dep-fid))))))

(define (resolve-geis-load-path feature-id)
  "Return the absolute bundle directories FEATURE-ID needs, dependencies
first and the target bundle last.  Throw 'geis-bundle-error on missing
bundles, invalid manifests, version mismatches, bundle-sha256 mismatches,
or dependency cycles.  Shared dependencies appear exactly once."
  (let ((order '()))
    (let visit! ((raw-fid feature-id) (stack '()))
      (let ((fid (%fid->string raw-fid)))
        (when (member fid stack)
          (throw 'geis-bundle-error (format #f "dependency cycle detected at '~a'" fid)))
        (let ((dir (find-geis-bundle fid)))
          (unless dir
            (throw 'geis-bundle-error
                   (format #f "missing ~a bundle '~a'" (if (null? stack) "" "dependency") fid)))
          (let ((manifest (read-geis-manifest (string-append dir "/manifest.scm"))))
            (unless manifest
              (throw 'geis-bundle-error (format #f "invalid manifest for bundle '~a'" fid)))
            (for-each
             (lambda (dep)
               (let ((dep-fid (and (pair? dep) (car dep)))
                     (spec (and (pair? dep) (cdr dep))))
                 (%verify-dep-edge! dep-fid spec)
                 (unless (assoc (%fid->string dep-fid) order string=)
                   (visit! dep-fid (cons fid stack)))))
             (%geis-deps manifest))
            (unless (assoc fid order string=)
              (set! order (append order
                                  (list (cons fid (canonicalize-path dir))))))))))
    (map cdr order)))

;;;
;;; Bundle packing
;;;
(define (%clean-path path)
  "PATH with a trailing slash removed (except \"/\" itself)."
  (if (and (> (string-length path) 1) (string-suffix? "/" path))
      (substring path 0 (- (string-length path) 1))
      path))

(define (geis-bundle-pack feature-id)
        "Pack the bundle FEATURE-ID for sharing: copy it to a fresh temp dir,
rewrite the copy's manifest.scm with fresh sha256 pins, tar the copy into
dist/badb-geis-<feature-id>-<version>.tar.gz, and write a .sha256 sidecar
holding the copy's bundle hash.  Return ((tarball . path)
(bundle-sha256 . hash)).  Throw
'geis-bundle-error when the bundle is missing or fails validation."
        (let ((bundle-dir (find-geis-bundle feature-id)))
          (unless bundle-dir
            (throw 'geis-bundle-error
                   (format #f "bundle not found for feature-id '~a'" feature-id)))
          (let ((errors (geis-manifest-valid? bundle-dir)))
            (unless (null? errors)
              (throw 'geis-bundle-error
                     (format #f "manifest validation failed: ~a" (string-join errors "; ")))))
          (let*
              ((manifest (read-geis-manifest (string-append bundle-dir "/manifest.scm")))
               (version (manifest-field manifest 'version))
               (temp-parent (mkdtemp "/tmp/badb-pack-XXXXXX"))
               (copy-dir (string-append temp-parent "/" (basename bundle-dir))))
            (catch #t
              (lambda ()
                (mkdir copy-dir)
                (for-each
                  (lambda (rel)
                    (let ((dst (string-append copy-dir "/" rel)))
                      (%mkdir-p (dirname dst))
                      (copy-file (string-append bundle-dir "/" rel) dst)))
                  (geis-bundle-files bundle-dir))
                (let*
                    ((pins (compute-geis-sha256-pins copy-dir))
                     (new-manifest
                       (append
                         (remove
                           (lambda (pair)
                             (eq? (car pair) 'sha256))
                           manifest)
                         (list (cons 'sha256 pins)))))
                  (call-with-output-file
                    (string-append copy-dir "/manifest.scm")
                    (lambda (port)
                      (write new-manifest port))))
                (let*
                    ((dist-dir (string-append (canonicalize-path ".") "/dist"))
                     (tarball-name (format #f "badb-geis-~a-~a.tar.gz" feature-id version))
                     (tarball-path (string-append dist-dir "/" tarball-name))
                     (bundle-hash (geis-bundle-hash copy-dir)))
                  (%mkdir-p dist-dir)
                  (system* "tar" "-czf" tarball-path "-C" temp-parent (basename copy-dir))
                  (call-with-output-file
                    (string-append tarball-path ".sha256")
                    (lambda (port)
                      (display bundle-hash port)
                      (newline port)))
                  (let
                      ((res (list (cons 'tarball tarball-path) (cons 'bundle-sha256 bundle-hash))))
                    (%delete-tree! temp-parent)
                    res)))
              (lambda (key . args)
                (catch #t
                  (lambda ()
                    (%delete-tree! temp-parent))
                  (lambda _
                    #f))
                (apply throw key args))))))

;;;
;;; Bundle installation
;;;
(define (geis-bundle-install bundle-dir)
  "Install the bundle at BUNDLE-DIR: validate its manifest, resolve its
geis dependencies, lock every dependency and the target (dependencies
first) into the gate's read-only store, verify the target against the
locked copy, and return a report alist:
((feature-id . fid) (bundle-sha256 . hash) (locked . (fid ...))
 (verify-verdict . \"pass\"|\"fail\") (verify-reason . \"...\")).
Throw 'geis-bundle-error when validation fails."
  (let ((manifest (read-geis-manifest
                   (string-append bundle-dir "/manifest.scm"))))
    (unless manifest
      (throw 'geis-bundle-error
             (format #f "invalid manifest in bundle at '~a'" bundle-dir)))
    (let ((errors (geis-manifest-valid? bundle-dir)))
      (unless (null? errors)
        (throw 'geis-bundle-error
               (format #f "manifest validation failed: ~a"
                       (string-join errors "; ")))))
    (let* ((fid (manifest-field manifest 'feature-id))
           (dep-dirs (resolve-geis-load-path fid))
           (target-hash (geis-bundle-hash bundle-dir))
           (locked-fids '()))
      (for-each
       (lambda (dir)
         (let* ((dep-manifest (read-geis-manifest
                               (string-append dir "/manifest.scm")))
                (dep-fid (manifest-field dep-manifest 'feature-id)))
           (lock-geis-bundle (%clean-path dir) dep-fid)
           (set! locked-fids (append locked-fids (list dep-fid)))))
       dep-dirs)
      (let ((verdict (verify-locked-geis-bundle fid)))
        (list (cons 'feature-id fid)
              (cons 'bundle-sha256 target-hash)
              (cons 'locked locked-fids)
              (cons 'verify-verdict (assoc-ref verdict 'verdict))
              (cons 'verify-reason (assoc-ref verdict 'reason)))))))
(define (%list-dir d)
        " scandir without the dot entries; empty list on error."
        (or
          (catch #t
            (lambda ()
              (scandir d
                       (lambda (n)
                         (not (member n '("." ".."))))))
            (lambda _
              #f))
          '()))

(define (%copy-into! src dst)
        "Copy file SRC to DST creating parent dirs; #f when SRC is #f or missing."
        (if (and src (file-exists? src))
          (begin (%mkdir-p (dirname dst)) (copy-file src dst) #t)
          #f))

(define (%manifest-with manifest key value)
        "Return MANIFEST with KEY bound to VALUE (replacing any prior binding)."
        (append
          (remove
            (lambda (pair)
              (eq? (car pair) key))
            manifest)
          (list (cons key value))))

(define
  (geis-capture-pending-prompt! request rendered)
  "Capture a /geis add request and its rendered prompt into the gate's
pending store (<gate-root>/geis-pending.json) for the bundle assembly
step to pick up.  Defensive: returns #f instead of raising when the
store cannot be written."
  (catch #t
    (lambda ()
      (%mkdir-p (geis-gate-root))
      (let
          ((path (string-append (geis-gate-root) "/geis-pending.json")))
        (call-with-output-file path
                               (lambda (port)
                                 (display
                                   (scm->json-string
                                     (list (cons 'request request)
                                           (cons 'rendered rendered)
                                           (cons 'timestamp (number->string (get-internal-real-time)))))
                                   port)))
        #t))
    (lambda _
      #f)))

(define (geis-take-pending-prompt)
        "Read and consume the pending /geis add prompt store.  Returns an alist
with request and rendered fields, or #f when absent."
        (let
            ((path (string-append (geis-gate-root) "/geis-pending.json")))
          (if (file-exists? path)
            (let
                ((pending
                   (catch #t
                     (lambda ()
                       (call-with-input-file path
                                             (lambda (port)
                                               (json-string->scm (get-string-all port)))))
                     (lambda _
                       #f))))
              (catch #t
                (lambda ()
                  (delete-file path))
                (lambda _
                  #f))
              (and (pair? pending)
                   (list
                     (cons 'request (assoc-ref pending "request"))
                     (cons 'rendered (assoc-ref pending "rendered")))))
            #f)))

(define*
  (geis-bundle-assemble! feature-id
                         #:key
                         (workflow-path #f)
                         (cassette-path #f)
                         (request #f)
                         (rendered-prompt #f)
                         (synthesis-prompt #f)
                         (target-path #f)
                         (bundle-dir #f))
  "Assemble provenance artifacts into the bundle for FEATURE-ID: copy the
locked spec (when the bundle has no spec yet), the synthesis workflow,
the captured prompts (request, rendered /geis add prompt, synthesis
invocation record) and the cassette; then refresh the manifest (fields,
provenance, sha256 pins) and validate it.  Returns the manifest alist;
throws 'geis-bundle-error on validation failure."
  (let*
      ((fid (%fid->string feature-id))
       (dir (or bundle-dir (string-append (car (geis-bundle-search-path)) "/" fid))))
    (%mkdir-p dir)
    (unless
        (file-exists? (string-append dir "/spec.geis.scm"))
      (let ((locked (locked-spec-path fid)))
        (when (file-exists? locked)
          (%copy-into! locked (string-append dir "/spec.geis.scm")))))
    (let*
        ((wf-captured? (%copy-into! workflow-path (string-append dir "/workflow.sema")))
         (cass-name (and cassette-path (basename cassette-path)))
         (cass-captured?
           (and cass-name
                (%copy-into! cassette-path (string-append dir "/cassettes/" cass-name))))
         (prompts-dir (string-append dir "/prompts")))
      (%mkdir-p prompts-dir)
      (when request
        (call-with-output-file
          (string-append prompts-dir "/request.md")
          (lambda (port)
            (display request port))))
      (when rendered-prompt
        (call-with-output-file
          (string-append prompts-dir "/geis-add.md")
          (lambda (port)
            (display rendered-prompt port))))
      (when synthesis-prompt
        (call-with-output-file
          (string-append prompts-dir "/synthesis.md")
          (lambda (port)
            (display synthesis-prompt port))))
      (let*
          ((manifest-path (string-append dir "/manifest.scm"))
           (base
             (or (read-geis-manifest manifest-path)
                 (list (cons 'bundle-version 1)
                       (cons 'feature-id fid)
                       (cons 'version "1.0.0")
                       (cons 'spec "spec.geis.scm"))))
           (m1
             (if wf-captured?
               (%manifest-with base 'workflow "workflow.sema")
               base))
           (m2
             (if cass-captured?
               (%manifest-with m1 'cassettes "cassettes")
               m1))
           (prompt-files
             (filter
               (lambda (p)
                 (file-exists? (string-append dir "/" p)))
               (list "prompts/request.md" "prompts/geis-add.md" "prompts/synthesis.md")))
           (m3
             (if (pair? prompt-files)
               (%manifest-with m2 'prompts prompt-files)
               m2))
           (old-prov (or (manifest-field m3 'provenance) '()))
           (prov1
             (append
               (remove
                 (lambda (p)
                   (eq? (car p) 'origin))
                 old-prov)
               (list (cons 'origin "agent-synthesized"))))
           (prov2
             (if target-path
               (append
                 (remove
                   (lambda (p)
                     (eq? (car p) 'target-path))
                   prov1)
                 (list (cons 'target-path target-path)))
               prov1))
           (prov3
             (if cass-captured?
               (append
                 (remove
                   (lambda (p)
                     (eq? (car p) 'cassette))
                   prov2)
                 (list (cons 'cassette (string-append "cassettes/" cass-name))))
               prov2))
           (m4 (%manifest-with m3 'provenance prov3))
           (m5 (%manifest-with m4 'sha256 (compute-geis-sha256-pins dir))))
        (call-with-output-file manifest-path
                               (lambda (port)
                                 (write m5 port)
                                 (newline port)))
        (let ((errors (geis-manifest-valid? dir)))
          (unless (null? errors)
            (throw 'geis-bundle-error
                   (format #f "assembled manifest invalid: ~a" (string-join errors "; ")))))
        m5))))

(define*
  (geis-bundle-replay feature-id #:key (timeout 120) (target-path #f))
  "Replay the synthesis workflow for FEATURE-ID in cassette mode against
its locked contract, with the replay target taken from provenance's
target-path or the bundle's plugin.scm, and compare the re-synthesized
content against the pinned plugin.  Returns a report alist:
((feature-id ...) (workflow-verdict ...) (plugin-match ...) (pinned-hash ...)
 (replayed-hash ...)).  Note: the recorded synthesis target file is
modified by the replay (cassette mode re-runs the edits)."
  (let*
      ((fid (%fid->string feature-id)) (dir (find-geis-bundle fid)))
    (if dir
      (let*
          ((manifest (read-geis-manifest (string-append dir "/manifest.scm")))
           (wf (manifest-field manifest 'workflow))
           (cass-dir (manifest-field manifest 'cassettes))
           (prov (manifest-field manifest 'provenance))
           (recorded-target (and (list? prov) (manifest-field prov 'target-path))))
        (if (and (string? wf) (string? cass-dir))
          (let*
              ((cass-files (%list-dir (string-append dir "/" cass-dir)))
               (cassette
                 (and (pair? cass-files) (string-append dir "/" cass-dir "/" (car cass-files))))
               (spec-path (string-append dir "/spec.geis.scm"))
               (plugin-path (string-append dir "/plugin.scm"))
               (has-plugin? (file-exists? plugin-path))
               (target
                 (or target-path
                     recorded-target
                     (and has-plugin?
                          (let*
                              ((tmp (mkdtemp "/tmp/badb-replay-XXXXXX"))
                               (dst (string-append tmp "/plugin.scm")))
                            (%copy-into! plugin-path dst)
                            dst))))
               (run-dir (mkdtemp "/tmp/badb-replay-run-XXXXXX")))
            (if (and cassette target)
              (let*
                  ((res
                     (run-synthesis-workflow fid
                                             #:spec-path
                                             spec-path
                                             #:target-path
                                             target
                                             #:workflow-path
                                             (string-append dir "/" wf)
                                             ;; Replay reproduces the pinned
                                             ;; synthesis with the DEFAULT
                                             ;; synthesis persona (identity +
                                             ;; directives from the
                                             ;; persona-synthesizer geasa),
                                             ;; independent of local config.
                                             #:persona
                                             (persona-prompt-text
                                              "persona-synthesizer")
                                             #:cassette
                                             cassette
                                             #:cassette-mode
                                             "replay"
                                             #:run-dir
                                             run-dir))
                   (wf-result (assoc-ref res 'result))
                   (verdict
                     (or
                       (and (list? wf-result) (assoc-ref wf-result "verdict"))
                       (assoc-ref res 'status)))
                   (replayed-hash (sha256-hex (%read-file-string target)))
                   (pinned-hash
                     (if has-plugin?
                       (sha256-hex (%read-file-string plugin-path))
                       #f))
                   (match
                     (cond
                       ((not has-plugin?) "n/a (kernel-referenced bundle)")
                       ((string= replayed-hash pinned-hash) #t)
                       (else #f))))
                (list (cons 'feature-id fid)
                      (cons 'workflow-verdict verdict)
                      (cons 'plugin-match match)
                      (cons 'pinned-hash pinned-hash)
                      (cons 'replayed-hash replayed-hash)))
              (list (cons 'feature-id fid)
                    (cons 'workflow-verdict "fail")
                    (cons 'plugin-match "n/a")
                    (cons 'pinned-hash #f)
                    (cons 'replayed-hash #f)
                    (cons 'reason
                          (if cassette
                            "no synthesis target recorded"
                            "no cassette recorded")))))
          (list (cons 'feature-id fid)
                (cons 'workflow-verdict "fail")
                (cons 'plugin-match "n/a")
                (cons 'pinned-hash #f)
                (cons 'replayed-hash #f)
                (cons 'reason "bundle records no workflow or cassettes"))))
      (throw 'geis-bundle-error
             (format #f "bundle not found for feature-id '~a'" fid)))))