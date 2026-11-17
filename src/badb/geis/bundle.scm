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
               #:use-module
               (srfi srfi-234)
               #:use-module
               (srfi srfi-11)
               #:use-module
               (semver)
               #:use-module
               (semver ranges)
               #:use-module
               (json)
               #:use-module
               (badb kernel geis-gate)
               #:export
               (user-geis-store-dir
                 geis-bundle-search-path find-geis-bundle
                 read-geis-manifest
                 manifest-field
                 geis-manifest-valid?
                 geis-manifest-capabilities-valid?
                 geis-bundle-files
                 compute-geis-sha256-pins
                 geis-bundle-hash
                 resolve-geis-load-path
                 geis-bundle-pack
                 geis-pack-base-collection
                 geis-bundle-install
                 geis-capture-pending-prompt!
                 geis-take-pending-prompt
                 geis-bundle-assemble!
                 geis-bundle-replay
                 geis-bundle-pin!
                 geis-bundle-ccache-dir
                 geis-compile-bundle!
                 geis-bootstrap-base!))

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

(define (%valid-capability? capability)
        (and (list? capability)
             (= (length capability) 2)
             (memq (car capability) '(fs-read fs-write net-http))
             (string? (cadr capability))
             (not (string-null? (cadr capability)))))

(define (geis-manifest-capabilities-valid? manifest)
        "Return #t when optional capabilities are a duplicate-free list of
(kind target) requests using the supported capability kinds."
        (let ((capabilities (manifest-field manifest 'capabilities)))
          (or (not capabilities)
              (and (list? capabilities)
                   (every %valid-capability? capabilities)
                   (= (length capabilities) (length (delete-duplicates capabilities equal?)))))))

;;;
;;; Search path and discovery
;;;
(define (%home-dir)
        "Return the user's home directory, or #f when it cannot be determined."
        (let ((home (getenv "HOME")))
          (if (and home (not (string-null? home)))
            home
            (let ((pw (catch #t (lambda () (getpwnam "root")) (lambda _ #f))))
              (and pw (dirname (pw:dir pw)))))))

(define (user-geis-store-dir)
        "Return the user-level geasa store directory.
Honors BADB_USER_GEIS_DIR, then $XDG_DATA_HOME/badb/geasa, defaulting to
~/.local/share/badb/geasa."
        (let ((explicit (getenv "BADB_USER_GEIS_DIR")))
          (if (and explicit (not (string-null? explicit)))
            explicit
            (let* ((data (getenv "XDG_DATA_HOME"))
                   (base (if (and data (not (string-null? data)))
                           data
                           (let ((h (%home-dir)))
                             (if h (string-append h "/.local/share") ".local/share")))))
              (string-append base "/badb/geasa")))))

(define (%load-path-geasa-dirs)
        "Return '<dir>/geasa' directories that exist in %load-path."
        (filter file-exists?
                (map (lambda (lp) (string-append lp "/geasa")) %load-path)))

(define (geis-bundle-search-path)
        "Return the bundle search roots: BADB_GEIS_PATH split on colons when
set and non-empty; otherwise 'geis', the user geasa store, 'src/geasa',
and any '<load-path>/geasa' directories present in %load-path."
        (let ((path (getenv "BADB_GEIS_PATH")))
          (if (or (not path) (string-null? path))
            (append (list "geis" (user-geis-store-dir) "src/geasa")
                    (%load-path-geasa-dirs))
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

(define
  (%validate-relative-member! add! bundle-dir path path-label member-label)
  (unless (%safe-relative-path? path)
    (add! (format #f "~a '~a' is not a safe relative path" path-label path)))
  (unless (%bundle-member-exists? bundle-dir path)
    (add! (format #f "~a '~a' does not exist in bundle" member-label path))))

(define
  (%validate-feature-id-field! add! bundle-dir manifest)
  (let
      ((fid (manifest-field manifest 'feature-id)))
    (unless (%valid-feature-id? fid)
      (add! (format #f "feature-id '~a' is missing or not sanitized" fid)))
    (when
        (and (string? fid) (not (string= fid (basename bundle-dir))))
      (add!
        (format #f
                "feature-id '~a' does not match directory name '~a'"
                fid
                (basename bundle-dir))))))

(define
  (%validate-plugin-field! add! bundle-dir plugin)
  (when plugin
    (let
        ((module (and (list? plugin) (manifest-field plugin 'module)))
         (file (and (list? plugin) (manifest-field plugin 'file))))
      (unless
          (and (list? module) (every symbol? module))
        (add! "plugin field must carry (module . list-of-symbols)"))
      (if (string? file)
        (%validate-relative-member! add!
                                    bundle-dir
                                    file
                                    "plugin file path"
                                    "plugin file")
        (add! "plugin field must carry (file . string)")))))

(define
  (%validate-prompts-field! add! bundle-dir prompts)
  (when prompts
    (unless
        (and (list? prompts) (every string? prompts))
      (add! "prompts field must be a list of file paths"))
    (for-each
      (lambda (p)
        (%validate-relative-member! add! bundle-dir p "prompt path" "prompt file"))
      (if (list? prompts)
        prompts
        '()))))

(define (%validate-tools-field! add! tools)
        (when
            (and tools
                 (not
                   (and (list? tools)
                        (every
                          (lambda (t)
                            (or (string? t)
                                (symbol? t)
                                (and (pair? t) (or (symbol? (car t)) (string? (car t))))))
                          tools))))
          (add! "tools field must be a list of tool names or specifications")))

(define (geis-manifest-valid? bundle-dir)
        "Validate the bundle at BUNDLE-DIR.  Return '() when the manifest is valid,
otherwise a non-empty list of human-readable reason strings."
        (let
            ((errors-cell (list '()))
             (manifest (read-geis-manifest (string-append bundle-dir "/manifest.scm"))))
          (define (add! reason) (set-car! errors-cell (cons reason (car errors-cell))))
          (if manifest
            (begin
              (unless
                  (equal? 1 (manifest-field manifest 'bundle-version))
                (add! "bundle-version must be exactly 1"))
              (%validate-feature-id-field! add! bundle-dir manifest)
              (let ((ver (manifest-field manifest 'version)))
                (unless
                    (and (string? ver) (not (string-null? ver)))
                  (add! "version must be a non-empty string")))
              (let ((spec (manifest-field manifest 'spec)))
                (if spec
                  (when (string? spec)
                    (%validate-relative-member! add! bundle-dir spec "spec path" "spec file"))
                  (add! "spec field is missing")))
              (%validate-plugin-field! add! bundle-dir (manifest-field manifest 'plugin))
              (let
                  ((workflow (manifest-field manifest 'workflow)))
                (when workflow
                  (%validate-relative-member! add!
                                              bundle-dir
                                              workflow
                                              "workflow path"
                                              "workflow file")))
              (%validate-prompts-field! add! bundle-dir (manifest-field manifest 'prompts))
              (let
                  ((fixtures (manifest-field manifest 'fixtures)))
                (when fixtures
                  (%validate-relative-member! add!
                                              bundle-dir
                                              fixtures
                                              "fixtures path"
                                              "fixtures directory")))
              (let
                  ((cassettes (manifest-field manifest 'cassettes)))
                (when cassettes
                  (%validate-relative-member! add!
                                              bundle-dir
                                              cassettes
                                              "cassettes path"
                                              "cassettes directory")))
              (%validate-tools-field! add! (manifest-field manifest 'tools))
              (unless
                  (geis-manifest-capabilities-valid? manifest)
                (add! "capabilities must be unique (fs-read|fs-write|net-http target) requests"))
              (let
                  ((wf-exports (manifest-field manifest 'workflow-exports)))
                (when
                    (and wf-exports (not (and (list? wf-exports) (every string? wf-exports))))
                  (add! "workflow-exports field must be a list of workflow name strings")))
              (let ((precompile (assoc 'precompile manifest)))
                (when (and precompile (not (boolean? (cdr precompile))))
                  (add! "precompile field must be a boolean")))
              (let ((pins (manifest-field manifest 'sha256)))
                (when pins
                  (if (list? pins)
                    (%validate-sha256-pins! errors-cell bundle-dir pins)
                    (add! "sha256 field must be a list of (file . hash) pins")))))
            (add! "manifest.scm is missing or unreadable"))
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
                  (and (string? want-ver) (string? have-ver) (%semver-match? have-ver want-ver))
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

(define (%semver-match? have want)
        "Return #t if version HAVE satisfies version constraint WANT using semver."
        (catch #t
          (lambda ()
            (let ((have-sv (string->semver have)))
              (or (and (semver? have-sv)
                       (let ((want-sv (catch #t (lambda () (string->semver want)) (lambda _ #f))))
                         (and want-sv (semver=? have-sv want-sv))))
                  (let ((range (string->semver-range want)))
                    (semver-range-contains? range have-sv)))))
          (lambda _
            (and (string? have) (string? want) (string= have want)))))

(define (resolve-geis-load-path feature-id)
        "Return the absolute bundle directories FEATURE-ID needs, dependencies
first and the target bundle last.  Throw 'geis-bundle-error on missing
bundles, invalid manifests, version mismatches, bundle-sha256 mismatches,
or dependency cycles.  Shared dependencies appear exactly once."
        (let* ((fid (%fid->string feature-id))
               (dir (find-geis-bundle fid)))
          (unless dir
            (throw 'geis-bundle-error (format #f "missing bundle '~a'" fid)))
          (let ((manifest (read-geis-manifest (string-append dir "/manifest.scm"))))
            (unless manifest
              (throw 'geis-bundle-error (format #f "invalid manifest for bundle '~a'" fid)))

            ;; Build dependency graph in SRFI-234 format: ((node (dep1 dep2 ...)) ...)
            (let ((graph '()))
              (letrec ((collect! (lambda (current-fid)
                                   (let* ((current-str (%fid->string current-fid))
                                          (current-dir (find-geis-bundle current-str)))
                                     (unless current-dir
                                       (throw 'geis-bundle-error
                                              (format #f "missing dependency bundle '~a'" current-str)))
                                     (let ((current-manifest (read-geis-manifest (string-append current-dir "/manifest.scm"))))
                                       (unless current-manifest
                                         (throw 'geis-bundle-error (format #f "invalid manifest for bundle '~a'" current-str)))

                                       ;; Add current node to graph if not already present
                                       (unless (assoc current-str graph)
                                         (set! graph (cons (list current-str) graph)))

                                       ;; Process dependencies
                                       (for-each
                                         (lambda (dep)
                                           (let* ((dep-fid (and (pair? dep) (car dep)))
                                                  (spec (and (pair? dep) (cdr dep)))
                                                  (dep-str (%fid->string dep-fid)))
                                             (%verify-dep-edge! dep-fid spec)
                                             (unless (assoc dep-str graph)
                                               (collect! dep-fid))
                                             ;; Add dependency to current node's neighbor list
                                             (let ((node-entry (assoc current-str graph)))
                                               (unless (member dep-str (cdr node-entry))
                                                 (set-cdr! node-entry (cons dep-str (cdr node-entry)))))))
                                         (%geis-deps current-manifest)))))))

                      (collect! fid)

                      ;; Perform topological sort
                      (let-values (((result cycle? cycle-nodes) (topological-sort/details graph)))
                                  (if cycle?
                                    ;; Map cycle detection to existing error format
                                    (let ((cycle-fid (car cycle-nodes)))
                                      (throw 'geis-bundle-error (format #f "dependency cycle detected at '~a'" cycle-fid)))
                                    ;; Success: reverse to get dependencies first, then map to directories
                                    (map (lambda (node-fid)
                                           (canonicalize-path (find-geis-bundle node-fid)))
                                         (reverse result)))))))))

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
                  (let ((status (system* "tar" "-czf" tarball-path "-C" temp-parent (basename copy-dir))))
                    (unless (and (number? status) (zero? (status:exit-val status)))
                      (throw 'geis-bundle-error
                             (format #f "failed to create bundle tarball '~a' (tar exit: ~a)"
                                     tarball-path status))))
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

(define* (geis-pack-base-collection #:key (source-dir "src/geasa") (version "0.1.0") (precompile? #t))
         "Pack all valid bundles under SOURCE-DIR into dist/badb-base-geasa-<version>.tar.gz
with a manifest-index.scm mapping each feature-id to its version, bundle-sha256,
and dependencies (plus workflows/plugin-synthesis.sema when present).
Returns ((tarball . path) (bundle-count . n) (index . entries))."
         (unless (%directory? source-dir)
           (throw 'geis-bundle-error
                  (format #f "base geasa directory not found: '~a'" source-dir)))
         (let* ((temp-parent (mkdtemp "/tmp/badb-base-pack-XXXXXX"))
                (stage-dir (string-append temp-parent "/badb-base-geasa"))
                (geasa-stage (string-append stage-dir "/geasa"))
                (entries (sort (or (scandir source-dir
                                            (lambda (n)
                                              (and (not (member n '("." "..")))
                                                   (%directory? (string-append source-dir "/" n))
                                                   (file-exists? (string-append source-dir "/" n "/manifest.scm")))))
                                   '())
                               string<?))
                (index-entries '()))
           (catch #t
             (lambda ()
               (%mkdir-p geasa-stage)
               (for-each
                 (lambda (fid)
                   (let* ((src-bundle (string-append source-dir "/" fid))
                          (errors (geis-manifest-valid? src-bundle)))
                     (unless (null? errors)
                       (throw 'geis-bundle-error
                              (format #f "manifest validation failed for '~a': ~a"
                                      fid (string-join errors "; "))))
                     (let* ((manifest (read-geis-manifest (string-append src-bundle "/manifest.scm")))
                            (ver (manifest-field manifest 'version))
                            (deps (map (lambda (d) (%fid->string (car d))) (%geis-deps manifest)))
                            (dst-bundle (string-append geasa-stage "/" fid)))
                       (mkdir dst-bundle)
                       (for-each
                         (lambda (rel)
                           (let ((dst (string-append dst-bundle "/" rel)))
                             (%mkdir-p (dirname dst))
                             (copy-file (string-append src-bundle "/" rel) dst)))
                         (geis-bundle-files src-bundle))
                       (let* ((pins (compute-geis-sha256-pins dst-bundle))
                              (m1 (append (remove (lambda (pair) (memq (car pair) '(sha256 precompile))) manifest)
                                          (if precompile? (list (cons 'precompile #t)) '())
                                          (list (cons 'sha256 pins))))
                              (bhash (geis-bundle-hash dst-bundle)))
                         (call-with-output-file
                           (string-append dst-bundle "/manifest.scm")
                           (lambda (port) (write m1 port)))
                         (set! index-entries
                               (cons (list (cons 'feature-id fid)
                                           (cons 'version ver)
                                           (cons 'bundle-sha256 bhash)
                                           (cons 'depends deps))
                                     index-entries))))))
                 entries)
               (when (file-exists? "workflows/plugin-synthesis.sema")
                 (%mkdir-p (string-append stage-dir "/workflows"))
                 (copy-file "workflows/plugin-synthesis.sema"
                            (string-append stage-dir "/workflows/plugin-synthesis.sema")))
               (let* ((ordered-index (reverse index-entries))
                      (dist-dir (string-append (canonicalize-path ".") "/dist"))
                      (tarball-name (format #f "badb-base-geasa-~a.tar.gz" version))
                      (tarball-path (string-append dist-dir "/" tarball-name)))
                 (call-with-output-file
                   (string-append stage-dir "/manifest-index.scm")
                   (lambda (port) (write ordered-index port)))
                 (%mkdir-p dist-dir)
                 (let ((status (system* "tar" "-czf" tarball-path "-C" temp-parent "badb-base-geasa")))
                   (unless (and (number? status) (zero? (status:exit-val status)))
                     (throw 'geis-bundle-error
                            (format #f "failed to create base geasa tarball '~a'" tarball-path))))
                 (call-with-output-file
                   (string-append tarball-path ".sha256")
                   (lambda (port)
                     (display (sha256-hex (%read-file-string tarball-path)) port)
                     (newline port)))
                 (%delete-tree! temp-parent)
                 (list (cons 'tarball tarball-path)
                       (cons 'bundle-count (length ordered-index))
                       (cons 'index ordered-index))))
             (lambda (key . args)
               (catch #t (lambda () (%delete-tree! temp-parent)) (lambda _ #f))
               (apply throw key args)))))

;;;
;;; Bundle installation
;;;
(define (geis-bundle-ccache-dir bundle-hash)
        "Return the ccache directory for BUNDLE-HASH under BADB_CCACHE_DIR or
$XDG_CACHE_HOME/badb/ccache."
        (let* ((explicit (getenv "BADB_CCACHE_DIR"))
               (base (if (and explicit (not (string-null? explicit)))
                       explicit
                       (let ((xdg (getenv "XDG_CACHE_HOME"))
                             (home (getenv "HOME")))
                         (string-append
                           (cond
                             ((and xdg (not (string-null? xdg))) xdg)
                             ((and home (not (string-null? home))) (string-append home "/.cache"))
                             (else ".cache"))
                           "/badb/ccache")))))
          (string-append base "/" bundle-hash)))

(define (geis-compile-bundle! bundle-dir bundle-hash fid)
        "Compile all .scm plugin/library modules in BUNDLE-DIR (excluding
manifest.scm, spec.geis.scm, and fixtures/) into
<ccache-root>/<bundle-hash>/geasa/<fid>/ using `guild compile'.
Returns the list of compiled .go paths."
        (let* ((ccache-root (geis-bundle-ccache-dir bundle-hash))
               (out-dir (string-append ccache-root "/geasa/" fid))
               (scm-files
                 (filter (lambda (rel)
                           (and (string-suffix? ".scm" rel)
                                (not (string=? rel "spec.geis.scm"))
                                (not (string-prefix? "fixtures/" rel))))
                         (geis-bundle-files bundle-dir)))
               ;; guild does not inherit the in-process %load-path: pass (badb ...)
               ;; in src/ and sibling bundles in the store parent explicitly.
               (l-flags (append-map
                          (lambda (dir) (list "-L" dir))
                          (delete-duplicates
                            (append (list (dirname (user-geis-store-dir)) "src" "." "geis")
                                    %load-path))))
               (compiled '()))
          (%mkdir-p out-dir)
          (for-each
            (lambda (rel)
              (let* ((src (string-append bundle-dir "/" rel))
                     (base (if (string-suffix? ".scm" rel)
                             (substring rel 0 (- (string-length rel) 4))
                             rel))
                     (dst (string-append out-dir "/" base ".go")))
                (%mkdir-p (dirname dst))
                (let ((status (apply system* "guild" "compile"
                                     (append l-flags (list "-o" dst src)))))
                  (if (and (number? status) (zero? (status:exit-val status)))
                    (set! compiled (cons dst compiled))
                    (format (current-error-port)
                            "warning: failed to precompile ~a/~a~%" fid rel)))))
            scm-files)
          (reverse compiled)))

(define* (geis-bundle-install bundle-dir #:key (compile? #f))
         "Install the bundle at BUNDLE-DIR: validate its manifest, resolve its
geis dependencies, lock every dependency and the target (dependencies
first) into the gate's read-only store, verify the target against the
locked copy, and return a report alist:
((feature-id . fid) (bundle-sha256 . hash) (locked . (fid ...))
 (verify-verdict . \"pass\"|\"fail\") (verify-reason . \"...\")
 (compiled . (go-path ...))).
When VERIFY-VERDICT is \"pass\" and either COMPILE? is #t or the manifest
sets (precompile . #t), pre-compile the bundle's .scm modules into
$XDG_CACHE_HOME/badb/ccache/<bundle-sha256>/.
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
             (let* ((verdict (verify-locked-geis-bundle
                               fid
                               #:timeout 120
                               #:load-paths (list (dirname (user-geis-store-dir)) "src" "." "geis")))
                    (verdict-str (assoc-ref verdict 'verdict))
                    (should-compile? (and (equal? verdict-str "pass")
                                          (or compile?
                                              (eq? #t (manifest-field manifest 'precompile)))))
                    (compiled-files (if should-compile?
                                      (geis-compile-bundle! bundle-dir target-hash fid)
                                      '())))
               (list (cons 'feature-id fid)
                     (cons 'bundle-sha256 target-hash)
                     (cons 'locked locked-fids)
                     (cons 'verify-verdict verdict-str)
                     (cons 'verify-reason (assoc-ref verdict 'reason))
                     (cons 'verify-failures (assoc-ref verdict 'failures))
                     (cons 'compiled compiled-files))))))

(define* (geis-bootstrap-base! #:key (from #f) (no-compile? #f))
         "Bootstrap base geasa bundles from FROM (a directory or .tar.gz archive,
defaulting to $BADB_BOOTSTRAP_SOURCE or \"src/geasa\") into (user-geis-store-dir),
validating, locking, verifying, and precompiling (unless NO-COMPILE? is #t).
Returns ((store . dir) (installed . (fid ...)) (failed . (fid ...)))."
         (let* ((source (or from (getenv "BADB_BOOTSTRAP_SOURCE") "src/geasa"))
                (temp-dir #f)
                (store (user-geis-store-dir))
                (user-root (dirname store)))
           (catch #t
             (lambda ()
               (let ((geasa-src
                       (cond
                         ((%directory? source) source)
                         ((and (file-exists? source) (string-suffix? ".tar.gz" source))
                           (set! temp-dir (mkdtemp "/tmp/badb-bootstrap-XXXXXX"))
                           (let ((st (system* "tar" "-xzf" source "-C" temp-dir)))
                             (unless (and (number? st) (zero? (status:exit-val st)))
                               (throw 'geis-bundle-error
                                      (format #f "failed to extract bootstrap archive '~a'" source))))
                           (let ((nested (string-append temp-dir "/badb-base-geasa/geasa"))
                                 (wf (string-append temp-dir "/badb-base-geasa/workflows/plugin-synthesis.sema")))
                             (when (file-exists? wf)
                               (%mkdir-p (string-append user-root "/workflows"))
                               (copy-file wf (string-append user-root "/workflows/plugin-synthesis.sema")))
                             (if (%directory? nested) nested temp-dir)))
                         (else
                           (throw 'geis-bundle-error
                                  (format #f "bootstrap source not found: '~a'" source))))))
                 (%mkdir-p store)
                 (let ((fids (sort (or (scandir geasa-src
                                                (lambda (n)
                                                  (and (not (member n '("." "..")))
                                                       (%directory? (string-append geasa-src "/" n))
                                                       (file-exists? (string-append geasa-src "/" n "/manifest.scm")))))
                                       '())
                                   string<?))
                       (installed '())
                       (failed '()))
                   (for-each
                     (lambda (fid)
                       (let ((src-b (string-append geasa-src "/" fid))
                             (dst-b (string-append store "/" fid)))
                         (when (file-exists? dst-b) (%delete-tree! dst-b))
                         (%mkdir-p dst-b)
                         (for-each
                           (lambda (rel)
                             (let ((dst (string-append dst-b "/" rel)))
                               (%mkdir-p (dirname dst))
                               (copy-file (string-append src-b "/" rel) dst)))
                           (geis-bundle-files src-b))
                         (copy-file (string-append src-b "/manifest.scm")
                                    (string-append dst-b "/manifest.scm"))))
                     fids)
                   (for-each
                     (lambda (fid)
                       (let ((dst-b (string-append store "/" fid)))
                         (catch #t
                           (lambda ()
                             (let ((rep (geis-bundle-install dst-b #:compile? (not no-compile?))))
                               (if (equal? (assoc-ref rep 'verify-verdict) "pass")
                                 (set! installed (cons fid installed))
                                 (begin
                                   (format (current-error-port) "bootstrap: ~a failed verification: ~a~%~s~%"
                                           fid (assoc-ref rep 'verify-reason) (assoc-ref rep 'verify-failures))
                                   (set! failed (cons fid failed))))))
                           (lambda (key . args)
                             (format (current-error-port) "bootstrap: ~a failed: ~s ~s~%" fid key args)
                             (set! failed (cons fid failed))))))
                     fids)
                   (when temp-dir (%delete-tree! temp-dir))
                   (list (cons 'store store)
                         (cons 'installed (reverse installed))
                         (cons 'failed (reverse failed))))))
             (lambda (key . args)
               (when temp-dir (catch #t (lambda () (%delete-tree! temp-dir)) (lambda _ #f)))
               (apply throw key args)))))
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
  (geis-bundle-replay feature-id
                      #:key
                      (timeout 120)
                      (target-path #f)
                      (workflow-runner #f)
                      (persona-resolver #f))
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
               (has-plugin? (file-exists? plugin-path)))
            (if
                (and cassette (or target-path recorded-target has-plugin?))
              (let*
                  ((tmp-dir
                     (and (not target-path)
                          (not recorded-target)
                          has-plugin?
                          (mkdtemp "/tmp/badb-replay-XXXXXX")))
                   (target (or target-path recorded-target (string-append tmp-dir "/plugin.scm")))
                   (run-dir (mkdtemp "/tmp/badb-replay-run-XXXXXX"))
                   (run-wf
                     (or workflow-runner
                         (module-ref
                           (resolve-interface '(geasa runtimes-synthesis plugin))
                           'run-synthesis-workflow)))
                   (resolve-persona
                     (or persona-resolver
                         (module-ref
                           (resolve-interface '(geasa runtimes-synthesis plugin))
                           'persona-prompt-text))))
                (dynamic-wind
                  (lambda ()
                    (when tmp-dir (%copy-into! plugin-path target)))
                  (lambda ()
                    (let*
                        ((res
                           (run-wf fid
                                   #:spec-path
                                   spec-path
                                   #:target-path
                                   target
                                   #:workflow-path
                                   (string-append dir "/" wf)
                                   #:persona
                                   (resolve-persona "persona-synthesizer")
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
                            (cons 'replayed-hash replayed-hash))))
                  (lambda ()
                    (%delete-tree! run-dir)
                    (when tmp-dir (%delete-tree! tmp-dir)))))
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

(define (geis-bundle-pin! bundle-dir)
        "Recompute sha256 pins for BUNDLE-DIR, update its manifest.scm, and validate."
        (let ((manifest-path (string-append bundle-dir "/manifest.scm")))
          (unless (file-exists? manifest-path)
            (throw 'geis-bundle-error (format #f "manifest not found at '~a'" manifest-path)))
          (let ((manifest (read-geis-manifest manifest-path)))
            (unless manifest
              (throw 'geis-bundle-error (format #f "failed to read manifest at '~a'" manifest-path)))
            (let* ((pins (compute-geis-sha256-pins bundle-dir))
                   (updated (%manifest-with manifest 'sha256 pins)))
              (call-with-output-file manifest-path
                                     (lambda (port)
                                       (write updated port)
                                       (newline port)))
              (let ((errors (geis-manifest-valid? bundle-dir)))
                (unless (null? errors)
                  (throw 'geis-bundle-error
                         (format #f "manifest invalid after re-pinning '~a': ~s" bundle-dir errors)))
                pins)))))
