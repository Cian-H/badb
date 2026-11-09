;;; test/specs/geis-bundle.geis.scm — Tests for (badb geis bundle)
;;;
;;; Covers manifest reading (pure (read), never eval), manifest validation
;;; (schema, path safety, sha256 pins), deterministic bundle hashing,
;;; search-path discovery, dependency resolution (ordering, diamond
;;; dedup, cycles, pin mismatches), and file enumeration/pinning.
;;; All fixtures live in a throwaway mkdtemp workspace with a
;;; save/restored BADB_GEIS_PATH; nothing touches the repo's geis/.

(define-module (test test-geis-bundle)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (badb geis bundle)
               #:use-module (badb kernel digest)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1))

;;;
;;; Fixture helpers
;;;
(define %temp-dir #f)
(define %geis-root #f)
(define %saved-geis-path #f)

(define (write-file! path text)
        "Write TEXT to PATH; parent directories must already exist."
        (call-with-output-file path (lambda (port) (display text port))))

(define (write-manifest! bundle-dir alist)
        "Write ALIST as bundle-dir/manifest.scm via the printer (no quoting games)."
        (call-with-output-file (string-append bundle-dir "/manifest.scm")
                               (lambda (port) (write alist port))))

(define (base-manifest fid)
        "Minimal valid manifest for FID."
        `((bundle-version . 1)
          (feature-id . ,fid)
          (version . "1.0.0")
          (spec . "spec.geis.scm")))

(define (make-bundle! root fid . extra-fields)
        "Create a minimal valid bundle under ROOT/FID with EXTRA-FIELD manifest
entries appended.  Returns the bundle directory."
        (let ((dir (string-append root "/" fid)))
          (mkdir dir)
          (write-file! (string-append dir "/spec.geis.scm")
                       "(test-begin \"x\")\n(test-end \"x\")\n")
          (write-manifest! dir (append (base-manifest fid) extra-fields))
          dir))

(define (index-of pred lst)
        "Index of the first element satisfying PRED, or #f."
        (let loop ((lst lst) (idx 0))
                  (cond ((null? lst) #f)
                        ((pred (car lst)) idx)
                        (else (loop (cdr lst) (+ idx 1))))))

(define (throws-geis-error? thunk needle)
        "THUNK must throw 'geis-bundle-error whose message contains NEEDLE."
        (catch 'geis-bundle-error
          (lambda () (thunk) #f)
          (lambda (key . args)
            (and (eq? key 'geis-bundle-error)
                 (string-contains (car args) needle)))))

;;;
;;; Workspace
;;;
(set! %temp-dir (mkdtemp "/tmp/badb-geis-bundle-test-XXXXXX"))
(set! %geis-root (string-append %temp-dir "/geis"))
(mkdir %geis-root)
(set! %saved-geis-path (getenv "BADB_GEIS_PATH"))
(setenv "BADB_GEIS_PATH" %geis-root)

(spec-begin "geis-bundle")

;;;
;;; 1. manifest-read
;;;
(test-group "manifest-read"
            (let ((dir (make-bundle! %geis-root "test-read")))
              (test-assert "valid manifest reads to alist with expected fields"
                           (let ((m (read-geis-manifest (string-append dir "/manifest.scm"))))
                             (and (list? m)
                                  (equal? 1 (manifest-field m 'bundle-version))
                                  (equal? "test-read" (manifest-field m 'feature-id))
                                  (equal? "1.0.0" (manifest-field m 'version))
                                  (equal? "spec.geis.scm" (manifest-field m 'spec)))))
              (test-assert "missing file returns #f"
                           (not (read-geis-manifest (string-append %temp-dir "/no-such-manifest.scm"))))
              (test-assert "syntactically broken content returns #f"
                           (begin
                             (write-file! (string-append %temp-dir "/broken.scm") "(unclosed paren")
                             (not (read-geis-manifest (string-append %temp-dir "/broken.scm")))))
              (test-assert "non-list sexp (bare number) returns #f"
                           (begin
                             (write-file! (string-append %temp-dir "/number.scm") "42")
                             (not (read-geis-manifest (string-append %temp-dir "/number.scm")))))
              (test-assert "non-list sexp (bare symbol) returns #f"
                           (begin
                             (write-file! (string-append %temp-dir "/symbol.scm") "some-symbol")
                             (not (read-geis-manifest (string-append %temp-dir "/symbol.scm")))))))

;;;
;;; 2. manifest-validate
;;;
(test-group "manifest-validate"
            (test-assert "fully valid bundle returns empty list"
                         (null? (geis-manifest-valid? (make-bundle! %geis-root "valid-bundle"))))
            (test-assert "bundle with all optional fields returns empty list"
                         (let* ((dir (string-append %geis-root "/full-bundle"))
                                (manifest `((bundle-version . 1)
                                            (feature-id . "full-bundle")
                                            (version . "1.0.0")
                                            (spec . "spec.geis.scm")
                                            (plugin . ((module . (geis full-bundle plugin))
                                                       (file . "plugin.scm")))
                                            (workflow . "workflow.sema")
                                            (prompts . ("prompts/request.md"))
                                            (fixtures . "fixtures")
                                            (cassettes . "cassettes"))))
                           (mkdir dir)
                           (write-file! (string-append dir "/spec.geis.scm") "(test-begin \"x\")\n")
                           (write-file! (string-append dir "/plugin.scm") "(define-module (geis full-bundle plugin))\n")
                           (write-file! (string-append dir "/workflow.sema") ";; workflow\n")
                           (mkdir (string-append dir "/prompts"))
                           (write-file! (string-append dir "/prompts/request.md") "request\n")
                           (mkdir (string-append dir "/fixtures"))
                           (mkdir (string-append dir "/cassettes"))
                           (write-manifest! dir manifest)
                           (null? (geis-manifest-valid? dir))))
            (test-assert "bundle with tools and workflow-exports returns empty list"
                         (let* ((dir (string-append %geis-root "/tools-bundle"))
                                (manifest `((bundle-version . 1)
                                            (feature-id . "tools-bundle")
                                            (version . "1.0.0")
                                            (spec . "spec.geis.scm")
                                            (tools . ("custom_tool_1" "custom_tool_2"))
                                            (workflow-exports . ("pipeline_1")))))
                           (mkdir dir)
                           (write-file! (string-append dir "/spec.geis.scm") "(test-begin \"x\")\n")
                           (write-manifest! dir manifest)
                           (null? (geis-manifest-valid? dir))))
            (test-assert "feature-id not matching dir basename returns non-empty"
                         (let* ((dir (string-append %geis-root "/wrong-name")))
                           (mkdir dir)
                           (write-file! (string-append dir "/spec.geis.scm") "(test-begin \"x\")\n")
                           (write-manifest! dir (base-manifest "other-name"))
                           (pair? (geis-manifest-valid? dir))))
            (test-assert "missing spec field returns non-empty"
                         (let* ((dir (string-append %geis-root "/missing-spec")))
                           (mkdir dir)
                           (write-manifest! dir '((bundle-version . 1)
                                                  (feature-id . "missing-spec")
                                                  (version . "1.0.0")))
                           (pair? (geis-manifest-valid? dir))))
            (test-assert "spec file missing on disk returns non-empty"
                         (let* ((dir (string-append %geis-root "/missing-spec-file")))
                           (mkdir dir)
                           (write-manifest! dir (base-manifest "missing-spec-file"))
                           (pair? (geis-manifest-valid? dir))))
            (test-assert "path escape with .. returns non-empty"
                         (let* ((dir (string-append %geis-root "/path-escape")))
                           (mkdir dir)
                           (write-manifest! dir `((bundle-version . 1)
                                                  (feature-id . "path-escape")
                                                  (version . "1.0.0")
                                                  (spec . "../../evil.scm")))
                           (pair? (geis-manifest-valid? dir))))
            (test-assert "absolute spec path returns non-empty"
                         (let* ((dir (string-append %geis-root "/abs-path")))
                           (mkdir dir)
                           (write-manifest! dir `((bundle-version . 1)
                                                  (feature-id . "abs-path")
                                                  (version . "1.0.0")
                                                  (spec . "/absolute/path.scm")))
                           (pair? (geis-manifest-valid? dir))))
            (test-assert "wrong sha256 pin returns non-empty"
                         (let* ((dir (string-append %geis-root "/pinned-bad"))
                                (content "test content")
                                (good (sha256-hex content))
                                (bad (string-append (if (string-prefix? "0" good) "1" "0")
                                                    (substring good 1))))
                           (mkdir dir)
                           (write-file! (string-append dir "/spec.geis.scm") content)
                           (write-manifest! dir `((bundle-version . 1)
                                                  (feature-id . "pinned-bad")
                                                  (version . "1.0.0")
                                                  (spec . "spec.geis.scm")
                                                  (sha256 . (("spec.geis.scm" . ,bad)))))
                           (pair? (geis-manifest-valid? dir))))
            (test-assert "correct sha256 pins return empty list"
                         (let* ((dir (string-append %geis-root "/pinned-good"))
                                (content "test content")
                                (hash (sha256-hex content)))
                           (mkdir dir)
                           (write-file! (string-append dir "/spec.geis.scm") content)
                           (write-manifest! dir `((bundle-version . 1)
                                                  (feature-id . "pinned-good")
                                                  (version . "1.0.0")
                                                  (spec . "spec.geis.scm")
                                                  (sha256 . (("spec.geis.scm" . ,hash)))))
                           (null? (geis-manifest-valid? dir))))
            (test-assert "empty feature-id returns non-empty"
                         (let* ((dir (string-append %geis-root "/empty-fid")))
                           (mkdir dir)
                           (write-file! (string-append dir "/spec.geis.scm") "(test-begin \"x\")\n")
                           (write-manifest! dir `((bundle-version . 1)
                                                  (feature-id . "")
                                                  (version . "1.0.0")
                                                  (spec . "spec.geis.scm")))
                           (pair? (geis-manifest-valid? dir))))
            (test-assert "feature-id with slash returns non-empty"
                         (let* ((dir (string-append %geis-root "/slash-fid")))
                           (mkdir dir)
                           (write-file! (string-append dir "/spec.geis.scm") "(test-begin \"x\")\n")
                           (write-manifest! dir `((bundle-version . 1)
                                                  (feature-id . "bad/fid")
                                                  (version . "1.0.0")
                                                  (spec . "spec.geis.scm")))
                           (pair? (geis-manifest-valid? dir))))
            (test-assert "plugin field without module returns non-empty"
                         (let* ((dir (string-append %geis-root "/bad-plugin")))
                           (mkdir dir)
                           (write-file! (string-append dir "/spec.geis.scm") "(test-begin \"x\")\n")
                           (write-file! (string-append dir "/plugin.scm") "(define-module (geis bad-plugin plugin))\n")
                           (write-manifest! dir `((bundle-version . 1)
                                                  (feature-id . "bad-plugin")
                                                  (version . "1.0.0")
                                                  (spec . "spec.geis.scm")
                                                  (plugin . ((file . "plugin.scm")))))
                           (pair? (geis-manifest-valid? dir))))
            (test-assert "plugin field without file returns non-empty"
                         (let* ((dir (string-append %geis-root "/bad-plugin2")))
                           (mkdir dir)
                           (write-file! (string-append dir "/spec.geis.scm") "(test-begin \"x\")\n")
                           (write-manifest! dir `((bundle-version . 1)
                                                  (feature-id . "bad-plugin2")
                                                  (version . "1.0.0")
                                                  (spec . "spec.geis.scm")
                                                  (plugin . ((module . (geis bad-plugin2 plugin))))))
                           (pair? (geis-manifest-valid? dir))))
            (test-assert "missing version returns non-empty"
                         (let* ((dir (string-append %geis-root "/no-version")))
                           (mkdir dir)
                           (write-file! (string-append dir "/spec.geis.scm") "(test-begin \"x\")\n")
                           (write-manifest! dir `((bundle-version . 1)
                                                  (feature-id . "no-version")
                                                  (spec . "spec.geis.scm")))
                           (pair? (geis-manifest-valid? dir))))
            (test-assert "bundle-version not 1 returns non-empty"
                         (let* ((dir (string-append %geis-root "/wrong-bversion")))
                           (mkdir dir)
                           (write-file! (string-append dir "/spec.geis.scm") "(test-begin \"x\")\n")
                           (write-manifest! dir `((bundle-version . 2)
                                                  (feature-id . "wrong-bversion")
                                                  (version . "1.0.0")
                                                  (spec . "spec.geis.scm")))
                           (pair? (geis-manifest-valid? dir)))))

;;;
;;; 3. bundle-hash
;;;
(test-group "bundle-hash"
            (let ((dir1 (mkdtemp "/tmp/badb-hash-a-XXXXXX"))
                  (dir2 (mkdtemp "/tmp/badb-hash-b-XXXXXX")))
              (define (fill! root fid content extra)
                      (let ((dir (string-append root "/" fid)))
                        (mkdir dir)
                        (write-file! (string-append dir "/spec.geis.scm") content)
                        (when extra
                          (write-file! (string-append dir "/extra.txt") extra))
                        (write-manifest! dir (base-manifest fid))
                        dir))
              (test-assert "identical content in different dirs yields same hash"
                           (let ((h1 (geis-bundle-hash (fill! dir1 "same" "shared content" #f)))
                                 (h2 (geis-bundle-hash (fill! dir2 "same" "shared content" #f))))
                             (string= h1 h2)))
              (test-assert "changing one file's content changes hash"
                           (let ((h1 (geis-bundle-hash (fill! dir1 "differs" "content A" #f)))
                                 (h2 (geis-bundle-hash (fill! dir2 "differs" "content B" #f))))
                             (not (string= h1 h2))))
              (test-assert "adding a file changes hash"
                           (let ((h1 (geis-bundle-hash (fill! dir1 "extra" "content" #f)))
                                 (h2 (geis-bundle-hash (fill! dir2 "extra" "content" "extra file"))))
                             (not (string= h1 h2))))))

;;;
;;; 4. discovery
;;;
(test-group "discovery"
            (let
                ((root1 (string-append %temp-dir "/root1"))
                 (root2 (string-append %temp-dir "/root2")))
              (mkdir root1)
              (mkdir root2)
              (make-bundle! root1 "bundle-a")
              (make-bundle! root2 "bundle-b")
              (test-assert "bundle in first search root found"
                           (begin
                             (setenv "BADB_GEIS_PATH" (string-append root1 ":" root2))
                             (let ((found (find-geis-bundle "bundle-a")))
                               (and found (string-prefix? (string-append root1 "/bundle-a") found)))))
              (test-assert
                "bundle only in second root found there"
                (begin
                  (setenv "BADB_GEIS_PATH" (string-append root1 ":" root2))
                  (let ((found (find-geis-bundle "bundle-b")))
                    (and found (string-prefix? (string-append root2 "/bundle-b") found)))))
              (test-assert "nonexistent feature-id returns #f"
                           (begin
                             (setenv "BADB_GEIS_PATH" (string-append root1 ":" root2))
                             (not (find-geis-bundle "nonexistent"))))
              (test-assert
                "empty BADB_GEIS_PATH defaults to harness + project roots"
                (begin (setenv "BADB_GEIS_PATH" "")
                       (equal? (quote ("src/geasa" "geis")) (geis-bundle-search-path))))
              (test-assert
                "unset BADB_GEIS_PATH defaults to harness + project roots"
                (begin (unsetenv "BADB_GEIS_PATH")
                       (equal? (quote ("src/geasa" "geis")) (geis-bundle-search-path))))
              (test-assert "multiple roots split in order"
                           (begin
                             (setenv "BADB_GEIS_PATH" (string-append root1 ":" root2 ":/some/other"))
                             (equal? (list root1 root2 "/some/other") (geis-bundle-search-path))))
              (setenv "BADB_GEIS_PATH" %geis-root)))

;;;
;;; 5. resolve-geis-load-path
;;;
(test-group "resolve-geis-load-path"
            (let ((deps-root (string-append %temp-dir "/deps")))
              (mkdir deps-root)
              ;; deps-field builds one (depends . ((geis . ...))) manifest entry from
              ;; (fid hash) pairs, using explicit cons/list to avoid deep quasiquotes.
              (define (deps-field . pairs)
                      (list (cons 'depends
                                  (list (cons 'geis
                                              (map (lambda (p)
                                                     (cons (string->symbol (car p))
                                                           (list (cons 'version "1.0.0")
                                                                 (cons 'bundle-sha256 (cadr p)))))
                                                   pairs))))))
              (define bundle-c (make-bundle! deps-root "c"))
              (define bundle-b
                      (apply make-bundle! deps-root "b"
                             (deps-field (list "c" (geis-bundle-hash bundle-c)))))
              (define bundle-a
                      (apply make-bundle! deps-root "a"
                             (deps-field (list "b" (geis-bundle-hash bundle-b)))))
              (define bundle-d
                      (apply make-bundle! deps-root "d"
                             (deps-field (list "b" (geis-bundle-hash bundle-b))
                                         (list "c" (geis-bundle-hash bundle-c)))))
              (setenv "BADB_GEIS_PATH" deps-root)
              (test-assert "linear chain a->b->c returns (c b a) order"
                           (let ((result (resolve-geis-load-path "a")))
                             (and (= 3 (length result))
                                  (string= (canonicalize-path bundle-c) (first result))
                                  (string= (canonicalize-path bundle-b) (second result))
                                  (string= (canonicalize-path bundle-a) (third result)))))
              (test-assert "diamond w->y,w->z,y->x,z->x: shared dep once, before dependents"
                           (let* ((bundle-x (make-bundle! deps-root "x"))
                                  (bundle-y (apply make-bundle! deps-root "y"
                                                   (deps-field (list "x" (geis-bundle-hash bundle-x)))))
                                  (bundle-z (apply make-bundle! deps-root "z"
                                                   (deps-field (list "x" (geis-bundle-hash bundle-x)))))
                                  (bundle-w (apply make-bundle! deps-root "w"
                                                   (deps-field (list "y" (geis-bundle-hash bundle-y))
                                                               (list "z" (geis-bundle-hash bundle-z)))))
                                  (result (resolve-geis-load-path "w")))
                             (and (= 4 (length result))
                                  (= 4 (length (delete-duplicates result string=)))
                                  (let ((x-idx (index-of (lambda (p) (string= (canonicalize-path bundle-x) p)) result))
                                        (y-idx (index-of (lambda (p) (string= (canonicalize-path bundle-y) p)) result))
                                        (z-idx (index-of (lambda (p) (string= (canonicalize-path bundle-z) p)) result))
                                        (w-idx (index-of (lambda (p) (string= (canonicalize-path bundle-w) p)) result)))
                                    (and x-idx y-idx z-idx w-idx
                                         (< x-idx y-idx)
                                         (< x-idx z-idx)
                                         (< y-idx w-idx)
                                         (< z-idx w-idx))))))
              (test-assert "missing dependency throws geis-bundle-error mentioning missing"
                           (begin
                             (write-manifest! bundle-a
                                              (append (base-manifest "a")
                                                      (deps-field (list "ghost" (geis-bundle-hash bundle-c)))))
                             (throws-geis-error? (lambda () (resolve-geis-load-path "a")) "missing")))
              (test-assert "bundle-sha256 mismatch throws mentioning bundle-sha256"
                           (begin
                             (write-manifest! bundle-a
                                              (append (base-manifest "a")
                                                      (deps-field (list "b" (make-string 64 #\0)))))
                             (throws-geis-error? (lambda () (resolve-geis-load-path "a")) "bundle-sha256")))
              (test-assert "version mismatch throws mentioning version"
                           (begin
                             (write-manifest! bundle-a
                                              (append (base-manifest "a")
                                                      (deps-field (list "b" (geis-bundle-hash bundle-b)))))
                             (write-manifest! bundle-b
                                              (append (base-manifest "b")
                                                      (deps-field (list "c" (geis-bundle-hash bundle-c)))))
                             ;; pin b with a wrong version
                             (write-manifest! bundle-a
                                              (append (base-manifest "a")
                                                      (list (cons 'depends
                                                                  (list (cons 'geis
                                                                              (list (cons 'b
                                                                                          (list (cons 'version "9.9.9")
                                                                                                (cons 'bundle-sha256
                                                                                                      (geis-bundle-hash bundle-b)))))))))))
                             (throws-geis-error? (lambda () (resolve-geis-load-path "a")) "version")))
              (test-assert "self-cycle throws mentioning cycle"
                           (begin
                             (write-manifest! bundle-a
                                              (append (base-manifest "a")
                                                      (deps-field (list "a" (geis-bundle-hash bundle-a)))))
                             (throws-geis-error? (lambda () (resolve-geis-load-path "a")) "cycle")))
              (test-assert "mutual cycle a<->b throws mentioning cycle"
                           (begin
                             (write-manifest! bundle-a
                                              (append (base-manifest "a")
                                                      (deps-field (list "b" (geis-bundle-hash bundle-b)))))
                             (write-manifest! bundle-b
                                              (append (base-manifest "b")
                                                      (deps-field (list "a" (geis-bundle-hash bundle-a)))))
                             (throws-geis-error? (lambda () (resolve-geis-load-path "a")) "cycle")))
              (setenv "BADB_GEIS_PATH" %geis-root)))

;;;
;;; 6. pins and file enumeration
;;;
(test-group "pins"
            (let ((dir (string-append %geis-root "/pins-test")))
              (mkdir dir)
              (mkdir (string-append dir "/subdir"))
              (write-manifest! dir (base-manifest "pins-test"))
              (write-file! (string-append dir "/spec.geis.scm") "test spec")
              (write-file! (string-append dir "/file1.txt") "content1")
              (write-file! (string-append dir "/subdir/file2.txt") "content2")
              (test-assert "geis-bundle-files lists all files except manifest.scm, sorted"
                           (let ((files (geis-bundle-files dir)))
                             (and (equal? '("file1.txt" "spec.geis.scm" "subdir/file2.txt") files))))
              (test-assert "compute-geis-sha256-pins excludes manifest.scm"
                           (let ((pins (compute-geis-sha256-pins dir)))
                             (not (any (lambda (pin) (string= "manifest.scm" (car pin))) pins))))
              (test-assert "compute-geis-sha256-pins includes subdirectory files"
                           (let ((pins (compute-geis-sha256-pins dir)))
                             (any (lambda (pin) (string= "subdir/file2.txt" (car pin))) pins)))
              (test-assert "compute-geis-sha256-pins hashes match content sha256"
                           (let* ((pins (compute-geis-sha256-pins dir))
                                  (lookup (lambda (f) (assoc-ref pins f))))
                             (and (string= (sha256-hex "content1") (lookup "file1.txt"))
                                  (string= (sha256-hex "content2") (lookup "subdir/file2.txt"))
                                  (string= (sha256-hex "test spec") (lookup "spec.geis.scm")))))))

;;;
;;; Teardown: restore environment before the suite exits
;;;
(if %saved-geis-path
  (setenv "BADB_GEIS_PATH" %saved-geis-path)
  (unsetenv "BADB_GEIS_PATH"))

(spec-end)
(spec-exit)
