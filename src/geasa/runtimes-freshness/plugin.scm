(define-module (geasa runtimes-freshness plugin)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (badb kernel digest)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (freshness-pin-check
            freshness-result? freshness-result-status freshness-result-pass?
            freshness-result-entries freshness-result-reason freshness-result-bundle
            freshness-result-checked-at
            freshness-entry? freshness-entry-file freshness-entry-expected
            freshness-entry-actual freshness-entry-ok? freshness-entry-reason
            freshness-bundle-pins
            make-freshness-cache freshness-cache?
            freshness-verify freshness-cache-size freshness-cache-entry
            freshness-cache-invalidate!))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  TIER 1 — PIN CHECK
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Result is an alist: ((status . "pass"|"fail") (bundle . BUNDLE-DIR)
;;                      (checked-at . UNIX-SECONDS) (entries . ENTRY...) (reason . STR|#f))
(define (freshness-result? obj)
  (and (pair? obj)
       (assoc 'status obj)
       (assoc 'bundle obj)
       (assoc 'checked-at obj)
       (assoc 'entries obj)))

(define (freshness-result-status res)
  (assoc-ref res 'status))

(define (freshness-result-pass? res)
  (string=? (freshness-result-status res) "pass"))

(define (freshness-result-entries res)
  (assoc-ref res 'entries))

(define (freshness-result-reason res)
  (assoc-ref res 'reason))

(define (freshness-result-bundle res)
  (assoc-ref res 'bundle))

(define (freshness-result-checked-at res)
  (assoc-ref res 'checked-at))

;; Entry is an alist: ((file . STR) (expected . PIN|#f) (actual . HASH|#f) (ok . BOOL) (reason . STR|#f))
(define (freshness-entry? obj)
  (and (pair? obj)
       (assoc 'file obj)
       (assoc 'expected obj)
       (assoc 'actual obj)
       (assoc 'ok obj)))

(define (freshness-entry-file entry)
  (assoc-ref entry 'file))

(define (freshness-entry-expected entry)
  (assoc-ref entry 'expected))

(define (freshness-entry-actual entry)
  (assoc-ref entry 'actual))

(define (freshness-entry-ok? entry)
  (assoc-ref entry 'ok))

(define (freshness-entry-reason entry)
  (assoc-ref entry 'reason))

;; Read manifest.scm from bundle directory
(define (read-manifest bundle-dir)
  "Read and return the manifest alist from BUNDLE-DIR/manifest.scm, or #f."
  (let ((path (string-append bundle-dir "/manifest.scm")))
    (catch #t
      (lambda ()
        (if (file-exists? path)
            (call-with-input-file path read)
            #f))
      (lambda _ #f))))

;; Get pins from manifest
(define (freshness-bundle-pins bundle-dir)
  "Return the sha256 pin alist from BUNDLE-DIR's manifest.scm, or #f when
the manifest (or its sha256 field) cannot be read."
  (let ((manifest (read-manifest bundle-dir)))
    (and manifest
         (let ((pins (assoc-ref manifest 'sha256)))
           (and pins pins)))))

;; Hash file content
(define (hash-file path)
  "Return the SHA256 hex digest of the UTF-8 text of the file at PATH, or
#f when PATH is missing or unreadable (directories included)."
  (catch #t
    (lambda ()
      (and (file-exists? path)
           (call-with-input-file path
             (lambda (p)
               (set-port-encoding! p "UTF-8")
               (sha256-hex (get-string-all p))))))
    (lambda _ #f)))

;; Create a freshness entry for a file
(define (make-entry file bundle-dir expected actual)
  "Create a freshness-entry alist for FILE in BUNDLE-DIR with EXPECTED pin and ACTUAL hash."
  (let* ((file-path (string-append bundle-dir "/" file))
         (file-exists (file-exists? file-path))
         (ok (and file-exists expected actual (string=? expected actual)))
         (reason (cond
                  ((not file-exists) "file missing on disk")
                  ((not expected) "no pin in manifest")
                  ((not actual) "file unreadable")
                  ((not ok) "hash mismatch")
                  (else #f))))
    (list (cons 'file file)
          (cons 'expected expected)
          (cons 'actual actual)
          (cons 'ok ok)
          (cons 'reason reason))))

;; Main pin check function
(define (freshness-pin-check bundle-dir)
  "Check bundle freshness by comparing pinned hashes against actual file hashes.
Returns a freshness-result alist."
  (let* ((manifest (read-manifest bundle-dir))
         (checked-at (current-time)))
    
    (if (not manifest)
        ;; No manifest - whole check fails
        (list (cons 'status "fail")
              (cons 'bundle bundle-dir)
              (cons 'checked-at checked-at)
              (cons 'entries '())
              (cons 'reason "manifest.scm not found or unreadable"))
        
        ;; Process each pinned file
        (let* ((pins (assoc-ref manifest 'sha256))
               (files-to-check '("plugin.scm" "spec.geis.scm"))
               (entries '())
               (all-ok? #t))
          
          (for-each
           (lambda (file)
             (let* ((pin-entry (assoc file pins))
                    (expected (if pin-entry (cdr pin-entry) #f))
                    (file-path (string-append bundle-dir "/" file))
                    (actual (hash-file file-path))
                    (entry (make-entry file bundle-dir expected actual)))
               (set! entries (cons entry entries))
               (when (not (freshness-entry-ok? entry))
                 (set! all-ok? #f))))
           files-to-check)
          
          (let* ((status (if all-ok? "pass" "fail"))
                 (bad (find (lambda (e) (not (freshness-entry-ok? e)))
                            entries))
                 (reason (and bad (freshness-entry-reason bad))))
            (list (cons 'status status)
                  (cons 'bundle bundle-dir)
                  (cons 'checked-at checked-at)
                  (cons 'entries (reverse entries))
                  (cons 'reason reason)))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  TIER 2 — LAZY VERDICT CACHE
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cache representation: (freshness-cache . ALIST)
;; where ALIST is a list of cached verdict entries, each entry is:
;;   ((pins . PIN-ALIST) (timestamp . UNIX-SECONDS) (verdict . THUNK-RESULT))

(define (make-freshness-cache)
  "Create a new, empty freshness cache."
  (cons 'freshness-cache '()))

(define (freshness-cache? obj)
  "Return #t if OBJ is a freshness cache."
  (and (pair? obj) (eq? (car obj) 'freshness-cache)))

(define (freshness-cache-entries cache)
  "Return the alist of cached entries from CACHE."
  (cdr cache))

(define (freshness-cache-size cache)
  "Return the number of cached entries."
  (length (freshness-cache-entries cache)))

(define (freshness-cache-entry cache bundle-dir)
  "Return the cached alist entry for BUNDLE-DIR, or #f if not cached."
  (let ((pins (freshness-bundle-pins bundle-dir)))
    (and pins
         (find (lambda (entry)
                 (equal? pins (assoc-ref entry 'pins)))
               (freshness-cache-entries cache)))))

(define (freshness-verify cache bundle-dir thunk)
  "Return cached verdict for BUNDLE-DIR, or call THUNK and cache result.
If pins cannot be read, THUNK is called every time (never cached)."
  (let ((pins (freshness-bundle-pins bundle-dir)))
    (if (not pins)
        ;; Cannot read pins - never cache, always replay
        (thunk)
        
        ;; Check cache
        (let ((existing (freshness-cache-entry cache bundle-dir)))
          (if existing
              (assoc-ref existing 'verdict)
              
              ;; Not cached - run thunk and cache
              (let* ((verdict (thunk))
                     (timestamp (current-time))
                     (entry (list (cons 'pins pins)
                                  (cons 'timestamp timestamp)
                                  (cons 'verdict verdict))))
                
                ;; Update cache by creating new cache with updated entries
                (let ((new-entries (cons entry (freshness-cache-entries cache))))
                  (set-cdr! cache new-entries))
                verdict))))))

(define (freshness-cache-invalidate! cache bundle-dir)
  "Remove cached entry for BUNDLE-DIR. Return the removed entry or #f."
  (let ((pins (freshness-bundle-pins bundle-dir)))
    (and pins
         (let* ((entries (freshness-cache-entries cache))
                (matching (filter (lambda (entry)
                                    (equal? pins (assoc-ref entry 'pins)))
                                  entries))
                (new-entries (filter (lambda (entry)
                                       (not (equal? pins (assoc-ref entry 'pins))))
                                     entries)))
           (set-cdr! cache new-entries)
           (and (pair? matching) (car matching))))))
