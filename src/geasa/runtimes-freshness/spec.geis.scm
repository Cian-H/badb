;;; test-freshness.scm — Tests for Lazy Geis Verification (geasa runtimes-freshness plugin)
;;;
;;; Two-tier freshness for geis bundles (roadmap: "Lazy geis verification").
;;;
;;; TIER 1 — PIN CHECK (cheap, always at startup).  (freshness-pin-check
;;; BUNDLE-DIR) hashes the bundle's plugin.scm and spec.geis.scm and
;;; compares them against the (FILE . SHA256HEX) pins in its manifest.scm.
;;; Pure Guile: hashing only, no subprocess, no spec replay.  It returns a
;;; structured result alist
;;;
;;;   ((status . "pass"|"fail") (bundle . BUNDLE-DIR)
;;;    (checked-at . UNIX-SECONDS) (entries . ENTRY...) (reason . STR|#f))
;;;
;;; where each per-file ENTRY is
;;;
;;;   ((file . "plugin.scm"|"spec.geis.scm") (expected . PIN|#f)
;;;    (actual . HASH|#f) (ok . BOOL) (reason . STR|#f))
;;;
;;; status is "pass" iff every entry is ok.  A pinned file that is
;;; missing on disk, a missing pin, or a hash mismatch makes that entry
;;; ok=#f, with expected/actual populated where known and a
;;; human-readable reason; a missing or unreadable manifest.scm makes the
;;; whole check fail with a top-level reason.
;;;
;;; TIER 2 — LAZY FULL REPLAY (expensive, on demand).  (make-freshness-cache)
;;; creates a fresh per-session verdict cache.  (freshness-verify CACHE
;;; BUNDLE-DIR REPLAY-THUNK) returns the verdict of REPLAY-THUNK, calling
;;; the thunk ONLY when no fresh cached verdict exists — the cache never
;;; spawns subprocesses itself.  Verdicts live only inside the cache
;;; object (per-session TTL: they die with the cache object) and each
;;; cached entry is keyed at least by the bundle's manifest pins plus a
;;; timestamp:
;;;
;;;   ((pins . PIN-ALIST) (timestamp . UNIX-SECONDS) (verdict . THUNK-RESULT))
;;;
;;; Invalidation: changed pins (manifest sha256 field altered) or an
;;; explicit (freshness-cache-invalidate! CACHE BUNDLE-DIR) force a
;;; re-verify on the next call.  A bundle whose pins cannot be read is
;;; never cached: the thunk runs on every call (fail open to
;;; verification, never to skipping it).
;;;
;;; IMPLEMENTATION NOTE — the plugin module (geasa runtimes-freshness
;;; plugin) is plain loadable Guile with no implicit environment: every
;;; binding it uses must come from an explicit #:use-module in its
;;; define-module form — e.g. (ice-9 format) for format, (ice-9 rdelim)
;;; for read-line.  The module must load cleanly under `guile -L src
;;; -L .`; a module that fails to load fails the whole contract with a
;;; subprocess error before any test can run.
;;;
;;; REPRESENTATION NOTE — do NOT use define-record-type (SRFI-9) or any
;;; record syntax in this plugin.  Guile's record macros are easy to get
;;; subtly wrong (constructor specs differ between SRFI-9 and Guile's
;;; native records, and a syntax error kills module load).  Represent
;;; ALL internal state with plain Scheme data instead:
;;;   - the cache object: a one-entry mutable box, e.g. (cons 'freshness-cache
;;;     ALIST) where ALIST maps bundle-dir -> cached verdict entry, with
;;;     set-cdr! used to update it; freshness-cache? then checks the tag
;;;     (eq? 'freshness-cache (car obj));
;;;   - the pin-check result and its per-file entries: plain alists with
;;;     symbol keys, e.g. ((status . "pass") (bundle . DIR) (checked-at
;;;     . N) (entries . (...)) (reason . #f));
;;;   - each CACHED VERDICT ENTRY: a plain alist carrying at least the
;;;     'pins, 'timestamp and 'verdict keys.
;;; The contract sees every value only through the exported accessors,
;;; so any plain-data representation is fine as long as the accessors
;;; agree with it.  Keep the implementation free of macros entirely.
;;;
;;; GUILE API NOTE — Guile's assoc-ref takes EXACTLY TWO arguments,
;;; (assoc-ref ALIST KEY); there is NO third default-value argument —
;;; (assoc-ref alist key #f) is an arity error.  To read a key that may
;;; be absent, use (let ((cell (assoc KEY ALIST))) (if cell (cdr cell)
;;; #f)).  Every accessor in this plugin must follow that idiom.
;;;
;;; PIN-CHECK ALGORITHM — for each FILE in ("plugin.scm" "spec.geis.scm"):
;;;   path     = (string-append BUNDLE-DIR "/" FILE)  ; note the "/"
;;;   expected = the (FILE . HASH) pin from the manifest 'sha256 alist,
;;;              or #f when the manifest pins no hash for FILE
;;;   actual   = (sha256-hex CONTENT) where CONTENT is the whole file
;;;              read as a UTF-8 string (get-string-all from
;;;              (ice-9 textual-ports) inside call-with-input-file), or
;;;              #f when the file cannot be read
;;;   ok       = (and (string? expected) (string? actual)
;;;                   (string= expected actual))
;;;   reason   = a human string for the first failing condition, else #f
;;; The result status is "pass" iff every entry is ok, else "fail"; a
;;; missing or unreadable manifest.scm yields status "fail" with a
;;; top-level reason and NO entries.  The result alist always carries
;;; all five keys (status bundle checked-at entries reason), with
;;; reason #f on pass.
;;;
;;; CACHE INVARIANTS — (freshness-bundle-pins BUNDLE-DIR) returns the
;;; manifest 'sha256 alist for a readable manifest, else #f (NOT '()).
;;; (freshness-verify CACHE DIR THUNK): read pins; when pins are #f call
;;; THUNK and return its value WITHOUT caching anything; otherwise look
;;; up the entry for DIR whose 'pins equal the current pins — if found
;;; return its 'verdict without calling THUNK, else call THUNK and
;;; store ((pins . PINS) (timestamp . SECONDS) (verdict . RESULT))
;;; for DIR.  (freshness-cache-invalidate! CACHE DIR) removes the entry
;;; for DIR, returning #f when there was none to remove.

(define-module (test test-freshness)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (srfi srfi-1)
               #:use-module (badb kernel digest)
               #:use-module (geasa runtimes-freshness plugin))

(spec-begin "runtimes-freshness")

;;;
;;; Fixtures — tiny bundles created by THIS spec under a private temp
;;; root.  Other geasa bundles' pins are never touched.
;;;
(define %fixture-root (mkdtemp "/tmp/badb-freshness-spec-XXXXXX"))
(define %fixture-dirs '())

(define %plugin-content "(define (fixture-fn) 'ok)\n")
(define %spec-content ";; fixture spec content\n")

(define (write-string-file path str)
        (call-with-output-file path (lambda (p) (display str p))))

(define (write-manifest path fid pins)
        (call-with-output-file path
                               (lambda (p)
                                 (write `((bundle-version . 1)
                                          (feature-id . ,fid)
                                          (version . "1.0.0")
                                          (spec . "spec.geis.scm")
                                          (sha256 ,@pins))
                                        p))))

(define (make-fixture! fid)
        "Create an honest fixture bundle with correct pins; return its directory."
        (let ((dir (string-append %fixture-root "/" fid)))
          (mkdir dir)
          (set! %fixture-dirs (cons dir %fixture-dirs))
          (write-string-file (string-append dir "/plugin.scm") %plugin-content)
          (write-string-file (string-append dir "/spec.geis.scm") %spec-content)
          (write-manifest (string-append dir "/manifest.scm") fid
                          (list (cons "plugin.scm" (sha256-hex %plugin-content))
                                (cons "spec.geis.scm" (sha256-hex %spec-content))))
          dir))

(define (tamper-plugin-pin! dir)
        "Alter the plugin.scm pin in DIR's manifest (content untouched).
Return the tampered hash."
        (let* ((path (string-append dir "/manifest.scm"))
               (manifest (call-with-input-file path read))
               (pins (assoc-ref manifest 'sha256))
               (hash (cdr (assoc "plugin.scm" pins)))
               (bad (string-append
                      (string (if (char=? (string-ref hash 0) #\0) #\1 #\0))
                      (substring hash 1))))
          (write-manifest path (basename dir)
                          (map (lambda (pin)
                                 (if (string=? (car pin) "plugin.scm")
                                   (cons "plugin.scm" bad)
                                   pin))
                               pins))
          bad))

(define (entry-for file entries)
        (find (lambda (e) (string=? file (freshness-entry-file e))) entries))

;;;
;;; Tier 1 — pin check
;;;
(test-group "tier 1: pin check"
            (let*
                ((honest (make-fixture! "fresh-honest")) (res (freshness-pin-check honest)))
              (test-equal "honest bundle status pass" "pass" (freshness-result-status res))
              (test-assert "honest bundle pass?" (freshness-result-pass? res))
              (test-equal "both pinned files checked"
                          2
                          (length (freshness-result-entries res)))
              (test-assert "every per-file entry ok"
                           (every freshness-entry-ok? (freshness-result-entries res)))
              (test-assert "pass result has no reason" (not (freshness-result-reason res)))
              (test-equal "result carries bundle dir" honest (freshness-result-bundle res))
              (test-assert "result carries a timestamp"
                           (number? (freshness-result-checked-at res)))
              (let
                  ((entry (entry-for "plugin.scm" (freshness-result-entries res))))
                (test-assert "plugin entry present" (and entry #t))
                (test-equal "expected is the manifest pin"
                            (sha256-hex %plugin-content)
                            (freshness-entry-expected entry))
                (test-equal "actual is the content hash"
                            (sha256-hex %plugin-content)
                            (freshness-entry-actual entry))))
            (let*
                ((tampered (make-fixture! "fresh-tampered"))
                 (bad-hash (tamper-plugin-pin! tampered))
                 (res (freshness-pin-check tampered)))
              (test-equal "tampered bundle status fail" "fail" (freshness-result-status res))
              (test-assert "tampered bundle not pass" (not (freshness-result-pass? res)))
              (test-assert "fail result carries a reason"
                           (string? (freshness-result-reason res)))
              (let
                  ((entry (entry-for "plugin.scm" (freshness-result-entries res))))
                (test-assert "tampered entry present" (and entry #t))
                (test-assert "tampered entry not ok" (not (freshness-entry-ok? entry)))
                (test-equal "expected is the tampered pin"
                            bad-hash
                            (freshness-entry-expected entry))
                (test-equal "actual is the real content hash"
                            (sha256-hex %plugin-content)
                            (freshness-entry-actual entry))
                (test-assert "tampered entry carries a reason"
                             (string? (freshness-entry-reason entry))))
              (test-assert "untampered file still ok"
                           (freshness-entry-ok? (entry-for "spec.geis.scm" (freshness-result-entries res)))))
            (let
                ((broken (make-fixture! "fresh-missing-file")))
              (delete-file (string-append broken "/spec.geis.scm"))
              (let ((res (freshness-pin-check broken)))
                (test-equal "missing pinned file fails" "fail" (freshness-result-status res))
                (test-assert "missing file entry not ok"
                             (not
                               (freshness-entry-ok? (entry-for "spec.geis.scm" (freshness-result-entries res)))))))
            (let
                ((empty (string-append %fixture-root "/fresh-no-manifest")))
              (mkdir empty)
              (set! %fixture-dirs (cons empty %fixture-dirs))
              (let ((res (freshness-pin-check empty)))
                (test-equal "missing manifest fails" "fail" (freshness-result-status res))
                (test-assert "missing manifest not pass" (not (freshness-result-pass? res)))
                (test-assert "missing manifest has a reason"
                             (string? (freshness-result-reason res)))))
            (let ((honest (make-fixture! "fresh-pins")))
              (test-equal "bundle pins read from manifest"
                          '("plugin.scm" "spec.geis.scm")
                          (sort (map car (freshness-bundle-pins honest)) string<?))
              (test-assert "bundle pins carry the right hashes"
                           (equal? (sha256-hex %plugin-content)
                                   (cdr (assoc "plugin.scm" (freshness-bundle-pins honest)))))))

;;;
;;; Tier 2 — lazy full replay with verdict cache
;;;
(test-group "tier 2: lazy verdict cache"

            (test-assert "cache predicate accepts a cache" (freshness-cache? (make-freshness-cache)))
            (test-assert "cache predicate rejects non-caches" (not (freshness-cache? 'nope)))

            (let* ((bundle (make-fixture! "fresh-cache"))
                   (cache (make-freshness-cache))
                   (calls 0))
              (define (counting-thunk)
                      (set! calls (+ calls 1))
                      '(verdict pass))
              (test-equal "cache starts empty" 0 (freshness-cache-size cache))
              (let ((v1 (freshness-verify cache bundle counting-thunk)))
                (test-equal "first call replays" 1 calls)
                (test-equal "verdict returned on first call" '(verdict pass) v1))
              (let ((v2 (freshness-verify cache bundle counting-thunk)))
                (test-equal "second call does NOT replay" 1 calls)
                (test-equal "cached verdict equal" '(verdict pass) v2))
              (test-equal "cache holds one entry" 1 (freshness-cache-size cache))
              (let ((entry (freshness-cache-entry cache bundle)))
                (test-assert "entry exists" (and entry #t))
                (test-assert "entry keyed by pins" (assoc 'pins entry))
                (test-assert "entry keyed by a numeric timestamp"
                             (and (assoc 'timestamp entry) (number? (assoc-ref entry 'timestamp))))
                (test-equal "entry pins match the manifest"
                            (freshness-bundle-pins bundle) (assoc-ref entry 'pins))
                (test-equal "entry carries the verdict" '(verdict pass)
                            (assoc-ref entry 'verdict)))
              (test-assert "invalidate removes the entry" (freshness-cache-invalidate! cache bundle))
              (let ((v3 (freshness-verify cache bundle counting-thunk)))
                (test-equal "re-verify after invalidation" 2 calls)
                (test-equal "verdict after re-verify" '(verdict pass) v3))
              (test-assert "invalidate again removes re-created entry"
                           (freshness-cache-invalidate! cache bundle))
              (test-equal "invalidate on empty cache is #f"
                          #f (freshness-cache-invalidate! cache bundle))
              (let ((v4 (freshness-verify cache bundle counting-thunk)))
                (test-equal "re-verify after second invalidation" 3 calls)
                (test-equal "verdict after second re-verify" '(verdict pass) v4))
              (tamper-plugin-pin! bundle)
              (let ((v5 (freshness-verify cache bundle counting-thunk)))
                (test-equal "changed pins force a replay" 4 calls)
                (test-equal "verdict after pins change" '(verdict pass) v5))))

(let* ((bundle (make-fixture! "fresh-session"))
       (c1 (make-freshness-cache))
       (c2 (make-freshness-cache))
       (calls 0))
  (define (thunk) (set! calls (+ calls 1)) 'fresh)
  (freshness-verify c1 bundle thunk)
  (test-equal "first cache replays" 1 calls)
  (freshness-verify c1 bundle thunk)
  (test-equal "same cache reuses the verdict" 1 calls)
  (freshness-verify c2 bundle thunk)
  (test-equal "fresh cache replays again (per-session TTL)" 2 calls))

(let* ((nomani (string-append %fixture-root "/fresh-nomanifest-cache"))
       (cache (make-freshness-cache))
       (calls 0))
  (define (thunk) (set! calls (+ calls 1)) 'v)
  (mkdir nomani)
  (set! %fixture-dirs (cons nomani %fixture-dirs))
  (freshness-verify cache nomani thunk)
  (freshness-verify cache nomani thunk)
  (test-equal "unreadable pins replay every time" 2 calls)
  (test-equal "nothing cached for unreadable pins" 0 (freshness-cache-size cache)))

;;;
;;; Teardown — delete the private fixture root (files, then dirs).
;;;
(for-each
  (lambda (dir)
    (for-each
      (lambda (file)
        (let ((path (string-append dir "/" file)))
          (catch #t
            (lambda () (when (file-exists? path) (delete-file path)))
            (lambda _ #f))))
      '("manifest.scm" "plugin.scm" "spec.geis.scm"))
    (catch #t (lambda () (rmdir dir)) (lambda _ #f)))
  %fixture-dirs)
(catch #t (lambda () (rmdir %fixture-root)) (lambda _ #f))

(spec-end)
(spec-exit)
