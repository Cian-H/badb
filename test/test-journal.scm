;;; badb — Checkpoint journal test suite.
;;;
;;; Tests persistent checkpoint journaling, automatic iteration/timestamp,
;;; filtering, persistence, and integrity verification.

(define-module (test test-journal)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-64)
               #:use-module (ice-9 ftw)
               #:use-module (badb kernel journal))

;;;
;;; Test helpers
;;;

(define (rm-rf path)
        (when (file-exists? path)
          (if (file-is-directory? path)
            (begin
              (for-each (lambda (file)
                          (unless (member file '("." ".."))
                            (rm-rf (string-append path "/" file))))
                        (or (scandir path) '()))
              (rmdir path))
            (delete-file path))))

(define (create-temp-journal-path)
        (let ((dir (mkdtemp "/tmp/badb-journal-test-XXXXXX")))
          (string-append dir "/journal.jsonl")))

(define (cleanup-temp-file filepath)
        (rm-rf (dirname filepath)))

;;;
;;; Test Suite
;;;

(test-begin "journal")

;; 1. Creation and Predicate
(test-group "creation"
            (let* ((path (create-temp-journal-path))
                   (j (make-journal path)))
              (test-assert "journal? is true for journal instance" (journal? j))
              (test-assert "journal? is false for non-journals" (not (journal? "not-a-journal")))
              (test-equal "journal-path returns the configured path" path (journal-path j))
              (test-assert "file exists on disk after make-journal" (file-exists? path))
              (test-equal "empty journal has count 0" 0 (journal-count j))
              (test-equal "empty journal has null entries" '() (journal-entries j))
              (test-assert "empty journal has #f latest" (eq? #f (journal-latest j)))
              (cleanup-temp-file path)))

(test-group "nested-directory-creation"
            (let* ((base-dir (mkdtemp "/tmp/badb-journal-nested-XXXXXX"))
                   (path (string-append base-dir "/a/b/c/nested-journal.jsonl"))
                   (j (make-journal path)))
              (test-assert "parent directories created automatically" (file-exists? path))
              (test-assert "journal is valid" (journal? j))
              (rm-rf base-dir)))

;; 2. Serialization and Lookup Helpers
(test-group "serialization-helpers"
            (let* ((entry '((feature-id . "feat-auth")
                            (commit . "abc1234")
                            (test-verdict . "pass")
                            (iteration . 1)))
                   (json-str (journal-entry->json-string entry))
                   (parsed (json-string->journal-entry json-str)))
              (test-assert "json-string is non-empty string" (and (string? json-str) (> (string-length json-str) 0)))
              (test-equal "lookup by symbol finds value" "feat-auth" (journal-entry-ref parsed 'feature-id))
              (test-equal "lookup by string finds value" "feat-auth" (journal-entry-ref parsed "feature-id"))
              (test-equal "lookup missing key returns default"
                          'missing-default (journal-entry-ref parsed 'non-existent 'missing-default))
              (test-equal "lookup iteration is integer" 1 (journal-entry-ref parsed 'iteration))))

;; 3. Appending and Automatic Field Injection
(test-group "append-and-auto-fields"
            (let* ((path (create-temp-journal-path))
                   (j (make-journal path))
                   (e1 (journal-append! j '((feature-id . "F1") (test-verdict . "pass"))))
                   (e2 (journal-append! j '((feature-id . "F2") (test-verdict . "fail"))))
                   (e3 (journal-append! j '((feature-id . "F1") (test-verdict . "pass")))))

              (test-equal "auto iteration starts at 1" 1 (journal-entry-ref e1 'iteration))
              (test-equal "auto iteration increments to 2" 2 (journal-entry-ref e2 'iteration))
              (test-equal "auto iteration increments to 3" 3 (journal-entry-ref e3 'iteration))

              (test-assert "e1 has ISO-8601 UTC timestamp" (let ((ts (journal-entry-ref e1 'timestamp)))
                                                             (and (string? ts)
                                                                  (string-contains ts "T")
                                                                  (string-suffix? "Z" ts))))

              (test-equal "total count is 3" 3 (journal-count j))

              (let ((entries (journal-entries j)))
                (test-equal "retrieved 3 entries in FIFO order"
                            '(1 2 3) (map (lambda (e) (journal-entry-ref e 'iteration)) entries)))

              (cleanup-temp-file path)))

;; 4. Explicit Field Overrides
(test-group "explicit-field-overrides"
            (let* ((path (create-temp-journal-path))
                   (j (make-journal path))
                   (custom-ts "2026-01-01T00:00:00Z")
                   (e1 (journal-append! j `((iteration . 10)
                                            (timestamp . ,custom-ts)
                                            (feature-id . "custom"))))
                   (e2 (journal-append! j '((feature-id . "next")))))

              (test-equal "custom iteration is preserved" 10 (journal-entry-ref e1 'iteration))
              (test-equal "custom timestamp is preserved" custom-ts (journal-entry-ref e1 'timestamp))
              (test-equal "subsequent iteration increments from custom max" 11 (journal-entry-ref e2 'iteration))

              (cleanup-temp-file path)))

;; 5. Filtering and Latest Access
(test-group "filtering-and-latest"
            (let* ((path (create-temp-journal-path))
                   (j (make-journal path)))
              (journal-append! j '((feature-id . "feature-A") (commit . "c1") (test-verdict . "pass")))
              (journal-append! j '((feature-id . "feature-B") (commit . "c2") (test-verdict . "fail")))
              (journal-append! j '((feature-id . "feature-A") (commit . "c3") (test-verdict . "pass")))

              (test-equal "count for feature-A is 2" 2 (journal-count j #:feature-id "feature-A"))
              (test-equal "count for feature-B is 1" 1 (journal-count j #:feature-id "feature-B"))
              (test-equal "count for non-existent feature is 0" 0 (journal-count j #:feature-id "feature-C"))

              (let ((entries-a (journal-entries j #:feature-id "feature-A")))
                (test-equal "feature-A entries have commits c1 and c3"
                            '("c1" "c3") (map (lambda (e) (journal-entry-ref e 'commit)) entries-a)))

              (let ((latest-a (journal-latest j #:feature-id "feature-A")))
                (test-assert "latest for feature-A is non-false" latest-a)
                (test-equal "latest for feature-A has commit c3" "c3" (journal-entry-ref latest-a 'commit)))

              (let ((latest-overall (journal-latest j)))
                (test-equal "overall latest is the last appended entry"
                            3 (journal-entry-ref latest-overall 'iteration)))

              (test-assert "latest for non-existent feature is #f"
                           (eq? #f (journal-latest j #:feature-id "non-existent")))

              (cleanup-temp-file path)))

;; 6. Record Type Support
(test-group "record-type-support"
            (let* ((path (create-temp-journal-path))
                   (j (make-journal path))
                   (rec (make-journal-entry #:feature-id "feat-rec"
                                            #:commit "deadbeef"
                                            #:test-verdict "pass"
                                            #:author "agent-smith"
                                            #:details '((note . "checkpoint"))))
                   (appended (journal-append! j rec))
                   (latest (journal-latest j)))

              (test-assert "record entry is appended" appended)
              (test-equal "retrieved author matches" "agent-smith" (journal-entry-ref latest 'author))
              (test-equal "retrieved commit matches" "deadbeef" (journal-entry-ref latest 'commit))
              (test-equal "retrieved test-verdict matches" "pass" (journal-entry-ref latest 'test-verdict))

              (cleanup-temp-file path)))

;; 7. Persistence Across Instances
(test-group "persistence-across-instances"
            (let* ((path (create-temp-journal-path))
                   (j1 (make-journal path)))
              (journal-append! j1 '((feature-id . "F1") (commit . "hash1")))
              (journal-append! j1 '((feature-id . "F2") (commit . "hash2")))

              ;; Open separate instance on the same file
              (let ((j2 (make-journal path)))
                (test-equal "new instance sees existing count" 2 (journal-count j2))
                (test-equal "new instance sees existing entries"
                            '("hash1" "hash2") (map (lambda (e) (journal-entry-ref e 'commit)) (journal-entries j2)))

                ;; Append via new instance
                (let ((e3 (journal-append! j2 '((feature-id . "F3") (commit . "hash3")))))
                  (test-equal "auto iteration increments sequentially" 3 (journal-entry-ref e3 'iteration))
                  (test-equal "total count is now 3" 3 (journal-count j2))
                  (test-equal "instance 1 also sees updated count" 3 (journal-count j1))))

              (cleanup-temp-file path)))

;; 8. Integrity Verification - Valid Cases
(test-group "integrity-valid"
            (let* ((path (create-temp-journal-path))
                   (j (make-journal path)))
              ;; Empty journal
              (let ((res (journal-verify-integrity j)))
                (test-assert "empty journal is valid" (assoc-ref res 'valid))
                (test-equal "empty journal has count 0" 0 (assoc-ref res 'count)))

              ;; Multiple valid entries
              (journal-append! j '((feature-id . "F1")))
              (journal-append! j '((feature-id . "F2")))
              (journal-append! j '((feature-id . "F3")))

              (let ((res (journal-verify-integrity j)))
                (test-assert "populated journal is valid" (assoc-ref res 'valid))
                (test-equal "verified count is 3" 3 (assoc-ref res 'count)))

              (cleanup-temp-file path)))

;; 9. Integrity Verification - Corrupted Cases
(test-group "integrity-corrupted-json"
            (let* ((path (create-temp-journal-path))
                   (j (make-journal path)))
              ;; Write 3 lines where line 2 is corrupted JSON
              (call-with-output-file path
                                     (lambda (port)
                                       (display "{\"iteration\": 1, \"feature-id\": \"F1\"}\n" port)
                                       (display "MALFORMED NON JSON LINE\n" port)
                                       (display "{\"iteration\": 3, \"feature-id\": \"F3\"}\n" port)))

              (let ((res (journal-verify-integrity j)))
                (test-assert "corrupt JSON flags valid as #f" (not (assoc-ref res 'valid)))
                (test-equal "corrupt JSON identifies line 2" 2 (assoc-ref res 'corrupt-line))
                (test-assert "corrupt JSON provides error description" (string? (assoc-ref res 'error))))

              ;; Verify journal-entries skips corrupted line and reads valid lines
              (let ((valid-entries (journal-entries j)))
                (test-equal "journal-entries skips corrupt line" 2 (length valid-entries))
                (test-equal "iterations of valid entries"
                            '(1 3) (map (lambda (e) (journal-entry-ref e 'iteration)) valid-entries)))

              (cleanup-temp-file path)))

(test-group "integrity-non-monotonic"
            (let* ((path (create-temp-journal-path))
                   (j (make-journal path)))
              ;; Equal iterations (1 <= 1)
              (call-with-output-file path
                                     (lambda (port)
                                       (display "{\"iteration\": 1, \"feature-id\": \"F1\"}\n" port)
                                       (display "{\"iteration\": 1, \"feature-id\": \"F2\"}\n" port)))

              (let ((res (journal-verify-integrity j)))
                (test-assert "equal iteration flags valid as #f" (not (assoc-ref res 'valid)))
                (test-equal "equal iteration identifies line 2" 2 (assoc-ref res 'corrupt-line))
                (test-assert "error message mentions non-monotonic"
                             (string-contains (assoc-ref res 'error) "Non-monotonic")))

              ;; Decreasing iterations (5 > 2)
              (call-with-output-file path
                                     (lambda (port)
                                       (display "{\"iteration\": 5, \"feature-id\": \"F1\"}\n" port)
                                       (display "{\"iteration\": 2, \"feature-id\": \"F2\"}\n" port)))

              (let ((res (journal-verify-integrity j)))
                (test-assert "decreasing iteration flags valid as #f" (not (assoc-ref res 'valid)))
                (test-equal "decreasing iteration identifies line 2" 2 (assoc-ref res 'corrupt-line)))

              (cleanup-temp-file path)))

(test-group "integrity-missing-or-non-numeric-iteration"
            (let* ((path (create-temp-journal-path))
                   (j (make-journal path)))
              ;; Missing iteration
              (call-with-output-file path
                                     (lambda (port)
                                       (display "{\"iteration\": 1, \"feature-id\": \"F1\"}\n" port)
                                       (display "{\"feature-id\": \"F2\"}\n" port)))

              (let ((res (journal-verify-integrity j)))
                (test-assert "missing iteration flags valid as #f" (not (assoc-ref res 'valid)))
                (test-equal "missing iteration identifies line 2" 2 (assoc-ref res 'corrupt-line)))

              ;; Non-numeric iteration
              (call-with-output-file path
                                     (lambda (port)
                                       (display "{\"iteration\": 1, \"feature-id\": \"F1\"}\n" port)
                                       (display "{\"iteration\": \"two\", \"feature-id\": \"F2\"}\n" port)))

              (let ((res (journal-verify-integrity j)))
                (test-assert "string iteration flags valid as #f" (not (assoc-ref res 'valid)))
                (test-equal "string iteration identifies line 2" 2 (assoc-ref res 'corrupt-line)))

              (cleanup-temp-file path)))

(test-end "journal")
