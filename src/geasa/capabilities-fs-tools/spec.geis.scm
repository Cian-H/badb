;;; spec.geis.scm — Contract specification for capabilities-fs-tools

(use-modules (badb spec)
             (badb kernel registry)
             (geasa capabilities-fs-tools plugin)
             (geasa runtimes-rollback plugin)
             (geasa runtimes-verify plugin)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-64))

(spec-begin "capabilities-fs-tools")

(define %test-dir "/tmp/badb-test-fs-tools")

(define (setup-test-dir!)
        (when (file-exists? %test-dir)
          (system (format #f "rm -rf ~a" %test-dir)))
        (mkdir %test-dir))

(define (teardown-test-dir!)
        (when (file-exists? %test-dir)
          (system (format #f "rm -rf ~a" %test-dir))))

(setup-test-dir!)

(test-group "fs-read-file"
            (let ((sample (string-append %test-dir "/sample.txt")))
              (call-with-output-file sample
                                     (lambda (p) (display "line 1\nline 2\nline 3\n" p)))
              (let ((out (fs-read-file sample 0 2)))
                (test-assert "contains file header" (string-contains out "showing lines 0..1"))
                (test-assert "contains line 1" (string-contains out "line 1"))
                (test-assert "contains line 2" (string-contains out "line 2"))
                (test-assert "elided footer" (string-contains out "1 more lines elided"))))

            (let ((missing (string-append %test-dir "/nonexistent.txt")))
              (test-assert "missing file returns error"
                           (string-contains (fs-read-file missing) "does not exist"))))

(test-group "fs-write-file"
            (let* ((out-file (string-append %test-dir "/written.txt"))
                   (res (fs-write-file out-file "alpha\nbeta\ngamma\n")))
              (test-assert "reports wrote count" (string-contains res "Wrote"))
              (test-assert "file exists on disk" (file-exists? out-file))
              (test-equal "file content matches"
                          "alpha\nbeta\ngamma\n"
                          (call-with-input-file out-file read-string))))

(test-group "fs-edit-file"
            (let ((code-file (string-append %test-dir "/module.scm")))
              (call-with-output-file code-file
                                     (lambda (p) (display "(define (hello) 'old-world)\n(define (foo) 'bar)\n" p)))

              ;; 1. Single replacement
              (let ((res (fs-edit-file code-file "'old-world" "'new-world")))
                (test-assert "edit reports replaced 1 occurrence" (string-contains res "replaced 1 occurrence"))
                (test-assert "disk content updated"
                             (string-contains (call-with-input-file code-file read-string) "'new-world")))

              ;; 2. Target not found
              (let ((res (fs-edit-file code-file "non-existent-token" "replacement")))
                (test-assert "reports target text not found"
                             (string-contains res "target text not found")))

              ;; 3. Multiple matches without replace-all? fails with line numbers
              (call-with-output-file code-file
                                     (lambda (p) (display "dup-token line 1\nother line\ndup-token line 3\n" p)))
              (let ((res (fs-edit-file code-file "dup-token" "unique-token")))
                (test-assert "reports multiple matches" (string-contains res "matched 2 locations"))
                (test-assert "includes line numbers" (string-contains res "lines 1, 3")))

              ;; 4. Multiple matches with replace-all? succeeds
              (let ((res (fs-edit-file code-file "dup-token" "unique-token" #:replace-all? #t)))
                (test-assert "reports 2 occurrences replaced" (string-contains res "replaced 2 occurrence(s)"))
                (test-assert "all occurrences replaced on disk"
                             (not (string-contains (call-with-input-file code-file read-string) "dup-token"))))

              ;; 5. Verification failure on broken Scheme leaves file untouched
              (call-with-output-file code-file
                                     (lambda (p) (display "(define (valid-form) 42)\n" p)))
              (let* ((v (make-verifier #:check-syntax? #t #:run-tests? #f))
                     (res (fs-edit-file code-file "42" "(broken-paren" #:verifier v)))
                (test-assert "edit aborted on verification failure" (string-contains res "Edit aborted"))
                (test-assert "file on disk untouched"
                             (string-contains (call-with-input-file code-file read-string) "42")))))

(test-group "hash-line-anchors"
            ;; FNV-1a anchor contract: trailing-whitespace-trimmed, 12-bit, 3 lowercase hex chars
            (test-equal "FNV-1a anchor golden vector: foo" "ed7" (fs-line-anchor "foo"))
            (test-equal "FNV-1a anchor golden vector: hello" "cab" (fs-line-anchor "hello"))
            (test-equal "FNV-1a anchor golden vector: empty line" "dc5" (fs-line-anchor ""))
            (test-equal "anchor ignores trailing whitespace"
                        (fs-line-anchor "abc")
                        (fs-line-anchor "abc   "))
            (test-assert "anchor is 3 lowercase hex chars"
                         (let
                             ((a (fs-line-anchor "(define (hello) 'old-world)")))
                           (and (= 3 (string-length a)) (string->number a 16) (equal? a "f15"))))
            (let
                ((sample (string-append %test-dir "/anchors.txt")))
              (call-with-output-file sample
                                     (lambda (p)
                                       (display "alpha\nbeta\ngamma\n" p)))
              (let ((out (fs-read-file sample 0 3)))
                (test-assert
                  "read output annotates lines with N:HHH content prefix"
                  (string-contains out "0:dab→alpha"))
                (test-assert
                  "read output annotates later lines with their own anchors"
                  (string-contains out "1:4c7→beta"))
                (test-assert
                  "read header still reports the shown window"
                  (string-contains out "showing lines 0..2")))))

(test-group "fs-edit-lines"
            (define el-file (string-append %test-dir "/edit-lines.txt"))
            (define (write-el! content)
                    (call-with-output-file el-file
                                           (lambda (p)
                                             (display content p))))
            (define (el-content) (call-with-input-file el-file read-string))
            ;; 1. Single anchored range replacement
            (write-el! "one\ntwo\nthree\nfour\n")
            (let
                ((res
                   (fs-edit-lines el-file
                                  (list
                                    '(("from" . 1) ("from_hash" . "829")
                                      ("to" . 2)
                                      ("to_hash" . "3c3")
                                      ("new_text" . "TWO\nTHREE"))))))
              (test-assert "reports the applied edit"
                           (string-contains res "applied 1 edit(s)"))
              (test-equal "replaces the anchored range on disk"
                          "one\nTWO\nTHREE\nfour\n"
                          (el-content)))
            ;; 2. Stale anchor (e.g. lines inserted above since the read) fails closed
            (write-el! "one\ntwo\nthree\nfour\n")
            (let
                ((res
                   (fs-edit-lines el-file
                                  (list
                                    '(("from" . 1) ("from_hash" . "9ef")
                                      ("to" . 1)
                                      ("to_hash" . "9ef")
                                      ("new_text" . "nah"))))))
              (test-assert "stale anchor is rejected" (string-contains res "stale read"))
              (test-assert
                "stale anchor message asks for a re-read"
                (string-contains res "re-read"))
              (test-assert
                "stale anchor leaves the file untouched"
                (string-contains (el-content) "two\nthree")))
            ;; 3. Batches are atomic: one stale edit rejects everything
            (write-el! "one\ntwo\nthree\nfour\n")
            (let
                ((res
                   (fs-edit-lines el-file
                                  (list
                                    '(("from" . 0) ("from_hash" . "9ef")
                                      ("to" . 0)
                                      ("to_hash" . "9ef")
                                      ("new_text" . "ONE!"))
                                    '(("from" . 2) ("from_hash" . "wrong")
                                      ("to" . 2)
                                      ("to_hash" . "3c3")
                                      ("new_text" . "XXX"))))))
              (test-assert
                "batch with a stale edit is rejected"
                (string-contains res "stale read"))
              (test-assert
                "batch rejection leaves the valid edit unapplied"
                (string-contains (el-content) "one\ntwo\nthree\nfour\n")))
            ;; 4. Two valid edits in one batch apply bottom-up
            (write-el! "one\ntwo\nthree\nfour\n")
            (let
                ((res
                   (fs-edit-lines el-file
                                  (list
                                    '(("from" . 0) ("from_hash" . "9ef")
                                      ("to" . 0)
                                      ("to_hash" . "9ef")
                                      ("new_text" . "ONE!"))
                                    '(("from" . 2) ("from_hash" . "3c3")
                                      ("to" . 2)
                                      ("to_hash" . "3c3")
                                      ("new_text" . "THREE!"))))))
              (test-assert "reports both applied edits"
                           (string-contains res "applied 2 edit(s)"))
              (test-equal "both batch edits land on disk"
                          "ONE!\ntwo\nTHREE!\nfour\n"
                          (el-content)))
            ;; 5. Insertion via multi-line new_text keeps the anchored line
            (write-el! "one\ntwo\nthree\n")
            (let
                ((res
                   (fs-edit-lines el-file
                                  (list
                                    '(("from" . 1) ("from_hash" . "829")
                                      ("to" . 1)
                                      ("to_hash" . "829")
                                      ("new_text" . "inserted\ntwo"))))))
              (test-assert "insertion edit applies" (string-contains res "applied 1 edit(s)"))
              (test-equal
                "inserted lines appear above the anchored line"
                "one\ninserted\ntwo\nthree\n"
                (el-content)))
            ;; 6. Deletion via empty new_text
            (write-el! "one\ntwo\nthree\n")
            (let
                ((res
                   (fs-edit-lines el-file
                                  (list
                                    '(("from" . 1) ("from_hash" . "829")
                                      ("to" . 1)
                                      ("to_hash" . "829")
                                      ("new_text" . ""))))))
              (test-assert "deletion edit applies" (string-contains res "applied 1 edit(s)"))
              (test-equal "deleted range is gone from disk" "one\nthree\n" (el-content)))
            ;; 7. Range and shape validation
            (write-el! "one\ntwo\n")
            (test-assert "out-of-range edit is rejected"
                         (string-contains
                           (fs-edit-lines el-file
                                          (list
                                            '(("from" . 0) ("from_hash" . "9ef")
                                              ("to" . 5)
                                              ("to_hash" . "e11")
                                              ("new_text" . "X"))))
                           "outside lines 0..1"))
            (test-assert "inverted range is rejected"
                         (string-contains
                           (fs-edit-lines el-file
                                          (list
                                            '(("from" . 1) ("from_hash" . "829")
                                              ("to" . 0)
                                              ("to_hash" . "9ef")
                                              ("new_text" . "X"))))
                           "outside lines 0..1"))
            (test-assert "overlapping edits are rejected"
                         (string-contains
                           (fs-edit-lines el-file
                                          (list
                                            '(("from" . 0) ("from_hash" . "9ef")
                                              ("to" . 1)
                                              ("to_hash" . "829")
                                              ("new_text" . "A"))
                                            '(("from" . 1) ("from_hash" . "829")
                                              ("to" . 1)
                                              ("to_hash" . "829")
                                              ("new_text" . "B"))))
                           "overlap"))
            (test-assert "missing edit fields are rejected"
                         (string-contains (fs-edit-lines el-file (list '(("from" . 0)))) "needs integer"))
            (test-assert "empty edit list is rejected"
                         (string-contains (fs-edit-lines el-file '()) "at least one edit"))
            ;; 8. JSON-native shapes: vector of symbol-keyed alists
            (write-el! "one\ntwo\nthree\n")
            (let
                ((res
                   (fs-edit-lines el-file
                                  (vector
                                    '((from . 1) (from_hash . "829") (to . 1) (to_hash . "829") (new_text . "zwei"))))))
              (test-assert
                "JSON array/vector edits are accepted"
                (string-contains res "applied 1 edit(s)"))
              (test-equal "vector edit lands on disk" "one\nzwei\nthree\n" (el-content)))
            ;; 9. Verification failure leaves the file untouched
            (call-with-output-file
              (string-append %test-dir "/edit-lines-check.scm")
              (lambda (p)
                (display "(define (valid-form) 42)\n" p)))
            (let*
                ((check-file (string-append %test-dir "/edit-lines-check.scm"))
                 (h (fs-line-anchor "(define (valid-form) 42)"))
                 (v (make-verifier #:check-syntax? #t #:run-tests? #f))
                 (res
                   (fs-edit-lines check-file
                                  (vector
                                    `(("from" . 0) ("from_hash" . ,h)
                                      ("to" . 0)
                                      ("to_hash" . ,h)
                                      ("new_text" . "(broken-paren")))
                                  #:verifier
                                  v)))
              (test-assert
                "verified edit aborts on syntax failure"
                (string-contains res "Edit aborted"))
              (test-assert
                "verified edit leaves the file untouched"
                (string-contains
                  (call-with-input-file check-file
                                        (lambda (p)
                                          (read-string p)))
                  "42"))))

(test-group "register-fs-tools!"
            (let ((reg (make-registry)))
              (register-fs-tools! reg
                                  #:shell-executor
                                  (lambda (cmd)
                                    (list 'ok cmd)))
              (test-assert "read_file registered"
                           (procedure? (registry-lookup reg "read_file")))
              (test-assert "write_file registered"
                           (procedure? (registry-lookup reg "write_file")))
              (test-assert "edit_file registered"
                           (procedure? (registry-lookup reg "edit_file")))
              (test-assert "edit_lines registered"
                           (procedure? (registry-lookup reg "edit_lines")))
              (test-assert "bash registered" (procedure? (registry-lookup reg "bash")))))

(teardown-test-dir!)

(test-group "fs-read-file compiler line numbers"
            (let ((d "/tmp/badb-test-fs-lines"))
              (when (file-exists? d) (system (format #f "rm -rf ~a" d)))
              (mkdir d)
              (let ((nums (string-append d "/numbers.txt")))
                (call-with-output-file nums
                                       (lambda (p)
                                         (for-each
                                           (lambda (i)
                                             (format p "line ~a~%" i))
                                           (iota 20 1))))
                (let
                    ((out (fs-read-file nums #f #f #:start-line 5 #:end-line 7)))
                  (test-assert
                    "1-based range maps to 0-based window 4..6"
                    (string-contains out "showing lines 4..6"))
                  (test-assert "window includes start_line content"
                               (string-contains out "line 5"))
                  (test-assert "window includes end_line content" (string-contains out "line 7")))
                (test-assert "reversed range reports an error"
                             (string-contains
                               (fs-read-file nums #f #f #:start-line 9 #:end-line 3)
                               "before start_line"))
                (let ((out2 (fs-read-file nums "3" "4")))
                  (test-assert "string offset/limit still accepted"
                               (string-contains out2 "showing lines 3..6")))
                (system (format #f "rm -rf ~a" d)))))

(test-group "fs-file-search"
            (let ((d "/tmp/badb-test-fs-search"))
              (when (file-exists? d) (system (format #f "rm -rf ~a" d)))
              (mkdir d)
              (let
                  ((a (string-append d "/searchable.scm")) (b (string-append d "/notes.md")))
                (call-with-output-file a
                                       (lambda (p)
                                         (display "(define (hello)\n  'world)\n\n(define (helper)\n  'other)\n" p)))
                (call-with-output-file b
                                       (lambda (p)
                                         (display "world note\nplain text\nworld again\n" p)))
                (let
                    ((content (fs-file-search "world" #:path d)))
                  (test-assert "header reports match count" (string-contains content "match(es)"))
                  (test-assert "content lines are anchor-annotated"
                               (string-contains content "0:"))
                  (test-assert "anchor arrow rendered" (string-contains content "→"))
                  (test-assert "hash tip nudges edit_lines"
                               (string-contains content "edit_lines from/from_hash")))
                (let
                    ((counted (fs-file-search "world" #:path d #:output-mode "count")))
                  (test-assert "count mode lists per-file tallies"
                               (string-contains counted "searchable.scm:1"))
                  (test-assert "count mode omits content anchors"
                               (not (string-contains counted "→"))))
                (let
                    ((files (fs-file-search "world" #:path d #:output-mode "files_with_matches")))
                  (test-assert "files mode lists paths" (string-contains files "notes.md"))
                  (test-assert "files mode omits line noise" (not (string-contains files ":0:"))))
                (let
                    ((globbed (fs-file-search "world" #:path d #:glob "*.md")))
                  (test-assert
                    "glob excludes non-matching suffixes"
                    (not (string-contains globbed "searchable.scm")))
                  (test-assert "glob keeps matching suffixes"
                               (string-contains globbed "notes.md")))
                (let
                    ((contextual (fs-file-search "plain" #:path b #:context 1)))
                  (test-assert "context spans surrounding lines"
                               (string-contains contextual "world note")))
                (test-assert "missing path errors cleanly"
                             (string-contains
                               (fs-file-search "x" #:path "/no/such/dir-here")
                               "does not exist"))
                (test-assert "invalid regexp errors cleanly"
                             (string-contains (fs-file-search "[oops") "invalid regular expression"))
                (system (format #f "rm -rf ~a" d)))))

(test-group "fs-tools registration schemas"
            (let ((reg (make-registry)))
              (register-fs-tools! reg
                                  #:shell-executor
                                  (lambda (cmd)
                                    "[exit 0]"))
              (test-assert
                "bash schema co-locates the routing constraint"
                (string-contains
                  (format #f "~s" (registry-metadata reg "bash"))
                  "allow-unix-edit"))
              (test-assert
                "read_file schema advertises 1-based compiler lines"
                (string-contains
                  (format #f "~s" (registry-metadata reg "read_file"))
                  "start_line"))
              (test-assert "file_search is registered"
                           (procedure? (registry-lookup reg "file_search")))))

(spec-end)
(spec-exit)
