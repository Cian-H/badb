;;; badb — Journal compatibility shim
(define-module (badb journal)
               #:use-module (badb kernel journal)
               #:re-export (make-journal
                             journal?
                             journal-path
                             journal-append!
                             journal-entries
                             journal-latest
                             journal-count
                             journal-entry->json-string
                             json-string->journal-entry
                             journal-verify-integrity
                             journal-entry-ref
                             safe-journal-append!
                             <journal-entry>
                             make-journal-entry
                             journal-entry?
                             journal-entry-timestamp
                             journal-entry-iteration
                             journal-entry-feature-id
                             journal-entry-commit
                             journal-entry-test-verdict
                             journal-entry-author
                             journal-entry-details
                             journal-entry-ast-diff-hash
                             journal-entry-spec-hash
                             journal-entry-quickcheck-seed
                             journal-entry-quickcheck-verdict
                             journal-entry-kernel-replay-verdict))
