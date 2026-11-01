;;; test-rollback.scm — Tests for Transactional Time-Travel & Rollback (badb rollback)

(define-module (test test-rollback)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (badb kernel registry)
  #:use-module (badb kernel journal)
  #:use-module (badb runtimes rollback))

(test-begin "badb-rollback")

(define %test-ws "/tmp/badb-test-rollback-ws")
(define %test-jrn-path "/tmp/badb-test-rollback-jrn.jsonl")

(define (setup-test-env!)
  (system (format #f "rm -rf ~a ~a && mkdir -p ~a/src" %test-ws %test-jrn-path %test-ws))
  (call-with-output-file (string-append %test-ws "/src/main.scm")
    (lambda (p) (display "(define (main) 'original)\n" p)))
  (call-with-output-file (string-append %test-ws "/README.md")
    (lambda (p) (display "# Original Readme\n" p))))

(define (teardown-test-env!)
  (system (format #f "rm -rf ~a ~a" %test-ws %test-jrn-path)))

;;;
;;; 1. Manager Construction
;;;
(test-group "rollback-manager-construction"
  (setup-test-env!)
  (let ((mgr (make-rollback-manager #:workspace-dir %test-ws)))
    (test-assert "rollback-manager? recognizes record" (rollback-manager? mgr))
    (test-equal "workspace-dir matches" %test-ws (rollback-manager-workspace-dir mgr))
    (test-assert "initially no checkpoints" (null? (list-checkpoints mgr))))
  (teardown-test-env!))

;;;
;;; 2. File Checkpointing & Restoration
;;;
(test-group "rollback-file-restoration"
  (setup-test-env!)
  (let* ((mgr (make-rollback-manager #:workspace-dir %test-ws))
         (main-file (string-append %test-ws "/src/main.scm"))
         (new-file (string-append %test-ws "/src/extra.scm"))
         (chkpt (create-checkpoint! mgr "pre-edit checkpoint" #:files '("src/main.scm" "src/extra.scm"))))

    (test-assert "checkpoint created" (checkpoint? chkpt))
    (test-equal "checkpoint count is 1" 1 (length (list-checkpoints mgr)))

    ;; Modify existing file and create new file
    (call-with-output-file main-file
      (lambda (p) (display "(define (main) 'corrupted-edit)\n" p)))
    (call-with-output-file new-file
      (lambda (p) (display "(define (extra) 'unwanted)\n" p)))

    (test-assert "main-file has new content"
      (string-contains (call-with-input-file main-file read-line) "corrupted-edit"))
    (test-assert "new-file exists" (file-exists? new-file))

    ;; Perform rollback
    (let ((res (rollback-to-checkpoint! mgr (checkpoint-id chkpt))))
      (test-equal "status is rolled_back" "rolled_back" (assoc-ref res 'status))
      (test-assert "main-file content was restored"
        (string-contains (call-with-input-file main-file read-line) "'original"))
      (test-assert "new-file was deleted" (not (file-exists? new-file)))))
  (teardown-test-env!))

;;;
;;; 3. Registry State Checkpointing & Rollback
;;;
(test-group "rollback-registry-restoration"
  (setup-test-env!)
  (let* ((reg (make-registry))
         (mgr (make-rollback-manager #:workspace-dir %test-ws #:registry reg)))

    (registry-attach! reg "feat-a" (lambda () "version-1") #:metadata '((v . 1)))
    (registry-attach! reg "feat-b" (lambda () "stable") #:metadata '((status . "ok")))

    (let ((chkpt (create-checkpoint! mgr "registry checkpoint" #:files '())))
      ;; Mutate registry: update feat-a and add feat-c
      (registry-attach! reg "feat-a" (lambda () "version-2") #:metadata '((v . 2)))
      (registry-attach! reg "feat-c" (lambda () "experimental") #:metadata '((v . 0)))

      (test-equal "feat-a is v2" 2 (assoc-ref (registry-metadata reg "feat-a") 'v))
      (test-assert "feat-c exists" (registry-lookup reg "feat-c"))

      ;; Rollback
      (let ((res (rollback-to-checkpoint! mgr (checkpoint-id chkpt))))
        (test-equal "status is rolled_back" "rolled_back" (assoc-ref res 'status))
        (test-equal "feat-a restored to v1" 1 (assoc-ref (registry-metadata reg "feat-a") 'v))
        (test-assert "feat-c was detached" (not (registry-lookup reg "feat-c")))
        (test-assert "feat-b still intact" (registry-lookup reg "feat-b")))))
  (teardown-test-env!))

;;;
;;; 4. Checkpoint Queries & Latest Rollback
;;;
(test-group "rollback-queries-and-latest"
  (setup-test-env!)
  (let* ((mgr (make-rollback-manager #:workspace-dir %test-ws))
         (main-file (string-append %test-ws "/src/main.scm"))
         (c1 (create-checkpoint! mgr "C1" #:files '("src/main.scm"))))

    (call-with-output-file main-file (lambda (p) (display "edit 1\n" p)))
    (let ((c2 (create-checkpoint! mgr "C2" #:files '("src/main.scm"))))
      (call-with-output-file main-file (lambda (p) (display "edit 2\n" p)))

      (test-equal "checkpoint count is 2" 2 (length (list-checkpoints mgr)))
      (test-assert "get-checkpoint finds C1" (get-checkpoint mgr (checkpoint-id c1)))

      ;; rollback latest should restore edit 1
      (rollback-latest! mgr)
      (test-equal "restored to edit 1" "edit 1" (call-with-input-file main-file read-line))

      ;; rollback to C1 should restore original
      (rollback-to-checkpoint! mgr (checkpoint-id c1))
      (test-assert "restored to original"
        (string-contains (call-with-input-file main-file read-line) "'original"))))
  (teardown-test-env!))

;;;
;;; 5. with-checkpoint Reversible Block
;;;
(test-group "rollback-with-checkpoint"
            (setup-test-env!)
            (let*
                ((mgr (make-rollback-manager #:workspace-dir %test-ws))
                 (main-file (string-append %test-ws "/src/main.scm")))
              ;; Successful block
              (with-checkpoint mgr
                               "successful block"
                               (lambda ()
                                 (call-with-output-file main-file
                                                        (lambda (p)
                                                          (display "good-edit\n" p)))
                                 'success))
              (test-equal "good-edit preserved" "good-edit" (call-with-input-file main-file read-line))
              (with-checkpoint mgr
                               "falsy block"
                               (lambda ()
                                 (call-with-output-file main-file
                                                        (lambda (p)
                                                          (display "falsy-edit\n" p)))
                                 #f))
              (test-equal "reverted to good-edit after #f return"
                "good-edit" (call-with-input-file main-file read-line))
              ;; Failing block with exception: auto-rolls back
              (catch 'test-error
                (lambda ()
                  (with-checkpoint mgr
                                   "failing block"
                                   (lambda ()
                                     (call-with-output-file main-file
                                                            (lambda (p)
                                                              (display "broken-edit\n" p)))
                                     (throw 'test-error "something broke"))))
                (lambda _
                  #t))
              (test-equal "reverted to good-edit after error"
                "good-edit" (call-with-input-file main-file read-line)))
            (teardown-test-env!))

;;;
;;; 6. Disk Persistence Reload & Full-Scan New-File Cleanup
;;;
(test-group "rollback-persistence-and-new-files"
  (setup-test-env!)
  (let* ((mgr (make-rollback-manager #:workspace-dir %test-ws))
         (main-file (string-append %test-ws "/src/main.scm"))
         (untracked-new (string-append %test-ws "/src/newly-created.scm"))
         (chkpt (create-checkpoint! mgr "full workspace snapshot")))
    ;; Modify main.scm and create a brand-new file not known at checkpoint time
    (call-with-output-file main-file (lambda (p) (display "modified\n" p)))
    (call-with-output-file untracked-new (lambda (p) (display "(define new 1)\n" p)))
    (test-assert "untracked-new exists before rollback" (file-exists? untracked-new))

    ;; Reload manager from disk storage-dir and rollback using reloaded checkpoint
    (let* ((reloaded-mgr (make-rollback-manager #:workspace-dir %test-ws))
           (res (rollback-to-checkpoint! reloaded-mgr (checkpoint-id chkpt))))
      (test-equal "reloaded manager has 1 checkpoint" 1 (length (list-checkpoints reloaded-mgr)))
      (test-equal "reloaded rollback succeeded" "rolled_back" (assoc-ref res 'status))
      (test-assert "main.scm restored from disk snapshot"
        (string-contains (call-with-input-file main-file read-line) "'original"))
      (test-assert "newly-created.scm removed on full-scan rollback"
        (not (file-exists? untracked-new)))))
  (teardown-test-env!))

(test-end "badb-rollback")
