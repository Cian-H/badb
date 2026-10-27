;;; badb — Transactional Time-Travel & Rollback Engine
;;;
;;; Provides workspace filesystem and kernel registry checkpointing,
;;; atomic rollbacks, undo history, and time-travel state restoration.

(define-module (badb rollback)
  #:use-module (badb util)
  #:use-module (badb registry)
  #:use-module (badb journal)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (<rollback-manager>
            make-rollback-manager
            rollback-manager?
            rollback-manager-workspace-dir
            rollback-manager-registry
            rollback-manager-journal
            rollback-manager-storage-dir
            rollback-manager-checkpoints

            <checkpoint>
            make-checkpoint
            checkpoint?
            checkpoint-id
            checkpoint-description
            checkpoint-timestamp
            checkpoint-files
            checkpoint-registry-state

            ;; Operations
            create-checkpoint!
            list-checkpoints
            get-checkpoint
            rollback-to-checkpoint!
            rollback-latest!
            delete-checkpoint!
            with-checkpoint))

;;;
;;; Filesystem Helpers
;;;
(define (read-file-content path)
  "Read the entire content of PATH as string, or #f if unreadable/non-existent."
  (catch #t
    (lambda ()
      (if (file-exists? path)
          (call-with-input-file path read-string)
          #f))
    (lambda _ #f)))

(define (write-file-content path content)
  "Write CONTENT string to PATH, creating parent directories if needed."
  (ensure-parent-directory! path)
  (call-with-output-file path
    (lambda (port) (display (or content "") port))))

;;;
;;; Data Records
;;;
(define-record-type <checkpoint>
  (%make-checkpoint id description timestamp files registry-state)
  checkpoint?
  (id checkpoint-id)
  (description checkpoint-description)
  (timestamp checkpoint-timestamp)
  (files checkpoint-files)                   ;; alist: ((rel-path . (existed? . content)))
  (registry-state checkpoint-registry-state)) ;; alist: ((feature-id . (impl . metadata)))

(define* (make-checkpoint id description #:key (timestamp #f) (files '()) (registry-state '()))
  (%make-checkpoint id
                    description
                    (or timestamp (date->string (current-date 0) "~4"))
                    files
                    registry-state))

(define-record-type <rollback-manager>
  (%make-rollback-manager workspace-dir registry journal storage-dir checkpoints)
  rollback-manager?
  (workspace-dir rollback-manager-workspace-dir)
  (registry rollback-manager-registry)
  (journal rollback-manager-journal)
  (storage-dir rollback-manager-storage-dir)
  (checkpoints rollback-manager-checkpoints set-rollback-manager-checkpoints!))

(define* (make-rollback-manager #:key
                                (workspace-dir (getcwd))
                                (registry #f)
                                (journal #f)
                                (storage-dir #f))
  "Create a new <rollback-manager> instance."
  (let ((eff-storage (or storage-dir (string-append workspace-dir "/.badb/checkpoints"))))
    (%make-rollback-manager workspace-dir registry journal eff-storage '())))

;;;
;;; Checkpoint Creation
;;;
(define %ignore-dirs
  '(".git" ".cache" "node_modules" "target" ".badb" "dist" "build"))

(define (scan-text-files root-dir)
  "Return relative paths of files under ROOT-DIR, excluding %ignore-dirs."
  (define (walk dir prefix)
    (catch 'system-error
      (lambda ()
        (let* ((stream (opendir dir))
               (entries (let loop ((acc '()))
                          (let ((e (readdir stream)))
                            (if (eof-object? e)
                                (begin (closedir stream) (reverse acc))
                                (loop (cons e acc)))))))
          (append-map
           (lambda (entry)
             (if (or (member entry '("." ".."))
                     (member entry %ignore-dirs)
                     (string-prefix? "." entry))
                 '()
                 (let* ((full (string-append dir "/" entry))
                        (rel (if (string-null? prefix) entry (string-append prefix "/" entry))))
                   (cond
                    ((file-is-directory? full) (walk full rel))
                    ((file-exists? full) (list rel))
                    (else '())))))
           entries)))
      (lambda _ '())))
  (walk root-dir ""))

(define (snapshot-registry reg)
  "Snapshot features and metadata from REGISTRY."
  (if (and reg (registry? reg))
      (let ((entries (registry-list reg)))
        (map (lambda (entry)
               (let* ((f-id (assoc-ref entry 'id))
                      (meta (assoc-ref entry 'metadata))
                      (impl (registry-lookup reg f-id)))
                 (cons f-id (cons impl meta))))
             entries))
      '()))

(define*
  (create-checkpoint! mgr description #:key (files #f))
  "Create a snapshot checkpoint of the specified FILES (or entire workspace if #f)
and active registry state. Persists metadata to disk and journal if configured."
  (unless (rollback-manager? mgr)
    (error "create-checkpoint!: expected <rollback-manager>" mgr))
  (let*
      ((ws (rollback-manager-workspace-dir mgr))
       (reg (rollback-manager-registry mgr))
       (jrn (rollback-manager-journal mgr))
       (id (format #f "chkpt-~a-~a" (current-time) (random 100000)))
       (target-files
         (cond
           ((list? files) files)
           (else (scan-text-files ws))))
       (file-snapshots
         (map
           (lambda (rel-path)
             (let*
                 ((full-path
                    (if (string-prefix? "/" rel-path)
                      rel-path
                      (string-append ws "/" rel-path)))
                  (existed? (file-exists? full-path))
                  (content (and existed? (read-file-content full-path))))
               (cons rel-path (cons existed? content))))
           target-files))
       (reg-state (snapshot-registry reg))
       (chkpt
         (make-checkpoint id
                          description
                          #:files
                          file-snapshots
                          #:registry-state
                          reg-state)))
    ;; Prepend to in-memory checkpoint list
    (set-rollback-manager-checkpoints! mgr
                                       (cons chkpt (rollback-manager-checkpoints mgr)))
    ;; Persist to disk
    (let
        ((chkpt-file (format #f "~a/~a.json" (rollback-manager-storage-dir mgr) id)))
      (catch #t
        (lambda ()
          (ensure-parent-directory! chkpt-file)
          (call-with-output-file chkpt-file
                                 (lambda (port)
                                   (scm->json
                                     `((id . ,id) (description . ,description)
                                       (timestamp . ,(checkpoint-timestamp chkpt))
                                       (files-count . ,(length file-snapshots))
                                       (registry-count . ,(length reg-state)))
                                     port))))
        (lambda _
          #f)))
    ;; Record in Journal if present
    (when (and jrn (journal? jrn))
      (catch #t
        (lambda ()
          (journal-append! jrn
                           `((checkpoint . ,id)
                             (status . "checkpoint_created")
                             (feature-id . "rollback")
                             (details .
                                      ((description . ,description) (files-count . ,(length file-snapshots)))))))
        (lambda _
          #f)))
    chkpt))

;;;
;;; Checkpoint Queries
;;;
(define (list-checkpoints mgr)
  "Return list of all recorded <checkpoint> records, newest first."
  (rollback-manager-checkpoints mgr))

(define (get-checkpoint mgr id)
  "Find a checkpoint by ID."
  (find (lambda (c) (string=? (checkpoint-id c) id))
        (rollback-manager-checkpoints mgr)))

;;;
;;; Rollback Application
;;;
(define (restore-registry! reg saved-state)
  "Restore kernel registry to SAVED-STATE."
  (when (and reg (registry? reg) (list? saved-state))
    ;; Detach current features not in saved state
    (let* ((current-entries (registry-list reg))
           (current-feats (map (lambda (e) (assoc-ref e 'id)) current-entries))
           (saved-feat-names (map car saved-state)))
      (for-each (lambda (f)
                  (unless (member f saved-feat-names)
                    (registry-detach! reg f)))
                current-feats))
    ;; Re-attach saved features
    (for-each (lambda (entry)
                (let* ((f-id (car entry))
                       (impl (cadr entry))
                       (meta (cddr entry)))
                  (registry-attach! reg f-id impl #:metadata meta)))
              saved-state)))

(define
  (rollback-to-checkpoint! mgr checkpoint-id)
  "Rollback workspace files and kernel registry to the state at CHECKPOINT-ID.
Returns an alist describing restoration results."
  (unless (rollback-manager? mgr)
    (error "rollback-to-checkpoint!: expected <rollback-manager>" mgr))
  (let ((chkpt (get-checkpoint mgr checkpoint-id)))
    (if (not chkpt)
      `((status . "error")
        (error . ,(format #f "Checkpoint '~a' not found" checkpoint-id)))
      (let*
          ((ws (rollback-manager-workspace-dir mgr))
           (reg (rollback-manager-registry mgr))
           (jrn (rollback-manager-journal mgr))
           (restored-files '())
           (removed-files '()))
        ;; 1. Restore files recorded in checkpoint
        (for-each
          (lambda (entry)
            (let*
                ((rel-path (car entry)) (existed? (cadr entry))
                 (content (cddr entry))
                 (full-path
                   (if (string-prefix? "/" rel-path)
                     rel-path
                     (string-append ws "/" rel-path))))
              (cond
                (existed?
                  (unless
                      (and (file-exists? full-path)
                           (let ((cur (read-file-content full-path))) (and cur (string=? cur content))))
                    (write-file-content full-path content))
                  (set! restored-files (cons rel-path restored-files)))
                (else
                  ;; File didn't exist at checkpoint: delete if it was created
                  (when (file-exists? full-path)
                    (delete-file full-path)
                    (set! removed-files (cons rel-path removed-files)))))))
          (checkpoint-files chkpt))
        ;; 2. Restore Registry
        (restore-registry! reg (checkpoint-registry-state chkpt))
        ;; 3. Log to Journal
        (when (and jrn (journal? jrn))
          (catch #t
            (lambda ()
              (journal-append! jrn
                               `((checkpoint . ,checkpoint-id)
                                 (status . "rolled_back")
                                 (feature-id . "rollback")
                                 (details .
                                          ((description . ,(checkpoint-description chkpt))
                                           (restored-count . ,(length restored-files))
                                           (removed-count . ,(length removed-files)))))))
            (lambda _
              #f)))
        `((status . "rolled_back")
          (checkpoint-id . ,checkpoint-id)
          (description . ,(checkpoint-description chkpt))
          (restored-files . ,(reverse restored-files))
          (removed-files . ,(reverse removed-files)))))))

(define (rollback-latest! mgr)
  "Rollback to the most recent checkpoint."
  (let ((checkpoints (rollback-manager-checkpoints mgr)))
    (if (null? checkpoints)
        '((status . "error") (error . "No checkpoints available to rollback"))
        (rollback-to-checkpoint! mgr (checkpoint-id (car checkpoints))))))

(define (delete-checkpoint! mgr id)
  "Remove a checkpoint from memory and storage."
  (set-rollback-manager-checkpoints!
   mgr
   (filter (lambda (c) (not (string=? (checkpoint-id c) id)))
           (rollback-manager-checkpoints mgr)))
  (let ((chkpt-file (format #f "~a/~a.json" (rollback-manager-storage-dir mgr) id)))
    (when (file-exists? chkpt-file)
      (catch #t (lambda () (delete-file chkpt-file)) (lambda _ #f))))
  #t)

;;;
;;; Reversible Block Execution
;;;
(define (with-checkpoint mgr description thunk)
        "Execute THUNK inside a checkpointed block. If THUNK completes normally without
returning #f or error alist, returns the result. If THUNK raises an exception or
returns #f, automatically rolls back to the checkpoint and re-raises/returns failure."
        (let
            ((chkpt (create-checkpoint! mgr description)))
          (catch #t
            (lambda ()
              (let ((result (thunk)))
                (if
                    (or (not result)
                        (and (pair? result) (equal? (assoc-ref result 'status) "error")))
                  (begin (rollback-to-checkpoint! mgr (checkpoint-id chkpt)) result)
                  result)))
            (lambda (key . args)
              (rollback-to-checkpoint! mgr (checkpoint-id chkpt))
              (apply throw key args)))))