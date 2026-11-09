;;; badb — Transactional Time-Travel & Rollback Engine
;;;
;;; Provides workspace filesystem and kernel registry checkpointing,
;;; atomic rollbacks, undo history, and time-travel state restoration.

(define-module (geasa runtimes-rollback plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (badb kernel registry)
               #:use-module (badb kernel journal)
               #:use-module (geasa foundation-log plugin)
               #:use-module (ice-9 rdelim)
               #:use-module (ice-9 format)
               #:use-module (ice-9 optargs)
               #:use-module (json)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:use-module (uuid generate)
               #:use-module (uuid utils)
               #:use-module (pfds hamts)
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
                          scan-text-files
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
        "Read the entire content of PATH as string, or #f if unreadable/non-existent/binary."
        (and (file-exists? path)
             (not (file-is-directory? path))
             (let ((s (file->string path)))
               (and (string? s) (not (string-index s #\nul)) s))))

(define write-file-content string->file)

;;;
;;; Data Records
;;;
(define-record-type <checkpoint>
                    (%make-checkpoint id description timestamp files registry-state full-scan?)
                    checkpoint?
                    (id checkpoint-id)
                    (description checkpoint-description)
                    (timestamp checkpoint-timestamp)
                    (files checkpoint-files set-checkpoint-files!)                   ;; HAMT: path -> (existed? . content)
                    (registry-state checkpoint-registry-state set-checkpoint-registry-state!) ;; HAMT: feature-id -> (impl . metadata)
                    (full-scan? checkpoint-full-scan?))

(define* (make-checkpoint id description #:key (timestamp #f) (files '()) (registry-state '()) (full-scan? #f))
         (%make-checkpoint id
                           description
                           (or timestamp (current-iso8601-timestamp))
                           (if (hamt? files) files (alist->hamt files string-hash string=?))
                           (if (hamt? registry-state) registry-state (alist->hamt registry-state string-hash string=?))
                           full-scan?))

(define-record-type <rollback-manager>
                    (%make-rollback-manager workspace-dir registry journal storage-dir checkpoints)
                    rollback-manager?
                    (workspace-dir rollback-manager-workspace-dir)
                    (registry rollback-manager-registry)
                    (journal rollback-manager-journal)
                    (storage-dir rollback-manager-storage-dir)
                    (checkpoints rollback-manager-checkpoints set-rollback-manager-checkpoints!))

(define (load-persisted-checkpoints storage-dir)
        "Load persisted <checkpoint> records from STORAGE-DIR JSON files if present."
        (if (and storage-dir (file-exists? storage-dir) (file-is-directory? storage-dir))
          (catch #t
            (lambda ()
              (let* ((stream (opendir storage-dir))
                     (entries (let loop ((acc '()))
                                        (let ((e (readdir stream)))
                                          (if (eof-object? e)
                                            (begin (closedir stream) (sort acc string>?))
                                            (loop (cons e acc)))))))
                (filter-map
                  (lambda (fname)
                    (and (string-suffix? ".json" fname)
                         (catch #t
                           (lambda ()
                             (let* ((full (string-append storage-dir "/" fname))
                                    (parsed (call-with-input-file full json->scm))
                                    (id (assoc-ref parsed "id"))
                                    (desc (or (assoc-ref parsed "description") ""))
                                    (ts (assoc-ref parsed "timestamp"))
                                    (full-scan? (and (assoc-ref parsed "full-scan") #t))
                                    (raw-files (assoc-ref parsed "files"))
                                    (files-list (cond
                                                  ((vector? raw-files) (vector->list raw-files))
                                                  ((list? raw-files) raw-files)
                                                  (else '())))
                                    (file-snaps
                                      (filter-map
                                        (lambda (item)
                                          (and (list? item)
                                               (let ((p (assoc-ref item "path"))
                                                     (ex (assoc-ref item "existed"))
                                                     (ct (assoc-ref item "content")))
                                                 (and p (cons p (cons (and ex #t) (and ex ct)))))))
                                        files-list)))
                               (and id
                                    (make-checkpoint id desc
                                                     #:timestamp ts
                                                     #:files file-snaps
                                                     #:full-scan? full-scan?))))
                           (lambda _ #f))))
                  entries)))
            (lambda _ '()))
          '()))

(define* (make-rollback-manager #:key
                                (workspace-dir (getcwd))
                                (registry #f)
                                (journal #f)
                                (storage-dir #f))
         "Create a new <rollback-manager> instance."
         (let* ((eff-storage (or storage-dir (string-append workspace-dir "/.badb/checkpoints")))
                (persisted (load-persisted-checkpoints eff-storage)))
           (%make-rollback-manager workspace-dir registry journal eff-storage persisted)))

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
        "Snapshot features and metadata from REGISTRY as a HAMT."
        (if (and reg (registry? reg))
          (let ((entries (registry-list reg))
                (hamt (make-hamt string-hash string=?)))
            (fold (lambda (entry acc)
                    (let* ((f-id (assoc-ref entry 'id))
                           (meta (assoc-ref entry 'metadata))
                           (impl (registry-lookup reg f-id)))
                      (hamt-set acc f-id (cons impl meta))))
                  hamt
                  entries))
          (make-hamt string-hash string=?)))

(define (resolve-workspace-path ws rel-path)
        "Resolve REL-PATH against workspace directory WS."
        (if (string-prefix? "/" rel-path)
          rel-path
          (string-append ws "/" rel-path)))

(define* (create-checkpoint! mgr description #:key (files #f))
         "Create a snapshot checkpoint of the specified FILES (or entire workspace if #f)
and active registry state. Persists metadata to disk and journal if configured."
         (unless (rollback-manager? mgr)
           (error "create-checkpoint!: expected <rollback-manager>" mgr))
         (let*
             ((ws (rollback-manager-workspace-dir mgr))
              (reg (rollback-manager-registry mgr))
              (jrn (rollback-manager-journal mgr))
              (id (uuid->string (generate-uuid 'uuidv4)))
              (full-scan? (not (list? files)))
              (target-files
                (cond
                  ((list? files) files)
                  (else (scan-text-files ws))))
              (file-snapshots
                (let ((hamt (make-hamt string-hash string=?)))
                  (fold (lambda (rel-path acc)
                          (let* ((full-path (resolve-workspace-path ws rel-path))
                                 (existed? (file-exists? full-path))
                                 (content (and existed? (read-file-content full-path))))
                            (hamt-set acc rel-path (cons existed? content))))
                        hamt
                        target-files)))
              (reg-state (snapshot-registry reg))
              (chkpt
                (make-checkpoint id
                                 description
                                 #:files
                                 file-snapshots
                                 #:registry-state
                                 reg-state
                                 #:full-scan?
                                 full-scan?)))
           ;; Prepend to in-memory checkpoint list
           (set-rollback-manager-checkpoints! mgr
                                              (cons chkpt (rollback-manager-checkpoints mgr)))
           ;; Persist to disk (including file snapshots)
           (let
               ((chkpt-file (format #f "~a/~a.json" (rollback-manager-storage-dir mgr) id))
                (files-count (if (hamt? file-snapshots) (hamt-size file-snapshots) (length file-snapshots)))
                (registry-count (if (hamt? reg-state) (hamt-size reg-state) (length reg-state)))
                (serialized-files
                  (list->vector
                    (if (hamt? file-snapshots)
                      (hamt-fold (lambda (path file-data acc)
                                   (let ((existed? (car file-data))
                                         (content (cdr file-data)))
                                     (cons `((path . ,path)
                                             (existed . ,existed?)
                                             (content . ,(or content "")))
                                           acc)))
                                 '()
                                 file-snapshots)
                      (map (lambda (entry)
                             `((path . ,(car entry))
                               (existed . ,(cadr entry))
                               (content . ,(or (cddr entry) ""))))
                           file-snapshots)))))
             (catch #t
               (lambda ()
                 (ensure-parent-directory! chkpt-file)
                 (call-with-output-file chkpt-file
                                        (lambda (port)
                                          (scm->json
                                            `((id . ,id) (description . ,description)
                                              (timestamp . ,(checkpoint-timestamp chkpt))
                                              (full-scan . ,full-scan?)
                                              (files-count . ,files-count)
                                              (registry-count . ,registry-count)
                                              (files . ,serialized-files))
                                            port))))
               (lambda _
                 #f)))
           ;; Record in Journal if present
           (safe-journal-append! jrn
                                 `((checkpoint . ,id)
                                   (status . "checkpoint_created")
                                   (feature-id . "rollback")
                                   (details .
                                            ((description . ,description)
                                             (files-count .
                                                          ,(if (hamt? file-snapshots)
                                                             (hamt-size file-snapshots)
                                                             (length file-snapshots)))))))
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
        "Restore kernel registry to SAVED-STATE (HAMT or alist)."
        (when (and reg (registry? reg))
          ;; Detach current features not in saved state
          (let* ((current-entries (registry-list reg))
                 (current-feats (map (lambda (e) (assoc-ref e 'id)) current-entries))
                 (saved-feat-names (if (hamt? saved-state)
                                     (hamt-fold (lambda (k v acc) (cons k acc)) '() saved-state)
                                     (map car saved-state))))
            (for-each (lambda (f)
                        (unless (member f saved-feat-names)
                          (registry-detach! reg f)))
                      current-feats))
          ;; Re-attach saved features
          (if (hamt? saved-state)
            (hamt-fold (lambda (f-id entry _)
                         (let* ((impl (car entry))
                                (meta (cdr entry)))
                           (registry-attach! reg f-id impl #:metadata meta)))
                       #f
                       saved-state)
            (for-each (lambda (entry)
                        (let* ((f-id (car entry))
                               (impl (cadr entry))
                               (meta (cddr entry)))
                          (registry-attach! reg f-id impl #:metadata meta)))
                      saved-state))))

(define (rollback-to-checkpoint! mgr checkpoint-id)
        "Rollback workspace files and kernel registry to the state at CHECKPOINT-ID.
Returns an alist describing restoration results."
        (unless (rollback-manager? mgr)
          (error "rollback-to-checkpoint!: expected <rollback-manager>" mgr))
        (let ((chkpt (get-checkpoint mgr checkpoint-id)))
          (if chkpt
            (let* ((ws (rollback-manager-workspace-dir mgr))
                   (reg (rollback-manager-registry mgr))
                   (jrn (rollback-manager-journal mgr))
                   (restored-files '())
                   (removed-files '()))
              ;; 1. Restore files recorded in checkpoint
              (let* ((files-hamt (checkpoint-files chkpt))
                     (files-list (if (hamt? files-hamt)
                                   (hamt-fold (lambda (k v acc) (cons (cons k v) acc)) '() files-hamt)
                                   files-hamt)))
                (for-each
                  (lambda (entry)
                    (let* ((rel-path (car entry))
                           (file-data (cdr entry))
                           (existed? (car file-data))
                           (content (cdr file-data))
                           (full-path (resolve-workspace-path ws rel-path)))
                      (cond
                        (existed?
                          (unless (and (file-exists? full-path)
                                       (let ((cur (read-file-content full-path)))
                                         (and cur (string=? cur content))))
                            (write-file-content full-path content))
                          (set! restored-files (cons rel-path restored-files)))
                        (else
                          ;; File didn't exist at checkpoint: delete if it was created
                          (when (file-exists? full-path)
                            (delete-file full-path)
                            (set! removed-files (cons rel-path removed-files)))))))
                  files-list))
              ;; 1b. If checkpoint was a full workspace scan, remove any newly created files
              (when (checkpoint-full-scan? chkpt)
                (let ((files-data (checkpoint-files chkpt)))
                  (for-each
                    (lambda (cur-rel)
                      (let ((in-chkpt? (if (hamt? files-data)
                                         (hamt-ref files-data cur-rel #f)
                                         (assoc cur-rel files-data))))
                        (unless in-chkpt?
                          (let ((full-path (resolve-workspace-path ws cur-rel)))
                            (when (file-exists? full-path)
                              (delete-file full-path)
                              (set! removed-files (cons cur-rel removed-files)))))))
                    (scan-text-files ws))))
              ;; 2. Restore Registry
              (restore-registry! reg (checkpoint-registry-state chkpt))
              ;; 3. Log to Journal
              (safe-journal-append! jrn
                                    `((checkpoint . ,checkpoint-id)
                                      (status . "rolled_back")
                                      (feature-id . "rollback")
                                      (details . ((description . ,(checkpoint-description chkpt))
                                                  (restored-count . ,(length restored-files))
                                                  (removed-count . ,(length removed-files))))))
              `((status . "rolled_back")
                (checkpoint-id . ,checkpoint-id)
                (description . ,(checkpoint-description chkpt))
                (restored-files . ,(reverse restored-files))
                (removed-files . ,(reverse removed-files))))
            `((status . "error")
              (error . ,(format #f "Checkpoint '~a' not found" checkpoint-id))))))

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