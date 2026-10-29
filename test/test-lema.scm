;;; test/test-lema.scm — Lema oracle seam test suite.
;;;
;;; Tests the badb <-> Lema seam end-to-end against scripts/lema-stub
;;; (the protocol test double): the pure SHA-256 digest, socket path
;;; resolution, the client round-trips, the kernel-owned prove gate
;;; (both verdict paths and fail-closed on an unreachable socket), and
;;; the identity-pinning journal fields pinned by the transaction
;;; commit path when BADB_LEMA_SOCKET is set.

(define-module (test test-lema)
  #:use-module (badb journal)
  #:use-module (badb lema)
  #:use-module (badb lema-mcp)
  #:use-module (badb registry)
  #:use-module (badb transaction)
  #:use-module (badb util)
  #:use-module (ice-9 ftw)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;;;
;;; Test helpers
;;;

(define (rm-rf path)
  "Recursively delete the file or directory at PATH."
  (when (file-exists? path)
    (if (file-is-directory? path)
        (begin
          (for-each (lambda (file)
                      (unless (member file '("." ".."))
                        (rm-rf (string-append path "/" file))))
                    (or (scandir path) '()))
          (rmdir path))
        (delete-file path))))

(define (stub-script-path)
  "Locate scripts/lema-stub relative to this test file or the cwd."
  (let ((from-here (string-append (dirname (current-filename))
                                  "/../scripts/lema-stub")))
    (if (file-exists? from-here)
        (canonicalize-path from-here)
        (canonicalize-path "scripts/lema-stub"))))

(define (wait-for-socket! socket-path)
  "Block until SOCKET-PATH exists, polling up to 30 seconds; raise otherwise."
  (let loop ((n 0))
    (cond
     ((file-exists? socket-path) #t)
     ((> n 30) (error "stub socket never appeared:" socket-path))
     (else (sleep 1) (loop (+ n 1))))))

(define (spawn-stub! socket-path)
  "Fork and exec scripts/lema-stub on SOCKET-PATH, wait for the socket
to appear, and return the child pid."
  (let ((pid (primitive-fork)))
    (if (= pid 0)
        (begin
          (setenv "GUILE_AUTO_COMPILE" "0")
          (apply execl "/usr/bin/env" "env" "guile" "--no-auto-compile" "-s"
                 (stub-script-path)
                 (list socket-path)))
        (begin
          (wait-for-socket! socket-path)
          pid))))

(define (kill-stub! pid socket-path)
  "Stop the stub server PID (SIGTERM and reap) and remove SOCKET-PATH."
  (catch #t
    (lambda ()
      (kill pid SIGTERM)
      (waitpid pid))
    (lambda _ #f))
  (when (file-exists? socket-path)
    (delete-file socket-path)))

;;;
;;; Test Suite
;;;

(test-begin "lema")

(test-group "sha256-digest"
  (test-equal "sha256-hex matches the FIPS test vector for \"abc\""
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (sha256-hex "abc"))
  (test-equal "sha256-hex of the empty string"
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (sha256-hex ""))
  (test-equal "sha256-hex over multiple 64-byte blocks"
    "41edece42d63e8d9bf515a9ba6932e1c20cbc9f5a5d134645adb5db1b9737ea3"
    (sha256-hex (make-string 1000 #\a)))
  (test-assert "lema-ast-diff-hash yields a 64-char hex digest"
    (let ((digest (lema-ast-diff-hash '((define (f x) x) (define y 1)))))
      (and (= 64 (string-length digest))
           (every (lambda (c)
                    (memv c (string->list "0123456789abcdef")))
                  (string->list digest))))))

(test-group "socket-path-resolution"
  (let ((saved (getenv "BADB_LEMA_SOCKET")))
    (unsetenv "BADB_LEMA_SOCKET")
    (test-equal "default socket path lives under HOME"
      (string-append (or (getenv "HOME") "/tmp") "/.local/share/lema/lema.sock")
      (lema-socket-path))
    (test-assert "gate is disarmed without BADB_LEMA_SOCKET"
      (not (lema-gate-enabled?)))
    (setenv "BADB_LEMA_SOCKET" "/tmp/lema-test-path.sock")
    (test-equal "BADB_LEMA_SOCKET overrides the socket path"
      "/tmp/lema-test-path.sock" (lema-socket-path))
    (test-equal "lema-gate-enabled? returns the armed socket path"
      "/tmp/lema-test-path.sock" (lema-gate-enabled?))
    (if saved
        (setenv "BADB_LEMA_SOCKET" saved)
        (unsetenv "BADB_LEMA_SOCKET"))))

(test-group "lema-live"
  (let* ((temp-dir (mkdtemp "/tmp/badb-lema-test-XXXXXX"))
         (socket-path (string-append temp-dir "/lema.sock"))
         (saved-env (getenv "BADB_LEMA_SOCKET")))
    (setenv "BADB_LEMA_SOCKET" socket-path)
    (let ((pid (spawn-stub! socket-path)))
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          ;; 1. Client round-trips against the stub.
          (let ((conn (lema-connect socket-path)))
            (test-assert "lema-connect returns a port" (port? conn))
            (test-assert "publish-contract returns ok"
              (assoc-ref (lema-publish-contract! "demo-ok"
                                                 "(define (f x) x)"
                                                 "spec text"
                                                 #:conn conn)
                         "ok"))
            (let ((record (lema-get-contract "demo-ok" #:conn conn)))
              (test-equal "get-contract round-trips the id"
                "demo-ok" (assoc-ref record "id"))
              (test-equal "get-contract round-trips the code"
                "(define (f x) x)" (assoc-ref record "code"))
              (test-equal "get-contract round-trips the spec"
                "spec text" (assoc-ref record "spec")))
            (test-equal "submit passes for a published contract"
              "passed" (assoc-ref (lema-submit! "demo-ok" "hash1" "evidence"
                                                #:conn conn)
                                  "verdict"))
            (test-assert "publish-contract for the failing id"
              (assoc-ref (lema-publish-contract! "demo-fail" "code" "spec"
                                                 #:conn conn)
                         "ok"))
            (test-equal "submit fails for the -fail id convention"
              "failed" (assoc-ref (lema-submit! "demo-fail" "hash1" "evidence"
                                                #:conn conn)
                                  "verdict"))
            (test-assert "replay-errors records the failure"
              (> (vector-length (assoc-ref (lema-replay-errors "demo-fail"
                                                               #:conn conn)
                                            "errors"))
                 0))
            (test-assert "list-verified contains the passing contract only"
              (and (member "demo-ok"
                           (vector->list (assoc-ref (lema-list-verified #:conn conn)
                                                    "ids")))
                   (not (member "demo-fail"
                                (vector->list (assoc-ref (lema-list-verified #:conn conn)
                                                         "ids"))))))
            (lema-close conn))

          ;; 2. The kernel-owned prove gate.
          (test-equal "prove gate returns the passed verdict"
            "passed" (assoc-ref (lema-prove-gate "demo-ok" "hash1" "evidence")
                                'verdict))
          (test-equal "prove gate returns the failed verdict for -fail"
            "failed" (assoc-ref (lema-prove-gate "demo-fail" "hash1" "evidence")
                                'verdict))
          (setenv "BADB_LEMA_SOCKET" (string-append temp-dir "/nonexistent.sock"))
          (test-error "prove gate raises on an unreachable socket"
            'lema-unreachable
            (lema-prove-gate "demo-ok" "hash1" "evidence"))
          (test-assert "prove gate raises rather than returning a pass"
            (catch #t
              (lambda ()
                (lema-prove-gate "demo-ok" "hash1" "evidence")
                #f)
              (lambda (key . args)
                (eq? key 'lema-unreachable))))
          (setenv "BADB_LEMA_SOCKET" socket-path)

          ;; 3. Identity pinning on the transaction commit path.
          (lema-publish-contract! "pin-ok" "(define (pin-me x) (+ x 1))" "spec")
          (let* ((jrn (make-journal (string-append temp-dir "/journal.jsonl")))
                 (mgr (make-transaction-manager (make-registry) jrn))
                 (forms '((define (pin-feature-ok x) (+ x 1)))))
            (test-equal "gate-enabled commit succeeds"
              "committed"
              (assoc-ref (apply-transaction! mgr "pin-feature-ok" forms
                                            (lambda (mod) #t)
                                            #:metadata '((contract-id . "pin-ok")))
                         'status))
            (let ((latest (journal-latest jrn)))
              (test-equal "commit record pins the contract id"
                "pin-ok" (assoc-ref latest "contract-id"))
              (test-equal "commit record pins the kernel replay verdict"
                "passed" (assoc-ref latest "kernel-replay-verdict"))
              (test-equal "commit record pins the normalized AST diff hash"
                (lema-ast-diff-hash forms)
                (assoc-ref latest "ast-diff-hash"))))

          ;; 4. A failed gate verdict reverts the commit (fail closed).
          (lema-publish-contract! "pin-fail" "(define (pin-me x) x)" "spec")
          (let* ((jrn2 (make-journal (string-append temp-dir "/journal2.jsonl")))
                 (mgr2 (make-transaction-manager (make-registry) jrn2)))
            (test-equal "failed gate verdict reverts the transaction"
              "reverted"
              (assoc-ref (apply-transaction! mgr2 "pin-feature-fail"
                                              '((define (pin-feature-fail x) x))
                                              (lambda (mod) #t)
                                              #:metadata '((contract-id . "pin-fail")))
                         'status))
            (test-assert "reverted feature is not in the registry"
              (not (registry-lookup (transaction-manager-registry mgr2)
                                    "pin-feature-fail")))
            (test-equal "revert checkpoint logged for the failed gate"
              "revert" (assoc-ref (journal-latest jrn2) "checkpoint")))

          ;; 5. Without the env flag the commit path is unchanged.
          (unsetenv "BADB_LEMA_SOCKET")
          (let* ((jrn3 (make-journal (string-append temp-dir "/journal3.jsonl")))
                 (mgr3 (make-transaction-manager (make-registry) jrn3)))
            (test-equal "gate-disabled commit succeeds without contacting Lema"
              "committed"
              (assoc-ref (apply-transaction! mgr3 "pin-no-gate"
                                              '((define (pin-no-gate x) x))
                                              (lambda (mod) #t)
                                              #:metadata '((contract-id . "pin-ok")))
                         'status))
            (let ((latest (journal-latest jrn3)))
              (test-assert "no contract-id pinned when the gate is disarmed"
                (not (assoc-ref latest "contract-id")))
              (test-assert "no kernel-replay-verdict pinned when the gate is disarmed"
                (not (assoc-ref latest "kernel-replay-verdict")))
              (test-assert "no ast-diff-hash pinned when the gate is disarmed"
                (not (assoc-ref latest "ast-diff-hash")))))
          (setenv "BADB_LEMA_SOCKET" socket-path)

          ;; 6. The lema MCP server (authoring/diagnostics).
          (let ((names (map (lambda (tool) (assoc-ref tool 'name))
                            (vector->list (assoc-ref (lema-mcp-server) "tools")))))
            (test-equal "lema MCP advertises the five authoring/diagnostics tools"
              '("publish-contract" "submit" "replay-errors" "list-verified" "get-contract")
              names)
            (test-equal "lema MCP advertises the expected server name"
              "lema" (assoc-ref (lema-mcp-server) "name")))
          (let* ((handlers (assoc-ref (lema-mcp-server) "handlers"))
                 (pub-fn (assoc-ref handlers "publish-contract"))
                 (list-fn (assoc-ref handlers "list-verified")))
            (test-assert "lema MCP publish-contract handler round-trips"
              (assoc-ref (json-string->scm
                          (pub-fn `(("id" . "mcp-c")
                                    ("code" . "c")
                                    ("spec" . "s"))))
                         "ok"))
            (test-assert "lema MCP list-verified handler reaches the same socket"
              (member "demo-ok"
                      (vector->list
                       (assoc-ref (json-string->scm (list-fn '()))
                                  "ids"))))))
        (lambda ()
          (kill-stub! pid socket-path)
          (if saved-env
              (setenv "BADB_LEMA_SOCKET" saved-env)
              (unsetenv "BADB_LEMA_SOCKET"))
          (rm-rf temp-dir))))))

(test-end "lema")
