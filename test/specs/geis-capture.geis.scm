;;; test/specs/geis-capture.geis.scm — Tests for geis prompt capture, bundle
;;; assembly, and bundle replay.
;;;
;;; Covers the provenance half of the bundle system: /geis add capture
;;; (pending prompt store), geis-bundle-assemble! (workflow + prompts +
;;; cassette + locked spec landing in the bundle with a refreshed pinned
;;; manifest), and geis-bundle-replay (cassette-mode re-synthesis against
;;; the locked spec with a plugin content-hash comparison).  The replay
;;; group uses the real Sema binary with the runtimes-synthesis pilot's
;;; recorded cassette, so it is a full-stack test.
;;;
;;; All state is isolated: BADB_GEIS_PATH and BADB_GEIS_GATE_ROOT point at
;;; a mkdtemp workspace; the repo fixture math-utils is mutated and restored
;;; around the replay group exactly as the runtimes-synthesis spec does.

(define-module (test test-geis-capture)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (badb geis bundle)
  #:use-module (badb kernel geis-gate)
  #:use-module (badb kernel digest)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1))

(define Q (string #\"))

(define (write-file! path text)
  (call-with-output-file path (lambda (port) (display text port))))

(define (read-file-string path)
  (call-with-input-file path
    (lambda (port) (set-port-encoding! port "UTF-8") (get-string-all port))))

;; The module content the cassette's write_file interactions record - the
;; honest "synthesized" form to pin as the replay's expected plugin.
(define (cassette-write-content cassette-path)
  (call-with-input-file cassette-path
    (lambda (port)
      (let loop ((line (read-line port)) (content #f))
        (if (eof-object? line)
            content
            (let* ((entry (catch #t
                            (lambda () (json-string->scm line))
                            (lambda _ #f)))
                   (tcs (and entry (assoc-ref entry "tool_calls"))))
              (if (vector? tcs)
                  (let scan-tcs ((i 0) (found content))
                    (if (>= i (vector-length tcs))
                        (loop (read-line port) found)
                        (let* ((tc (vector-ref tcs i))
                               (args (and tc (assoc-ref tc "arguments"))))
                          (if (and args (assoc-ref args "content"))
                              (scan-tcs (+ i 1) (assoc-ref args "content"))
                              (scan-tcs (+ i 1) found)))))
                  (loop (read-line port) content))))))))

;; A runnable spec fixture that passes the gate's diagnostics contract,
;; built from parts so no literal embeds quote characters.
(define (passing-spec-text)
  (string-append
   "(use-modules (srfi srfi-64) (json))\n"
   "(test-begin " Q "capture-fixture" Q ")\n"
   "(test-assert " Q "ok" Q " #t)\n"
   "(test-end " Q "capture-fixture" Q ")\n"
   "(display (scm->json-string '((summary . ((total . 1) (passed . 1) (failed . 0)))"
   " (verdict . " Q "pass" Q ") (seed . " Q "capture" Q ") (failures . ()))))\n"
   "(newline)\n"))

;;;
;;; Workspace
;;;
(define %temp-dir (mkdtemp "/tmp/badb-geis-capture-XXXXXX"))
(define %geis-root (string-append %temp-dir "/geis"))
(define %gate-root (string-append %temp-dir "/gate"))
(mkdir %geis-root)
(define %saved-geis-path (getenv "BADB_GEIS_PATH"))
(define %saved-gate-root (getenv "BADB_GEIS_GATE_ROOT"))
(setenv "BADB_GEIS_PATH" %geis-root)
(setenv "BADB_GEIS_GATE_ROOT" %gate-root)

(spec-begin "geis-capture")

;;;
;;; 1. Pending prompt capture (/geis add side)
;;;
(test-group "pending-prompt"
  (test-assert "capture writes the pending store and returns true"
    (geis-capture-pending-prompt! "retrieval for the numpy docs"
                                  "rendered prompt text"))
  (test-assert "pending file exists under the gate root"
    (file-exists? (string-append %gate-root "/geis-pending.json")))
  (let ((pending (geis-take-pending-prompt)))
    (test-assert "take returns the captured fields"
      (and (pair? pending)
           (string= "retrieval for the numpy docs"
                    (assoc-ref pending 'request))
           (string= "rendered prompt text"
                    (assoc-ref pending 'rendered)))))
  (test-assert "take deletes the store (second take is #f)"
    (not (geis-take-pending-prompt)))
  (test-assert "capture with no gate root degrades gracefully"
    (begin
      (unsetenv "BADB_GEIS_GATE_ROOT")
      (let ((ok? (geis-capture-pending-prompt! "x" "y")))
        (setenv "BADB_GEIS_GATE_ROOT" %gate-root)
        #t))))  ; must not throw; verdict value is unspecified

;;;
;;; 2. Bundle assembly (provenance capture)
;;;
(test-group "assemble"
  (let* ((fid "capture-assemble")
         (spec-src (string-append %temp-dir "/capture-fixture.scm"))
         (wf-src (string-append %temp-dir "/wf.sema"))
         (cassette-src (string-append %temp-dir "/cap-cassette.jsonl")))
    (write-file! spec-src (passing-spec-text))
    (write-file! wf-src ";; workflow content\n")
    (write-file! cassette-src "{\"llm\": \"traffic\"}\n")
    ;; lock the fixture spec so assemble can copy the LOCKED form
    (lock-geis spec-src fid)
    (let ((manifest (geis-bundle-assemble! fid
                                           #:workflow-path wf-src
                                           #:cassette-path cassette-src
                                           #:request "the original request"
                                           #:rendered-prompt "the rendered add prompt"
                                           #:synthesis-prompt "workflow: wf.sema"
                                           #:target-path "test/fixtures/lib/math-utils.scm")))
      (test-assert "assemble returns a manifest alist"
        (and (pair? manifest) (string= fid (manifest-field manifest 'feature-id))))
      (test-assert "bundle dir created under the search root"
        (file-exists? (string-append %geis-root "/" fid "/manifest.scm")))
      (test-assert "locked spec copied into the bundle"
        (string= (read-file-string (string-append %gate-root "/specs/" fid ".scm"))
                 (read-file-string (string-append %geis-root "/" fid "/spec.geis.scm"))))
      (test-assert "workflow captured"
        (file-exists? (string-append %geis-root "/" fid "/workflow.sema")))
      (test-assert "cassette captured"
        (file-exists? (string-append %geis-root "/" fid "/cassettes/cap-cassette.jsonl")))
      (test-assert "request prompt captured"
        (string= "the original request"
                 (read-file-string (string-append %geis-root "/" fid "/prompts/request.md"))))
      (test-assert "rendered add prompt captured"
        (string= "the rendered add prompt"
                 (read-file-string (string-append %geis-root "/" fid "/prompts/geis-add.md"))))
      (test-assert "synthesis invocation record captured"
        (string= "workflow: wf.sema"
                 (read-file-string (string-append %geis-root "/" fid "/prompts/synthesis.md"))))
      (test-assert "manifest records workflow + prompts + cassettes + provenance"
        (and (string= "workflow.sema" (manifest-field manifest 'workflow))
             (pair? (manifest-field manifest 'prompts))
             (string= "cassettes" (manifest-field manifest 'cassettes))
             (pair? (manifest-field manifest 'provenance))))
      (test-assert "manifest records the synthesis target path"
        (string= "test/fixtures/lib/math-utils.scm"
                 (assoc-ref (manifest-field manifest 'provenance) 'target-path)))
      (test-assert "refreshed manifest validates"
        (null? (geis-manifest-valid? (string-append %geis-root "/" fid)))))))

;;;
;;; 3. Bundle replay (full-stack cassette replay + plugin hash comparison)
;;;
(test-group "replay"
  ;; Cassette replay is keyed by the recorded request, which embeds the
  ;; feature-id and spec path, so the replay must run under the recorded
  ;; identity: the pilot bundle's fid, the repo gate root, and the repo
  ;; fixture target.  The temp geis root holds a copy of the pilot bundle
  ;; augmented with plugin.scm (the repaired content) so the plugin-match
  ;; comparison is meaningful.
  (let* ((fid "math-utils-synthesized")
         (mu-path "test/fixtures/lib/math-utils.scm")
         (original-mu (read-file-string mu-path))
         (tmp-geis %geis-root)
         (bundle-dir (string-append tmp-geis "/" fid))
         (repo-pilot "geis/runtimes-synthesis"))
    (mkdir bundle-dir)
    (system* "cp" (string-append repo-pilot "/workflow.sema")
             (string-append bundle-dir "/workflow.sema"))
    (mkdir (string-append bundle-dir "/cassettes"))
    (system* "cp" (string-append repo-pilot "/cassettes/synthesis-fixture.jsonl")
             (string-append bundle-dir "/cassettes/"))
    ;; the pinned plugin: the content the cassette's synthesis records
    ;; (the honest "what synthesis produced" form)
    (write-file! (string-append bundle-dir "/plugin.scm")
                 (cassette-write-content
                  (string-append repo-pilot "/cassettes/synthesis-fixture.jsonl")))
    ;; ensure the contract is locked exactly as recorded: the cassette's
    ;; interactions embed the locked spec CONTENT (the spec_runner results),
    ;; so lock the pilot's fixture spec, not the full suite spec
    (lock-geis (string-append repo-pilot "/fixtures/spec-pass.scm") fid)
    ;; manifest declaring workflow + cassettes + provenance target
    (call-with-output-file (string-append bundle-dir "/manifest.scm")
      (lambda (port)
        (write (list (cons 'bundle-version 1)
                     (cons 'feature-id fid)
                     (cons 'version "1.0.0")
                     (cons 'spec "spec.geis.scm")
                     (cons 'workflow "workflow.sema")
                     (cons 'cassettes "cassettes")
                     (cons 'provenance (list (cons 'origin "agent-synthesized")
                                              (cons 'target-path mu-path))))
               port)))
    ;; switch the search root to the temp copy; the gate root stays the
    ;; repo default so the recorded request matches
    (setenv "BADB_GEIS_PATH" tmp-geis)
    ;; the broken target: cassette replay must repair it deterministically;
    ;; the mutation and the restore are both inside the dynamic-wind so any
    ;; error still restores the fixture
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (write-file! mu-path
          (string-append "(define-module (test fixtures lib math-utils)\n"
                         "  #:export (clamp double))\n\n"
                         "(define (clamp lo hi x)\n  (min hi (max lo x)))\n\n"
                         "(define (double x)\n  (+ x 1))\n"))
        (let* ((report (geis-bundle-replay fid))
               (verdict (assoc-ref report 'workflow-verdict))
               (match (assoc-ref report 'plugin-match)))
          (test-assert "replay report names the feature"
            (string= fid (assoc-ref report 'feature-id)))
          (test-equal "cassette replay verdict is pass"
            "pass" verdict)
          (test-assert "replayed plugin matches the pinned plugin content"
            (eq? #t match))
          (test-assert "replayed hash is 64-hex"
            (and (string? (assoc-ref report 'replayed-hash))
                 (= 64 (string-length (assoc-ref report 'replayed-hash)))))))
      (lambda ()
        (write-file! mu-path original-mu)
        (if %saved-geis-path
            (setenv "BADB_GEIS_PATH" %saved-geis-path)
            (unsetenv "BADB_GEIS_PATH"))))))

;;;
;;; Teardown
;;;
(if %saved-geis-path
    (setenv "BADB_GEIS_PATH" %saved-geis-path)
    (unsetenv "BADB_GEIS_PATH"))
(if %saved-gate-root
    (setenv "BADB_GEIS_GATE_ROOT" %saved-gate-root)
    (unsetenv "BADB_GEIS_GATE_ROOT"))
(system* "rm" "-rf" %temp-dir)

(spec-end)
(spec-exit)
