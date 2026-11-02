;;; test/adversarial-capabilities.scm — Empirical Challenger Stress Harness for Capabilities Subsystem
;;;
;;; Adversarially challenge and stress-test:
;;; 1. JSON-RPC Protocol Framing & Transport Resilience ((badb kernel ipc) & (geasa capabilities-mcp-server plugin))
;;; 2. Filetree MCP Security, Limits & Traversal ((badb capabilities filetree-mcp))
;;; 3. History Tools Summarization & Context Compactor Integration ((geasa capabilities-history-tools plugin))
;;; 4. Record Type Descriptors

(use-modules (srfi srfi-64)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-13)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 ftw)
             (json)
             ;; Canonical Capability Subsystem modules
             (badb kernel ipc)
             (geasa capabilities-mcp plugin)
             (geasa capabilities-mcp-server plugin)
             (badb capabilities filetree-mcp)
             (geasa capabilities-history-tools plugin)
             (geasa runtimes-context plugin))

(define (non-blank-lines str)
  (filter (lambda (s) (not (string-null? (string-trim-both s))))
          (string-split str #\newline)))

(test-begin "adversarial-capabilities")

;;; =========================================================================
;;; 1. JSON-RPC PROTOCOL FRAMING & MCP SERVER/CLIENT RESILIENCE
;;; =========================================================================
(test-group "mcp-jsonrpc-framing-adversarial"

  ;; --- 1.1 Truncated / malformed JSON lines skipped, valid request handled ---
  (test-assert "malformed-and-truncated-json-skipped-cleanly"
    (let ((out (open-output-string))
          (srv (make-mcp-server "test" "1.0" #() '())))
      (parameterize ((current-input-port
                       (open-input-string
                         (string-append
                           "{\"jsonrpc\": \"2.0\", \"id\": 1\n" ; truncated
                           "NOT A JSON STRING AT ALL {{{}}\n"  ; garbage
                           "   \n"                             ; whitespace
                           "42\n"                              ; number not object
                           "[1, 2, 3]\n"                       ; array not object
                           "\"just a string\"\n"               ; string not object
                           "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}\n")))
                     (current-output-port out))
        (mcp-serve-stdio srv))
      (let* ((lines (non-blank-lines (get-output-string out)))
             (res (and (= (length lines) 1) (json-string->scm (car lines)))))
        (and res
             (equal? (assoc-ref res "id") 2)
             (equal? (assoc-ref (assoc-ref res "result") "status") "ok")))))

  ;; --- 1.2 Non-integer Request IDs (strings, large numbers, floats) ---
  (test-assert "non-integer-request-ids-preserved-accurately"
    (let ((out (open-output-string))
          (srv (make-mcp-server "test" "1.0" #() '())))
      (parameterize ((current-input-port
                       (open-input-string
                         (string-append
                           "{\"jsonrpc\":\"2.0\",\"id\":\"req-uuid-abc-123\",\"method\":\"ping\"}\n"
                           "{\"jsonrpc\":\"2.0\",\"id\":999999999999999999,\"method\":\"ping\"}\n"
                           "{\"jsonrpc\":\"2.0\",\"id\":42.5,\"method\":\"ping\"}\n")))
                     (current-output-port out))
        (mcp-serve-stdio srv))
      (let* ((lines (non-blank-lines (get-output-string out)))
             (r1 (json-string->scm (list-ref lines 0)))
             (r2 (json-string->scm (list-ref lines 1)))
             (r3 (json-string->scm (list-ref lines 2))))
        (and (equal? (assoc-ref r1 "id") "req-uuid-abc-123")
             (equal? (assoc-ref r2 "id") 999999999999999999)
             (equal? (assoc-ref r3 "id") 42.5)))))

  ;; --- 1.3 Notifications (no ID) never produce responses ---
  (test-assert "notifications-produce-zero-responses-even-if-unknown-or-error"
    (let ((out (open-output-string))
          (srv (make-mcp-server "test" "1.0" #() '())))
      (parameterize ((current-input-port
                       (open-input-string
                         (string-append
                           "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n"
                           "{\"jsonrpc\":\"2.0\",\"method\":\"unknown/notification\"}\n"
                           "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"nonexistent\"}}\n"
                           "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"ping\"}\n")))
                     (current-output-port out))
        (mcp-serve-stdio srv))
      (let ((lines (non-blank-lines (get-output-string out))))
        (and (= (length lines) 1)
             (let ((res (json-string->scm (car lines))))
               (equal? (assoc-ref res "id") 99))))))

  ;; --- 1.4 Unknown method yields code -32601 ---
  (test-assert "unknown-method-returns-32601-error"
    (let ((out (open-output-string))
          (srv (make-mcp-server "test" "1.0" #() '())))
      (parameterize ((current-input-port
                       (open-input-string
                         "{\"jsonrpc\":\"2.0\",\"id\":101,\"method\":\"bad/method/xyz\"}\n"))
                     (current-output-port out))
        (mcp-serve-stdio srv))
      (let* ((lines (non-blank-lines (get-output-string out)))
             (res (json-string->scm (car lines)))
             (err (assoc-ref res "error")))
        (and (equal? (assoc-ref res "id") 101)
             err
             (= (assoc-ref err "code") -32601)
             (string-contains (assoc-ref err "message") "unknown method")))))

  ;; --- 1.5 Unknown tool in tools/call yields code -32602 ---
  (test-assert "unknown-tool-returns-32602-error"
    (let ((out (open-output-string))
          (srv (make-mcp-server "test" "1.0" #() '())))
      (parameterize ((current-input-port
                       (open-input-string
                         "{\"jsonrpc\":\"2.0\",\"id\":102,\"method\":\"tools/call\",\"params\":{\"name\":\"phantom_tool\"}}\n"))
                     (current-output-port out))
        (mcp-serve-stdio srv))
      (let* ((lines (non-blank-lines (get-output-string out)))
             (res (json-string->scm (car lines)))
             (err (assoc-ref res "error")))
        (and (equal? (assoc-ref res "id") 102)
             err
             (= (assoc-ref err "code") -32602)
             (string-contains (assoc-ref err "message") "unknown tool: phantom_tool")))))

  ;; --- 1.6 Malformed params (number, string, null, missing) in tools/call ---
  (test-assert "malformed-or-missing-params-handled-without-crash"
    (let ((out (open-output-string))
          (srv (make-mcp-server "test" "1.0" #() '())))
      (parameterize ((current-input-port
                       (open-input-string
                         (string-append
                           "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":123}\n"
                           "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\",\"params\":\"str\"}\n"
                           "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\"}\n"
                           "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{}}\n"
                           "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"ping\"}\n")))
                     (current-output-port out))
        (mcp-serve-stdio srv))
      (let* ((lines (non-blank-lines (get-output-string out)))
             (objs (map json-string->scm lines)))
        (and (= (length objs) 5)
             ;; first 4 return error -32602
             (every (lambda (o)
                      (and (assoc-ref o "error")
                           (= (assoc-ref (assoc-ref o "error") "code") -32602)))
                    (take objs 4))
             ;; 5th is ping success
             (equal? (assoc-ref (list-ref objs 4) "id") 5)
             (equal? (assoc-ref (assoc-ref (list-ref objs 4) "result") "status") "ok")))))

  ;; --- 1.7 Crashing tool handler caught and reported cleanly ---
  (test-assert "crashing-tool-handler-caught-and-server-survives"
    (let* ((out (open-output-string))
           (srv (make-mcp-server "test" "1.0"
                                 #(((name . "explode")))
                                 `(("explode" . ,(lambda (args)
                                                   (scm-error 'fatal-test-error "explode"
                                                              "deliberate explosion" '() #f)))))))
      (parameterize ((current-input-port
                       (open-input-string
                         (string-append
                           "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"tools/call\",\"params\":{\"name\":\"explode\",\"arguments\":{}}}\n"
                           "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"ping\"}\n")))
                     (current-output-port out))
        (mcp-serve-stdio srv))
      (let* ((lines (non-blank-lines (get-output-string out)))
             (r1 (json-string->scm (list-ref lines 0)))
             (r2 (json-string->scm (list-ref lines 1)))
             (content (assoc-ref (assoc-ref r1 "result") "content")))
        (and (= (length lines) 2)
             (vector? content)
             (string-contains (assoc-ref (vector-ref content 0) "text") "tool error: fatal-test-error")
             (equal? (assoc-ref r2 "id") 11)
             (equal? (assoc-ref (assoc-ref r2 "result") "status") "ok")))))

  ;; --- 1.8 Immediate and truncated EOF handling on stdio ---
  (test-assert "immediate-and-truncated-eof-exits-cleanly"
    (let ((res-empty (parameterize ((current-input-port (open-input-string ""))
                                    (current-output-port (open-output-string)))
                       (mcp-serve-stdio (make-mcp-server "test" "1.0" #() '()))))
          (res-trunc (parameterize ((current-input-port (open-input-string "{\"jsonrpc\":\"2.0\",\"id\":1"))
                                    (current-output-port (open-output-string)))
                       (mcp-serve-stdio (make-mcp-server "test" "1.0" #() '())))))
      (and (eq? res-empty #t)
           (eq? res-trunc #t))))

  ;; --- 1.9 Broken pipe / closed port write resilience ---
  (test-assert "ipc-write-response-on-closed-port-returns-false-cleanly"
    (let ((p (open-output-string)))
      (close-port p)
      (eq? (ipc-write-response! '((jsonrpc . "2.0") (id . 1) (result . ((status . "ok")))) p)
           #f)))

  ;; --- 1.10 Channel and Client close idempotency ---
  (test-assert "channel-and-client-close-is-strictly-idempotent"
    (let* ((chan (make-ipc-channel (open-input-string "") (open-output-string)))
           (cli (make-mcp-client (open-input-string "") (open-output-string))))
      (and (eq? (ipc-channel-close! chan) #t)
           (begin (ipc-channel-close! chan) (ipc-channel-close! chan) #t)
           (ipc-channel-closed? chan)
           (begin (mcp-close-client cli) (mcp-close-client cli) (mcp-close-client cli) #t)
           (mcp-client-closed? cli))))

  ;; --- 1.11 Client ipc-call error and EOF handling ---
  (test-assert "ipc-call-and-mcp-call-error-paths"
    (let* ((in-eof (open-input-string ""))
           (out1 (open-output-string))
           (chan-eof (make-ipc-channel in-eof out1))
           (in-mismatch (open-input-string "{\"jsonrpc\":\"2.0\",\"id\":999,\"result\":{}}\n"))
           (out2 (open-output-string))
           (chan-mismatch (make-ipc-channel in-mismatch out2))
           (in-err (open-input-string "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32600,\"message\":\"bad req\"}}\n"))
           (out3 (open-output-string))
           (chan-err (make-ipc-channel in-err out3))
           (in-mcperr (open-input-string "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32603,\"message\":\"internal err\"}}\n"))
           (out4 (open-output-string))
           (cli-err (make-mcp-client in-mcperr out4)))
      (and
        ;; Unexpected EOF
        (catch #t
          (lambda () (ipc-call chan-eof "ping") #f)
          (lambda (k . args) (string-contains (cadr args) "unexpected EOF")))
        ;; ID Mismatch
        (catch #t
          (lambda () (ipc-call chan-mismatch "ping") #f)
          (lambda (k . args) (string-contains (cadr args) "response ID mismatch")))
        ;; JSON-RPC error via ipc-call
        (catch 'ipc-error
          (lambda () (ipc-call chan-err "ping") #f)
          (lambda (k sub msg args data)
            (equal? (caar data) '("message" . "bad req"))))
        ;; JSON-RPC error via mcp-call
        (catch 'mcp-error
          (lambda () (mcp-call cli-err "ping") #f)
          (lambda (k sub msg args data)
            (= (car args) -32603))))))
)

;;; =========================================================================
;;; 2. FILETREE MCP SECURITY, BOUNDARIES, CAPS & RECURSION
;;; =========================================================================
(test-group "filetree-mcp-adversarial"

  ;; --- 2.1 Workspace Escape: Relative and Absolute Path Traversal ---
  (test-assert "path-traversal-escapes-rejected"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-ft-XXXXXX"))
           (ws (string-append tmp "/workspace")))
      (mkdir ws)
      (let ((rel-1 (filetree-read ws '(( "path" . "../../../../etc/passwd"))))
            (rel-2 (filetree-read ws '(( "path" . "../../secret.txt"))))
            (abs-1 (filetree-read ws '(( "path" . "/etc/passwd"))))
            (out-rel (filetree-outline ws '(( "path" . "../../../../etc/hosts"))))
            (out-abs (filetree-outline ws '(( "path" . "/bin/sh")))))
        (rmdir ws)
        (rmdir tmp)
        (and (or (string-contains rel-1 "path escapes workspace root")
                 (string-contains rel-1 "no such path"))
             (or (string-contains rel-2 "path escapes workspace root")
                 (string-contains rel-2 "no such path"))
             (string-contains abs-1 "path escapes workspace root")
             (or (string-contains out-rel "path escapes workspace root")
                 (string-contains out-rel "no such path"))
             (string-contains out-abs "path escapes workspace root")))))

  ;; --- 2.2 Symlink Workspace Escape Prevention ---
  (test-assert "symlink-pointing-outside-workspace-rejected"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-ft-XXXXXX"))
           (secret (string-append tmp "/outside_secret.txt"))
           (ws (string-append tmp "/workspace"))
           (sym (string-append ws "/leak_link")))
      (mkdir ws)
      (call-with-output-file secret (lambda (p) (display "CONFIDENTIAL" p)))
      (symlink secret sym)
      (let ((r-res (filetree-read ws '(( "path" . "leak_link"))))
            (o-res (filetree-outline ws '(( "path" . "leak_link")))))
        (delete-file sym)
        (delete-file secret)
        (rmdir ws)
        (rmdir tmp)
        (and (string-contains r-res "path escapes workspace root")
             (string-contains o-res "path escapes workspace root")))))

  ;; --- 2.3 Symlink Pointing Inside Workspace Allowed ---
  (test-assert "internal-symlink-allowed-and-resolved"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-ft-XXXXXX"))
           (real (string-append tmp "/real.txt"))
           (sym (string-append tmp "/sym.txt")))
      (call-with-output-file real (lambda (p) (display "REAL DATA" p)))
      (symlink real sym)
      (let ((r-res (filetree-read tmp '(( "path" . "sym.txt")))))
        (delete-file sym)
        (delete-file real)
        (rmdir tmp)
        (string=? r-res "REAL DATA"))))

  ;; --- 2.4 Circular Directory Symlink Does Not Cause Infinite Loop ---
  (test-assert "circular-symlink-does-not-loop-infinitely"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-ft-XXXXXX"))
           (circ (string-append tmp "/loop_dir")))
      (symlink tmp circ)
      (call-with-output-file (string-append tmp "/file.txt") (lambda (p) (display "f" p)))
      (let ((t (filetree-tree tmp '(( "depth" . 10))))
            (f (filetree-find tmp '(( "pattern" . "file")))))
        (delete-file circ)
        (delete-file (string-append tmp "/file.txt"))
        (rmdir tmp)
        (and (string-contains t "file.txt")
             (string-contains t "loop_dir")
             (string-contains f "1 match:")
             (string-contains f "file.txt")))))

  ;; --- 2.5 Outline and Read On Directories Rejected ---
  (test-assert "outline-and-read-reject-directories"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-ft-XXXXXX"))
           (subdir (string-append tmp "/sub")))
      (mkdir subdir)
      (let ((r-root (filetree-read tmp '(( "path" . "."))))
            (r-sub  (filetree-read tmp '(( "path" . "sub"))))
            (o-root (filetree-outline tmp '(( "path" . "."))))
            (o-sub  (filetree-outline tmp '(( "path" . "sub")))))
        (rmdir subdir)
        (rmdir tmp)
        (and (string-contains r-root "read: path is a directory")
             (string-contains r-sub "read: path is a directory")
             (string-contains o-root "outline: path is a directory")
             (string-contains o-sub "outline: path is a directory")))))

  ;; --- 2.6 Non-Existent Paths on Outline and Read ---
  (test-assert "outline-and-read-reject-non-existent-paths"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-ft-XXXXXX"))
           (r-miss (filetree-read tmp '(( "path" . "does_not_exist.scm"))))
           (o-miss (filetree-outline tmp '(( "path" . "does_not_exist.scm")))))
      (rmdir tmp)
      (and (string-contains r-miss "no such path")
           (string-contains o-miss "no such path"))))

  ;; --- 2.7 Unreadable Subdirectory In Workspace Handled Gracefully ---
  (test-assert "unreadable-directory-handled-without-crash"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-ft-XXXXXX"))
           (unreadable (string-append tmp "/unreadable_sub")))
      (mkdir unreadable)
      (chmod unreadable #o000)
      (let ((t (filetree-tree tmp '(( "depth" . 3))))
            (f (filetree-find tmp '(( "pattern" . "anything")))))
        (chmod unreadable #o755)
        (rmdir unreadable)
        (rmdir tmp)
        (and (string-contains t "unreadable_sub/")
             (string-contains t "1 directories, 0 files")
             (string-contains f "0 matches:")))))

  ;; --- 2.8 Deep Directory Nesting (20 levels) ---
  (test-assert "deep-directory-nesting-traversal"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-ft-XXXXXX"))
           (curr tmp))
      (do ((i 0 (+ i 1)))
          ((= i 20))
        (let ((next (format #f "~a/lvl~a" curr i)))
          (mkdir next)
          (set! curr next)))
      (call-with-output-file (format #f "~a/bottom.txt" curr) (lambda (p) (display "bottom" p)))
      (let ((t-shallow (filetree-tree tmp '(( "depth" . 2))))
            (t-deep    (filetree-tree tmp '(( "depth" . 25))))
            (f-find    (filetree-find tmp '(( "pattern" . "bottom.txt")))))
        ;; Cleanup
        (delete-file (format #f "~a/bottom.txt" curr))
        (let clean ((d curr))
          (unless (string=? d tmp)
            (rmdir d)
            (clean (dirname d))))
        (rmdir tmp)
        (and (string-contains t-shallow "(truncated)")
             (string-contains t-deep "20 directories, 1 files")
             (string-contains f-find "1 match:")
             (string-contains f-find "bottom.txt")))))

  ;; --- 2.9 Entry Caps: Tree (500) and Find (100) ---
  (test-assert "entry-caps-enforced-for-tree-and-find"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-ft-XXXXXX")))
      (do ((i 0 (+ i 1)))
          ((= i 520))
        (call-with-output-file (format #f "~a/sample_~a.txt" tmp i)
          (lambda (p) (display "data" p))))
      (let ((t (filetree-tree tmp '(( "depth" . 3))))
            (f (filetree-find tmp '(( "pattern" . "sample_")))))
        (do ((i 0 (+ i 1)))
            ((= i 520))
          (delete-file (format #f "~a/sample_~a.txt" tmp i)))
        (rmdir tmp)
        (and (string-contains t "500 files (truncated)")
             (string-contains f "520 matches:")
             (string-contains f "(showing first 100)")))))

  ;; --- 2.10 Read Character Limits and Truncation Bounding ---
  (test-assert "read-character-limits-and-truncation-bounds"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-ft-XXXXXX"))
           (f (string-append tmp "/bounded.txt")))
      (call-with-output-file f
        (lambda (p) (display (make-string 300 #\k) p)))
      (let ((r-0    (filetree-read tmp '(( "path" . "bounded.txt") ( "max-chars" . 0))))
            (r-50   (filetree-read tmp '(( "path" . "bounded.txt") ( "max-chars" . 50))))
            (r-neg  (filetree-read tmp '(( "path" . "bounded.txt") ( "max-chars" . -100))))
            (r-bad  (filetree-read tmp '(( "path" . "bounded.txt") ( "max-chars" . "invalid"))))
            (r-full (filetree-read tmp '(( "path" . "bounded.txt") ( "max-chars" . 1000)))))
        (delete-file f)
        (rmdir tmp)
        (and (string=? r-0 "\n...(truncated at 0 chars)")
             (= (string-length r-50) (+ 50 (string-length "\n...(truncated at 50 chars)")))
             (string=? r-neg "\n...(truncated at 0 chars)")
             (string-contains r-bad "not a number")
             (= (string-length r-full) 300)))))
)

;;; =========================================================================
;;; 3. HISTORY TOOLS & CONTEXT INTEGRATION STRESS
;;; =========================================================================
(test-group "history-tools-adversarial"

  ;; --- 3.1 Edge input types to summarize-turns ---
  (test-assert "summarize-turns-edge-types"
    (let ((s-nil (summarize-turns '()))
          (s-vec (summarize-turns #()))
          (s-false (summarize-turns #f)))
      (and (string-contains s-nil "Total turns: 0")
           (string-contains s-nil "Turns by role: (none)")
           (string-contains s-vec "Total turns: 0")
           (string-contains s-false "Total turns: 0"))))

  ;; --- 3.2 Non-string fields in turn records coerced cleanly ---
  (test-assert "turn-field-coerces-non-string-and-missing-safely"
    (let ((t1 '((role . 123) (content . #t)))
          (t2 '((role . system) (content . (a b c))))
          (t3 '()))
      (and (string=? (turn-field t1 'role) "123")
           (string=? (turn-field t1 'content) "#t")
           (string=? (turn-field t2 'role) "system")
           (string=? (turn-field t2 'content) "(a b c)")
           (string=? (turn-field t3 'role) "")
           (string=? (turn-field t3 'content) ""))))

  ;; --- 3.3 Large history turns summarization scalability (2,000 turns) ---
  (test-assert "summarize-turns-scales-to-2000-turns"
    (let* ((turns (do ((i 0 (+ i 1))
                       (acc '() (cons (list (cons 'role (if (even? i) "user" "assistant"))
                                            (cons 'content (format #f "turn ~a with topic_~a" i (modulo i 8))))
                                      acc)))
                      ((= i 2000) (reverse acc))))
           (t0 (get-internal-real-time))
           (summary (summarize-turns turns))
           (t1 (get-internal-real-time))
           (elapsed-ms (/ (* (- t1 t0) 1000.0) internal-time-units-per-second)))
      (and (< elapsed-ms 500.0) ; Must complete in < 500 ms
           (string-contains summary "Total turns: 2000")
           (string-contains summary "assistant=1000, user=1000")
           (string-contains summary "Topics:"))))

  ;; --- 3.4 Topic extraction with stopwords and tie-breaking ---
  (test-assert "top-topic-words-filtering-and-tiebreaking"
    (let* ((all-stopwords (list '((role . "user") (content . "the a an and or if then else of to in on"))))
           (s-stop (summarize-turns all-stopwords))
           (tied-words (list '((role . "user") (content . "zebra apple zebra apple banana banana orange orange"))))
           (s-tied (summarize-turns tied-words)))
      (and (string-contains s-stop "Topics: (none)")
           ;; apple, banana, orange, zebra all count = 2 -> alphabetical: apple banana orange zebra
           (string-contains s-tied "Topics: apple banana orange zebra"))))

  ;; --- 3.5 summarize-history-file error conditions ---
  (test-assert "summarize-history-file-error-paths"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-hist-XXXXXX"))
           (unreadable (string-append tmp "/unreadable.jsonl")))
      (call-with-output-file unreadable (lambda (p) (display "{\"role\":\"user\",\"content\":\"hi\"}\n" p)))
      (chmod unreadable #o000)
      (let ((res-miss (summarize-history-file "/non/existent/history.jsonl"))
            (res-dir  (summarize-history-file tmp))
            (res-unr  (summarize-history-file unreadable)))
        (chmod unreadable #o644)
        (delete-file unreadable)
        (rmdir tmp)
        (and (eq? res-miss #f)
             (eq? res-dir #f)
             (eq? res-unr #f)))))

  ;; --- 3.6 summarize-history-file skips corrupted JSONL lines ---
  (test-assert "summarize-history-file-filters-corrupted-jsonl"
    (let* ((tmp (mkdtemp "/tmp/badb-challenger-hist-XXXXXX"))
           (f (string-append tmp "/mixed.jsonl")))
      (call-with-output-file f
        (lambda (p)
          (display "{\"role\":\"user\",\"content\":\"alpha one\"}\n" p)
          (display "MALFORMED NON JSON LINE\n" p)
          (display "9999\n" p)
          (display "\n" p)
          (display "{\"role\":\"assistant\",\"content\":\"response alpha\"}\n" p)
          (display "{\"missing\":\"role and content\"}\n" p)
          (display "{\"role\":\"user\",\"content\":\"alpha two\"}\n" p)))
      (let ((summary (summarize-history-file f)))
        (delete-file f)
        (rmdir tmp)
        (and summary
             (string-contains summary "Total turns: 3")
             (string-contains summary "assistant=1, user=2")
             (string-contains summary "alpha one")
             (string-contains summary "alpha two")))))
)

;;; =========================================================================
;;; 4. RECORD TYPE DESCRIPTORS
;;; =========================================================================
(test-group "record-descriptors"

  ;; --- 4.1 Record Type Descriptors Defined & Predicates Functional ---
  (test-assert "record-type-descriptors-functional"
    (let* ((srv (make-mcp-server "srv" "1.0" #() '()))
           (cli (make-mcp-client (open-input-string "") (open-output-string)))
           (mgr (make-mcp-manager))
           (chan (make-mcp-channel (open-input-string "") (open-output-string)))
           (tool (make-mcp-tool "tool1" "desc" '((type . "object"))))
           (prompt (make-mcp-prompt "p1" "desc" '()))
           (res (make-mcp-resource "uri://test" "r1" "desc" "text/plain")))
      (and (mcp-server? srv)
           (mcp-client? cli)
           (mcp-manager? mgr)
           (mcp-channel? chan)
           (mcp-tool? tool)
           (mcp-prompt? prompt)
           (mcp-resource? res)
           (string=? (mcp-server-name srv) "srv")
           (string=? (mcp-tool-name tool) "tool1")
           (string=? (mcp-prompt-name prompt) "p1")
           (string=? (mcp-resource-uri res) "uri://test")))))

(test-end "adversarial-capabilities")
