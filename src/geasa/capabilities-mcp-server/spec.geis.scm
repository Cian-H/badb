;;; test-mcp-server.scm — Tests for (geasa capabilities-mcp-server plugin) and (badb capabilities filetree-mcp)
;;;
;;; Drives the stdio MCP core from string ports, asserting the
;;; initialize handshake, tools/list, tools/call dispatch, and the
;;; unknown-method error path, then exercises the filetree handler
;;; functions directly against a temp directory with known contents.

(define-module (test test-mcp-server)
  #:use-module (srfi srfi-1)
  #:use-module (badb spec)
  #:use-module (srfi srfi-64)
  #:use-module (json)
  #:use-module (ice-9 format)
  #:use-module (geasa capabilities-mcp-server plugin)
  #:use-module (badb capabilities filetree-mcp))

(spec-begin "capabilities-mcp-server")

;;;
;;; 1. Stdio driver: request lines in, parsed JSON-RPC out.
;;;

(define (serve-lines! server lines)
  "Feed LINES of JSON-RPC requests to SERVER's stdio loop, capturing
the output; return the responses parsed from JSON, in order."
  (let ((out (open-output-string)))
    (parameterize ((current-input-port
                     (open-input-string (string-join lines "\n")))
                   (current-output-port out))
      (mcp-serve-stdio server))
    (filter-map
      (lambda (line)
        (let ((trimmed (string-trim-both line)))
          (if (string-null? trimmed)
              #f
              (json-string->scm trimmed))))
      (string-split (get-output-string out) #\newline))))

(define (response-for responses id)
  "Return the response in RESPONSES carrying the given request ID."
  (find (lambda (r) (equal? (assoc-ref r "id") id)) responses))

(define (response-text response)
  "Return the text carried by a tools/call result RESPONSE."
  (assoc-ref (vector-ref (assoc-ref (assoc-ref response "result") "content") 0)
             "text"))

(define echo-server
  (let ((tools (list `((name . "echo")
                       (description . "echoes its argument")
                       (inputSchema . ((type . "object")))))))
    `(("name" . "echo-test")
      ("version" . "0.0.1")
      ("tools" . ,(list->vector tools))
      ("handlers"
        . (("echo" . ,(lambda (args)
                        (format #f "echo: ~a" (assoc-ref args "x")))))))))

;;;
;;; 2. Core protocol: handshake, ping, list, dispatch, errors.
;;;

(test-group "mcp-core-protocol"
  (let ((responses
          (serve-lines!
            echo-server
            (list
              "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}"
              "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
              "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}"
              "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/list\"}"
              "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\",\"arguments\":{\"x\":\"hi\"}}}"
              "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\",\"params\":{\"name\":\"nope\",\"arguments\":{}}}"
              "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"bogus/method\"}"))))
    (test-equal "notifications get no response" 6 (length responses))

    (let* ((init (response-for responses 1))
           (result (assoc-ref init "result"))
           (info (assoc-ref result "serverInfo")))
      (test-equal "initialize echoes the server name"
                  "echo-test"
                  (assoc-ref info "name"))
      (test-equal "initialize echoes the server version"
                  "0.0.1"
                  (assoc-ref info "version"))
      (test-equal "initialize pins the protocol version"
                  "2024-11-05"
                  (assoc-ref result "protocolVersion"))
      (test-assert "initialize advertises the tools capability"
        (assoc-ref (assoc-ref result "capabilities") "tools")))

    (test-equal "ping returns ok"
                "ok"
                (assoc-ref (assoc-ref (response-for responses 2) "result")
                           "status"))

    (let* ((listing (response-for responses 3))
           (tools (assoc-ref (assoc-ref listing "result") "tools")))
      (test-assert "tools/list returns a vector of tool defs" (vector? tools))
      (test-equal "tools/list carries the defined tools" 1 (vector-length tools))
      (test-equal "tool def carries its name"
                  "echo"
                  (assoc-ref (vector-ref tools 0) "name")))

    (test-equal "tools/call dispatches to the handler"
                "echo: hi"
                (response-text (response-for responses 4)))

    (test-equal "unknown tool is a JSON-RPC error"
                -32602
                (assoc-ref (assoc-ref (response-for responses 5) "error") "code"))
    (test-assert "unknown tool error names the tool"
      (string-contains
        (assoc-ref (assoc-ref (response-for responses 5) "error") "message")
        "nope"))

    (test-equal "unknown method is a JSON-RPC error"
                -32601
                (assoc-ref (assoc-ref (response-for responses 6) "error") "code"))))

;;;
;;; 3. Filetree fixtures: a temp directory with known contents.
;;;

(define %fixture-dir "/tmp/badb-test-filetree")

(define (write-file! path contents)
  "Write CONTENTS to PATH, creating it fresh."
  (call-with-output-file path (lambda (port) (display contents port))))

(define (make-fixture!)
  "Recreate the %fixture-dir tree of known contents: two top-level
files, a nested sub/deep directory, plus .git and hidden entries that
must never be listed."
  (system* "rm" "-rf" %fixture-dir)
  (mkdir %fixture-dir)
  (write-file! (string-append %fixture-dir "/alpha.scm")
    ";;; fixture\n(define %fixture-constant 42)\n\n(define (fixture-handler args)\n  \"doc\"\n  (assoc-ref args \"x\"))\n")
  (write-file! (string-append %fixture-dir "/notes.txt")
    "first note line\nsecond note line\n")
  (mkdir (string-append %fixture-dir "/sub"))
  (mkdir (string-append %fixture-dir "/sub/deep"))
  (write-file! (string-append %fixture-dir "/sub/deep/buried.scm")
    "(define buried 1)\n")
  (mkdir (string-append %fixture-dir "/.git"))
  (write-file! (string-append %fixture-dir "/.git/config") "[core]\n")
  (write-file! (string-append %fixture-dir "/.hidden") "hidden\n"))

(make-fixture!)

;;;
;;; 4. tree: indented project tree.
;;;

(test-group "filetree-tree"
  (let ((out (filetree-tree %fixture-dir '())))
    (test-assert "tree lists top-level files" (string-contains out "alpha.scm"))
    (test-assert "tree marks directories with a slash suffix"
                 (string-contains out "sub/"))
    (test-assert "tree reaches the default depth of 3"
                 (string-contains out "buried.scm"))
    (test-assert "tree skips .git" (not (string-contains out ".git")))
    (test-assert "tree skips hidden entries" (not (string-contains out ".hidden")))
    (test-assert "tree reports file and directory counts"
                 (string-contains out "2 directories, 3 files")))

  (let* ((out (filetree-tree %fixture-dir '(("depth" . 1))))
         (summary (last (string-split out #\newline))))
    (test-assert "depth 1 lists top-level entries"
                 (string-contains out "alpha.scm"))
    (test-assert "depth 1 hides deeper entries"
                 (not (string-contains out "buried.scm")))
    (test-equal "depth 1 counts only listed entries and flags truncation"
                "1 directories, 2 files (truncated)"
                summary)))

;;;
;;; 5. find: case-insensitive name search.
;;;

(test-group "filetree-find"
  (let ((out (filetree-find %fixture-dir '(("pattern" . "ALPHA")))))
    (test-assert "find matches case-insensitively"
                 (string-contains out "alpha.scm"))
    (test-assert "find reports a single match in the singular"
                 (string-contains out "1 match:")))

  (let ((out (filetree-find %fixture-dir '(("pattern" . "buried")))))
    (test-assert "find returns relative paths"
                 (string-contains out "sub/deep/buried.scm")))

  (let ((out (filetree-find %fixture-dir '(("pattern" . "s")))))
    (test-assert "find reports plural matches" (string-contains out "4 matches:"))
    (test-assert "find never returns .git contents"
                 (not (string-contains out "config")))
    (test-assert "find never returns hidden entries"
                 (not (string-contains out ".hidden"))))

  (test-assert "find without a pattern reports the missing argument"
    (string-contains (filetree-find %fixture-dir '()) "missing argument: pattern")))

(test-group "filetree-find-cap"
  (let ((dir "/tmp/badb-test-filetree-cap"))
    (system* "rm" "-rf" dir)
    (mkdir dir)
    (let loop ((i 0))
      (when (< i 105)
        (write-file! (string-append dir "/hit-" (number->string i) ".txt") "x\n")
        (loop (+ i 1))))
    (let* ((out (filetree-find dir '(("pattern" . "hit"))))
           (hits (filter (lambda (l) (string-contains l "hit-"))
                         (string-split out #\newline))))
      (test-assert "find reports the full count past the cap"
                   (string-contains out "105 matches:"))
      (test-equal "find caps listed matches at 100" 100 (length hits))
      (test-assert "find notes the truncation"
                   (string-contains out "showing first 100")))
    (system* "rm" "-rf" dir)))

;;;
;;; 6. outline: per-file structure.
;;;

(test-group "filetree-outline"
  (let ((out (filetree-outline %fixture-dir '(("path" . "alpha.scm")))))
    (test-assert "scheme outline lists constant defines with line numbers"
                 (string-contains out "2: define %fixture-constant"))
    (test-assert "scheme outline lists procedure defines"
                 (string-contains out "4: define fixture-handler")))

  (let ((out (filetree-outline %fixture-dir '(("path" . "notes.txt")))))
    (test-assert "plain outline reports the line count"
                 (string-contains out "2 lines"))
    (test-assert "plain outline reports the first line"
                 (string-contains out "first note line")))

  (test-assert "outline rejects directories"
    (string-contains (filetree-outline %fixture-dir '(("path" . "sub")))
                     "directory"))
  (test-assert "outline rejects paths escaping the workspace root"
    (string-contains (filetree-outline %fixture-dir '(("path" . "../../etc")))
                     "escapes workspace root"))
  (test-assert "outline rejects missing paths"
    (string-contains (filetree-outline %fixture-dir '(("path" . "no-such.scm")))
                     "no such path"))
  (test-assert "outline without a path reports the missing argument"
    (string-contains (filetree-outline %fixture-dir '()) "missing argument: path")))

;;;
;;; 7. read: bounded file contents.
;;;

(test-group "filetree-read"
  (let ((out (filetree-read %fixture-dir '(("path" . "alpha.scm")))))
    (test-assert "read returns the file contents"
                 (string-contains out "fixture-handler"))
    (test-assert "untruncated read carries no truncation note"
                 (not (string-contains out "truncated"))))

  (let ((out (filetree-read %fixture-dir
                            '(("path" . "alpha.scm") ("max-chars" . 10)))))
    (test-assert "bounded read truncates at max-chars"
                 (string-prefix? ";;; fixtur" out))
    (test-assert "bounded read notes the truncation point"
                 (string-contains out "truncated at 10 chars")))

  (test-assert "read rejects paths escaping the workspace root"
    (string-contains (filetree-read %fixture-dir
                                    '(("path" . "../../../etc/passwd")))
                     "escapes workspace root"))
  (test-assert "read rejects directories"
    (string-contains (filetree-read %fixture-dir '(("path" . "sub")))
                     "directory")))

;;;
;;; 8. Server assembly: the filetree server over stdio.
;;;

(test-group "filetree-server"
  (let ((server (make-filetree-server %fixture-dir)))
    (test-equal "server def carries the filetree name"
                "filetree"
                (assoc-ref server "name"))
    (test-equal "server def carries the module version"
                %filetree-version
                (assoc-ref server "version"))

    (let ((responses
            (serve-lines!
              server
              (list
                "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}"
                "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"
                "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"tree\",\"arguments\":{}}}"
                "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"read\",\"arguments\":{\"path\":\"notes.txt\"}}}"))))
      (test-equal "initialize advertises the filetree name"
                  "filetree"
                  (assoc-ref
                    (assoc-ref (assoc-ref (response-for responses 1) "result")
                               "serverInfo")
                    "name"))
      (test-equal "tools/list serves the four filetree tools"
                  4
                  (vector-length
                    (assoc-ref (assoc-ref (response-for responses 2) "result")
                               "tools")))
      (test-assert "tree tool call returns the fixture tree"
        (string-contains (response-text (response-for responses 3)) "alpha.scm"))
      (test-assert "read tool call returns the file contents"
        (string-contains (response-text (response-for responses 4))
                         "first note line")))))

(spec-end)
(spec-exit)

(system* "rm" "-rf" %fixture-dir)
