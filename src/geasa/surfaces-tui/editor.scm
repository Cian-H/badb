;;; (geasa surfaces-tui editor) — Text Wrapping, Slash Commands & Line Editor
;;;
;;; Extracted from surfaces-tui plugin.scm. Provides text wrapping, slash
;;; command registry/parsing, cursor editing operations, and input history.

(define-module (geasa surfaces-tui editor)
               #:use-module (geasa foundation-util plugin)
               #:use-module (json)
               #:use-module (ice-9 format)
               #:use-module (ice-9 rdelim)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:use-module (srfi srfi-19)
               #:export (wrap-line
                          wrap-text
                          %slash-commands
                          selector-active?
                          matching-slash-commands
                          %selector-max-visible
                          selector-visible-slice
                          parse-command
                          clamp-cursor
                          insert-at
                          backspace-at
                          delete-at
                          input-visible-window
                          %max-input-rows
                          input-wrapped-rows
                          %history-max-entries
                          <input-history>
                          input-history?
                          history-cap-entries
                          make-history
                          history-entries
                          history-recall-position
                          history-length
                          history-add
                          history-recall
                          history-reset-nav
                          history-entry->jsonl-line
                          jsonl-line->history-entry
                          default-history-file-path
                          load-history-file
                          append-history-file-entry!))

;;;
;;; Text Formatting & Word Wrapping Helpers
;;;

(define (wrap-line line max-width)
        "Wrap a single LINE into a list of strings with length <= MAX-WIDTH,
breaking on whitespace when possible."
        (let ((line-str (cond
                          ((string? line) line)
                          ((not line) "")
                          (else (format #f "~a" line)))))
          (if (or (<= (string-length line-str) max-width) (<= max-width 0))
            (list line-str)
            (let ((words (string-split line-str #\space)))
              (let loop ((words words) (current "") (acc '()))
                        (if (null? words)
                          (reverse (if (string-null? current) acc (cons current acc)))
                          (let* ((w (car words))
                                 (candidate (if (string-null? current)
                                              w
                                              (string-append current " " w))))
                            (cond
                              ((<= (string-length candidate) max-width)
                                (loop (cdr words) candidate acc))
                              ((string-null? current)
                                ;; Single word longer than max-width: hard-break it
                                (loop (cons (substring w max-width) (cdr words))
                                      ""
                                      (cons (substring w 0 max-width) acc)))
                              (else
                                (loop words "" (cons current acc)))))))))))

(define (wrap-text text max-width)
        "Wrap multi-line TEXT into lines that each fit within MAX-WIDTH."
        (let* ((safe-str (cond
                           ((string? text) text)
                           ((not text) "")
                           (else (format #f "~a" text))))
               (raw-lines (string-split safe-str #\newline)))
          (append-map (lambda (line) (wrap-line (if (string? line) line "") max-width)) raw-lines)))

;;;
;;; Slash Commands Registry & Parsing
;;;

(define %slash-commands
        '(("help" . "Show help manual & shortcuts")
          ("model" . "Switch or inspect active LLM model")
          ("models" . "List models available on the endpoint")
          ("endpoint" . "Switch or inspect endpoint URL")
          ("provider" . "Switch or inspect model provider preset (e.g. lmstudio)")
          ("mcp" . "Manage MCP servers (list, start, stop, add, remove, tools)")
          ("config" . "Inspect, reload, or save badb configuration")
          ("theme" . "Inspect or switch the active UI theme")
          ("guardrail" . "Inspect or set guardrail security policy level")
          ("compact" . "Compact conversation history context")
          ("resume" . "Resume a previous chat session from disk")
          ("checkpoint" . "Create a workspace and registry checkpoint")
          ("rollback" . "Revert to latest or specified checkpoint")
          ("fork" . "Create a conversation branch at the current or saved checkpoint")
          ("replay" . "Restore a saved conversation and workspace checkpoint")
          ("rules" . "Inspect discovered project rules & workspace tree")
          ("geis" . "Contract-gated dynamic harness synthesis (/geis add <desc>, model [name], list, verify, status)")
          ("context" . "View conversation token stats and metrics")
          ("ctx" . "Context index search and token savings stats")
          ("skills" . "List available agent skills")
          ("memories" . "Inspect persistent project memories")
          ("usage" . "Show per-session token usage report")
          ("features" . "List kernel registered features")
          ("journal" . "View checkpoint provenance audit log")
          ("test" . "Run bootstrap verification test suite")
          ("sh" . "Execute shell command (!<cmd>)")
          ("eval" . "Evaluate Scheme code in REPL (,<expr>)")
          ("steer" . "Queue steering text for the running agent")
          ("interrupt" . "Halt the running agent at its next boundary")
          ("clear" . "Clear chat viewport history")
          ("quit" . "Exit badb microharness")
          ("exit" . "Exit badb microharness")
          ("dump" . "Export the conversation to a text file [path]")))

(define (selector-active? input-buffer)
        "Return #t if the slash command selector palette should be visible."
        (and (string-prefix? "/" input-buffer)
             (not (string-contains input-buffer " "))))

(define (matching-slash-commands input-buffer)
        "Return list of (%slash-commands) items matching the current query in INPUT-BUFFER."
        (let* ((prefix (if (string-prefix? "/" input-buffer)
                         (substring input-buffer 1)
                         input-buffer))
               (clean-q (string-downcase (string-trim-both prefix))))
          (if (string-null? clean-q)
            %slash-commands
            (filter (lambda (item)
                      (string-contains (string-downcase (car item)) clean-q))
                    %slash-commands))))

(define %selector-max-visible 7)

(define* (selector-visible-slice matches #:optional (max-visible %selector-max-visible) (sel-index 0))
         "Return (values visible-items offset sel-pos above-count below-count):
the window of at most MAX-VISIBLE items of MATCHES positioned so SEL-INDEX
stays visible inside the slice. Returns offset, relative sel-pos within the slice,
and above/below scroll counts."
         (let* ((items (if (list? matches) matches '()))
                (count (length items))
                (cap (max 1 (or max-visible %selector-max-visible)))
                (sel (min (max 0 sel-index) (max 0 (- count 1)))))
           (if (<= count cap)
             (values items 0 sel 0 0)
             (let* ((start (min (max 0 (- (+ sel 1) cap)) (- count cap)))
                    (vis-items (take (drop items start) cap))
                    (offset start)
                    (sel-pos (- sel offset))
                    (above-count offset)
                    (below-count (- count offset cap)))
               (values vis-items offset sel-pos above-count below-count)))))

(define (parse-command input)
        "Parse user INPUT into (command . arg-string) or #f if not a slash command."
        (let ((trimmed (string-trim-both input)))
          (if (string-prefix? "/" trimmed)
            (let* ((without-slash (substring trimmed 1))
                   (space-pos (string-index without-slash #\space)))
              (if space-pos
                (cons (substring without-slash 0 space-pos)
                      (string-trim-both (substring without-slash (+ space-pos 1))))
                (cons without-slash "")))
            #f)))

;;;
;;; Input Buffer Cursor Editing Helpers
;;;

(define (clamp-cursor pos buffer)
        "Clamp cursor position POS into the valid range 0..(string-length BUFFER)."
        (max 0 (min pos (string-length buffer))))

(define (insert-at buffer pos str)
        "Insert STR into BUFFER at cursor position POS.
Return (values new-buffer new-pos) with the cursor advanced past STR."
        (let* ((p (clamp-cursor pos buffer))
               (new-buffer (string-append (substring buffer 0 p)
                                          str
                                          (substring buffer p))))
          (values new-buffer (+ p (string-length str)))))

(define (backspace-at buffer pos)
        "Delete the character before cursor position POS in BUFFER.
A no-op when POS is 0. Return (values new-buffer new-pos)."
        (let ((p (clamp-cursor pos buffer)))
          (if (<= p 0)
            (values buffer 0)
            (values (string-append (substring buffer 0 (- p 1))
                                   (substring buffer p))
                    (- p 1)))))

(define (delete-at buffer pos)
        "Delete the character at cursor position POS in BUFFER (KEY_DC semantics).
A no-op when POS is at the end. Return (values new-buffer new-pos)."
        (let* ((p (clamp-cursor pos buffer))
               (len (string-length buffer)))
          (if (>= p len)
            (values buffer p)
            (values (string-append (substring buffer 0 p)
                                   (substring buffer (+ p 1)))
                    p))))

(define (input-visible-window buffer cursor-pos avail-width)
        "Compute the horizontally scrolled slice of BUFFER that fits a window
AVAIL-WIDTH columns wide such that CURSOR-POS is always visible.
Return (values visible-substring cursor-x-in-window). When the buffer fits
entirely no scrolling occurs; otherwise the window follows the cursor and
prefers to keep at least one column of right context in view."
        (let* ((len (string-length buffer))
               (pos (clamp-cursor cursor-pos buffer))
               (w (max 1 avail-width)))
          (cond
            ((<= len w)
              (values buffer pos))
            ((= w 1)
              (let ((start (min pos len)))
                (values (substring buffer start (min len (+ start 1))) 0)))
            ((<= pos (- w 2))
              (values (substring buffer 0 w) pos))
            ((>= pos len)
              (let ((start (- len w)))
                (values (substring buffer start len) w)))
            (else
              (let ((start (- pos (- w 2))))
                (values (substring buffer start (min len (+ start w)))
                        (- pos start)))))))

;; Maximum rows the wrapped input may occupy before it scrolls.
(define %max-input-rows 4)

(define* (input-wrapped-rows buffer cursor-pos avail-width max-rows)
         "Lay out BUFFER for multi-line wrapped display AVAIL-WIDTH columns wide,
keeping at most MAX-ROWS lines and scrolling so the line holding the
editing cursor stays visible. Return (values visible-lines cursor-row
cursor-col): VISIBLE-LINES is the list of rendered lines, CURSOR-ROW the
0-based index of the cursor's line within VISIBLE-LINES, CURSOR-COL the
column of the cursor within that line."
         (let* ((w (max 1 avail-width))
                (mr (max 1 max-rows))
                (lines (wrap-text buffer w))
                (prefix (substring buffer 0 (clamp-cursor cursor-pos buffer)))
                (prefix-lines (wrap-text prefix w))
                (cur-row (max 0 (- (length prefix-lines) 1)))
                (cur-col (string-length (car (last-pair prefix-lines))))
                (total (length lines))
                (scrolled (if (<= total mr)
                            0
                            (max 0
                                 (min (max 0 (- cur-row (- mr 1)))
                                      (- total mr)))))
                (visible (take (drop lines scrolled)
                               (min mr (- total scrolled)))))
           (values visible
                   (max 0 (- cur-row scrolled))
                   (min cur-col w))))

;;;
;;; Input History Recall & Session Persistence
;;;

(define %history-max-entries 500)

(define-record-type <input-history>
                    (%make-input-history entries nav-index)
                    input-history?
                    (entries history-entries set-history-entries!)
                    (nav-index history-recall-position set-history-recall-position!))

(define (history-cap-entries entries)
        "Return ENTRIES truncated to the newest %history-max-entries items,
dropping the oldest entries when over the cap."
        (let ((len (length entries)))
          (if (> len %history-max-entries)
            (take-right entries %history-max-entries)
            entries)))

(define* (make-history #:optional (initial-entries '()))
         "Create a fresh input history seeded with INITIAL-ENTRIES (oldest first),
capped at %history-max-entries, with the recall position at the newest
boundary (draft state)."
         (%make-input-history (history-cap-entries initial-entries) #f))

(define (history-length hist)
        "Return the number of recorded entries in HIST."
        (length (history-entries hist)))

(define (history-add hist entry)
        "Append ENTRY (trimmed) to HIST, skipping empty input and entries
identical to the most recent one. Cap the list at %history-max-entries
(oldest dropped) and reset the recall position to the newest boundary.
Return #t when the entry was added, #f when it was skipped."
        (let* ((raw (if (string? entry) entry ""))
               (trimmed (string-trim-both raw))
               (entries (history-entries hist)))
          (cond
            ((string-null? trimmed)
              (set-history-recall-position! hist #f)
              #f)
            ((and (pair? entries) (string=? (last entries) trimmed))
              (set-history-recall-position! hist #f)
              #f)
            (else
              (set-history-entries! hist
                                    (history-cap-entries (append entries (list trimmed))))
              (set-history-recall-position! hist #f)
              #t))))

(define (history-recall hist delta)
        "Advance the recall position of HIST by DELTA entries (-1 = older,
+1 = newer) and return the entry at the new position. Return #f when the
position moves past the newest entry, meaning the caller should restore
the in-progress draft input. The position clamps at the oldest entry."
        (let* ((entries (history-entries hist))
               (len (length entries)))
          (if (= len 0)
            #f
            (let ((target (+ (or (history-recall-position hist) len) delta)))
              (cond
                ((>= target len)
                  (set-history-recall-position! hist #f)
                  #f)
                ((< target 0)
                  (set-history-recall-position! hist 0)
                  (car entries))
                (else
                  (set-history-recall-position! hist target)
                  (list-ref entries target)))))))

(define (history-reset-nav hist)
        "Reset the recall position of HIST to the newest boundary (draft state)."
        (set-history-recall-position! hist #f))

(define (history-entry->jsonl-line input mode ts)
        "Serialize a history record (INPUT string, MODE tag, TS unix seconds)
into a single JSON object line for the persistent history journal."
        (scm->json-string
          `(("input" . ,(if (string? input) input (format #f "~a" (or input ""))))
            ("mode" . ,(cond
                         ((string? mode) mode)
                         ((symbol? mode) (symbol->string mode))
                         (else "chat")))
            ("ts" . ,(if (number? ts) ts 0)))))

(define (jsonl-line->history-entry line)
        "Parse a single JSONL LINE into a history entry alist of the form
((\"input\" . STRING) (\"mode\" . STRING) (\"ts\" . NUMBER)). Return #f for
blank or malformed lines and for entries without a string input."
        (let ((parsed (safe-json-parse line)))
          (if (and (list? parsed) (string? (assoc-ref parsed "input")))
            `(("input" . ,(assoc-ref parsed "input"))
              ("mode" . ,(let ((m (assoc-ref parsed "mode")))
                           (cond
                             ((string? m) m)
                             ((symbol? m) (symbol->string m))
                             (else "chat"))))
              ("ts" . ,(let ((t (assoc-ref parsed "ts")))
                         (if (number? t) t 0))))
            #f)))

(define (default-history-file-path)
        "Return the default persistent input history path (.badb/history.jsonl
under the current workspace directory)."
        ".badb/history.jsonl")

(define (load-history-file path)
        "Load persisted input history from the JSONL file at PATH, returning the
list of input strings oldest-first, capped at %history-max-entries with
consecutive duplicates collapsed. An absent or unreadable file yields an
empty history."
        (catch #t
          (lambda ()
            (if (and (string? path) (file-exists? path))
              (let* ((raw (or (file->string path) ""))
                     (entries (filter-map jsonl-line->history-entry
                                          (string-split raw #\newline))))
                (history-cap-entries
                  (let loop ((rest (map (lambda (e) (assoc-ref e "input")) entries))
                             (acc '()))
                            (cond
                              ((null? rest) (reverse acc))
                              ((and (pair? acc) (string=? (car acc) (car rest)))
                                (loop (cdr rest) acc))
                              (else
                                (loop (cdr rest) (cons (car rest) acc)))))))
              '()))
          (lambda _ '())))

(define (append-history-file-entry! path input mode)
        "Append a single history record for INPUT and MODE to the JSONL file at
PATH, creating the enclosing directory when missing. Return #t on success
and #f on any filesystem failure."
        (catch #t
          (lambda ()
            (ensure-parent-directory! path)
            (let ((port (open-file path "a")))
              (display (history-entry->jsonl-line input mode
                                                  (time-second (current-time)))
                       port)
              (newline port)
              (close-port port)
              #t))
          (lambda _ #f)))
