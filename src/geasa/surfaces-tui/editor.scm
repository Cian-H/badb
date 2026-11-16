;;; (geasa surfaces-tui editor) — Text Wrapping, Slash Commands & Line Editor
;;;
;;; Extracted from surfaces-tui plugin.scm. Provides text wrapping, slash
;;; command registry/parsing, cursor editing operations, and input history.

(define-module (geasa surfaces-tui editor)
               #:use-module (geasa foundation-util plugin)
               #:use-module (json)
               #:use-module (ice-9 format)
               #:use-module (ice-9 ftw)
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
                          prompt-mode-on-char
                          prompt-mode-on-backspace
                          prompt-mode-on-escape
                          prompt-mode-submit-text
                          format-slash-completion
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
                          append-history-file-entry!
                          fuzzy-score
                          fuzzy-match-history
                          <history-search>
                          history-search?
                          history-search-active?
                          history-search-query
                          history-search-matches
                          history-search-selected-index
                          history-search-saved-buffer
                          history-search-saved-pos
                          make-history-search
                          history-search-open
                          history-search-update
                          history-search-move
                          history-search-selected
                          history-search-accept
                          history-search-cancel
                          history-search-handle-key
                          extract-at-token
                          at-selector-active?
                          collect-workspace-files
                          matching-at-candidates
                          apply-at-completion
                          <keymap>
                          keymap?
                          make-keymap
                          make-default-keymap
                          normalize-key-spec
                          parse-key-spec
                          format-key-spec
                          key-event->spec
                          keymap-bind!
                          keymap-unbind!
                          keymap-lookup
                          keymap-lookup-entry
                          keymap-bindings
                          keymap-action-key
                          keymap->menu-items))

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
          ("dump" . "Export the conversation to a text file [path]")
          ("image" . "Preview an image via notcurses/Kitty (/image <path>)")))

(define* (selector-active? input-buffer #:optional (prompt-mode 'chat))
         "Return #t if the slash command selector palette should be visible.
Active when PROMPT-MODE is 'slash (and no space has been typed yet) or when
INPUT-BUFFER begins with \"/\" with no spaces."
         (let ((buf (if (string? input-buffer) input-buffer "")))
           (and (not (string-contains buf " "))
                (or (eq? prompt-mode 'slash)
                    (string-prefix? "/" buf)))))

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

(define (%split-command-body body)
        "Split slash command BODY (without leading slash) into (cmd . arg-string)."
        (let ((space-pos (string-index body #\space)))
          (if space-pos
            (cons (substring body 0 space-pos)
                  (string-trim-both (substring body (+ space-pos 1))))
            (cons body ""))))

(define* (parse-command input #:optional (prompt-mode 'chat))
         "Parse user INPUT into (command . arg-string) or #f if not a slash command.
When PROMPT-MODE is 'slash, INPUT without a leading slash is parsed directly."
         (let ((trimmed (string-trim-both (if (string? input) input ""))))
           (cond
             ((string-prefix? "/" trimmed)
               (%split-command-body (substring trimmed 1)))
             ((eq? prompt-mode 'slash)
               (%split-command-body trimmed))
             (else #f))))

(define (%coerce-input-char ch)
        "Convert a char, integer codepoint, or 1-char string CH into a Scheme char."
        (cond
          ((char? ch) ch)
          ((integer? ch) (integer->char ch))
          ((and (string? ch) (not (string-null? ch))) (string-ref ch 0))
          (else #f)))

(define (prompt-mode-on-char prompt-mode input-buffer cursor-pos ch)
        "Handle printable character CH in PROMPT-MODE with INPUT-BUFFER at CURSOR-POS.
Typing '!' or '/' on an empty 'chat prompt absorbs the trigger character and
transitions to 'shell or 'slash respectively; '(' enters 'scheme keeping '(' in
the buffer. Return (values new-mode new-buffer new-pos absorbed?)."
        (let ((c (%coerce-input-char ch))
              (buf (if (string? input-buffer) input-buffer "")))
          (cond
            ((not c)
              (values prompt-mode buf (clamp-cursor cursor-pos buf) #f))
            ((and (eq? prompt-mode 'chat) (string-null? buf))
              (cond
                ((char=? c #\!) (values 'shell "" 0 #t))
                ((char=? c #\/) (values 'slash "" 0 #t))
                ((char=? c #\() (values 'scheme "(" 1 #f))
                (else (values 'chat (string c) 1 #f))))
            (else
              (call-with-values (lambda () (insert-at buf cursor-pos (string c)))
                                (lambda (new-buf new-pos)
                                  (values prompt-mode new-buf new-pos #f)))))))

(define (prompt-mode-on-backspace prompt-mode input-buffer cursor-pos)
        "Handle Backspace in PROMPT-MODE with INPUT-BUFFER at CURSOR-POS.
On a non-empty buffer, deletes one character backward; on an empty non-'chat
prompt ('slash, 'shell, or 'scheme), returns to 'chat mode.
Return (values new-mode new-buffer new-pos)."
        (let ((buf (if (string? input-buffer) input-buffer "")))
          (cond
            ((> (string-length buf) 0)
              (call-with-values (lambda () (backspace-at buf cursor-pos))
                                (lambda (new-buf new-pos)
                                  (values prompt-mode new-buf new-pos))))
            ((not (eq? prompt-mode 'chat))
              (values 'chat "" 0))
            (else
              (values 'chat "" 0)))))

(define (prompt-mode-on-escape prompt-mode input-buffer cursor-pos)
        "Handle Escape for prompt mode transitions. Leaves modal prompts or an
active slash selector back to 'chat with an empty buffer.
Return (values new-mode new-buffer new-pos)."
        (let ((buf (if (string? input-buffer) input-buffer "")))
          (cond
            ((not (eq? prompt-mode 'chat))
              (values 'chat "" 0))
            ((selector-active? buf prompt-mode)
              (values 'chat "" 0))
            (else
              (values prompt-mode buf (clamp-cursor cursor-pos buf))))))

(define (prompt-mode-submit-text prompt-mode input-buffer)
        "Reconstruct canonical input text for PROMPT-MODE and INPUT-BUFFER
before classification or history persistence."
        (let ((trimmed (string-trim-both (if (string? input-buffer) input-buffer ""))))
          (case prompt-mode
            ((slash)
              (if (string-prefix? "/" trimmed)
                trimmed
                (string-append "/" trimmed)))
            ((shell)
              (if (string-prefix? "!" trimmed)
                trimmed
                (string-append "!" trimmed)))
            (else trimmed))))

(define* (format-slash-completion cmd #:optional (prompt-mode 'chat) (with-space? #f))
         "Format completed slash command CMD for insertion into the input buffer
under PROMPT-MODE. In 'slash mode the leading '/' is omitted because it is
absorbed into the prompt indicator."
         (string-append (if (eq? prompt-mode 'slash) "" "/")
                        cmd
                        (if with-space? " " "")))

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

;;;
;;; History Fuzzy Finder (Ctrl+R) & Interactive @ Mention Completion
;;;
(define (%word-boundary-char? ch)
        "Return #t if CH separates path/identifier segments for fuzzy scoring."
        (memv ch '(#\/ #\- #\_ #\. #\space #\:)))

(define (fuzzy-score query candidate)
        "Compute an integer fuzzy-match score of QUERY against CANDIDATE
(case-insensitive ordered subsequence match), or #f when QUERY does not
match. Higher scores indicate tighter matches (exact > prefix > substring >
word-boundary > scattered)."
        (cond
          ((not (string? candidate)) #f)
          ((or (not (string? query)) (string-null? (string-trim-both query))) 0)
          (else
            (let* ((q (string-downcase query))
                   (c (string-downcase candidate))
                   (qlen (string-length q))
                   (clen (string-length c)))
              (if (> qlen clen)
                #f
                (let loop ((qi 0) (ci 0) (prev-ci -1) (score 0))
                          (cond
                            ((>= qi qlen)
                              (+ score
                                 (if (string=? q c) 200 0)
                                 (if (string-prefix? q c) 120 0)
                                 (if (string-contains c q) 80 0)
                                 (- (quotient clen 10))))
                            ((>= ci clen) #f)
                            ((char=? (string-ref q qi) (string-ref c ci))
                              (let* ((consec (if (and (>= prev-ci 0) (= ci (+ prev-ci 1))) 15 0))
                                     (boundary (if (or (= ci 0)
                                                       (%word-boundary-char? (string-ref c (- ci 1))))
                                                 10
                                                 0))
                                     (gap (if (>= prev-ci 0) (- ci prev-ci 1) 0)))
                                (loop (+ qi 1) (+ ci 1) ci (+ score 5 consec boundary (- gap)))))
                            (else
                              (loop qi (+ ci 1) prev-ci score)))))))))

(define (%dedupe-newest-first entries)
        "Return unique strings from ENTRIES (oldest-first) ordered newest-first."
        (let ((seen (make-hash-table 63)))
          (let loop ((rest (reverse entries)) (acc '()))
                    (if (null? rest)
                      (reverse acc)
                      (let ((item (car rest)))
                        (if (or (not (string? item)) (hash-ref seen item))
                          (loop (cdr rest) acc)
                          (begin
                            (hash-set! seen item #t)
                            (loop (cdr rest) (cons item acc)))))))))

(define* (fuzzy-match-history hist-or-entries query #:optional (limit #f))
         "Search HIST-OR-ENTRIES (<input-history> or list of strings) for entries
matching QUERY via fuzzy-score. Returns unique matches ordered by score
descending with newest-first recency breaking ties, optionally capped at LIMIT."
         (let* ((raw (cond
                       ((input-history? hist-or-entries) (history-entries hist-or-entries))
                       ((list? hist-or-entries) hist-or-entries)
                       (else '())))
                (unique (%dedupe-newest-first raw))
                (q (if (string? query) (string-trim-both query) ""))
                (matched (if (string-null? q)
                           unique
                           (map car
                                (stable-sort
                                  (filter-map (lambda (entry)
                                                (let ((s (fuzzy-score q entry)))
                                                  (and s (cons entry s))))
                                              unique)
                                  (lambda (a b) (> (cdr a) (cdr b))))))))
           (if (and (integer? limit) (>= limit 0) (> (length matched) limit))
             (take matched limit)
             matched)))

(define-record-type <history-search>
                    (%make-history-search active? query matches selected-index saved-buffer saved-pos)
                    history-search?
                    (active? history-search-active?)
                    (query history-search-query)
                    (matches history-search-matches)
                    (selected-index history-search-selected-index)
                    (saved-buffer history-search-saved-buffer)
                    (saved-pos history-search-saved-pos))

(define* (make-history-search #:key (active? #f) (query "") (matches '())
                              (selected-index 0) (saved-buffer "") (saved-pos 0))
         "Construct an immutable <history-search> state for Ctrl+R interactive
history filtering."
         (%make-history-search active?
                               (if (string? query) query "")
                               (if (list? matches) matches '())
                               (max 0 selected-index)
                               (if (string? saved-buffer) saved-buffer "")
                               (max 0 saved-pos)))

(define (history-search-open hist saved-buffer saved-pos)
        "Enter interactive Ctrl+R history search over HIST, stashing SAVED-BUFFER
and SAVED-POS for cancellation restore."
        (let* ((buf (if (string? saved-buffer) saved-buffer ""))
               (pos (clamp-cursor saved-pos buf))
               (matches (fuzzy-match-history hist "")))
          (make-history-search #:active? #t
                               #:query ""
                               #:matches matches
                               #:selected-index 0
                               #:saved-buffer buf
                               #:saved-pos pos)))

(define (history-search-update state hist new-query)
        "Update the query of <history-search> STATE to NEW-QUERY and recompute
fuzzy matches against HIST, resetting selection to the top match."
        (let* ((q (if (string? new-query) new-query ""))
               (matches (fuzzy-match-history hist q)))
          (make-history-search #:active? #t
                               #:query q
                               #:matches matches
                               #:selected-index 0
                               #:saved-buffer (history-search-saved-buffer state)
                               #:saved-pos (history-search-saved-pos state))))

(define (history-search-move state delta)
        "Move the selected index in <history-search> STATE by DELTA, wrapping
across available matches."
        (let* ((matches (history-search-matches state))
               (count (length matches)))
          (if (= count 0)
            state
            (let* ((m (remainder (+ (history-search-selected-index state) delta) count))
                   (idx (if (< m 0) (+ m count) m)))
              (make-history-search #:active? (history-search-active? state)
                                   #:query (history-search-query state)
                                   #:matches matches
                                   #:selected-index idx
                                   #:saved-buffer (history-search-saved-buffer state)
                                   #:saved-pos (history-search-saved-pos state))))))

(define (history-search-selected state)
        "Return the currently highlighted history entry string in STATE, or #f."
        (let* ((matches (history-search-matches state))
               (count (length matches)))
          (if (= count 0)
            #f
            (list-ref matches (min (max 0 (history-search-selected-index state)) (- count 1))))))

(define (history-search-accept state)
        "Confirm the selected match in STATE. Return (values new-buffer new-pos
closed-state)."
        (let* ((chosen (or (history-search-selected state) (history-search-saved-buffer state)))
               (pos (string-length chosen)))
          (values chosen pos (make-history-search))))

(define (history-search-cancel state)
        "Cancel history search and restore the pre-search buffer.
Return (values saved-buffer saved-pos closed-state)."
        (values (history-search-saved-buffer state)
                (history-search-saved-pos state)
                (make-history-search)))

(define (history-search-handle-key state hist ch)
        "Dispatch keypress CH inside an active Ctrl+R history search STATE.
Return (values action new-state result-buffer result-pos) where ACTION is one of
'accept, 'cancel, 'update, or 'none."
        (cond
          ((not (history-search-active? state))
            (values 'none state #f #f))
          ((or (eqv? ch 10) (eqv? ch 13) (eqv? ch #\newline) (eqv? ch #\return))
            (call-with-values (lambda () (history-search-accept state))
                              (lambda (buf pos closed)
                                (values 'accept closed buf pos))))
          ((or (eqv? ch 27) (eqv? ch 7))
            (call-with-values (lambda () (history-search-cancel state))
                              (lambda (buf pos closed)
                                (values 'cancel closed buf pos))))
          ((or (eqv? ch 18) (eqv? ch 'down) (eqv? ch 'ctrl-r))
            (values 'update (history-search-move state 1) #f #f))
          ((or (eqv? ch 16) (eqv? ch 'up) (eqv? ch 'ctrl-p))
            (values 'update (history-search-move state -1) #f #f))
          ((or (eqv? ch 8) (eqv? ch 127) (eqv? ch #\backspace) (eqv? ch 'backspace))
            (let* ((q (history-search-query state))
                   (new-q (if (string-null? q) q (substring q 0 (- (string-length q) 1)))))
              (values 'update (history-search-update state hist new-q) #f #f)))
          ((let ((c (%coerce-input-char ch)))
             (and c (>= (char->integer c) 32) (not (= (char->integer c) 127)) c))
            => (lambda (c)
                 (values 'update
                         (history-search-update state hist (string-append (history-search-query state) (string c)))
                         #f
                         #f)))
          (else
            (values 'none state #f #f))))

;;;
;;; Interactive @ Mention Completion
;;;
(define (%token-delimiter? ch)
        "Return #t when CH ends/separates a prompt token."
        (memv ch '(#\space #\tab #\newline #\return)))

(define* (extract-at-token input-buffer #:optional (cursor-pos #f))
         "Inspect the token immediately preceding CURSOR-POS in INPUT-BUFFER.
Return an alist ((start . N) (end . M) (query . STR) (type . 'file|'any)) when
the cursor is inside a whitespace-delimited @mention (ignoring escaped @@), or
#f otherwise."
         (let* ((buf (if (string? input-buffer) input-buffer ""))
                (pos (clamp-cursor (or cursor-pos (string-length buf)) buf)))
           (let scan ((i (- pos 1)))
                     (cond
                       ((< i 0)
                         (%parse-at-slice buf 0 pos))
                       ((%token-delimiter? (string-ref buf i))
                         (%parse-at-slice buf (+ i 1) pos))
                       (else
                         (scan (- i 1)))))))

(define (%parse-at-slice buf start end)
        "Parse BUF[START..END] as an @mention token or return #f."
        (if (>= start end)
          #f
          (let ((tok (substring buf start end)))
            (cond
              ((not (string-prefix? "@" tok)) #f)
              ((string-prefix? "@@" tok) #f)
              ((string-prefix? "@file:" tok)
                `((start . ,start) (end . ,end) (query . ,(substring tok 6)) (type . file)))
              (else
                `((start . ,start) (end . ,end) (query . ,(substring tok 1)) (type . any)))))))

(define* (at-selector-active? input-buffer #:optional (cursor-pos #f) (prompt-mode 'chat))
         "Return #t when an @ mention autocomplete popup should be shown at
CURSOR-POS in INPUT-BUFFER under PROMPT-MODE."
         (and (memq prompt-mode '(chat steering))
              (not (selector-active? input-buffer prompt-mode))
              (pair? (extract-at-token input-buffer cursor-pos))))

(define %ignored-workspace-dirs
        '(".git" ".badb" ".worktrees" ".serena" ".cache" "node_modules"))

(define (%skip-workspace-entry? name)
        "Return #t if directory entry NAME should be skipped when scanning files."
        (or (member name '("." ".."))
            (member name %ignored-workspace-dirs)
            (string-suffix? ".go" name)
            (string-suffix? ".log" name)))

(define* (collect-workspace-files #:optional (root ".") (max-files 500))
         "Walk ROOT recursively, returning a sorted list of relative file paths
(capped at MAX-FILES), skipping VCS and generated directories."
         (let ((cap (max 1 max-files))
               (base (if (and (string? root) (not (string-null? root))) root ".")))
           (define (walk dir rel-prefix acc)
                   (if (>= (length acc) cap)
                     acc
                     (let ((entries (catch #t
                                      (lambda () (or (scandir dir valid-dir-entry?) '()))
                                      (lambda _ '()))))
                       (let loop ((rest (sort entries string<?)) (out acc))
                                 (if (or (null? rest) (>= (length out) cap))
                                   out
                                   (let ((name (car rest)))
                                     (if (%skip-workspace-entry? name)
                                       (loop (cdr rest) out)
                                       (let* ((full (string-append dir "/" name))
                                              (rel (if (string-null? rel-prefix)
                                                     name
                                                     (string-append rel-prefix "/" name)))
                                              (is-dir? (catch #t (lambda () (file-is-directory? full)) (lambda _ #f))))
                                         (if is-dir?
                                           (loop (cdr rest) (walk full rel out))
                                           (loop (cdr rest) (cons rel out)))))))))))
           (reverse (walk base "" '()))))

(define (%normalize-at-candidate item)
        "Normalize string or (label . kind) candidate ITEM into (label . kind)."
        (cond
          ((and (pair? item) (string? (car item)))
            (cons (car item) (ensure-string (or (cdr item) "file"))))
          ((string? item) (cons item "file"))
          (else (cons (ensure-string item) "item"))))

(define* (matching-at-candidates input-buffer #:key (cursor-pos #f) (candidates #f)
                                 (root ".") (shortcuts '()) (limit 50))
         "Return up to LIMIT matching (label . kind) pairs for the active @ token
at CURSOR-POS in INPUT-BUFFER. Uses CANDIDATES when provided, else scans ROOT
workspace files plus SHORTCUTS."
         (let ((tok (extract-at-token input-buffer cursor-pos)))
           (if (not tok)
             '()
             (let* ((query (assoc-ref tok 'query))
                    (type (assoc-ref tok 'type))
                    (pool (append
                            (if (and (eq? type 'any) (list? shortcuts))
                              (map (lambda (s)
                                     (cons (ensure-string (if (pair? s) (car s) s)) "shortcut"))
                                   shortcuts)
                              '())
                            (map %normalize-at-candidate
                                 (or candidates (collect-workspace-files root)))))
                    (scored (if (string-null? query)
                              (map (lambda (item) (cons item 0)) pool)
                              (stable-sort
                                (filter-map (lambda (item)
                                              (let ((s (fuzzy-score query (car item))))
                                                (and s (cons item s))))
                                            pool)
                                (lambda (a b) (> (cdr a) (cdr b))))))
                    (items (map car scored)))
               (if (and (integer? limit) (>= limit 0) (> (length items) limit))
                 (take items limit)
                 items)))))

(define* (apply-at-completion buffer cursor-pos chosen #:key (add-space? #t))
         "Replace the @ token ending at CURSOR-POS in BUFFER with CHOSEN
(string or (label . kind) pair). Files expand as @file:PATH; shortcuts/symbols
expand as @NAME. Return (values new-buffer new-pos)."
         (let* ((buf (if (string? buffer) buffer ""))
                (pos (clamp-cursor cursor-pos buf))
                (tok (extract-at-token buf pos)))
           (if (not tok)
             (values buf pos)
             (let* ((start (assoc-ref tok 'start))
                    (end (assoc-ref tok 'end))
                    (label (if (pair? chosen) (ensure-string (car chosen)) (ensure-string chosen)))
                    (kind (and (pair? chosen) (ensure-string (cdr chosen))))
                    (prefix (if (or (eq? (assoc-ref tok 'type) 'file)
                                    (equal? kind "file"))
                              "@file:"
                              "@"))
                    (replacement (string-append prefix label (if add-space? " " "")))
                    (new-buf (string-append (substring buf 0 start)
                                            replacement
                                            (substring buf end))))
               (values new-buf (+ start (string-length replacement)))))))

;;;
;;; Configurable Keymaps (<keymap>) & Ctrl+I / Tab Resolution
;;;
(define-record-type <keymap>
                    (%make-keymap table)
                    keymap?
                    (table keymap-table))

(define %key-name-aliases
        '(("escape" . "esc")
          ("return" . "enter")
          ("bs" . "backspace")
          ("del" . "delete")
          ("pgup" . "page-up")
          ("pgdn" . "page-down")))

(define (%normalize-base-key name)
        "Canonicalize a base key token NAME (lowercase string)."
        (or (assoc-ref %key-name-aliases name) name))

(define (%parse-key-string raw)
        "Parse lowercase key string RAW into canonical symbol (handling ctrl+, c-,
^, alt+, m-, shift+, s- prefixes uniformly)."
        (let ((s (string-downcase (string-trim-both raw))))
          (cond
            ((string-prefix? "ctrl+" s)
              (string->symbol (string-append "ctrl-" (%normalize-base-key (substring s 5)))))
            ((string-prefix? "ctrl-" s)
              (string->symbol (string-append "ctrl-" (%normalize-base-key (substring s 5)))))
            ((string-prefix? "c-" s)
              (string->symbol (string-append "ctrl-" (%normalize-base-key (substring s 2)))))
            ((and (string-prefix? "^" s) (> (string-length s) 1))
              (string->symbol (string-append "ctrl-" (%normalize-base-key (substring s 1)))))
            ((string-prefix? "alt+" s)
              (string->symbol (string-append "alt-" (%normalize-base-key (substring s 4)))))
            ((string-prefix? "alt-" s)
              (string->symbol (string-append "alt-" (%normalize-base-key (substring s 4)))))
            ((string-prefix? "m-" s)
              (string->symbol (string-append "alt-" (%normalize-base-key (substring s 2)))))
            ((string-prefix? "shift+" s)
              (string->symbol (string-append "shift-" (%normalize-base-key (substring s 6)))))
            ((string-prefix? "shift-" s)
              (string->symbol (string-append "shift-" (%normalize-base-key (substring s 6)))))
            ((string-prefix? "s-" s)
              (string->symbol (string-append "shift-" (%normalize-base-key (substring s 2)))))
            (else
              (string->symbol (%normalize-base-key s))))))

(define (parse-key-spec spec)
        "Parse key specification SPEC (symbol or string like \"Ctrl+R\" or \"C-r\")
into a canonical symbol key spec."
        (cond
          ((symbol? spec) (%parse-key-string (symbol->string spec)))
          ((string? spec) (%parse-key-string spec))
          (else (ensure-symbol spec))))

(define (normalize-key-spec spec)
        "Normalize key specification SPEC to a canonical symbol."
        (parse-key-spec spec))

(define (format-key-spec spec)
        "Format canonical key SPEC as a human-readable label (e.g. \"Ctrl+R\", \"Tab\")."
        (let*
            ((sym (normalize-key-spec spec)) (s (symbol->string sym)))
          (cond
            ((eq? sym 'tab) "Tab")
            ((eq? sym 'shift-tab) "Shift+Tab")
            ((eq? sym 'esc) "Esc")
            ((eq? sym 'enter) "Enter")
            ((eq? sym 'backspace) "Backspace")
            ((eq? sym 'delete) "Delete")
            ((eq? sym 'page-up) "PgUp")
            ((eq? sym 'page-down) "PgDn")
            ((string-prefix? "ctrl-" s)
              (string-append "Ctrl+" (string-capitalize (substring s 5))))
            ((string-prefix? "alt-" s)
              (string-append "Alt+" (string-capitalize (substring s 4))))
            ((string-prefix? "shift-" s)
              (string-append "Shift+" (string-capitalize (substring s 6))))
            (else (string-capitalize s)))))

(define %ctrl-code->spec
        '((1 . ctrl-a)
          (2 . ctrl-b)
          (3 . ctrl-c)
          (4 . ctrl-d)
          (5 . ctrl-e)
          (6 . ctrl-f)
          (7 . ctrl-g)
          (11 . ctrl-k)
          (12 . ctrl-l)
          (14 . ctrl-n)
          (15 . ctrl-o)
          (16 . ctrl-p)
          (18 . ctrl-r)
          (20 . ctrl-t)
          (21 . ctrl-u)
          (23 . ctrl-w)
          (24 . ctrl-x)
          (25 . ctrl-y)
          (26 . ctrl-z)))

(define* (key-event->spec ch #:key (ctrl-up-key #f) (ctrl-down-key #f)
                          (kitty-ctrl? #f) (key-constants '()))
         "Translate raw terminal input CH into a canonical key spec symbol.
Resolves the traditional ASCII 9 (Tab vs Ctrl+I) collision cleanly: byte 9 / #\\tab
maps to 'tab unless KITTY-CTRL? is true (for terminals distinguishing CSI-u Ctrl+I)."
         (let ((code (cond
                       ((integer? ch) ch)
                       ((char? ch) (char->integer ch))
                       (else #f))))
           (cond
             ((and ctrl-up-key (eqv? code ctrl-up-key)) 'ctrl-up)
             ((and ctrl-down-key (eqv? code ctrl-down-key)) 'ctrl-down)
             ((and code (assv-ref key-constants code)) => identity)
             ((eqv? code 9) (if kitty-ctrl? 'ctrl-i 'tab))
             ((and kitty-ctrl? (or (eqv? code 105) (eqv? code 73))) 'ctrl-i)
             ((or (eqv? code 10) (eqv? code 13)) 'enter)
             ((eqv? code 27) 'esc)
             ((or (eqv? code 8) (eqv? code 127)) 'backspace)
             ((and code (assv-ref %ctrl-code->spec code)) => identity)
             ((eqv? code 258) 'down)
             ((eqv? code 259) 'up)
             ((eqv? code 260) 'left)
             ((eqv? code 261) 'right)
             ((eqv? code 262) 'home)
             ((eqv? code 263) 'backspace)
             ((eqv? code 330) 'delete)
             ((eqv? code 338) 'page-down)
             ((eqv? code 339) 'page-up)
             ((eqv? code 343) 'enter)
             ((eqv? code 353) 'shift-tab)
             ((eqv? code 360) 'end)
             ((and code (>= code 32) (<= code 126))
               (string->symbol (string (integer->char code))))
             (else #f))))

(define (make-keymap)
        "Create an empty mutable <keymap>."
        (%make-keymap (make-hash-table 63)))

(define* (keymap-bind! km key-spec action #:key (context 'global) (description #f))
         "Bind KEY-SPEC in CONTEXT (default 'global) to ACTION symbol inside KM.
Passing ACTION as #f removes the binding. Return KM."
         (let ((k (cons (ensure-symbol context) (normalize-key-spec key-spec)))
               (tbl (keymap-table km)))
           (if action
             (hash-set! tbl k (cons (ensure-symbol action)
                                    (or description (symbol->string (ensure-symbol action)))))
             (hash-remove! tbl k))
           km))

(define* (keymap-unbind! km key-spec #:key (context 'global))
         "Remove any binding for KEY-SPEC in CONTEXT from KM."
         (hash-remove! (keymap-table km)
                       (cons (ensure-symbol context) (normalize-key-spec key-spec)))
         km)

(define* (keymap-lookup-entry km key-spec #:optional (context 'global))
         "Look up (action . description) for KEY-SPEC in CONTEXT, falling back to
'global if CONTEXT has no override. Return #f when unbound."
         (let ((tbl (keymap-table km))
               (ctx (ensure-symbol context))
               (norm (normalize-key-spec key-spec)))
           (or (hash-ref tbl (cons ctx norm))
               (and (not (eq? ctx 'global))
                    (hash-ref tbl (cons 'global norm))))))

(define* (keymap-lookup km key-spec #:optional (context 'global))
         "Look up the action symbol bound to KEY-SPEC in CONTEXT (with 'global
fallback), or #f if unbound."
         (let ((entry (keymap-lookup-entry km key-spec context)))
           (and entry (car entry))))

(define* (keymap-bindings km #:optional (context #f))
         "Return a deterministically sorted list of binding alists in KM,
optionally filtered to CONTEXT."
         (let ((target-ctx (and context (ensure-symbol context)))
               (acc '()))
           (hash-for-each
             (lambda (k v)
               (when (or (not target-ctx) (eq? (car k) target-ctx))
                 (set! acc
                       (cons `((context . ,(car k))
                               (key . ,(cdr k))
                               (action . ,(car v))
                               (description . ,(cdr v)))
                             acc))))
             (keymap-table km))
           (sort acc
                 (lambda (a b)
                   (let ((ca (symbol->string (assoc-ref a 'context)))
                         (cb (symbol->string (assoc-ref b 'context))))
                     (if (string=? ca cb)
                       (string<? (format-key-spec (assoc-ref a 'key))
                                 (format-key-spec (assoc-ref b 'key)))
                       (string<? ca cb)))))))

(define* (keymap-action-key km action #:optional (context 'global))
         "Return the first canonical key spec bound to ACTION in CONTEXT (or 'global),
or #f when unbound."
         (let ((act (ensure-symbol action))
               (bindings (keymap-bindings km context)))
           (or (any (lambda (b) (and (eq? (assoc-ref b 'action) act) (assoc-ref b 'key)))
                    bindings)
               (and (not (eq? context 'global))
                    (any (lambda (b) (and (eq? (assoc-ref b 'action) act) (assoc-ref b 'key)))
                         (keymap-bindings km 'global))))))

(define* (keymap->menu-items km #:optional (context #f))
         "Convert bindings in KM (optionally filtered by CONTEXT) into menu items
compatible with (geasa surfaces-tui menus) make-menu."
         (map (lambda (b)
                (let* ((ctx (assoc-ref b 'context))
                       (key (assoc-ref b 'key))
                       (act (assoc-ref b 'action))
                       (desc (assoc-ref b 'description))
                       (label (format-key-spec key))
                       (detail (format #f "~a [~a] — ~a" act ctx desc)))
                  (cons (cons label detail)
                        (list 'keymap-bind ctx key act))))
              (keymap-bindings km context)))

(define (make-default-keymap)
        "Build the default TUI <keymap> resolving the Ctrl+I/Tab collision:
'tab is dedicated to 'complete (never 'interrupt), 'interrupt uses 'ctrl-c or 'esc
in 'steering mode, and collapse/interrupt/fuzzy-history/external-editor are all
rebindable."
        (let ((km (make-keymap)))
          ;; Completion & navigation: Tab is strictly completion, never interrupt.
          (keymap-bind! km
                        'tab
                        'complete
                        #:context
                        'global
                        #:description
                        "Autocomplete slash command or @ mention")
          (keymap-bind! km
                        'shift-tab
                        'complete-prev
                        #:context
                        'global
                        #:description
                        "Cycle completion backwards")
          ;; Fuzzy history (Ctrl+R) & external $EDITOR (Ctrl+X)
          (keymap-bind! km
                        'ctrl-r
                        'fuzzy-history
                        #:context
                        'global
                        #:description
                        "Fuzzy search command/chat history")
          (keymap-bind! km
                        'ctrl-x
                        'external-editor
                        #:context
                        'global
                        #:description
                        "Edit prompt in external $EDITOR")
          ;; Collapse toggle: dedicated Ctrl+O globally + Ctrl+E when card selected
          (keymap-bind! km
                        'ctrl-o
                        'toggle-collapse
                        #:context
                        'global
                        #:description
                        "Toggle collapse of selected message card")
          (keymap-bind! km
                        'ctrl-e
                        'toggle-collapse
                        #:context
                        'msgsel
                        #:description
                        "Toggle collapse of selected message card")
          (keymap-bind! km
                        'ctrl-e
                        'line-end
                        #:context
                        'chat
                        #:description
                        "Move cursor to end of line")
          ;; Distinct Ctrl+I (when terminal reports CSI-u Ctrl+I) does not interrupt
          (keymap-bind! km
                        'ctrl-i
                        'toggle-collapse
                        #:context
                        'global
                        #:description
                        "Toggle collapse (Kitty/CSI-u Ctrl+I)")
          ;; Agent run steering & interrupt (rebindable; Tab is NOT bound here)
          (keymap-bind! km
                        'ctrl-c
                        'interrupt
                        #:context
                        'steering
                        #:description
                        "Force-stop running agent & subprocesses")
          (keymap-bind! km
                        'esc
                        'interrupt
                        #:context
                        'steering
                        #:description
                        "Interrupt running agent")
          (keymap-bind! km
                        'enter
                        'steer-send
                        #:context
                        'steering
                        #:description
                        "Send steering message to running agent")
          ;; Standard editing & selection
          (keymap-bind! km 'ctrl-c 'exit #:context 'chat #:description "Exit badb TUI")
          (keymap-bind! km 'ctrl-d 'exit #:context 'global #:description "Exit badb TUI")
          (keymap-bind! km
                        'ctrl-a
                        'line-start
                        #:context
                        'global
                        #:description
                        "Move cursor to start of input")
          (keymap-bind! km
                        'home
                        'line-start
                        #:context
                        'global
                        #:description
                        "Move cursor to start of input")
          (keymap-bind! km
                        'end
                        'line-end
                        #:context
                        'global
                        #:description
                        "Move cursor to end of input")
          (keymap-bind! km
                        'ctrl-up
                        'select-prev-message
                        #:context
                        'global
                        #:description
                        "Select previous message card")
          (keymap-bind! km
                        'ctrl-down
                        'select-next-message
                        #:context
                        'global
                        #:description
                        "Select next message card")
          (keymap-bind! km
                        'page-up
                        'scroll-page-up
                        #:context
                        'global
                        #:description
                        "Scroll transcript up")
          (keymap-bind! km
                        'page-down
                        'scroll-page-down
                        #:context
                        'global
                        #:description
                        "Scroll transcript down")
          (keymap-bind! km
                        'up
                        'history-prev
                        #:context
                        'chat
                        #:description
                        "Recall older input history")
          (keymap-bind! km
                        'down
                        'history-next
                        #:context
                        'chat
                        #:description
                        "Recall newer input history")
          (keymap-bind! km
                        'up
                        'selector-prev
                        #:context
                        'selector
                        #:description
                        "Highlight previous selector item")
          (keymap-bind! km
                        'down
                        'selector-next
                        #:context
                        'selector
                        #:description
                        "Highlight next selector item")
          (keymap-bind! km
                        'up
                        'selector-prev
                        #:context
                        'menu
                        #:description
                        "Highlight previous menu item")
          (keymap-bind! km
                        'down
                        'selector-next
                        #:context
                        'menu
                        #:description
                        "Highlight next menu item")
          (keymap-bind! km
                        'enter
                        'submit
                        #:context
                        'global
                        #:description
                        "Submit input or apply selection")
          (keymap-bind! km
                        'esc
                        'dismiss
                        #:context
                        'global
                        #:description
                        "Dismiss popup or exit modal prompt")
          (keymap-bind! km
                        'backspace
                        'delete-backward
                        #:context
                        'global
                        #:description
                        "Delete character before cursor")
          (keymap-bind! km
                        'delete
                        'delete-forward
                        #:context
                        'global
                        #:description
                        "Delete character at cursor")
          (keymap-bind! km
                        'shift-enter
                        'insert-newline
                        #:context
                        'global
                        #:description
                        "Insert newline without submitting")
          km))