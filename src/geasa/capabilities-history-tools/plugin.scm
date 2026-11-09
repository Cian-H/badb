;;; badb/capabilities/history-tools.scm — Conversation History Summarization
;;;
;;; Pure helpers that summarize a conversation's llm-history turn list and
;;; persisted session JSONL files. summarize-turns produces a compact human
;;; (and model) readable digest of an in-memory conversation: total turns,
;;; per-role counts, an approximate token estimate (reusing the (geasa runtimes-context
;;; plugin) char heuristic), the most recent user messages truncated for
;;; display, and a topic sketch of the top non-stopword words across user
;;; messages. summarize-history-file parses a session JSONL file (one
;;; {\"role\":...,\"content\":...} object per line, the tui session log shape)
;;; directly with (json), skipping malformed lines, and summarizes it.

(define-module (geasa capabilities-history-tools plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa runtimes-context plugin)
               #:use-module (json)
               #:use-module (ice-9 format)
               #:use-module (ice-9 rdelim)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-13)
               #:export (jsonl-line->turn
                          summarize-turns
                          summarize-history-file
                          turn-field
                          user-turn?
                          extract-words
                          meaningful-word?
                          word-frequencies
                          top-topic-words
                          %stopwords
                          %summary-recent-user-count
                          %summary-recent-user-chars
                          %summary-topic-count))

;;;
;;; Turn Accessors & Word Extraction
;;;
(define (turn-field turn key)
        "Return TURN's KEY field (symbol or string key accepted) as a string,
coercing non-string values; a missing field yields the empty string."
        (let ((v (or (and (pair? turn) (assoc-ref turn key))
                     (and (pair? turn) (assoc-ref turn (symbol->string key))))))
          (cond
            ((string? v) v)
            ((symbol? v) (symbol->string v))
            ((not v) "")
            (else (format #f "~a" v)))))

(define (user-turn? turn)
        "Return #t when TURN has role \"user\"."
        (string=? (turn-field turn 'role) "user"))

(define %summary-recent-user-count 5)
(define %summary-recent-user-chars 80)
(define %summary-topic-count 8)

(define %stopwords
        '("a" "an" "the" "and" "or" "but" "if" "then" "else" "when" "while"
          "of" "to" "in" "on" "at" "by" "for" "with" "from" "as" "into"
          "is" "are" "was" "were" "be" "been" "being" "am" "do" "does" "did"
          "have" "has" "had" "will" "would" "can" "could" "should" "may"
          "i" "me" "my" "you" "your" "he" "him" "his" "she" "her" "it" "its"
          "we" "us" "our" "they" "them" "their" "this" "that" "these" "those"
          "not" "no" "so" "just" "up" "down" "out" "over" "about" "there"
          "here" "what" "which" "who" "whom" "how" "why" "all" "any" "some"
          "now" "than" "also" "please"))

(define (extract-words text)
        "Return the lowercase words in TEXT split on non-alphabetic characters;
single-character fragments are discarded."
        (let loop ((chars (string->list (if (string? text) text (format #f "~a" text))))
                   (word '())
                   (acc '()))
                  (cond
                    ((null? chars)
                      (reverse (if (null? word)
                                 acc
                                 (cons (list->string (reverse word)) acc))))
                    ((char-alphabetic? (car chars))
                      (loop (cdr chars) (cons (char-downcase (car chars)) word) acc))
                    (else
                      (loop (cdr chars) '()
                            (if (null? word)
                              acc
                              (cons (list->string (reverse word)) acc)))))))

(define (meaningful-word? word)
        "Return #t when WORD is at least two characters and not a stopword."
        (and (>= (string-length word) 2)
             (not (member word %stopwords))))

(define (word-frequencies words)
        "Return an alist ((WORD . COUNT) ...) counting WORDS in first-seen order."
        (fold (lambda (w acc)
                (let ((pair (assoc w acc)))
                  (if pair
                    (begin (set-cdr! pair (+ 1 (cdr pair))) acc)
                    (cons (cons w 1) acc))))
              '()
              words))

;;;
;;; Topic Sketch
;;;
(define (top-topic-words turns count)
        "Return up to COUNT top topic words across the user messages of TURNS:
the most frequent meaningful (non-stopword, lowercase) words, sorted by
descending count with alphabetical order breaking ties."
        (let* ((contents (map (lambda (t) (turn-field t 'content))
                              (filter user-turn? turns)))
               (words (filter meaningful-word? (append-map extract-words contents)))
               (freqs (word-frequencies words))
               (ranked (sort freqs
                             (lambda (a b)
                               (or (> (cdr a) (cdr b))
                                   (and (= (cdr a) (cdr b))
                                        (string<? (car a) (car b))))))))
          (take ranked (min count (length ranked)))))

;;;
;;; Summarization
;;;
(define (summarize-turns turns)
        "Return a formatted multi-line summary of TURNS, the llm-history list of
role/content alists: total turns, per-role counts, an approximate token
estimate, the ~5 most recent user messages truncated to ~80 characters, and
a topic sketch of the top 8 non-stopword words across user messages."
        (let* ((turn-list (if (vector? turns) (vector->list turns) (or turns '())))
               (roles (map (lambda (t) (turn-field t 'role)) turn-list))
               (role-counts (sort (word-frequencies roles)
                                  (lambda (a b) (string<? (car a) (car b)))))
               (users (filter user-turn? turn-list))
               (recent-users (take (reverse users)
                                   (min %summary-recent-user-count (length users))))
               (topics (top-topic-words turn-list %summary-topic-count)))
          (string-join
            (list
              (format #f "Total turns: ~a" (length turn-list))
              (format #f "Estimated tokens: ~a" (estimate-conversation-tokens turn-list))
              (format #f
                      "Turns by role: ~a"
                      (if (null? role-counts)
                        "(none)"
                        (string-join (map (lambda (p) (format #f "~a=~a" (car p) (cdr p)))
                                          role-counts)
                                     ", ")))
              (string-join
                (cons "Recent user messages:"
                      (if (null? recent-users)
                        '("  (none)")
                        (reverse
                          (fold (lambda (t acc)
                                  (cons (format #f "  ~a. ~a"
                                                (+ 1 (length acc))
                                                (truncate-display (turn-field t 'content)
                                                                  %summary-recent-user-chars))
                                        acc))
                                '()
                                recent-users))))
                "\n")
              (format #f
                      "Topics: ~a"
                      (if (null? topics)
                        "(none)"
                        (string-join (map car topics) " "))))
            "\n")))

(define (jsonl-line->turn line)
        "Parse a single JSONL LINE into a turn alist of the form
((role . STRING) (content . STRING)); return #f for blank or malformed
lines and for entries without string role and content fields."
        (let ((parsed (safe-json-parse line)))
          (if (and (list? parsed)
                   (string? (assoc-ref parsed "role"))
                   (string? (assoc-ref parsed "content")))
            `((role . ,(assoc-ref parsed "role"))
              (content . ,(assoc-ref parsed "content")))
            #f)))

(define (summarize-history-file path)
        "Load the session JSONL file at PATH (one {\"role\":...,\"content\":...}
object per line), skip malformed lines, and return a summary string of the
parsed turns. Return #f when PATH is missing or unreadable."
        (and (string? path)
             (catch #t
               (lambda ()
                 (and (file-exists? path)
                      (summarize-turns
                        (call-with-input-file path
                                              (lambda (port)
                                                (let loop ((line (read-line port)) (acc '()))
                                                          (cond
                                                            ((eof-object? line) (reverse acc))
                                                            (else
                                                              (let ((turn (jsonl-line->turn line)))
                                                                (loop (read-line port)
                                                                      (if turn (cons turn acc) acc)))))))))))
               (lambda _ #f))))
