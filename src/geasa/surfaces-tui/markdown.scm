;;; (geasa surfaces-tui markdown) — Markdown, Backticks, Syntax Highlighting & Terminal Protocols
;;;
;;; Provides an mdflow wrapper with a pure-Scheme Markdown/backtick renderer fallback,
;;; real-time input formatting, shell/Scheme/code syntax highlighting, OSC 8 clickable
;;; link formatting & extraction, Nerd Font detection with Unicode fallback, and
;;; Kitty/notcurses ncvisual image capability detection and rendering helpers.

(define-module (geasa surfaces-tui markdown)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 regex)
               #:use-module
               (rnrs bytevectors)
               #:use-module
               (srfi srfi-1)
               #:autoload
               (system foreign)
               (%null-pointer)
               #:autoload
               (syntax-highlight)
               (highlight)
               #:autoload
               (syntax-highlight scheme)
               (lex-scheme)
               #:autoload
               (syntax-highlight c)
               (lex-c)
               #:autoload
               (syntax-highlight lisp)
               (lex-lisp)
               #:autoload
               (notcurses core)
               (notcurses_check_pixel_support notcurses_canopen_images)
               #:autoload
               (notcurses visual)
               (ncvisual_from_file ncvisual_blit ncvisual_geom ncvisual_destroy)
               #:autoload
               (notcurses structs)
               (make-ncvisual-options make-ncvgeom
                                      ncvgeom-pixy
                                      ncvgeom-pixx
                                      ncvgeom-rcelly
                                      ncvgeom-rcellx
                                      ->pointer)
               #:autoload
               (notcurses constants)
               (NCPIXEL_NONE NCPIXEL_SIXEL
                             NCPIXEL_LINUXFB
                             NCPIXEL_ITERM2
                             NCPIXEL_KITTY_STATIC
                             NCPIXEL_KITTY_ANIMATED
                             NCPIXEL_KITTY_SELFREF
                             NCBLIT_PIXEL
                             NCBLIT_2x2
                             NCSCALE_SCALE)
               #:autoload
               (gcrypt base64)
               (base64-encode)
               #:export
               (;; Syntax highlighting syntax-tag->theme-role
                highlight-scheme-line
                 highlight-scheme-text
                 highlight-shell-line
                 highlight-shell-text
                 highlight-code-line
                 highlight-code-text
                 process-fenced-code-blocks
                 ;; mdflow & Markdown rendering
                 mdflow-binary-path
                 mdflow-available?
                 render-markdown-with-mdflow
                 parse-markdown-blocks
                 segment-markdown-inline
                 segment-input-line
                 render-markdown
                 render-markdown-lines
                 ;; OSC 8 clickable links
                 sanitize-osc-text
                 format-osc8-link
                 file-path->file-uri
                 extract-clickable-links
                 find-link-at-column
                 ;; Nerd Fonts
                 %nerd-fonts-mode
                 set-nerd-fonts-mode!
                 detect-nerd-fonts
                 nerd-fonts-enabled?
                 nerd-icon
                 ;; Kitty / ncvisual image protocol
                 detect-image-protocol
                 kitty-graphics-supported?
                 image-file-path?
                 extract-markdown-image-paths
                 format-kitty-image-escape
                 render-image-ncvisual
                 format-image-placeholder))

;;;
;;; Small String & Segment Helpers
;;;
(define (string-prefix-ci? prefix str)
        (and (string? prefix)
             (string? str)
             (>= (string-length str) (string-length prefix))
             (string-ci=? prefix (substring str 0 (string-length prefix)))))

(define (string-suffix-ci? suffix str)
        (and (string? suffix)
             (string? str)
             (>= (string-length str) (string-length suffix))
             (string-ci=? suffix (substring str (- (string-length str) (string-length suffix))))))

(define (merge-adjacent-segments segs)
        "Coalesce consecutive segments that share the same (role bold? dim?) style."
        (let loop ((rest segs) (acc '()))
                  (if (null? rest)
                    (reverse acc)
                    (let ((cur (car rest)))
                      (if (string-null? (car cur))
                        (loop (cdr rest) acc)
                        (if (and (pair? acc)
                                 (eq? (cadr cur) (cadr (car acc)))
                                 (eq? (caddr cur) (caddr (car acc)))
                                 (eq? (cadddr cur) (cadddr (car acc))))
                          (let ((prev (car acc)))
                            (loop (cdr rest)
                                  (cons (list (string-append (car prev) (car cur))
                                              (cadr prev)
                                              (caddr prev)
                                              (cadddr prev))
                                        (cdr acc))))
                          (loop (cdr rest) (cons cur acc))))))))

(define (slice-segments-to-width segs width)
        "Split a single-line segment list SEGS into multiple lines each <= WIDTH visible chars."
        (if (<= width 0)
          (list segs)
          (let loop ((rest segs)
                     (cur-line '())
                     (cur-len 0)
                     (lines '()))
                    (if (null? rest)
                      (reverse (cons (merge-adjacent-segments (reverse cur-line)) lines))
                      (let* ((seg (car rest))
                             (txt (car seg))
                             (role (cadr seg))
                             (bold? (caddr seg))
                             (dim? (cadddr seg))
                             (tlen (string-length txt))
                             (room (- width cur-len)))
                        (cond
                          ((<= tlen room)
                            (loop (cdr rest) (cons seg cur-line) (+ cur-len tlen) lines))
                          ((> room 0)
                            (let* ((head (substring txt 0 room))
                                   (tail (substring txt room))
                                   (done-line (merge-adjacent-segments
                                                (reverse (cons (list head role bold? dim?) cur-line)))))
                              (loop (cons (list tail role bold? dim?) (cdr rest))
                                    '()
                                    0
                                    (cons done-line lines))))
                          (else
                            (loop rest
                                  '()
                                  0
                                  (cons (merge-adjacent-segments (reverse cur-line)) lines)))))))))

(define (wrap-plain-line line width)
        "Wrap a plain string LINE to WIDTH columns on word boundaries when possible."
        (let ((s (or line "")))
          (if (or (<= width 0) (<= (string-length s) width))
            (list s)
            (let loop ((words (string-split s #\space)) (cur "") (acc '()))
                      (if (null? words)
                        (reverse (if (string-null? cur) acc (cons cur acc)))
                        (let* ((w (car words))
                               (cand (if (string-null? cur) w (string-append cur " " w))))
                          (cond
                            ((<= (string-length cand) width)
                              (loop (cdr words) cand acc))
                            ((string-null? cur)
                              (loop (cons (substring w width) (cdr words))
                                    ""
                                    (cons (substring w 0 width) acc)))
                            (else
                              (loop words "" (cons cur acc))))))))))

(define (split-segment-words seg)
        "Split a single styled segment SEG into word/space sub-segments preserving style."
        (let* ((txt (car seg))
               (role (cadr seg))
               (bold? (caddr seg))
               (dim? (cadddr seg))
               (len (string-length txt)))
          (let loop ((i 0) (acc '()))
                    (if (>= i len)
                      (reverse acc)
                      (let ((sp? (char=? (string-ref txt i) #\space)))
                        (let scan ((j (+ i 1)))
                                  (if (and (< j len) (eq? sp? (char=? (string-ref txt j) #\space)))
                                    (scan (+ j 1))
                                    (loop j (cons (list (substring txt i j) role bold? dim?) acc)))))))))

(define (wrap-segments-word-wise segs width)
        "Word-wrap styled segments SEGS to WIDTH visible columns."
        (if (<= width 0)
          (list (merge-adjacent-segments segs))
          (let ((atoms (append-map split-segment-words segs)))
            (let loop ((rest atoms) (cur-line '()) (cur-len 0) (lines '()))
                      (if (null? rest)
                        (reverse (cons (merge-adjacent-segments (reverse cur-line)) lines))
                        (let* ((atom (car rest))
                               (txt (car atom))
                               (role (cadr atom))
                               (bold? (caddr atom))
                               (dim? (cadddr atom))
                               (alen (string-length txt)))
                          (cond
                            ((and (= cur-len 0) (string=? (string-trim txt) ""))
                              ;; Skip leading spaces at start of wrapped continuation line
                              (loop (cdr rest) cur-line 0 lines))
                            ((<= (+ cur-len alen) width)
                              (loop (cdr rest) (cons atom cur-line) (+ cur-len alen) lines))
                            ((= cur-len 0)
                              ;; Single token longer than width: hard-slice
                              (loop (cons (list (substring txt width) role bold? dim?) (cdr rest))
                                    '()
                                    0
                                    (cons (list (list (substring txt 0 width) role bold? dim?)) lines)))
                            (else
                              ;; Start a new line (skip trailing space atom if any)
                              (loop (if (string=? (string-trim txt) "") (cdr rest) rest)
                                    '()
                                    0
                                    (cons (merge-adjacent-segments (reverse cur-line)) lines))))))))))

(define (segments->plain-text segs)
        "Concatenate the text strings from segment list SEGS."
        (string-concatenate (map car segs)))

;;;
;;; Syntax Highlighting (Scheme, C, Lisp, Shell, Generic)
;;;
(define (syntax-tag->theme-role tag)
        "Map a (syntax-highlight) token TAG symbol to a badb theme role."
        (cond
          ((memq tag '(comment)) 'dim)
          ((memq tag '(string)) 'accent)
          ((memq tag '(special keyword)) 'scheme)
          ((memq tag '(symbol)) 'default)
          ((memq tag '(number constant)) 'notice)
          (else 'default)))

(define (highlights->segments highlights)
        (let loop ((hls highlights) (acc '()))
                  (if (null? hls)
                    (merge-adjacent-segments (reverse acc))
                    (let* ((h (car hls))
                           (text (cond
                                   ((string? h) h)
                                   ((and (pair? h) (string? (last h))) (last h))
                                   (else "")))
                           (tag (if (pair? h) (car h) #f))
                           (role (if tag (syntax-tag->theme-role tag) 'default)))
                      (loop (cdr hls)
                            (cons (list text role (eq? tag 'special) (eq? tag 'comment)) acc))))))

(define (get-syntax-lexer lang)
        (let ((l (string-downcase (string-trim-both (or lang "")))))
          (cond
            ((member l '("scheme" "scm" "guile")) lex-scheme)
            ((member l '("lisp" "elisp" "cl" "common-lisp")) lex-lisp)
            ((member l '("c" "h" "cpp" "c++")) lex-c)
            (else #f))))

(define (highlight-scheme-line line)
        "Syntax-highlight a single Scheme LINE into styled segments."
        (let ((s (or line "")))
          (if (string-null? s)
            (list (list "" 'default #f #f))
            (catch #t
              (lambda ()
                (let ((hls (highlight lex-scheme s)))
                  (if (pair? hls)
                    (highlights->segments hls)
                    (list (list s 'scheme #f #f)))))
              (lambda _
                (list (list s 'scheme #f #f)))))))

(define (highlight-scheme-text text)
        "Syntax-highlight multi-line Scheme TEXT, returning one segment list per line."
        (map highlight-scheme-line (string-split (or text "") #\newline)))

(define %shell-builtins
        '("sudo" "git" "cd" "export" "set" "unset" "echo" "printf" "grep" "rg"
          "fd" "find" "ls" "cat" "head" "tail" "sed" "awk" "guix" "guile"
          "bd" "make" "cargo" "npm" "uv" "python" "python3" "pytest" "rm" "mv"
          "cp" "mkdir" "chmod" "chown" "curl" "ssh" "scp" "docker" "nu" "fish"))

(define (shell-word-role word first-word?)
        (cond
          ((string-prefix? "$" word) (values 'notice #f #f))
          ((or (string-prefix? "--" word)
               (and (> (string-length word) 1)
                    (char=? (string-ref word 0) #\-)))
            (values 'dim #f #t))
          ((member word '("|" "||" "&&" ";" ">" ">>" "<" "<<" "2>&1" "&"))
            (values 'warning #t #f))
          ((or first-word? (member word %shell-builtins))
            (values 'shell #t #f))
          (else (values 'default #f #f))))

(define (highlight-shell-line line)
        "Syntax-highlight a single shell command LINE into styled segments."
        (let* ((s (or line ""))
               (len (string-length s)))
          (if (= len 0)
            (list (list "" 'default #f #f))
            (let loop ((i 0) (first-word? #t) (acc '()))
                      (if (>= i len)
                        (merge-adjacent-segments (reverse acc))
                        (let ((ch (string-ref s i)))
                          (cond
                            ;; Whitespace
                            ((char-whitespace? ch)
                              (let ws ((j (+ i 1)))
                                      (if (and (< j len) (char-whitespace? (string-ref s j)))
                                        (ws (+ j 1))
                                        (loop j first-word? (cons (list (substring s i j) 'default #f #f) acc)))))
                            ;; Comment to end of line
                            ((char=? ch #\#)
                              (loop len #f (cons (list (substring s i len) 'dim #f #t) acc)))
                            ;; Single or double quoted string
                            ((or (char=? ch #\") (char=? ch #\'))
                              (let q ((j (+ i 1)) (escaped? #f))
                                     (cond
                                       ((>= j len)
                                         (loop len #f (cons (list (substring s i len) 'accent #f #f) acc)))
                                       (escaped?
                                         (q (+ j 1) #f))
                                       ((and (char=? ch #\") (char=? (string-ref s j) #\\))
                                         (q (+ j 1) #t))
                                       ((char=? (string-ref s j) ch)
                                         (loop (+ j 1) #f (cons (list (substring s i (+ j 1)) 'accent #f #f) acc)))
                                       (else (q (+ j 1) #f)))))
                            ;; Pipe / semicolon / redirect operators
                            ((memv ch '(#\| #\& #\; #\> #\<))
                              (let op ((j (+ i 1)))
                                      (if (and (< j len) (memv (string-ref s j) '(#\| #\& #\> #\<)))
                                        (op (+ j 1))
                                        (loop j #t (cons (list (substring s i j) 'warning #t #f) acc)))))
                            ;; Word / flag / variable / command
                            (else
                              (let wd ((j (+ i 1)))
                                      (if (and (< j len)
                                               (not (char-whitespace? (string-ref s j)))
                                               (not (memv (string-ref s j) '(#\| #\& #\; #\> #\< #\# #\" #\'))))
                                        (wd (+ j 1))
                                        (let ((word (substring s i j)))
                                          (call-with-values
                                            (lambda () (shell-word-role word first-word?))
                                            (lambda (role bold? dim?)
                                              (let ((next-first?
                                                      (and first-word? (string=? word "sudo"))))
                                                (loop j next-first? (cons (list word role bold? dim?) acc))))))))))))))))

(define (highlight-shell-text text)
        "Syntax-highlight multi-line shell TEXT, returning one segment list per line."
        (map highlight-shell-line (string-split (or text "") #\newline)))

(define %generic-keywords
        '("def" "fn" "func" "function" "return" "if" "else" "elif" "for" "while"
          "match" "case" "import" "from" "export" "const" "let" "var" "pub"
          "struct" "enum" "impl" "trait" "class" "async" "await" "true" "false"
          "null" "nil" "none" "True" "False" "None"))

(define (highlight-generic-code-line line)
        "Fallback syntax highlighter for languages without a dedicated lexer."
        (let* ((s (or line ""))
               (len (string-length s)))
          (if (= len 0)
            (list (list "" 'default #f #f))
            (let ((trimmed (string-trim s)))
              (cond
                ((or (string-prefix? "//" trimmed)
                     (string-prefix? "#" trimmed)
                     (string-prefix? "--" trimmed))
                  (list (list s 'dim #f #t)))
                (else
                  (let loop ((i 0) (acc '()))
                            (if (>= i len)
                              (merge-adjacent-segments (reverse acc))
                              (let ((ch (string-ref s i)))
                                (cond
                                  ((or (char=? ch #\") (char=? ch #\'))
                                    (let q ((j (+ i 1)) (esc? #f))
                                           (cond
                                             ((>= j len) (loop len (cons (list (substring s i len) 'accent #f #f) acc)))
                                             (esc? (q (+ j 1) #f))
                                             ((char=? (string-ref s j) #\\) (q (+ j 1) #t))
                                             ((char=? (string-ref s j) ch)
                                               (loop (+ j 1) (cons (list (substring s i (+ j 1)) 'accent #f #f) acc)))
                                             (else (q (+ j 1) #f)))))
                                  ((or (char-alphabetic? ch) (char=? ch #\_))
                                    (let wd ((j (+ i 1)))
                                            (if (and (< j len)
                                                     (let ((c (string-ref s j)))
                                                       (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_))))
                                              (wd (+ j 1))
                                              (let ((word (substring s i j)))
                                                (if (member word %generic-keywords)
                                                  (loop j (cons (list word 'scheme #t #f) acc))
                                                  (loop j (cons (list word 'default #f #f) acc)))))))
                                  ((char-numeric? ch)
                                    (let num ((j (+ i 1)))
                                             (if (and (< j len)
                                                      (let ((c (string-ref s j)))
                                                        (or (char-numeric? c) (char=? c #\.))))
                                               (num (+ j 1))
                                               (loop j (cons (list (substring s i j) 'notice #f #f) acc)))))
                                  (else
                                    (loop (+ i 1) (cons (list (string ch) 'default #f #f) acc)))))))))))))

(define (highlight-code-line line lang)
        "Syntax-highlight a single code LINE in language LANG into styled segments."
        (let* ((s (or line ""))
               (l (string-downcase (string-trim-both (or lang ""))))
               (lexer (get-syntax-lexer l)))
          (cond
            ((member l '("sh" "bash" "zsh" "shell" "nu" "nushell" "fish"))
              (highlight-shell-line s))
            (lexer
              (catch #t
                (lambda ()
                  (let ((hls (highlight lexer s)))
                    (if (pair? hls)
                      (highlights->segments hls)
                      (list (list s 'default #f #f)))))
                (lambda _ (highlight-generic-code-line s))))
            (else (highlight-generic-code-line s)))))

(define (highlight-code-text text lang)
        "Syntax-highlight multi-line code TEXT in language LANG."
        (map (lambda (line) (highlight-code-line line lang))
             (string-split (or text "") #\newline)))

(define (process-fenced-code-blocks text)
        "Mark fenced code blocks in TEXT with [CODE:role]...[/CODE] tags for
segment-line rendering; degrade gracefully to raw TEXT on error."
        (if (or (not (string? text)) (not (string-contains text "```")))
          text
          (catch #t
            (lambda ()
              (let loop ((lines (string-split text #\newline))
                         (in-code? #f)
                         (lang "")
                         (result '()))
                        (if (null? lines)
                          (string-join (reverse result) "\n")
                          (let* ((line (car lines))
                                 (trimmed (string-trim-both line)))
                            (cond
                              ((and (not in-code?) (string-prefix? "```" trimmed))
                                (let* ((rest (string-trim-both (substring trimmed 3)))
                                       (sp (string-index rest #\space))
                                       (blk-lang (if sp (substring rest 0 sp) rest)))
                                  (loop (cdr lines) #t blk-lang (cons (format #f "```~a" blk-lang) result))))
                              ((and in-code? (string-prefix? "```" trimmed))
                                (loop (cdr lines) #f "" (cons "```" result)))
                              (in-code?
                                (let* ((segs (highlight-code-line line lang))
                                       (tagged (string-join
                                                 (map (lambda (seg)
                                                        (format #f "[CODE:~a]~a[/CODE]" (cadr seg) (car seg)))
                                                      segs)
                                                 "")))
                                  (loop (cdr lines) in-code? lang (cons tagged result))))
                              (else
                                (loop (cdr lines) in-code? lang (cons line result))))))))
            (lambda _ text))))

;;;
;;; mdflow External Wrapper + Pure-Scheme Fallback (ui-wp2.1)
;;;
(define*
  (mdflow-binary-path #:optional (override #f))
  "Locate the mdflow binary via OVERRIDE, BADB_MDFLOW_BIN, or PATH; return #f if absent."
  (cond
    ((and (string? override) (not (string-null? override)))
      (and (executable-file? override) override))
    ((and (getenv "BADB_MDFLOW_BIN") (not (string-null? (getenv "BADB_MDFLOW_BIN"))))
      (let ((env-bin (getenv "BADB_MDFLOW_BIN")))
        (and (executable-file? env-bin) env-bin)))
    (else (find-in-path "mdflow"))))

(define* (mdflow-available? #:optional (override #f))
         "Return #t when an executable mdflow binary can be resolved."
         (and (mdflow-binary-path override) #t))

(define %ansi-csi-rx (make-regexp "\x1b\\[[0-9;?]*[A-Za-z]"))

(define (strip-ansi-escapes str)
        (regexp-substitute/global #f %ansi-csi-rx (or str "") 'pre "" 'post))

(define* (render-markdown-with-mdflow text #:key (width 80) (bin #f))
         "Invoke external mdflow binary on TEXT (via temp file, no shell interpolation).
Returns a list of plain lines on success, or #f if mdflow is unavailable or fails."
         (let ((exe (mdflow-binary-path bin)))
           (and exe
                (string? text)
                (catch #t
                  (lambda ()
                    (let* ((tmpl (string-copy "/tmp/badb-mdflow-XXXXXX"))
                           (port (mkstemp! tmpl)))
                      (dynamic-wind
                        (lambda () #t)
                        (lambda ()
                          (set-port-encoding! port "UTF-8")
                          (display text port)
                          (close-port port)
                          (let* ((pipe (open-pipe* OPEN_READ
                                                   exe
                                                   "--width"
                                                   (number->string (max 10 width))
                                                   tmpl))
                                 (lines (let read-loop ((acc '()))
                                                       (let ((ln (read-line pipe)))
                                                         (if (eof-object? ln)
                                                           (reverse acc)
                                                           (read-loop (cons (strip-ansi-escapes ln) acc))))))
                                 (status (close-pipe pipe)))
                            (and (zero? (status:exit-val status)) lines)))
                        (lambda ()
                          (catch #t (lambda () (unless (port-closed? port) (close-port port))) (lambda _ #f))
                          (catch #t (lambda () (when (file-exists? tmpl) (delete-file tmpl))) (lambda _ #f))))))
                  (lambda _ #f)))))

;;;
;;; Pure-Scheme Markdown Block & Inline Parser
;;;
(define %heading-rx (make-regexp "^(#{1,6})[ \t]+(.*)$"))
(define %ordered-list-rx (make-regexp "^([ \t]*)([0-9]+[.)])[ \t]+(.*)$"))
(define %unordered-list-rx (make-regexp "^([ \t]*)([-*+])[ \t]+(.*)$"))
(define %blockquote-rx (make-regexp "^[ \t]*>[ \t]?(.*)$"))
(define %hr-rx (make-regexp "^[ \t]*([-*_])([ \t]*\\1){2,}[ \t]*$"))

(define (fence-delimiter trimmed)
        (cond
          ((string-prefix? "```" trimmed) (cons "```" (string-trim-both (substring trimmed 3))))
          ((string-prefix? "~~~" trimmed) (cons "~~~" (string-trim-both (substring trimmed 3))))
          (else #f)))

(define (parse-markdown-blocks text)
        "Parse Markdown TEXT into a list of typed block descriptors:
  (heading level title)
  (code-block lang lines)
  (blockquote lines)
  (list-item indent marker content)
  (hr)
  (paragraph lines)
  (blank)"
        (let
            ((raw-lines (string-split (or text "") #\newline)))
          (let loop
            ((lines raw-lines) (blocks '())
             (para-acc '())
             (quote-acc '())
             (in-fence #f)
             (fence-lang "")
             (fence-lines '()))
            (define (flush-para b)
                    (if (null? para-acc)
                      b
                      (cons (list 'paragraph (reverse para-acc)) b)))
            (define (flush-quote b)
                    (if (null? quote-acc)
                      b
                      (cons (list 'blockquote (reverse quote-acc)) b)))
            (define (flush-all b) (set! b (flush-para b))
                    (set! para-acc '())
                    (set! b (flush-quote b))
                    (set! quote-acc '())
                    b)
            (if (null? lines)
              (let
                  ((b
                     (if in-fence
                       (cons (list 'code-block fence-lang (reverse fence-lines)) blocks)
                       (flush-all blocks))))
                (reverse b))
              (let*
                  ((line (car lines)) (trimmed (string-trim-both line)))
                (if in-fence
                  (if (string-prefix? in-fence trimmed)
                    (loop (cdr lines)
                          (cons (list 'code-block fence-lang (reverse fence-lines)) blocks) '() '() #f ""
                          '())
                    (loop (cdr lines) blocks '() '() in-fence fence-lang (cons line fence-lines)))
                  (let ((fence (fence-delimiter trimmed)))
                    (cond
                      (fence
                        (let*
                            ((b (flush-all blocks)) (lang-str (cdr fence))
                             (sp (string-index lang-str #\space)))
                          (loop (cdr lines) b '() '() (car fence)
                                (if sp
                                  (substring lang-str 0 sp)
                                  lang-str) '())))
                      ((string-null? trimmed)
                        (let ((b (flush-all blocks)))
                          (loop (cdr lines)
                                (if (and (pair? b) (eq? (caar b) 'blank))
                                  b
                                  (cons (list 'blank) b)) '() '() #f
                                "" '())))
                      ((regexp-exec %hr-rx line)
                        (let ((b (flush-all blocks)))
                          (loop (cdr lines) (cons (list 'hr) b) '() '() #f "" '())))
                      ((regexp-exec %heading-rx trimmed) =>
                                                         (lambda (m)
                                                           (let
                                                               ((b (flush-all blocks))
                                                                (lvl (string-length (match:substring m 1)))
                                                                (title (string-trim-both (match:substring m 2))))
                                                             (loop (cdr lines) (cons (list 'heading lvl title) b) '() '() #f "" '()))))
                      ((regexp-exec %blockquote-rx line) =>
                                                         (lambda (m)
                                                           (let ((b (flush-para blocks)))
                                                             (set! para-acc '())
                                                             (set! quote-acc (cons (match:substring m 1) quote-acc))
                                                             (loop (cdr lines) b '() quote-acc #f "" '()))))
                      ((regexp-exec %unordered-list-rx line) =>
                                                             (lambda (m)
                                                               (let
                                                                   ((b (flush-all blocks))
                                                                    (indent (string-length (match:substring m 1)))
                                                                    (body (match:substring m 3)))
                                                                 (loop (cdr lines) (cons (list 'list-item indent 'bullet body) b) '() '() #f ""
                                                                       '()))))
                      ((regexp-exec %ordered-list-rx line) =>
                                                           (lambda (m)
                                                             (let
                                                                 ((b (flush-all blocks))
                                                                  (indent (string-length (match:substring m 1)))
                                                                  (marker (match:substring m 2))
                                                                  (body (match:substring m 3)))
                                                               (loop (cdr lines) (cons (list 'list-item indent marker body) b) '() '() #f ""
                                                                     '()))))
                      (else
                        (let ((b (flush-quote blocks)))
                          (set! quote-acc '())
                          (loop (cdr lines) b (cons line para-acc) '() #f "" '())))))))))))

(define (count-run str pos ch)
        (let ((len (string-length str)))
          (let loop ((i pos))
                    (if (and (< i len) (char=? (string-ref str i) ch))
                      (loop (+ i 1))
                      (- i pos)))))

(define (find-closing-backticks str start run-len)
        (let ((len (string-length str))
              (needle (make-string run-len #\`)))
          (let loop ((i start))
                    (let ((idx (string-contains str needle i)))
                      (cond
                        ((not idx) #f)
                        ((and (> idx 0) (char=? (string-ref str (- idx 1)) #\`))
                          (loop (+ idx 1)))
                        ((and (< (+ idx run-len) len) (char=? (string-ref str (+ idx run-len)) #\`))
                          (loop (+ idx run-len)))
                        (else idx))))))

(define (find-closing-delim str start delim)
        (let ((dlen (string-length delim)))
          (let loop ((i start))
                    (let ((idx (string-contains str delim i)))
                      (cond
                        ((not idx) #f)
                        ((= idx start) (loop (+ idx 1)))
                        (else idx))))))

(define (parse-md-link str pos)
        "If STR at POS starts a Markdown link [label](target), return (values label target end-pos); else (values #f #f #f)."
        (let ((len (string-length str)))
          (if (or (>= pos len) (not (char=? (string-ref str pos) #\[)))
            (values #f #f #f)
            (let ((close-br (string-index str #\] (+ pos 1))))
              (if (and close-br
                       (< (+ close-br 1) len)
                       (char=? (string-ref str (+ close-br 1)) #\())
                (let ((close-par (string-index str #\) (+ close-br 2))))
                  (if close-par
                    (values (substring str (+ pos 1) close-br)
                            (string-trim-both (substring str (+ close-br 2) close-par))
                            (+ close-par 1))
                    (values #f #f #f)))
                (values #f #f #f))))))

(define* (segment-markdown-inline text #:key (base-role 'default) (base-bold? #f) (base-dim? #f))
         "Parse inline Markdown formatting (backticks, **bold**, *italic*, ~~strike~~,
[label](url)) into styled segments ((str role bold? dim?) ...)."
         (let* ((s (or text ""))
                (len (string-length s)))
           (let loop ((i 0) (acc '()))
                     (if (>= i len)
                       (merge-adjacent-segments (reverse acc))
                       (let ((ch (string-ref s i)))
                         (cond
                           ;; Inline backtick span (`...` or ``...``)
                           ((char=? ch #\`)
                             (let* ((run (count-run s i #\`))
                                    (end (find-closing-backticks s (+ i run) run)))
                               (if end
                                 (let ((inner (string-trim-both (substring s (+ i run) end))))
                                   (loop (+ end run)
                                         (cons (list (if (string-null? inner) (substring s (+ i run) end) inner)
                                                     'accent
                                                     #f
                                                     #f)
                                               acc)))
                                 (loop (+ i run)
                                       (cons (list (substring s i (+ i run)) base-role base-bold? base-dim?) acc)))))
                           ;; Markdown link [label](url)
                           ((char=? ch #\[)
                             (call-with-values
                               (lambda () (parse-md-link s i))
                               (lambda (label target end-pos)
                                 (if end-pos
                                   (loop end-pos
                                         (cons (list label 'accent #t #f) acc))
                                   (loop (+ i 1)
                                         (cons (list "[" base-role base-bold? base-dim?) acc))))))
                           ;; Bold **...** or __...__
                           ((and (< (+ i 1) len)
                                 (or (and (char=? ch #\*) (char=? (string-ref s (+ i 1)) #\*))
                                     (and (char=? ch #\_) (char=? (string-ref s (+ i 1)) #\_))))
                             (let* ((delim (substring s i (+ i 2)))
                                    (end (find-closing-delim s (+ i 2) delim)))
                               (if end
                                 (loop (+ end 2)
                                       (cons (list (substring s (+ i 2) end) base-role #t base-dim?) acc))
                                 (loop (+ i 2)
                                       (cons (list delim base-role base-bold? base-dim?) acc)))))
                           ;; Strikethrough ~~...~~
                           ((and (< (+ i 1) len)
                                 (char=? ch #\~)
                                 (char=? (string-ref s (+ i 1)) #\~))
                             (let ((end (find-closing-delim s (+ i 2) "~~")))
                               (if end
                                 (loop (+ end 2)
                                       (cons (list (substring s (+ i 2) end) 'dim #f #t) acc))
                                 (loop (+ i 2)
                                       (cons (list "~~" base-role base-bold? base-dim?) acc)))))
                           ;; Italic *...* (require non-space after opening *)
                           ((and (char=? ch #\*)
                                 (< (+ i 1) len)
                                 (not (char-whitespace? (string-ref s (+ i 1)))))
                             (let ((end (find-closing-delim s (+ i 1) "*")))
                               (if (and end (not (char-whitespace? (string-ref s (- end 1)))))
                                 (loop (+ end 1)
                                       (cons (list (substring s (+ i 1) end) 'dim #f #t) acc))
                                 (loop (+ i 1)
                                       (cons (list "*" base-role base-bold? base-dim?) acc)))))
                           (else
                             (let scan ((j (+ i 1)))
                                       (if (or (>= j len)
                                               (memv (string-ref s j) '(#\` #\[ #\* #\_ #\~)))
                                         (loop j (cons (list (substring s i j) base-role base-bold? base-dim?) acc))
                                         (scan (+ j 1)))))))))))

(define* (segment-input-line line #:key (prompt-mode 'chat))
         "Real-time syntax and Markdown segmenter for the interactive input buffer.
Invariant: concatenating all segment strings ALWAYS equals LINE verbatim so
cursor indices remain exact."
         (let ((s (or line "")))
           (cond
             ((string-null? s)
               (list (list "" 'default #f #f)))
             ((or (eq? prompt-mode 'shell) (string-prefix? "!" s))
               (if (string-prefix? "!" s)
                 (cons (list "!" 'shell #t #f) (highlight-shell-line (substring s 1)))
                 (highlight-shell-line s)))
             ((or (eq? prompt-mode 'scheme)
                  (string-prefix? "(" s)
                  (string-prefix? "," s))
               (if (string-prefix? "," s)
                 (cons (list "," 'scheme #t #f) (highlight-scheme-line (substring s 1)))
                 (highlight-scheme-line s)))
             ((or (eq? prompt-mode 'slash) (string-prefix? "/" s))
               (let ((sp (string-index s #\space)))
                 (if sp
                   (list (list (substring s 0 sp) 'accent #t #f)
                         (list (substring s sp) 'default #f #f))
                   (list (list s 'accent #t #f)))))
             (else
               ;; Chat input: preserve every character verbatim while highlighting
               ;; inline `backticks`, **bold**, @mentions, and URLs.
               (let ((len (string-length s)))
                 (let loop ((i 0) (acc '()))
                           (if (>= i len)
                             (merge-adjacent-segments (reverse acc))
                             (let ((ch (string-ref s i)))
                               (cond
                                 ((char=? ch #\`)
                                   (let* ((run (count-run s i #\`))
                                          (end (find-closing-backticks s (+ i run) run)))
                                     (if end
                                       (loop (+ end run)
                                             (cons (list (substring s i (+ end run)) 'accent #f #f) acc))
                                       (loop (+ i run)
                                             (cons (list (substring s i (+ i run)) 'accent #f #f) acc)))))
                                 ((and (< (+ i 1) len)
                                       (char=? ch #\*)
                                       (char=? (string-ref s (+ i 1)) #\*))
                                   (let ((end (find-closing-delim s (+ i 2) "**")))
                                     (if end
                                       (loop (+ end 2)
                                             (cons (list (substring s i (+ end 2)) 'default #t #f) acc))
                                       (loop (+ i 2)
                                             (cons (list "**" 'default #f #f) acc)))))
                                 ((and (char=? ch #\@)
                                       (or (= i 0) (char-whitespace? (string-ref s (- i 1)))))
                                   (let wd ((j (+ i 1)))
                                           (if (and (< j len) (not (char-whitespace? (string-ref s j))))
                                             (wd (+ j 1))
                                             (loop j (cons (list (substring s i j) 'notice #t #f) acc)))))
                                 ((or (string-prefix? "https://" (substring s i))
                                      (string-prefix? "http://" (substring s i)))
                                   (let wd ((j (+ i 1)))
                                           (if (and (< j len) (not (char-whitespace? (string-ref s j))))
                                             (wd (+ j 1))
                                             (loop j (cons (list (substring s i j) 'notice #f #f) acc)))))
                                 (else
                                   (loop (+ i 1) (cons (list (string ch) 'default #f #f) acc))))))))))))

(define* (wrap-styled-inline text width prefix-segs #:key (base-role 'default) (base-bold? #f) (base-dim? #f))
         "Wrap TEXT to WIDTH with PREFIX-SEGS on the first line and matching indent on continuations."
         (let* ((prefix-w (string-length (segments->plain-text prefix-segs)))
                (body-w (max 8 (- width prefix-w)))
                (body-segs (segment-markdown-inline text
                                                    #:base-role base-role
                                                    #:base-bold? base-bold?
                                                    #:base-dim? base-dim?))
                (wrapped-lines (wrap-segments-word-wise body-segs body-w))
                (indent-str (make-string prefix-w #\space)))
           (let loop ((lines wrapped-lines) (first? #t) (acc '()))
                     (if (null? lines)
                       (reverse acc)
                       (let* ((lead (if first?
                                      prefix-segs
                                      (if (> prefix-w 0)
                                        (list (list indent-str 'default #f #f))
                                        '())))
                              (full-segs (merge-adjacent-segments (append lead (car lines)))))
                         (loop (cdr lines)
                               #f
                               (cons (cons (segments->plain-text full-segs) full-segs) acc)))))))

(define (render-code-block-rows lang code-lines width nerd-fonts?)
        (let* ((safe-lang (if (string-null? lang) "code" lang))
               (icon (nerd-icon 'file nerd-fonts?))
               (header-label (format #f "╭─ ~a ~a " icon safe-lang))
               (rule-n (max 2 (- width (string-length header-label))))
               (header-str (string-append header-label (make-string rule-n #\─)))
               (header-row (cons header-str (list (list header-str 'dim #f #t))))
               (footer-str (string-append "╰" (make-string (max 4 (- width 1)) #\─)))
               (footer-row (cons footer-str (list (list footer-str 'dim #f #t))))
               (gutter (list (list "│ " 'dim #f #t)))
               (inner-w (max 8 (- width 2)))
               (body-rows
                 (append-map
                   (lambda (ln)
                     (let* ((code-segs (highlight-code-line ln lang))
                            (wrapped (slice-segments-to-width code-segs inner-w)))
                       (map (lambda (wline)
                              (let ((full (merge-adjacent-segments (append gutter wline))))
                                (cons (segments->plain-text full) full)))
                            wrapped)))
                   code-lines)))
          (append (list header-row) body-rows (list footer-row))))

(define* (render-markdown-scheme text #:key (width 80) (nerd-fonts? (nerd-fonts-enabled?)))
         "Pure-Scheme Markdown renderer returning ((plain-line . segments) ...)."
         (let ((blocks (parse-markdown-blocks text))
               (safe-w (max 12 width)))
           (append-map
             (lambda (blk)
               (case (car blk)
                 ((blank)
                   (list (cons "" (list (list "" 'default #f #f)))))
                 ((hr)
                   (let ((rule (make-string (min safe-w 48) #\─)))
                     (list (cons rule (list (list rule 'dim #f #t))))))
                 ((heading)
                   (let* ((lvl (cadr blk))
                          (title (caddr blk))
                          (prefix (if (<= lvl 2) "▌ " (string-append (make-string lvl #\#) " "))))
                     (wrap-styled-inline title safe-w
                                         (list (list prefix 'accent #t #f))
                                         #:base-role 'accent
                                         #:base-bold? #t)))
                 ((code-block)
                   (render-code-block-rows (cadr blk) (caddr blk) safe-w nerd-fonts?))
                 ((blockquote)
                   (append-map
                     (lambda (qline)
                       (wrap-styled-inline qline safe-w
                                           (list (list "│ " 'dim #f #t))
                                           #:base-role 'dim
                                           #:base-dim? #t))
                     (cadr blk)))
                 ((list-item)
                   (let* ((indent (min 8 (cadr blk)))
                          (marker (caddr blk))
                          (body (cadddr blk))
                          (pad (make-string indent #\space))
                          (trimmed-body (string-trim body))
                          (task-checked? (string-prefix-ci? "[x] " trimmed-body))
                          (task-open? (string-prefix? "[ ] " trimmed-body))
                          (clean-body
                            (cond
                              ((or task-checked? task-open?) (substring trimmed-body 4))
                              (else body)))
                          (glyph
                            (cond
                              (task-checked? (string-append (nerd-icon 'check nerd-fonts?) " "))
                              (task-open? "☐ ")
                              ((eq? marker 'bullet) "• ")
                              (else (string-append marker " ")))))
                     (wrap-styled-inline clean-body safe-w
                                         (list (list (string-append pad glyph) 'accent #t #f)))))
                 ((paragraph)
                   (let ((joined (string-join (cadr blk) " ")))
                     (wrap-styled-inline joined safe-w '())))
                 (else '())))
             blocks)))

(define* (render-markdown text #:key (width 80) (use-mdflow? #t) (nerd-fonts? (nerd-fonts-enabled?)))
         "Render Markdown TEXT into ((plain-line . segments) ...) wrapped to WIDTH.
Uses mdflow when available and USE-MDFLOW? is true; otherwise falls back to the
pure-Scheme Markdown renderer."
         (let ((mdflow-lines (and use-mdflow?
                                  (mdflow-available?)
                                  (render-markdown-with-mdflow text #:width width))))
           (if mdflow-lines
             (map (lambda (ln)
                    (let ((segs (segment-markdown-inline ln)))
                      (cons ln segs)))
                  mdflow-lines)
             (render-markdown-scheme text #:width width #:nerd-fonts? nerd-fonts?))))

(define* (render-markdown-lines text #:key (width 80) (use-mdflow? #t) (nerd-fonts? (nerd-fonts-enabled?)))
         "Render Markdown TEXT into a list of plain wrapped line strings."
         (map car (render-markdown text #:width width #:use-mdflow? use-mdflow? #:nerd-fonts? nerd-fonts?)))

;;;
;;; OSC 8 Clickable Links & Target Extraction (ui-wp2.5)
;;;
(define (sanitize-osc-text str)
        "Strip terminal escape sequences (CSI/OSC) and control characters (ESC, BEL,
C0/DEL) from STR so untrusted strings cannot break out of OSC 0/2/8 sequences."
        (if (string? str)
          (let ((no-csi (strip-ansi-escapes str)))
            (list->string
              (filter
                (lambda (c)
                  (let ((code (char->integer c))) (and (>= code 32) (not (= code 127)))))
                (string->list no-csi))))
          ""))

(define* (format-osc8-link url #:optional (label url) (id #f))
         "Wrap LABEL in an OSC 8 terminal hyperlink targeting URL."
         (let* ((safe-url (sanitize-osc-text url))
                (safe-label (sanitize-osc-text (or label url)))
                (params (if (and (string? id) (not (string-null? id)))
                          (string-append "id=" (sanitize-osc-text id))
                          "")))
           (string-append "\x1b]8;" params ";" safe-url "\x07"
                          safe-label
                          "\x1b]8;;\x07")))

(define* (file-path->file-uri path #:optional (cwd (getcwd)))
         "Convert a local file PATH into a file:///abs/path URI."
         (let* ((clean (sanitize-osc-text path))
                (abs (if (string-prefix? "/" clean)
                       clean
                       (string-append (string-trim-right cwd #\/) "/" clean))))
           (string-append "file://" abs)))

(define %md-link-rx (make-regexp "\\[([^]]+)\\]\\(([^)]+)\\)"))
(define %url-rx (make-regexp "(https?://[^ \t\n\r<>\"')\\]]+|file://[^ \t\n\r<>\"')\\]]+)"))
(define %file-path-rx
        (make-regexp "(^|[ \t(\\[`'\"])((~|\\.{1,2})?/[^ \t\n\r<>\"')\\]:]+|[A-Za-z0-9_.-]+(/[A-Za-z0-9_.-]+)+\\.[A-Za-z0-9]+)(:([0-9]+))?"))

(define (trim-url-punctuation url)
        (string-trim-right url (lambda (c) (memv c '(#\. #\, #\; #\: #\? #\!)))))

(define (parse-line-suffix target)
        (let ((colon (string-rindex target #\:)))
          (if colon
            (let ((num (string->number (substring target (+ colon 1)))))
              (if (and num (> num 0))
                (values (substring target 0 colon) num)
                (values target #f)))
            (values target #f))))

(define (overlaps-existing? start end links)
        (any (lambda (lk)
               (let ((s (assoc-ref lk 'start))
                     (e (assoc-ref lk 'end)))
                 (and (< start e) (< s end))))
             links))

(define (extract-clickable-links text)
        "Extract Markdown links, bare HTTP/file URLs, and file paths from TEXT.
Returns a list of link alists sorted by start column."
        (let ((s (or text ""))
              (acc '()))
          ;; 1. Markdown [label](target)
          (let loop ((pos 0))
                    (let ((m (regexp-exec %md-link-rx s pos)))
                      (when m
                        (let* ((start (match:start m 0))
                               (end (match:end m 0))
                               (label (match:substring m 1))
                               (raw-target (string-trim-both (match:substring m 2)))
                               (kind (if (or (string-prefix-ci? "http://" raw-target)
                                             (string-prefix-ci? "https://" raw-target)
                                             (string-prefix-ci? "file://" raw-target))
                                       'url
                                       'file)))
                          (call-with-values
                            (lambda () (parse-line-suffix raw-target))
                            (lambda (target line-no)
                              (set! acc (cons `((label . ,label)
                                                (target . ,target)
                                                (kind . ,kind)
                                                (start . ,start)
                                                (end . ,end)
                                                (line . ,line-no))
                                              acc))))
                          (loop end)))))
          ;; 2. Bare URLs
          (let loop ((pos 0))
                    (let ((m (regexp-exec %url-rx s pos)))
                      (when m
                        (let* ((start (match:start m 1))
                               (raw-url (match:substring m 1))
                               (url (trim-url-punctuation raw-url))
                               (end (+ start (string-length url))))
                          (unless (overlaps-existing? start end acc)
                            (set! acc (cons `((label . ,url)
                                              (target . ,url)
                                              (kind . url)
                                              (start . ,start)
                                              (end . ,end)
                                              (line . #f))
                                            acc)))
                          (loop (max end (+ pos 1)))))))
          ;; 3. File paths
          (let loop ((pos 0))
                    (let ((m (regexp-exec %file-path-rx s pos)))
                      (when m
                        (let* ((start (match:start m 2))
                               (path-part (match:substring m 2))
                               (line-str (match:substring m 6))
                               (line-no (and line-str (string->number line-str)))
                               (end (if line-str (match:end m 5) (match:end m 2))))
                          (unless (overlaps-existing? start end acc)
                            (set! acc (cons `((label . ,(if line-str
                                                          (string-append path-part ":" line-str)
                                                          path-part))
                                              (target . ,path-part)
                                              (kind . file)
                                              (start . ,start)
                                              (end . ,end)
                                              (line . ,line-no))
                                            acc)))
                          (loop (max (match:end m 0) (+ pos 1)))))))
          (sort acc (lambda (a b) (< (assoc-ref a 'start) (assoc-ref b 'start))))))

(define (find-link-at-column text col)
        "Return the clickable link alist covering column COL in TEXT, or #f."
        (and (integer? col)
             (>= col 0)
             (find (lambda (lk)
                     (and (>= col (assoc-ref lk 'start))
                          (< col (assoc-ref lk 'end))))
                   (extract-clickable-links text))))

;;;
;;; Nerd Font Detection & Icons with Unicode Fallback (ui-wp2.5)
;;;
(define %nerd-fonts-mode (make-parameter 'auto))

(define (set-nerd-fonts-mode! mode)
        "Set Nerd Font mode to 'auto, #t (force enabled), or #f (force Unicode fallback)."
        (when (memq mode '(auto #t #f))
          (%nerd-fonts-mode mode)))

(define %detected-nerd-fonts-cache 'unset)

(define (parse-bool-env val)
        (and (string? val)
             (let ((v (string-downcase (string-trim-both val))))
               (cond
                 ((member v '("1" "true" "yes" "on")) 'true)
                 ((member v '("0" "false" "no" "off")) 'false)
                 (else #f)))))

(define (fc-list-has-nerd-font?)
        (catch #t
          (lambda ()
            (let* ((pipe (open-pipe* OPEN_READ "fc-list" ":family"))
                   (found? (let loop ()
                                     (let ((ln (read-line pipe)))
                                       (cond
                                         ((eof-object? ln) #f)
                                         ((or (string-contains-ci ln "nerd")
                                              (string-contains-ci ln "nf ")
                                              (string-contains-ci ln "nfm"))
                                           #t)
                                         (else (loop)))))))
              (close-pipe pipe)
              found?))
          (lambda _ #f)))

(define (detect-nerd-fonts)
        "Auto-detect whether Nerd Font glyphs are supported in the current terminal/host."
        (let ((env-flag (parse-bool-env (getenv "BADB_NERD_FONTS")))
              (term (or (getenv "TERM") ""))
              (term-prog (string-downcase (or (getenv "TERM_PROGRAM") ""))))
          (cond
            ((eq? env-flag 'true) #t)
            ((eq? env-flag 'false) #f)
            ((string-ci=? term "dumb") #f)
            ((member term-prog '("ghostty" "kitty" "wezterm" "alacritty")) #t)
            ((getenv "KITTY_WINDOW_ID") #t)
            ((getenv "GHOSTTY_RESOURCES_DIR") #t)
            (else
              (when (eq? %detected-nerd-fonts-cache 'unset)
                (set! %detected-nerd-fonts-cache (fc-list-has-nerd-font?)))
              %detected-nerd-fonts-cache))))

(define* (nerd-fonts-enabled? #:optional (override 'unset))
         "Return #t when Nerd Font icons should be emitted."
         (cond
           ((boolean? override) override)
           ((boolean? (%nerd-fonts-mode)) (%nerd-fonts-mode))
           (else (detect-nerd-fonts))))

(define %icon-table
        '((user        "󰀄" . "●")
          (assistant   "󰚩" . "◆")
          (thinking    "󰟶" . "◇")
          (shell       "" . "$")
          (scheme      "λ" . "λ")
          (tool        "󰒓" . "⚙")
          (tool-result "󰄬" . "✓")
          (error       "󰅚" . "✗")
          (warning     "󰀪" . "⚠")
          (notice      "󰋼" . "ℹ")
          (system      "󰣖" . "•")
          (steer       "󰁔" . "»")
          (folder      "󰉋" . "▸")
          (file        "󰈔" . "·")
          (link        "󰌷" . "↗")
          (image       "󰋩" . "▣")
          (git         "󰊢" . "±")
          (lock        "󰌾" . "#")
          (check       "󰄬" . "✓")
          (cross       "󰅖" . "✗")
          (collapsed   "" . "›")
          (expanded    "" . "⌄")
          (crow        "󰆄" . "❖")))

(define* (nerd-icon name #:optional (nerd? (nerd-fonts-enabled?)))
         "Return the icon glyph for symbol NAME using Nerd Fonts when NERD? is true,
otherwise a clean Unicode fallback."
         (let ((entry (assq name %icon-table)))
           (if entry
             (if nerd? (cadr entry) (cddr entry))
             "•")))

;;;
;;; Kitty Image Protocol & notcurses ncvisual Support (ui-wp2.5)
;;;
(define (pixel-support->symbol code)
        (cond
          ((= code NCPIXEL_KITTY_SELFREF) 'kitty-selfref)
          ((= code NCPIXEL_KITTY_ANIMATED) 'kitty-animated)
          ((= code NCPIXEL_KITTY_STATIC) 'kitty-static)
          ((= code NCPIXEL_ITERM2) 'iterm2)
          ((= code NCPIXEL_SIXEL) 'sixel)
          ((= code NCPIXEL_LINUXFB) 'linuxfb)
          (else 'none)))

(define* (detect-image-protocol #:optional (nc #f))
         "Detect terminal pixel/image protocol support ('kitty-selfref, 'kitty-animated,
'kitty-static, 'kitty, 'iterm2, 'sixel, 'linuxfb, or 'none). Probes notcurses when NC
is provided, otherwise inspects BADB_IMAGE_PROTOCOL and terminal environment variables."
         (let ((override (getenv "BADB_IMAGE_PROTOCOL")))
           (cond
             ((and override (not (string-null? override)))
               (string->symbol (string-downcase (string-trim-both override))))
             (nc
               (catch #t
                 (lambda ()
                   (pixel-support->symbol (notcurses_check_pixel_support nc)))
                 (lambda _ 'none)))
             (else
               (let ((term (string-downcase (or (getenv "TERM") "")))
                     (term-prog (string-downcase (or (getenv "TERM_PROGRAM") ""))))
                 (cond
                   ((or (getenv "KITTY_WINDOW_ID")
                        (string-contains term "kitty")
                        (string=? term-prog "ghostty")
                        (getenv "GHOSTTY_RESOURCES_DIR"))
                     'kitty)
                   ((string=? term-prog "wezterm") 'kitty)
                   ((string=? term-prog "iterm.app") 'iterm2)
                   ((string-contains term "sixel") 'sixel)
                   (else 'none)))))))

(define* (kitty-graphics-supported? #:optional (nc #f))
         "Return #t when Kitty graphics protocol is supported."
         (and (memq (detect-image-protocol nc)
                    '(kitty kitty-static kitty-animated kitty-selfref))
              #t))

(define %image-extensions
        '(".png" ".jpg" ".jpeg" ".gif" ".webp" ".bmp" ".tiff" ".svg"))

(define (image-file-path? path)
        "Return #t if PATH ends with a recognized image extension."
        (and (string? path)
             (let ((lower (string-downcase (string-trim-both path))))
               (any (lambda (ext) (string-suffix-ci? ext lower)) %image-extensions))))

(define* (format-kitty-image-escape path #:key (id 1) (cols #f) (rows #f))
         "Build a Kitty graphics protocol APC escape sequence to display image PATH."
         (let* ((safe-path (sanitize-osc-text path))
                (b64 (base64-encode (string->utf8 safe-path)))
                (dims (string-append
                        (if (integer? cols) (format #f ",c=~a" cols) "")
                        (if (integer? rows) (format #f ",r=~a" rows) ""))))
           (format #f "\x1b_Ga=T,f=100,t=f,i=~a~a;~a\x1b\\"
                   (if (integer? id) id 1)
                   dims
                   b64)))

(define*
  (render-image-ncvisual nc
                         plane
                         path
                         #:key
                         (y 0)
                         (x 0)
                         (max-rows 20)
                         (max-cols 60))
  "Decode and blit image PATH onto notcurses PLANE using ncvisual.
Returns an alist ((ok . #t) (blitter . sym) (rows . N) (cols . N)) on success,
or #f when images are unsupported, PATH does not exist, or blitting fails."
  (and nc
       plane
       (string? path)
       (file-exists? path)
       (catch #t
         (lambda ()
           (and (notcurses_canopen_images nc)
                (let ((vis (ncvisual_from_file path)))
                  (if (or (not vis) (equal? vis %null-pointer))
                    #f
                    (dynamic-wind
                      (lambda ()
                        #t)
                      (lambda ()
                        (let*
                            ((proto (detect-image-protocol nc))
                             (blitter
                               (if (eq? proto 'none)
                                 NCBLIT_2x2
                                 NCBLIT_PIXEL))
                             (opts
                               (make-ncvisual-options #:n
                                                      plane
                                                      #:scaling
                                                      NCSCALE_SCALE
                                                      #:y
                                                      y
                                                      #:x
                                                      x
                                                      #:leny
                                                      max-rows
                                                      #:lenx
                                                      max-cols
                                                      #:blitter
                                                      blitter))
                             (geom (make-ncvgeom)))
                          (ncvisual_geom nc vis (->pointer opts) (->pointer geom))
                          (let
                              ((out-plane (ncvisual_blit nc vis (->pointer opts))))
                            (if
                                (or (not out-plane) (equal? out-plane %null-pointer))
                              #f
                              `((ok . #t)
                                (blitter .
                                         ,(if (= blitter NCBLIT_PIXEL)
                                            'pixel
                                            '2x2))
                                (rows . ,(ncvgeom-rcelly geom))
                                (cols . ,(ncvgeom-rcellx geom))
                                (pixy . ,(ncvgeom-pixy geom))
                                (pixx . ,(ncvgeom-pixx geom)))))))
                      (lambda ()
                        (ncvisual_destroy vis)))))))
         (lambda _
           #f))))

(define %md-image-rx
        (make-regexp "!\\[[^]]*\\]\\(([^)]+)\\)"))

(define (extract-markdown-image-paths text)
        "Extract image file paths from Markdown ![alt](path) references in TEXT."
        (if (string? text)
          (let loop
            ((start 0) (acc '()))
            (let ((m (regexp-exec %md-image-rx text start)))
              (if m
                (let
                    ((path (string-trim-both (match:substring m 1))))
                  (loop (match:end m)
                        (if
                            (and (not (string-null? path)) (image-file-path? path))
                          (cons path acc)
                          acc)))
                (reverse acc))))
          '()))

(define* (format-image-placeholder path
                                   #:key (width 40)
                                   (protocol (detect-image-protocol))
                                   (nerd-fonts? (nerd-fonts-enabled?)))
         "Format a single-line image card placeholder when inline pixel blitting is deferred."
         (let* ((icon (nerd-icon 'image nerd-fonts?))
                (base (basename (or path "image")))
                (proto-tag (if (eq? protocol 'none) "" (format #f " (~a)" protocol)))
                (raw (format #f "[~a ~a~a]" icon base proto-tag)))
           (if (> (string-length raw) (max 8 width))
             (string-append (substring raw 0 (max 5 (- width 1))) "…]")
             raw)))
