;;; (geasa capabilities-context-index bm25) — Pure-Guile BM25 Search Engine
;;;
;;; Fast, zero-dependency Okapi BM25 scoring with light suffix stemming,
;;; document chunking (markdown and sliding-window text), exact substring boost,
;;; and S-expression index serialization.

(define-module (geasa capabilities-context-index bm25)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (pfds hamts)
  #:export (<chunk>
            make-chunk
            chunk?
            chunk-id
            chunk-source-id
            chunk-section
            chunk-start-line
            chunk-end-line
            chunk-text
            chunk-token-count
            chunk-token-freqs

            <bm25-index>
            make-bm25-index
            bm25-index?
            index-chunks-table
            index-doc-freqs
            index-total-tokens
            index-doc-count
            index-raw-bytes
            index-returned-bytes
            index-next-chunk-id
            index-next-doc-id
            set-index-raw-bytes!
            set-index-returned-bytes!
            set-index-next-doc-id!

            tokenize-text
            stem-token
            take-n
            drop-n
            chunk-markdown
            chunk-plain-lines
            bm25-add-chunk!
            bm25-search
            save-bm25-index!
            load-bm25-index))

;;;
;;; Records
;;;
(define-record-type <chunk>
  (make-chunk id source-id section start-line end-line text token-count token-freqs)
  chunk?
  (id chunk-id)
  (source-id chunk-source-id)
  (section chunk-section)
  (start-line chunk-start-line)
  (end-line chunk-end-line)
  (text chunk-text)
  (token-count chunk-token-count)
  (token-freqs chunk-token-freqs))

(define-record-type <bm25-index>
  (%make-bm25-index chunks-table doc-freqs total-tokens doc-count raw-bytes returned-bytes next-chunk-id next-doc-id)
  bm25-index?
  (chunks-table index-chunks-table set-index-chunks-table!)
  (doc-freqs index-doc-freqs set-index-doc-freqs!)
  (total-tokens index-total-tokens set-index-total-tokens!)
  (doc-count index-doc-count set-index-doc-count!)
  (raw-bytes index-raw-bytes set-index-raw-bytes!)
  (returned-bytes index-returned-bytes set-index-returned-bytes!)
  (next-chunk-id index-next-chunk-id set-index-next-chunk-id!)
  (next-doc-id index-next-doc-id set-index-next-doc-id!))

(define (make-bm25-index)
  (%make-bm25-index (make-hamt string-hash string=?)
                    (make-hamt string-hash string=?)
                    0 0 0 0 1 1))

;;;
;;; Tokenization & Stemming
;;;
(define (stem-token word)
  "Light suffix stemmer for English identifiers."
  (let ((len (string-length word)))
    (cond
      ((and (> len 6) (string-suffix? "tion" word))
       (substring word 0 (- len 4)))
      ((and (> len 5) (string-suffix? "ings" word))
       (substring word 0 (- len 4)))
      ((and (> len 4) (string-suffix? "ing" word))
       (let* ((base (substring word 0 (- len 3)))
              (blen (string-length base)))
         (if (and (> blen 2)
                  (char=? (string-ref base (- blen 1)) (string-ref base (- blen 2)))
                  (member (string-ref base (- blen 1)) '(#\b #\d #\g #\m #\n #\p #\t)))
             (substring base 0 (- blen 1))
             base)))
      ((and (> len 4) (string-suffix? "ies" word))
       (string-append (substring word 0 (- len 3)) "y"))
      ((and (> len 4) (string-suffix? "ed" word))
       (let* ((base (substring word 0 (- len 2)))
              (blen (string-length base)))
         (if (and (> blen 2)
                  (char=? (string-ref base (- blen 1)) (string-ref base (- blen 2)))
                  (member (string-ref base (- blen 1)) '(#\b #\d #\g #\m #\n #\p #\t)))
             (substring base 0 (- blen 1))
             base)))
      ((and (> len 4) (string-suffix? "ly" word))
       (substring word 0 (- len 2)))
      ((and (> len 3) (string-suffix? "es" word))
       (substring word 0 (- len 2)))
      ((and (> len 3) (string-suffix? "s" word) (not (string-suffix? "ss" word)))
       (substring word 0 (- len 1)))
      (else word))))

(define (tokenize-text str)
  "Tokenize STR into lowercased stemmed tokens, splitting on non-alphanumeric delimiters."
  (let* ((lower (string-downcase str))
         (raw-tokens (string-tokenize lower char-set:letter+digit)))
    (filter-map
     (lambda (tok)
       (let ((trimmed (string-trim-both tok)))
         (if (>= (string-length trimmed) 2)
             (stem-token trimmed)
             #f)))
     raw-tokens)))

(define (count-tokens tokens)
  (let ((counts (make-hash-table (max 1 (length tokens)))))
    (for-each
     (lambda (tok)
       (let ((cur (hash-ref counts tok 0)))
         (hash-set! counts tok (+ cur 1))))
     tokens)
    counts))

;;;
;;; Chunking Helpers
;;;
(define (take-n lst n)
  (let loop ((l lst) (i 0) (acc '()))
    (if (or (null? l) (>= i n))
        (reverse acc)
        (loop (cdr l) (+ i 1) (cons (car l) acc)))))

(define (drop-n lst n)
  (let loop ((l lst) (i 0))
    (if (or (null? l) (>= i n))
        l
        (loop (cdr l) (+ i 1)))))

(define* (chunk-plain-lines text #:key (window-size 35) (overlap 5))
  "Chunk plain text into window-sized slices with overlap.
Returns list of ((section start-line end-line text) ...)."
  (let* ((lines (string-split text #\newline))
         (total (length lines)))
    (if (zero? total)
        '()
        (let loop ((offset 0) (res '()))
          (let* ((remaining (drop-n lines offset))
                 (slice (take-n remaining window-size))
                 (slice-len (length slice))
                 (chunk-start (+ offset 1))
                 (chunk-end (+ offset slice-len))
                 (chunk-text (string-join slice "\n"))
                 (new-res (cons (list #f chunk-start chunk-end chunk-text) res))
                 (step (max 1 (- window-size overlap)))
                 (next-offset (+ offset step)))
            (if (or (>= next-offset total) (<= (length remaining) window-size))
                (reverse new-res)
                (loop next-offset new-res)))))))

(define (chunk-markdown source-id text)
  "Chunk markdown TEXT by headers (#, ##, ###) into a list of chunk specs:
((section start-line end-line text) ...)"
  (let* ((lines (string-split text #\newline))
         (total (length lines)))
    (if (zero? total)
        '()
        (let loop ((ln 1)
                   (remaining lines)
                   (current-section "root")
                   (section-start 1)
                   (accum-lines '())
                   (results '()))
          (cond
            ((null? remaining)
             (if (null? accum-lines)
                 (reverse results)
                 (reverse (cons (list current-section section-start (- ln 1)
                                      (string-join (reverse accum-lines) "\n"))
                                results))))
            (else
             (let* ((line (car remaining))
                    (is-header? (or (string-prefix? "# " line)
                                    (string-prefix? "## " line)
                                    (string-prefix? "### " line)
                                    (string-prefix? "#### " line))))
               (if (and is-header? (pair? accum-lines))
                   (let ((completed-chunk
                          (list current-section section-start (- ln 1)
                                (string-join (reverse accum-lines) "\n")))
                         (new-sec (string-trim-both
                                   (string-trim line (char-set #\# #\space)))))
                     (loop (+ ln 1)
                           (cdr remaining)
                           (if (string-null? new-sec) "header" new-sec)
                           ln
                           (list line)
                           (cons completed-chunk results)))
                   (let ((sec-name (if is-header?
                                       (string-trim-both (string-trim line (char-set #\# #\space)))
                                       current-section)))
                     (loop (+ ln 1)
                           (cdr remaining)
                           sec-name
                           section-start
                           (cons line accum-lines)
                           results))))))))))

;;;
;;; Indexing & Scoring
;;;
(define (bm25-add-chunk! idx source-id section start-line end-line text)
  "Add a text chunk to the BM25 index."
  (let* ((cid (index-next-chunk-id idx))
         (tokens (tokenize-text text))
         (token-count (length tokens))
         (token-freqs (count-tokens tokens))
         (chunk (make-chunk cid source-id section start-line end-line text token-count token-freqs)))
    (set-index-next-chunk-id! idx (+ cid 1))
    (set-index-chunks-table! idx
                             (hamt-set (index-chunks-table idx)
                                       (number->string cid) chunk))
    (set-index-doc-count! idx (+ (index-doc-count idx) 1))
    (set-index-total-tokens! idx (+ (index-total-tokens idx) token-count))
    (set-index-doc-freqs!
     idx
     (hash-fold
      (lambda (tok _ df-hamt)
        (hamt-set df-hamt tok (+ (hamt-ref df-hamt tok 0) 1)))
      (index-doc-freqs idx)
      token-freqs))
    chunk))

(define*
  (bm25-search idx query #:key (source-id #f) (top-k 5))
  "Search IDX for QUERY using Okapi BM25 (k1=1.2, b=0.75) with substring match bonus.
Returns a list of ((score . <chunk>) ...) sorted in descending order of score."
  (let*
      ((q-tokens (tokenize-text query))
       (q-lower (string-downcase (string-trim-both query)))
       (N (index-doc-count idx))
       (avgdl (/ (max 1 (index-total-tokens idx)) (max 1.0 N)))
       (k1 1.2)
       (b 0.75)
       (candidates '()))
    (when (and (> N 0) (pair? q-tokens))
      (set! candidates
            (hamt-fold
              (lambda (_ chunk acc)
                (if
                    (or (not source-id) (string=? (chunk-source-id chunk) source-id))
                  (let*
                      ((c-freqs (chunk-token-freqs chunk))
                       (dl (chunk-token-count chunk))
                       (score
                         (fold
                           (lambda (q-tok score-acc)
                             (let ((tf (hash-ref c-freqs q-tok 0)))
                               (if (> tf 0)
                                 (let*
                                     ((df (hamt-ref (index-doc-freqs idx) q-tok 0))
                                      ;; Non-negative Okapi IDF
                                      (idf (log (+ 1.0 (/ (+ (- N df) 0.5) (+ df 0.5)))))
                                      (denom (+ tf (* k1 (+ (- 1.0 b) (* b (/ dl (max 1.0 avgdl)))))))
                                      (tf-part (/ (* tf (+ k1 1.0)) (max 0.0001 denom))))
                                   (+ score-acc (* idf tf-part)))
                                 score-acc)))
                           0.0
                           q-tokens))
                       ;; Exact substring boost: +2.0 if whole query is in chunk text
                       (txt-lower (string-downcase (chunk-text chunk)))
                       (sub-boost
                         (if
                             (and (not (string-null? q-lower)) (string-contains txt-lower q-lower))
                           2.0
                           0.0))
                       ;; Section name boost
                       (sec-boost
                         (if
                             (and (chunk-section chunk)
                                  (not (string-null? q-lower))
                                  (string-contains (string-downcase (chunk-section chunk)) q-lower))
                           1.0
                           0.0))
                       (final-score (+ score sub-boost sec-boost)))
                    (if (> final-score 0.01)
                      (cons (cons final-score chunk) acc)
                      acc))
                  acc))
              '()
              (index-chunks-table idx))))
    (let
        ((sorted
           (sort candidates
                 (lambda (cand-a cand-b)
                   (> (car cand-a) (car cand-b))))))
      (take-n sorted top-k))))

;;;
;;; Persistence
;;;
(define (mkdir-p path)
  (unless (or (string-null? path) (string=? path "/") (string=? path ".") (file-exists? path))
    (mkdir-p (dirname path))
    (catch #t
      (lambda () (mkdir path))
      (lambda _ #f))))

(define (save-bm25-index! idx file-path)
  "Save IDX to FILE-PATH as an S-expression."
  (mkdir-p (dirname file-path))
  (let* ((chunks (hamt-fold (lambda (_ c acc) (cons c acc))
                            '()
                            (index-chunks-table idx)))
         (chunk-sexps
          (map (lambda (c)
                 `((id . ,(chunk-id c))
                   (source-id . ,(chunk-source-id c))
                   (section . ,(chunk-section c))
                   (start-line . ,(chunk-start-line c))
                   (end-line . ,(chunk-end-line c))
                   (text . ,(chunk-text c))))
               chunks))
         (data `((version . 1)
                 (raw-bytes . ,(index-raw-bytes idx))
                 (returned-bytes . ,(index-returned-bytes idx))
                 (next-doc-id . ,(index-next-doc-id idx))
                 (chunks . ,chunk-sexps))))
    (call-with-output-file file-path
      (lambda (p) (write data p)))))

(define (load-bm25-index file-path)
  "Load BM25 index from FILE-PATH. Returns a newly reconstructed <bm25-index>."
  (if (file-exists? file-path)
      (let* ((data (catch #t
                     (lambda () (call-with-input-file file-path read))
                     (lambda _ #f)))
             (idx (make-bm25-index)))
        (when (pair? data)
          (let ((rb (assoc-ref data 'raw-bytes))
                (retb (assoc-ref data 'returned-bytes))
                (ndi (assoc-ref data 'next-doc-id))
                (chunks (assoc-ref data 'chunks)))
            (when rb (set-index-raw-bytes! idx rb))
            (when retb (set-index-returned-bytes! idx retb))
            (when ndi (set-index-next-doc-id! idx ndi))
            (when (list? chunks)
              (for-each
               (lambda (entry)
                 (let ((sid (assoc-ref entry 'source-id))
                       (sec (assoc-ref entry 'section))
                       (sl (assoc-ref entry 'start-line))
                       (el (assoc-ref entry 'end-line))
                       (txt (assoc-ref entry 'text)))
                   (when (and sid txt)
                     (bm25-add-chunk! idx sid sec sl el txt))))
               chunks))))
        idx)
      (make-bm25-index)))
