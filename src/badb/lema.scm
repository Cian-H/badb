;;; badb — Lema contract-oracle seam.
;;;
;;; Lema is the external contract-verification oracle. The real Lema
;;; store/server does not exist yet; this module is THE seam between
;;; badb and any Lema implementation. The protocol below is the
;;; contract between badb and every Lema implementation (the real
;;; server, or scripts/lema-stub for testing). Swapping the real Lema
;;; in behind this protocol requires no changes on the badb side.
;;;
;;; PROTOCOL
;;; =========
;;; Transport: UNIX-domain socket, one newline-delimited JSON object
;;; per line. Each request is a single JSON object on one line
;;; containing a "command" string plus command-specific fields; each
;;; response is a single JSON object on one line. A connection may
;;; carry multiple sequential commands and is closed by the client.
;;;
;;; Socket path: $BADB_LEMA_SOCKET when set, else the default
;;; $HOME/.local/share/lema/lema.sock.
;;;
;;; Commands:
;;;   PUBLISH-CONTRACT {id, code, spec}
;;;       -> {"ok": true}
;;;   SUBMIT {contract_id, ast_diff_hash, evidence}
;;;       -> {"verdict": "passed" | "failed", "details": STRING}
;;;   REPLAY-ERRORS {contract_id}
;;;       -> {"errors": [STRING, ...]}
;;;   LIST-VERIFIED {}
;;;       -> {"ids": [STRING, ...]}
;;;   GET-CONTRACT {id}
;;;       -> the contract record {"id", "code", "spec"},
;;;          or {"error": STRING} when unknown.
;;;
;;; Unknown commands elicit {"error": STRING}.
;;;
;;; THE PROVE GATE
;;; ==============
;;; (lema-prove-gate contract-id ast-diff-hash evidence) is
;;; kernel-owned: it opens the socket ITSELF, sends SUBMIT, and
;;; returns the verdict alist. The kernel never trusts
;;; agent-reported verdicts — only its own connection to Lema
;;; produces one. ANY transport failure RAISES: the gate fails
;;; CLOSED. No Lema means no pass; callers must treat errors as
;;; unverified, never as pass.
;;;
;;; IDENTITY PINNING
;;; =================
;;; When $BADB_LEMA_SOCKET is set and a transaction commit carries a
;;; contract id (the metadata key contract-id), the kernel runs the
;;; prove gate and pins on the journal commit record:
;;;   ast-diff-hash          — SHA-256 hex digest of the normalized
;;;                            staged AST diff text (each top-level
;;;                            form rendered with ~s, joined with
;;;                            newlines; reproducible with printf |
;;;                            sha256sum)
;;;   contract-id            — the Lema contract the commit answered to
;;;   kernel-replay-verdict  — "passed", from the kernel's own replay
;;;
;;; The hash helper is implemented in pure Scheme (SHA-256, FIPS
;;; 180-4) because no sha256 helper exists in the repo yet and the
;;; kernel must not shell out to external tools for a trust anchor.

(define-module (badb lema)
  #:use-module (badb util)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (lema-socket-path
            lema-gate-enabled?
            lema-connect
            lema-close
            lema-send-command!
            lema-request
            lema-publish-contract!
            lema-submit!
            lema-list-verified
            lema-get-contract
            lema-replay-errors
            lema-prove-gate
            lema-transaction-pins
            lema-ast-diff-hash
            sha256-hex))

;;;
;;; SHA-256 (FIPS 180-4), pure Scheme
;;;
(define %sha256-mask #xFFFFFFFF)

(define %sha256-h0
  (list #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
        #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

(define %sha256-k
  (vector #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
          #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
          #xd807aa98 #x12835b01 #x243185be #x550c7dc3
          #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
          #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
          #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
          #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
          #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
          #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
          #x650a7354 #x766a0abb #x81c2c92e #x92722c85
          #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
          #xd192e819 #xd6990624 #xf40e3585 #x106aa070
          #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
          #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
          #x748f82ee #x78a5636f #x84c87814 #x8cc70208
          #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(define (sha256-rotr x n)
  "Rotate the 32-bit word X right by N bits."
  (logand %sha256-mask
          (logior (ash x (- n)) (ash x (- 32 n)))))

(define (sha256-add . words)
  "Sum WORDS modulo 2^32."
  (logand (apply + words) %sha256-mask))

(define (sha256-process-block state block offset)
  "Compress the 64-byte BLOCK at OFFSET into STATE (list of 8 words),
returning the new state list."
  (let ((w (make-vector 64 0)))
    (let fill ((i 0))
      (when (< i 16)
        (let ((base (+ offset (* 4 i))))
          (vector-set! w i
                       (+ (ash (bytevector-u8-ref block base) 24)
                          (ash (bytevector-u8-ref block (+ base 1)) 16)
                          (ash (bytevector-u8-ref block (+ base 2)) 8)
                          (bytevector-u8-ref block (+ base 3)))))
        (fill (+ i 1))))
    (let extend ((i 16))
      (when (< i 64)
        (let* ((w15 (vector-ref w (- i 15)))
               (w2 (vector-ref w (- i 2)))
               (s0 (logxor (sha256-rotr w15 7)
                           (sha256-rotr w15 18)
                           (ash w15 -3)))
               (s1 (logxor (sha256-rotr w2 17)
                           (sha256-rotr w2 19)
                           (ash w2 -10))))
          (vector-set! w i
                       (sha256-add (vector-ref w (- i 16))
                                   s0
                                   (vector-ref w (- i 7))
                                   s1))
          (extend (+ i 1)))))
    (let loop ((i 0)
               (a (list-ref state 0)) (b (list-ref state 1))
               (c (list-ref state 2)) (d (list-ref state 3))
               (e (list-ref state 4)) (f (list-ref state 5))
               (g (list-ref state 6)) (h (list-ref state 7)))
      (if (= i 64)
          (list (sha256-add (list-ref state 0) a)
                (sha256-add (list-ref state 1) b)
                (sha256-add (list-ref state 2) c)
                (sha256-add (list-ref state 3) d)
                (sha256-add (list-ref state 4) e)
                (sha256-add (list-ref state 5) f)
                (sha256-add (list-ref state 6) g)
                (sha256-add (list-ref state 7) h))
          (let* ((s1 (logxor (sha256-rotr e 6)
                             (sha256-rotr e 11)
                             (sha256-rotr e 25)))
                 (ch (logxor (logand e f) (logand (lognot e) g)))
                 (t1 (sha256-add h s1 ch
                                (vector-ref %sha256-k i)
                                (vector-ref w i)))
                 (s0 (logxor (sha256-rotr a 2)
                             (sha256-rotr a 13)
                             (sha256-rotr a 22)))
                 (maj (logxor (logand a b)
                              (logand a c)
                              (logand b c)))
                 (t2 (sha256-add s0 maj)))
            (loop (+ i 1)
                  (sha256-add t1 t2) a b c
                  (sha256-add d t1) e f g))))))

(define (sha256-padded-length len)
  "Smallest multiple of 64 that is at least LEN + 9 (message + 0x80 + 8 length bytes)."
  (let ((r (remainder (+ len 9) 64)))
    (+ len 9 (if (zero? r) 0 (- 64 r)))))

(define (sha256-hex text)
  "Return the SHA-256 hex digest of the UTF-8 encoding of TEXT."
  (let* ((msg (string->utf8 text))
         (len (bytevector-length msg))
         (total (sha256-padded-length len))
         (padded (make-bytevector total 0)))
    (bytevector-copy! msg 0 padded 0 len)
    (bytevector-u8-set! padded len #x80)
    (let ((bits (ash len 3)))
      (let put-length ((k 0))
        (when (< k 8)
          (bytevector-u8-set! padded
                              (+ (- total 8) k)
                              (logand (ash bits (* -8 (- 7 k))) #xFF))
          (put-length (+ k 1)))))
    (let loop ((offset 0) (state %sha256-h0))
      (if (= offset total)
          (apply string-append
                 (map (lambda (word)
                        (format #f "~8,'0x" word))
                      state))
          (loop (+ offset 64)
                (sha256-process-block state padded offset))))))

;;;
;;; Socket path resolution
;;;
(define (lema-default-socket-path)
  "Return the default Lema socket path under $HOME."
  (string-append (or (getenv "HOME") "/tmp")
                 "/.local/share/lema/lema.sock"))

(define (lema-socket-path)
  "Return the Lema oracle socket path: $BADB_LEMA_SOCKET when set,
else the default ~/.local/share/lema/lema.sock."
  (let ((env (getenv "BADB_LEMA_SOCKET")))
    (if (and env (not (string-null? env)))
        env
        (lema-default-socket-path))))

(define (lema-gate-enabled?)
  "Return the configured socket path when the prove gate is armed
($BADB_LEMA_SOCKET set), else #f. An unset variable means the gate is
disarmed and commits behave exactly as before the seam existed."
  (let ((env (getenv "BADB_LEMA_SOCKET")))
    (and env (not (string-null? env)) env)))

;;;
;;; Connection handling
;;;
(define* (lema-connect #:optional (socket-path (lema-socket-path)))
  "Open a connection to the Lema oracle socket at SOCKET-PATH,
returning a bidirectional socket port. Raises 'lema-unreachable when
the socket cannot be created or refuses connections — a working
connection is a hard requirement for any verdict."
  (let ((sock (catch 'system-error
                (lambda () (socket AF_UNIX SOCK_STREAM 0))
                (lambda args
                  (scm-error 'lema-unreachable "lema-connect"
                             "cannot create socket: ~a"
                             (list (cadr args)) #f)))))
    (catch 'system-error
      (lambda ()
        (connect sock AF_UNIX socket-path)
        sock)
      (lambda args
        (close-port sock)
        (scm-error 'lema-unreachable "lema-connect"
                   "cannot reach Lema oracle at ~s: ~a"
                   (list socket-path (cadr args)) #f)))))

(define (lema-close conn)
  "Close a Lema connection opened by lema-connect, ignoring errors."
  (catch #t
    (lambda () (unless (port-closed? conn) (close-port conn)))
    (lambda _ #f)))

;;;
;;; Protocol round-trips
;;;
(define* (lema-send-command! conn command fields)
  "Send one protocol COMMAND with FIELDS (an alist) on open CONN and
return the parsed response alist. Raises 'lema-protocol on a missing,
empty, or malformed response."
  (display (scm->json-string (cons (cons 'command command) fields)) conn)
  (newline conn)
  (force-output conn)
  (let ((line (read-line conn)))
    (if (or (eof-object? line) (string-null? (string-trim-both line)))
        (scm-error 'lema-protocol "lema-send-command!"
                   "empty response from Lema for command ~s"
                   (list command) #f)
        (let ((resp (catch #t
                      (lambda () (json-string->scm (string-trim-both line)))
                      (lambda _ #f))))
          (if (and (list? resp) (pair? resp))
              resp
              (scm-error 'lema-protocol "lema-send-command!"
                         "malformed JSON response from Lema: ~s"
                         (list line) #f))))))

(define* (lema-request command fields
                       #:key (conn #f) (socket-path (lema-socket-path)))
  "Perform one protocol round-trip for COMMAND with FIELDS. When CONN
is given, reuse it (and leave it open); otherwise open a fresh
connection to SOCKET-PATH and close it after the response. Response
alists carry STRING keys as parsed from JSON."
  (if conn
      (lema-send-command! conn command fields)
      (let ((c (lema-connect socket-path)))
        (catch #t
          (lambda ()
            (let ((resp (lema-send-command! c command fields)))
              (lema-close c)
              resp))
          (lambda (key . args)
            (lema-close c)
            (apply throw key args))))))

(define* (lema-publish-contract! id code spec #:key (conn #f))
  "Publish contract ID with CODE (the contract source) and SPEC (the
declarative spec text) to Lema. Returns the response alist, e.g.
((\"ok\" . #t))."
  (lema-request "PUBLISH-CONTRACT"
                `((id . ,id) (code . ,code) (spec . ,spec))
                #:conn conn))

(define* (lema-submit! contract-id ast-diff-hash evidence #:key (conn #f))
  "Submit a verification request for CONTRACT-ID with AST-DIFF-HASH and
EVIDENCE to Lema. Returns the response alist with string keys, e.g.
((\"verdict\" . \"passed\") (\"details\" . \"...\")). EVIDENCE may be
any object; non-strings are serialized to JSON first."
  (lema-request "SUBMIT"
                `((contract_id . ,contract-id)
                  (ast_diff_hash . ,ast-diff-hash)
                  (evidence . ,(if (string? evidence)
                                   evidence
                                   (safe-json-string evidence))))
                #:conn conn))

(define* (lema-list-verified #:key (conn #f))
  "Return the ids of contracts verified by Lema:
((\"ids\" . #(\"...\" ...)))."
  (lema-request "LIST-VERIFIED" '() #:conn conn))

(define* (lema-get-contract id #:key (conn #f))
  "Return the contract record alist for ID as reported by Lema, or an
error alist ((\"error\" . STRING)) when the contract is unknown."
  (lema-request "GET-CONTRACT" `((id . ,id)) #:conn conn))

(define* (lema-replay-errors contract-id #:key (conn #f))
  "Return the recorded replay errors for CONTRACT-ID:
((\"errors\" . #(\"...\" ...)))."
  (lema-request "REPLAY-ERRORS" `((contract_id . ,contract-id))
                #:conn conn))

;;;
;;; THE PROVE GATE (kernel-owned)
;;;
(define (lema-prove-gate contract-id ast-diff-hash evidence)
  "Kernel-owned prove gate: open the Lema socket itself, send SUBMIT
for CONTRACT-ID, and return the verdict alist
((verdict . \"passed\"|\"failed\") (details . STRING)). The verdict is
taken only from the kernel's own connection to Lema; agent-reported
verdicts are never trusted. ANY transport failure raises
('lema-unreachable, 'lema-protocol, or the underlying system error) —
the gate fails CLOSED: no Lema means no pass, and callers must treat
errors as unverified, never as pass."
  (let* ((resp (lema-submit! contract-id ast-diff-hash evidence))
         (verdict (assoc-ref resp "verdict")))
    (if (member verdict '("passed" "failed"))
        `((verdict . ,verdict)
          (details . ,(ensure-string (or (assoc-ref resp "details") ""))))
        (scm-error 'lema-protocol "lema-prove-gate"
                   "malformed SUBMIT response from Lema: ~s"
                   (list resp) #f))))

;;;
;;; Identity pinning for transaction commits
;;;
(define (lema-contract-id-ref metadata)
  "Extract the contract id from METADATA (symbol, string, or keyword
variants of contract-id / contract_id), or #f when absent."
  (or (prop-ref metadata 'contract-id)
      (prop-ref metadata 'contract_id)))

(define (lema-ast-diff-hash forms)
  "Return the identity-pinning hash for the staged AST patch FORMS:
the SHA-256 hex digest of the normalized diff text. Normalization
choice (documented): each top-level form is rendered with ~s and
terminated with a newline, so the digest is reproducible externally
(one form per line piped to sha256sum). A raw string is used verbatim
as the diff text."
  (let ((diff-text
         (if (string? forms)
             forms
             (apply string-append
                    (map (lambda (form)
                           (string-append (format #f "~s" form) "\n"))
                         (if (list? forms) forms (list forms)))))))
    (sha256-hex diff-text)))

(define (lema-transaction-pins forms metadata)
  "Run the prove gate for a transaction commit carrying METADATA and
the staged FORMS, returning the identity-pin outcome for the journal
record:
  ((gate . \"disabled\"))  — BADB_LEMA_SOCKET not set (current behavior)
  ((gate . \"skipped\"))    — gate armed but the commit carries no contract id
  ((gate . \"passed\") (ast-diff-hash . H) (contract-id . ID)
   (kernel-replay-verdict . \"passed\"))
  ((gate . \"failed\") (details . STRING))
Transport failures RAISE (fail closed); this procedure never reports a
pass it did not receive from the oracle itself."
  (let ((socket-path (lema-gate-enabled?)))
    (if (not socket-path)
        '((gate . "disabled"))
        (let ((contract-id (lema-contract-id-ref metadata)))
          (if (not contract-id)
              '((gate . "skipped"))
              (let* ((id (ensure-string contract-id))
                     (hash (lema-ast-diff-hash forms))
                     (evidence (safe-json-string (sanitize-for-json metadata)))
                     (verdict-alist (lema-prove-gate id hash evidence)))
                (if (string=? (assoc-ref verdict-alist 'verdict) "passed")
                    `((gate . "passed")
                      (ast-diff-hash . ,hash)
                      (contract-id . ,id)
                      (kernel-replay-verdict . "passed"))
                    `((gate . "failed")
                      (details . ,(assoc-ref verdict-alist 'details))))))))))
