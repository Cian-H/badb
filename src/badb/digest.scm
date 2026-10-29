;;; badb — Cryptographic digest primitives.
;;;
;;; SHA-256 (FIPS 180-4) implemented in pure Scheme. The kernel must
;;; not shell out to external tools for trust anchors (spec locking,
;;; identity pinning), so this module is the single source of the
;;; digest used across the spec gate and the checkpoint journal.

(define-module (badb digest)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:export (sha256-hex))

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
  (logior (ash (logand x %sha256-mask) (- n))
          (ash (logand x (- (ash 1 n) 1)) (- 32 n))))

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
