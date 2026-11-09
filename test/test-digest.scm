;;; test/test-digest.scm — pure Guile SHA-256 (FIPS 180-4) test suite.
;;;
;;; Verifies the digest against the official FIPS 180-4 test vectors
;;; and cross-checks a randomized input against the system sha256sum.

(define-module (test test-digest)
               #:use-module (badb kernel digest)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 rdelim)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-64))

(test-begin "digest")

(test-group "fips-180-4-vectors"
            (test-equal "sha256-hex of the empty string"
                        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                        (sha256-hex ""))
            (test-equal "sha256-hex matches the FIPS vector for \"abc\""
                        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
                        (sha256-hex "abc"))
            (test-equal "sha256-hex of the two-block FIPS vector"
                        "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
                        (sha256-hex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
            (test-equal "sha256-hex over multiple 64-byte blocks"
                        "41edece42d63e8d9bf515a9ba6932e1c20cbc9f5a5d134645adb5db1b9737ea3"
                        (sha256-hex (make-string 1000 #\a)))
            (test-equal "sha256-hex of the million-'a' FIPS vector (multi-block stress)"
                        "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
                        (sha256-hex (make-string 1000000 #\a))))

(test-group "digest-shape"
            (test-assert "digest is 64 lowercase hex chars"
                         (let ((digest (sha256-hex "deterministic shape probe")))
                           (and (= 64 (string-length digest))
                                (every (lambda (c)
                                         (memv c (string->list "0123456789abcdef")))
                                       (string->list digest)))))
            (test-equal "digest is deterministic"
                        (sha256-hex "deterministic shape probe")
                        (sha256-hex "deterministic shape probe")))

(test-group "sha256sum-cross-check"
            (let* ((tv (gettimeofday))
                   (seed (+ (* (car tv) 1000000) (cdr tv) (getpid)))
                   (state (seed->random-state (modulo seed 4294967296)))
                   (alphabet
                     (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 .,;:-_()[]{}!@#$%^&*+=/?<>~`|'\"\\"))
                   (len (+ 1000 (random 4000 state)))
                   (content
                     (apply string-append
                            (map (lambda (_)
                                   (string (list-ref alphabet (random (length alphabet) state))))
                                 (iota len))))
                   (tmp (string-append "/tmp/badb-digest-" (number->string (getpid)) ".scm"))
                   (sum-line
                     (begin
                       (call-with-output-file tmp (lambda (port) (display content port)))
                       (let* ((pipe (open-pipe* OPEN_READ "sha256sum" tmp))
                              (line (read-line pipe)))
                         (close-pipe pipe)
                         (delete-file tmp)
                         line))))
              (test-assert "cross-check reference line was produced"
                           (and (string? sum-line)
                                (not (string-null? sum-line))))
              (test-equal "randomized digest agrees with sha256sum"
                          (string-take sum-line 64)
                          (sha256-hex content))))

(test-end "digest")
