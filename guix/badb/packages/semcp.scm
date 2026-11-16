;;; semcp — Guix package for the prebuilt structural-editing-mcp CLI/MCP binary.
;;;
;;; Cian-H/structural-editing-mcp is an SBCL-compiled structural AST editing,
;;; static analysis, and staged workspace tool.  Upstream publishes prebuilt
;;; x86_64-linux release binaries with SHA-256 digests; this package pins the
;;; v2026.11.16.3 binary by checksum, patches its ELF interpreter and RUNPATH
;;; to resolve glibc and zlib from the Guix store, and re-attaches the
;;; trailing SBCL core image so the self-contained executable runs in pure Guix
;;; environments.

(define-module (badb packages semcp)
               #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix build-system copy)
               #:use-module (gnu packages base)
               #:use-module (gnu packages compression)
               #:use-module (gnu packages elf)
               #:use-module ((guix licenses) #:prefix license:))

(define-public semcp
               (package
                 (name "semcp")
                 (version "2026.11.16.3")
                 (source
                   (origin
                     (method url-fetch)
                     (uri (string-append
                            "https://github.com/Cian-H/structural-editing-mcp/releases/download/v"
                            version "/semcp-linux-x86_64"))
                     (sha256
                       (base32 "1m5fi6lcdvwfyyf93lsvxnp4qi66n9cxsvl8dqkd9mq6bd58fvw9"))))
                 (build-system copy-build-system)
                 (arguments
                   '(#:strip-binaries? #f
                     #:install-plan '(("semcp" "bin/"))
                     #:modules ((guix build copy-build-system)
                                (guix build utils)
                                (rnrs bytevectors)
                                (ice-9 binary-ports))
                     #:phases
                     (modify-phases %standard-phases
                      (replace 'unpack
                       (lambda* (#:key source #:allow-other-keys)
                        (copy-file source "semcp")
                        (chmod "semcp" #o755)
                        #t))
                      (add-after 'unpack 'patch-elf
                       (lambda* (#:key inputs #:allow-other-keys)
                        (let* ((semcp "semcp")
                               (bv (call-with-input-file semcp get-bytevector-all #:binary #t))
                               (len (bytevector-length bv))
                               (core-offset (bytevector-u64-ref bv (- len 16) (endianness little)))
                               (magic (let ((m (make-bytevector 8)))
                                       (bytevector-copy! bv (- len 8) m 0 8)
                                       m))
                               (core-len (- (- len 16) core-offset))
                               (core (let ((c (make-bytevector core-len)))
                                      (bytevector-copy! bv core-offset c 0 core-len)
                                      c))
                               (rpath
                                (string-join
                                 (map (lambda (input)
                                       (string-append (assoc-ref inputs input) "/lib"))
                                  '("glibc" "zlib"))
                                 ":")))
                         (invoke "patchelf" "--set-interpreter"
                          (search-input-file inputs "lib/ld-linux-x86-64.so.2")
                          semcp)
                         (invoke "patchelf" "--set-rpath" rpath semcp)
                         (let* ((patched-len (stat:size (stat semcp)))
                                (rem (modulo patched-len 4096))
                                (pad (if (zero? rem) 0 (- 4096 rem)))
                                (new-offset (+ patched-len pad))
                                (footer (make-bytevector 8 0)))
                          (bytevector-u64-set! footer 0 new-offset (endianness little))
                          (let ((port (open-file semcp "ab")))
                           (when (> pad 0)
                            (put-bytevector port (make-bytevector pad 0)))
                           (put-bytevector port core)
                           (put-bytevector port footer)
                           (put-bytevector port magic)
                           (close-port port))))))
                      (add-after 'patch-elf 'check-binary
                       (lambda _
                        (invoke "./semcp" "version"))))))
                 (inputs
                   (list glibc zlib))
                 (native-inputs
                   (list patchelf))
                 (home-page "https://github.com/Cian-H/structural-editing-mcp")
                 (synopsis "Structural AST editing and static analysis CLI and MCP server")
                 (description
                   "semcp (structural-editing-mcp) provides AST-level code reading,
surgical editing, refactoring, static analysis (lint, complexity, duplicates,
bindings), and staged workspace branching across Lisp/Scheme, Python,
JavaScript/TypeScript, Rust, Go, C/C++, and Java.")
                 (supported-systems '("x86_64-linux"))
                 (license license:lgpl3)))
