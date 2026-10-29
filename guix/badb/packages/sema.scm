;;; sema — Guix package for the prebuilt sema-lang CLI release binary.
;;;
;;; sema-lisp/sema is a Rust workspace of 18 crates with a large
;;; dependency graph; building it from source in the guix channel
;;; pinned in this repo's channels.scm is not currently practical, so
;;; this package pins the x86_64-unknown-linux-gnu release binary from
;;; the v2.0.0 GitHub release instead.  Upstream publishes the binary
;;; with a sha256 digest and GitHub artifact attestations.  Every
;;; install fetches the exact same bytes and the digest is verified by
;;; guix, but note this is integrity pinning, not a source build: guix
;;; cannot reproduce these bytes from source.
;;;
;;; Unlike tui-test (musl, static), upstream publishes only glibc
;;; binaries for Linux.  The binary is dynamically linked against
;;; libudev, libgcc_s, and glibc, and its ELF interpreter and RUNPATH
;;; reference the build host's paths, so the patch-elf phase below
;;; rewrites them to point at the store copies from the declared
;;; inputs.  The gnu-build-system validate-runpath phase (inherited
;;; through copy-build-system) then verifies that every needed library
;;; resolves within the RUNPATH.

(define-module (badb packages sema)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module ((guix licenses) #:prefix license:))

(define-public sema
  (package
    (name "sema")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/sema-lisp/sema/releases/download/v"
             version "/sema-lang-x86_64-unknown-linux-gnu.tar.xz"))
       (sha256
        (base32 "1j19nj9dayn8fl2srz8hd34l2w8xajsnr8da3dqxw66ymk6680v1"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("sema" "bin/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-elf
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((sema "sema")
                   (rpath
                    (string-join
                     (map (lambda (input)
                            (string-append (assoc-ref inputs input) "/lib"))
                          '("glibc" "gcc" "eudev"))
                     ":")))
               (invoke "patchelf" "--set-interpreter"
                       (search-input-file inputs "lib/ld-linux-x86-64.so.2")
                       sema)
               (invoke "patchelf" "--set-rpath" rpath sema)))))))
    (inputs
     (list glibc `(,gcc "lib") eudev))
    (native-inputs
     (list patchelf))
    (home-page "https://sema-lang.com")
    (synopsis
     "Lisp with LLM agents as language primitives, compiled to a bytecode VM")
    (description
     "Sema is a Scheme-like Lisp where prompts are s-expressions,
conversations are persistent data structures, and LLM calls are just
another form of evaluation.  The language compiles to a fast bytecode
VM and ships as a single binary; agent scaffolding such as retries,
caching, cost caps, rate limits, tool dispatch, and conversation state
is provided by the runtime.  Sema also exposes MCP tooling, embeddings,
and reranking as language-level facilities.")
    (supported-systems '("x86_64-linux"))
    (license license:expat)))
