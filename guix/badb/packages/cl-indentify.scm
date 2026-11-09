;;; cl-indentify — Command-line code beautifier for Common Lisp and Scheme.

(define-module (badb packages cl-indentify)
               #:use-module (guix packages)
               #:use-module (guix gexp)
               #:use-module (guix build-system asdf)
               #:use-module (gnu packages lisp-xyz)
               #:use-module ((guix licenses) #:prefix license:))

(define-public cl-indentify
               (package
                 (inherit sbcl-cl-indentify)
                 (name "cl-indentify")
                 (arguments
                   (list
                     #:phases
                     #~(modify-phases %standard-phases
                                      (add-after 'create-asdf-configuration 'build-program
                                                 (lambda* (#:key outputs #:allow-other-keys)
                                                          (build-program
                                                            (string-append #$output "/bin/cl-indentify")
                                                            outputs
                                                            #:dependencies '("cl-indentify/cli")
                                                            #:entry-program '((apply (read-from-string "indentify/cli:main") arguments))
                                                            #:compress? #f))))))
                 (synopsis "Command-line code beautifier for Common Lisp and Scheme")
                 (description
                   "cl-indentify is a command-line tool and library to automatically indent
Common Lisp and Scheme source files.")))
