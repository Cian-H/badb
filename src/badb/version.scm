(define-module (badb version)
               #:use-module (ice-9 textual-ports)
               #:export (badb-version %badb-version))

(define (find-version-file)
        "Locate the shared VERSION file relative to (badb version) or cwd."
        (let* ((mod-path (%search-load-path "badb/version.scm"))
               (root-dir (and mod-path
                              (dirname (dirname (dirname mod-path)))))
               (candidate (and root-dir
                               (string-append root-dir "/VERSION"))))
          (cond
            ((and candidate (file-exists? candidate)) candidate)
            ((file-exists? "VERSION") "VERSION")
            (else #f))))

(define (badb-version)
        "Read the shared application version from VERSION, falling back to \"0.1.0\"."
        (let ((path (find-version-file)))
          (if (not path)
            "0.1.0"
            (catch #t
              (lambda ()
                (let ((trimmed (string-trim-both
                                 (call-with-input-file path get-string-all))))
                  (if (string-null? trimmed) "0.1.0" trimmed)))
              (lambda _ "0.1.0")))))

(define %badb-version (badb-version))
