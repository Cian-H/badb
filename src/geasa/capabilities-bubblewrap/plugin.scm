;;; Bubblewrap command construction for isolated project geis workers.
(define-module (geasa capabilities-bubblewrap plugin)
               #:use-module (ice-9 ftw)
               #:use-module (srfi srfi-13)
               #:export (bubblewrap-worker-command))

(define (store-paths paths)
        (filter (lambda (path) (string-prefix? "/gnu/store/" path)) paths))

(define (bubblewrap-worker-command bundle-dir src-root load-path compiled-path)
        "Return argv for a fail-closed bubblewrap worker around one project bundle."
        (let* ((path (string-split (or (getenv "PATH") "") #\:))
               (timeout (search-path path "timeout"))
               (bwrap (search-path path "bwrap"))
               (guile (search-path path "guile")))
          (unless (and timeout bwrap guile (file-exists? "/gnu/store"))
            (error "bubblewrap geis worker requires Guix store, coreutils timeout, bwrap, and Guile"))
          (let ((guile-load-path
                  (string-join (cons "/badb/src" (store-paths load-path)) ":"))
                (guile-compiled-path
                  (string-join (store-paths compiled-path) ":")))
            (list timeout "-k" "2" "120"
                  bwrap "--die-with-parent" "--unshare-all" "--new-session"
                  "--dir" "/gnu"
                  "--ro-bind" "/gnu/store" "/gnu/store"
                  "--dir" "/badb"
                  "--ro-bind" src-root "/badb/src"
                  "--ro-bind" bundle-dir "/badb/bundle"
                  "--tmpfs" "/tmp"
                  "--proc" "/proc"
                  "--dev" "/dev"
                  "--chdir" "/tmp"
                  "--clearenv"
                  "--setenv" "HOME" "/tmp"
                  "--setenv" "GUILE_LOAD_PATH" guile-load-path
                  "--setenv" "GUILE_LOAD_COMPILED_PATH" guile-compiled-path
                  "--"
                  guile "--no-auto-compile" "-L" "/badb/src"
                  "-s" "/badb/src/badb/geis/worker.scm"))))
