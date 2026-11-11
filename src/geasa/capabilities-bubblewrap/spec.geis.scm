(define-module (test capabilities-bubblewrap-spec)
               #:use-module (srfi srfi-64)
               #:use-module (srfi srfi-1)
               #:use-module (badb spec)
               #:use-module (geasa capabilities-bubblewrap plugin))

(spec-begin "capabilities-bubblewrap")

(test-group "bubblewrap-worker-policy"
            (let ((argv (bubblewrap-worker-command "/tmp/geis" "/tmp/src"
                                                   '("/gnu/store/profile/share/guile/site/3.0"
                                                     "/home/user/private/modules")
                                                   '("/gnu/store/profile/lib/guile/3.0/ccache"))))
              (test-assert "worker starts in isolated namespaces"
                           (and (member "--unshare-all" argv)
                                (member "--new-session" argv)))
              (test-assert "worker has no shared network namespace"
                           (not (member "--share-net" argv)))
              (test-assert "bundle and source are read-only mounts"
                           (and (member "--ro-bind" argv)
                                (member "/badb/bundle" argv)
                                (member "/badb/src" argv)))
              (test-assert "worker gets a clean environment and temporary home"
                           (and (member "--clearenv" argv)
                                (member "HOME" argv)
                                (member "/tmp" argv)))
              (test-assert "module paths exclude host paths outside the Guix store"
                           (let ((load-path (list-ref argv (+ 1 (list-index (lambda (x) (equal? x "GUILE_LOAD_PATH")) argv)))))
                             (and (string-contains load-path "/gnu/store/profile/share/guile/site/3.0")
                                  (not (string-contains load-path "/home/user/private/modules")))))))

(spec-end)
(spec-exit)
