;;; pack-manifest.scm — Runtime-only manifest for the badb relocatable pack.
;;;
;;; Unlike the root manifest.scm (the full development environment),
;;; this manifest pulls in just the badb package itself; its inputs and
;;; propagated-inputs (guile, guile-json, notcurses, guile-quickcheck,
;;; sema) form the complete runtime closure embedded in `guix pack -RR`
;;; tarballs built by scripts/dist.

(use-modules (guix profiles)
             (badb packages badb))

(packages->manifest (list badb))
