;;; badb/cli.scm — Backwards-compatibility facade for (badb surfaces cli).
(define-module (badb cli)
  #:use-module (badb surfaces cli)
  #:re-export (bold
               cyan
               green
               red
               supports-color?))
