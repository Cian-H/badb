;;; badb/theme.scm — Backwards-compatibility facade for (badb surfaces theme).
(define-module (badb theme)
  #:use-module (badb surfaces theme)
  #:re-export (<theme>
               make-theme
               theme?
               theme-name
               theme-roles
               theme-ref
               all-theme-names
               lookup-theme
               default-theme
               theme-role-names
               valid-theme-color?))
