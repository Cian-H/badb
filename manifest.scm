(use-modules (badb packages cl-indentify)
             (badb packages guile-notcurses)
             (guix profiles))

(concatenate-manifests
  (list
    (specifications->manifest
      (list "coreutils"
            "git"
            "grep"
            "guile"
            "bubblewrap"
            "guile-json"
            "notcurses"
            "sed"
            "findutils"
            "make"
            "tar"
            "gzip"
            "unzip"
            "tui-test"
            "uv"
            "sema"
            "prek"
            "rtk"
            "guile-quickcheck"
            "guile-fibers"
            "guile-git"
            "guile-lib"
            "guile-libyaml"
            "guile-semver"
            "guile-ts"
            "guile-srfi-234"
            "guile-uuid"
            "guile-pfds"
            "guile-syntax-highlight"
            "guile-sundown"
            "guile-pipe"
            "guile-srfi-197"
            "guile-srfi-158"
            "guile-knots"
            "guile-lens"
            "guile-file-names"
            "guile-fslib"
            "guile-lsp-server"
            "tree-sitter-scheme"
            "guile-sqlite3"
            "semcp"))
    (packages->manifest (list cl-indentify guile-notcurses))))
