(use-modules (badb packages cl-indentify)
             (badb packages guile-notcurses)
             (guix profiles))

(concatenate-manifests
  (list
    (specifications->manifest
      (list "bubblewrap"
            "coreutils"
            "findutils"
            "git"
            "grep"
            "guile"
            "guile-fibers"
            "guile-file-names"
            "guile-fslib"
            "guile-git"
            "guile-json"
            "guile-knots"
            "guile-lens"
            "guile-lib"
            "guile-libyaml"
            "guile-lsp-server"
            "guile-pfds"
            "guile-pipe"
            "guile-quickcheck"
            "guile-semver"
            "guile-sqlite3"
            "guile-srfi-158"
            "guile-srfi-197"
            "guile-srfi-234"
            "guile-sundown"
            "guile-syntax-highlight"
            "guile-ts"
            "guile-uuid"
            "gzip"
            "make"
            "notcurses"
            "prek"
            "rtk"
            "sed"
            "sema"
            "semcp"
            "tar"
            "tree-sitter-scheme"
            "tui-test"
            "unzip"
            "uv"))
    (packages->manifest (list cl-indentify guile-notcurses))))
