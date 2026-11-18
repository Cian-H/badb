;;; spec.geis.scm — Kernel contract for (geasa capabilities-git plugin)
;;;
;;; Degraded-mode contract: with no Git repository present the bundle
;;; must still load, register its tools, and report unavailability as a
;;; value (never an unwind). Commit semantics are verified against a
;;; throwaway repository so the contract stays hermetic.

(define-module (test capabilities-git-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa capabilities-git plugin))

(spec-begin "capabilities-git")

(test-group "git-contract"
            (test-assert
              "plugin module loads and exposes git-status"
              (procedure? git-status))
            (test-assert
              "git-status on a missing repo degrades to a message"
              (string? (git-status "/nonexistent/repo-xyz")))
            (test-assert
              "git-log on a missing repo degrades to a message"
              (string? (git-log "/nonexistent/repo-xyz")))
            (test-assert "git-commit refuses empty message"
                         (string-contains (git-commit "/nonexistent/repo-xyz" "") "error"))
            (test-assert
              "git-commit on a missing repo degrades to a message"
              (string? (git-commit "/nonexistent/repo-xyz" "msg")))
            (let
                ((repo (mkdtemp "/tmp/badb-git-spec-XXXXXX")))
              (dynamic-wind
                (lambda ()
                  #t)
                (lambda ()
                  (system* "git" "-C" repo "init" "-q")
                  (system* "git" "-C" repo "config" "user.name" "Test")
                  (system* "git" "-C" repo "config" "user.email" "test@example.com")
                  (test-assert
                    "git-status succeeds on initialized repo"
                    (string-contains (git-status repo) "##"))
                  (test-assert "git-commit refuses clean repo"
                               (string-contains (git-commit repo "empty") "nothing to commit"))
                  (call-with-output-file
                    (string-append repo "/hello.txt")
                    (lambda (p)
                      (display "hello\n" p)))
                  (test-assert "git-status reports untracked file"
                               (string-contains (git-status repo) "hello.txt"))
                  (test-assert
                    "git-commit stages and commits with #:stage-all #t"
                    (string-contains (git-commit repo "initial commit" #:stage-all #t) "committed:"))
                  (test-assert "git-log lists committed message"
                               (string-contains (git-log repo) "initial commit")))
                (lambda ()
                  (system* "rm" "-rf" repo)))))

(spec-end)
(spec-exit)