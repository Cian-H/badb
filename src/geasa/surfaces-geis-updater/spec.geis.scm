(define-module (test surfaces-geis-updater-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa surfaces-geis-manager plugin)
               #:use-module (geasa surfaces-geis-updater plugin)
               #:use-module (geasa surfaces-widgets plugin))

(spec-begin "surfaces-geis-updater")

(test-group "extension-registration"
            (clear-geis-manager-extensions!)
            (register-geis-updater-extensions!)
            (test-assert "update-status column registered"
                         (assoc 'update-status (list-geis-manager-columns)))
            (test-assert "updater-summary banner registered"
                         (assoc 'updater-summary (list-geis-manager-banners)))
            (test-assert "check action c registered"
                         (assoc #\c (list-geis-manager-actions)))
            (test-assert "update action u registered"
                         (assoc #\u (list-geis-manager-actions)))
            (test-assert "update-all action U registered"
                         (assoc #\U (list-geis-manager-actions)))
            (test-assert "sync action s registered"
                         (assoc #\s (list-geis-manager-actions)))
            (test-assert "mode action m registered"
                         (assoc #\m (list-geis-manager-actions)))
            (unregister-geis-updater-extensions!)
            (test-assert "update-status column unregistered"
                         (not (assoc 'update-status (list-geis-manager-columns))))
            (test-assert "updater-summary banner unregistered"
                         (not (assoc 'updater-summary (list-geis-manager-banners)))))

(test-group "badges-and-banners"
            (let ((pinned-badge (geis-updater-entry-badge 'user '("demo" (pinned? . #t))))
                  (curr-badge (geis-updater-entry-badge 'user '("demo" (pinned? . #f)))))
              (test-assert "pinned badge is widget" (widget? pinned-badge))
              (test-assert "current badge is widget" (widget? curr-badge)))
            (let ((banner (geis-updater-banner-text 'user "/tmp" 1795014000)))
              (test-assert "banner text is string" (string? banner))
              (test-assert "banner includes Policy:" (string-contains banner "Policy:"))))

(test-group "startup-notices"
            (test-equal "notify notice"
                        "2 geasa update(s) available — /geasa"
                        (geis-updater-startup-notice
                          '(((scope . user) (mode . notify)
                             (entries . (("a" (status . update-available))
                                         ("b" (status . update-available))))))))
            (test-equal "auto notice"
                        "Updated 1 geasa — takes effect next launch"
                        (geis-updater-startup-notice
                          '(((scope . user) (mode . auto)
                             (entries . (("a" (status . updated))))))))
            (test-equal "no updates returns #f"
                        #f
                        (geis-updater-startup-notice
                          '(((scope . user) (mode . notify)
                             (entries . (("a" (status . up-to-date)))))))))

(test-group "cycle-mode"
            (let ((ws (mkdtemp "/tmp/badb-updater-ui-XXXXXX")))
              (mkdir (string-append ws "/.badb"))
              (let ((m1 (geis-updater-cycle-mode! 'project ws)))
                (test-equal "cycles notify -> auto" 'auto m1))
              (let ((m2 (geis-updater-cycle-mode! 'project ws)))
                (test-equal "cycles auto -> manual" 'manual m2))
              (let ((m3 (geis-updater-cycle-mode! 'project ws)))
                (test-equal "cycles manual -> notify" 'notify m3))))

(spec-end)
(spec-exit)
