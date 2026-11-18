(define-module (test surfaces-geis-updater-spec)
               #:use-module
               (srfi srfi-64)
               #:use-module
               (badb spec)
               #:use-module
               (geasa surfaces-overlay plugin)
               #:use-module
               (geasa surfaces-geis-manager plugin)
               #:use-module
               (geasa surfaces-geis-updater plugin)
               #:use-module
               (geasa surfaces-notify plugin)
               #:use-module
               (geasa surfaces-widgets plugin))

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

(test-group "cycle-mode-and-modal-integration"
            (let
                ((ws (mkdtemp "/tmp/badb-updater-ui-XXXXXX")))
              (mkdir (string-append ws "/.badb"))
              (let
                  ((m1 (geis-updater-cycle-mode! 'project ws)))
                (test-equal "cycles notify -> auto" 'auto m1))
              (let
                  ((m2 (geis-updater-cycle-mode! 'project ws)))
                (test-equal "cycles auto -> manual" 'manual m2))
              (let
                  ((m3 (geis-updater-cycle-mode! 'project ws)))
                (test-equal "cycles manual -> notify" 'notify m3))
              (test-assert "persists config.scm on disk"
                           (file-exists? (string-append ws "/.badb/config.scm")))
              (clear-geis-manager-extensions!)
              (register-geis-updater-extensions!)
              (let*
                  ((ov (make-geis-manager-overlay #:scope 'user #:workspace ws)) (lines '()))
                ((overlay-render ov) ov
                 `((draw-ops! .
                              ,(lambda (ops _rect)
                                 (set! lines (render-ops->lines ops 100 28)))))
                 28
                 100
                 0)
                (let ((txt (string-join lines "\n")))
                  (test-assert "modal shows Policy: banner" (string-contains txt "Policy:"))
                  (test-assert "modal shows Status header" (string-contains txt "Status")))
                (test-equal "Tab switches scope to project"
                            'handled
                            ((overlay-on-key ov) ov 9 '((key-tab . 9))))
                (test-equal "scope updated to project after Tab"
                            'project
                            (geis-manager-scope (modal-overlay-state ov))))
              (unregister-geis-updater-extensions!)
              (system* "rm" "-rf" ws)))

(test-group "notify-integration"
            (clear-notifications!)
            (clear-geis-manager-extensions!)
            (register-geis-updater-extensions!)
            (let* ((ws (mkdtemp "/tmp/badb-updater-notify-XXXXXX")))
              (mkdir (string-append ws "/.badb"))
              (let ((ov (make-geis-manager-overlay #:scope 'project #:workspace ws)))
                ((overlay-on-key ov) ov (char->integer #\s) '())
                (let ((hist (notify-history)))
                  (test-assert "sync action emits notification" (pair? hist))
                  (let ((latest (car hist)))
                    (test-equal "notification id is geis-updater:sync:project"
                                "geis-updater:sync:project"
                                (notification-id latest))
                    (test-equal "resolved level is info"
                                'info
                                (notification-level latest))
                    (test-equal "source is surfaces-geis-updater"
                                "surfaces-geis-updater"
                                (notification-source latest)))))
              (unregister-geis-updater-extensions!)
              (clear-notifications!)
              (system* "rm" "-rf" ws)))

(spec-end)
(spec-exit)
