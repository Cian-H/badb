(define-module (test surfaces-drawer-spec)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa surfaces-drawer plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin))

(spec-begin "surfaces-drawer")

(test-group "compute-drawer-layout"
            (call-with-values
              (lambda () (compute-drawer-layout (make-rect 0 0 120 40) #:open? #t))
              (lambda (main-r drawer-r)
                (test-assert "drawer visible on 120 cols" (rect? drawer-r))
                (test-equal "main + drawer = 120" 120 (+ (rect-width main-r) (rect-width drawer-r)))))
            (call-with-values
              (lambda () (compute-drawer-layout (make-rect 0 0 90 40) #:open? #t))
              (lambda (main-r drawer-r)
                (test-equal "auto-collapse < 100 cols" #f drawer-r)
                (test-equal "main gets full width" 90 (rect-width main-r))))
            (call-with-values
              (lambda () (compute-drawer-layout (make-rect 0 0 120 40) #:open? #f))
              (lambda (main-r drawer-r)
                (test-equal "closed drawer is #f" #f drawer-r)
                (test-equal "main gets full width when closed" 120 (rect-width main-r)))))

(test-group "panel-registry-and-state"
            (test-assert "todos registered" (assq 'todos (list-drawer-panels)))
            (test-assert "subagents registered" (assq 'subagents (list-drawer-panels)))
            (test-assert "files registered" (assq 'files (list-drawer-panels)))
            (let ((st (make-drawer-state)))
              (test-equal "starts open" #t (drawer-open? st))
              (test-equal "toggle closes" #f (toggle-drawer! st))
              (test-equal "starts on todos" 'todos (drawer-active-panel st))
              (test-equal "cycle to subagents" 'subagents (cycle-drawer-panel! st))
              (let ((lines (render-widget-to-lines
                             (render-drawer-widget st (make-rect 0 0 36 12)
                                                   #:context '((subagents . ("explorer"))))
                             36 12)))
                (test-assert "drawer widget renders" (> (length lines) 0)))))

(test-group "completion-preview-overlay"
            (let* ((slist (make-selectable-list '("src/a.scm" "src/b.scm")))
                   (ov (make-completion-preview-overlay
                         slist
                         (lambda (it) (string-append "Preview: " it))
                         #:anchor-y 5))
                   (lines '()))
              (test-assert "is overlay" (overlay? ov))
              ((overlay-render ov) ov
               `((draw-ops! . ,(lambda (ops _r) (set! lines (render-ops->lines ops 80 24)))))
               24 80 5)
              (test-assert "rendered non-empty" (> (length lines) 0))
              (test-equal "j moves down" 'handled
                          ((overlay-on-key ov) ov (char->integer #\j) '()))
              (test-equal "q closes" 'close
                          ((overlay-on-key ov) ov (char->integer #\q) '()))))

(test-group "tui-registration"
            (let ((cats '())
                  (cmds '()))
              (register-drawer-tui!
                #:register-category! (lambda (p _d) (set! cats (cons p cats)))
                #:register-command! (lambda (n _d _a . _r) (set! cmds (cons n cmds))))
              (test-assert "View category" (member '("View") cats))
              (test-assert "/drawer command" (member "drawer" cmds))))

(spec-end)
(spec-exit)
