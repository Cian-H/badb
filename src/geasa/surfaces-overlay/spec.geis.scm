;;; Contract spec for surfaces-overlay

(define-module (test surfaces-overlay-spec)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-64)
               #:use-module (badb spec)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (geasa surfaces-overlay plugin))

(spec-begin "surfaces-overlay")

(test-group "core-overlay-protocol"
            (let ((ov (make-overlay #:kind 'test #:data 42)))
              (test-assert "overlay? true" (overlay? ov))
              (test-equal "overlay-kind" 'test (overlay-kind ov))
              (test-equal "overlay-data" 42 (overlay-data ov))
              (set-overlay-data! ov 99)
              (test-equal "set-overlay-data!" 99 (overlay-data ov))
              (test-equal "default on-key passes" 'pass ((overlay-on-key ov) ov #\a '()))
              (test-equal "default on-click #f" #f ((overlay-on-click ov) ov 'row '()))))

(test-group "menu-overlay"
            (let* ((sl (make-selectable-list (list (cons "alpha" "first") (cons "beta" "second"))))
                   (ov (make-menu-overlay sl))
                   (rendered #f)
                   (clicks '()))
              (test-assert "menu-overlay? true" (menu-overlay? ov))
              (test-assert "menu-overlay-menu" (selectable-list? (menu-overlay-menu ov)))
              ((overlay-render ov)
               ov
               `((draw-popup! . ,(lambda (title rows sel _w _y _x _ab _bl)
                                   (set! rendered (list title (length rows) sel))))
                 (register-click! . ,(lambda (y target)
                                       (set! clicks (cons (cons y target) clicks)))))
               30 80 25)
              (test-equal "menu rendered" '("Menu" 2 0) rendered)
              (test-equal "menu clicks registered" 2 (length clicks))
              (test-equal "menu down key" 'handled ((overlay-on-key ov) ov 'down '((key-down . down))))
              (test-equal "menu moved" 1 (selectable-list-selected-index (menu-overlay-menu ov)))))

(test-group "modal-overlay-and-slots"
            (let* ((closed? #f)
                   (drawn-ops #f)
                   (clicks '())
                   (ov (make-modal-overlay
                         #:title "Test Modal"
                         #:tabs '((user . "User") (project . "Project"))
                         #:state '((count . 0))
                         #:body-renderer (lambda (_ov st _rect)
                                           (text-widget (format #f "count=~a" (assq-ref st 'count))))
                         #:detail-renderer (lambda (_ov _st _rect)
                                             (text-widget "detail-body"))
                         #:footer-hints '(("Enter" . "inspect") ("q" . "close"))
                         #:key-bindings `((#\q . ,(lambda (_ov _st _ctx) 'close))
                                          (#\d . ,(lambda (o _st _ctx)
                                                    (set-modal-show-detail?! o (not (modal-show-detail? o)))
                                                    'handled)))
                         #:on-close (lambda (_ov _st) (set! closed? #t)))))
              (test-assert "modal-overlay? true" (modal-overlay? ov))
              (test-equal "initial tab" 'user (modal-active-tab ov))
              (test-equal "initial detail collapsed" #f (modal-show-detail? ov))
              (modal-register-slot! ov 'top-banner (lambda (_ov _st) (pill-widget "Update available")))
              (modal-register-slot! ov 'footer-actions '(("u" . "update")))
              (test-equal "slot provider count" 1 (length (modal-slot-providers ov 'top-banner)))
              ((overlay-render ov)
               ov
               `((draw-ops! . ,(lambda (ops rect) (set! drawn-ops (cons (length ops) (rect-width rect)))))
                 (register-click! . ,(lambda (y target) (set! clicks (cons (cons y target) clicks)))))
               30 80 28)
              (test-assert "modal rendered draw-ops" (and (pair? drawn-ops) (> (car drawn-ops) 0)))
              (test-assert "modal clicks registered" (> (length clicks) 0))
              (test-equal "tab switches active tab" 'handled ((overlay-on-key ov) ov 9 '()))
              (test-equal "tab switched to project" 'project (modal-active-tab ov))
              (test-equal "d toggles detail" 'handled ((overlay-on-key ov) ov (char->integer #\d) '()))
              (test-equal "detail expanded" #t (modal-show-detail? ov))
              (test-equal "q closes modal" 'close ((overlay-on-key ov) ov (char->integer #\q) '()))
              (test-assert "on-close fired" closed?)))

(test-group "overlay-stack-layering"
            (let* ((stack (make-overlay-stack))
                   (render-order '())
                   (floating-keyed? #f)
                   (m1 (make-modal-overlay #:title "Base"
                                           #:key-bindings `((#\x . ,(lambda (_o _s _c) 'handled)))))
                   (m2 (make-modal-overlay #:title "Top"
                                           #:key-bindings `((#\z . ,(lambda (_o _s _c) 'close)))))
                   (fl (make-overlay #:kind 'floating
                                     #:render (lambda (_ov _ctx _my _mx _ay)
                                                (set! render-order (append render-order '(floating)))
                                                #t)
                                     #:on-key (lambda (_ov _ch _ctx)
                                                (set! floating-keyed? #t)
                                                'handled))))
              (test-assert "overlay-stack?" (overlay-stack? stack))
              (test-equal "empty top modal is #f" #f (overlay-stack-top-modal stack))
              (overlay-stack-add-floating! stack fl)
              (overlay-stack-push-modal! stack m1)
              (overlay-stack-push-modal! stack m2)
              (test-equal "top modal is m2" m2 (overlay-stack-top-modal stack))
              (test-equal "2 modals" 2 (length (overlay-stack-modals stack)))
              (test-equal "1 floating" 1 (length (overlay-stack-floatings stack)))
              (overlay-stack-render stack
                                    `((draw-ops! . ,(lambda (_ops _r)
                                                      (set! render-order (append render-order '(modal))))))
                                    30 80 28)
              (test-equal "modals render before floating" '(modal modal floating) render-order)
              ;; Key goes to m2, not m1 or floating
              (test-equal "x passes m2" 'pass (overlay-stack-handle-key stack (char->integer #\x) '()))
              (test-assert "floating never captured key" (not floating-keyed?))
              ;; z closes m2 and pops back to m1
              (test-equal "z closes m2" 'close (overlay-stack-handle-key stack (char->integer #\z) '()))
              (test-equal "focus returned to m1" m1 (overlay-stack-top-modal stack))
              (test-equal "x now handled by m1" 'handled (overlay-stack-handle-key stack (char->integer #\x) '()))
              (overlay-stack-remove-floating! stack fl)
              (test-equal "floating removed" 0 (length (overlay-stack-floatings stack)))))

(test-group "confirm-and-input-dialogs"
            (let* ((stack (make-overlay-stack))
                   (confirmed? #f)
                   (cancelled? #f)
                   (dlg-ok (make-confirm-dialog #:title "Delete?"
                                                #:message "Are you sure?"
                                                #:danger? #t
                                                #:on-confirm (lambda () (set! confirmed? #t))))
                   (dlg-no (make-confirm-dialog #:title "Cancel?"
                                                #:message "Keep?"
                                                #:on-cancel (lambda () (set! cancelled? #t)))))
              (overlay-stack-push-modal! stack dlg-ok)
              (overlay-stack-handle-key stack (char->integer #\y) '())
              (test-assert "y confirmed" confirmed?)
              (test-equal "confirm popped" #f (overlay-stack-top-modal stack))
              (overlay-stack-push-modal! stack dlg-no)
              (overlay-stack-handle-key stack 27 '())
              (test-assert "esc cancelled" cancelled?)
              (test-equal "cancel popped" #f (overlay-stack-top-modal stack)))
            (let* ((stack (make-overlay-stack))
                   (submitted #f)
                   (lines '())
                   (dlg (make-input-dialog #:title "Name"
                                           #:prompt "Enter name:"
                                           #:validator (lambda (s)
                                                         (if (< (string-length s) 2)
                                                           "Too short"
                                                           #f))
                                           #:on-submit (lambda (s) (set! submitted s)))))
              (overlay-stack-push-modal! stack dlg)
              ;; Type 'a', press Enter -> validation error keeps dialog open
              (overlay-stack-handle-key stack (char->integer #\a) '())
              (overlay-stack-handle-key stack 10 '())
              (test-equal "validation blocks submit" #f submitted)
              (test-equal "dialog still open" dlg (overlay-stack-top-modal stack))
              (overlay-stack-render stack
                                    `((draw-ops! . ,(lambda (ops _r)
                                                      (set! lines (render-ops->lines ops 80 30)))))
                                    30 80 28)
              (test-assert "validation error rendered"
                           (any (lambda (l) (string-contains l "Too short")) lines))
              ;; Type 'b', press Enter -> submits & pops
              (overlay-stack-handle-key stack (char->integer #\b) '())
              (overlay-stack-handle-key stack 10 '())
              (test-equal "submitted ab" "ab" submitted)
              (test-equal "input dialog popped" #f (overlay-stack-top-modal stack))))

(test-group "modal-filterable-support"
            (let* ((sl (make-selectable-list '("alpha" "beta" "surfaces-tui")))
                   (lines '())
                   (ov (make-modal-overlay
                         #:title "Filterable"
                         #:filterable? #t
                         #:filter-key-fn (lambda (x) x)
                         #:body-renderer (lambda (_ov _st _rect)
                                           (selectable-list-widget sl)))))
              (test-equal "/ activates filter" 'handled ((overlay-on-key ov) ov (char->integer #\/) '()))
              (test-assert "filter is active" (filter-state-active? (modal-filter-state ov)))
              ((overlay-on-key ov) ov (char->integer #\t) '())
              ((overlay-on-key ov) ov (char->integer #\u) '())
              (test-equal "query is tu" "tu" (modal-filter-query ov))
              ((overlay-render ov) ov
               `((draw-ops! . ,(lambda (ops _r) (set! lines (render-ops->lines ops 80 24)))))
               24 80 24)
              (test-assert "filter input bar rendered"
                           (any (lambda (l) (string-contains l "/ tu")) lines))
              (test-assert "matching item rendered"
                           (any (lambda (l) (string-contains l "surfaces-tui")) lines))
              (test-assert "non-matching item filtered out"
                           (not (any (lambda (l) (string-contains l "alpha")) lines)))
              ;; Enter locks filter; first Esc clears filter; second Esc closes modal
              ((overlay-on-key ov) ov 10 '())
              (test-equal "first esc clears filter" 'handled ((overlay-on-key ov) ov 27 '()))
              (test-equal "query cleared" "" (modal-filter-query ov))
              (test-equal "second esc closes modal" 'close ((overlay-on-key ov) ov 27 '()))))

(spec-end)
(spec-exit)
