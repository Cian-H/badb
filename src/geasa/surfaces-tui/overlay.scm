;;; (geasa surfaces-tui overlay) — Compatibility Shim for (geasa surfaces-overlay plugin)
;;;
;;; Re-exports <overlay> protocol and wraps make-menu-overlay with
;;; (geasa surfaces-tui menus) operations for backward compatibility.

(define-module (geasa surfaces-tui overlay)
               #:use-module (geasa surfaces-tui menus)
               #:use-module ((geasa surfaces-overlay plugin)
                             #:select (<overlay>
                                        make-overlay
                                        overlay?
                                        overlay-kind
                                        overlay-data
                                        set-overlay-data!
                                        overlay-render
                                        overlay-on-key
                                        overlay-on-click
                                        menu-overlay?
                                        menu-overlay-menu
                                        set-menu-overlay-menu!
                                        make-modal-overlay
                                        modal-overlay?
                                        modal-overlay-state
                                        set-modal-overlay-state!
                                        modal-active-tab
                                        set-modal-active-tab!
                                        modal-show-detail?
                                        set-modal-show-detail?!
                                        modal-extension-slots
                                        modal-slot-providers
                                        modal-register-slot!
                                        (make-menu-overlay . ov:make-menu-overlay)))
               #:re-export (<overlay>
                             make-overlay
                             overlay?
                             overlay-kind
                             overlay-data
                             set-overlay-data!
                             overlay-render
                             overlay-on-key
                             overlay-on-click
                             menu-overlay?
                             menu-overlay-menu
                             set-menu-overlay-menu!
                             make-modal-overlay
                             modal-overlay?
                             modal-overlay-state
                             set-modal-overlay-state!
                             modal-active-tab
                             set-modal-active-tab!
                             modal-show-detail?
                             set-modal-show-detail?!
                             modal-extension-slots
                             modal-slot-providers
                             modal-register-slot!)
               #:export (make-menu-overlay))

(define %tui-menu-ops
        `((is-menu? . ,menu?)
          (count . ,menu-count)
          (visible-slice . ,menu-visible-slice)
          (item-label . ,menu-item-label)
          (item-desc . ,menu-item-description)
          (title . ,menu-breadcrumb-title)
          (selected-index . ,menu-selected-index)
          (move . ,menu-move)
          (trigger-highlight! . ,menu-trigger-highlight!)
          (pop . ,menu-pop)
          (on-cancel . ,menu-on-cancel)
          (items . ,menu-items)
          (item-action . ,menu-item-action)
          (selected-item . ,menu-selected-item)
          (max-visible . ,%menu-max-visible)))

(define (make-menu-overlay m)
        "Wrap a <menu> M in a pluggable <overlay>."
        (ov:make-menu-overlay m #:menu-ops %tui-menu-ops))
