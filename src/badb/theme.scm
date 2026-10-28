;;; badb — Semantic Theming
;;;
;;; Pure-data color theme layer for the TUI and other renderers.
;;; Provides an ncurses-free indirection from semantic color roles
;;; (header, user, error, ...) to (fg bg attrs) color specs so color
;;; schemes become swappable without touching display code.

(define-module (badb theme) #:use-module
               (badb util)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-9)
               #:use-module
               (ice-9 optargs)
               #:export
               (make-theme
                theme?
                theme-name
                theme-roles
                theme-ref
                all-theme-names
                lookup-theme
                default-theme
                theme-role-names))

;;;
;;; Theme Record Definition
;;;
(define-record-type <theme>
  (%make-theme name roles)
  theme?
  (name theme-name)
  (roles theme-roles))

;; Canonical semantic roles every complete theme should define.
(define theme-role-names
  '(frame header status user assistant system notice error shell scheme
    thought tool-call tool-result accent dim selection success warning))

;;;
;;; Spec Normalization
;;;
(define %valid-colors
  '(black red green yellow blue magenta cyan white default))

(define (normalize-color color)
  "Coerce COLOR to a valid color symbol, defaulting to 'default."
  (if (memq color %valid-colors) color 'default))

(define (normalize-attrs attrs)
  "Coerce ATTRS to a canonical attribute form: 'none, a single attribute
symbol, or a list of attribute symbols."
  (cond
   ((memq attrs '(none bold dim reverse)) attrs)
   ((and (list? attrs) (not (null? attrs))
         (every (lambda (a) (memq a '(bold dim reverse))) attrs))
    attrs)
   (else 'none)))

(define (normalize-spec spec)
  "Coerce raw SPEC into canonical (fg bg attrs) list form. Accepts a
full (fg bg attrs) list, shorter (fg) / (fg bg) lists, or a bare color."
  (let ((parts (cond
                ((list? spec) spec)
                ((pair? spec) (list (car spec) (cdr spec)))
                (else (list spec)))))
    (list (normalize-color (if (pair? parts) (car parts) 'default))
          (normalize-color (if (and (pair? parts) (pair? (cdr parts))) (cadr parts) 'default))
          (normalize-attrs (if (and (pair? parts) (pair? (cdr parts)) (pair? (cddr parts)))
                               (caddr parts)
                               'none)))))

(define (normalize-theme-roles roles)
  "Coerce raw ROLES alist into canonical (role fg bg attrs) entries."
  (if (list? roles)
      (filter-map
       (lambda (entry)
         (and (pair? entry)
              (cons (ensure-symbol (car entry)) (normalize-spec (cdr entry)))))
       roles)
      '()))

(define* (make-theme #:key (name "custom") (roles '()))
  "Create a new <theme> record named NAME with ROLES, an alist mapping
role symbols to (fg bg attrs) color specs."
  (%make-theme (ensure-string name) (normalize-theme-roles roles)))

;;;
;;; Built-in Themes
;;;
;; badb-dark mirrors the current TUI color pairs: cyan=info/header,
;; green=user/success, white=assistant, yellow=system/warning, red=error,
;; magenta=shell, blue=frame.
(define %badb-dark-theme
  (make-theme
   #:name "badb-dark"
   #:roles
   '((default white black none)
     (frame blue black none)
     (header cyan black bold)
     (status cyan black none)
     (user green black none)
     (assistant white black none)
     (system yellow black none)
     (notice cyan black none)
     (error red black bold)
     (shell magenta black none)
     (scheme cyan black none)
     (thought white black dim)
     (tool-call cyan black bold)
     (tool-result green black none)
     (accent cyan black bold)
     (dim white black dim)
     (selection black cyan none)
     (success green black none)
     (warning yellow black bold))))

(define %midnight-theme
  (make-theme
   #:name "midnight"
   #:roles
   '((default white black none)
     (frame blue black none)
     (header blue black bold)
     (status blue black none)
     (user cyan black none)
     (assistant white black none)
     (system blue black none)
     (notice cyan black none)
     (error red black bold)
     (shell magenta black none)
     (scheme cyan black none)
     (thought white black dim)
     (tool-call cyan black bold)
     (tool-result blue black none)
     (accent cyan black bold)
     (dim white black dim)
     (selection black blue none)
     (success green black none)
     (warning yellow black none))))

(define %nord-theme
  (make-theme
   #:name "nord"
   #:roles
   '((default white black none)
     (frame blue black none)
     (header cyan black bold)
     (status blue black none)
     (user green black none)
     (assistant white black none)
     (system blue black none)
     (notice cyan black none)
     (error red black none)
     (shell magenta black none)
     (scheme cyan black none)
     (thought white black dim)
     (tool-call cyan black bold)
     (tool-result green black none)
     (accent cyan black bold)
     (dim white black dim)
     (selection black cyan none)
     (success green black none)
     (warning yellow black none))))

(define %solarized-dark-theme
  (make-theme
   #:name "solarized-dark"
   #:roles
   '((default white black none)
     (frame blue black none)
     (header cyan black bold)
     (status blue black none)
     (user green black none)
     (assistant white black none)
     (system yellow black none)
     (notice cyan black none)
     (error red black bold)
     (shell magenta black none)
     (scheme yellow black none)
     (thought white black dim)
     (tool-call cyan black bold)
     (tool-result blue black none)
     (accent yellow black bold)
     (dim white black dim)
     (selection black yellow none)
     (success green black none)
     (warning yellow black bold))))

(define %paper-theme
  (make-theme
   #:name "paper"
   #:roles
   '((default black white none)
     (frame blue white none)
     (header blue white bold)
     (status blue white none)
     (user green white none)
     (assistant black white none)
     (system yellow white none)
     (notice blue white none)
     (error red white bold)
     (shell magenta white none)
     (scheme blue white none)
     (thought black white dim)
     (tool-call blue white bold)
     (tool-result green white none)
     (accent blue white bold)
     (dim black white dim)
     (selection black cyan none)
     (success green white none)
     (warning yellow white bold))))

(define %mono-theme
  (make-theme
   #:name "mono"
   #:roles
   '((default white black none)
     (frame default default none)
     (header white black bold)
     (status white black none)
     (user white black bold)
     (assistant white black none)
     (system default default none)
     (notice white black bold)
     (error white black reverse)
     (shell default default bold)
     (scheme default default bold)
     (thought default default dim)
     (tool-call white black bold)
     (tool-result default default none)
     (accent white black bold)
     (dim default default dim)
     (selection black white none)
     (success white black none)
     (warning white black bold))))

(define %builtin-themes
  (list %badb-dark-theme
        %midnight-theme
        %nord-theme
        %solarized-dark-theme
        %paper-theme
        %mono-theme))

;; The default theme, returnable via (default-theme) and settable via
;; (default-theme NEW-THEME) or parameterize.
(define default-theme
  (make-parameter %badb-dark-theme))

;;;
;;; Lookup & Access
;;;
(define (all-theme-names)
  "Return the list of built-in theme names."
  (map theme-name %builtin-themes))

(define (lookup-theme name)
  "Return the built-in theme named NAME (string or symbol), or #f if unknown."
  (let ((n (string-downcase (ensure-string name))))
    (find (lambda (th) (string=? (string-downcase (theme-name th)) n))
          %builtin-themes)))

(define (theme-ref theme role)
  "Return the (fg bg attrs) color spec for ROLE in THEME.
Missing roles fall back to the theme's 'default role; a missing or
invalid THEME falls back to the current default theme. A theme with no
'default role yields the neutral (default default none) spec."
  (let* ((th (if (theme? theme) theme (default-theme)))
         (r (ensure-symbol role))
         (entry (or (assq r (theme-roles th))
                    (assq 'default (theme-roles th)))))
    (if entry (cdr entry) (list 'default 'default 'none))))
