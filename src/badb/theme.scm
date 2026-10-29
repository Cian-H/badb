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
                theme-role-names
                valid-theme-color?))

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

(define (valid-theme-color? color)
  "Return #t when COLOR is a usable theme color: a named color symbol
from %valid-colors, or a #rrggbb hex string for exact palettes."
  (or (memq color %valid-colors)
      (and (string? color)
           (= 7 (string-length color))
           (char=? #\# (string-ref color 0))
           (string->number (substring color 1) 16))))

(define (normalize-color color)
  "Coerce COLOR to a valid color: a named color symbol from
%valid-colors, or a #rrggbb hex string (resolved to raw RGB further
down the render chain, so themes can carry exact palettes like
Tokyo Night's #7aa2f7 blue). Anything else defaults to 'default."
  (if (valid-theme-color? color)
    (if (string? color) (string-downcase color) color)
    'default))

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

;;;;
;;;; Community themes (Helix-style library). Foreground colors use
;;;; exact #rrggbb palettes; dark themes keep bg=black, which the
;;;; notcurses backend renders as the terminal's own (transparent)
;;;; background, while light themes paint their light base opaquely.
;;;;

;; Tokyo Night - the modern blue-heavy night theme
(define %tokyo-night-theme
  (make-theme
   #:name "tokyo-night"
   #:roles
   '((default "#c0caf5" black none)
     (frame "#565f89" black none)
     (header "#7aa2f7" black bold)
     (status "#7dcfff" black none)
     (user "#9ece6a" black none)
     (assistant "#c0caf5" black none)
     (system "#e0af68" black none)
     (notice "#7dcfff" black none)
     (error "#f7768e" black bold)
     (shell "#bb9af7" black none)
     (scheme "#7dcfff" black none)
     (thought "#565f89" black dim)
     (tool-call "#7aa2f7" black bold)
     (tool-result "#9ece6a" black none)
     (accent "#7aa2f7" black bold)
     (dim "#565f89" black dim)
     (selection "#1a1b26" "#7aa2f7" none)
     (success "#9ece6a" black none)
     (warning "#e0af68" black bold))))

;; Tokyo Night Storm - deeper, more muted
(define %tokyo-night-storm-theme
  (make-theme
   #:name "tokyo-night-storm"
   #:roles
   '((default "#a9b1d6" black none)
     (frame "#565f89" black none)
     (header "#7aa2f4" black bold)
     (status "#7dcfff" black none)
     (user "#9ece6a" black none)
     (assistant "#a9b1d6" black none)
     (system "#e0af68" black none)
     (notice "#7dcfff" black none)
     (error "#f7768e" black bold)
     (shell "#9d7cd8" black none)
     (scheme "#7dcfff" black none)
     (thought "#565f89" black dim)
     (tool-call "#7aa2f4" black bold)
     (tool-result "#9ece6a" black none)
     (accent "#7aa2f4" black bold)
     (dim "#565f89" black dim)
     (selection "#24283b" "#7aa2f4" none)
     (success "#9ece6a" black none)
     (warning "#e0af68" black bold))))

;; Tokyo Night Day - the light variant
(define %tokyo-night-day-theme
  (make-theme
   #:name "tokyo-night-day"
   #:roles
   '((default "#3760bf" "#e1e2e7" none)
     (frame "#a1a6c3" "#e1e2e7" none)
     (header "#2e7de9" "#e1e2e7" bold)
     (status "#007197" "#e1e2e7" none)
     (user "#587539" "#e1e2e7" none)
     (assistant "#3760bf" "#e1e2e7" none)
     (system "#8c6c3e" "#e1e2e7" none)
     (notice "#007197" "#e1e2e7" none)
     (error "#f52a65" "#e1e2e7" bold)
     (shell "#9854f1" "#e1e2e7" none)
     (scheme "#007197" "#e1e2e7" none)
     (thought "#a1a6c3" "#e1e2e7" dim)
     (tool-call "#2e7de9" "#e1e2e7" bold)
     (tool-result "#587539" "#e1e2e7" none)
     (accent "#2e7de9" "#e1e2e7" bold)
     (dim "#a1a6c3" "#e1e2e7" dim)
     (selection "#e1e2e7" "#2e7de9" none)
     (success "#587539" "#e1e2e7" none)
     (warning "#8c6c3e" "#e1e2e7" bold))))

;; Catppuccin Mocha - the pastel dark standard
(define %catppuccin-mocha-theme
  (make-theme
   #:name "catppuccin-mocha"
   #:roles
   '((default "#cdd6f4" black none)
     (frame "#a6adc8" black none)
     (header "#89b4fa" black bold)
     (status "#89dceb" black none)
     (user "#a6e3a1" black none)
     (assistant "#cdd6f4" black none)
     (system "#f9e2af" black none)
     (notice "#94e2d5" black none)
     (error "#f38ba8" black bold)
     (shell "#cba6f7" black none)
     (scheme "#94e2d5" black none)
     (thought "#a6adc8" black dim)
     (tool-call "#89b4fa" black bold)
     (tool-result "#a6e3a1" black none)
     (accent "#b4befe" black bold)
     (dim "#a6adc8" black dim)
     (selection "#1e1e2e" "#89b4fa" none)
     (success "#a6e3a1" black none)
     (warning "#fab387" black bold))))

;; Catppuccin Latte - the light counterpart
(define %catppuccin-latte-theme
  (make-theme
   #:name "catppuccin-latte"
   #:roles
   '((default "#4c4f69" "#eff1f5" none)
     (frame "#9ca0b0" "#eff1f5" none)
     (header "#8caaee" "#eff1f5" bold)
     (status "#179299" "#eff1f5" none)
     (user "#40a02b" "#eff1f5" none)
     (assistant "#4c4f69" "#eff1f5" none)
     (system "#df8e1d" "#eff1f5" none)
     (notice "#179299" "#eff1f5" none)
     (error "#e64553" "#eff1f5" bold)
     (shell "#ca9ee6" "#eff1f5" none)
     (scheme "#179299" "#eff1f5" none)
     (thought "#9ca0b0" "#eff1f5" dim)
     (tool-call "#8caaee" "#eff1f5" bold)
     (tool-result "#40a02b" "#eff1f5" none)
     (accent "#babbf1" "#eff1f5" bold)
     (dim "#9ca0b0" "#eff1f5" dim)
     (selection "#eff1f5" "#8caaee" none)
     (success "#40a02b" "#eff1f5" none)
     (warning "#ef9f76" "#eff1f5" bold))))

;; Gruvbox Dark - warm retro groove
(define %gruvbox-dark-theme
  (make-theme
   #:name "gruvbox-dark"
   #:roles
   '((default "#ebdbb2" black none)
     (frame "#928374" black none)
     (header "#83a598" black bold)
     (status "#8ec07c" black none)
     (user "#b8bb26" black none)
     (assistant "#ebdbb2" black none)
     (system "#fabd2f" black none)
     (notice "#8ec07c" black none)
     (error "#fb4917" black bold)
     (shell "#d3869b" black none)
     (scheme "#8ec07c" black none)
     (thought "#928374" black dim)
     (tool-call "#83a598" black bold)
     (tool-result "#b8bb26" black none)
     (accent "#fe8019" black bold)
     (dim "#928374" black dim)
     (selection "#282828" "#fabd2f" none)
     (success "#b8bb26" black none)
     (warning "#fabd2f" black bold))))

;; Gruvbox Light - the daytime groove
(define %gruvbox-light-theme
  (make-theme
   #:name "gruvbox-light"
   #:roles
   '((default "#3c3836" "#fbf1c7" none)
     (frame "#7c6f64" "#fbf1c7" none)
     (header "#458588" "#fbf1c7" bold)
     (status "#689d6a" "#fbf1c7" none)
     (user "#98971a" "#fbf1c7" none)
     (assistant "#3c3836" "#fbf1c7" none)
     (system "#d79921" "#fbf1c7" none)
     (notice "#689d6a" "#fbf1c7" none)
     (error "#cc241d" "#fbf1c7" bold)
     (shell "#b16286" "#fbf1c7" none)
     (scheme "#689d6a" "#fbf1c7" none)
     (thought "#7c6f64" "#fbf1c7" dim)
     (tool-call "#458588" "#fbf1c7" bold)
     (tool-result "#98971a" "#fbf1c7" none)
     (accent "#d65d0e" "#fbf1c7" bold)
     (dim "#7c6f64" "#fbf1c7" dim)
     (selection "#fbf1c7" "#458588" none)
     (success "#98971a" "#fbf1c7" none)
     (warning "#d79921" "#fbf1c7" bold))))

;; Dracula
(define %dracula-theme
  (make-theme
   #:name "dracula"
   #:roles
   '((default "#f8f8f2" black none)
     (frame "#6272a4" black none)
     (header "#bd93f9" black bold)
     (status "#8be9fd" black none)
     (user "#50fa7b" black none)
     (assistant "#f8f8f2" black none)
     (system "#f1fa8c" black none)
     (notice "#8be9fd" black none)
     (error "#ff5555" black bold)
     (shell "#ff79c6" black none)
     (scheme "#8be9fd" black none)
     (thought "#6272a4" black dim)
     (tool-call "#bd93f9" black bold)
     (tool-result "#50fa7b" black none)
     (accent "#bd93f9" black bold)
     (dim "#6272a4" black dim)
     (selection "#282a36" "#bd93f9" none)
     (success "#50fa7b" black none)
     (warning "#ffb86c" black bold))))

;; Kanagawa - the Japanese wave
(define %kanagawa-theme
  (make-theme
   #:name "kanagawa"
   #:roles
   '((default "#dcd7ba" black none)
     (frame "#727169" black none)
     (header "#7e9cd8" black bold)
     (status "#7fb4ca" black none)
     (user "#98bb6c" black none)
     (assistant "#dcd7ba" black none)
     (system "#dca561" black none)
     (notice "#7fb4ca" black none)
     (error "#c34043" black bold)
     (shell "#957fb8" black none)
     (scheme "#7fb4ca" black none)
     (thought "#727169" black dim)
     (tool-call "#7e9cd8" black bold)
     (tool-result "#98bb6c" black none)
     (accent "#ffa066" black bold)
     (dim "#727169" black dim)
     (selection "#1f1f28" "#7e9cd8" none)
     (success "#98bb6c" black none)
     (warning "#ffa066" black bold))))

;; Rose Pine - the soft sunset
(define %rose-pine-theme
  (make-theme
   #:name "rose-pine"
   #:roles
   '((default "#e0def4" black none)
     (frame "#6e6a6f" black none)
     (header "#c4a7e7" black bold)
     (status "#9ccfd8" black none)
     (user "#9ccfd8" black none)
     (assistant "#e0def4" black none)
     (system "#f6c177" black none)
     (notice "#9ccfd8" black none)
     (error "#eb6f92" black bold)
     (shell "#ebbcba" black none)
     (scheme "#9ccfd8" black none)
     (thought "#6e6a6f" black dim)
     (tool-call "#c4a7e7" black bold)
     (tool-result "#9ccfd8" black none)
     (accent "#ebbcba" black bold)
     (dim "#6e6a6f" black dim)
     (selection "#191724" "#c4a7e7" none)
     (success "#9ccfd8" black none)
     (warning "#f6c177" black bold))))

;; Everforest Dark - the calm forest
(define %everforest-dark-theme
  (make-theme
   #:name "everforest-dark"
   #:roles
   '((default "#d3c6aa" black none)
     (frame "#859289" black none)
     (header "#7fbbb3" black bold)
     (status "#83c094" black none)
     (user "#a7c080" black none)
     (assistant "#d3c6aa" black none)
     (system "#dbbc7f" black none)
     (notice "#83c094" black none)
     (error "#e67e80" black bold)
     (shell "#d699b6" black none)
     (scheme "#83c094" black none)
     (thought "#859289" black dim)
     (tool-call "#7fbbb3" black bold)
     (tool-result "#a7c080" black none)
     (accent "#e69875" black bold)
     (dim "#859289" black dim)
     (selection "#2b3339" "#7fbbb3" none)
     (success "#a7c080" black none)
     (warning "#dbbc7f" black bold))))

;; Monokai - the classic editor dark
(define %monokai-theme
  (make-theme
   #:name "monokai"
   #:roles
   '((default "#f8f8f2" black none)
     (frame "#75715e" black none)
     (header "#66d9ef" black bold)
     (status "#66d9ef" black none)
     (user "#a6e22e" black none)
     (assistant "#f8f8f2" black none)
     (system "#e6db74" black none)
     (notice "#66d9ef" black none)
     (error "#f92672" black bold)
     (shell "#ae81ff" black none)
     (scheme "#66d9ef" black none)
     (thought "#75715e" black dim)
     (tool-call "#66d9ef" black bold)
     (tool-result "#a6e22e" black none)
     (accent "#fd971f" black bold)
     (dim "#75715e" black dim)
     (selection "#272822" "#66d9ef" none)
     (success "#a6e22e" black none)
     (warning "#e6db74" black bold))))

;; One Dark - the Atom editor classic
(define %onedark-theme
  (make-theme
   #:name "onedark"
   #:roles
   '((default "#abb2bf" black none)
     (frame "#5c6370" black none)
     (header "#61afef" black bold)
     (status "#56b6c2" black none)
     (user "#98c379" black none)
     (assistant "#abb2bf" black none)
     (system "#e5c07b" black none)
     (notice "#56b6c2" black none)
     (error "#e06c75" black bold)
     (shell "#c678dd" black none)
     (scheme "#56b6c2" black none)
     (thought "#5c6370" black dim)
     (tool-call "#61afef" black bold)
     (tool-result "#98c379" black none)
     (accent "#61afef" black bold)
     (dim "#5c6370" black dim)
     (selection "#282c34" "#61afef" none)
     (success "#98c379" black none)
     (warning "#e5c07b" black bold))))

(define %builtin-themes
  (list %badb-dark-theme
        %midnight-theme
        %nord-theme
        %solarized-dark-theme
        %paper-theme
        %mono-theme
        %tokyo-night-theme
        %tokyo-night-storm-theme
        %tokyo-night-day-theme
        %catppuccin-mocha-theme
        %catppuccin-latte-theme
        %gruvbox-dark-theme
        %gruvbox-light-theme
        %dracula-theme
        %kanagawa-theme
        %rose-pine-theme
        %everforest-dark-theme
        %monokai-theme
        %onedark-theme))

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
