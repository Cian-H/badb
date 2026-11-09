;;; badb/nc.scm — notcurses backend for the badb TUI, via Guile's FFI.
;;;
;;; Exposes the exact guile-ncurses call surface that (geasa surfaces-tui plugin)
;;; resolves at runtime (initscr, getch, attr-on!, color-pair, KEY_*,
;;; COLOR_* ...), implemented on top of libnotcurses 3.x with
;;; pointer-based FFI calls. The curses attribute/color-pair model is
;;; emulated on notcurses channels:
;;;
;;;   - attr-on!/attr-off! with a color-pair id set plane fg/bg RGB
;;;     from the pair table built by init-pair!; turning a pair off
;;;     restores default colors, exactly like curses attribute bits.
;;;   - A_BOLD maps to NCSTYLE_BOLD; A_REVERSE swaps the tracked
;;;     fg/bg channels (notcurses 3 has no reverse style), resolving
;;;     'default channels to the terminal's concrete default colors
;;;     first so a swap over defaults is visible; A_DIM has
;;;     no notcurses equivalent and is a no-op.
;;;   - getch returns notcurses input ids. Plain codepoints and the
;;;     NCKEY_* special keys are distinct by construction, and modifier
;;;     combos are native: Ctrl+Up/Ctrl+Down are synthesized as the
;;;     KEY_FIND/KEY_OPEN sentinel codes that key-defined reports for
;;;     the xterm escape sequences, so the TUI's resolution logic and
;;;     dispatch work unchanged. Escape arrives as 27 and Tab as 9,
;;;     matching the dispatch's expectations. Mouse reporting is
;;;     disabled at init, so no button events reach the key dispatch.
;;;     Kitty-keyboard release events (delivered after each press when
;;;     the terminal negotiates that protocol) are consumed, so getch
;;;     reports presses only, exactly like curses.
;;;   - Terminal pastes arrive as a dense run of individual key
;;;     events, and notcurses 3.0.16 has no bracketed paste, so a
;;;     paste-burst API batches them instead: input-pending? peeks
;;;     without consuming (lookahead requeues through the pushback
;;;     queue), and drain-input-burst! collects the contiguous
;;;     payload run - pasted newlines count as data, not Enter -
;;;     until a 4ms quiescence re-check, pushing back the first real
;;;     key it meets so getch delivers it intact.
;;;
;;; Library discovery mirrors how the TUI is deployed: the current
;;; guix environment (GUIX_ENVIRONMENT), the user's profile, then the
;;; system loader. The module loads libnotcurses, libnotcurses-core and
;;; libnotcurses-ffi, resolving each symbol from whichever exports it.

(define-module (geasa surfaces-tui nc)
               #:use-module (srfi srfi-1)
               #:use-module (system foreign)
               #:use-module (system foreign-library)
               #:use-module (rnrs bytevectors)
               #:export (initscr endwin cbreak! noecho! keypad! curs-set
                                 erase refresh getmaxy getmaxx move addstr getch
                                 attr-on! attr-off! has-colors? start-color! init-pair!
                                 color-pair nodelay! nodelay
                                 ERR
                                 A_BOLD A_DIM A_REVERSE
                                 KEY_ENTER KEY_BACKSPACE KEY_UP KEY_DOWN KEY_PPAGE KEY_NPAGE
                                 KEY_LEFT KEY_RIGHT KEY_HOME KEY_END KEY_DC KEY_RESIZE
                                 KEY_FIND KEY_OPEN
                                 key-defined define-key
                                 beep
                                 cursor-shape
                                 drain-mouse-events
                                 input-pending? drain-input-burst! pushback-count
                                 COLOR_BLACK COLOR_RED COLOR_GREEN COLOR_YELLOW
                                 COLOR_BLUE COLOR_MAGENTA COLOR_CYAN COLOR_WHITE))

;;;
;;; Library discovery and FFI plumbing
;;;

(define (library-candidates soname)
        (append
          (filter-map
            (lambda (dir) (and dir (string-append dir "/lib/" soname)))
            (list (getenv "GUIX_ENVIRONMENT")
                  (and=> (getenv "HOME")
                         (lambda (h) (string-append h "/.guix-profile")))))
          (list soname)))

(define %handles
        (filter-map
          (lambda (soname)
            (let loop ((paths (library-candidates soname)))
                      (if (null? paths)
                        #f
                        (or (false-if-exception (load-foreign-library (car paths)))
                            (loop (cdr paths))))))
          '("libnotcurses.so.3" "libnotcurses-core.so.3" "libnotcurses-ffi.so.3")))

(when (null? %handles)
  (error "libnotcurses not found; run badb inside ./scripts/devenv"))

;; pointer->procedure needs foreign type *objects* (int, uint32, void
;; are module variables from (system foreign)); only '* is passed as a
;; symbol. Translate our declarative symbols accordingly.
(define (resolve-type t)
        (case t
          ((int) int)
          ((uint32) uint32)
          ((void) void)
          (else t)))

(define (lookup-procedure name ret arg-types)
        (let loop ((handles %handles))
                  (if (null? handles)
                    (error "notcurses symbol not found" name)
                    (or (false-if-exception
                          (pointer->procedure (resolve-type ret)
                                              (dynamic-func name (car handles))
                                              (map resolve-type arg-types)))
                        (loop (cdr handles))))))

(define %init
        (lookup-procedure "notcurses_init" '*
                          '(* *)))
(define %stop
        (lookup-procedure "notcurses_stop" 'int '(*)))
(define %stdplane
        (lookup-procedure "notcurses_stdplane" '* '(*)))
(define %render
        (lookup-procedure "notcurses_render" 'int '(*)))
(define %nc-refresh
        (lookup-procedure "notcurses_refresh" 'int '(* * *)))
(define %get
        (lookup-procedure "notcurses_get" 'uint32 '(* * *)))
(define %get-nblock
        (lookup-procedure "notcurses_get_nblock" 'uint32 '(* *)))
(define %mice-enable
        (lookup-procedure "notcurses_mice_enable" 'int '(* uint32)))
(define %cursor-enable
        (lookup-procedure "notcurses_cursor_enable" 'int '(* int int)))
(define %cursor-disable
        (lookup-procedure "notcurses_cursor_disable" 'int '(*)))
(define %putstr-yx
        (lookup-procedure "ncplane_putstr_yx" 'int (list '* 'int 'int '*)))
(define %plane-erase
        (lookup-procedure "ncplane_erase" 'void '(*)))
(define %plane-dim-yx
        (lookup-procedure "ncplane_dim_yx" 'void '(* * *)))
(define %plane-cursor-yx
        (lookup-procedure "ncplane_cursor_yx" 'void '(* * *)))
(define %on-styles
        (lookup-procedure "ncplane_on_styles" 'void (list '* uint32)))
(define %off-styles
        (lookup-procedure "ncplane_off_styles" 'void (list '* uint32)))
(define %set-fg-rgb
        (lookup-procedure "ncplane_set_fg_rgb" 'int (list '* uint32)))
(define %set-bg-rgb
        (lookup-procedure "ncplane_set_bg_rgb" 'int (list '* uint32)))
(define %set-fg-default
        (lookup-procedure "ncplane_set_fg_default" 'void '(*)))
(define %set-bg-default
        (lookup-procedure "ncplane_set_bg_default" 'void '(*)))
(define %default-fg
        (lookup-procedure "notcurses_default_foreground" 'int '(* *)))
(define %default-bg
        (lookup-procedure "notcurses_default_background" 'int '(* *)))

;; libc's nanosleep, used for the input-drain quiescence windows in
;; initscr. Resolved through the same environment/profile/system
;; search as libnotcurses.
(define %libc
        (let loop ((paths (library-candidates "libc.so.6")))
                  (cond
                    ((null? paths) #f)
                    ((false-if-exception (load-foreign-library (car paths))))
                    (else (loop (cdr paths))))))

(define %nanosleep
        (and %libc
             (pointer->procedure void
                                 (dynamic-func "nanosleep" %libc)
                                 '(* *))))

;; ioctl(fd, TIOCGWINSZ, struct winsize*) to poll the terminal's true
;; geometry. SIGWINCH is delivered to whichever thread is not blocking
;; it, and guile runs worker threads (subprocess pipes), so the main
;; thread's blocking read() is not always interrupted; ncurses polled
;; the tty size itself, and we must do the same to report resizes
;; without waiting for the next keypress.
(define %ioctl
        (and %libc
             (pointer->procedure int
                                 (dynamic-func "ioctl" %libc)
                                 (list int uint64 '*))))
(define %tiocgwinsz #x5413)              ; x86_64 linux
(define %winsize (make-bytevector 8 0))  ; { u16 rows, cols, xpix, ypix }
(define %known-rows 0)
(define %known-cols 0)

(define (sync-known-size!)
        (when (zero? (%ioctl 1 %tiocgwinsz (bytevector->pointer %winsize)))
          (set! %known-rows (bytevector-u16-native-ref %winsize 0))
          (set! %known-cols (bytevector-u16-native-ref %winsize 2))))

(define (terminal-size-changed?)
        (and %ioctl
             (zero? (%ioctl 1 %tiocgwinsz (bytevector->pointer %winsize)))
             (or (not (= (bytevector-u16-native-ref %winsize 0) %known-rows))
                 (not (= (bytevector-u16-native-ref %winsize 2) %known-cols)))))

;; struct timespec on x86_64: { int64 tv_sec; int64 tv_nsec; }
(define %sleep-req (make-bytevector 16 0))

(define (%sleep-ms ms)
        (when %nanosleep
          (bytevector-u64-native-set! %sleep-req 0 0)
          (bytevector-u64-native-set! %sleep-req 8 (* ms 1000000))
          (%nanosleep (bytevector->pointer %sleep-req) %null-pointer)))

;;;
;;; Constants
;;;
;;; NCKEY_* are preterunicode(n) = 1115000 + n from notcurses' nckeys.h;
;;; KEY_FIND/KEY_OPEN are sentinels for Ctrl+Up/Ctrl+Down defined far
;;; outside the preterunicode range so no input collides with them.

(define %preterunicode-base 1115000)
(define %nckey-invalid (+ %preterunicode-base 0))
(define %nckey-mod-ctrl 4)
(define %nctype-release 3)              ; ncintype_e NCTYPE_RELEASE
(define %u32-err 4294967295)            ; (uint32_t)-1, the error return
(define %ncstyle-bold #x02)
(define %ncstyle-none #x00)

(define ERR -1)
(define A_BOLD #x10000)
(define A_DIM  #x20000)
(define A_REVERSE #x40000)

(define KEY_UP (+ %preterunicode-base 2))
(define KEY_RIGHT (+ %preterunicode-base 3))
(define KEY_DOWN (+ %preterunicode-base 4))
(define KEY_LEFT (+ %preterunicode-base 5))
(define KEY_DC (+ %preterunicode-base 7))
(define KEY_BACKSPACE (+ %preterunicode-base 8))
(define KEY_NPAGE (+ %preterunicode-base 9))
(define KEY_PPAGE (+ %preterunicode-base 10))
(define KEY_HOME (+ %preterunicode-base 11))
(define KEY_END (+ %preterunicode-base 12))
(define KEY_ENTER (+ %preterunicode-base 121))
(define KEY_RESIZE (+ %preterunicode-base 1))
;; Sentinels for Ctrl+Up/Ctrl+Down, deliberately far outside the
;; preterunicode range (which tops out at preterunicode(500) =
;; NCKEY_EOF) so no synthesized input can collide with them.
(define KEY_FIND 2000000)
(define KEY_OPEN 2000001)

;; Curses color constants and their RGB equivalents (classic xterm 8).
(define COLOR_BLACK 0)
(define COLOR_RED 1)
(define COLOR_GREEN 2)
(define COLOR_YELLOW 3)
(define COLOR_BLUE 4)
(define COLOR_MAGENTA 5)
(define COLOR_CYAN 6)
(define COLOR_WHITE 7)

(define %color-rgb
        #(#x000000 #xcd0000 #x00cd00 #xcdcd00
          #x0000cd #xcd00cd #x00cdcd #xe5e5e5))

;;;
;;; Emulated curses state
;;;

(define %nc %null-pointer)
(define %plane %null-pointer)
(define %pairs (make-hash-table 61))       ; pair id -> (fg . bg) rgb
(define %fg 'default)
(define %bg 'default)
(define %nodelay? #f)
(define %dim? #f)
(define %cursor-visible? #f)
(define %cursor-y 0)
(define %cursor-x 0)
(define %next-y 0)                          ; pending (move) position
(define %next-x 0)

(define (dimmed-rgb rgb)
        "Halve each RGB channel of RGB, the notcurses-substitute for a dim
text style: curses' A_DIM renders as reduced foreground intensity."
        (logand (ash rgb -1) #x7f7f7f))
(define (apply-foreground!)
        (cond
          ((and (eq? %fg 'default) (not %dim?)) (%set-fg-default %plane))
          ((eq? %fg 'default) (%set-fg-rgb %plane (dimmed-rgb (terminal-default-fg))))
          (%dim? (%set-fg-rgb %plane (dimmed-rgb %fg)))
          (else (%set-fg-rgb %plane %fg))))
(define (apply-background!)
        (if (eq? %bg 'default)
          (%set-bg-default %plane)
          (%set-bg-rgb %plane %bg)))
(define (apply-channels!)
        (apply-foreground!)
        (apply-background!))

;;;
;;; Session lifecycle
;;;

(define (initscr)
        ;; The TUI runs on the alternate screen: on exit the terminal
        ;; restores the pre-TUI screen, so no rendered frame leaks into the
        ;; shell's scrollback (the primary-screen layout left the last frame
        ;; stranded in history, mixing with subsequent shell output).
        (let* ((opts (make-bytevector 64 0))
               ;; NCOPTION_SUPPRESS_BANNERS (0x20). The alternate screen is
               ;; the default; NO_ALTERNATE_SCREEN is deliberately unset.
               (flags #x20))
          (bytevector-u64-native-set! opts 32 flags)
          (set! %nc (%init (bytevector->pointer opts) %null-pointer))
          (when (null-pointer? %nc)
            (error "notcurses_init failed"))
          ;; Mouse support: NCMICE_BUTTON_EVENT (0x2) enables button presses
          ;; and wheel ticks (move/drag events stay off). Mouse events are
          ;; queued by getch and drained by the application via
          ;; drain-mouse-events; they never surface as key codes.
          (%mice-enable %nc 2)
          (set! %plane (%stdplane %nc))
          (set! %fg 'default)
          (set! %bg 'default)
          (set! %pairs (make-hash-table 61))
          (when %ioctl (sync-known-size!))
          ;; Terminals answer notcurses_init's capability/palette queries a
          ;; few milliseconds after init returns, and notcurses only drains
          ;; part of those replies; whatever it misses would surface through
          ;; getch as stray keypresses. Drain until the queue is quiet, like
          ;; curses, whose initscr never leaves reply residue behind.
          (drain-input!)
          %plane))

(define (endwin)
        (unless (null-pointer? %nc)
          ;; Restore the user's default cursor style: DECSCUSR is global to
          ;; the terminal (not per-screen), so without this the TUI's mode
          ;; cursor would outlive the session.
          (cursor-shape 'default)
          (%stop %nc)
          (set! %nc %null-pointer)
          (set! %plane %null-pointer)))

;; notcurses always runs cbreak/noecho with keypad-style decoding; these
;; exist only for call-surface compatibility.
(define (cbreak!) #t)
(define (noecho!) #t)
(define (keypad! win enabled?) #t)
(define (has-colors?) #t)
(define (start-color!) #t)

;;;
;;; Geometry, drawing, rendering
;;;

(define %dim-y (make-bytevector 4))
(define %dim-x (make-bytevector 4))
(define (refresh-dims!)
        (%plane-dim-yx %plane
                       (bytevector->pointer %dim-y)
                       (bytevector->pointer %dim-x)))

(define (getmaxy win) (refresh-dims!) (bytevector-u32-native-ref %dim-y 0))
(define (getmaxx win) (refresh-dims!) (bytevector-u32-native-ref %dim-x 0))

(define (move win y x)
        (set! %next-y y)
        (set! %next-x x)
        ;; The physical cursor sits at the last position drawn or moved to,
        ;; like curses; refresh places it.
        (set! %cursor-y y)
        (set! %cursor-x x)
        #t)

(define (addstr win str)
        (let ((y %next-y) (x %next-x))
          (when (and (>= y 0) (>= x 0))
            (%putstr-yx %plane y x (string->pointer str)))
          (set! %cursor-y y)
          (set! %cursor-x (+ x (string-length str)))
          #t))

(define (erase win) (%plane-erase %plane) #t)

(define (refresh win)
        (let ((r (%render %nc)))
          (when %cursor-visible?
            ;; Clamp the tracked position to the current plane size: after a
            ;; resize the old coordinates can lie outside the plane, and
            ;; notcurses_cursor_enable silently fails on invalid input.
            (refresh-dims!)
            (let ((maxy (bytevector-u32-native-ref %dim-y 0))
                  (maxx (bytevector-u32-native-ref %dim-x 0)))
              (set! %cursor-y (min (max %cursor-y 0) (- maxy 1)))
              (set! %cursor-x (min (max %cursor-x 0) (- maxx 1)))
              (%cursor-enable %nc %cursor-y %cursor-x)))
          r))

(define (curs-set visibility)
        (cond
          ((= visibility 0)
            (set! %cursor-visible? #f)
            (%cursor-disable %nc))
          (else
            (set! %cursor-visible? #t)
            (%cursor-enable %nc %cursor-y %cursor-x)))
        #t)

;; Cursor shapes via DECSCUSR (CSI Ps SP q), supported by xterm,
;; kitty, alacritty, ghostty, foot, gnome-terminal and iTerm2;
;; terminals that don't know the sequence ignore it harmlessly. The
;; bytes go to the shared tty like beep(), between frames - notcurses
;; does not model cursor shape, so there is no diff to conflict with.
;; Shapes are cached: repeated styling calls are no-ops, so the TUI
;; can restyle on every render without spamming the terminal.
(define %cursor-shape-codes
        '((default . 0)
          (block-blink . 1)
          (block . 2)
          (underline-blink . 3)
          (underline . 4)
          (bar-blink . 5)
          (bar . 6)))
(define %last-cursor-shape 'none)

(define (cursor-shape shape)
        "Set the terminal cursor SHAPE - a symbol from %cursor-shape-codes -
via DECSCUSR, written directly to the tty. Shape 'default (Ps 0)
restores the terminal's configured default cursor, which endwin emits
so the TUI never leaves the user's cursor style clobbered."
        (let ((code (assq-ref %cursor-shape-codes shape)))
          (when (and code (not (eq? shape %last-cursor-shape)))
            (catch #t
              (lambda ()
                (format (current-error-port) "\x1b[~a q" code)
                (force-output (current-error-port)))
              (lambda _ #f))
            (set! %last-cursor-shape shape))))

;;;
;;; Attributes and colors (curses emulation)
;;;

(define %default-fg-slot (make-bytevector 4))
(define %default-bg-slot (make-bytevector 4))

;; The terminal's default fg/bg as concrete RGB (from notcurses' OSC
;; 10/11 queries), falling back to the classic xterm defaults when
;; notcurses could not determine them. A_REVERSE over 'default
;; channels needs concrete values: swapping 'default with 'default
;; would render as a no-op.
(define (terminal-default-fg)
        (if (= (%default-fg %nc (bytevector->pointer %default-fg-slot)) 0)
          (logand (bytevector-u32-native-ref %default-fg-slot 0) #xffffff)
          #xe5e5e5))
(define (terminal-default-bg)
        (if (= (%default-bg %nc (bytevector->pointer %default-bg-slot)) 0)
          (logand (bytevector-u32-native-ref %default-bg-slot 0) #xffffff)
          #x000000))

(define (init-pair! id fg bg)
        ;; fg/bg are either palette indices 0..7 (the COLOR_* constants,
        ;; mapped through %color-rgb) or raw 0xRRGGBB integers (exact theme
        ;; palettes like Tokyo Night's blue).
        (hashv-set! %pairs id
                    (cons (if (<= fg 7) (vector-ref %color-rgb fg) fg)
                          (if (<= bg 7) (vector-ref %color-rgb bg) bg)))
        #t)

(define (color-pair id) id)

(define (pair-background rgb)
        "Background channel for a color pair whose RGB bg is CDR of RGB: a
black background is the curses convention for 'whatever the terminal's
background is' (ncurses rendered it through the terminal's default
pair), so it maps to notcurses' transparent default background instead
of painting an opaque black that mismatches the user's terminal. Any
other color is a genuine painted background (selection highlights,
reversed cards) and stays opaque."
        (if (= (cdr rgb) #x000000)
          'default
          (cdr rgb)))

(define (attr-on! win attr)
        (cond
          ((eqv? attr A_BOLD) (%on-styles %plane %ncstyle-bold))
          ((eqv? attr A_REVERSE)
            ;; Substitute 'default channels with the terminal's concrete
            ;; default colors before swapping; plain fg/bg RGB swaps on top
            ;; of that.
            (let ((f (if (eq? %fg 'default) (terminal-default-fg) %fg))
                  (b (if (eq? %bg 'default) (terminal-default-bg) %bg)))
              (set! %fg b)
              (set! %bg f)
              (apply-channels!)))
          ((eqv? attr A_DIM)
            ;; Emulated: halve the foreground intensity while dim is on.
            (set! %dim? #t)
            (apply-foreground!))
          (else
            (let ((rgb (hashv-ref %pairs attr)))
              (when rgb
                (set! %fg (car rgb))
                (set! %bg (pair-background rgb))
                (apply-channels!)))))
        #t)

(define (attr-off! win attr)
        (cond
          ((eqv? attr A_BOLD) (%off-styles %plane %ncstyle-bold))
          ((eqv? attr A_REVERSE)
            ;; attr-on! resolved 'default to concrete RGB, so this swap
            ;; restores the pre-reverse colors (default colors return as
            ;; their RGB equivalents, rendering identically).
            (let ((f %fg) (b %bg))
              (set! %fg b)
              (set! %bg f)
              (apply-channels!)))
          ((eqv? attr A_DIM)
            (set! %dim? #f)
            (apply-foreground!))
          (else
            ;; Turning a color pair off restores default colors.
            (set! %fg 'default)
            (set! %bg 'default)
            (apply-channels!)))
        #t)

;;;
;;; Input
;;;

;; ncinput layout (x86_64, notcurses 3.0.16):
;;   uint32 id (0) | int y (4) | int x (8) | char utf8[5] (12) |
;;   bool alt (17) | bool shift (18) | bool ctrl (19) |
;;   int evtype (20) | unsigned modifiers (24)
;; struct is 52 bytes; 128 leaves ample headroom.
(define %ncinput (make-bytevector 128 0))
(define %ncinput-ptr (bytevector->pointer %ncinput))

;; Pushback queue: events read for lookahead (input-pending? peeks,
;; drain-input-burst! stops on the first non-payload key) are requeued
;; here as (id . copy) pairs, the copy taken at push time so a later
;; read restores the full ncinput struct - y/x, evtype, modifiers and
;; eff_text all keep answering for a pushed-back event.
(define %pushback '())

(define (%ncinput-restore! copy)
        (let loop ((i 0))
                  (when (< i 128)
                    (bytevector-u8-set! %ncinput i (bytevector-u8-ref copy i))
                    (loop (+ i 1)))))

(define (push-back-event! id)
        (set! %pushback
              (append %pushback
                      (list (cons id (bytevector-copy %ncinput))))))

(define (pushback-count)
        "Number of events currently held in the pushback queue. The queue
drains as reads pop from it and drain-input! discards it wholesale;
tests use this to probe that no event was lost or duplicated."
        (length %pushback))

(define (input-pending?)
        "Return #t when another input event is immediately available,
WITHOUT consuming it: a non-empty pushback queue counts, and one
non-blocking peek is made otherwise - a real event found by the peek
is pushed back so the next read delivers it intact. #f when nothing
is queued (or outside a session, before initscr). notcurses reports
\"no input available\" as 0 and errors as (uint32)-1."
        (cond
          ((pair? %pushback) #t)
          ((null-pointer? %nc) #f)
          (else
            (let ((id (%get-nblock %nc %ncinput-ptr)))
              (if (or (= id 0) (= id %u32-err))
                #f
                (begin (push-back-event! id) #t))))))

;; Consume and discard one event if one is immediately available,
;; pushback first: drain-input! must throw input away, unlike the
;; non-consuming input-pending?.
(define (%discard-input-event?)
        (cond
          ((pair? %pushback)
            (set! %pushback (cdr %pushback))
            #t)
          ((null-pointer? %nc) #f)
          (else
            (let ((r (%get-nblock %nc %ncinput-ptr)))
              (not (or (= r 0) (= r %u32-err)))))))

;; Consume everything currently queued, then require two consecutive
;; 50ms windows with no input so replies still in flight are caught.
(define (drain-input!)
        (let drain ((quiet 0))
                   (let flush ()
                              (when (%discard-input-event?) (flush)))
                   (%sleep-ms 50)
                   (cond
                     ((%discard-input-event?) (drain 0))
                     ((< quiet 1) (drain (+ quiet 1)))
                     (else #t))))

(define (input-modifiers)
        (bytevector-u32-native-ref %ncinput 24))

(define (input-release?)
        (= (bytevector-s32-native-ref %ncinput 20) %nctype-release))

(define (input-ctrl?)
        (= (logand (input-modifiers) %nckey-mod-ctrl) %nckey-mod-ctrl))

(define (read-raw-id-nblock)
        "One non-blocking raw event read: a pushed-back event is popped
first, restoring %ncinput from its saved copy so release, modifier
and eff_text queries answer for it, before notcurses is polled. #f
when no event is available or no session is open."
        (cond
          ((pair? %pushback)
            (let ((front (car %pushback)))
              (set! %pushback (cdr %pushback))
              (%ncinput-restore! (cdr front))
              (car front)))
          ((null-pointer? %nc) #f)
          (else
            (let ((r (%get-nblock %nc %ncinput-ptr)))
              (if (or (= r 0) (= r %u32-err)) #f r)))))

(define (read-raw-id)
        (if %nodelay?
          (read-raw-id-nblock)
          ;; Blocking read, emulated as a poll loop: SIGWINCH may be swallowed
          ;; by a non-main thread (see terminal-size-changed?), so an
          ;; unbounded notcurses_get would miss resizes until the next
          ;; keypress. Poll every 20ms: sub-perceptual input latency, and a
          ;; resize is reported within one poll.
          (let poll ()
                    (cond
                      ((read-raw-id-nblock))
                      ((terminal-size-changed?) KEY_RESIZE)
                      (else (%sleep-ms 20) (poll))))))

(define (input-eff-text)  ;; ncinput.eff_text[0] (uint32 at offset 36): the composed codepoint
        ;; taking modifiers into account (shift+1 -> '!', ctrl+d -> 0x04).
        (bytevector-u32-native-ref %ncinput 36))

;; Mouse events: NCKEY_BUTTON1..10 (preterunicode 201..210) arrive in
;; the ncinput struct with the cell coordinates. Button presses and
;; wheel ticks queue as (button-number y x) for the application to
;; drain; releases are skipped. Queuing returns ERR from getch so a
;; click or scroll wakes the blocking read immediately instead of
;; waiting for the next keypress.
(define %mouse-queue '())

(define (queue-mouse-event! id)
        (set! %mouse-queue
              (append %mouse-queue
                      (list (list (- id %preterunicode-base 200)
                                  (bytevector-s32-native-ref %ncinput 4)
                                  (bytevector-s32-native-ref %ncinput 8))))))

(define (drain-mouse-events)
        "Return the queued mouse events as a list of (BUTTON Y X) - button
number 1..10 (4 = wheel up, 5 = wheel down), Y/X in cell coordinates -
and clear the queue."
        (let ((events %mouse-queue))
          (set! %mouse-queue '())
          events))

(define %delivered-keys
        ;; Every synthesized key id this backend hands to the application.
        ;; Anything else in the preterunicode range is skipped by getch.
        (list KEY_UP KEY_RIGHT KEY_DOWN KEY_LEFT KEY_DC KEY_BACKSPACE
              KEY_NPAGE KEY_PPAGE KEY_HOME KEY_END KEY_ENTER KEY_RESIZE
              KEY_FIND KEY_OPEN))

(define (getch win)
        (let loop ((id (read-raw-id)))
                  (cond
                    ((not id) ERR)
                    ;; 0 = no input available, (uint32)-1 = error, and the
                    ;; never-delivered NCKEY_INVALID sentinel: all are ERR.
                    ((or (= id 0) (= id %u32-err) (= id %nckey-invalid)) ERR)
                    ;; Terminals with the kitty keyboard protocol negotiated send a
                    ;; release event after every press; curses has no releases, so
                    ;; skip them lest each keypress dispatch twice.
                    ((input-release?) (loop (read-raw-id)))
                    ;; Mouse: button presses and wheel ticks queue as mouse events;
                    ;; button releases are skipped. ERR wakes the blocking main loop
                    ;; so the app drains the event right away.
                    ((and (>= id (+ %preterunicode-base 201))
                          (<= id (+ %preterunicode-base 210)))
                      (if (input-release?)
                        (loop (read-raw-id))
                        (begin (queue-mouse-event! id) ERR)))
                    ;; Ctrl+Up / Ctrl+Down arrive as NCKEY_UP/DOWN with the ctrl
                    ;; modifier; synthesize the sentinels key-defined reports for
                    ;; the xterm sequences so existing dispatch keeps working.
                    ((and (= id KEY_UP) (input-ctrl?)) KEY_FIND)
                    ((and (= id KEY_DOWN) (input-ctrl?)) KEY_OPEN)
                    ;; notcurses resizes its planes lazily (at the next render);
                    ;; notcurses_refresh applies the new geometry immediately, so
                    ;; the TUI's render that follows this KEY_RESIZE lays out at
                    ;; the correct size. NULL out-params: we read dims via
                    ;; ncplane_dim_yx as usual.
                    ((= id KEY_RESIZE)
                      (catch #t
                        (lambda ()
                          (%nc-refresh %nc %null-pointer %null-pointer)
                          (when %ioctl (sync-known-size!)))
                        (lambda _ #f))
                      KEY_RESIZE)
                    ;; The kitty protocol reports plain keys as their unmodified id
                    ;; plus a modifier bitmask ('!' is 1+shift). Compose to curses
                    ;; semantics:
                    ;;   - notcurses' eff_text is only safe for shift/capslock
                    ;;     composition of printables: for ctrl input it gives the
                    ;;     UPPERCASE letter (ctrl+e -> 'E'), and it must never
                    ;;     override an already-composed control code.
                    ;;   - special NCKEY_* ids never reach here.
                    ;; ctrl+letter is the classic control code (a=1..z=26). notcurses
                    ;; reports it two ways: kitty protocol sends the lowercase id
                    ;; plus the ctrl modifier, while legacy control codes arrive
                    ;; normalized to the UPPERCASE letter with the ctrl modifier
                    ;; (0x05 -> 'E'). Compose both to the control code.
                    ((and (input-ctrl?)
                          (or (and (>= id 97) (<= id 122))
                              (and (>= id 65) (<= id 90))))
                      (if (< id 97) (- id 64) (- id 96)))
                    ((and (not (input-ctrl?))
                          (>= id 32)
                          (< id %preterunicode-base)
                          (> (input-eff-text) 0))
                      (input-eff-text))
                    ;; Synthesized keys we do not deliver (modifier keys like
                    ;; NCKEY_LSHIFT, media keys, mouse buttons, ...) are noise under
                    ;; the kitty protocol; skip them and read the next event rather
                    ;; than leaking a huge codepoint into printable-character
                    ;; handling, where integer->char would crash the app.
                    ((and (>= id %preterunicode-base)
                          (not (memv id %delivered-keys)))
                      (loop (read-raw-id)))
                    (else id))))

;;;
;;; Paste bursts
;;;

;; A terminal paste arrives as a dense run of individual key events;
;; rendering once per event shows one-by-one latency, and a pasted
;; newline (id 10 or 13) is indistinguishable from Enter, so multiline
;; paste would submit mid-paste. notcurses 3.0.16 has no
;; bracketed-paste support, so the burst is bounded by quiescence:
;; drain-input-burst! collects the contiguous run of payload
;; codepoints and pushes back the first real key it meets.

(define (%burst-payload-codepoint id)
        "The paste-payload codepoint for the raw event id ID, or #f when
the event is a real key that must end the burst. Payload: printables
(id 32..preterunicode, normalized through eff_text exactly like
getch, never under ctrl - a ctrl-modified key composes to a control
code in getch and is a real key) plus pasted newlines 10 and 13,
which are collected as data so a multiline paste stays one burst
instead of submitting on its first line."
        (cond
          ((or (= id 10) (= id 13)) id)
          ((and (>= id 32)
                (< id %preterunicode-base)
                (not (input-ctrl?)))
            (let ((eff (input-eff-text)))
              (if (> eff 0) eff id)))
          (else #f)))

(define (drain-input-burst!)
        "Collect the contiguous burst of paste-payload input currently
queued, as codepoint ids in arrival order. Release events are
skipped and interleaved mouse events are queued (as getch would)
without ending the burst; the first event getch would deliver as a
real key - arrows, escape, KEY_ENTER, specials, ctrl compositions -
is pushed back for the next read and ends the burst. An emptied
queue does not end the burst outright: one 4ms quiescence re-check
catches a paste still in flight across two reads, and quiet ends the
burst. Returns '() without waiting when nothing is queued and
nothing has been collected, so a consumer that calls this on regular
typing never blocks."
        (let loop ((acc '()) (slept? #f))
                  (let ((id (read-raw-id-nblock)))
                    (cond
                      ((not id)
                        (if (or slept? (null? acc))
                          (reverse! acc)
                          (begin (%sleep-ms 4) (loop acc #t))))
                      ((input-release?) (loop acc #f))
                      ((and (>= id (+ %preterunicode-base 201))
                            (<= id (+ %preterunicode-base 210)))
                        (queue-mouse-event! id)
                        (loop acc #f))
                      (else
                        (let ((cp (%burst-payload-codepoint id)))
                          (if cp
                            (loop (cons cp acc) #f)
                            (begin (push-back-event! id) (reverse! acc)))))))))

(define (nodelay! win enabled?)
        (set! %nodelay? (if enabled? #t #f))
        #t)
(define nodelay nodelay!)

;; The ctrl-arrow escape sequences are decoded natively by notcurses;
;; key-defined/define-key keep the TUI's startup resolution working.
(define (key-defined seq)
        (cond
          ((string=? seq "\x1b[1;5A") KEY_FIND)
          ((string=? seq "\x1b[1;5B") KEY_OPEN)
          (else #f)))

(define (define-key seq code) #t)

;; Ring the terminal bell: a raw BEL byte on stderr, which shares the
;; tty in TUI usage. Safe to call at any time; never disturbs the
;; rendered frame.
(define (beep)
        (catch #t
          (lambda ()
            (display "\a" (current-error-port))
            (force-output (current-error-port)))
          (lambda _ #f)))
