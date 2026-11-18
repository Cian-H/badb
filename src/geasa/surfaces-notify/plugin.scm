;;; (geasa surfaces-notify plugin) — Thread-Safe Notification Bus, Floating Toasts & History Modal
;;;
;;; Provides a mutex-guarded notification store (<notification>, notify!,
;;; notify-info!, notify-warn!, notify-error!, notify-progress!, notify-resolve!,
;;; notify-dismiss!, notify-tick!, notify-active, notify-history,
;;; clear-notifications!), OSC 9/777 desktop attention formatting
;;; (format-osc-desktop-notify), passive 'floating corner toast overlay
;;; (make-notify-floating-overlay), and /notifications history modal
;;; (make-notifications-modal-overlay, register-notifications-tui!).

(define-module (geasa surfaces-notify plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (ice-9 format)
               #:use-module (ice-9 threads)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (<notification>
                          make-notification
                          notification?
                          notification-id
                          notification-level
                          notification-title
                          notification-message
                          notification-created-at
                          notification-updated-at
                          notification-ttl-seconds
                          notification-source
                          notification-spinner-frame
                          current-notify-clock
                          notify!
                          notify-info!
                          notify-warn!
                          notify-error!
                          notify-progress!
                          notify-resolve!
                          notify-dismiss!
                          notify-tick!
                          notify-active
                          notify-history
                          clear-notifications!
                          format-osc-desktop-notify
                          make-notify-floating-overlay
                          make-notifications-modal-overlay
                          register-notifications-tui!))

;;;
;;; Notification Record & Store State
;;;
(define-record-type <notification>
                    (%make-notification id level title message created-at updated-at ttl-seconds source spinner-frame)
                    notification?
                    (id notification-id)
                    (level notification-level)
                    (title notification-title)
                    (message notification-message)
                    (created-at notification-created-at)
                    (updated-at notification-updated-at)
                    (ttl-seconds notification-ttl-seconds)
                    (source notification-source)
                    (spinner-frame notification-spinner-frame))

(define* (make-notification id level message
                            #:key
                            (title #f)
                            (created-at 0)
                            (updated-at created-at)
                            (ttl-seconds #f)
                            (source #f)
                            (spinner-frame 0))
         "Construct an immutable <notification> record."
         (%make-notification id level title (ensure-string (or message ""))
                             created-at updated-at ttl-seconds source spinner-frame))

(define %default-history-limit 100)
(define %braille-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))

(define current-notify-clock
        (make-parameter current-time))

(define %notify-mutex (make-mutex))
(define %notify-seq 0)
(define %notify-active '())
(define %notify-history '())

(define (default-ttl-for-level level)
        (case level
          ((info) 4)
          ((warn) 6)
          ((error) 10)
          ((progress) #f)
          (else 4)))

(define (next-notify-id!)
        (set! %notify-seq (+ %notify-seq 1))
        (format #f "notif-~a" %notify-seq))

(define (take-up-to lst n)
        (let loop ((rest lst) (k n) (acc '()))
                  (if (or (null? rest) (<= k 0))
                    (reverse acc)
                    (loop (cdr rest) (- k 1) (cons (car rest) acc)))))

(define (replace-or-prepend lst notif)
        (let ((id (notification-id notif)))
          (cons notif
                (filter (lambda (n) (not (equal? (notification-id n) id))) lst))))

(define (record-history! notif)
        (set! %notify-history
              (take-up-to (replace-or-prepend %notify-history notif)
                          %default-history-limit)))

(define* (notify! message
                  #:key
                  (level 'info)
                  (title #f)
                  (ttl #f)
                  (source #f)
                  (id #f)
                  (now #f))
         "Push or update a notification in the thread-safe bus and return its ID."
         (with-mutex %notify-mutex
                     (let* ((ts (or now ((current-notify-clock))))
                            (nid (or id (next-notify-id!)))
                            (existing (find (lambda (n) (equal? (notification-id n) nid)) %notify-active))
                            (created (if existing (notification-created-at existing) ts))
                            (resolved-ttl (if (eq? ttl 'none)
                                            #f
                                            (or ttl
                                                (and existing
                                                     (eq? level (notification-level existing))
                                                     (notification-ttl-seconds existing))
                                                (default-ttl-for-level level))))
                            (frame (if existing (notification-spinner-frame existing) 0))
                            (notif (%make-notification nid
                                                       level
                                                       (or title (and existing (notification-title existing)))
                                                       (ensure-string (or message ""))
                                                       created
                                                       ts
                                                       resolved-ttl
                                                       (or source (and existing (notification-source existing)))
                                                       frame)))
                       (set! %notify-active (replace-or-prepend %notify-active notif))
                       (record-history! notif)
                       nid)))

(define* (notify-info! message #:key (title #f) (ttl #f) (source #f) (id #f) (now #f))
         "Emit an 'info notification and return its ID."
         (notify! message #:level 'info #:title title #:ttl ttl #:source source #:id id #:now now))

(define* (notify-warn! message #:key (title #f) (ttl #f) (source #f) (id #f) (now #f))
         "Emit a 'warn notification and return its ID."
         (notify! message #:level 'warn #:title title #:ttl ttl #:source source #:id id #:now now))

(define* (notify-error! message #:key (title #f) (ttl #f) (source #f) (id #f) (now #f))
         "Emit an 'error notification and return its ID."
         (notify! message #:level 'error #:title title #:ttl ttl #:source source #:id id #:now now))

(define* (notify-progress! id message #:key (title #f) (source #f) (now #f))
         "Create or update an in-place 'progress notification with no TTL until resolved."
         (with-mutex %notify-mutex
                     (let* ((ts (or now ((current-notify-clock))))
                            (nid (or id (next-notify-id!)))
                            (existing (find (lambda (n) (equal? (notification-id n) nid)) %notify-active))
                            (created (if existing (notification-created-at existing) ts))
                            (frame (if existing (+ 1 (notification-spinner-frame existing)) 0))
                            (notif (%make-notification nid
                                                       'progress
                                                       (or title (and existing (notification-title existing)))
                                                       (ensure-string (or message ""))
                                                       created
                                                       ts
                                                       #f
                                                       (or source (and existing (notification-source existing)))
                                                       frame)))
                       (set! %notify-active (replace-or-prepend %notify-active notif))
                       (record-history! notif)
                       nid)))

(define* (notify-resolve! id message #:key (level 'info) (ttl #f) (title #f) (source #f) (now #f))
         "Morph an in-progress notification ID into a resolved state with TTL fade."
         (with-mutex %notify-mutex
                     (let* ((ts (or now ((current-notify-clock))))
                            (existing (or (find (lambda (n) (equal? (notification-id n) id)) %notify-active)
                                          (find (lambda (n) (equal? (notification-id n) id)) %notify-history)))
                            (created (if existing (notification-created-at existing) ts))
                            (resolved-ttl (or ttl (default-ttl-for-level level)))
                            (notif (%make-notification id
                                                       level
                                                       (or title (and existing (notification-title existing)))
                                                       (ensure-string (or message ""))
                                                       created
                                                       ts
                                                       resolved-ttl
                                                       (or source (and existing (notification-source existing)))
                                                       (if existing (notification-spinner-frame existing) 0))))
                       (set! %notify-active (replace-or-prepend %notify-active notif))
                       (record-history! notif)
                       id)))

(define (notify-dismiss! id)
        "Dismiss notification ID from the active toast list."
        (with-mutex %notify-mutex
                    (set! %notify-active
                          (filter (lambda (n) (not (equal? (notification-id n) id))) %notify-active))
                    #t))

(define* (notify-tick! #:optional (now #f))
         "Expire stale active notifications at NOW and advance spinner frames on 'progress items."
         (with-mutex %notify-mutex
                     (let ((ts (or now ((current-notify-clock)))))
                       (set! %notify-active
                             (filter-map
                               (lambda (n)
                                 (let ((ttl (notification-ttl-seconds n)))
                                   (cond
                                     ((and (number? ttl) (>= (- ts (notification-updated-at n)) ttl))
                                       #f)
                                     ((eq? (notification-level n) 'progress)
                                       (%make-notification (notification-id n)
                                                           (notification-level n)
                                                           (notification-title n)
                                                           (notification-message n)
                                                           (notification-created-at n)
                                                           (notification-updated-at n)
                                                           ttl
                                                           (notification-source n)
                                                           (+ 1 (notification-spinner-frame n))))
                                     (else n))))
                               %notify-active))
                       %notify-active)))

(define (notify-active)
        "Return a snapshot list of currently active <notification> records."
        (with-mutex %notify-mutex
                    (list-copy %notify-active)))

(define (notify-history)
        "Return a snapshot list of recent <notification> records (newest first, max 100)."
        (with-mutex %notify-mutex
                    (list-copy %notify-history)))

(define (clear-notifications!)
        "Clear all active notifications and history."
        (with-mutex %notify-mutex
                    (set! %notify-active '())
                    (set! %notify-history '())
                    #t))

;;;
;;; OSC 9 / OSC 777 Desktop Attention Helper
;;;
(define (sanitize-osc-text s)
        (string-map
          (lambda (ch)
            (let ((code (char->integer ch)))
              (if (or (< code 32) (= code 127))
                #\space
                ch)))
          (ensure-string (or s ""))))

(define (sanitize-osc777-field s)
        (string-map
          (lambda (ch)
            (if (char=? ch #\;) #\: ch))
          (sanitize-osc-text s)))

(define* (format-osc-desktop-notify title body #:key (style 'osc9))
         "Format a terminal desktop notification escape sequence (OSC 9 or OSC 777)."
         (let* ((esc (string (integer->char 27)))
                (bel (string (integer->char 7)))
                (clean-title (sanitize-osc-text title))
                (clean-body (sanitize-osc-text body)))
           (case style
             ((osc777)
               (string-append esc "]777;notify;"
                              (sanitize-osc777-field clean-title)
                              ";"
                              (sanitize-osc777-field clean-body)
                              bel))
             (else
               (let ((msg (cond
                            ((and (> (string-length clean-title) 0)
                                  (> (string-length clean-body) 0))
                              (string-append clean-title ": " clean-body))
                            ((> (string-length clean-body) 0) clean-body)
                            (else clean-title))))
                 (string-append esc "]9;" msg bel))))))

;;;
;;; Passive Floating Corner Toast Overlay
;;;
(define (notify-level-glyph notif)
        (case (notification-level notif)
          ((progress)
            (list-ref %braille-frames
                      (modulo (notification-spinner-frame notif) (length %braille-frames))))
          ((warn) "⚠")
          ((error) "✗")
          (else "●")))

(define (notify-level-role level)
        (case level
          ((warn) 'warn)
          ((error) 'error)
          ((progress) 'accent)
          (else 'info)))

(define (wrap-toast-text str width)
        (let* ((w (max 1 width))
               (s (ensure-string (or str "")))
               (len (string-length s)))
          (if (<= len w)
            (list s)
            (let loop ((idx 0) (acc '()))
                      (if (>= idx len)
                        (reverse acc)
                        (let ((end (min len (+ idx w))))
                          (loop end (cons (substring s idx end) acc))))))))

(define (build-toast-widget notif box-w)
        (let* ((inner-w (max 8 (- box-w 4)))
               (lvl (notification-level notif))
               (role (notify-level-role lvl))
               (glyph (notify-level-glyph notif))
               (ttl (notification-title notif))
               (msg (notification-message notif))
               (hdr-text (if (and (string? ttl) (> (string-length ttl) 0))
                           (clip-str (string-append glyph " " ttl) inner-w)
                           #f))
               (body-lines (wrap-toast-text
                             (if hdr-text msg (string-append glyph " " msg))
                             inner-w))
               (body-widgets
                 (map (lambda (line)
                        (text-widget line #:role (if hdr-text 'default role)))
                      (take-up-to body-lines 3)))
               (children (if hdr-text
                           (cons (text-widget hdr-text #:role role #:bold? #t) body-widgets)
                           body-widgets)))
          (box-widget (vstack-widget children) #:role role)))

(define (compute-toast-origin anchor maxy maxx box-w total-h)
        (let ((x (case anchor
                   ((top-left bottom-left) 1)
                   (else (max 1 (- maxx box-w 1)))))
              (y (case anchor
                   ((bottom-left bottom-right) (max 1 (- maxy total-h 2)))
                   (else 1))))
          (cons x y)))

(define* (make-notify-floating-overlay #:key
                                       (anchor 'top-right)
                                       (max-visible 4)
                                       (max-width 48)
                                       (clock current-time))
         "Create a passive 'floating <overlay> rendering active notifications in a corner."
         (make-overlay
           #:kind 'floating
           #:data anchor
           #:render
           (lambda (_ov draw-ctx maxy maxx _anchor-y)
             (let* ((now (clock))
                    (active (take-up-to (notify-tick! now) (max 1 max-visible)))
                    (draw-ops! (assq-ref draw-ctx 'draw-ops!)))
               (if (or (null? active) (not draw-ops!) (< maxx 16) (< maxy 5))
                 #f
                 (let* ((box-w (min max-width (max 16 (- maxx 2))))
                        (cards (map (lambda (n) (build-toast-widget n box-w)) active))
                        (stack (vstack-widget cards))
                        (measured (widget-measure-size stack box-w (max 3 (- maxy 2))))
                        (total-h (min (max 3 (cdr measured)) (max 3 (- maxy 2))))
                        (origin (compute-toast-origin anchor maxy maxx box-w total-h))
                        (rect (make-rect (car origin) (cdr origin) box-w total-h))
                        (ops (widget-render-rect stack rect)))
                   (draw-ops! ops rect)
                   #t))))
           #:on-key (lambda (_ov _ch _ctx) 'pass)
           #:on-click (lambda (_ov _target _ctx) #f)))

;;;
;;; /notifications History Modal
;;;
(define-record-type <notifications-modal-state>
                    (%make-notifications-modal-state list-state filter-query filter-active? clock status-message)
                    notifications-modal-state?
                    (list-state notif-modal-list set-notif-modal-list!)
                    (filter-query notif-modal-filter-query set-notif-modal-filter-query!)
                    (filter-active? notif-modal-filter-active? set-notif-modal-filter-active?!)
                    (clock notif-modal-clock)
                    (status-message notif-modal-status-message set-notif-modal-status-message!))

(define (notif-matches-query? n query)
        (if (or (not (string? query)) (string=? query ""))
          #t
          (let ((q (string-downcase query)))
            (any (lambda (field)
                   (and (string? field)
                        (string-contains (string-downcase field) q)))
                 (list (symbol->string (notification-level n))
                       (or (notification-title n) "")
                       (notification-message n)
                       (or (notification-source n) ""))))))

(define (notif-modal-refresh! st)
        (let* ((q (notif-modal-filter-query st))
               (items (filter (lambda (n) (notif-matches-query? n q)) (notify-history)))
               (old-idx (selectable-list-selected-index (notif-modal-list st)))
               (slist (make-selectable-list items #:selected-index old-idx #:max-visible 12)))
          (set-notif-modal-list! st slist)
          st))

(define (format-notif-age ts now)
        (let ((diff (max 0 (- now ts))))
          (cond
            ((< diff 60) (format #f "~as" diff))
            ((< diff 3600) (format #f "~am" (quotient diff 60)))
            (else (format #f "~ah" (quotient diff 3600))))))

(define (render-notif-row n active-ids now width)
        (let* ((id (notification-id n))
               (lvl (notification-level n))
               (active? (member id active-ids))
               (age (format-notif-age (notification-updated-at n) now))
               (ttl (or (notification-title n) ""))
               (msg (notification-message n))
               (label (if (> (string-length ttl) 0)
                        (string-append ttl ": " msg)
                        msg))
               (badge (status-badge-widget (notify-level-role lvl)
                                           (symbol->string lvl)
                                           #:glyph (notify-level-glyph n)))
               (state-str (if active? "live" age))
               (msg-w (max 8 (- width 24))))
          (hstack-widget
            (list badge
                  (text-widget (pad-str label msg-w))
                  (text-widget (pad-str state-str 7) #:role 'dim #:dim? #t))
            #:gap 1)))

(define (render-notifications-body _ov st rect)
        (let* ((w (rect-width rect))
               (slist (notif-modal-list st))
               (now ((notif-modal-clock st)))
               (active-ids (map notification-id (notify-active)))
               (q (notif-modal-filter-query st))
               (f-active? (notif-modal-filter-active? st))
               (filter-bar
                 (if (or f-active? (> (string-length q) 0))
                   (list (text-widget (string-append "/ " q (if f-active? "▏" ""))
                                      #:role (if f-active? 'accent 'dim)))
                   '()))
               (list-w
                 (if (null? (selectable-list-items slist))
                   (text-widget "  No notifications recorded." #:role 'dim #:dim? #t)
                   (selectable-list-widget
                     slist
                     #:item-renderer
                     (lambda (n _selected?)
                       (render-notif-row n active-ids now w)))))
               (status (notif-modal-status-message st))
               (status-w (if (and (string? status) (> (string-length status) 0))
                           (list (text-widget status #:role 'info))
                           '())))
          (vstack-widget (append filter-bar (list list-w) status-w))))

(define (render-notifications-detail _ov st rect)
        (let* ((n (list-selected-item (notif-modal-list st)))
               (w (rect-width rect)))
          (if (not n)
            (text-widget "No notification selected." #:role 'dim #:dim? #t)
            (let* ((src (or (notification-source n) "system"))
                   (ttl (or (notification-title n) "(none)"))
                   (hdr (format #f "[~a] ~a  (id: ~a, source: ~a)"
                                (notification-level n) ttl (notification-id n) src))
                   (lines (wrap-toast-text (notification-message n) (max 10 (- w 2)))))
              (vstack-widget
                (cons (text-widget (clip-str hdr w) #:bold? #t)
                      (map (lambda (ln) (text-widget ln)) (take-up-to lines 6))))))))

(define (handle-notif-filter-key ov st ch key-ctx)
        (let ((key-enter (assq-ref key-ctx 'key-enter))
              (key-bs (assq-ref key-ctx 'key-backspace)))
          (cond
            ((eqv? ch 27)
              (set-notif-modal-filter-query! st "")
              (set-notif-modal-filter-active?! st #f)
              (notif-modal-refresh! st)
              'handled)
            ((or (eqv? ch key-enter) (eqv? ch 10) (eqv? ch 13))
              (set-notif-modal-filter-active?! st #f)
              'handled)
            ((or (and key-bs (eqv? ch key-bs)) (eqv? ch 8) (eqv? ch 127))
              (let ((q (notif-modal-filter-query st)))
                (when (> (string-length q) 0)
                  (set-notif-modal-filter-query! st (substring q 0 (- (string-length q) 1)))
                  (notif-modal-refresh! st)))
              'handled)
            ((and (integer? ch) (>= ch 32) (< ch 127))
              (set-notif-modal-filter-query!
                st
                (string-append (notif-modal-filter-query st) (string (integer->char ch))))
              (notif-modal-refresh! st)
              'handled)
            (else 'handled))))

(define (make-notif-modal-on-key base-on-key)
        (lambda (ov ch key-ctx)
          (let ((st (modal-overlay-state ov)))
            (if (notif-modal-filter-active? st)
              (handle-notif-filter-key ov st ch key-ctx)
              (cond
                ((and (eqv? ch 27) (> (string-length (notif-modal-filter-query st)) 0))
                  (set-notif-modal-filter-query! st "")
                  (notif-modal-refresh! st)
                  'handled)
                (else (base-on-key ov ch key-ctx)))))))

(define* (make-notifications-modal-overlay #:key (clock current-time))
         "Create the /notifications history modal <overlay>."
         (let* ((st (%make-notifications-modal-state
                      (make-selectable-list '() #:max-visible 12)
                      ""
                      #f
                      clock
                      #f))
                (_ (notif-modal-refresh! st))
                (ov
                  (make-modal-overlay
                    #:title "Notifications"
                    #:state st
                    #:width 0.85
                    #:height 0.75
                    #:body-renderer render-notifications-body
                    #:detail-renderer render-notifications-detail
                    #:show-detail? #t
                    #:footer-hints '(("j/k" . "nav")
                                     ("/" . "filter")
                                     ("Enter" . "detail")
                                     ("d" . "dismiss")
                                     ("c" . "clear")
                                     ("q" . "close"))
                    #:key-bindings
                    (list
                      (cons (list #\j 'down)
                            (lambda (_o s _ctx)
                              (set-notif-modal-list! s (list-move (notif-modal-list s) 1))
                              'handled))
                      (cons (list #\k 'up)
                            (lambda (_o s _ctx)
                              (set-notif-modal-list! s (list-move (notif-modal-list s) -1))
                              'handled))
                      (cons #\/
                            (lambda (_o s _ctx)
                              (set-notif-modal-filter-active?! s #t)
                              'handled))
                      (cons 'enter
                            (lambda (o _s _ctx)
                              (set-modal-show-detail?! o (not (modal-show-detail? o)))
                              'handled))
                      (cons #\d
                            (lambda (_o s _ctx)
                              (let ((item (list-selected-item (notif-modal-list s))))
                                (when item
                                  (notify-dismiss! (notification-id item))
                                  (set-notif-modal-status-message! s "Dismissed")
                                  (notif-modal-refresh! s)))
                              'handled))
                      (cons #\c
                            (lambda (_o s _ctx)
                              (clear-notifications!)
                              (set-notif-modal-status-message! s "History cleared")
                              (notif-modal-refresh! s)
                              'handled))
                      (cons #\q
                            (lambda (_o _s _ctx) 'close))))))
           (make-overlay
             #:kind (overlay-kind ov)
             #:data (overlay-data ov)
             #:render (overlay-render ov)
             #:on-key (make-notif-modal-on-key (overlay-on-key ov))
             #:on-click (overlay-on-click ov))))

(define* (register-notifications-tui! #:key register-category! register-command! open-overlay!)
         "Register the Notifications category and /notifications slash command."
         (when (procedure? register-category!)
           (register-category! '("Notifications") "View recent notifications and progress history"))
         (when (procedure? register-command!)
           (register-command!
             "notifications"
             "Open notification history modal"
             (lambda _
               (when (procedure? open-overlay!)
                 (open-overlay! (make-notifications-modal-overlay))))
             #:path '("Notifications"))))
