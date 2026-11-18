;;; (geasa surfaces-command-palette plugin) — Chord-free Ctrl+K / /keys modal

(define-module (geasa surfaces-command-palette plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (command-palette-state?
                          command-palette-query
                          command-palette-commands
                          command-palette-keybindings
                          command-palette-capturing-key?
                          command-palette-status-message
                          fuzzy-match-score
                          make-command-palette-overlay
                          register-command-palette-tui!))

(define-record-type <command-palette-state>
                    (%make-command-palette-state
                      commands keybindings query commands-list keys-list
                      capturing-key? pending-conflict on-execute on-rebind status-message)
                    command-palette-state?
                    (commands command-palette-commands)
                    (keybindings command-palette-keybindings set-command-palette-keybindings!)
                    (query command-palette-query set-command-palette-query!)
                    (commands-list command-palette-commands-list set-command-palette-commands-list!)
                    (keys-list command-palette-keys-list set-command-palette-keys-list!)
                    (capturing-key? command-palette-capturing-key? set-command-palette-capturing-key?!)
                    (pending-conflict command-palette-pending-conflict set-command-palette-pending-conflict!)
                    (on-execute command-palette-on-execute)
                    (on-rebind command-palette-on-rebind)
                    (status-message command-palette-status-message set-command-palette-status-message!))

(define (alist-get alist key default)
        (let ((pair (assq key alist)))
          (if pair (cdr pair) default)))

(define (fuzzy-match-score query text)
        "Case-insensitive subsequence match. Returns integer score or #f."
        (let ((qlen (string-length query))
              (tlen (string-length text)))
          (if (= qlen 0)
            0
            (let ((q (string-downcase query))
                  (t (string-downcase text)))
              (let loop ((qi 0) (ti 0) (score 0) (prev-match -2))
                        (cond
                          ((= qi qlen) score)
                          ((= ti tlen) #f)
                          ((char=? (string-ref q qi) (string-ref t ti))
                            (let ((bonus (if (= ti (+ prev-match 1)) 10 1)))
                              (loop (+ qi 1) (+ ti 1) (+ score bonus) ti)))
                          (else
                            (loop qi (+ ti 1) score prev-match))))))))

(define (filter-commands commands query)
        (if (string=? query "")
          commands
          (let ((scored
                  (filter-map
                    (lambda (cmd)
                      (let* ((label (alist-get cmd 'label ""))
                             (desc (alist-get cmd 'description ""))
                             (s1 (fuzzy-match-score query label))
                             (s2 (fuzzy-match-score query desc))
                             (best (cond ((and s1 s2) (max s1 s2))
                                         (s1 s1)
                                         (s2 s2)
                                         (else #f))))
                        (and best (cons best cmd))))
                    commands)))
            (map cdr (sort scored (lambda (a b) (> (car a) (car b))))))))

(define (refresh-commands-list! st)
        (let ((filtered (filter-commands (command-palette-commands st)
                                         (command-palette-query st))))
          (set-command-palette-commands-list!
            st
            (make-selectable-list filtered #:selected-index 0 #:max-visible 10))))

(define (refresh-keys-list! st)
        (let ((old-idx (selectable-list-selected-index (command-palette-keys-list st)))
              (bindings (command-palette-keybindings st)))
          (set-command-palette-keys-list!
            st
            (make-selectable-list
              bindings
              #:selected-index (if (null? bindings) 0 (min old-idx (- (length bindings) 1)))
              #:max-visible 10))))

(define (render-commands-tab st width)
        (let* ((search-bar
                 (text-widget (format #f "Search: ~a_" (command-palette-query st))
                              #:role 'accent #:bold? #t))
               (slist (command-palette-commands-list st))
               (body
                 (if (null? (selectable-list-items slist))
                   (text-widget "No matching commands." #:dim? #t)
                   (selectable-list-widget
                     slist
                     #:item-renderer
                     (lambda (cmd selected?)
                       (let ((label (alist-get cmd 'label ""))
                             (desc (alist-get cmd 'description "")))
                         (hstack-widget
                           (list (text-widget (if selected? ">" " ") #:bold? selected?)
                                 (text-widget (pad-str label 20) #:bold? selected?)
                                 (text-widget (clip-str desc (max 10 (- width 26))) #:dim? #t))
                           #:gap 1)))))))
          (vstack-widget (list search-bar body) #:gap 1)))

(define (render-keybindings-tab st width)
        (let* ((header
                 (cond
                   ((command-palette-pending-conflict st)
                     (text-widget "Duplicate key! Press y to override or n/Esc to cancel."
                                  #:role 'warn #:bold? #t))
                   ((command-palette-capturing-key? st)
                     (text-widget "Press a single key to bind (Esc to cancel)..."
                                  #:role 'accent #:bold? #t))
                   (else
                     (text-widget "Single-key bindings (Enter to rebind, no chords)" #:dim? #t))))
               (slist (command-palette-keys-list st))
               (body
                 (if (null? (selectable-list-items slist))
                   (text-widget "No keybindings registered." #:dim? #t)
                   (selectable-list-widget
                     slist
                     #:item-renderer
                     (lambda (kb selected?)
                       (let ((kstr (alist-get kb 'key ""))
                             (label (alist-get kb 'label "")))
                         (hstack-widget
                           (list (text-widget (if selected? ">" " ") #:bold? selected?)
                                 (pill-widget kstr #:role 'accent)
                                 (text-widget (clip-str label (max 10 (- width 16))) #:bold? selected?))
                           #:gap 1)))))))
          (vstack-widget (list header body) #:gap 1)))

(define (render-palette-body ov st rect)
        (let* ((tab (modal-active-tab ov))
               (width (rect-width rect))
               (status (command-palette-status-message st))
               (content
                 (case tab
                   ((commands) (render-commands-tab st width))
                   ((keybindings) (render-keybindings-tab st width))
                   (else (text-widget "Unknown tab" #:dim? #t)))))
          (if status
            (vstack-widget (list content (text-widget status #:role 'ok #:bold? #t)) #:gap 1)
            content)))

(define (find-key-conflict bindings target-id new-key)
        (find (lambda (kb)
                (and (not (equal? (alist-get kb 'id #f) target-id))
                     (equal? (alist-get kb 'key "") new-key)))
              bindings))

(define (apply-rebind! st target-id new-key)
        (let ((updated
                (map (lambda (kb)
                       (if (equal? (alist-get kb 'id #f) target-id)
                         (map (lambda (pair)
                                (if (eq? (car pair) 'key) (cons 'key new-key) pair))
                              kb)
                         kb))
                     (command-palette-keybindings st))))
          (set-command-palette-keybindings! st updated)
          (refresh-keys-list! st)
          (set-command-palette-capturing-key?! st #f)
          (set-command-palette-pending-conflict! st #f)
          (set-command-palette-status-message! st (format #f "Bound to '~a'" new-key))
          (when (command-palette-on-rebind st)
            ((command-palette-on-rebind st) target-id new-key))))

(define (handle-keybinding-capture st raw-key)
        (let ((selected (list-selected-item (command-palette-keys-list st)))
              (key (if (integer? raw-key) (integer->char raw-key) raw-key)))
          (cond
            ((eq? key 'escape)
              (set-command-palette-capturing-key?! st #f)
              (set-command-palette-pending-conflict! st #f)
              (set-command-palette-status-message! st "Rebind cancelled")
              #t)
            ((command-palette-pending-conflict st)
              (let ((conflict (command-palette-pending-conflict st)))
                (cond
                  ((equal? key #\y)
                    (apply-rebind! st (car conflict) (cdr conflict))
                    #t)
                  ((equal? key #\n)
                    (set-command-palette-capturing-key?! st #f)
                    (set-command-palette-pending-conflict! st #f)
                    (set-command-palette-status-message! st "Rebind cancelled")
                    #t)
                  (else #t))))
            ((and selected (char? key) (>= (char->integer key) 32))
              (let* ((target-id (alist-get selected 'id #f))
                     (new-key (string key))
                     (dup (find-key-conflict (command-palette-keybindings st) target-id new-key)))
                (if dup
                  (begin
                    (set-command-palette-pending-conflict! st (cons target-id new-key))
                    (set-command-palette-status-message!
                      st
                      (format #f "Key '~a' already bound to ~a" new-key (alist-get dup 'label "")))
                    #t)
                  (begin
                    (apply-rebind! st target-id new-key)
                    #t))))
            (else #t))))

(define (handle-command-search-key st raw-key)
        (let ((key (if (integer? raw-key) (integer->char raw-key) raw-key)))
          (cond
            ((or (eq? key 'backspace) (equal? key #\backspace) (equal? key #\del))
              (let ((q (command-palette-query st)))
                (when (> (string-length q) 0)
                  (set-command-palette-query! st (substring q 0 (- (string-length q) 1)))
                  (refresh-commands-list! st)))
              #t)
            ((and (char? key) (>= (char->integer key) 32))
              (set-command-palette-query! st (string-append (command-palette-query st) (string key)))
              (refresh-commands-list! st)
              #t)
            (else #f))))

(define* (make-command-palette-overlay
           #:key
           (commands '())
           (keybindings '())
           (active-tab 'commands)
           (on-execute #f)
           (on-rebind #f))
         "Create the chord-free Ctrl+K / /keys modal <overlay>."
         (let ((st (%make-command-palette-state
                     commands
                     keybindings
                     ""
                     (make-selectable-list commands #:selected-index 0 #:max-visible 10)
                     (make-selectable-list keybindings #:selected-index 0 #:max-visible 10)
                     #f
                     #f
                     on-execute
                     on-rebind
                     #f)))
           (make-modal-overlay
             #:title "Command Palette & Keybindings"
             #:tabs '((commands . "Commands") (keybindings . "Keybindings"))
             #:active-tab active-tab
             #:state st
             #:body-renderer render-palette-body
             #:footer-hints '(("Tab" . "switch") ("Enter" . "run/rebind") ("Esc" . "close"))
             #:key-bindings
             (list
               (cons 'down (lambda (ov _st _ctx)
                             (let ((s (modal-overlay-state ov)))
                               (if (eq? (modal-active-tab ov) 'commands)
                                 (set-command-palette-commands-list!
                                   s (list-move (command-palette-commands-list s) 1))
                                 (set-command-palette-keys-list!
                                   s (list-move (command-palette-keys-list s) 1)))
                               #t)))
               (cons 'up (lambda (ov _st _ctx)
                           (let ((s (modal-overlay-state ov)))
                             (if (eq? (modal-active-tab ov) 'commands)
                               (set-command-palette-commands-list!
                                 s (list-move (command-palette-commands-list s) -1))
                               (set-command-palette-keys-list!
                                 s (list-move (command-palette-keys-list s) -1)))
                             #t)))
               (cons 'enter (lambda (ov _st _ctx)
                              (let ((s (modal-overlay-state ov)))
                                (if (eq? (modal-active-tab ov) 'commands)
                                  (let ((cmd (list-selected-item (command-palette-commands-list s))))
                                    (when cmd
                                      (let ((handler (alist-get cmd 'handler #f)))
                                        (when handler (handler cmd))
                                        (when (command-palette-on-execute s)
                                          ((command-palette-on-execute s) cmd))))
                                    'close)
                                  (begin
                                    (set-command-palette-capturing-key?! s #t)
                                    #t))))))
             #:on-unhandled-key
             (lambda (ov s k _ctx)
               (cond
                 ((command-palette-capturing-key? s)
                   (handle-keybinding-capture s k))
                 ((eq? (modal-active-tab ov) 'commands)
                   (handle-command-search-key s k))
                 ((equal? k #\j)
                   (set-command-palette-keys-list!
                     s (list-move (command-palette-keys-list s) 1))
                   #t)
                 ((equal? k #\k)
                   (set-command-palette-keys-list!
                     s (list-move (command-palette-keys-list s) -1))
                   #t)
                 (else #f))))))

(define* (register-command-palette-tui!
           #:key
           (register-command! #f)
           (register-key! #f)
           (push-overlay! #f)
           (commands-provider (lambda () '()))
           (keybindings-provider (lambda () '()))
           (on-execute #f)
           (on-rebind #f))
         "Register /keys command and Ctrl+K shortcut in the TUI."
         (let ((open-tab
                 (lambda (tab)
                   (when push-overlay!
                     (push-overlay!
                       (make-command-palette-overlay
                         #:commands (commands-provider)
                         #:keybindings (keybindings-provider)
                         #:active-tab tab
                         #:on-execute on-execute
                         #:on-rebind on-rebind))))))
           (when register-command!
             (register-command! "keys" "Open keybindings & command palette"
                                (lambda (_args) (open-tab 'keybindings))))
           (when register-key!
             (register-key! "C-k" "Open command palette"
                            (lambda () (open-tab 'commands))))
           #t))
