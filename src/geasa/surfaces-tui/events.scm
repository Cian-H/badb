;;; (geasa surfaces-tui events) — TUI Mouse, Keyboard & Main Event Loop
;;;
;;; Handles mouse events, mid-run steering input, idle modal editing,
;;; slash/@-mention/history-search palettes, and generic <overlay> dispatch
;;; on <tui-state>.

(define-module (geasa surfaces-tui events)
               #:use-module
               (geasa capabilities-macro plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (geasa runtimes-react plugin)
               #:use-module
               (geasa surfaces-tui editor)
               #:use-module
               (geasa surfaces-tui session)
               #:use-module
               (geasa surfaces-tui transcript)
               #:use-module
               (geasa surfaces-tui markdown)
               #:use-module
               (geasa surfaces-tui commands)
               #:use-module
               (geasa surfaces-tui steering)
               #:use-module
               (geasa surfaces-tui overlay)
               #:use-module
               (geasa surfaces-tui state)
               #:use-module
               (geasa surfaces-tui renderer)
               #:use-module
               (geasa surfaces-tui agent-loop)
               #:use-module
               (geasa surfaces-tui actions)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 threads)
               #:use-module
               (srfi srfi-1)
               #:export
               (tui-collect-enter-burst tui-handle-mouse-event! tui-run-event-loop!))

(define
  (tui-collect-enter-burst ch input-pending? drain-input-burst!)
  "Return (cons ch burst) when Enter is part of a multiline paste, or #f."
  (and (input-pending?)
       (let ((burst (drain-input-burst!))) (and (pair? burst) (cons ch burst)))))

(define (make-overlay-key-ctx st hooks)
        (let ((nref (tui-nref st)))
          `((key-up . ,(nref 'KEY_UP))
            (key-down . ,(nref 'KEY_DOWN))
            (key-left . ,(nref 'KEY_LEFT))
            (key-right . ,(nref 'KEY_RIGHT))
            (key-tab . 9)
            (key-backspace . ,(nref 'KEY_BACKSPACE))
            (key-enter . ,(nref 'KEY_ENTER))
            (on-select! .
                        ,(lambda (item)
                           (tui-apply-menu-selection! st item hooks)))
            (on-close! .
                       ,(lambda ()
                          (tui-clear-overlay! st))))))

(define (tui-handle-mouse-event! st ev hooks)
        "Handle a single decoded mouse event EV on ST. Returns #t if dirty."
        (let*
            ((button (car ev))
             (y (cadr ev))
             (x (and (pair? (cddr ev)) (caddr ev))))
          (cond
            ((= button 4)
              (let
                  ((new (min (tui-max-scroll-seen st) (+ (tui-scroll-offset st) 3))))
                (if (= new (tui-scroll-offset st))
                  #f
                  (begin (set-tui-scroll-offset! st new) #t))))
            ((= button 5)
              (let ((new (max 0 (- (tui-scroll-offset st) 3))))
                (if (= new (tui-scroll-offset st))
                  #f
                  (begin (set-tui-scroll-offset! st new) #t))))
            ((= button 1)
              (let
                  ((target (hashq-ref (tui-click-targets st) y)) (ov (tui-active-overlay st)))
                (cond
                  ((and ov
                        target
                        ((overlay-on-click ov) ov
                         target
                         `((on-select! .
                                       ,(lambda (item)
                                          (tui-apply-menu-selection! st item hooks))))))
                    #t)
                  ((and
                     (selector-active? (tui-input-buffer st) (tui-prompt-mode st))
                     (pair? target)
                     (eq? (car target) 'palette-item))
                    (set-tui-selector-index! st (cdr target))
                    #t)
                  ((eq? target 'jump-to-bottom)
                    (set-tui-scroll-offset! st 0)
                    (tui-clear-unseen-rows! st)
                    #t)
                  ((and (pair? target) (eq? (car target) 'status-bar-pills))
                    (let* ((col (or x 0))
                           (hit (find (lambda (sp)
                                        (and (>= col (car sp)) (< col (cadr sp))))
                                      (cdr target))))
                      (when hit
                        (case (caddr hit)
                          ((config) (tui-open-config-modal! st))
                          ((context) (tui-open-context-inspector-modal! st 'budget))
                          ((geasa) (tui-handle-command! st '("geis" . "") hooks))
                          ((agents) (tui-open-agent-monitor-modal! st))
                          ((notifications) (tui-open-notifications-modal! st))))
                      (and hit #t)))
                  ((eq? target 'quick-action-strip)
                    (let* ((row-txt (or (hashq-ref (tui-row-texts st) y) ""))
                           (col (max 0 (- (or x 2) 2)))
                           (hit-token
                             (find (lambda (tok)
                                     (let ((p (string-contains row-txt tok)))
                                       (and p (>= col p) (< col (+ p (string-length tok))))))
                                   '("Ctrl+K Palette" "/geis" "/mcp" "/sessions" "@file"))))
                      (cond
                        ((equal? hit-token "Ctrl+K Palette") (tui-open-command-palette! st hooks))
                        ((equal? hit-token "/geis") (tui-handle-command! st '("geis" . "") hooks))
                        ((equal? hit-token "/mcp") (tui-open-mcp-modal! st))
                        ((equal? hit-token "/sessions") (tui-open-session-browser-modal! st 'sessions))
                        ((equal? hit-token "@file") (tui-apply-edit! st "@" 1)))
                      (and hit-token #t)))
                  ((integer? target)
                    (let*
                        ((row-txt (hashq-ref (tui-row-texts st) y))
                         (copy-pos (and (string? row-txt) (string-contains row-txt "[copy]")))
                         (col (and (integer? x) (- x 2)))
                         (link
                           (and (string? row-txt)
                                (integer? x)
                                (>= x 2)
                                (find-link-at-column row-txt (- x 2)))))
                      (if (and copy-pos col (>= col copy-pos) (< col (+ copy-pos 6)))
                        (let* ((msg (list-ref (tui-messages st) target))
                               (blocks (extract-fenced-code-blocks (or (prop-ref msg 'content) ""))))
                          (when (pair? blocks)
                            (let* ((seen-headers
                                     (let hloop ((ry 0) (cnt 0))
                                                (if (> ry y)
                                                  cnt
                                                  (let ((rt (hashq-ref (tui-row-texts st) ry))
                                                        (tg (hashq-ref (tui-click-targets st) ry)))
                                                    (hloop (+ ry 1)
                                                           (if (and (eqv? tg target)
                                                                    (string? rt)
                                                                    (string-prefix? "╭─ " rt))
                                                             (+ cnt 1)
                                                             cnt))))))
                                   (b-idx (max 0 (min (- (length blocks) 1) (- seen-headers 1))))
                                   (code (list-ref blocks b-idx)))
                              (clipboard-copy! code)
                              (set-tui-status-text! st (format #f "COPIED CODE (~a chars)" (string-length code)))))
                          #t)
                        (begin
                          (when link
                            (let
                                ((kind (assoc-ref link 'kind))
                                 (dest (assoc-ref link 'target))
                                 (line-num (assoc-ref link 'line)))
                              (cond
                                ((and (eq? kind 'file) (string? dest) (file-exists? dest))
                                  (open-in-external-editor dest
                                                           #:line
                                                           line-num
                                                           #:suspend-tty!
                                                           (lambda () (tui-suspend-tty! st))
                                                           #:resume-tty!
                                                           (lambda () (tui-resume-tty! st))))
                                ((and (eq? kind 'url) (string? dest))
                                  (catch #t
                                    (lambda ()
                                      (let ((p (open-pipe* OPEN_READ "xdg-open" dest))) (close-pipe p)))
                                    (lambda _
                                      #f))))))
                          (set-tui-msgsel-index! st target)
                          #t))))
                  (else
                    (if (tui-msgsel-index st)
                      (begin (set-tui-msgsel-index! st #f) #t)
                      #f)))))
            (else #f))))

(define (selected-command-name st matches)
        (and (pair? matches)
             (let
                 ((safe-idx (max 0 (min (tui-selector-index st) (- (length matches) 1)))))
               (car (list-ref matches safe-idx)))))

(define (dispatch-running-steering-input! st raw urgent? on-cmd!)
        "Dispatch trimmed mid-run steering input RAW on ST (urgent or queued)."
        (unless (string-null? raw)
          (let ((disp (dispatch-mid-run-input raw)))
            (case (car disp)
              ((safe-slash)
                (tui-execute-mid-run-safe-command! st (cadr disp) (caddr disp) on-cmd!))
              ((blocked-slash)
                (tui-add-sys-msg! st
                                  (format #f
                                          "Command /~a cannot run while the agent is active (safe mid-run: /context, /usage, /theme, /help, /subagents, /stop, /steer)."
                                          (cadr disp))))
              ((steer)
                (if urgent?
                  (tui-trigger-urgent-steering! st (cdr disp))
                  (begin
                    (with-mutex (tui-steering-mutex st)
                                (steering-push! (tui-steering-queue st) (cdr disp)))
                    (tui-add-msg! st "steer" (format #f "Steering queued: ~a" (cdr disp))))))
              (else #f)))))

(define (handle-running-tab! st)
        "Handle Tab while running: complete mid-run slash command or request interrupt."
        (if (selector-active? (tui-steering-buffer st))
          (let*
              ((matches (matching-mid-run-slash-commands (tui-steering-buffer st)))
               (safe-idx
                 (if (null? matches)
                   0
                   (max 0 (min (tui-steering-selector-index st) (- (length matches) 1)))))
               (chosen (and (pair? matches) (car (list-ref matches safe-idx)))))
            (when chosen
              (set-tui-steering-buffer! st
                                        (if (string=? chosen "steer")
                                          "/steer "
                                          (string-append "/" chosen)))
              (set-tui-steering-selector-index! st 0)))
          (begin (kill-active-subprocess!)
                 (with-mutex (tui-steering-mutex st)
                             (steering-push! (tui-steering-queue st) 'interrupt))
                 (set-tui-interrupt-requested?! st #t)
                 (set-tui-status-text! st "INTERRUPT REQUESTED")
                 (tui-add-msg! st
                               "steer"
                               "Interrupt requested — halting at the next iteration boundary.")))
        'render)

(define (handle-running-enter! st on-cmd!)
        "Handle Enter while running: expand selected mid-run slash or queue steering."
        (let
            ((raw
               (if (selector-active? (tui-steering-buffer st))
                 (let
                     ((matches (matching-mid-run-slash-commands (tui-steering-buffer st))))
                   (if (null? matches)
                     (string-trim-both (tui-steering-buffer st))
                     (let*
                         ((safe-idx (max 0 (min (tui-steering-selector-index st) (- (length matches) 1))))
                          (chosen (car (list-ref matches safe-idx))))
                       (if (string=? chosen "steer")
                         ""
                         (string-append "/" chosen)))))
                 (string-trim-both (tui-steering-buffer st)))))
          (if
              (and (selector-active? (tui-steering-buffer st))
                   (string-null? raw)
                   (pair? (matching-mid-run-slash-commands (tui-steering-buffer st))))
            (begin (set-tui-steering-buffer! st "/steer ")
                   (set-tui-steering-selector-index! st 0))
            (begin (set-tui-steering-buffer! st "")
                   (set-tui-steering-selector-index! st 0)
                   (dispatch-running-steering-input! st raw #f on-cmd!))))
        'render)

(define (handle-running-up-down! st delta)
        "Step mid-run slash selector or scroll transcript by DELTA (-1 Up, +1 Down)."
        (if (selector-active? (tui-steering-buffer st))
          (let
              ((matches (matching-mid-run-slash-commands (tui-steering-buffer st))))
            (unless (null? matches)
              (let*
                  ((cnt (length matches))
                   (cur (max 0 (min (tui-steering-selector-index st) (- cnt 1)))))
                (set-tui-steering-selector-index! st (modulo (+ cur delta) cnt)))))
          (set-tui-scroll-offset! st
                                  (if (< delta 0)
                                    (min (tui-max-scroll-seen st) (+ (tui-scroll-offset st) 1))
                                    (max 0 (- (tui-scroll-offset st) 1)))))
        'render)

(define (handle-running-key! st ch hooks)
        "Handle key CH while the agent thread is active. Returns 'continue or 'render."
        (let*
            ((nref (tui-nref st)) (KEY_UP (nref 'KEY_UP))
             (KEY_DOWN (nref 'KEY_DOWN))
             (KEY_ENTER (nref 'KEY_ENTER))
             (KEY_BACKSPACE (nref 'KEY_BACKSPACE))
             (KEY_PPAGE (nref 'KEY_PPAGE))
             (KEY_NPAGE (nref 'KEY_NPAGE))
             (KEY_RESIZE (nref 'KEY_RESIZE))
             (ov (tui-active-overlay st))
             (on-cmd!
               (lambda (pair)
                 (tui-handle-command! st pair hooks))))
          (cond
            ((and ov (not (eqv? ch 3)))
              (if (or (not ch) (eqv? ch -1))
                (begin (usleep 20000) 'continue)
                (let
                    ((res ((overlay-on-key ov) ov ch (make-overlay-key-ctx st hooks))))
                  (when (eq? res 'close) (tui-clear-overlay! st))
                  (if (eq? res 'pass)
                    'continue
                    'render))))
            ((eqv? ch 3) (tui-clear-overlay! st) (tui-force-stop-run! st) 'continue)
            ((eqv? ch 21)
              (let
                  ((trimmed (string-trim-both (tui-steering-buffer st))))
                (set-tui-steering-buffer! st "")
                (set-tui-steering-selector-index! st 0)
                (dispatch-running-steering-input! st trimmed #t on-cmd!))
              'render)
            ((eqv? ch 9) (handle-running-tab! st))
            ((or (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13))
              (handle-running-enter! st on-cmd!))
            ((or (eqv? ch KEY_BACKSPACE) (eqv? ch 8) (eqv? ch 127))
              (let ((sb (tui-steering-buffer st)))
                (when (> (string-length sb) 0)
                  (set-tui-steering-buffer! st (substring sb 0 (- (string-length sb) 1)))))
              (set-tui-steering-selector-index! st 0)
              'render)
            ((eqv? ch 27)
              (if (string-null? (tui-steering-buffer st))
                (tui-force-stop-run! st)
                (begin (set-tui-steering-buffer! st "")
                       (set-tui-steering-selector-index! st 0)
                       (tui-render! st)))
              'continue)
            ((eqv? ch KEY_PPAGE)
              (set-tui-scroll-offset! st
                                      (min (tui-max-scroll-seen st) (+ (tui-scroll-offset st) 10)))
              'render)
            ((eqv? ch KEY_NPAGE)
              (set-tui-scroll-offset! st (max 0 (- (tui-scroll-offset st) 10)))
              'render)
            ((eqv? ch KEY_UP) (handle-running-up-down! st -1))
            ((eqv? ch KEY_DOWN) (handle-running-up-down! st 1))
            ((eqv? ch KEY_RESIZE) 'render)
            ((and (integer? ch) (>= ch 32) (<= ch 126))
              (set-tui-steering-buffer! st
                                        (string-append (tui-steering-buffer st) (string (integer->char ch))))
              (set-tui-steering-selector-index! st 0)
              'render)
            (else (usleep 20000)
                  (let*
                      ((now (get-internal-real-time))
                       (status (tui-status-text st))
                       (spin-due?
                         (and
                           (or (agent-run-active? status) (string=? status "STREAMING..."))
                           (>= (- now (tui-last-spin-time st))
                               (quotient internal-time-units-per-second 12)))))
                    (when spin-due? (set-tui-last-spin-time! st now))
                    (if spin-due?
                      'render
                      'continue))))))

(define
  (handle-idle-enter! st ch input-pending? drain-input-burst! hooks)
  (let
      ((burst (tui-collect-enter-burst ch input-pending? drain-input-burst!))
       (beep ((tui-nref st) 'beep))
       (on-cmd!
         (lambda (pair)
           (tui-handle-command! st pair hooks))))
    (if burst
      (begin (tui-insert-burst! st burst) 'render)
      (if
          (selector-active? (tui-input-buffer st) (tui-prompt-mode st))
        (let
            ((matches
               (deduplicate-slash-commands (matching-slash-commands (tui-input-buffer st)))))
          (if (null? matches)
            (let
                ((trimmed (string-trim-both (tui-input-buffer st)))
                 (was-slash? (eq? (tui-prompt-mode st) 'slash)))
              (set-tui-input-buffer! st "")
              (set-tui-cursor-pos! st 0)
              (set-tui-selector-index! st 0)
              (when was-slash? (set-tui-prompt-mode! st 'chat))
              (if (string-null? trimmed)
                'render
                (let*
                    ((canonical
                       (if was-slash?
                         (string-append "/" trimmed)
                         trimmed))
                     (action (classify-input canonical))
                     (res
                       (and action
                            (eq? (car action) 'slash)
                            (tui-handle-command! st (cons (cadr action) (caddr action)) hooks))))
                  (if (eq? res 'exit)
                    'done
                    'render))))
            (let
                ((chosen (selected-command-name st matches)))
              (if
                  (member chosen '("provider" "sh" "eval" "steer" "edit" "todo" "diff"))
                (begin
                  (set-tui-input-buffer! st
                                         (format-slash-completion chosen (tui-prompt-mode st) #t))
                  (set-tui-cursor-pos! st (string-length (tui-input-buffer st)))
                  (set-tui-selector-index! st 0)
                  'render)
                (begin (set-tui-input-buffer! st "")
                       (set-tui-cursor-pos! st 0)
                       (set-tui-selector-index! st 0)
                       (when (eq? (tui-prompt-mode st) 'slash) (set-tui-prompt-mode! st 'chat))
                       (tui-record-history! st (string-append "/" chosen) 'slash)
                       (let
                           ((res (tui-handle-command! st (cons chosen "") hooks)))
                         (if (eq? res 'exit)
                           'done
                           'render)))))))
        (let
            ((trimmed (string-trim-both (tui-input-buffer st)))
             (cur-mode (tui-prompt-mode st)))
          (set-tui-input-buffer! st "")
          (set-tui-cursor-pos! st 0)
          (set-tui-selector-index! st 0)
          (when (eq? cur-mode 'slash) (set-tui-prompt-mode! st 'chat))
          (cond
            ((string-null? trimmed) 'render)
            ((eq? cur-mode 'shell)
              (tui-record-history! st (if (string-prefix? "!" trimmed) trimmed (string-append "!" trimmed)) 'shell)
              (tui-add-cmd-msg! st trimmed)
              (tui-execute-shell! st trimmed)
              'render)
            ((eq? cur-mode 'scheme)
              (tui-record-history! st trimmed 'scheme)
              (tui-add-scm-msg! st trimmed)
              (tui-execute-scheme! st trimmed)
              'render)
            (else
              (let*
                  ((canonical (prompt-mode-submit-text cur-mode trimmed))
                   (action (classify-input canonical)))
                (case (car action)
                  ((slash)
                    (let* ((scmd (cadr action))
                           (sarg (caddr action))
                           (hist-str (if (string-null? (string-trim-both sarg))
                                       (string-append "/" scmd)
                                       (string-append "/" scmd " " sarg))))
                      (tui-record-history! st hist-str 'slash)
                      (let ((res (catch #t
                                   (lambda () (tui-handle-command! st (cons scmd sarg) hooks))
                                   (lambda (k . args)
                                     (tui-add-err-msg! st (format #f "Command error (~a): ~s" k args))
                                     'render))))
                        (if (eq? res 'exit)
                          'done
                          'render))))
                  ((shell)
                    (let ((shell-cmd (cdr action)))
                      (tui-record-history! st (if (string-prefix? "!" shell-cmd) shell-cmd (string-append "!" shell-cmd)) 'shell)
                      (tui-add-cmd-msg! st shell-cmd)
                      (tui-execute-shell! st shell-cmd)
                      'render))
                  ((scheme)
                    (let ((code-str (cdr action)))
                      (tui-record-history! st code-str 'scheme)
                      (tui-add-scm-msg! st code-str)
                      (tui-execute-scheme! st code-str)
                      'render))
                  ((chat)
                    (let ((delivered (expand-macros (cdr action))))
                      (tui-record-history! st delivered 'chat)
                      (tui-add-user-msg! st delivered)
                      (set-tui-current-run-id! st (+ (tui-current-run-id st) 1))
                      (set-tui-force-stop-requested?! st #f)
                      (set-tui-urgent-steer-requested?! st #f)
                      (set-tui-agent-thread! st
                                             (call-with-new-thread
                                               (lambda ()
                                                 (catch #t
                                                   (lambda ()
                                                     (tui-execute-chat-stream! st delivered on-cmd!)
                                                     (beep))
                                                   (lambda (key . args)
                                                     (tui-add-err-msg! st (format #f "Chat error (~a): ~s" key args))
                                                     (set-tui-status-text! st "READY")
                                                     (tui-render! st))))))
                      'render)))))))))))

(define (handle-idle-external-editor! st)
        "Open external editor for the current input buffer on ST."
        (call-with-values
          (lambda ()
            (read-via-external-editor #:initial-text
                                      (tui-input-buffer st)
                                      #:suspend-tty!
                                      (lambda () (tui-suspend-tty! st))
                                      #:resume-tty!
                                      (lambda () (tui-resume-tty! st))))
          (lambda (ok? res)
            (if ok?
              (begin
                (set-tui-input-buffer! st (string-trim-right res #\newline))
                (set-tui-cursor-pos! st (string-length (tui-input-buffer st)))
                (tui-clamp-selector-index! st))
              (tui-add-err-msg! st (format #f "External editor error: ~a" res)))))
        'render)

(define (handle-idle-tab! st)
        "Apply slash-command or @-mention completion on Tab."
        (cond
          ((and (not (history-recall-position (tui-input-history st)))
                (selector-active? (tui-input-buffer st) (tui-prompt-mode st)))
            (let*
                ((matches
                   (deduplicate-slash-commands (matching-slash-commands (tui-input-buffer st))))
                 (chosen (selected-command-name st matches)))
              (when chosen
                (let
                    ((needs-arg?
                       (and
                         (member chosen
                                 '("model" "endpoint"
                                   "provider"
                                   "mcp"
                                   "config"
                                   "sh"
                                   "eval"
                                   "steer"
                                   "edit"
                                   "todo"
                                   "diff"))
                         #t)))
                  (set-tui-input-buffer! st
                                         (format-slash-completion chosen (tui-prompt-mode st) needs-arg?))
                  (set-tui-cursor-pos! st (string-length (tui-input-buffer st)))
                  (set-tui-selector-index! st 0)))))
          ((at-selector-active? (tui-input-buffer st)
                                (tui-cursor-pos st)
                                (tui-prompt-mode st))
            (unless (tui-workspace-files-cache st)
              (set-tui-workspace-files-cache! st (collect-workspace-files)))
            (let
                ((cands
                   (matching-at-candidates (tui-input-buffer st)
                                           #:cursor-pos
                                           (tui-cursor-pos st)
                                           #:candidates
                                           (tui-workspace-files-cache st)
                                           #:limit
                                           25)))
              (when (pair? cands)
                (let*
                    ((safe-i (max 0 (min (tui-at-selector-index st) (- (length cands) 1))))
                     (chosen (list-ref cands safe-i)))
                  (call-with-values
                    (lambda ()
                      (apply-at-completion (tui-input-buffer st) (tui-cursor-pos st) chosen))
                    (lambda (nb np)
                      (set-tui-input-buffer! st nb)
                      (set-tui-cursor-pos! st np)
                      (set-tui-at-selector-index! st 0))))))))
        'render)

(define (handle-idle-escape! st)
        "Handle Escape while idle: exit prompt mode/selector or clear message selection."
        (cond
          ((or
             (selector-active? (tui-input-buffer st) (tui-prompt-mode st))
             (not (eq? (tui-prompt-mode st) 'chat)))
            (call-with-values
              (lambda ()
                (prompt-mode-on-escape (tui-prompt-mode st)
                                       (tui-input-buffer st)
                                       (tui-cursor-pos st)))
              (lambda (nm nb np)
                (set-tui-prompt-mode! st nm)
                (set-tui-input-buffer! st nb)
                (set-tui-cursor-pos! st np)
                (set-tui-selector-index! st 0))))
          ((and (integer? (tui-msgsel-index st)) (string-null? (tui-input-buffer st)))
            (set-tui-msgsel-index! st #f)))
        'render)

(define (apply-recalled-history-entry! st entry)
        "Apply recalled history ENTRY to ST, updating prompt mode and buffer."
        (cond
          ((string-prefix? "!" entry)
            (let ((body (substring entry 1)))
              (set-tui-prompt-mode! st 'shell)
              (set-tui-input-buffer! st body)
              (set-tui-cursor-pos! st (string-length body))))
          ((string-prefix? "/" entry)
            (let ((body (substring entry 1)))
              (set-tui-prompt-mode! st 'slash)
              (set-tui-input-buffer! st body)
              (set-tui-cursor-pos! st (string-length body))))
          ((string-prefix? "(" entry)
            (set-tui-prompt-mode! st 'scheme)
            (set-tui-input-buffer! st entry)
            (set-tui-cursor-pos! st (string-length entry)))
          (else
            (set-tui-prompt-mode! st 'chat)
            (set-tui-input-buffer! st entry)
            (set-tui-cursor-pos! st (string-length entry)))))

(define (handle-idle-up-down! st delta)
        "Step active selector or input history by DELTA (-1 for Up, +1 for Down)."
        (cond
          ((and (not (history-recall-position (tui-input-history st)))
                (selector-active? (tui-input-buffer st) (tui-prompt-mode st)))
            (let
                ((matches
                   (deduplicate-slash-commands (matching-slash-commands (tui-input-buffer st)))))
              (unless (null? matches)
                (let*
                    ((count (length matches))
                     (cur (max 0 (min (tui-selector-index st) (- count 1)))))
                  (set-tui-selector-index! st (modulo (+ cur delta) count))))))
          ((at-selector-active? (tui-input-buffer st)
                                (tui-cursor-pos st)
                                (tui-prompt-mode st))
            (let
                ((cands
                   (matching-at-candidates (tui-input-buffer st)
                                           #:cursor-pos
                                           (tui-cursor-pos st)
                                           #:candidates
                                           (tui-workspace-files-cache st)
                                           #:limit
                                           25)))
              (unless (null? cands)
                (let*
                    ((cnt (length cands)) (cur (max 0 (min (tui-at-selector-index st) (- cnt 1)))))
                  (set-tui-at-selector-index! st (modulo (+ cur delta) cnt))))))
          ((< delta 0)
            (unless
                (history-recall-position (tui-input-history st))
              (set-tui-history-draft! st (tui-input-buffer st))
              (set-tui-history-draft-pos! st (tui-cursor-pos st)))
            (let
                ((entry (history-recall (tui-input-history st) -1)))
              (when entry
                (apply-recalled-history-entry! st entry))))
          (else
            (let*
                ((was-recalling? (history-recall-position (tui-input-history st)))
                 (entry (history-recall (tui-input-history st) 1)))
              (if entry
                (apply-recalled-history-entry! st entry)
                (when was-recalling?
                  (set-tui-prompt-mode! st 'chat)
                  (set-tui-input-buffer! st (tui-history-draft st))
                  (set-tui-cursor-pos! st (tui-history-draft-pos st)))))))
        'render)

(define (apply-transcript-search-scroll! st ts)
        (let ((scroll (transcript-search-scroll-offset
                        ts
                        (tui-asm-total st)
                        (tui-last-view-height st))))
          (when scroll
            (set-tui-scroll-offset! st scroll))))

(define (handle-transcript-search-key! st ch KEY_ENTER KEY_BACKSPACE)
        (let ((ts (tui-transcript-search st)))
          (cond
            ((eqv? ch 27)
              (set-tui-transcript-search! st (transcript-search-close ts))
              'render)
            ((transcript-search-input-active? ts)
              (cond
                ((or (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13))
                  (let ((next (transcript-search-with-input-active ts #f)))
                    (set-tui-transcript-search! st next)
                    (apply-transcript-search-scroll! st next))
                  'render)
                ((or (eqv? ch KEY_BACKSPACE) (eqv? ch 127) (eqv? ch 8))
                  (let* ((q (transcript-search-query ts))
                         (nq (if (> (string-length q) 0)
                               (substring q 0 (- (string-length q) 1))
                               ""))
                         (next (transcript-search-update ts
                                                         nq
                                                         (tui-messages st)
                                                         (tui-last-content-width st)
                                                         (tui-row-cache st))))
                    (set-tui-transcript-search! st next)
                    (apply-transcript-search-scroll! st next))
                  'render)
                ((and (integer? ch) (>= ch 32) (<= ch 126))
                  (let* ((nq (string-append (transcript-search-query ts) (string (integer->char ch))))
                         (next (transcript-search-update ts
                                                         nq
                                                         (tui-messages st)
                                                         (tui-last-content-width st)
                                                         (tui-row-cache st))))
                    (set-tui-transcript-search! st next)
                    (apply-transcript-search-scroll! st next))
                  'render)
                (else 'render)))
            ((or (eqv? ch 110) (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13))
              (let ((next (transcript-search-step ts 1)))
                (set-tui-transcript-search! st next)
                (apply-transcript-search-scroll! st next))
              'render)
            ((eqv? ch 78)
              (let ((next (transcript-search-step ts -1)))
                (set-tui-transcript-search! st next)
                (apply-transcript-search-scroll! st next))
              'render)
            ((or (eqv? ch 47) (eqv? ch 6))
              (set-tui-transcript-search! st (transcript-search-with-input-active ts #t))
              'render)
            (else #f))))

(define
  (handle-idle-key! st ch input-pending? drain-input-burst! hooks)
  "Handle key CH while idle. Returns 'render, 'continue, or 'done."
  (let*
      ((nref (tui-nref st)) (stdscr (tui-stdscr st))
       (getmaxy (nref 'getmaxy))
       (KEY_UP (nref 'KEY_UP))
       (KEY_DOWN (nref 'KEY_DOWN))
       (KEY_LEFT (nref 'KEY_LEFT))
       (KEY_RIGHT (nref 'KEY_RIGHT))
       (KEY_HOME (nref 'KEY_HOME))
       (KEY_END (nref 'KEY_END))
       (KEY_DC (nref 'KEY_DC))
       (KEY_ENTER (nref 'KEY_ENTER))
       (KEY_BACKSPACE (nref 'KEY_BACKSPACE))
       (KEY_PPAGE (nref 'KEY_PPAGE))
       (KEY_NPAGE (nref 'KEY_NPAGE))
       (KEY_RESIZE (nref 'KEY_RESIZE))
       (ov (tui-active-overlay st))
       (hs (tui-history-search st)))
    (when
        (and (tui-nodelay-proc st) (tui-banner-anim-done? st))
      ((tui-nodelay-proc st) stdscr #f))
    (unless
        (or (not ch) (eqv? ch -1) (eqv? ch KEY_UP) (eqv? ch KEY_DOWN))
      (history-reset-nav (tui-input-history st)))
    (cond
      ((or (not ch) (eqv? ch -1))
        (if (tui-banner-anim-done? st)
          'render
          (begin (usleep 25000)
                 (if (tui-advance-startup-banner! st)
                   'render
                   'continue))))
      ((and ov (not (or (eqv? ch 3) (eqv? ch 4))))
        (let* ((stk (tui-overlay-stack st))
               (stk-top (overlay-stack-top-modal stk))
               (ctx (make-overlay-key-ctx st hooks)))
          (if (eq? ov stk-top)
            (let ((res (catch #t
                         (lambda () (overlay-stack-handle-key stk ch ctx))
                         (lambda (k . args)
                           (tui-add-err-msg! st (format #f "Overlay key error (~a): ~s" k args))
                           'handled))))
              (when (and (eq? res 'pass) (eqv? ch 121))
                (let ((data (overlay-data ov)))
                  (when data
                    (clipboard-copy! (format #f "~a" data)))))
              (when (eq? (tui-active-overlay st) ov)
                (set-tui-active-overlay! st (overlay-stack-top-modal stk))))
            (let ((res ((overlay-on-key ov) ov ch ctx)))
              (when (eq? res 'close) (tui-clear-overlay! st)))))
        'render)
      ((eqv? ch 11)
        (tui-open-command-palette! st hooks)
        'render)
      ((and (history-search-active? hs) (not (or (eqv? ch 3) (eqv? ch 4))))
        (let ((norm-ch (cond
                         ((eqv? ch KEY_DOWN) 'up)
                         ((eqv? ch KEY_UP) 'down)
                         ((eqv? ch KEY_ENTER) 10)
                         ((eqv? ch KEY_BACKSPACE) 'backspace)
                         (else ch))))
          (call-with-values
            (lambda ()
              (history-search-handle-key hs (tui-input-history st) norm-ch))
            (lambda (action new-st res-buf res-pos)
              (set-tui-history-search! st new-st)
              (case action
                ((accept)
                  (if (and (string? res-buf) (string-prefix? "/" res-buf))
                    (let ((body (substring res-buf 1)))
                      (set-tui-prompt-mode! st 'slash)
                      (set-tui-input-buffer! st body)
                      (set-tui-cursor-pos! st (string-length body)))
                    (begin
                      (set-tui-input-buffer! st res-buf)
                      (set-tui-cursor-pos! st res-pos)))
                  (tui-clamp-selector-index! st))
                ((cancel)
                  (set-tui-input-buffer! st res-buf)
                  (set-tui-cursor-pos! st res-pos)
                  (tui-clamp-selector-index! st))))))
        'render)
      ((and (transcript-search-active? (tui-transcript-search st))
            (not (or (eqv? ch 3) (eqv? ch 4)))
            (handle-transcript-search-key! st ch KEY_ENTER KEY_BACKSPACE))
        => identity)
      ((eqv? ch 6)
        (set-tui-transcript-search! st (transcript-search-open (tui-transcript-search st)))
        'render)
      ((or (eqv? ch 26) (eqv? ch 31))
        (let ((snap (editor-undo-pop! st)))
          (when snap
            (set-tui-input-buffer! st (car snap))
            (set-tui-cursor-pos! st (cdr snap))
            (tui-clamp-selector-index! st)))
        'render)
      ((eqv? ch 21)
        (editor-undo-push! st (tui-input-buffer st) (tui-cursor-pos st))
        (call-with-values
          (lambda () (kill-to-line-start (tui-input-buffer st) (tui-cursor-pos st)))
          (lambda (nb np)
            (tui-apply-edit! st nb np)
            (tui-clamp-selector-index! st)))
        'render)
      ((eqv? ch 23)
        (editor-undo-push! st (tui-input-buffer st) (tui-cursor-pos st))
        (call-with-values
          (lambda () (kill-word-backward (tui-input-buffer st) (tui-cursor-pos st)))
          (lambda (nb np)
            (tui-apply-edit! st nb np)
            (tui-clamp-selector-index! st)))
        'render)
      ((eqv? ch 18)
        (set-tui-history-search! st
                                 (history-search-open (tui-input-history st)
                                                      (tui-input-buffer st)
                                                      (tui-cursor-pos st)))
        'render)
      ((eqv? ch 24) (handle-idle-external-editor! st))
      ((or (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13))
        (handle-idle-enter! st ch input-pending? drain-input-burst! hooks))
      ((eqv? ch 9) (handle-idle-tab! st))
      ((eqv? ch 27) (handle-idle-escape! st))
      ((eqv? ch KEY_UP) (handle-idle-up-down! st -1))
      ((eqv? ch KEY_DOWN) (handle-idle-up-down! st 1))
      ((or (eqv? ch KEY_BACKSPACE) (eqv? ch 127) (eqv? ch 8))
        (editor-undo-push! st (tui-input-buffer st) (tui-cursor-pos st))
        (call-with-values
          (lambda ()
            (prompt-mode-on-backspace (tui-prompt-mode st)
                                      (tui-input-buffer st)
                                      (tui-cursor-pos st)))
          (lambda (nm nb np)
            (set-tui-prompt-mode! st nm)
            (set-tui-input-buffer! st nb)
            (set-tui-cursor-pos! st np)
            (tui-clamp-selector-index! st)))
        'render)
      ((eqv? ch KEY_PPAGE)
        (let
            ((new
               (min (tui-max-scroll-seen st)
                    (+ (tui-scroll-offset st) (max 1 (quotient (- (getmaxy stdscr) 6) 2))))))
          (if (= new (tui-scroll-offset st))
            'continue
            (begin (set-tui-scroll-offset! st new) 'render))))
      ((eqv? ch KEY_NPAGE)
        (let
            ((new
               (max 0 (- (tui-scroll-offset st) (max 1 (quotient (- (getmaxy stdscr) 6) 2))))))
          (if (= new (tui-scroll-offset st))
            'continue
            (begin (set-tui-scroll-offset! st new) 'render))))
      ((eqv? ch (tui-ctrl-up-key st))
        (unless (null? (tui-messages st))
          (set-tui-msgsel-index! st
                                 (if (integer? (tui-msgsel-index st))
                                   (max 0 (- (tui-msgsel-index st) 1))
                                   (- (length (tui-messages st)) 1))))
        'render)
      ((eqv? ch (tui-ctrl-down-key st))
        (when
            (and (integer? (tui-msgsel-index st)) (pair? (tui-messages st)))
          (set-tui-msgsel-index! st
                                 (min (- (length (tui-messages st)) 1) (+ (tui-msgsel-index st) 1))))
        'render)
      ((eqv? ch KEY_LEFT)
        (set-tui-cursor-pos! st
                             (clamp-cursor (- (tui-cursor-pos st) 1) (tui-input-buffer st)))
        'render)
      ((eqv? ch KEY_RIGHT)
        (set-tui-cursor-pos! st
                             (clamp-cursor (+ (tui-cursor-pos st) 1) (tui-input-buffer st)))
        'render)
      ((or (eqv? ch KEY_HOME) (eqv? ch 1)) (set-tui-cursor-pos! st 0) 'render)
      ((eqv? ch KEY_END)
        (if (and (> (tui-scroll-offset st) 0)
                 (or (string-null? (tui-input-buffer st))
                     (= (tui-cursor-pos st) (string-length (tui-input-buffer st)))))
          (begin
            (set-tui-scroll-offset! st 0)
            (tui-clear-unseen-rows! st))
          (set-tui-cursor-pos! st (string-length (tui-input-buffer st))))
        'render)
      ((eqv? ch 15)
        (if (and (integer? (tui-msgsel-index st))
                 (pair? (tui-messages st))
                 (< (tui-msgsel-index st) (length (tui-messages st))))
          (tui-toggle-selected-collapse! st)
          (call-with-values
            (lambda () (transcript-fold-all! (tui-messages st) 'toggle))
            (lambda (next-msgs collapsed? changed)
              (set-tui-messages! st next-msgs)
              (row-cache-clear! (tui-row-cache st))
              (set-tui-status-text!
                st
                (format #f "~a ~a card~a"
                        (if collapsed? "Folded" "Unfolded")
                        changed
                        (if (= changed 1) "" "s"))))))
        'render)
      ((eqv? ch 5)
        (if
            (and (integer? (tui-msgsel-index st))
                 (pair? (tui-messages st))
                 (< (tui-msgsel-index st) (length (tui-messages st))))
          (tui-toggle-selected-collapse! st)
          (unless (tui-expand-latest-collapsed! st)
            (set-tui-cursor-pos! st (string-length (tui-input-buffer st)))))
        'render)
      ((eqv? ch KEY_DC)
        (editor-undo-push! st (tui-input-buffer st) (tui-cursor-pos st))
        (call-with-values
          (lambda ()
            (delete-at (tui-input-buffer st) (tui-cursor-pos st)))
          (lambda (nb np)
            (tui-apply-edit! st nb np)))
        (tui-clamp-selector-index! st)
        'render)
      ((or (eqv? ch 3) (eqv? ch 4)) 'done)
      ((eqv? ch KEY_RESIZE) 'render)
      ((and (integer? ch) (>= ch 32))
        (editor-undo-push! st (tui-input-buffer st) (tui-cursor-pos st))
        (let
            ((burst
               (if (input-pending?)
                 (cons ch (drain-input-burst!))
                 (list ch))))
          (if (> (length burst) 1)
            (tui-insert-burst! st burst)
            (let ((c (integer->char ch)))
              (call-with-values
                (lambda ()
                  (prompt-mode-on-char (tui-prompt-mode st)
                                       (tui-input-buffer st)
                                       (tui-cursor-pos st)
                                       c))
                (lambda (nm nb np _absorbed?)
                  (set-tui-prompt-mode! st nm)
                  (set-tui-input-buffer! st nb)
                  (set-tui-cursor-pos! st np)
                  (tui-clamp-selector-index! st))))))
        'render)
      (else 'render))))

(define
  (tui-run-event-loop! st
                       input-pending?
                       drain-input-burst!
                       drain-mouse-events
                       hooks)
  "Run the main TUI event loop on ST until exit."
  (let*
      ((nref (tui-nref st)) (stdscr (tui-stdscr st))
       (getch (nref 'getch)))
    (let loop
      ((needs-render? #t))
      (let
          ((mouse-dirty?
             (let ((evs (drain-mouse-events)))
               (and (pair? evs)
                    (any identity
                         (map
                           (lambda (ev)
                             (tui-handle-mouse-event! st ev hooks))
                           evs))))))
        (when
            (or needs-render? mouse-dirty? (tui-render-dirty? st))
          (set-tui-render-dirty?! st #f)
          (tui-render! st))
        (when
            (and (tui-nodelay-proc st)
                 (or (tui-agent-running? st) (not (tui-banner-anim-done? st))))
          ((tui-nodelay-proc st) stdscr #t))
        (let ((ch (key->code (getch stdscr))))
          (if (tui-agent-running? st)
            (case (handle-running-key! st ch hooks)
              ((render) (loop #t))
              (else (loop #f)))
            (case
                (handle-idle-key! st ch input-pending? drain-input-burst! hooks)
              ((done) 'done)
              ((render) (loop #t))
              (else (loop #f)))))))))