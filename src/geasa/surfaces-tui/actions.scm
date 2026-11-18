;;; (geasa surfaces-tui actions) — TUI Command, Menu & Config Action Dispatch
;;;
;;; Implements slash-command handlers, interactive menu selection dispatch,
;;; endpoint/provider/theme/guardrail configuration mutations, shell/Scheme
;;; execution, and checkpoint/rollback/geis actions on <tui-state>.

(define-module (geasa surfaces-tui actions)
               #:use-module
               (geasa foundation-config plugin)
               #:use-module
               (geasa foundation-log plugin)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (badb geis bundle)
               #:use-module
               (geasa providers-model openai)
               #:use-module
               (geasa capabilities-mcp plugin)
               #:use-module
               (geasa capabilities-skills plugin)
               #:use-module
               (geasa agents-lifecycle plugin)
               #:use-module
               (geasa policies-guardrail plugin)
               #:use-module
               (geasa runtimes-context plugin)
               #:use-module
               (geasa runtimes-react plugin)
               #:use-module
               (geasa runtimes-rollback plugin)
               #:use-module
               (geasa runtimes-verify plugin)
               #:use-module
               (geasa surfaces-theme plugin)
               #:use-module
               (geasa surfaces-geis-manager plugin)
               #:use-module
               (geasa surfaces-geis-updater plugin)
               #:use-module
               (geasa surfaces-notify plugin)
               #:use-module
               (geasa surfaces-mcp-manager plugin)
               #:use-module
               (geasa surfaces-session-browser plugin)
               #:use-module
               (geasa surfaces-context-inspector plugin)
               #:use-module
               (geasa surfaces-agent-monitor plugin)
               #:use-module
               (geasa surfaces-command-palette plugin)
               #:use-module
               (geasa surfaces-config-modal plugin)
               #:use-module
               (geasa surfaces-diff-review plugin)
               #:use-module
               (geasa surfaces-knowledge-browser plugin)
               #:use-module
               (geasa surfaces-drawer plugin)
               #:use-module
               (geasa surfaces-tui editor)
               #:use-module
               (geasa surfaces-tui session)
               #:use-module
               (geasa surfaces-tui transcript)
               #:use-module
               (geasa surfaces-tui markdown)
               #:use-module
               (geasa surfaces-tui widgets)
               #:use-module
               (geasa surfaces-tui menus)
               #:use-module
               (geasa surfaces-tui commands)
               #:use-module
               (geasa surfaces-tui steering)
               #:use-module
               (geasa surfaces-tui tools)
               #:use-module
               (geasa surfaces-tui state)
               #:use-module
               (geasa surfaces-tui renderer)
               #:use-module
               (geasa surfaces-tui agent-loop)
               #:use-module
               (json)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 ftw)
               #:use-module
               (ice-9 popen)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 threads)
               #:use-module
               (pfds hamts)
               #:use-module
               (srfi srfi-1)
               #:use-module
               (srfi srfi-19)
               #:export
               (tui-prompt-password-modal tui-execute-shell!
                                          tui-execute-scheme!
                                          tui-set-endpoint-url!
                                          tui-switch-provider-preset!
                                          tui-apply-live-theme!
                                          tui-open-theme-picker!
                                          tui-apply-menu-selection!
                                          tui-handle-command!
                                          tui-open-notifications-modal!
                                          tui-open-mcp-modal!
                                          tui-open-session-browser-modal!
                                          tui-open-context-inspector-modal!
                                          tui-open-agent-monitor-modal!
                                          tui-open-command-palette!
                                          tui-open-config-modal!
                                          tui-open-diff-review-modal!
                                          tui-open-knowledge-browser-modal!
                                          tui-register-modular-extensions!))

(define (tui-prompt-password-modal st prompt-str)
        "Masked password prompt modal for safe sudo execution (never echoes or logs)."
        (let*
            ((nref (tui-nref st)) (stdscr (tui-stdscr st))
             (getmaxy (nref 'getmaxy))
             (getmaxx (nref 'getmaxx))
             (move (nref 'move))
             (refresh (nref 'refresh))
             (getch (nref 'getch))
             (KEY_ENTER (nref 'KEY_ENTER))
             (KEY_BACKSPACE (nref 'KEY_BACKSPACE))
             (nodelay-proc (tui-nodelay-proc st))
             (pw "")
             (maxy (getmaxy stdscr))
             (maxx (getmaxx stdscr)))
          (when nodelay-proc (nodelay-proc stdscr #f))
          (let pw-loop
            ()
            (let*
                ((y (max 2 (- maxy 3)))
                 (masked (make-string (string-length pw) #\*))
                 (line (format #f " [SUDO] ~a: ~a" (or prompt-str "Password") masked)))
              (tui-draw-role! st 'warning #t)
              (tui-safe-addstr st stdscr y 0 (make-string maxx #\space) maxx)
              (tui-safe-addstr st stdscr y 0 line maxx)
              (tui-draw-role! st 'warning #f)
              (move stdscr y (min (- maxx 1) (string-length line)))
              (refresh stdscr)
              (let ((ch (key->code (getch stdscr))))
                (cond
                  ((or (eqv? ch 27) (eqv? ch 3) (eqv? ch 4)) #f)
                  ((or (eqv? ch KEY_ENTER) (eqv? ch 10) (eqv? ch 13)) pw)
                  ((or (eqv? ch KEY_BACKSPACE) (eqv? ch 8) (eqv? ch 127))
                    (when (> (string-length pw) 0)
                      (set! pw (substring pw 0 (- (string-length pw) 1))))
                    (pw-loop))
                  ((and (integer? ch) (>= ch 32) (<= ch 126))
                    (set! pw (string-append pw (string (integer->char ch))))
                    (pw-loop))
                  (else (pw-loop))))))))

(define (tui-execute-shell! st cmd)
        "Execute shell CMD on ST and append output or error messages."
        (tui-set-status-text-if! st "EXEC...")
        (tui-render! st)
        (if (string-null? cmd)
          (tui-add-sys-msg! st "Usage: !<command> (e.g. !git status, !ls -la)")
          (call-with-values
            (lambda ()
              (execute-shell-command cmd
                                     #:password-provider
                                     (lambda (p)
                                       (tui-prompt-password-modal st p))))
            (lambda (out exit-val err)
              (cond
                (err (tui-add-err-msg! st err))
                ((= exit-val 0)
                  (tui-add-msg! st
                                "shell-out"
                                (if (string-null? out)
                                  "(process completed with exit status 0)"
                                  out)))
                (else
                  (tui-add-err-msg! st
                                    (if (string-null? out)
                                      (format #f "[process exited with status ~a]" exit-val)
                                      (format #f "[exit status ~a]\n~a" exit-val out))))))))
        (set-tui-status-text! st "READY")
        (tui-render! st))

(define (tui-set-status-text-if! st txt) (set-tui-status-text! st txt))

(define (tui-execute-scheme! st code-str)
        "Evaluate Scheme CODE-STR in ST's repl-mod and append result or error."
        (set-tui-status-text! st "EVAL...")
        (tui-render! st)
        (if (string-null? code-str)
          (tui-add-sys-msg! st
                            "Usage: ,<expr> or (<expr>) (e.g. ,(+ 1 2) or (registry-list reg))")
          (call-with-values
            (lambda ()
              (eval-scheme-code code-str (tui-repl-mod st)))
            (lambda (res err)
              (if err
                (tui-add-err-msg! st err)
                (tui-add-msg! st "scheme-out" res)))))
        (set-tui-status-text! st "READY")
        (tui-render! st))

(define
  (tui-set-endpoint-url! st raw-url success-fmt)
  "Validate RAW-URL as an HTTP(S) URI, apply to ST, save config, and log."
  (let
      ((trimmed (string-trim-both raw-url)) (cfg (tui-cfg st)))
    (if (valid-endpoint-uri? trimmed)
      (let
          ((endpoint-changed? (not (equal? trimmed (tui-cur-endpoint st))))
           (matched-prov (badb-config-provider-for-endpoint cfg trimmed)))
        (set-tui-cur-endpoint! st trimmed)
        (badb-config-set-model-endpoint! cfg trimmed)
        (when endpoint-changed?
          (let
              ((new-key (and matched-prov (get-prop matched-prov 'api-key #f))))
            (set-tui-cur-api-key! st new-key)
            (badb-config-set-model-api-key! cfg new-key)))
        (tui-refresh-client! st)
        (tui-try-save-config! st)
        (tui-add-sys-msg! st (format #f success-fmt (tui-cur-endpoint st))))
      (if
          (find
            (lambda (p)
              (and (pair? p) (string=? (ensure-string (car p)) trimmed)))
            (badb-config-providers cfg))
        (tui-add-err-msg! st
                          (format #f
                                  "Invalid endpoint URL '~a' (expected http://... or https://...). '~a' is a provider preset — use '/provider ~a' to switch to it."
                                  trimmed
                                  trimmed
                                  trimmed))
        (tui-add-err-msg! st
                          (format #f
                                  "Invalid endpoint URL '~a' (expected http://... or https://...)."
                                  trimmed))))))

(define (tui-switch-provider-preset! st name)
        "Switch ST's active model configuration to provider preset NAME."
        (let ((cfg (tui-cfg st)))
          (if (badb-config-switch-provider! cfg name)
            (begin
              (set-tui-cur-model! st (badb-config-model-name cfg))
              (set-tui-cur-endpoint! st (badb-config-model-endpoint cfg))
              (set-tui-cur-api-key! st (badb-config-model-api-key cfg))
              (tui-refresh-client! st)
              (tui-try-save-config! st)
              (tui-add-sys-msg! st
                                (format #f
                                        "Switched to provider '~a': model ~a at ~a (saved to config)"
                                        name
                                        (tui-cur-model st)
                                        (tui-cur-endpoint st))))
            (tui-add-err-msg! st
                              (format #f "Unknown provider preset '~a'. Use /provider to list presets." name)))))

(define (tui-apply-live-theme! st name)
        "Apply theme NAME to ST live without persisting."
        (let ((th (lookup-theme name)))
          (when th
            (set-tui-ui-theme! st th)
            #t)))

(define*
  (tui-open-theme-picker! st #:optional (parent-menu #f))
  "Open the live-preview theme picker menu on ST."
  (let
      ((cur-name
         (or (badb-config-ui-theme (tui-cfg st)) (theme-name (tui-ui-theme st)))))
    (tui-set-active-menu! st
                          (make-theme-preview-menu cur-name
                                                   (lambda (n)
                                                     (tui-apply-live-theme! st n))
                                                   #:parent
                                                   parent-menu
                                                   #:back-action
                                                   (and (not parent-menu) 'back-to-slash)))))

(define (handle-config-cmd! st arg)
        (let*
            ((cfg (tui-cfg st)) (tokens (split-words arg))
             (subcmd
               (if (null? tokens)
                 "show"
                 (car tokens)))
             (rest
               (if (null? tokens)
                 '()
                 (cdr tokens))))
          (string-case
            (if (string-null? arg)
              ""
              subcmd)
            (""
             (tui-set-active-menu! st
                                   (make-menu "Configuration"
                                              (config-menu-items-for-state cfg
                                                                           (tui-cur-model st)
                                                                           (tui-cur-endpoint st)
                                                                           (tui-policy st))
                                              #:back-action
                                              'back-to-slash)))
            ("show"
             (tui-add-sys-msg! st
                               (format-config-summary cfg
                                                      (tui-cur-model st)
                                                      (tui-cur-endpoint st)
                                                      (tui-cur-journal-path st)
                                                      #:policy
                                                      (tui-policy st))))
            ("reload"
             (catch #t
               (lambda ()
                 (let ((new-cfg (load-config)))
                   (set-tui-cfg! st new-cfg)
                   (set-tui-cur-model! st (badb-config-model-name new-cfg))
                   (set-tui-cur-endpoint! st (badb-config-model-endpoint new-cfg))
                   (set-tui-cur-api-key! st
                                         (or (tui-initial-api-key st)
                                             (getenv "BADB_API_KEY")
                                             (getenv "OPENAI_API_KEY")
                                             (badb-config-model-api-key new-cfg)))
                   (tui-refresh-client! st)
                   (let ((new-mcp (make-mcp-manager new-cfg)))
                     (set-tui-mcp-mgr! st new-mcp)
                     (module-define! (tui-repl-mod st) 'cfg new-cfg)
                     (module-define! (tui-repl-mod st) 'mcp-mgr new-mcp))
                   (tui-add-sys-msg! st "Configuration reloaded from disk successfully.")))
               (lambda (k . a)
                 (tui-add-err-msg! st (format #f "Failed to reload config: ~a ~s" k a)))))
            ("save"
             (catch #t
               (lambda ()
                 (save-local-config! cfg)
                 (tui-add-sys-msg! st
                                   (format #f "Configuration saved to ~a" (default-local-config-path))))
               (lambda (k . a)
                 (tui-add-err-msg! st (format #f "Failed to save config: ~a ~s" k a)))))
            ("set"
             (if (< (length rest) 2)
               (tui-add-err-msg! st "Usage: /config set <model|endpoint|temperature> <value>")
               (let ((prop (car rest)) (val (cadr rest)))
                 (string-case prop
                              ("model" (set-tui-cur-model! st val)
                               (badb-config-set-model-name! cfg val)
                               (tui-refresh-client! st)
                               (tui-try-save-config! st)
                               (tui-add-sys-msg! st (format #f "Model set to '~a' and saved." val)))
                              ("endpoint" (tui-set-endpoint-url! st val "Endpoint set to '~a' and saved."))
                              ("temperature"
                               (let ((temp (string->number val)))
                                 (if (and temp (number? temp))
                                   (begin
                                     (badb-config-set-model-temperature! cfg temp)
                                     (tui-try-save-config! st)
                                     (tui-add-sys-msg! st (format #f "Temperature set to ~a and saved." temp)))
                                   (tui-add-err-msg! st "Invalid temperature: must be a number."))))
                              (else (tui-add-err-msg! st (format #f "Unknown config property: ~a" prop)))))))
            (else
              (tui-add-err-msg! st
                                (format #f
                                        "Unknown /config subcommand '~a'. Valid: show, reload, save, set"
                                        subcmd))))))

(define (handle-guardrail-cmd! st arg)
        (let
            ((trimmed (string-trim-both arg)) (policy (tui-policy st)))
          (if (string-null? trimmed)
            (tui-add-sys-msg! st (format-guardrail-status policy))
            (let ((sym (string->symbol trimmed)))
              (if
                  (member sym '(disabled permissive cautious strict paranoid))
                (begin (set-guardrail-policy-level! policy sym)
                       (tui-add-sys-msg! st (format #f "Guardrail policy level set to '~a'." sym)))
                (tui-add-err-msg! st
                                  "Valid levels: disabled, permissive, cautious, strict, paranoid"))))))

(define (handle-provider-cmd! st arg)
        (let ((trimmed (string-trim-both arg)))
          (if (string-null? trimmed)
            (tui-add-sys-msg! st
                              (format-provider-presets
                                (badb-config-providers (tui-cfg st))
                                (tui-cur-endpoint st)))
            (tui-switch-provider-preset! st trimmed))))

(define (handle-models-cmd! st)
        (call-with-values
          (lambda ()
            (fetch-live-models (tui-client st)))
          (lambda (ids err)
            (if (pair? ids)
              (tui-add-sys-msg! st
                                (format #f
                                        "Models on ~a:\n~a"
                                        (tui-cur-endpoint st)
                                        (string-join
                                          (map
                                            (lambda (id)
                                              (if
                                                  (and (string? (tui-cur-model st)) (string=? id (tui-cur-model st)))
                                                (format #f "• ~a (active)" id)
                                                (format #f "• ~a" id)))
                                            ids)
                                          "\n")))
              (tui-add-err-msg! st (format #f "Failed to list models: ~a" err))))))

(define (handle-theme-cmd! st arg)
        (let ((name (string-trim-both arg)))
          (cond
            ((string-null? name) (tui-open-theme-picker! st))
            ((lookup-theme name) =>
                                 (lambda (th)
                                   (badb-config-set-ui-theme! (tui-cfg st) name)
                                   (set-tui-ui-theme! st th)
                                   (tui-try-save-config! st)
                                   (tui-add-sys-msg! st (format #f "UI theme set to '~a' and saved." name))))
            (else
              (tui-add-err-msg! st
                                (format #f
                                        "Unknown theme '~a'. /theme alone opens the picker with all ~a themes."
                                        name
                                        (length (all-theme-names))))))))

(define*
  (handle-resume-cmd! st _arg #:optional (parent-menu #f))
  (let
      ((items (session-resurrection-menu-items (default-sessions-dir))))
    (if (null? items)
      (tui-add-sys-msg! st
                        "No saved sessions found. A session file is written after every chat turn.")
      (tui-set-active-menu! st
                            (make-menu "Resume Session"
                                       items
                                       #:parent
                                       parent-menu
                                       #:back-action
                                       (and (not parent-menu) 'back-to-slash))))))

(define (handle-compact-cmd! st _arg)
        (let*
            ((orig-tokens (estimate-conversation-tokens (tui-llm-history st)))
             (orig-count (length (tui-llm-history st)))
             (compacted (compact-messages (tui-compactor st) (tui-llm-history st)))
             (new-tokens (estimate-conversation-tokens compacted))
             (new-count (length compacted)))
          (set-tui-llm-history! st compacted)
          (tui-add-sys-msg! st
                            (format #f
                                    "Conversation context compacted:\n• Messages: ~a -> ~a\n• Estimated tokens: ~a -> ~a"
                                    orig-count
                                    new-count
                                    orig-tokens
                                    new-tokens))))

(define
  (checkpoint-conversation-path st checkpoint)
  (format #f
          "~a/~a.conversation.jsonl"
          (rollback-manager-storage-dir (tui-rollback-mgr st))
          (checkpoint-id checkpoint)))

(define
  (save-checkpoint-conversation! st checkpoint)
  (persist-session-file!
    (checkpoint-conversation-path st checkpoint)
    (tui-llm-history st)))

(define
  (restore-checkpoint-conversation! st checkpoint-id)
  (let
      ((checkpoint (get-checkpoint (tui-rollback-mgr st) checkpoint-id)))
    (and checkpoint
         (let*
             ((path (checkpoint-conversation-path st checkpoint))
              (history (load-session-file path)))
           (and (file-exists? path)
                (pair? history)
                (equal? (assoc-ref (car history) 'role) "system")
                history)))))

(define (handle-replay-cmd! st arg)
        (let*
            ((checkpoint-id (string-trim-both arg))
             (history
               (and (not (string-null? checkpoint-id))
                    (restore-checkpoint-conversation! st checkpoint-id))))
          (if history
            (let
                ((result (rollback-to-checkpoint! (tui-rollback-mgr st) checkpoint-id)))
              (if
                  (equal? (assoc-ref result 'status) "rolled_back")
                (begin (set-tui-llm-history! st history)
                       (persist-session-file! (tui-session-file st) history)
                       (set-tui-ctx-cache-key! st #f)
                       (set-tui-messages! st '())
                       (tui-add-sys-msg! st
                                         (format #f
                                                 "Replayed checkpoint ~a (~a)."
                                                 checkpoint-id
                                                 (assoc-ref result 'description))))
                (tui-add-err-msg! st (format #f "Replay failed: ~a" (assoc-ref result 'error)))))
            (tui-add-err-msg! st
                              "Replay needs a saved checkpoint ID. Use /checkpoint first; /journal lists checkpoint IDs."))))

(define (handle-fork-cmd! st arg)
        (let
            ((requested (string-trim-both arg)) (rb (tui-rollback-mgr st)))
          (if (string-null? requested)
            (let
                ((fork (create-checkpoint! rb "Conversation fork")))
              (if (save-checkpoint-conversation! st fork)
                (tui-add-sys-msg! st
                                  (format #f "Fork point saved as checkpoint ~a." (checkpoint-id fork)))
                (tui-add-err-msg! st
                                  "Fork point created, but its conversation snapshot could not be saved.")))
            (let
                ((restored (restore-checkpoint-conversation! st requested)))
              (if restored
                (let
                    ((result (rollback-to-checkpoint! rb requested)))
                  (if
                      (equal? (assoc-ref result 'status) "rolled_back")
                    (begin (set-tui-llm-history! st restored)
                           (set-tui-ctx-cache-key! st #f)
                           (set-tui-messages! st '())
                           (let
                               ((fork (create-checkpoint! rb (format #f "Fork of ~a" requested))))
                             (if (save-checkpoint-conversation! st fork)
                               (begin
                                 (persist-session-file! (tui-session-file st) restored)
                                 (tui-add-sys-msg! st
                                                   (format #f "Forked checkpoint ~a as ~a." requested (checkpoint-id fork))))
                               (tui-add-err-msg! st
                                                 "Fork checkpoint created, but its conversation snapshot could not be saved."))))
                    (tui-add-err-msg! st (format #f "Fork failed: ~a" (assoc-ref result 'error)))))
                (tui-add-err-msg! st
                                  "Fork checkpoint has no saved conversation. Use /checkpoint to create replayable checkpoints."))))))

(define (handle-checkpoint-cmd! st arg)
        (let*
            ((desc
               (if (string-null? (string-trim-both arg))
                 "User manual checkpoint"
                 (string-trim-both arg)))
             (chkpt (create-checkpoint! (tui-rollback-mgr st) desc)))
          (unless (save-checkpoint-conversation! st chkpt)
            (tui-add-err-msg! st
                              "Checkpoint created, but the conversation snapshot could not be saved."))
          (let* ((f-data (checkpoint-files chkpt))
                 (r-data (checkpoint-registry-state chkpt))
                 (f-cnt (if (hamt? f-data) (hamt-size f-data) (length f-data)))
                 (r-cnt (if (hamt? r-data) (hamt-size r-data) (length r-data))))
            (tui-add-sys-msg! st
                              (format #f
                                      "Created checkpoint '~a' (~a).\nFiles recorded: ~a, Registry entries: ~a."
                                      (checkpoint-id chkpt)
                                      (checkpoint-description chkpt)
                                      f-cnt
                                      r-cnt)))))

(define (handle-rollback-cmd! st arg)
        (let*
            ((trimmed (string-trim-both arg))
             (res
               (if (string-null? trimmed)
                 (rollback-latest! (tui-rollback-mgr st))
                 (rollback-to-checkpoint! (tui-rollback-mgr st) trimmed))))
          (if
              (equal? (assoc-ref res 'status) "rolled_back")
            (tui-add-sys-msg! st
                              (format #f
                                      "Successfully rolled back to checkpoint '~a' (~a).\nRestored ~a file(s), removed ~a new file(s)."
                                      (assoc-ref res 'checkpoint-id)
                                      (assoc-ref res 'description)
                                      (length (assoc-ref res 'restored-files))
                                      (length (assoc-ref res 'removed-files))))
            (tui-add-err-msg! st (format #f "Rollback failed: ~a" (assoc-ref res 'error))))))

(define (handle-dump-cmd! st arg)
        (let*
            ((trimmed (string-trim-both arg))
             (path
               (if (string-null? trimmed)
                 (string-append ".badb/conversation-"
                                (date->string (current-date 0) "~Y~m~dT~H~M~SZ")
                                ".txt")
                 trimmed)))
          (catch #t
            (lambda ()
              (ensure-directory! (dirname path))
              (call-with-output-file path
                                     (lambda (port)
                                       (display (format-conversation-transcript (tui-messages st)) port)))
              (tui-add-sys-msg! st (format #f "Conversation dumped to ~a" path)))
            (lambda (key . args)
              (tui-add-err-msg! st (format #f "Dump failed (~a): ~s" key args))))))

(define (handle-geis-cmd! st arg hooks)
        (let*
            ((cfg (tui-cfg st)) (jrn (tui-jrn st))
             (reg (tui-reg st))
             (nref (tui-nref st))
             (stdscr (tui-stdscr st))
             (getch (and nref (nref 'getch)))
             (beep (nref 'beep))
             (trimmed (string-trim-both arg))
             (space-pos (string-index trimmed #\space))
             (sub
               (if space-pos
                 (substring trimmed 0 space-pos)
                 trimmed))
             (rest-arg
               (if space-pos
                 (string-trim-both (substring trimmed (+ space-pos 1)))
                 ""))
             (format-geis-help-text (assq-ref hooks 'format-geis-help-text))
             (show-geis-bindings-uplot! (assq-ref hooks 'show-geis-bindings-uplot!))
             (spec-tool-status (assq-ref hooks 'spec-tool-status))
             (spec-tool-verify (assq-ref hooks 'spec-tool-verify))
             (add-sys!
               (lambda (m)
                 (tui-add-sys-msg! st m)))
             (add-err!
               (lambda (m)
                 (tui-add-err-msg! st m))))
          (cond
            ((or (string-null? trimmed)
                 (string=? sub "manager")
                 (string=? sub "user")
                 (string=? sub "project"))
              (handle-geasa-manager-cmd! st
                                         (if (string=? sub "project")
                                           "project"
                                           "user")))
            ((string=? sub "help")
              (tui-add-sys-msg! st
                                (if format-geis-help-text
                                  (format-geis-help-text)
                                  "")))
            ((string=? sub "bindings")
              (when show-geis-bindings-uplot!
                (show-geis-bindings-uplot! stdscr
                                           jrn
                                           (lambda ()
                                             (getch stdscr))))
              (tui-add-sys-msg! st (format-geis-bindings-graph jrn reg)))
            ((string=? sub "add")
              (if (string-null? rest-arg)
                (tui-add-sys-msg! st
                                  "Usage: /geis add <feature description>\nExample: /geis add retrieval for the numpy docs")
                (let
                    ((user-line (format #f "/geis add ~a" rest-arg)))
                  (tui-record-history! st user-line 'chat)
                  (tui-add-user-msg! st user-line)
                  (set-tui-status-text! st "SYNTHESIZING...")
                  (set-tui-current-run-id! st (+ (tui-current-run-id st) 1))
                  (set-tui-force-stop-requested?! st #f)
                  (set-tui-urgent-steer-requested?! st #f)
                  (tui-render! st)
                  (set-tui-agent-thread! st
                                         (call-with-new-thread
                                           (lambda ()
                                             (catch #t
                                               (lambda ()
                                                 (catch #t
                                                   (lambda ()
                                                     (geis-capture-pending-prompt! rest-arg (build-geis-add-prompt rest-arg)))
                                                   (lambda _
                                                     #f))
                                                 (tui-execute-chat-stream! st
                                                                           (build-geis-add-prompt rest-arg)
                                                                           (lambda (pair)
                                                                             (tui-handle-command! st pair hooks)))
                                                 (beep))
                                               (lambda (key . args)
                                                 (tui-add-err-msg! st (format #f "Geis synthesis error (~a): ~s" key args))
                                                 (set-tui-status-text! st "READY")
                                                 (tui-render! st)))))))))
            ((string=? sub "model")
              (if (string-null? rest-arg)
                (let
                    ((synth-model (badb-config-synthesis-model cfg #f)))
                  (tui-add-sys-msg! st
                                    (if synth-model
                                      (format #f
                                              "Geis synthesis model: ~a (configured in .badb/config.scm)\nUsage: /geis model [NAME]"
                                              synth-model)
                                      (format #f
                                              "Geis synthesis model: ~a (default)\nUsage: /geis model [NAME]"
                                              (badb-config-synthesis-model cfg)))))
                (let ((new-model (string-trim-both rest-arg)))
                  (badb-config-set-synthesis-model! cfg new-model)
                  (tui-try-save-config! st)
                  (tui-add-sys-msg! st
                                    (format #f "Geis synthesis model set to: ~a (saved to config)" new-model)))))
            ((string=? sub "list") (tui-add-sys-msg! st (spec-tool-status)))
            ((string=? sub "status")
              (tui-add-sys-msg! st
                                (apply spec-tool-status
                                       (if (string-null? rest-arg)
                                         '()
                                         (list rest-arg)))))
            ((string=? sub "verify")
              (if (string-null? rest-arg)
                (tui-add-sys-msg! st "Usage: /geis verify <feature-id>")
                (tui-add-sys-msg! st (spec-tool-verify rest-arg))))
            ((string=? sub "bootstrap") (execute-geis-bootstrap! add-sys! add-err!))
            ((string=? sub "onboard") (execute-geis-onboard! reg add-sys! add-err!))
            ((string=? sub "distill") (execute-geis-distill! rest-arg add-sys! add-err!))
            ((string=? sub "synthesize")
              (if (string-null? rest-arg)
                (tui-add-sys-msg! st
                                  "Usage: /geis synthesize <feature-id> [target-path] [spec-path]")
                (let*
                    ((parts
                       (filter
                         (lambda (s)
                           (not (string-null? s)))
                         (string-split rest-arg #\space)))
                     (fid (car parts))
                     (target-p (and (pair? (cdr parts)) (cadr parts)))
                     (spec-p (and (pair? (cdr parts)) (pair? (cddr parts)) (caddr parts))))
                  (set-tui-status-text! st "SYNTHESIZING...")
                  (tui-render! st)
                  (catch #t
                    (lambda ()
                      (let*
                          ((synth-fn
                             (module-ref
                               (false-if-exception (resolve-module '(geasa runtimes-synthesis plugin)))
                               'run-synthesis-workflow))
                           (res (synth-fn fid #:target-path target-p #:spec-path spec-p))
                           (status (assoc-ref res 'status))
                           (wf-result (assoc-ref res 'result)))
                        (if (eq? status 'passed)
                          (tui-add-sys-msg! st
                                            (format #f
                                                    "Geis synthesis passed for '~a'!\n~a"
                                                    fid
                                                    (if wf-result
                                                      (scm->json-string wf-result)
                                                      (or (assoc-ref res 'stdout) ""))))
                          (tui-add-err-msg! st
                                            (format #f
                                                    "Geis synthesis ~a for '~a'.\n~a\n~a"
                                                    status
                                                    fid
                                                    (or (assoc-ref res 'error) "")
                                                    (or (assoc-ref res 'stderr) ""))))))
                    (lambda (k . a)
                      (tui-add-err-msg! st (format #f "Geis synthesis error (~a): ~s" k a))))
                  (set-tui-status-text! st "READY"))))
            (else
              (tui-add-err-msg! st
                                (format #f
                                        "Unknown /geis subcommand '~a'. Type /geis help for available commands."
                                        sub))))))

(define (tui-apply-menu-selection! st item hooks)
        "Apply the action data carried by menu ITEM on ST."
        (let
            ((prev-menu (tui-active-menu st))
             (action (menu-item-action item))
             (cfg (tui-cfg st)))
          (define*
            (push-sub! m-or-title #:optional (items '()))
            (tui-set-active-menu! st
                                  (cond
                                    ((menu? m-or-title)
                                      (if prev-menu
                                        (menu-push prev-menu m-or-title)
                                        m-or-title))
                                    (prev-menu (menu-push prev-menu m-or-title items))
                                    (else (make-menu m-or-title items)))))
          (tui-clear-overlay! st)
          (cond
            ((symbol? action)
              (case action
                ((menu-back)
                  (let
                      ((parent (and prev-menu (menu-pop prev-menu))))
                    (when parent
                      (tui-set-active-menu! st parent)
                      (menu-trigger-highlight! parent))))
                ((back-to-slash) (tui-clear-overlay! st)
                                 (set-tui-prompt-mode! st 'slash)
                                 (set-tui-input-buffer! st "")
                                 (set-tui-cursor-pos! st 0)
                                 (set-tui-selector-index! st 0))
                ((open-config-menu)
                  (push-sub! "Configuration"
                             (config-menu-items-for-state cfg
                                                          (tui-cur-model st)
                                                          (tui-cur-endpoint st)
                                                          (tui-policy st))))
                ((open-model-menu)
                  (push-sub! "Model"
                             (build-live-or-recorded-model-menu-items (tui-client st) cfg (tui-cur-model st))))
                ((open-endpoint-menu)
                  (push-sub! "Endpoint" (endpoint-menu-items-for-config cfg)))
                ((open-provider-menu)
                  (push-sub! "Provider" (provider-menu-items cfg (tui-cur-endpoint st))))
                ((open-synthesizer-menu)
                  (push-sub! "Synthesiser" (synthesizer-config-menu-items cfg)))
                ((open-synth-provider-menu)
                  (push-sub! "Synthesis Provider" (synthesizer-provider-menu-items cfg)))
                ((open-synth-model-menu)
                  (push-sub! "Synthesis Model" (synthesizer-model-menu-items cfg)))
                ((open-theme-menu) (tui-open-theme-picker! st prev-menu))
                ((open-guardrail-menu) (push-sub! "Guardrail" (guardrail-menu-items)))
                ((open-keymap-menu)
                  (push-sub! "Keybindings" (keymap->menu-items (tui-active-keymap st))))
                ((open-resume-menu) (handle-resume-cmd! st "" prev-menu))
                ((prefill-model) (tui-prefill-input! st "/model "))
                ((prefill-endpoint) (tui-prefill-input! st "/endpoint "))
                ((prefill-temperature) (tui-prefill-input! st "/config set temperature "))
                ((prefill-synth-model) (tui-prefill-input! st "/geis model "))
                ((config-reload) (handle-config-cmd! st "reload"))
                ((config-save) (handle-config-cmd! st "save"))
                (else (tui-add-err-msg! st (format #f "Unknown menu action: ~a" action)))))
            ((and (pair? action) (eq? (car action) 'set-model))
              (let ((m (cdr action)))
                (set-tui-cur-model! st m)
                (badb-config-set-model-name! cfg m)
                (tui-refresh-client! st)
                (tui-try-save-config! st)
                (tui-add-sys-msg! st (format #f "Model switched to: ~a (saved to config)" m))))
            ((and (pair? action) (eq? (car action) 'switch-provider))
              (tui-switch-provider-preset! st (cdr action)))
            ((and (pair? action) (eq? (car action) 'set-synth-provider))
              (let ((prov (cdr action)))
                (apply-synthesis-provider-config! cfg prov)
                (tui-try-save-config! st)
                (tui-add-sys-msg! st
                                  (if prov
                                    (format #f "Synthesis provider set to '~a' (saved to config)." prov)
                                    "Synthesis provider reset to inherit chat provider (saved to config)."))))
            ((and (pair? action) (eq? (car action) 'set-synth-model))
              (let ((m (cdr action)))
                (badb-config-set-synthesis-model! cfg m)
                (tui-try-save-config! st)
                (tui-add-sys-msg! st
                                  (format #f "Geis synthesis model set to: ~a (saved to config)" m))))
            ((and (pair? action) (eq? (car action) 'set-theme))
              (let ((name (cdr action)))
                (badb-config-set-ui-theme! cfg name)
                (tui-apply-live-theme! st name)
                (tui-try-save-config! st)
                (tui-add-sys-msg! st (format #f "UI theme set to '~a' and saved." name))))
            ((and (pair? action) (eq? (car action) 'set-guardrail))
              (handle-guardrail-cmd! st (symbol->string (cdr action))))
            ((and (pair? action) (eq? (car action) 'open-command-category))
              (tui-set-active-menu! st
                                    (open-command-category-menu
                                      (or prev-menu (make-commands-root-menu))
                                      (cdr action))))
            ((and (pair? action) (eq? (car action) 'run-command))
              (tui-handle-command! st (cons (cdr action) "") hooks))
            ((and (pair? action) (eq? (car action) 'prefill-command))
              (tui-prefill-input! st (cdr action)))
            ((and (pair? action) (eq? (car action) 'keymap-bind))
              (tui-add-sys-msg! st
                                (format #f
                                        "Keybinding ~a in ~a mode (customize via keymap-bind!)."
                                        (cdr action)
                                        (car action))))
            ((and (pair? action) (eq? (car action) 'resume-session))
              (let*
                  ((path (cdr action))
                   (loaded (load-session-file path))
                   (turn-count
                     (length
                       (filter
                         (lambda (m)
                           (not (string=? (assoc-ref m 'role) "system")))
                         loaded))))
                (if (null? loaded)
                  (tui-add-err-msg! st
                                    (format #f "Session file ~a is empty or unreadable." (basename path)))
                  (begin (set-tui-llm-history! st loaded)
                         (set-tui-messages! st (session-turns->transcript-messages loaded))
                         (set-tui-ctx-cache-key! st #f)
                         (set-tui-scroll-offset! st 0)
                         (tui-add-sys-msg! st
                                           (format #f
                                                   "Resumed session ~a (~a turns)"
                                                   (truncate-string (basename path) %session-filename-max-length)
                                                   turn-count))))))
            (else (tui-add-err-msg! st "Unsupported menu action.")))))

(define (handle-geasa-manager-cmd! st arg)
        "Open the /geasa manager modal overlay (optionally targeting user or project scope)."
        (let*
            ((trimmed (string-trim-both (or arg "")))
             (scope
               (if (string=? trimmed "project")
                 'project
                 'user)))
          (register-geis-updater-extensions!)
          (tui-push-modal-overlay! st
                                   (make-geis-manager-overlay #:scope scope #:workspace (getcwd)))))

(define (tui-open-notifications-modal! st)
        "Open the /notifications modal overlay."
        (tui-push-modal-overlay! st (make-notifications-modal-overlay)))

(define (tui-open-mcp-modal! st)
        "Open the /mcp manager modal overlay."
        (let ((mgr (tui-mcp-mgr st)))
          (tui-push-modal-overlay!
            st
            (make-mcp-manager-overlay
              #:list-servers (lambda (_scope _ws) (if mgr (mcp-manager-list-servers mgr) '()))
              #:reconnect-server! (lambda (_scope name _ws)
                                    (when mgr
                                      (mcp-manager-stop-server! mgr name)
                                      (mcp-manager-start-server! mgr name)))))))

(define* (tui-open-session-browser-modal! st #:optional (active-tab 'sessions))
         "Open the /sessions & /journal modal overlay."
         (tui-push-modal-overlay!
           st
           (make-session-browser-overlay
             #:active-tab active-tab
             #:sessions-provider (lambda () (recent-session-summaries))
             #:journal (tui-jrn st)
             #:rollback-manager (tui-rollback-mgr st)
             #:on-resume (lambda (sess)
                           (let ((p (assoc-ref sess 'path)))
                             (when (and (string? p) (file-exists? p))
                               (let ((loaded (load-session-file p)))
                                 (when loaded
                                   (set-tui-messages! st (or (assoc-ref loaded 'messages) '()))
                                   (set-tui-llm-history! st (or (assoc-ref loaded 'llm-history) '()))))))))))

(define (collect-memory-entries)
        "Scan project (.badb/memories/) and user (~/.config/badb/memories/) markdown files."
        (let* ((clean? (equal? (getenv "BADB_CLEAN") "1"))
               (home (or (getenv "HOME") ""))
               (xdg (getenv "XDG_CONFIG_HOME"))
               (user-dir (if (and xdg (not (string-null? xdg)))
                           (string-append xdg "/badb/memories")
                           (string-append home "/.config/badb/memories")))
               (dirs (if clean?
                       '()
                       (list (cons "project" (string-append (getcwd) "/.badb/memories"))
                             (cons "user" user-dir))))
               (read-dir
                 (lambda (scope dir)
                   (if (not (file-exists? dir))
                     '()
                     (let ((files (sort (or (scandir dir (lambda (f) (string-suffix? ".md" f))) '()) string<?)))
                       (filter-map
                         (lambda (f)
                           (let ((p (string-append dir "/" f)))
                             (catch #t
                               (lambda ()
                                 `((name . ,(if (string-suffix? ".md" f)
                                              (substring f 0 (- (string-length f) 3))
                                              f))
                                   (scope . ,scope)
                                   (path . ,p)
                                   (content . ,(file->string p))))
                               (lambda _ #f))))
                         files))))))
          (append-map (lambda (pair) (read-dir (car pair) (cdr pair))) dirs)))

(define (collect-skill-entries reg)
        "Convert <skill> records in REG to knowledge-browser/context-inspector alists."
        (if (not reg)
          '()
          (let ((active-ids (map skill-id (skill-registry-active-skills reg))))
            (map (lambda (sk)
                   (let ((sid (skill-id sk)))
                     `((id . ,sid)
                       (name . ,(or (skill-name sk) sid))
                       (description . ,(or (skill-description sk) ""))
                       (content . ,(or (skill-body sk) (skill-description sk) ""))
                       (path . ,(or (skill-path sk) ""))
                       (enabled? . ,(and (member sid active-ids) #t)))))
                 (skill-registry-list reg)))))

(define* (tui-open-context-inspector-modal! st #:optional (active-tab 'budget))
         "Open the /context & /usage modal overlay."
         (let* ((comp (tui-compactor st))
                (hist (tui-llm-history st))
                (used (estimate-conversation-tokens hist))
                (total (if comp (context-compactor-effective-ceiling comp) 128000))
                (skill-items
                  (map (lambda (sk)
                         `((kind . skill)
                           (name . ,(assoc-ref sk 'name))
                           (tokens . ,(quotient (string-length (or (assoc-ref sk 'content) "")) 4))
                           (detail . ,(or (assoc-ref sk 'description) (assoc-ref sk 'path) ""))))
                       (collect-skill-entries (tui-skill-reg st))))
                (mem-items
                  (map (lambda (m)
                         `((kind . memory)
                           (name . ,(assoc-ref m 'name))
                           (tokens . ,(quotient (string-length (or (assoc-ref m 'content) "")) 4))
                           (detail . ,(or (assoc-ref m 'content) ""))))
                       (collect-memory-entries))))
           (tui-push-modal-overlay!
             st
             (make-context-inspector-overlay
               #:active-tab active-tab
               #:budget `((total . ,total)
                          (used . ,used)
                          (breakdown . (("history" . ,used))))
               #:items (append skill-items mem-items)
               #:usage `((prompt-tokens . ,(tui-session-prompt-tokens st))
                         (completion-tokens . ,(tui-session-completion-tokens st))
                         (cached-tokens . 0)
                         (turns . ,(tui-session-turns st)))
               #:on-compact (lambda () (handle-compact-cmd! st ""))))))

(define (tui-open-agent-monitor-modal! st)
        "Open the /agents live subagent mission control overlay."
        (tui-push-modal-overlay!
          st
          (make-agent-monitor-overlay
            #:list-agents (lambda ()
                            (map (lambda (lnk)
                                   `((id . ,(subagent-link-child-id lnk))
                                     (role . ,(or (subagent-link-label lnk) "subagent"))
                                     (status . ,(subagent-link-status lnk))
                                     (iterations . ,(subagent-link-turn-count lnk))))
                                 (all-subagent-links))))))

(define* (tui-open-command-palette! st #:optional (hooks '()) (active-tab 'commands))
         "Open the Ctrl+K / /keys fuzzy command palette overlay."
         (let ((km (tui-active-keymap st)))
           (tui-push-modal-overlay!
             st
             (make-command-palette-overlay
               #:active-tab active-tab
               #:commands (map (lambda (p)
                                 `((label . ,(string-append "/" (car p)))
                                   (description . ,(cdr p))
                                   (command . ,(car p))))
                               %slash-commands)
               #:keybindings (map (lambda (b)
                                    (let ((act (symbol->string (assoc-ref b 'action)))
                                          (ctx (symbol->string (assoc-ref b 'context))))
                                      `((id . ,(string-append ctx ":" act))
                                        (key . ,(format-key-spec (assoc-ref b 'key)))
                                        (label . ,(format #f "~a (~a)" act ctx))
                                        (action . ,act)
                                        (context . ,ctx)
                                        (description . ,(assoc-ref b 'description)))))
                                  (keymap-bindings km))
               #:on-execute (lambda (cmd-entry)
                              (let ((c (or (assoc-ref cmd-entry 'command)
                                           (assoc-ref cmd-entry 'label))))
                                (when (string? c)
                                  (tui-pop-modal-overlay! st)
                                  (tui-handle-command!
                                    st
                                    (parse-command (if (string-prefix? "/" c) c (string-append "/" c)))
                                    hooks))))
               #:on-rebind (lambda (old-b new-key)
                             (let ((act (string->symbol (or (assoc-ref old-b 'action) "noop")))
                                   (ctx (string->symbol (or (assoc-ref old-b 'context) "global"))))
                               (keymap-bind! km (parse-key-spec new-key) act #:context ctx)))))))

(define (tui-open-config-modal! st)
        "Open the /config tabbed workbench overlay."
        (let*
            ((cfg (tui-cfg st)) (pol (tui-policy st))
             (cur-theme
               (or
                 (and (tui-ui-theme st) (theme-name (tui-ui-theme st)))
                 (and cfg (badb-config-ui-theme cfg))
                 "badb-dark"))
             (cur-ep (tui-cur-endpoint st))
             (cur-model (ensure-string (or (tui-cur-model st) "")))
             (allow-missing? (and cfg (badb-config-allow-missing-thinking? cfg cur-ep)))
             (synth-model (or (and cfg (badb-config-synthesis-model cfg)) "auto"))
             (recorded-models
               (if cfg
                 (config-recorded-models cfg)
                 '()))
             (synth-choices
               (delete-duplicates
                 (cons synth-model (append '("auto" "local") recorded-models))
                 equal?)))
          (tui-push-modal-overlay! st
                                   (make-config-modal-overlay #:providers
                                                              (let*
                                                                  ((raw-provs
                                                                     (if cfg
                                                                       (badb-config-providers cfg)
                                                                       '()))
                                                                   (preset-entries
                                                                     (map
                                                                       (lambda (p)
                                                                         (let*
                                                                             ((pname (ensure-string (car p)))
                                                                              (plist
                                                                                (if (list? (cdr p))
                                                                                  (cdr p)
                                                                                  '()))
                                                                              (raw-ep (get-prop plist 'endpoint ""))
                                                                              (ep (ensure-string (or raw-ep "")))
                                                                              (raw-mdl
                                                                                (or (get-prop plist 'model #f)
                                                                                    (let ((ms (get-prop plist 'models #f))) (and (pair? ms) (car ms)))
                                                                                    ""))
                                                                              (mdl (ensure-string raw-mdl))
                                                                              (summary
                                                                                (if (string-null? mdl)
                                                                                  ep
                                                                                  (format #f "~a @ ~a" mdl ep)))
                                                                              (active? (and (string? cur-ep) (string? ep) (string=? ep cur-ep))))
                                                                           `((key . ,pname) (label . ,pname)
                                                                             (summary . ,summary)
                                                                             (active? . ,active?)
                                                                             (value .
                                                                                    ,(if active?
                                                                                       (string-append "★ " summary)
                                                                                       summary))
                                                                             (description .
                                                                                          ,(format #f "Endpoint: ~a | Model: ~a (Space/Enter to activate)" ep mdl)))))
                                                                       raw-provs))
                                                                   (base-entries
                                                                     (list
                                                                       `((key . "change-model") (label . "Model")
                                                                         (action . open-model-menu)
                                                                         (value .
                                                                                ,(if (string-null? cur-model)
                                                                                   "(none)"
                                                                                   cur-model))
                                                                         (description .
                                                                                      "Space/Enter to pick from live/recorded models or set a custom model"))
                                                                       `((key . "change-endpoint") (label . "Endpoint")
                                                                         (action . open-endpoint-menu)
                                                                         (value .
                                                                                ,(if (string-null? (ensure-string (or cur-ep "")))
                                                                                   "(none)"
                                                                                   cur-ep))
                                                                         (description . "Space/Enter to pick an endpoint preset or set a custom URL")))))
                                                                (append base-entries preset-entries))
                                                              #:guardrails
                                                              (list
                                                                `((key . "level") (label . "Guardrail level")
                                                                  (value .
                                                                         ,(if pol
                                                                            (symbol->string (guardrail-policy-level pol))
                                                                            "cautious"))
                                                                  (choices . ("strict" "cautious" "permissive"))
                                                                  (description . "Space/Enter to cycle: strict / cautious / permissive"))
                                                                `((key . "allow-missing-thinking")
                                                                  (label . "Allow missing thinking")
                                                                  (type . boolean)
                                                                  (value . ,(and allow-missing? #t))
                                                                  (description . "Space/Enter to toggle whether thinking field may be omitted")))
                                                              #:themes
                                                              (map
                                                                (lambda (tname)
                                                                  (let ((tstr (ensure-string tname)))
                                                                    `((key . ,tstr) (label . ,tstr)
                                                                      (value .
                                                                             ,(if (string-ci=? tstr (ensure-string cur-theme))
                                                                                "active"
                                                                                ""))
                                                                      (description .
                                                                                   ,(format #f "Highlight to preview ~a; Space/Enter to apply" tstr)))))
                                                                (all-theme-names))
                                                              #:synthesizer
                                                              (list
                                                                `((key . "synthesis-model")
                                                                  (label . "Synthesis model")
                                                                  (value . ,synth-model)
                                                                  (choices . ,synth-choices)
                                                                  (description . "Space/Enter to cycle synthesis model"))
                                                                `((key . "choose-synth-model")
                                                                  (label . "Choose/Set synthesis model")
                                                                  (action . open-synth-model-menu)
                                                                  (value . ,synth-model)
                                                                  (description .
                                                                               "Space/Enter to pick from recorded models or enter a custom model"))
                                                                `((key . "choose-synth-provider")
                                                                  (label . "Synthesis provider")
                                                                  (action . open-synth-provider-menu)
                                                                  (value . "configure")
                                                                  (description .
                                                                               "Space/Enter to pick synthesis provider or inherit chat provider")))
                                                              #:on-highlight
                                                              (lambda (item)
                                                                (when (pair? item)
                                                                  (cond
                                                                    ((eq? (assq-ref item 'tab) 'action)
                                                                      (let ((act (assq-ref item 'action)))
                                                                        (when (symbol? act)
                                                                          (tui-pop-modal-overlay! st)
                                                                          (tui-apply-menu-selection! st (cons (cons "" "") act) #f))))
                                                                    ((eq? (assq-ref item 'tab) 'providers)
                                                                      (let ((pname (assq-ref item 'key)))
                                                                        (when
                                                                            (and (string? pname) (not (string=? pname "active")))
                                                                          (handle-provider-cmd! st pname))))
                                                                    (else
                                                                      (let ((tname (assq-ref item 'key)))
                                                                        (when (string? tname)
                                                                          (tui-apply-live-theme! st tname)
                                                                          (set-tui-render-dirty?! st #t)))))))
                                                              #:on-save
                                                              (lambda (scope snapshot)
                                                                (let*
                                                                    ((themes-list (or (assq-ref snapshot 'theme) '()))
                                                                     (active-th
                                                                       (find
                                                                         (lambda (it)
                                                                           (equal? (assq-ref it 'value) "active"))
                                                                         themes-list))
                                                                     (guard-list (or (assq-ref snapshot 'guardrails) '()))
                                                                     (lvl-entry
                                                                       (find
                                                                         (lambda (it)
                                                                           (equal? (assq-ref it 'key) "level"))
                                                                         guard-list))
                                                                     (lvl-str (and lvl-entry (assq-ref lvl-entry 'value)))
                                                                     (synth-list (or (assq-ref snapshot 'synthesizer) '()))
                                                                     (synth-entry
                                                                       (find
                                                                         (lambda (it)
                                                                           (equal? (assq-ref it 'key) "synthesis-model"))
                                                                         synth-list))
                                                                     (synth-val (and synth-entry (assq-ref synth-entry 'value))))
                                                                  (when active-th
                                                                    (let ((tname (assq-ref active-th 'key)))
                                                                      (tui-apply-live-theme! st tname)
                                                                      (when cfg (badb-config-set-ui-theme! cfg tname))))
                                                                  (when (and pol (string? lvl-str))
                                                                    (set-guardrail-policy-level! pol (string->symbol lvl-str)))
                                                                  (when (and cfg (string? synth-val))
                                                                    (badb-config-set-synthesis-model! cfg synth-val))
                                                                  (unless (equal? (getenv "BADB_CLEAN") "1")
                                                                    (if (eq? scope 'user)
                                                                      (catch #t
                                                                        (lambda ()
                                                                          (save-global-config! cfg))
                                                                        (lambda _
                                                                          #f))
                                                                      (tui-try-save-config! st)))))))))

(define (%run-git-capture args)
        (catch #t
          (lambda ()
            (let* ((p (apply open-pipe* OPEN_READ "git" args))
                   (out (let loop ((acc '()))
                                  (let ((ln (read-line p)))
                                    (if (eof-object? ln)
                                      (string-join (reverse acc) "\n")
                                      (loop (cons ln acc)))))))
              (close-pipe p)
              out))
          (lambda _ "")))

(define (tui-open-diff-review-modal! st)
        "Open the /diff workspace hunk browser overlay."
        (let* ((status-out (%run-git-capture '("status" "--porcelain")))
               (files (filter-map
                        (lambda (ln)
                          (and (> (string-length ln) 3)
                               `((path . ,(string-trim-both (substring ln 3)))
                                 (status . ,(string-trim-both (substring ln 0 2))))))
                        (string-split status-out #\newline))))
          (tui-push-modal-overlay!
            st
            (make-diff-review-overlay
              #:files files
              #:diff-fn (lambda (entry)
                          (let ((p (and (pair? entry) (assoc-ref entry 'path))))
                            (if (string? p)
                              (%run-git-capture (list "diff" "--" p))
                              "")))))))

(define* (tui-open-knowledge-browser-modal! st #:optional (active-tab 'skills))
         "Open the /skills & /memories tabbed browser overlay."
         (let ((sreg (tui-skill-reg st)))
           (tui-push-modal-overlay!
             st
             (make-knowledge-browser-overlay
               #:active-tab active-tab
               #:skills (collect-skill-entries sreg)
               #:memories (collect-memory-entries)
               #:on-toggle-skill (lambda (sk new-val)
                                   (when sreg
                                     (let ((sid (or (assq-ref sk 'id) (assq-ref sk 'name))))
                                       (when sid
                                         (if new-val
                                           (skill-registry-enable! sreg sid)
                                           (skill-registry-disable! sreg sid))))))
               #:on-delete-memory (lambda (mem)
                                    (let ((p (assq-ref mem 'path)))
                                      (when (and (string? p) (file-exists? p))
                                        (catch #t (lambda () (delete-file p)) (lambda _ #f)))))))))

(define (tui-register-modular-extensions! st hooks)
        "Invoke all modular register-*-tui! hooks against ST's TUI registry."
        (let ((reg-cat (lambda (_p _d) #t))
              (reg-cmd (lambda (_n _d _a . _r) #t))
              (open-ov (lambda (ov) (tui-push-modal-overlay! st ov))))
          (register-notifications-tui! #:register-category! reg-cat #:register-command! reg-cmd #:open-overlay! open-ov)
          (register-mcp-manager-tui! #:register-category! reg-cat #:register-command! reg-cmd #:open-overlay! open-ov)
          (register-session-browser-tui! #:register-category! reg-cat #:register-command! reg-cmd #:open-overlay! open-ov)
          (register-context-inspector-tui! #:register-command! reg-cmd #:push-overlay! open-ov)
          (register-agent-monitor-tui! #:register-category! reg-cat #:register-command! reg-cmd #:open-overlay! open-ov)
          (register-command-palette-tui! #:register-command! reg-cmd #:push-overlay! open-ov)
          (register-config-modal-tui! #:register-category! reg-cat #:register-command! reg-cmd #:open-overlay! open-ov)
          (register-diff-review-tui! #:register-category! reg-cat #:register-command! reg-cmd #:open-overlay! open-ov)
          (register-knowledge-browser-tui! #:register-category! reg-cat #:register-command! reg-cmd #:open-overlay! open-ov)
          (register-drawer-tui! #:register-category! reg-cat #:register-command! reg-cmd
                                #:on-toggle-drawer! (lambda () (tui-toggle-drawer! st)))))

(define (tui-handle-command! st cmd-pair hooks)
        "Dispatch parsed slash command CMD-PAIR on ST."
        (let*
            ((cmd (resolve-command-alias (car cmd-pair)))
             (arg (cdr cmd-pair))
             (cfg (tui-cfg st))
             (reg (tui-reg st))
             (jrn (tui-jrn st))
             (nref (tui-nref st))
             (stdscr (tui-stdscr st))
             (getch (and nref (nref 'getch)))
             (add-sys!
               (lambda (m)
                 (tui-add-sys-msg! st m)))
             (add-err!
               (lambda (m)
                 (tui-add-err-msg! st m)))
             (format-git-diff-output (assq-ref hooks 'format-git-diff-output))
             (render-ncvisual-overlay! (assq-ref hooks 'render-ncvisual-overlay!)))
          (cond
            ((string=? cmd "help") (tui-add-sys-msg! st %tui-help-message))
            ((or (string=? cmd "commands") (string=? cmd "menu"))
              (tui-set-active-menu! st (make-commands-root-menu)))
            ((string=? cmd "keys")
              (tui-open-command-palette! st hooks 'keybindings))
            ((string=? cmd "notifications")
              (tui-open-notifications-modal! st))
            ((string=? cmd "sessions")
              (tui-open-session-browser-modal! st 'sessions))
            ((string=? cmd "agents")
              (tui-open-agent-monitor-modal! st))
            ((string=? cmd "drawer")
              (let ((sub (string-downcase (string-trim-both (or arg "")))))
                (if (member sub '("next" "cycle"))
                  (tui-cycle-drawer-panel! st)
                  (tui-toggle-drawer! st))))
            ((string=? cmd "todo")
              (call-with-values
                (lambda ()
                  (handle-todo-command! (tui-active-todo-list st) arg))
                (lambda (updated txt)
                  (set-tui-active-todo-list! st updated)
                  (tui-add-sys-msg! st txt))))
            ((string=? cmd "diff")
              (if (string-null? (string-trim-both (or arg "")))
                (tui-open-diff-review-modal! st)
                (tui-add-sys-msg! st
                                  (if format-git-diff-output
                                    (format-git-diff-output arg)
                                    ""))))
            ((string=? cmd "edit")
              (let*
                  ((parts (split-words arg))
                   (path (and (pair? parts) (car parts)))
                   (line-num (and (pair? parts) (pair? (cdr parts)) (string->number (cadr parts)))))
                (if path
                  (call-with-values
                    (lambda ()
                      (open-in-external-editor path
                                               #:line
                                               line-num
                                               #:suspend-tty!
                                               (lambda ()
                                                 (tui-suspend-tty! st))
                                               #:resume-tty!
                                               (lambda ()
                                                 (tui-resume-tty! st))))
                    (lambda (ok? res)
                      (unless ok? (tui-add-err-msg! st (format #f "Editor error: ~a" res)))))
                  (tui-add-sys-msg! st "Usage: /edit <file> [line]"))))
            ((string=? cmd "guardrail") (handle-guardrail-cmd! st arg))
            ((string=? cmd "copy")
              (let* ((sub (string-downcase (string-trim-both (or arg ""))))
                     (msgs (tui-messages st))
                     (assistant-msgs
                       (filter (lambda (m)
                                 (let ((r (prop-ref m 'role "")))
                                   (and (string? r) (string=? r "assistant"))))
                               msgs)))
                (cond
                  ((null? assistant-msgs)
                    (tui-add-sys-msg! st "Nothing to copy — no assistant reply yet."))
                  ((or (string=? sub "code") (string=? sub "block"))
                    (let loop ((rest (reverse assistant-msgs)))
                              (if (null? rest)
                                (tui-add-sys-msg! st "Nothing to copy — no fenced code block found.")
                                (let ((blocks (extract-fenced-code-blocks (or (prop-ref (car rest) 'content) ""))))
                                  (if (pair? blocks)
                                    (let ((code (car (reverse blocks))))
                                      (clipboard-copy! code)
                                      (set-tui-status-text! st (format #f "COPIED CODE (~a chars)" (string-length code)))
                                      (tui-add-sys-msg! st (format #f "Copied code block to clipboard (~a chars)." (string-length code))))
                                    (loop (cdr rest)))))))
                  (else
                    (let* ((last-msg (car (reverse assistant-msgs)))
                           (txt (ensure-string (or (prop-ref last-msg 'content) ""))))
                      (if (string-null? txt)
                        (tui-add-sys-msg! st "Nothing to copy — last assistant reply is empty.")
                        (begin
                          (clipboard-copy! txt)
                          (set-tui-status-text! st (format #f "COPIED (~a chars)" (string-length txt)))
                          (tui-add-sys-msg! st (format #f "Copied last reply to clipboard (~a chars)." (string-length txt))))))))))
            ((string=? cmd "fold")
              (let* ((sub (string-downcase (string-trim-both (or arg ""))))
                     (target (cond
                               ((member sub '("collapse" "on" "all" "close")) #t)
                               ((member sub '("expand" "off" "open" "unfold")) #f)
                               (else 'toggle))))
                (call-with-values
                  (lambda () (transcript-fold-all! (tui-messages st) target))
                  (lambda (next-msgs collapsed? changed)
                    (set-tui-messages! st next-msgs)
                    (row-cache-clear! (tui-row-cache st))
                    (let ((verb (if collapsed? "Folded" "Unfolded")))
                      (set-tui-status-text! st (format #f "~a ~a card~a" verb changed (if (= changed 1) "" "s")))
                      (tui-add-sys-msg! st (format #f "~a ~a tool/thinking card~a." verb changed (if (= changed 1) "" "s"))))))))
            ((string=? cmd "find")
              (let* ((q (string-trim-both (or arg "")))
                     (ts0 (transcript-search-open (tui-transcript-search st)))
                     (ts1 (if (string-null? q)
                            ts0
                            (transcript-search-with-input-active
                              (transcript-search-update ts0
                                                        q
                                                        (tui-messages st)
                                                        (tui-last-content-width st)
                                                        (tui-row-cache st))
                              #f))))
                (set-tui-transcript-search! st ts1)
                (let ((scroll (transcript-search-scroll-offset
                                ts1
                                (tui-asm-total st)
                                (tui-last-view-height st))))
                  (when scroll
                    (set-tui-scroll-offset! st scroll)))))
            ((string=? cmd "compact") (handle-compact-cmd! st arg))
            ((string=? cmd "resume") (handle-resume-cmd! st arg))
            ((string=? cmd "checkpoint") (handle-checkpoint-cmd! st arg))
            ((string=? cmd "rollback") (handle-rollback-cmd! st arg))
            ((string=? cmd "fork") (handle-fork-cmd! st arg))
            ((string=? cmd "replay") (handle-replay-cmd! st arg))
            ((string=? cmd "rules") (tui-add-sys-msg! st (format-rules-and-tree (getcwd))))
            ((string=? cmd "geis") (handle-geis-cmd! st arg hooks))
            ((string=? cmd "geasa") (handle-geis-cmd! st arg hooks))
            ((string=? cmd "context")
              (if (string-null? (string-trim-both (or arg "")))
                (tui-open-context-inspector-modal! st 'budget)
                (begin
                  (unless (tui-agent-running? st) (tui-sync-context-limit! st))
                  (execute-context-subcommand! (tui-compactor st)
                                               (tui-llm-history st)
                                               (tui-messages st)
                                               (tui-ctx-idx st)
                                               arg
                                               add-sys!
                                               add-err!
                                               #:jrn
                                               jrn
                                               #:reg
                                               reg))))
            ((string=? cmd "skills")
              (if (string-null? (string-trim-both (or arg "")))
                (tui-open-knowledge-browser-modal! st 'skills)
                (execute-skills-subcommand! (tui-skill-reg st) arg add-sys! add-err!)))
            ((string=? cmd "memories")
              (if (string-null? (string-trim-both (or arg "")))
                (tui-open-knowledge-browser-modal! st 'memories)
                (execute-memories-subcommand! arg add-sys! add-err!)))
            ((string=? cmd "usage")
              (if (string-null? (string-trim-both (or arg "")))
                (tui-open-context-inspector-modal! st 'usage)
                (begin
                  (unless (tui-agent-running? st) (tui-sync-context-limit! st))
                  (tui-add-sys-msg! st
                                    (format-usage-report (tui-compactor st)
                                                         (tui-llm-history st)
                                                         (tui-session-prompt-tokens st)
                                                         (tui-session-completion-tokens st)
                                                         (tui-session-estimated-tokens st)
                                                         (tui-session-turns st)
                                                         #:ctx-idx
                                                         (tui-ctx-idx st))))))
            ((string=? cmd "models") (handle-models-cmd! st))
            ((string=? cmd "model")
              (if (string-null? arg)
                (tui-set-active-menu! st
                                      (make-menu "Model"
                                                 (build-live-or-recorded-model-menu-items (tui-client st) cfg (tui-cur-model st))
                                                 #:back-action
                                                 'back-to-slash))
                (begin (set-tui-cur-model! st arg)
                       (badb-config-set-model-name! cfg arg)
                       (tui-refresh-client! st)
                       (tui-try-save-config! st)
                       (tui-add-sys-msg! st (format #f "Model switched to: ~a (saved to config)" arg)))))
            ((string=? cmd "endpoint")
              (if (string-null? arg)
                (tui-set-active-menu! st
                                      (make-menu "Endpoint"
                                                 (endpoint-menu-items-for-config cfg)
                                                 #:back-action
                                                 'back-to-slash))
                (tui-set-endpoint-url! st arg "Endpoint updated to: ~a (saved to config)")))
            ((string=? cmd "provider") (handle-provider-cmd! st arg))
            ((string=? cmd "mcp")
              (if (string-null? (string-trim-both (or arg "")))
                (tui-open-mcp-modal! st)
                (execute-mcp-subcommand! (tui-mcp-mgr st) arg add-sys! add-err!)))
            ((string=? cmd "config")
              (if (string-null? (string-trim-both (or arg "")))
                (tui-open-config-modal! st)
                (handle-config-cmd! st arg)))
            ((string=? cmd "theme") (handle-theme-cmd! st arg))
            ((string=? cmd "sh")
              (if (string-null? arg)
                (tui-add-sys-msg! st "Usage: /sh <command> (or !<command>)")
                (begin (tui-add-cmd-msg! st arg) (tui-execute-shell! st arg))))
            ((string=? cmd "eval")
              (if (string-null? arg)
                (tui-add-sys-msg! st
                                  "Usage: /eval <scheme-code> (or ,<scheme-code> or (<scheme-code>))")
                (begin (tui-add-scm-msg! st arg) (tui-execute-scheme! st arg))))
            ((string=? cmd "features") (tui-add-sys-msg! st (format-features-list reg)))
            ((string=? cmd "journal")
              (if (string-null? (string-trim-both (or arg "")))
                (tui-open-session-browser-modal! st 'checkpoints)
                (tui-add-sys-msg! st (format-journal-recent jrn))))
            ((string=? cmd "test")
              (set-tui-status-text! st "TESTING...")
              (tui-render! st)
              (catch #t
                (lambda ()
                  (let*
                      ((test-cmd
                         (if (string-null? (string-trim-both arg))
                           "./scripts/test"
                           (string-trim-both arg)))
                       (res (verify-test-suite #:command test-cmd))
                       (passed? (assoc-ref res 'passed?))
                       (exit-code (assoc-ref res 'exit-code))
                       (out (or (assoc-ref res 'output) "")))
                    (if passed?
                      (tui-add-sys-msg! st
                                        (format #f
                                                "Test suite PASSED (exit ~a).\n~a"
                                                exit-code
                                                (truncate-string (string-trim-both out) 600)))
                      (tui-add-err-msg! st
                                        (format #f
                                                "Test suite FAILED (exit ~a):\n~a"
                                                exit-code
                                                (truncate-string (string-trim-both out) 600))))))
                (lambda (k . a)
                  (tui-add-err-msg! st (format #f "Test error: ~a ~s" k a))))
              (set-tui-status-text! st "READY"))
            ((string=? cmd "subagents")
              (if (string-null? (string-trim-both (or arg "")))
                (tui-open-agent-monitor-modal! st)
                (tui-add-sys-msg! st (format-mid-run-subagents-summary reg))))
            ((string=? cmd "stop")
              (if (tui-agent-running? st)
                (tui-force-stop-run! st)
                (tui-add-sys-msg! st "No agent run is currently active.")))
            ((string=? cmd "steer")
              (call-with-values
                (lambda ()
                  (parse-steer-argument arg))
                (lambda (urgent? text)
                  (if (string-null? text)
                    (tui-add-sys-msg! st
                                      "Usage: /steer [--urgent|!] <message> (queued or pushed urgently)")
                    (if urgent?
                      (tui-trigger-urgent-steering! st text)
                      (begin
                        (with-mutex (tui-steering-mutex st)
                                    (steering-push! (tui-steering-queue st) text))
                        (tui-add-msg! st
                                      "steer"
                                      (if
                                          (or (tui-agent-running? st) (agent-run-active? (tui-status-text st)))
                                        (format #f "Steering queued for the running agent: ~a" text)
                                        (format #f "Steering queued for the next agent run: ~a" text)))))))))
            ((string=? cmd "interrupt")
              (kill-active-subprocess!)
              (with-mutex (tui-steering-mutex st)
                          (steering-push! (tui-steering-queue st) 'interrupt))
              (set-tui-interrupt-requested?! st #t)
              (tui-add-msg! st
                            "steer"
                            (if
                                (or (tui-agent-running? st) (agent-run-active? (tui-status-text st)))
                              "Interrupt requested — the agent halts at the next iteration boundary."
                              "Interrupt queued — the next agent run halts before its first LLM call.")))
            ((string=? cmd "clear")
              (set-tui-messages! st '())
              (set-tui-llm-history! st (list (tui-make-system-turn st)))
              (set-tui-scroll-offset! st 0)
              (set-tui-msgsel-index! st #f))
            ((string=? cmd "dump") (handle-dump-cmd! st arg))
            ((string=? cmd "image")
              (let ((p (string-trim-both arg)))
                (cond
                  ((string-null? p) (tui-add-sys-msg! st "Usage: /image <path>"))
                  ((not (file-exists? p))
                    (tui-add-err-msg! st (format #f "Image file not found: ~a" p)))
                  (else
                    (let
                        ((info
                           (and render-ncvisual-overlay!
                                (render-ncvisual-overlay! stdscr
                                                          p
                                                          (lambda ()
                                                            (getch stdscr))))))
                      (if info
                        (tui-add-sys-msg! st
                                          (format #f
                                                  "Rendered image ~a (~a, ~ax~a cells)"
                                                  p
                                                  (or (assoc-ref info 'blitter) 'ncvisual)
                                                  (or (assoc-ref info 'cols) "?")
                                                  (or (assoc-ref info 'rows) "?")))
                        (tui-add-sys-msg! st (format-image-placeholder p))))))))
            ((string=? cmd "quit") 'exit)
            (else
              (tui-add-err-msg! st
                                (format #f "Unknown command: /~a (type /help for commands)" cmd))))))