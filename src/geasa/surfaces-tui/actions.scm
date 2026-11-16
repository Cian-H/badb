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
               (ice-9 threads)
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
                                          tui-handle-command!))

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
                                                   parent-menu))))

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
                                                                           (tui-policy st)))))
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

(define (handle-resume-cmd! st _arg)
        (let
            ((items (session-resurrection-menu-items (default-sessions-dir))))
          (if (null? items)
            (tui-add-sys-msg! st
                              "No saved sessions found. A session file is written after every chat turn.")
            (tui-set-active-menu! st (make-menu "Resume Session" items)))))

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
          (tui-add-sys-msg! st
                            (format #f
                                    "Created checkpoint '~a' (~a).\nFiles recorded: ~a, Registry entries: ~a."
                                    (checkpoint-id chkpt)
                                    (checkpoint-description chkpt)
                                    (length (checkpoint-files chkpt))
                                    (length (checkpoint-registry-state chkpt))))))

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
             (getch (nref 'getch))
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
            ((or (string-null? trimmed) (string=? sub "help"))
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
                ((open-resume-menu) (handle-resume-cmd! st ""))
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
             (endwin (nref 'endwin))
             (refresh (nref 'refresh))
             (getch (nref 'getch))
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
              (tui-set-active-menu! st
                                    (make-menu "Keybindings" (keymap->menu-items (tui-active-keymap st)))))
            ((string=? cmd "todo")
              (call-with-values
                (lambda ()
                  (handle-todo-command! (tui-active-todo-list st) arg))
                (lambda (updated txt)
                  (set-tui-active-todo-list! st updated)
                  (tui-add-sys-msg! st txt))))
            ((string=? cmd "diff")
              (tui-add-sys-msg! st
                                (if format-git-diff-output
                                  (format-git-diff-output arg)
                                  "")))
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
                                               endwin
                                               #:resume-tty!
                                               refresh))
                    (lambda (ok? res)
                      (unless ok? (tui-add-err-msg! st (format #f "Editor error: ~a" res)))))
                  (tui-add-sys-msg! st "Usage: /edit <file> [line]"))))
            ((string=? cmd "guardrail") (handle-guardrail-cmd! st arg))
            ((string=? cmd "compact") (handle-compact-cmd! st arg))
            ((string=? cmd "resume") (handle-resume-cmd! st arg))
            ((string=? cmd "checkpoint") (handle-checkpoint-cmd! st arg))
            ((string=? cmd "rollback") (handle-rollback-cmd! st arg))
            ((string=? cmd "fork") (handle-fork-cmd! st arg))
            ((string=? cmd "replay") (handle-replay-cmd! st arg))
            ((string=? cmd "rules") (tui-add-sys-msg! st (format-rules-and-tree (getcwd))))
            ((string=? cmd "geis") (handle-geis-cmd! st arg hooks))
            ((string=? cmd "context")
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
                                           reg))
            ((string=? cmd "skills")
              (execute-skills-subcommand! (tui-skill-reg st) arg add-sys! add-err!))
            ((string=? cmd "memories") (execute-memories-subcommand! arg add-sys! add-err!))
            ((string=? cmd "usage")
              (unless (tui-agent-running? st) (tui-sync-context-limit! st))
              (tui-add-sys-msg! st
                                (format-usage-report (tui-compactor st)
                                                     (tui-llm-history st)
                                                     (tui-session-prompt-tokens st)
                                                     (tui-session-completion-tokens st)
                                                     (tui-session-estimated-tokens st)
                                                     (tui-session-turns st)
                                                     #:ctx-idx
                                                     (tui-ctx-idx st))))
            ((string=? cmd "models") (handle-models-cmd! st))
            ((string=? cmd "model")
              (if (string-null? arg)
                (tui-set-active-menu! st
                                      (make-menu "Model"
                                                 (build-live-or-recorded-model-menu-items (tui-client st) cfg (tui-cur-model st))))
                (begin (set-tui-cur-model! st arg)
                       (badb-config-set-model-name! cfg arg)
                       (tui-refresh-client! st)
                       (tui-try-save-config! st)
                       (tui-add-sys-msg! st (format #f "Model switched to: ~a (saved to config)" arg)))))
            ((string=? cmd "endpoint")
              (if (string-null? arg)
                (tui-set-active-menu! st
                                      (make-menu "Endpoint" (endpoint-menu-items-for-config cfg)))
                (tui-set-endpoint-url! st arg "Endpoint updated to: ~a (saved to config)")))
            ((string=? cmd "provider") (handle-provider-cmd! st arg))
            ((string=? cmd "mcp")
              (execute-mcp-subcommand! (tui-mcp-mgr st) arg add-sys! add-err!))
            ((string=? cmd "config") (handle-config-cmd! st arg))
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
            ((string=? cmd "journal") (tui-add-sys-msg! st (format-journal-recent jrn)))
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
              (tui-add-sys-msg! st (format-mid-run-subagents-summary reg)))
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