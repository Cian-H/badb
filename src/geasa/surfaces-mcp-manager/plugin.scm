;;; (geasa surfaces-mcp-manager plugin) — /mcp Server & Tool Inspector Modal

(define-module (geasa surfaces-mcp-manager plugin)
               #:use-module (geasa foundation-config plugin)
               #:use-module (geasa foundation-util plugin)
               #:use-module (geasa capabilities-mcp plugin)
               #:use-module (geasa surfaces-notify plugin)
               #:use-module (geasa surfaces-overlay plugin)
               #:use-module (geasa surfaces-widgets plugin)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:export (mcp-manager-ui-state?
                          mcp-manager-ui-scope
                          mcp-manager-ui-entries
                          mcp-manager-ui-selected-entry
                          mcp-manager-ui-status-message
                          set-mcp-manager-ui-status-message!
                          mcp-manager-ui-refresh!
                          make-mcp-manager-overlay
                          register-mcp-manager-tui!))

;;;
;;; State & Default Config Adapter
;;;
(define-record-type <mcp-manager-ui-state>
                    (%make-mcp-manager-ui-state scope
                                                workspace
                                                entries
                                                list
                                                filter-query
                                                filtered-list
                                                status-message
                                                list-servers-fn
                                                toggle-server-fn
                                                reconnect-server-fn
                                                add-server-fn
                                                remove-server-fn
                                                notify-progress-fn
                                                notify-resolve-fn)
                    mcp-manager-ui-state?
                    (scope mcp-manager-ui-scope set-mcp-manager-ui-scope!)
                    (workspace mcp-manager-ui-workspace)
                    (entries mcp-manager-ui-entries set-mcp-manager-ui-entries!)
                    (list mcp-manager-ui-list set-mcp-manager-ui-list!)
                    (filter-query mcp-manager-ui-filter-query set-mcp-manager-ui-filter-query!)
                    (filtered-list mcp-manager-ui-filtered-list set-mcp-manager-ui-filtered-list!)
                    (status-message mcp-manager-ui-status-message set-mcp-manager-ui-status-message!)
                    (list-servers-fn mcp-manager-ui-list-servers-fn)
                    (toggle-server-fn mcp-manager-ui-toggle-server-fn)
                    (reconnect-server-fn mcp-manager-ui-reconnect-server-fn)
                    (add-server-fn mcp-manager-ui-add-server-fn)
                    (remove-server-fn mcp-manager-ui-remove-server-fn)
                    (notify-progress-fn mcp-manager-ui-notify-progress-fn)
                    (notify-resolve-fn mcp-manager-ui-notify-resolve-fn))

(define (entry-name entry)
        (ensure-string (or (assq-ref entry 'name) "")))

(define (default-list-servers scope workspace)
        (let* ((path (if (eq? scope 'project)
                       (default-local-config-path workspace)
                       (default-global-config-path)))
               (cfg (catch #t
                      (lambda () (if (file-exists? path) (load-badb-config path) #f))
                      (lambda _ #f)))
               (servers (if cfg (badb-config-mcp-servers cfg) '())))
          (map (lambda (s)
                 (let* ((name (ensure-string (car s)))
                        (plist (if (list? (cdr s)) (cdr s) '()))
                        (cmd (or (assq-ref plist 'command) ""))
                        (enabled? (if (assq 'enabled? plist) (assq-ref plist 'enabled?) #t)))
                   `((name . ,name)
                     (command . ,(if (list? cmd) (string-join (map ensure-string cmd) " ") (ensure-string cmd)))
                     (transport . "stdio")
                     (enabled? . ,enabled?)
                     (status . ,(if enabled? 'stopped 'stopped))
                     (tools . ()))))
               servers)))

(define (mcp-manager-ui-effective-list ov st)
        (let ((q (if (and ov (modal-overlay? ov)) (modal-filter-query ov) "")))
          (if (equal? q (mcp-manager-ui-filter-query st))
            (mcp-manager-ui-filtered-list st)
            (let ((fl (filter-selectable-list (mcp-manager-ui-list st) q entry-name)))
              (set-mcp-manager-ui-filter-query! st q)
              (set-mcp-manager-ui-filtered-list! st fl)
              fl))))

(define (mcp-manager-ui-selected-entry st)
        (list-selected-item (or (mcp-manager-ui-filtered-list st) (mcp-manager-ui-list st))))

(define (mcp-manager-ui-refresh! st)
        (let* ((scope (mcp-manager-ui-scope st))
               (ws (mcp-manager-ui-workspace st))
               (list-fn (mcp-manager-ui-list-servers-fn st))
               (raw (catch #t
                      (lambda () (list-fn scope ws))
                      (lambda _ '())))
               (entries (sort (if (list? raw) raw '())
                              (lambda (a b) (string<? (entry-name a) (entry-name b)))))
               (old-idx (selectable-list-selected-index (mcp-manager-ui-list st)))
               (new-idx (if (null? entries) 0 (min old-idx (- (length entries) 1))))
               (sl (make-selectable-list entries #:selected-index new-idx #:max-visible 12)))
          (set-mcp-manager-ui-entries! st entries)
          (set-mcp-manager-ui-list! st sl)
          (set-mcp-manager-ui-filtered-list!
            st
            (filter-selectable-list sl (mcp-manager-ui-filter-query st) entry-name))
          st))

;;;
;;; Rendering Helpers
;;;
(define (status->badge status)
        (case status
          ((connected) (status-badge-widget 'ok "connected"))
          ((error) (status-badge-widget 'error "error"))
          (else (status-badge-widget 'muted "stopped"))))

(define (entry-tools entry)
        (or (assq-ref entry 'tools) '()))

(define (tool-field tool key default)
        (cond
          ((mcp-tool? tool)
            (case key
              ((name) (mcp-tool-name tool))
              ((description) (mcp-tool-description tool))
              ((input-schema) (mcp-tool-input-schema tool))
              (else default)))
          ((list? tool) (or (assq-ref tool key) default))
          (else default)))

(define (format-tool-schema schema)
        (cond
          ((not schema) "params: none")
          ((string? schema) schema)
          ((list? schema)
            (let* ((props (or (assq-ref schema 'properties) '()))
                   (req (or (assq-ref schema 'required) '()))
                   (req-list (if (vector? req) (vector->list req) (if (list? req) req '())))
                   (names (map (lambda (p)
                                 (let* ((k (ensure-string (car p)))
                                        (star (if (member k req-list) "*" "")))
                                   (string-append k star)))
                               props)))
              (if (null? names)
                "params: {}"
                (string-append "params: " (string-join names ", ")))))
          (else (format #f "~s" schema))))

(define (entry-row-cells entry)
        (let* ((name (entry-name entry))
               (enabled? (if (assq 'enabled? entry) (assq-ref entry 'enabled?) #t))
               (transport (ensure-string (or (assq-ref entry 'transport)
                                             (assq-ref entry 'command)
                                             "stdio")))
               (status (or (assq-ref entry 'status) 'stopped))
               (tools (entry-tools entry)))
          (list (toggle-widget enabled?)
                name
                transport
                (status->badge status)
                (format #f "~a" (length tools)))))

(define (render-mcp-body ov st _rect)
        (let* ((sl (mcp-manager-ui-effective-list ov st))
               (cols '(((header . "Active") (width . 9))
                       ((header . "Server") (width . 20))
                       ((header . "Transport") (width . 14))
                       ((header . "Status") (width . 14))
                       ((header . "Tools") (width . 6))))
               (entries (selectable-list-items sl))
               (rows (map entry-row-cells entries))
               (tbl (table-widget cols rows
                                  #:selected-index (selectable-list-selected-index sl)
                                  #:scroll-offset (selectable-list-scroll-offset sl)
                                  #:max-visible (selectable-list-max-visible sl)
                                  #:gap 1
                                  #:empty-text "No MCP servers configured in this scope."))
               (status-w (and (mcp-manager-ui-status-message st)
                              (text-widget (mcp-manager-ui-status-message st) #:role 'warn))))
          (vstack-widget (filter identity (list tbl status-w)) #:gap 1)))

(define (render-mcp-detail ov st _rect)
        (let ((entry (list-selected-item (mcp-manager-ui-effective-list ov st))))
          (if (not entry)
            (text-widget "No MCP server selected." #:dim? #t)
            (let* ((name (entry-name entry))
                   (cmd (ensure-string (or (assq-ref entry 'command) "")))
                   (err (assq-ref entry 'error))
                   (tools (entry-tools entry))
                   (hdr (text-widget (format #f "Server: ~a  (~a)" name cmd) #:bold? #t))
                   (err-w (and err (text-widget (format #f "Error: ~a" err) #:role 'error)))
                   (tool-ws (if (null? tools)
                              (list (text-widget "No tools exposed." #:dim? #t))
                              (map (lambda (t)
                                     (let ((tname (ensure-string (tool-field t 'name "tool")))
                                           (tdesc (ensure-string (tool-field t 'description "")))
                                           (tschema (format-tool-schema (tool-field t 'input-schema #f))))
                                       (text-widget (format #f "  • ~a — ~a (~a)" tname tdesc tschema)
                                                    #:wrap? #t)))
                                   tools))))
              (vstack-widget (filter identity (append (list hdr err-w) tool-ws)) #:gap 0)))))

;;;
;;; Key & Dialog Actions
;;;
(define (push-child-modal! key-ctx dlg)
        (let ((push-fn (assq-ref key-ctx 'push-modal!))
              (stack (assq-ref key-ctx 'overlay-stack)))
          (cond
            ((procedure? push-fn) (push-fn dlg) #t)
            ((overlay-stack? stack) (overlay-stack-push-modal! stack dlg) #t)
            (else #f))))

(define (handle-mcp-action ov st ch key-ctx)
        (let* ((_ (mcp-manager-ui-effective-list ov st))
               (scope (mcp-manager-ui-scope st))
               (ws (mcp-manager-ui-workspace st))
               (entry (mcp-manager-ui-selected-entry st)))
          (cond
            ((or (eqv? ch #\j) (eq? ch 'down))
              (set-mcp-manager-ui-filtered-list!
                st
                (list-move (mcp-manager-ui-effective-list ov st) 1))
              'handled)
            ((or (eqv? ch #\k) (eq? ch 'up))
              (set-mcp-manager-ui-filtered-list!
                st
                (list-move (mcp-manager-ui-effective-list ov st) -1))
              'handled)
            ((or (eqv? ch #\space) (eqv? ch #\t))
              (when entry
                (let* ((name (entry-name entry))
                       (enabled? (if (assq 'enabled? entry) (assq-ref entry 'enabled?) #t))
                       (next-enabled? (not enabled?))
                       (fn (mcp-manager-ui-toggle-server-fn st)))
                  (catch #t
                    (lambda ()
                      (when (procedure? fn)
                        (fn scope name next-enabled? ws))
                      (mcp-manager-ui-refresh! st)
                      (set-mcp-manager-ui-status-message!
                        st
                        (format #f "~a ~a" (if next-enabled? "Enabled" "Disabled") name)))
                    (lambda (k . _args)
                      (set-mcp-manager-ui-status-message! st (format #f "Toggle failed: ~a" k))))))
              'handled)
            ((eqv? ch #\r)
              (when entry
                (let* ((name (entry-name entry))
                       (nid (string-append "mcp-reconnect-" name))
                       (prog-fn (mcp-manager-ui-notify-progress-fn st))
                       (res-fn (mcp-manager-ui-notify-resolve-fn st))
                       (reconn-fn (mcp-manager-ui-reconnect-server-fn st)))
                  (when (procedure? prog-fn)
                    (prog-fn nid (format #f "Reconnecting ~a..." name) #:title "MCP" #:source 'mcp))
                  (catch #t
                    (lambda ()
                      (when (procedure? reconn-fn)
                        (reconn-fn scope name ws))
                      (mcp-manager-ui-refresh! st)
                      (set-mcp-manager-ui-status-message! st (format #f "Reconnected ~a" name))
                      (when (procedure? res-fn)
                        (res-fn nid (format #f "Connected ~a" name) #:level 'info)))
                    (lambda (k . _args)
                      (set-mcp-manager-ui-status-message! st (format #f "Reconnect failed: ~a" k))
                      (when (procedure? res-fn)
                        (res-fn nid (format #f "Reconnect failed for ~a" name) #:level 'error))))))
              'handled)
            ((eqv? ch #\a)
              (let ((dlg (make-input-dialog
                           #:title "Add MCP Server"
                           #:prompt "Server spec (name command...):"
                           #:placeholder "fs npx @modelcontextprotocol/server-filesystem ."
                           #:validator (lambda (raw)
                                         (if (string-null? (string-trim-both raw))
                                           "Server spec cannot be empty"
                                           #f))
                           #:on-submit
                           (lambda (raw)
                             (let ((spec (string-trim-both raw))
                                   (add-fn (mcp-manager-ui-add-server-fn st)))
                               (catch #t
                                 (lambda ()
                                   (when (procedure? add-fn)
                                     (add-fn scope spec ws))
                                   (mcp-manager-ui-refresh! st)
                                   (set-mcp-manager-ui-status-message! st (format #f "Added ~a" spec)))
                                 (lambda (k . _args)
                                   (set-mcp-manager-ui-status-message!
                                     st
                                     (format #f "Add failed: ~a" k)))))))))
                (push-child-modal! key-ctx dlg))
              'handled)
            ((eqv? ch #\d)
              (when entry
                (let* ((name (entry-name entry))
                       (rm-fn (mcp-manager-ui-remove-server-fn st))
                       (dlg (make-confirm-dialog
                              #:title "Remove MCP Server"
                              #:message (format #f "Remove ~a from ~a scope?" name scope)
                              #:confirm-label "Remove"
                              #:cancel-label "Cancel"
                              #:danger? #t
                              #:on-confirm
                              (lambda ()
                                (catch #t
                                  (lambda ()
                                    (when (procedure? rm-fn)
                                      (rm-fn scope name ws))
                                    (mcp-manager-ui-refresh! st)
                                    (set-mcp-manager-ui-status-message! st (format #f "Removed ~a" name)))
                                  (lambda (k . _args)
                                    (set-mcp-manager-ui-status-message!
                                      st
                                      (format #f "Remove failed: ~a" k))))))))
                  (push-child-modal! key-ctx dlg)))
              'handled)
            ((or (eqv? ch #\newline) (eqv? ch #\return) (eq? ch 'enter))
              (set-modal-show-detail?! ov (not (modal-show-detail? ov)))
              'handled)
            ((or (eqv? ch #\q) (eqv? ch #\esc) (eq? ch 'esc) (eqv? ch #\escape))
              'close)
            (else 'pass))))

;;;
;;; Modal Constructor & TUI Registration
;;;
(define %default-notify-progress! notify-progress!)
(define %default-notify-resolve! notify-resolve!)

(define* (make-mcp-manager-overlay #:key
                                   (scope 'user)
                                   (workspace (getcwd))
                                   (list-servers #f)
                                   (toggle-server! #f)
                                   (reconnect-server! #f)
                                   (add-server! #f)
                                   (remove-server! #f)
                                   (notify-progress! #f)
                                   (notify-resolve! #f))
         "Create the /mcp server & tool inspector modal <overlay>."
         (let* ((init-sl (make-selectable-list '() #:max-visible 12))
                (st (%make-mcp-manager-ui-state
                      scope
                      workspace
                      '()
                      init-sl
                      ""
                      init-sl
                      #f
                      (or list-servers default-list-servers)
                      toggle-server!
                      reconnect-server!
                      add-server!
                      remove-server!
                      (or notify-progress! %default-notify-progress!)
                      (or notify-resolve! %default-notify-resolve!)))
                (_ (mcp-manager-ui-refresh! st))
                (hints '(("j/k" . "nav")
                         ("Tab" . "scope")
                         ("/" . "filter")
                         ("Space" . "toggle")
                         ("r" . "reconnect")
                         ("a" . "add")
                         ("d" . "remove")
                         ("Enter" . "tools")
                         ("q" . "close"))))
           (make-modal-overlay
             #:title "MCP Manager"
             #:tabs '((user . "User") (project . "Project"))
             #:active-tab scope
             #:state st
             #:width 0.85
             #:height 0.8
             #:filterable? #t
             #:filter-key-fn entry-name
             #:body-renderer render-mcp-body
             #:detail-renderer render-mcp-detail
             #:show-detail? #f
             #:footer-hints hints
             #:key-bindings
             (map (lambda (k)
                    (cons k (lambda (ov s ctx) (handle-mcp-action ov s k ctx))))
                  (list #\j 'down #\k 'up #\space #\t #\r #\a #\d
                        #\newline #\return 'enter #\q 'esc #\esc #\escape))
             #:on-tab-change
             (lambda (_ov s new-tab)
               (set-mcp-manager-ui-scope! s new-tab)
               (set-mcp-manager-ui-status-message! s #f)
               (mcp-manager-ui-refresh! s)))))

(define* (register-mcp-manager-tui! #:key
                                    register-category!
                                    register-command!
                                    open-overlay!
                                    (list-servers #f)
                                    (toggle-server! #f)
                                    (reconnect-server! #f)
                                    (add-server! #f)
                                    (remove-server! #f))
         "Register the MCP category and /mcp slash command using the provided TUI callbacks."
         (when (procedure? register-category!)
           (register-category! '("MCP") "Manage MCP servers and exposed tools"))
         (when (procedure? register-command!)
           (register-command!
             "mcp"
             "Open MCP Manager modal"
             (lambda _
               (when (procedure? open-overlay!)
                 (open-overlay!
                   (make-mcp-manager-overlay
                     #:list-servers list-servers
                     #:toggle-server! toggle-server!
                     #:reconnect-server! reconnect-server!
                     #:add-server! add-server!
                     #:remove-server! remove-server!))))
             #:path '("MCP"))))
