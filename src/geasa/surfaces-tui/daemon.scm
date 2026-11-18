;;; (geasa surfaces-tui daemon) — Standalone UI Server Daemon & Remote Client Transport
;;;
;;; Separates the UI server (<ui-server>) from terminal rendering clients
;;; (<ui-client>) across Unix domain sockets, TCP sockets, stdio pipes, and
;;; SSH-tunneled stdio sessions.

(define-module (geasa surfaces-tui daemon)
               #:use-module
               (geasa foundation-util plugin)
               #:use-module
               (geasa agents-coordination plugin)
               #:use-module
               (geasa agents-lifecycle plugin)
               #:use-module
               (geasa surfaces-tui protocol)
               #:use-module
               (ice-9 format)
               #:use-module
               (ice-9 rdelim)
               #:use-module
               (ice-9 threads)
               #:use-module
               (srfi srfi-9)
               #:export
               (<ui-daemon> make-ui-daemon
                            ui-daemon?
                            ui-daemon-server
                            ui-daemon-transport
                            ui-daemon-host
                            ui-daemon-port
                            ui-daemon-socket-path
                            ui-daemon-status
                            ui-daemon-endpoint
                            ui-daemon-client-count
                            ui-daemon-start!
                            ui-daemon-stop!
                            ui-daemon-wait
                            <ui-remote-connection>
                            ui-remote-connection?
                            ui-remote-connection-client
                            ui-remote-connection-connected?
                            ui-remote-connection-wait
                            ui-client-connect-unix!
                            ui-client-connect-tcp!
                            ui-client-connect-ports!
                            ui-client-connect-ssh!
                            ui-remote-disconnect!
                            parse-ui-listen-spec
                            wire-ui-server-handlers!
                            run-ui-server!
                            run-remote-tui-client))

;;;
;;; Listen-spec parsing
;;;
(define (parse-ui-listen-spec spec)
        "Parse SPEC ('stdio', 'unix://PATH', or 'tcp://HOST:PORT') into an alist
with transport and endpoint fields, or #f if invalid."
        (cond
          ((not (string? spec)) #f)
          ((string=? spec "stdio") '((transport . stdio)))
          ((string-prefix? "unix://" spec)
            (let ((path (substring spec 7)))
              (and (not (string-null? path)) `((transport . unix) (socket-path . ,path)))))
          ((string-prefix? "tcp://" spec)
            (let*
                ((rest (substring spec 6)) (colon (string-rindex rest #\:)))
              (and colon
                   (let*
                       ((host (substring rest 0 colon))
                        (port-str (substring rest (+ colon 1)))
                        (port (string->number port-str)))
                     (and (not (string-null? host))
                          (integer? port)
                          (>= port 0)
                          (<= port 65535)
                          `((transport . tcp) (host . ,host) (port . ,port)))))))
          (else #f)))

;;;
;;; <ui-daemon>
;;;
(define-record-type <ui-daemon>
                    (%make-ui-daemon server
                                     transport
                                     host
                                     port
                                     socket-path
                                     in-port
                                     out-port
                                     on-client-connect
                                     on-client-disconnect
                                     listener-socket
                                     accept-thread
                                     client-ports
                                     client-threads
                                     client-seq
                                     stop?
                                     lock
                                     status)
                    ui-daemon?
                    (server ui-daemon-server)
                    (transport ui-daemon-transport)
                    (host ui-daemon-host)
                    (port ui-daemon-port set-ui-daemon-port!)
                    (socket-path ui-daemon-socket-path)
                    (in-port ui-daemon-in-port)
                    (out-port ui-daemon-out-port)
                    (on-client-connect ui-daemon-on-client-connect)
                    (on-client-disconnect ui-daemon-on-client-disconnect)
                    (listener-socket ui-daemon-listener-socket set-ui-daemon-listener-socket!)
                    (accept-thread ui-daemon-accept-thread set-ui-daemon-accept-thread!)
                    (client-ports ui-daemon-client-ports set-ui-daemon-client-ports!)
                    (client-threads ui-daemon-client-threads set-ui-daemon-client-threads!)
                    (client-seq ui-daemon-client-seq set-ui-daemon-client-seq!)
                    (stop? ui-daemon-stop? set-ui-daemon-stop?!)
                    (lock ui-daemon-lock)
                    (status ui-daemon-status set-ui-daemon-status!))

(define*
  (make-ui-daemon #:key
                  (server #f)
                  (transport 'unix)
                  (host "127.0.0.1")
                  (port 0)
                  (socket-path #f)
                  (in-port #f)
                  (out-port #f)
                  (on-client-connect #f)
                  (on-client-disconnect #f))
  "Create a standalone <ui-daemon> serving SERVER over TRANSPORT ('unix, 'tcp,
or 'stdio)."
  (%make-ui-daemon (or server (make-ui-server))
                   transport
                   host
                   port
                   socket-path
                   in-port
                   out-port
                   on-client-connect
                   on-client-disconnect
                   #f
                   #f
                   '()
                   '()
                   0
                   #f
                   (make-mutex)
                   'created))

(define (ui-daemon-endpoint daemon)
        "Return an endpoint descriptor alist for DAEMON."
        (case (ui-daemon-transport daemon)
          ((tcp)
            `((transport . tcp)
              (host . ,(ui-daemon-host daemon))
              (port . ,(ui-daemon-port daemon))))
          ((unix) `((transport . unix) (socket-path . ,(ui-daemon-socket-path daemon))))
          (else `((transport . ,(ui-daemon-transport daemon))))))

(define (ui-daemon-client-count daemon)
        "Return the number of currently connected remote clients on DAEMON."
        (length (ui-server-clients (ui-daemon-server daemon))))

(define (daemon-next-client-id! daemon)
        (with-mutex (ui-daemon-lock daemon)
                    (let ((n (+ (ui-daemon-client-seq daemon) 1)))
                      (set-ui-daemon-client-seq! daemon n)
                      (string->symbol (format #f "remote-~a" n)))))

(define (daemon-bind-tcp daemon)
        (let
            ((sock (socket PF_INET SOCK_STREAM 0))
             (host (ui-daemon-host daemon))
             (port (or (ui-daemon-port daemon) 0)))
          (catch #t
            (lambda ()
              (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
              (bind sock AF_INET (inet-pton AF_INET host) port)
              (listen sock 8)
              (set-ui-daemon-port! daemon (sockaddr:port (getsockname sock)))
              sock)
            (lambda (key . args)
              (catch #t
                (lambda ()
                  (close-port sock))
                (lambda _
                  #f))
              (apply throw key args)))))

(define (daemon-bind-unix daemon)
        (let ((path (ui-daemon-socket-path daemon)))
          (unless
              (and (string? path) (not (string-null? path)))
            (error "ui-daemon-start!: unix transport requires socket-path"))
          (when (file-exists? path) (delete-file path))
          (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
            (catch #t
              (lambda ()
                (bind sock AF_UNIX path)
                (listen sock 8)
                sock)
              (lambda (key . args)
                (catch #t
                  (lambda ()
                    (close-port sock))
                  (lambda _
                    #f))
                (apply throw key args))))))

(define (daemon-track-port! daemon port)
        (with-mutex (ui-daemon-lock daemon)
                    (set-ui-daemon-client-ports! daemon (cons port (ui-daemon-client-ports daemon)))))

(define (daemon-untrack-port! daemon port)
        (with-mutex (ui-daemon-lock daemon)
                    (set-ui-daemon-client-ports! daemon (delq port (ui-daemon-client-ports daemon)))))

(define
  (daemon-handle-client-session daemon in-port out-port close-on-exit?)
  (let*
      ((srv (ui-daemon-server daemon))
       (active-cid (daemon-next-client-id! daemon))
       (attached? #f))
    (daemon-track-port! daemon in-port)
    (unless (eq? in-port out-port) (daemon-track-port! daemon out-port))
    (ui-server-connect! srv active-cid #:transport-port out-port)
    (when
        (procedure? (ui-daemon-on-client-connect daemon))
      (catch #t
        (lambda ()
          ((ui-daemon-on-client-connect daemon) srv active-cid))
        (lambda _
          #f)))
    (let loop
      ()
      (unless (ui-daemon-stop? daemon)
        (let
            ((frame
               (catch #t
                 (lambda ()
                   (read-ui-frame in-port))
                 (lambda _
                   'eof))))
          (cond
            ((or (eof-object? frame) (eq? frame 'eof)) #f)
            ((not frame)
              (ui-server-emit! srv 'error '((reason . "invalid ui message")))
              (loop))
            (else
              (let
                  ((etype (ui-message-type frame))
                   (src (ui-message-source frame))
                   (payload (ui-message-payload frame)))
                (cond
                  ((eq? etype 'attach)
                    (unless (eq? src active-cid)
                      (ui-server-disconnect! srv active-cid)
                      (set! active-cid src)
                      (ui-server-connect! srv
                                          active-cid
                                          #:cols
                                          (or (prop-ref payload 'cols) 80)
                                          #:rows
                                          (or (prop-ref payload 'rows) 24)
                                          #:transport-port
                                          out-port))
                    (set! attached? #t)
                    (ui-server-handle-event! srv frame)
                    (loop))
                  ((eq? etype 'detach) (ui-server-handle-event! srv frame) #f)
                  (else (ui-server-handle-event! srv frame) (loop)))))))))
    (when
        (or attached? (assq active-cid (ui-server-clients srv)))
      (catch #t
        (lambda ()
          (ui-server-disconnect! srv active-cid))
        (lambda _
          #f)))
    (when
        (procedure? (ui-daemon-on-client-disconnect daemon))
      (catch #t
        (lambda ()
          ((ui-daemon-on-client-disconnect daemon) srv active-cid))
        (lambda _
          #f)))
    (daemon-untrack-port! daemon in-port)
    (unless (eq? in-port out-port) (daemon-untrack-port! daemon out-port))
    (when close-on-exit?
      (catch #t
        (lambda ()
          (close-port in-port))
        (lambda _
          #f))
      (unless (eq? in-port out-port)
        (catch #t
          (lambda ()
            (close-port out-port))
          (lambda _
            #f))))))

(define (daemon-accept-loop daemon sock)
        (let loop
          ()
          (unless (ui-daemon-stop? daemon)
            (let
                ((ready
                   (catch #t
                     (lambda ()
                       (car (select (list sock) '() '() 0 100000)))
                     (lambda _
                       '()))))
              (when
                  (and (pair? ready) (not (ui-daemon-stop? daemon)))
                (let
                    ((accepted
                       (catch #t
                         (lambda ()
                           (accept sock))
                         (lambda (key . args)
                           (unless (ui-daemon-stop? daemon)
                             (format (current-error-port)
                                     "warning: daemon accept failed: ~s ~s~%"
                                     key
                                     args))
                           #f))))
                  (when (pair? accepted)
                    (let*
                        ((client-port (car accepted))
                         (thr
                           (call-with-new-thread
                             (lambda ()
                               (daemon-handle-client-session daemon client-port client-port #t)))))
                      (with-mutex (ui-daemon-lock daemon)
                                  (set-ui-daemon-client-threads! daemon
                                                                 (cons thr (ui-daemon-client-threads daemon))))))))
              (loop)))))

(define (ui-daemon-start! daemon)
        "Start DAEMON listening on its configured transport ('unix, 'tcp, or 'stdio)."
        (set-ui-daemon-stop?! daemon #f)
        (let ((tr (ui-daemon-transport daemon)))
          (cond
            ((eq? tr 'tcp)
              (let ((sock (daemon-bind-tcp daemon)))
                (set-ui-daemon-listener-socket! daemon sock)
                (set-ui-daemon-accept-thread! daemon
                                              (call-with-new-thread
                                                (lambda ()
                                                  (daemon-accept-loop daemon sock))))))
            ((eq? tr 'unix)
              (let ((sock (daemon-bind-unix daemon)))
                (set-ui-daemon-listener-socket! daemon sock)
                (set-ui-daemon-accept-thread! daemon
                                              (call-with-new-thread
                                                (lambda ()
                                                  (daemon-accept-loop daemon sock))))))
            ((eq? tr 'stdio)
              (let
                  ((in (or (ui-daemon-in-port daemon) (current-input-port)))
                   (out (or (ui-daemon-out-port daemon) (current-output-port))))
                (set-ui-daemon-accept-thread! daemon
                                              (call-with-new-thread
                                                (lambda ()
                                                  (daemon-handle-client-session daemon in out #f))))))
            (else (error "ui-daemon-start!: unsupported transport" tr)))
          (set-ui-daemon-status! daemon 'running)
          daemon))

(define (ui-daemon-stop! daemon)
        "Stop DAEMON, close its listener and client ports, and join worker threads."
        (set-ui-daemon-stop?! daemon #t)
        (let ((sock (ui-daemon-listener-socket daemon)))
          (when (port? sock)
            (catch #t
              (lambda ()
                (close-port sock))
              (lambda _
                #f))
            (set-ui-daemon-listener-socket! daemon #f)))
        (for-each
          (lambda (p)
            (catch #t
              (lambda ()
                (close-port p))
              (lambda _
                #f)))
          (ui-daemon-client-ports daemon))
        (set-ui-daemon-client-ports! daemon '())
        (let ((ath (ui-daemon-accept-thread daemon)))
          (when (thread? ath)
            (catch #t
              (lambda ()
                (join-thread ath (+ (current-time) 2)))
              (lambda _
                #f))
            (set-ui-daemon-accept-thread! daemon #f)))
        (for-each
          (lambda (thr)
            (catch #t
              (lambda ()
                (join-thread thr (+ (current-time) 2)))
              (lambda _
                #f)))
          (ui-daemon-client-threads daemon))
        (set-ui-daemon-client-threads! daemon '())
        (when
            (and (eq? (ui-daemon-transport daemon) 'unix)
                 (string? (ui-daemon-socket-path daemon))
                 (file-exists? (ui-daemon-socket-path daemon)))
          (catch #t
            (lambda ()
              (delete-file (ui-daemon-socket-path daemon)))
            (lambda _
              #f)))
        (set-ui-daemon-status! daemon 'stopped)
        daemon)

(define (ui-daemon-wait daemon)
        "Block until DAEMON's accept/stdio thread finishes."
        (let ((ath (ui-daemon-accept-thread daemon)))
          (when (thread? ath) (join-thread ath))))

;;;
;;; <ui-remote-connection>
;;;
(define-record-type <ui-remote-connection>
                    (%make-ui-remote-connection client
                                                in-port
                                                out-port
                                                reader-thread
                                                stop?
                                                on-frame
                                                on-disconnect
                                                close-ports?
                                                pid)
                    ui-remote-connection?
                    (client ui-remote-connection-client)
                    (in-port ui-remote-connection-in-port)
                    (out-port ui-remote-connection-out-port)
                    (reader-thread ui-remote-connection-reader-thread
                                   set-ui-remote-connection-reader-thread!)
                    (stop? ui-remote-connection-stop? set-ui-remote-connection-stop?!)
                    (on-frame ui-remote-connection-on-frame)
                    (on-disconnect ui-remote-connection-on-disconnect)
                    (close-ports? ui-remote-connection-close-ports?)
                    (pid ui-remote-connection-pid))

(define (ui-remote-connection-connected? conn)
        "Return #t when CONN's underlying <ui-client> is marked connected and not stopped."
        (and (not (ui-remote-connection-stop? conn))
             (ui-client-connected? (ui-remote-connection-client conn))))

(define (start-client-reader! conn)
        (let
            ((cli (ui-remote-connection-client conn))
             (in (ui-remote-connection-in-port conn)))
          (set-ui-remote-connection-reader-thread! conn
                                                   (call-with-new-thread
                                                     (lambda ()
                                                       (let loop
                                                         ()
                                                         (unless (ui-remote-connection-stop? conn)
                                                           (let
                                                               ((frame
                                                                  (catch #t
                                                                    (lambda ()
                                                                      (read-ui-frame in))
                                                                    (lambda _
                                                                      'eof))))
                                                             (cond
                                                               ((or (eof-object? frame) (eq? frame 'eof)) #f)
                                                               ((not frame) (loop))
                                                               (else (ui-client-apply-frame! cli frame)
                                                                     (when
                                                                         (procedure? (ui-remote-connection-on-frame conn))
                                                                       (catch #t
                                                                         (lambda ()
                                                                           ((ui-remote-connection-on-frame conn) cli frame))
                                                                         (lambda _
                                                                           #f)))
                                                                     (let*
                                                                         ((bye? (eq? (ui-message-type frame) 'goodbye))
                                                                          (bye-cid (and bye? (prop-ref (ui-message-payload frame) 'client)))
                                                                          (self-bye?
                                                                            (and bye?
                                                                                 (or (not bye-cid)
                                                                                     (equal? (ensure-string bye-cid) (symbol->string (ui-client-id cli)))))))
                                                                       (unless self-bye? (loop))))))))
                                                       (when
                                                           (procedure? (ui-remote-connection-on-disconnect conn))
                                                         (catch #t
                                                           (lambda ()
                                                             ((ui-remote-connection-on-disconnect conn) cli))
                                                           (lambda _
                                                             #f))))))))

(define*
  (ui-client-connect-ports! client
                            in-port
                            out-port
                            #:key
                            (on-frame #f)
                            (on-disconnect #f)
                            (close-ports? #t)
                            (pid #f))
  "Attach CLIENT to IN-PORT/OUT-PORT, start its background frame reader, and
send an initial 'attach event."
  (ui-client-attach-port! client out-port)
  (let
      ((conn
         (%make-ui-remote-connection client
                                     in-port
                                     out-port
                                     #f
                                     #f
                                     on-frame
                                     on-disconnect
                                     close-ports?
                                     pid)))
    (start-client-reader! conn)
    (ui-client-send-event client
                          'attach
                          `((cols . ,(ui-client-cols client)) (rows . ,(ui-client-rows client))))
    conn))

(define*
  (ui-client-connect-unix! client
                           socket-path
                           #:key
                           (on-frame #f)
                           (on-disconnect #f))
  "Connect CLIENT to a <ui-daemon> listening on Unix domain socket SOCKET-PATH."
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
    (connect sock AF_UNIX socket-path)
    (ui-client-connect-ports! client
                              sock
                              sock
                              #:on-frame
                              on-frame
                              #:on-disconnect
                              on-disconnect
                              #:close-ports?
                              #t)))

(define*
  (ui-client-connect-tcp! client host port #:key (on-frame #f) (on-disconnect #f))
  "Connect CLIENT to a <ui-daemon> listening on TCP HOST:PORT."
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (connect sock AF_INET (inet-pton AF_INET host) port)
    (ui-client-connect-ports! client
                              sock
                              sock
                              #:on-frame
                              on-frame
                              #:on-disconnect
                              on-disconnect
                              #:close-ports?
                              #t)))

(define*
  (ui-client-connect-ssh! client
                          target
                          #:key
                          (ssh-bin "ssh")
                          (badb-bin "badb")
                          (on-frame #f)
                          (on-disconnect #f))
  "Spawn SSH to TARGET running `<badb-bin> tui --serve stdio` and connect CLIENT."
  (let*
      ((to-child (pipe)) (from-child (pipe)) (pid (primitive-fork)))
    (if (zero? pid)
      (begin (close-port (cdr to-child))
             (close-port (car from-child))
             (dup2 (port->fdes (car to-child)) 0)
             (dup2 (port->fdes (cdr from-child)) 1)
             (close-port (car to-child))
             (close-port (cdr from-child))
             (execlp ssh-bin ssh-bin target badb-bin "tui" "--serve" "stdio"))
      (begin (close-port (car to-child))
             (close-port (cdr from-child))
             (ui-client-connect-ports! client
                                       (car from-child)
                                       (cdr to-child)
                                       #:on-frame
                                       on-frame
                                       #:on-disconnect
                                       on-disconnect
                                       #:close-ports?
                                       #t
                                       #:pid
                                       pid)))))

(define (ui-remote-disconnect! conn)
        "Send 'detach, stop reader thread, and close underlying ports on CONN."
        (set-ui-remote-connection-stop?! conn #t)
        (let
            ((cli (ui-remote-connection-client conn))
             (in (ui-remote-connection-in-port conn))
             (out (ui-remote-connection-out-port conn)))
          (catch #t
            (lambda ()
              (ui-client-send-event cli 'detach '()))
            (lambda _
              #f))
          (when (ui-remote-connection-close-ports? conn)
            (catch #t
              (lambda ()
                (close-port in))
              (lambda _
                #f))
            (unless (eq? in out)
              (catch #t
                (lambda ()
                  (close-port out))
                (lambda _
                  #f))))
          (let
              ((thr (ui-remote-connection-reader-thread conn)))
            (when (thread? thr)
              (catch #t
                (lambda ()
                  (join-thread thr (+ (current-time) 2)))
                (lambda _
                  #f))))
          (when (integer? (ui-remote-connection-pid conn))
            (catch #t
              (lambda ()
                (waitpid (ui-remote-connection-pid conn) WNOHANG))
              (lambda _
                #f)))
          conn))

(define (ui-remote-connection-wait conn)
        "Block until CONN's reader thread terminates."
        (let
            ((thr (ui-remote-connection-reader-thread conn)))
          (when (thread? thr) (join-thread thr))))

;;;
;;; Default Server Handlers & CLI Entrypoints
;;;
(define*
  (wire-ui-server-handlers! srv
                            #:key
                            (on-submit #f)
                            (on-command #f)
                            (on-steer #f)
                            (on-key #f))
  "Register standard UI event handlers on SRV for 'attach, 'submit, 'command,
'steer, 'key, and 'resize."
  (ui-server-on-event! srv
                       'attach
                       (lambda (s msg)
                         (let*
                             ((payload (ui-message-payload msg))
                              (cols (or (prop-ref payload 'cols) (prop-ref (ui-server-state s) 'cols) 80))
                              (rows (or (prop-ref payload 'rows) (prop-ref (ui-server-state s) 'rows) 24)))
                           (ui-server-emit! s
                                            'frame
                                            `((cols . ,cols) (rows . ,rows)
                                              (status . ,(or (prop-ref (ui-server-state s) 'status) "READY"))
                                              (model . ,(or (prop-ref (ui-server-state s) 'model) "default")))))))
  (ui-server-on-event! srv
                       'submit
                       (lambda (s msg)
                         (let
                             ((text (or (prop-ref (ui-message-payload msg) 'text) "")))
                           (if (procedure? on-submit)
                             (on-submit s text msg)
                             (begin
                               (ui-server-emit! s 'transcript-append `((role . user) (text . ,text)))
                               (ui-server-emit! s
                                                'notify
                                                `((level . info) (text . ,(format #f "received: ~a" text)))))))))
  (ui-server-on-event! srv
                       'command
                       (lambda (s msg)
                         (let
                             ((cmd (or (prop-ref (ui-message-payload msg) 'command) "")))
                           (if (procedure? on-command)
                             (on-command s cmd msg)
                             (ui-server-emit! s
                                              'notify
                                              `((level . info) (text . ,(format #f "command: ~a" cmd))))))))
  (ui-server-on-event! srv
                       'steer
                       (lambda (s msg)
                         (let
                             ((text (or (prop-ref (ui-message-payload msg) 'text) "")))
                           (if (procedure? on-steer)
                             (on-steer s text msg)
                             (ui-server-emit! s
                                              'notify
                                              `((level . info) (text . ,(format #f "steer: ~a" text))))))))
  (when (procedure? on-key)
    (ui-server-on-event! srv
                         'key
                         (lambda (s msg)
                           (on-key s msg))))
  srv)

(define*
  (run-ui-server! #:key
                  (transport 'unix)
                  (host "127.0.0.1")
                  (port 0)
                  (socket-path #f)
                  (in-port #f)
                  (out-port #f)
                  (on-submit #f)
                  (on-command #f))
  "Start a standalone headless UI server daemon on TRANSPORT, route subagent
harness events into it, and block until stopped."
  (let*
      ((srv
         (make-ui-server #:initial-state '((status . "READY") (cols . 80) (rows . 24))))
       (_ (wire-ui-server-handlers! srv #:on-submit on-submit #:on-command on-command))
       (sub-id
         (harness-event-subscribe!
           (lambda (ev)
             (catch #t
               (lambda ()
                 (ui-server-route-subagent-event! srv
                                                  (harness-event-source-id ev)
                                                  (or (harness-event-target-id ev) (current-harness-id))
                                                  (harness-event-kind ev)
                                                  (harness-event-payload ev)))
               (lambda _
                 #f)))
           #:target-id
           (current-harness-id)))
       (daemon
         (make-ui-daemon #:server
                         srv
                         #:transport
                         transport
                         #:host
                         host
                         #:port
                         port
                         #:socket-path
                         socket-path
                         #:in-port
                         in-port
                         #:out-port
                         out-port)))
    (dynamic-wind
      (lambda ()
        (ui-daemon-start! daemon))
      (lambda ()
        (unless (eq? transport 'stdio)
          (format (current-error-port)
                  "badb ui-server listening on ~a~%"
                  (case transport
                    ((tcp)
                      (format #f "tcp://~a:~a" (ui-daemon-host daemon) (ui-daemon-port daemon)))
                    ((unix) (format #f "unix://~a" (ui-daemon-socket-path daemon)))
                    (else transport))))
        (ui-daemon-wait daemon))
      (lambda ()
        (catch #t
          (lambda ()
            (harness-event-unsubscribe! sub-id))
          (lambda _
            #f))
        (ui-daemon-stop! daemon)))))

(define*
  (run-remote-tui-client #:key
                         (transport 'unix)
                         (host "127.0.0.1")
                         (port 0)
                         (socket-path #f)
                         (ssh-target #f)
                         (in-port #f)
                         (out-port #f))
  "Connect a <ui-client> to a remote <ui-daemon> and run an interactive
line-oriented client loop until EOF or /quit."
  (let*
      ((cli (make-ui-client #:id 'remote-tui))
       (on-frame
         (lambda (_c frame)
           (let
               ((t (ui-message-type frame)) (p (ui-message-payload frame)))
             (case t
               ((notify)
                 (format #t
                         "[~a] ~a~%"
                         (or (prop-ref p 'level) 'info)
                         (or (prop-ref p 'text) "")))
               ((transcript-append)
                 (format #t
                         "~a: ~a~%"
                         (or (prop-ref p 'role) 'assistant)
                         (or (prop-ref p 'text) "")))
               ((subagent-event)
                 (format #t
                         "[subagent ~a ~a] ~a~%"
                         (or (prop-ref p 'harness-id) "")
                         (or (prop-ref p 'kind) "")
                         (or (prop-ref p 'detail) "")))
               ((goodbye) (format #t "[disconnected]~%"))
               (else #f))
             (force-output))))
       (conn
         (case transport
           ((unix) (ui-client-connect-unix! cli socket-path #:on-frame on-frame))
           ((tcp) (ui-client-connect-tcp! cli host port #:on-frame on-frame))
           ((ssh) (ui-client-connect-ssh! cli ssh-target #:on-frame on-frame))
           ((stdio ports)
             (ui-client-connect-ports! cli
                                       (or in-port (current-input-port))
                                       (or out-port (current-output-port))
                                       #:on-frame
                                       on-frame
                                       #:close-ports?
                                       #f))
           (else (error "run-remote-tui-client: unsupported transport" transport)))))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (let loop
          ()
          (let ((line (read-line (current-input-port))))
            (unless
                (or (eof-object? line) (string=? (string-trim-both line) "/quit"))
              (let ((trimmed (string-trim-both line)))
                (unless (string-null? trimmed)
                  (if (string-prefix? "/" trimmed)
                    (ui-client-send-event cli 'command `((command . ,trimmed)))
                    (ui-client-send-event cli 'submit `((text . ,trimmed))))))
              (loop)))))
      (lambda ()
        (ui-remote-disconnect! conn)))))