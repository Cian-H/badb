;;; test/test-ui-daemon.scm — SRFI-64 unit tests for standalone UI server daemon & remote clients

(define-module (test test-ui-daemon) #:use-module
               (geasa surfaces-tui protocol)
               #:use-module
               (geasa surfaces-tui daemon)
               #:use-module
               (ice-9 threads)
               #:use-module
               (srfi srfi-64))

(test-begin "ui-daemon")

(test-group "listen-spec-parsing"
            (test-equal "stdio spec" '((transport . stdio)) (parse-ui-listen-spec "stdio"))
            (test-equal "unix spec"
                        '((transport . unix) (socket-path . "/tmp/test.sock"))
                        (parse-ui-listen-spec "unix:///tmp/test.sock"))
            (test-equal "tcp spec"
                        '((transport . tcp) (host . "0.0.0.0") (port . 7777))
                        (parse-ui-listen-spec "tcp://0.0.0.0:7777"))
            (test-equal "empty unix path rejected" #f (parse-ui-listen-spec "unix://"))
            (test-equal "out-of-range tcp port rejected"
                        #f
                        (parse-ui-listen-spec "tcp://127.0.0.1:99999")))

(test-group "stdio-pipe-daemon-and-client"
            (let*
                ((c2s (pipe)) (s2c (pipe))
                 (srv (make-ui-server))
                 (_ (wire-ui-server-handlers! srv))
                 (daemon
                   (make-ui-daemon #:server
                                   srv
                                   #:transport
                                   'stdio
                                   #:in-port
                                   (car c2s)
                                   #:out-port
                                   (cdr s2c)))
                 (cli (make-ui-client #:id 'stdio-cli #:cols 120 #:rows 40)))
              (dynamic-wind
                (lambda ()
                  (ui-daemon-start! daemon))
                (lambda ()
                  (let
                      ((conn (ui-client-connect-ports! cli (car s2c) (cdr c2s) #:close-ports? #t)))
                    (dynamic-wind
                      (lambda ()
                        #t)
                      (lambda ()
                        (ui-client-send-event cli 'submit '((text . "stdio-hello")))
                        (let wait
                          ((i 0))
                          (when
                              (and (< i 40)
                                   (or (not (ui-client-connected? cli))
                                       (null? (ui-client-notifications cli))
                                       (not (ui-client-last-frame cli))))
                            (usleep 25000)
                            (wait (+ i 1))))
                        (ui-client-send-event cli 'resize '((cols . 132) (rows . 45)))
                        (let wait-srv
                          ((i 0))
                          (when
                              (and (< i 40) (not (equal? (assoc-ref (ui-server-state srv) 'cols) 132)))
                            (usleep 25000)
                            (wait-srv (+ i 1))))
                        (test-assert "stdio client connected" (ui-client-connected? cli))
                        (test-assert
                          "stdio client received initial frame"
                          (pair? (ui-client-last-frame cli)))
                        (test-assert "stdio client received notify"
                                     (pair? (ui-client-notifications cli)))
                        (test-equal "resize updated client cols" 132 (ui-client-cols cli))
                        (test-equal "resize updated server state cols"
                                    132
                                    (assoc-ref (ui-server-state srv) 'cols)))
                      (lambda ()
                        (ui-remote-disconnect! conn)))))
                (lambda ()
                  (ui-daemon-stop! daemon)))))

(test-group
  "multi-client-tcp-broadcast-and-subagent-routing"
  (let*
      ((srv (make-ui-server))
       (_ (wire-ui-server-handlers! srv))
       (daemon
         (make-ui-daemon #:server srv #:transport 'tcp #:host "127.0.0.1" #:port 0))
       (c1 (make-ui-client #:id 'tui-a #:cols 80 #:rows 24))
       (c2 (make-ui-client #:id 'tui-b #:cols 100 #:rows 30)))
    (dynamic-wind
      (lambda ()
        (ui-daemon-start! daemon))
      (lambda ()
        (let
            ((conn1 (ui-client-connect-tcp! c1 "127.0.0.1" (ui-daemon-port daemon)))
             (conn2 (ui-client-connect-tcp! c2 "127.0.0.1" (ui-daemon-port daemon))))
          (dynamic-wind
            (lambda ()
              #t)
            (lambda ()
              (let wait-conn
                ((i 0))
                (when
                    (and (< i 40)
                         (or (not (ui-client-connected? c1)) (not (ui-client-connected? c2))))
                  (usleep 25000)
                  (wait-conn (+ i 1))))
              (test-assert "daemon tracks 2 connected clients"
                           (= (ui-daemon-client-count daemon) 2))
              (ui-server-route-subagent-event! srv
                                               "sub-42"
                                               "root"
                                               'assistant-delta
                                               "working...")
              (let wait-ev
                ((i 0))
                (when
                    (and (< i 40)
                         (or (null? (ui-client-subagent-events c1))
                             (null? (ui-client-subagent-events c2))))
                  (usleep 25000)
                  (wait-ev (+ i 1))))
              (test-assert "client 1 received subagent event"
                           (pair? (ui-client-subagent-events c1)))
              (test-assert "client 2 received subagent event"
                           (pair? (ui-client-subagent-events c2))))
            (lambda ()
              (ui-remote-disconnect! conn1)
              (ui-remote-disconnect! conn2)))))
      (lambda ()
        (ui-daemon-stop! daemon)))))

(test-end "ui-daemon")