#lang racket

(provide spawn-tcp-driver
         (struct-out tcp-listen-on)
         (struct-out tcp-open-listener)
         (struct-out tcp-connect-to)
         (struct-out tcp-new-connection)
         (struct-out tcp-write)
         (struct-out tcp-read)
         (struct-out tcp-read-line)
         (struct-out tcp-input))

(require "actors.rkt")

(require (prefix-in tcp: racket/tcp))
(require racket/async-channel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol

;; send to the driver to open a tcp connection a listen for incoming connections
(struct tcp-listen-on (pid hostname port) #:transparent)
;; Response: address of the actor that will announce new connections
(struct tcp-open-listener (pid) #:transparent)
;; open a connection to a remote host
(struct tcp-connect-to (pid hostname port) #:transparent)
;; Announce the arrival of a new connection and a pid to use for reading
;; and writing to the remote
(struct tcp-new-connection (pid) #:transparent)

(struct tcp-write (bytes) #:transparent)
(struct tcp-read (pid bytes) #:transparent)
(struct tcp-read-line (pid) #:transparent)
(struct tcp-input (v) #:transparent)

(define (spawn-tcp-driver)
  (spawn
   (let loop ()
     (receive
      [(tcp-listen-on pid host port)
       (spawn-listener pid host port)
       (loop)]
      [(tcp-connect-to pid host port)
       (define-values (in-p out-p) (tcp:tcp-connect host port))
       (spawn-client-actor pid in-p out-p)]))))

(define (spawn-listener local-pid host port)
  (spawn
   (monitor local-pid)
   (send! local-pid (tcp-open-listener (self)))
   (define listener (tcp:tcp-listen port 128 #t host))
   (listener-thread local-pid listener)
   (let loop ()
     (receive
      [(down (== local-pid) _)
       ;; TODO shutdown listener thread
       (void)]
      [_ (loop)]))))

(define (listener-thread local-pid listener)
  (thread
   (thunk
    (with-handlers ([(const #t) (lambda (e) (displayln e))])
      (let loop ()
        (printf "Listening for new connections...\n")
        (define-values (in-p out-p) (tcp:tcp-accept listener))
        (printf "Accepted new connection\n")
        ;; TODO- set buffering?
        (spawn-client-actor local-pid in-p out-p)
        (loop))))))

(define (tcp-input-thread ch in-p driver-pid)
  (thread
   (thunk
    (let loop ()
      (match (async-channel-get ch)
        ['quit (void)]
        [(tcp-read-line pid)
         (define in (read-line in-p))
         (send! pid (tcp-input in))
         (cond
           [(eof-object? in) (send! driver-pid 'quit)]
           [else (loop)])])))))

(define (spawn-client-actor local-pid in-p out-p)
  (define (shutdown!)
    (close-output-port out-p)
    (close-input-port in-p))
  (spawn
   (monitor local-pid)
   (send! local-pid (tcp-new-connection (self)))
   (define control-ch (make-async-channel))
   (tcp-input-thread control-ch in-p (self))
   (let loop ()
     (receive
      [(tcp-write bs)
       (write-string bs out-p)
       (flush-output out-p)
       (loop)]
      [(and instr (tcp-read-line resp-pid))
       (async-channel-put control-ch instr)
       (loop)]
      ['quit
       (shutdown!)]
      [(down _ _)
       (shutdown!)]))))