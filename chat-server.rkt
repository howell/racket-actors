#lang racket

(require "actors.rkt")
(require "tcp-driver.rkt")

(define HOST "localhost")
(define PORT 5998)

;; messages
(struct speak (user what) #:transparent)
(struct chat (what) #:transparent)

(define (chat-server tcp-driver)
  (spawn
   (thunk
    (send! tcp-driver (tcp-listen-on (self) HOST PORT))
    (receive
     [(tcp-open-listener list-pid)
      ;; is this necessary?
      (monitor list-pid)
      ;; (Hashof PID Username)
      (let loop ([members (hash)])
        (receive
         [(tcp-new-connection conn-pid)
          (printf "new connection!\n")
          (define username (gensym 'user))
          (define new-pid (spawn-user-agent username (self) conn-pid))
          (monitor new-pid)
          (for ([peer (in-hash-values members)])
            (send! new-pid (chat (format "~a arrived\n" peer))))
          (define new-members (hash-set members new-pid username))
          (announce new-members (format "~a arrived\n" username))
          (loop new-members)]
         [(speak who what)
          (announce members (format "~a: ~a\n" who what))
          (loop members)]
         [(down pid _)
          (cond
            [(hash-has-key? members pid)
             (define username (hash-ref members pid))
             (define new-members (hash-remove members pid))
             (announce new-members (format "~a left\n" username))
             (loop new-members)]
            [else
             ;; since we are also monitoring the tcp-listener actor
             (loop members)])]))]))))

(define (announce members what)
  (for ([pid (in-hash-keys members)])
    (send! pid (chat what))))

(define (spawn-user-agent username server-pid tcp-pid)
  (spawn
   (thunk
    ;; hmmmm
    (monitor server-pid)
    (let loop ()
      (send! tcp-pid (tcp-read-line (self)))
      (receive
       [(chat what)
        (send! tcp-pid (tcp-write what))
        (loop)]
       [(tcp-input v)
        (cond
          [(eof-object? v)
           ;; exit
           (void)]
          [else
           (send! server-pid (speak username v))
           (send! tcp-pid (tcp-read-line (self)))
           (loop)])])))))

(module+ main
  (define tcp-driver (spawn-tcp-driver))
  (thread-wait (chat-server tcp-driver)))