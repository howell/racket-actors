#lang racket

(provide spawn
         receive
         monitor
         send!
         self
         (struct-out down))

(require (for-syntax syntax/parse))

(module+ test
  (require rackunit))

;; an ActorState is a (actor-state PID (Queueof Message) (Listof PID))
;; (for now a queue is a list)
(struct actor-state (pid mailbox links))

;; a PID is a Thread

;; a Message is one of
;;  (meta MetaMessage)
;;  (message Any), for application messages created with send!
(struct meta (msg) #:transparent)
(struct message (v) #:transparent)

;; a MetaMessage is one of
;;   (add-link PID)
;;   (down PID ExitReason)

;; an ExitReason is one of
;;  Exception
;;  'normal
;;  'no-proc

(struct add-link (pid) #:transparent)
(struct down (pid reason) #:transparent)

;; #f for when outside of a process
(define current-actor-state (make-parameter #f))

;; (-> Void) -> PID
(define (spawn thnk)
  (define the-thread
    (thread
     (thunk
      (define my-pid (current-thread))
      (current-actor-state (actor-state my-pid '() '()))
      (with-handlers ([(const #t) (lambda (e) (displayln e) (exit! e))])
        (thnk)
        (exit! 'normal)))))
  the-thread)

;; PID Any -> Void
(define (send! pid v)
  ;; actually want to allow non-actors to send messages
  (send-message! pid (message v)))

(define-syntax (receive stx)
  (syntax-parse stx
    [(_ [pat:expr body:expr ...] ...)
     #'(receive-message (match-lambda [pat body ...] ...))]))

;; PID -> Void
(define (monitor pid)
  (ensure-effects-available! 'monitor)
  (spawn-monitoring-thread (self) pid))

;; PID PID -> PID
(define (spawn-monitoring-thread behalf-pid subject-pid)
  (spawn
   (thunk
    (send-message! subject-pid (meta (add-link (self))))
    (sync
     (thread-dead-evt behalf-pid)
     (handle-evt (thread-dead-evt subject-pid)
                 (λ e (unless (thread-dead? behalf-pid)
                        ;; it seems like sending-a-message-on-exit could race
                        ;; with the just exiting event, so check our mailbox
                        (define m? (thread-try-receive))
                        (send-message! behalf-pid (or m? (meta (down subject-pid 'no-proc)))))))
     (handle-evt (thread-receive-evt)
                 (λ e
                   (define down-msg (thread-receive))
                   (unless (thread-dead? behalf-pid)
                        (send-message! behalf-pid down-msg))))))))

;; (Any -> Void) -> Void
(define (receive-message k)
  (ensure-effects-available! 'receive)
  (match-define (actor-state _ msgs links) (current-actor-state))
  (let loop ()
    (match (thread-receive)
      [(meta (add-link pid))
       (add-link! pid)
       (loop)]
      [(meta (and e (down pid reason)))
       (k e)]
      [(message v)
       (k v)])))

;; Symbol -> Void
(define (ensure-effects-available! loc)
  (unless (current-actor-state)
    (error loc "not available outside an actor")))

;; ExitReason -> Void
(define (exit! reason)
  (match-define (actor-state my-pid _ links) (current-actor-state))
  (for ([pid (in-list links)])
    (unless (thread-dead? pid)
      (send-message! pid (meta (down my-pid reason))))))

;; PID Message -> Void
(define (send-message! pid msg)
  ;; TODO - need to worry if destination is dead
  (cond
    [(thread-running? pid)
     (thread-send pid msg)]
    [else
     (printf "tried to send ~a to no-longer running process ~a\n" msg pid)]))


;; PID -> Void
;; private, no need to ensure effects available
(define (add-link! pid)
  (define current (current-actor-state))
  (current-actor-state (struct-copy actor-state
                                    current
                                    [links (cons pid (actor-state-links current))])))

;; -> PID
(define (self)
  (ensure-effects-available! 'self)
  (actor-state-pid (current-actor-state)))

(module+ test
  ;; test basic message exchange with ping/pong
  (let ()
    (define ping-pong-test-succeeded? #f)

    (define ping-actor
      (spawn
       (thunk
        (receive
         [`(ping ,pid)
          (displayln 'pong)
          (send! pid `(pong ,(self)))]))))

    (define pong-actor
      (spawn
       (thunk
        (displayln 'ping)
        (send! ping-actor `(ping, (self)))
        (receive
         [`(pong ,pid)
          (set! ping-pong-test-succeeded? #t)]))))

    (thread-wait pong-actor)
    (check-true ping-pong-test-succeeded?)))

(module+ test
  (let ()
    (define crash-pid #f)
    (define crash-reason #f)
    ;; test monitoring
    (define doomed-actor
      (spawn
       (thunk
        (receive
         ['crash
          (raise 'FIRE)]))))

    (define monitoring-actor
      (spawn
       (thunk
        (monitor doomed-actor)
        ;; allow the monitor to get going
        (sleep .1)
        (send! doomed-actor 'crash)
        (receive
         [(down pid reason)
          (set! crash-pid pid)
          (set! crash-reason reason)]))))

    (thread-wait monitoring-actor)
    (check-eq? crash-pid doomed-actor)
    (check-equal? crash-reason 'FIRE)))

(module+ test
  ;; test regular shutdown
  (let ()
    (define crash-pid #f)
    (define crash-reason #f)
    ;; test monitoring
    (define doomed-actor
      (spawn
       (thunk
        (receive
         ['crash
          (void)]))))

    (define monitoring-actor
      (spawn
       (thunk
        (monitor doomed-actor)
        ;; allow the monitor to get going
        (sleep .1)
        (send! doomed-actor 'crash)
        (receive
         [(down pid reason)
          (set! crash-pid pid)
          (set! crash-reason reason)]))))
    
    (thread-wait monitoring-actor)
    (check-eq? crash-pid doomed-actor)
    (check-equal? crash-reason 'normal)))