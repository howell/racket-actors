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

;; a PID is a Thread

;; an ExitReason is one of
;;  Exception
;;  'normal
;;  'no-proc

;; Sent to the monitor when the monitor-ee exits
(struct down (pid reason) #:transparent)

;; (Hashof PID ExitReason)
(define exit-reasons
  (make-weak-hash))

;; (-> Void) -> PID
(define (spawn-thunk thnk)
  (define the-thread
    (thread
     (thunk
      (with-handlers ([(const #t) (lambda (e) (displayln e) (exit! e))])
        (thnk)
        (exit! 'normal)))))
  the-thread)

(define-syntax-rule (spawn body ...)
  (spawn-thunk (thunk body ...)))

;; PID Any -> Void
(define (send! pid v)
  ;; actually want to allow non-actors to send messages
  (cond
    [(thread-running? pid)
     (thread-send pid v)]
    [else
     (printf "tried to send ~a to no-longer running process ~a\n" v pid)]))

(define-syntax (receive stx)
  (syntax-parse stx
    [(_ [pat:expr body:expr ...] ...)
     #'(receive-message (match-lambda [pat body ...] ...))]))

;; PID -> Void
(define (monitor pid)
  (spawn-monitoring-thread (self) pid))

;; PID PID -> PID
(define (spawn-monitoring-thread behalf-pid subject-pid)
  (spawn
   (sync
    (thread-dead-evt behalf-pid)
    (handle-evt
     (thread-dead-evt subject-pid)
     (λ e (unless (thread-dead? behalf-pid)
            (define reason (hash-ref exit-reasons subject-pid 'no-proc))
            (send! behalf-pid (down subject-pid reason))))))))

;; (Any -> Void) -> Void
(define (receive-message k)
  (k (thread-receive)))

;; ExitReason -> Void
(define (exit! reason)
  (hash-set! exit-reasons (self) reason))

;; -> PID
(define (self)
  (current-thread))

(module+ test
  ;; test basic message exchange with ping/pong
  (let ()
    (define ping-pong-test-succeeded? #f)

    (define ping-actor
      (spawn
       (receive
        [`(ping ,pid)
         (displayln 'pong)
         (send! pid `(pong ,(self)))])))

    (define pong-actor
      (spawn
       (displayln 'ping)
       (send! ping-actor `(ping, (self)))
       (receive
        [`(pong ,pid)
         (set! ping-pong-test-succeeded? #t)])))

    (thread-wait pong-actor)
    (check-true ping-pong-test-succeeded?)))

(module+ test
  (let ()
    (define crash-pid #f)
    (define crash-reason #f)
    ;; test monitoring
    (define doomed-actor
      (spawn
       (receive
        ['crash
         (raise 'FIRE)])))

    (define monitoring-actor
      (spawn
       (monitor doomed-actor)
       ;; allow the monitor to get going
       (sleep .1)
       (send! doomed-actor 'crash)
       (receive
        [(down pid reason)
         (set! crash-pid pid)
         (set! crash-reason reason)])))

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
       (receive
        ['crash
         (void)])))

    (define monitoring-actor
      (spawn
       (monitor doomed-actor)
       ;; allow the monitor to get going
       (sleep .1)
       (send! doomed-actor 'crash)
       (receive
        [(down pid reason)
         (set! crash-pid pid)
         (set! crash-reason reason)])))
    
    (thread-wait monitoring-actor)
    (check-eq? crash-pid doomed-actor)
    (check-equal? crash-reason 'normal)))