#lang racket

(provide spawn-timer-driver
         (struct-out set-timer)
         (struct-out timer-expired))

(require "../actors.rkt")

(module+ test
  (require rackunit))

;; a SetTimer message is (set-timer Ms PID Tag) where the Ms is an absolute time
;; at which the timer driver will send a TimerExpired message containing the
;; given Tag to PID.
(struct set-timer (ms pid tag) #:transparent)

;; a TimerExpired message is (timer-expired Tag), sent by (an agent of) the
;; driver to inform of a timer firing; the Tag is the same Tag from the SetTimer
;; message that created the timer
(struct timer-expired (tag) #:transparent)

;; a Ms is a natural number, representing an absolute time in milliseconds
;; a Tag is Any

(define (spawn-timer-driver)
  (spawn
   (let loop ()
     (receive
      [(set-timer ms pid tag)
       (spawn-timer ms pid tag)
       (loop)]))))

(define (spawn-timer ms pid tag)
  (thread
   (thunk
    (sync (alarm-evt ms))
    (send! pid (timer-expired tag)))))

(module+ test
  (define driver (spawn-timer-driver))
  (define begin-time (current-inexact-milliseconds))
  (define Δ 500)
  (define ϵ 10)
  (define observation #f)
  (define client
    (spawn
     (send! driver (set-timer (+ begin-time Δ) (self) 'abcd))
     (receive
      [(timer-expired 'abcd)
       (set! observation (current-inexact-milliseconds))])))
  (thread-wait client)
  (kill-thread driver)
  (displayln observation)
  (check-= observation (+ begin-time Δ) ϵ))