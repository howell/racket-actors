#lang racket

(require "../actors.rkt")
(require "../drivers/timer.rkt")
(require "../../platformer-lib/platform_lib.rkt")
(require racket/gui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data

;; an Id is a Symbol, representing the identitiy of an entity in the game
;; The Id of the player is 'player
(define player-id 'player)

;; TODO - wouldn't it be nice to use PIDs as Ids? The issue is that the current
;; design needs to distinguish the Id of the player...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Logic

;; Entities send the game logic movement requests. Requests carry the Id of the
;; entity making the request; requests that may illicit a response,
;; like can-jump?, also carry a PID to send the response to.
;; A MoveRequest is one of:
;;   (move-x Id Integer)
;;   (move-y Id Integer PID)
;;   (can-jump? Id PID)
(struct move-x (id dx) #:transparent)
(struct move-y (id dy) #:transparent)
(struct can-jump? (id PID) #:transparent)

;; Pieces of the environment come into being by sending (make-env Rect) to the
;; game-logic process
(struct make-env (r) #:transparent)

;; Enemies come into being by sending (make-enemy Pid Id Rect) to the game-logic
;; process
(struct make-enemy (pid id r) #:transparent)

;; a GameState is a
;;   (game-state Rect (Listof Rect) Rect (Hashof Id Rect) Posn)
;; representing the location of the player, the environment, the location
;; of the goal, the location of each enemy, and the size of the level
(struct game-state (player env goal enemies level-size) #:transparent)

(define (spawn-game-logic clock-pid render-pid player0 goal0 level-size)
  (spawn
   (send! clock-pid (tick-subscription))
   (let loop ([gs (game-state player0 '() goal0 (hash) level-size)])
     (receive
      [_ (loop gs)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player Avatar

;; Translate keyboard events to commands to the game logic

(define (spawn-player-avatar)
  (spawn #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Clock

;; a Tick is a (tick) signifying a tick of the game clock
;; a TickSubscription is a (tick-subscription PID), informing the game clock
;; to send the actor with pid PID a Tick message at each tick
(struct tick () #:transparent)
(struct tick-subscription (pid) #:transparent)

;; PostiveInteger PID -> PID
(define (spawn-game-clock frame-rate-ms timer-driver-pid)
  (spawn
   (define begin (current-inexact-milliseconds))
   (send! timer-driver-pid (set-timer (+ begin frame-rate-ms) (self) 0))
   (let loop ([subscribers (set)]
              [frame 0]
              [now begin])
     (receive
      [(timer-expired _)
       (broadcast! (in-set subscribers) (tick))
       (define next (+ now frame-rate-ms))
       (send! timer-driver-pid (set-timer next (self) (add1 frame)))
       (loop subscribers (add1 frame) next)]
      [(tick-subscription pid)
       ;; so we can stop sending messages to subscribers that exit
       (monitor pid)
       (loop (set-add subscribers pid) frame now)]
      [(down pid _)
       (loop (set-remove subscribers pid) frame now)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Driver

;; a KeyEvent is one of (key-press KeyCode) or (key-release KeyCode) signifying
;; the press and release of a key, respectively.
;; a KeyCode is as defined by the gui
(struct key-press (key) #:transparent)
(struct key-release (key) #:transparent)

;; Any -> Bool
(define (key-event? x)
  (or (key-press? x) (key-release? x)))

;; a KeyboardSubscription is a (keyboard-subscription PID), informing the
;; keyboard driver to send KeyEvent messages to the actor PID
(struct keyboard-subscription (pid) #:transparent)

;; Forward key events from the GUI to all subscribers to key events
(define (spawn-keyboard-driver)
  (spawn
   ;; subscribers : (Setof PID)
   (let loop ([subscribers (set)])
     (receive
      [(? key-event? ke)
       (broadcast! (in-set subscribers) ke)
       (loop subscribers)]
      [(keyboard-subscription pid)
       ;; if a subscriber exits stop sending them events
       (monitor pid)
       (loop (set-add subscribers pid))]
      [(down pid _)
       (loop (set-remove subscribers pid))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

;; (Sequenceof PID) Any -> Void
;; send a message to each actor in a collection
(define (broadcast! to msg)
  (for ([pid to])
    (send! pid msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gui stuff

(define game-canvas%
  (class canvas%
    (init-field key-handler)
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (define release-code (send event get-key-release-code))
      (cond
        [(release? key-code) (key-handler (key-release release-code))]
        [else (key-handler (key-press key-code))]))
    (super-new)))

(define (space? key)
  (equal? key #\space))

(define (release? key)
  (equal? key 'release))

(define (arrow? key)
  (match key
    [(or 'left 'right 'up 'down) #t]
    [_ #f]))

;; global (mutable) variable with the canvas's bottom-right posn 
(define canvas-bot-right #f)

(define (make-frame width height)
  (parameterize ((current-eventspace (make-eventspace)))
    (define frame (new frame%
                       [label "My Frame"]
                       [width width]
                       [height height]))
    (define canvas
      (new game-canvas%
           [parent frame]
           [key-handler (lambda (x) #;TODO (void))]))
    (send canvas focus)
    (send frame show #t)
    (define-values (x-max y-max) (send canvas get-client-size))
    (set! canvas-bot-right (posn x-max y-max))
    (define dc (send canvas get-dc))
    #f))