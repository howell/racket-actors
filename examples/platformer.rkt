#lang racket

(require "../actors.rkt")
(require "../drivers/timer.rkt")
(require "../../platformer-lib/platform_lib.rkt")
(require racket/gui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entities, Data

;; an Id is a Symbol, representing the identitiy of an entity in the game
;; n.b. distinct from PIDs!
(define player-id 'player)

;; an Enemy is (enemy Id PID Rect), representing the identity, controlling PID,
;; location, and shape of an enemy
(struct enemy (id pid rect) #:transparent)

(struct death () #:transparent)
(struct y-collision () #:transparent)

(define FRAMES-PER-SEC 30)
(define FRAME-PERIOD (floor (/ 1000 FRAMES-PER-SEC)))

(define GRAVITY-PER-SEC 6)
(define JUMP-V-PER-SEC -200)

(define EFFECTIVE-GRAVITY (/ GRAVITY-PER-SEC FRAMES-PER-SEC))
(define EFFECTIVE-JUMP-V (/ JUMP-V-PER-SEC FRAMES-PER-SEC))

(define TERMINAL-VELOCITY-PER-SEC 200)
(define EFFECTIVE-TERMINAL-VELOCITY
  (/ TERMINAL-VELOCITY-PER-SEC FRAMES-PER-SEC))

(define DX-PER-SEC 75)
(define EFFECTIVE-DX (/ DX-PER-SEC FRAMES-PER-SEC))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Logic

;; Entities send the game logic movement requests. Requests carry the Id of the
;; entity involved as well as a PID to inform of updates caused by the request.
;; A MoveRequest is one of:
;;   (move-x Id PID Integer)
;;   (move-y Id PID Integer PID)
;;   (player-can-jump? PID)
(struct move-x (id pid dx) #:transparent)
(struct move-y (id pid dy) #:transparent)
(struct player-can-jump? (pid) #:transparent)

;; response to player-can-jump?
(struct can-jump () #:transparent)
(struct cannot-jump () #:transparent)

;; Pieces of the environment come into being by sending (make-env Rect) to the
;; game-logic process
(struct make-env (r) #:transparent)

;; Enemies come into being by sending (make-enemy Id PID Rect) to the game-logic
;; process. The PID is informed if the player kills the enemy
(struct make-enemy (id pid r) #:transparent)

;; a GameState is a
;;   (game-state Rect (Listof Rect) Rect (Hashof Id Enemy) Posn)
;; representing the location of the player, the environment, the location
;; of the goal, the location of each enemy, and the size of the level
(struct game-state (player env goal enemies level-size) #:transparent)

;; PID PID Rect Rect Posn -> PID
(define (spawn-game-logic clock-pid render-pid player0 goal0 level-size)
  (spawn
   (send! clock-pid (tick-subscription (self)))
   (let loop ([gs (game-state player0 '() goal0 (hash) level-size)])
     (receive
      [(tick)
       (send! render-pid (render-gamestate gs))
       (loop gs)]
      [(move-x (== player-id) pid dx)
       (match (player-motion-x gs dx)
         ['level-complete
          (raise 'level-complete)]
         ['defeat
          (raise 'defeat)]
         [next-gs
          (loop next-gs)])]
      [(move-y (== player-id) pid dy)
       (match (player-motion-y gs dy pid)
         ['level-complete
          (raise 'level-complete)]
         ['defeat
          (raise 'defeat)]
         [next-gs
          (loop next-gs)])]
      [(move-x id pid dx)
       (match (enemy-motion-x gs id dx)
         ['defeat
          (raise 'defeat)]
         [next-gs
          (loop next-gs)])]
      [(move-y id pid dy)
       (match (enemy-motion-y gs id dy)
         ['defeat
          (raise 'defeat)]
         [next-gs
          (loop next-gs)])]
      [(player-can-jump? pid)
       (define on-top-of-something?
         (cdr (move-player-y (game-state-player gs)
                             1
                             (game-state-env gs))))
       (if on-top-of-something?
           (send! pid (can-jump))
           (send! pid (cannot-jump)))
       (loop gs)]
      [(make-enemy id pid r)
       (loop (add-enemy gs id pid r))]
      [(make-env r)
       (define new-env (cons r (game-state-env gs)))
       (define next-state (struct-copy game-state gs [env new-env]))
       (loop next-state)]))))

;; GameState Number return! -> (U GameState 'level-complete 'defeat)
;; move the player along the x-axis
(define (player-motion-x gs dx)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (match-define (posn x-limit y-limit) lsize)
  (define level-rect (rect (posn 0 0) x-limit y-limit))
  (define player-n (car (move-player-x player-old dx env-old)))
  (cond
    [(overlapping-rects? player-n cur-goal)
     'level-complete]
    [(not (overlapping-rects? player-n level-rect))
     'defeat]
    [(hit-enemy? enemies-old player-n)
     'defeat]
    [else
     (game-state player-n env-old cur-goal enemies-old lsize)]))

;; GameState Number PID -> (U GameState 'level-complete 'defeat)
;; move the player along the y-axis
(define (player-motion-y gs dy player-pid)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (match-define (posn x-limit y-limit) lsize)
  (define level-rect (rect (posn 0 0) x-limit y-limit))
  (match-define (cons player-n col?) (move-player-y player-old dy env-old))
  (define col-enemies
    (for/list ([e (hash-values enemies-old)]
               #:when (overlapping-rects? player-n (enemy-rect e)))
      e))
  (define enemies-new (hash-remove-enemies enemies-old col-enemies))
  (cond
    [(overlapping-rects? player-n cur-goal)
     'level-complete]
    [(not (overlapping-rects? player-n level-rect))
     'defeat]
    [(and (not (empty? col-enemies)) (negative? dy))
     ;; moved upwards into an enemy
     'defeat]
    [else
     (for ([e (in-list col-enemies)])
       (send! (enemy-pid e) (death)))
     (when col?
       (send! player-pid (y-collision)))
     (game-state player-n env-old cur-goal enemies-new lsize)]))

;; (Hashof Id Enemy) (Listof Enemy) -> (Hashof Id Enemy)
;; remove a bunch of enemies from a hash
(define (hash-remove-enemies h enemies)
  (hash-remove* h (map enemy-id enemies)))

;; (hashof Key Any) (listof Key) -> (hashof Key Any)
;; remove a bunch of keys from a hash
(define (hash-remove* h keys)
  (for/fold ([acc h])
            ([k keys])
    (hash-remove acc k)))

;; (Hashof Id Enemy) Rect -> Boolean
(define (hit-enemy? enemies-old player-n)
  (for/or ([e (in-hash-values enemies-old)])
    (overlapping-rects? player-n (enemy-rect e))))

;; GameState Id Number -> (U GameState 'defeat)
;; move an enemy along the x-axis
(define (enemy-motion-x gs id dx)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (define maybe-enemy (hash-ref enemies-old id #f))
  ;; the enemy might not be in the hash if it was recently killed
  (cond
    [maybe-enemy
     (match-define (enemy _  pid e-rect) maybe-enemy)
     (define e-rect-new (car (move-player-x e-rect dx env-old)))
     (cond
       [(overlapping-rects? player-old e-rect-new)
        'defeat]
       [else
        (define enemies-new (hash-set enemies-old id (enemy id pid e-rect-new)))
        (game-state player-old env-old cur-goal enemies-new lsize)])]
    [else gs]))

;; GameState Id Number -> (U GameState 'defeat)
(define (enemy-motion-y gs id dy)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (define maybe-enemy (hash-ref enemies-old id #f))
  ;; the enemy might not be in the hash if it was recently killed
  (cond
    [maybe-enemy
     (match-define (enemy _ pid e-rect) maybe-enemy)
     (match-define (cons e-rect-new col?) (move-player-y e-rect dy env-old))
     (define enemies-new (hash-set enemies-old id (enemy id pid e-rect-new)))
     (define player-collision? (overlapping-rects? player-old e-rect-new))
     (cond
       [(and player-collision? (positive? dy))
        ;; enemy fell on player
        'defeat]
       [player-collision?
        ;; enemy moved upward into player
        (send! pid (death))
        (define enemies-final (hash-remove enemies-new id))
        (game-state player-old env-old cur-goal enemies-final lsize)]
       [else
        (when col?
          (send! pid (y-collision)))
        (game-state player-old env-old cur-goal enemies-new lsize)])]
    [else gs]))

;; GameState Id PID Rect -> GameState
(define (add-enemy gs id pid r)
  (define old-enemies (game-state-enemies gs))
  (define new-enemies (hash-set old-enemies id (enemy id pid r)))
  (struct-copy game-state gs [enemies new-enemies]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player Avatar

;; Translate keyboard events to commands to the game logic

(define (spawn-player-avatar keyboard-driver game-logic game-clock)
  (define dx EFFECTIVE-DX)
  (define gravity EFFECTIVE-GRAVITY)
  (define jump-v EFFECTIVE-JUMP-V)
  (spawn
   (monitor game-logic)
   (send! game-clock (tick-subscription (self)))
   (send! keyboard-driver (keyboard-subscription (self)))
   (let loop ([left-down? #f]
              [right-down? #f]
              [pending-jump? #f]
              [vy 0])
     (receive
      [(tick)
       (define vx (- (if right-down? dx 0)
                     (if left-down? dx 0)))
       (send! game-logic (move-x player-id (self) vx))
       ;; if we can't jump, we lose a step of vertical motion. d'oh!
       ;; but I tried holding down the spacebar and it doesn't seem to
       ;; make much difference. phew!
       (unless pending-jump?
         (send! game-logic (move-y player-id (self) vy)))
       (define vy-new (min (+ vy gravity) EFFECTIVE-TERMINAL-VELOCITY))
       (loop left-down? right-down? pending-jump? vy-new)]
      [(y-collision)
       (when (negative? vy)
         (printf "Upwards collision!\n"))
       (loop left-down? right-down? pending-jump? 0)]
      [(can-jump)
       (loop left-down? right-down? #f jump-v)]
      [(cannot-jump)
       (loop left-down? right-down? #f vy)]
      [(key-press #\space)
       (send! game-logic (player-can-jump? (self)))
       (loop left-down? right-down? #t vy)]
      [(key-press 'left)
       (loop #t right-down? pending-jump? vy)]
      [(key-release 'left)
       (loop #f right-down? pending-jump? vy)]
      [(key-press 'right)
       (loop left-down? #t pending-jump? vy)]
      [(key-release 'right)
       (loop left-down? #f pending-jump? vy)]
      [(? key-event?)
       (loop left-down? right-down? pending-jump? vy)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering

;; a RenderGamestate message, (render-gamestate GameState), is sent to the
;; renderer to draw the gamestate to the screen.
(struct render-gamestate (gs) #:transparent)

(struct render-victory () #:transparent)

;; dc<%> -> PID
(define (spawn-renderer dc)
  (spawn
   (let loop ()
     (receive
      [(render-victory)
       (draw-victory dc)]
      [(render-gamestate gs)
       (draw-game-state dc gs)
       (loop)]))))

(define (draw-victory dc)
  (big-text dc "Victory!" "green"))

(define (big-text dc text color)
  (send dc suspend-flush)
  (send dc clear)
  (send dc set-text-mode 'solid)
  (send dc set-text-foreground color)
  (define fnt (make-object font% 100 'default))
  (send dc set-font fnt)
  (define tl-x (/ (posn-x canvas-bot-right) 6))
  (define tl-y (/ (posn-y canvas-bot-right) 4))
  (send dc draw-text text tl-x tl-y)
  (send dc resume-flush))

;; DC GameState -> Void
(define (draw-game-state dc gs)
  (match-define (game-state old-player old-env old-goal old-enemies lsize) gs)
  (render-game dc old-player old-env old-goal (hash-values old-enemies) lsize))

(define (star-points scale)
  (map (lambda (pr) (cons (* scale (car pr)) (* scale (cdr pr))))
       `((0 . 10)
         (2 . 6)
         (0 . 4)
         (3 . 4)
         (5 . 0)
         (7 . 4)
         (10 . 4)
         (8 . 6)
         (10 . 10)
         (5 . 7))))

;; drawing-context goal -> void
;; draws the goal as a 50x50 yellow star
(define (draw-goal dc g)
  (match-define (rect (posn x0 y0) _ _) g)
  (send dc set-brush "yellow" 'solid)
  (send dc set-pen "yellow" 1 'solid)
  (send dc set-smoothing 'aligned)
  (send dc draw-polygon (star-points 5) x0 y0))

;; drawing-context rect color -> void
;; draws a solid rectangle
(define (draw-rect dc r color)
  (match-define (rect (posn x0 y0) w h) r)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'solid)
  (send dc draw-rectangle x0 y0 w h))

;; drawing-context rect (listof rect) goal (listof enemy) -> void
;; draws the game
(define (draw-game dc player env gl enemies)
  (for ([r env])
    (draw-rect dc r "black"))
  (draw-goal dc gl)
  (for ([e enemies])
    (draw-rect dc (enemy-rect e) "red"))
  (draw-rect dc player "blue"))

;; num num num -> num
;; determine an offset for side-scrolling
(define (scroll-offset player canvas-size level-size)
  (define csize/2 (/ canvas-size 2))
  (cond
    ;; don't scroll when the player is close to the beginning of the level
    [(< (- player csize/2) 0) 0]
    ;; similarly, don't scroll when near the end
    [(> (+ player csize/2) level-size) (- level-size canvas-size)]
    ;; otherwise put the player at the center of the screen
    [else (- player csize/2)]))

(define (render-game canvas-dc player env gl enemies lsize)
  (match-define (posn x-size y-size) canvas-bot-right)
  (match-define (posn player-x player-y) (rect-top-left player))
  (match-define (posn x-limit y-limit) lsize)
  (define src-x (scroll-offset player-x x-size x-limit))
  (define src-y (scroll-offset player-y y-size y-limit))
  (define bitmap (make-object bitmap% x-limit y-limit))
  (define bitmap-dc (send bitmap make-dc))
  (draw-game bitmap-dc player env gl enemies)
  (send canvas-dc suspend-flush)
  (send canvas-dc clear)
  (send canvas-dc draw-bitmap-section bitmap 0 0 src-x src-y x-size y-size)
  (send canvas-dc resume-flush))

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

;; PID PositiveInteger PositiveInteger -> dc<%>
(define (make-frame key-pid width height)
  (parameterize ((current-eventspace (make-eventspace)))
    (define frame (new frame%
                       [label "My Frame"]
                       [width width]
                       [height height]))
    (define canvas
      (new game-canvas%
           [parent frame]
           [key-handler (lambda (x) (send! key-pid x))]))
    (send canvas focus)
    (send frame show #t)
    (define-values (x-max y-max) (send canvas get-client-size))
    (set! canvas-bot-right (posn x-max y-max))
    (send canvas get-dc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level Managment

;; a Level is a (level Rect (Listof Rect) Rect (PID PID -> Any) Posn)
;; representing the beginning position of the player,
;; the layout of the environment,
;; the initial position of the goal,
;; a procedure that will spawn processes for the initial enemies in the level
;;   when given the pids of the game-logic and game-clock actors
;; and the size of the level as an xy coordinate.
(struct level (player0 env0 goal make-enemies size) #:transparent)

;; (NonemptyListof Level) PID PID PID -> PID
(define (spawn-level-manager levels game-clock renderer keyboard-driver)
  (spawn
   (let loop ([levels levels])
     (match-define (cons level1 next-levels) levels)
     (let run-current ()
       (define game-logic
         (spawn-game-logic game-clock
                           renderer
                           (level-player0 level1)
                           (level-goal level1)
                           (level-size level1)))
       (define player
         (spawn-player-avatar keyboard-driver game-logic game-clock))
       (monitor game-logic)
       (load-level! level1 game-logic game-clock)
       (receive
        [(down (== game-logic) 'defeat)
         (run-current)]
        [(down (== game-logic) 'level-complete)
         (cond
           [(empty? next-levels)
            (send! renderer (render-victory))]
           [else
            (loop next-levels)])])))))

;; Level PID PID -> Void
(define (load-level! lvl game-logic game-clock)
  (match-define (level _ env _ mk-es _) lvl)
  (for ([r (in-list env)])
    (send! game-logic (make-env r)))
  (mk-es game-logic game-clock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enemies

;; Number Number Number Number (Natural Id -> Void) -> PID
(define (spawn-enemy game-logic game-clock x0 y0 w h behavior)
  (define id (gensym 'enemy))
  (spawn
   (monitor game-logic)
   (send! game-clock (tick-subscription (self)))
   (send! game-logic (make-enemy id (self) (rect (posn x0 y0) w h)))
   (let loop ([n 0])
     (receive
      [(death)
       #f]
      [(tick)
       (behavior n id)
       (loop (add1 n))]
      [(y-collision)
       (loop n)]))))

;; spawn an enemy that travels from (x0, y0) to (x0 + x-dist, y0) then back to
;; (x0, y0) at a rate of dx per clock tick
(define (make-horiz-enemy game-logic game-clock x0 y0 w h x-dist dx0)
  (define dx (/ (* dx0 24) FRAMES-PER-SEC))
  (define THRESHOLD (/ x-dist dx))
  (spawn-enemy game-logic game-clock
               x0 y0 w h
               (lambda (n id)
                 (define right? (< (modulo n (floor (* 2 THRESHOLD)))
                                   THRESHOLD))
                 (send! game-logic (move-x id (self) (if right?
                                                         dx
                                                         (- dx)))))))

;; spawn an enemy that travels from (x0, y0) to (x0, y0 + y-dist) then back to
;; (x0, y0) at a rate of dy per clock tick
(define (make-vert-enemy game-logic game-clock x0 y0 w h y-dist dy0)
  (define dy (/ (* dy0 24) FRAMES-PER-SEC))
  (define THRESHOLD (/ y-dist dy))
  (spawn-enemy game-logic game-clock
               x0 y0 w h
               (lambda (n id)
                 (define up? (< (modulo n (floor (* 2 THRESHOLD))) THRESHOLD))
                 (send! game-logic
                        (move-y id (self) (if up?
                                              dy
                                              (- dy)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Booting the game

;; spawning in the right order to resolve dependencies might be tricky...

(define (run-the-game)
  (define timer-driver (spawn-timer-driver))
  (define game-clock (spawn-game-clock FRAME-PERIOD timer-driver))
  (define keyboard-driver (spawn-keyboard-driver))
  (define dc (make-frame keyboard-driver 600 400))
  (define renderer (spawn-renderer dc))
  (spawn-level-manager ALL-LEVELS game-clock renderer keyboard-driver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level Data

(define (make-player x0 y0)
  (rect (posn x0 y0) 8 32))

(define (make-goal x0 y0)
  (rect (posn x0 y0) 50 50))

(define PLAYER0 (make-player 0 0))
(define GOAL0 (make-goal 900 150))

(define level0
  (level PLAYER0
         (list (rect (posn 0 200) 150 10)
               (rect (posn 400 200) 1000 10)
               (rect (posn 200 178) 50 10)
               (rect (posn 300 150) 50 10))
         GOAL0
         (lambda (game-logic game-clock)
           (make-horiz-enemy game-logic game-clock 0 180 20 20 130 2)
           (make-horiz-enemy game-logic game-clock 200 158 20 20 30 1)
           (make-horiz-enemy game-logic game-clock 300 130 20 20 30 1)
           (make-horiz-enemy game-logic game-clock 400 180 20 20 180 3))
         (posn 1000 400)))

(define GOAL1 (make-goal 500 150))

(define level1
  (level PLAYER0
         (list (rect (posn 0 200) 600 10))
         GOAL1
         (lambda (game-logic game-clock)
           (make-horiz-enemy game-logic game-clock 0 180 20 20 580 4)
           (make-horiz-enemy game-logic game-clock 0 140 20 20 580 8)
           (make-vert-enemy game-logic game-clock 50 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 100 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 150 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 200 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 250 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 300 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 350 125 20 20 75 4)
           (make-vert-enemy game-logic game-clock 400 125 20 20 75 4))
         (posn 600 400)))

;; int int int int int nat nat -> (list rect)
;; make a stair case starting at a given position
(define (ascending-stairs x0 y0 hdist vdist w h n)
  (for/list ([i (in-range n)])
    (define dx (* hdist i))
    (define dy (* vdist i))
    (rect (posn (+ x0 dx) (+ y0 dy)) w h)))

(define level2
  (let ([stairs (ascending-stairs (+ 50 50) (- 800 40)
                                  100 -40
                                  50 10
                                  10)]
        [birdies (lambda (game-logic game-clock)
                  (for/list ([i (in-range 5)])
                    (make-vert-enemy game-logic game-clock
                                     (+ 160 (* i 200))
                                     (- 650 (* i 80))
                                     20
                                     20
                                     120
                                     4)))])
    (level (make-player 0 750)
           (flatten (list stairs
                          (rect (posn 0 800) 50 200)))
           (make-goal 1100 950)
           birdies
           (posn 2000 1000))))

(define ALL-LEVELS (list level0 level1 level2))