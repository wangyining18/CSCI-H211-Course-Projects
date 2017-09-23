;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Space Invader lab 10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Author: Yining Wang, Xiner Zhang  

(require 2htdp/image)
(require 2htdp/universe)

; feature 1
; adding a image of "GAME OVER" when the player is dead
(define (last-scene world)
  (place-image (text "GAME OVER" 30 "black") (/ width 2) (/ height 2)
                    (empty-scene width height)))

; feature 2
; adding the states of the game
; by pressing "s" key the game pauses and by pressing "r", "left", "right", "space" the game resumes 

#|

TO-DO LIST (throw ideas at me)
1. Pick a game (Space Invaders)
2. We're going to use Big Bang
3. Defining a world
4. Artwork (and Render)
5. Making things exist mathematically (on-tick, on-key)
6. Tying all together (Big-bang)
7. ???
8. Profit

|#
; A World is (make-world Player Shots Aliens)

; An Alien is a ListofPosn
; A Shots is a ListofPosn
; A Player is number
; A State is one of :
; -- running
; -- pause
; state = "r" -> running  state= "s" pause
(define-struct world (player shots aliens state))
(define width 600)
(define height 400)

(define one (make-world 100 (list (make-posn 100 180) (make-posn 120 180) (make-posn 170 180)) (list (make-posn 100 100)) "r"))
; -------------------------------------------------------------------
(define player-img (square 20 "outline" "blue"))
(define alien-img (circle 10 "solid" "green"))
(define shot-img (circle 5 "solid" "red"))

; render : World -> Image
; draws the world on a 600x400 scene
(define (render world)
  (render-player (world-player world)
     (render-shots (world-shots world)
       (render-aliens (world-aliens world)
(empty-scene width height)))))

; render-player : Player Image -> Image
; draws the player at a specified X coordinate on the given image
(define (render-player n scn)
(place-image player-img n 400 scn))


; render-shots : Shots Image -> Image
; draws ALL of the shots
(define (render-shots ss scn)
(cond ((empty? ss) scn)
(else (place-image shot-img
(posn-x (first ss))
(posn-y (first ss))
(render-shots (rest ss) scn)))))

; render-aliens : Aliens Image -> Image
; draws all of the aliens on the given image
(define (render-aliens ss scn)
(cond ((empty? ss) scn)
(else (place-image alien-img
(posn-x (first ss))
(posn-y (first ss))
(render-aliens (rest ss) scn)))))

; -----------------------------------------------------------------
; tick : World -> World
; Moves everything in the world
(define (tick w)
  (cond ((string=? (world-state w) "s")w)
        (else (make-world (world-player w)
                          (tick-shots (remove-shots (world-shots w) (world-aliens w)))
                          (add-alien (tick-aliens (remove-aliens (world-aliens w) (world-shots w))))
                          "r"))))

; tick-shots : Shots -> Shots
; move all of the shots up on the screen
(define (tick-shots ss)
(cond ((empty? ss) empty)
(else
(cons
(make-posn (posn-x (first ss))
(- (posn-y (first ss)) 5))
(tick-shots (rest ss))))))

; tick-aliens : Aliens -> Aliens
; Move aliens down the screen
(define (tick-aliens ss)
(cond ((empty? ss) empty)
(else (cons
(make-posn (posn-x (first ss))
(+ 1 (posn-y (first ss))))
(tick-aliens (rest ss))))))
; ------------------------------------
; near-any? : Posn ListOfPosn -> Boolean
; Is the given posn close to any of the posns in the given list of posns?
(define (near-any? p lop)
(cond ((empty? lop) false)
(else (or (near? p (first lop))
(near-any? p (rest lop))))))

; near? : Posn Posn -> Boolean
; Are the two given posns close together?
(define (near? p p2)
(< (dist p p2) 15))

; dist : Posn Posn -> Number
; Computes the distance between the given positions
(define (dist p p2)
(sqrt (+ (sqr (- (posn-x p) (posn-x p2)))
(sqr (- (posn-y p) (posn-y p2))))))

; death-happens: world -> Boolean
; check the position of the aliens
(define (death-happens world)
(death-happens-helper (world-aliens world)))

(define (death-happens-helper aliens)
(cond
((empty? aliens) false)
(else
(or (<= height (posn-y (first aliens)))
(death-happens-helper (rest aliens))))))

; -----------------------------------

; add-shots; World -> World
; Add a new shot to the world
(define (add-shots world)
(make-world (world-player world)
(cons (make-posn (world-player world) (- height 5)) (world-shots world))
(world-aliens world)
"r"))


; remove-shots : Shots Aliens -> Shots
; removes shots when they hit an alien
(define (remove-shots ss as)
(cond ((empty? ss) ss)
(else (cond ((near-any? (first ss) as) (remove-shots (rest ss) as))
(else (cons (first ss) (remove-shots (rest ss) as)))))))

; remove-aliens : Aliens Shots -> Aliens
; removes aliens when they get hit by a shot
(define (remove-aliens as ss)
(cond ((empty? as) as)
(else (cond
((near-any? (first as) ss) (remove-shots (rest as) ss))
(else (cons (first as) (remove-shots (rest as) ss)))))))

; add-alien : Aliens -> Aliens
; adds one alien, given a random choice
(define (add-alien as)
(cond ((< (random 40) 1) (cons (make-posn (random width) 0) as))
(else as)))

; function key : World Key -> World
; updates the world with the desired move

(define (key world ke)
(cond ((key=? ke "left")(make-world (- (world-player world) 5)(world-shots world) (world-aliens world) "r") )
((key=? ke "right")(make-world (+ (world-player world) 5)(world-shots world) (world-aliens world) "r"))
((key=? ke " ") (add-shots (make-world (world-player world) (world-shots world) (world-aliens world) "r" )))
((key=? ke "s") (make-world (world-player world) (world-shots world) (world-aliens world) "s" ))
((key=? ke "r") (make-world (world-player world) (world-shots world) (world-aliens world) "r"))
(else world)))


;

(define (main initial)
(big-bang initial
(on-tick tick)
(to-draw render)
(on-key key)
(stop-when death-happens last-scene)))

; feature 1
; adding a image of "GAME OVER" when the player is dead
(define (last-scene1 world)
  (place-image (text "GAME OVER" 30 "black") (/ width 2) (/ height 2)
                    (empty-scene width height)))

; feature 2
; 




(main one)