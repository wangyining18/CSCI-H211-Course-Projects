;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; a World is a (make-world Snake Food)
(define-struct world (snake food))

; a Snake is a (make-snake ListofSegama Dir)
(define-struct snake (segs dir))

; CONSTANTS
(define SIZE 10)     
(define TICK 0.2)  
(define ROWS 30)     
(define COLS 30)     
(define SNAKE-IMG (circle 5 "solid" "red"))
(define FOOD-IMAGE (circle 5 "solid" "green"))
(define BACKGROUND (empty-scene 300 300))
(define INITIAL-WORLD
  (make-world
   (make-snake (list (make-posn 2 5) (make-posn 1 2) (make-posn 3 5)) 'right)
   (make-posn 15 15)))

; RENDERING
; render-world: World -> Image
; draw one frame of the game world

(define (render-world world)
  (snakeImage (world-snake world)
             (foodImage (world-food world)
                       BACKGROUND)))

; place-image-at-cell: Image Number Number Image -> Image
; draw img1 centered at given grid coords on img2


(define (place-image-at-cell img1 x y img2)
  (place-image img1
               (* SIZE (+ x 0.5))               
               (* SIZE (- ROWS (+ y 0.5))) 
               img2))

; snakeImage: Snake Image -> Image
; draw the snake

(define (snakeImage snake img)
  (segsImage (snake-segs snake) img))

; segsImage: LOS Image -> Image
; draw all segments

(define (segsImage los img)
  (cond [(empty? los) img]
        [else (place-image-at-cell
               SNAKE-IMG
               (posn-x (first los)) (posn-y (first los))
               (segsImage (rest los) img))]))

; foodImage: Food Image -> Image
; draw the food

(define (foodImage food img)
  (place-image-at-cell FOOD-IMAGE (posn-x food) (posn-y food) img))

; snake-grow: Snake -> Snake
; grow snake by adding on a new head in current direction
;(define segs2 (list (make-posn 2 5) (make-posn 3 5)))
;(define snake2 (make-snake segs2 'up))

(define (snake-grow snake)
  (make-snake (cons (new-seg (first (snake-segs snake)) (snake-dir snake))
                    (snake-segs snake))
              (snake-dir snake)))

; new-seg: Segment Dir -> Segment
; make a new segment by ofsetting an existing one


(define (new-seg seg dir)
   (cond [(symbol=? dir 'up) (make-posn (posn-x seg) (+ 1 (posn-y seg)))]
         [(symbol=? dir 'down) (make-posn (posn-x seg) (- (posn-y seg) 1))]
         [(symbol=? dir 'left) (make-posn (- (posn-x seg) 1) (posn-y seg))]
         [else (make-posn (+ 1 (posn-x seg)) (posn-y seg))]))
 
; move-snake: Snake -> Snake
; move snake head in current direction and make all
; remaining segments follow along.  Actually implemented
; by making a new head and deleting the old tail.

(define (move-snake snake)
  (make-snake (cons (new-seg (first (snake-segs snake)) (snake-dir snake))
                    (nuke-last (snake-segs snake)))
              (snake-dir snake)))

; error! (check-expect (nuke-last empty) ???)
(define (nuke-last los)
  (cond [(empty? (rest los)) empty]
        [else (cons (first los) (nuke-last (rest los)))]))


; eating?: World -> Boolean
; is the snake head on the food

(define (eating? w)
  (posn=? (first (snake-segs (world-snake w))) (world-food w)))

; posn=?: Posn Posn -> Boolean
; are the coordinates the same?

(define (posn=? a b)
  (and (= (posn-x a) (posn-x b)) (= (posn-y a) (posn-y b))))

; is-self?: World -> Boolean
; is the snake's head is eating with itself?

(define (is-self? w)
  (is-seg? (first (snake-segs (world-snake w))) (rest (snake-segs (world-snake w)))))

; is-seg?: Seg LOS -> Boolean
; is the snake's head is eating the segma?

(define (is-seg? s los)
  (cond [(empty? los) false]
        [else (or (posn=? s (first los)) (is-seg? s (rest los)))]))

; is-world?: World -> Boolean
; is the snake's head is eating border of the grid?

(define (is-world? w)
  (not (in-bounds? (first (snake-segs (world-snake w))))))

; in-bounds? Posn -> Boolean
; are the given grid cell coords in bounds?

(define (in-bounds? p)
  (and (>= (posn-x p) 0) (< (posn-x p) COLS)
       (>= (posn-y p) 0) (< (posn-y p) ROWS)))

; next-world: World -> World
; compute the new state of the game after a clock tick
(define (next-world w)
  (cond [(is-world? w) INITIAL-WORLD]
        [(is-self? w) INITIAL-WORLD]
        [(eating? w) (make-world (snake-grow (world-snake w))
                                 (make-posn (random COLS)
                                            (random ROWS)))]
        [else (make-world (move-snake (world-snake w))
                          (world-food w))]))

; key-handling: World Key -> World
; changes the direction of the head of the snake when you press the arrow keys.
(define (key-handling w k)
  (cond [(or (key=? k "up")
             (key=? k "down")
             (key=? k "left")
             (key=? k "right"))
         (make-world (make-snake (snake-segs (world-snake w))
                                 (string->symbol k))
                     (world-food w))]
        [else w]))

(big-bang INITIAL-WORLD
          (on-draw render-world)
          (on-tick next-world TICK)
          (on-key key-handling))