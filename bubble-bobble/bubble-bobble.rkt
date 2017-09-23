;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname stage33333) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;player is an angle
(define-struct world (player shot shottedlist))
(define-struct bullet (angle x y col state))

; A World is a (make-world Number Bullet ListOfBullet)

; A Bullet is one of:
;   -- empty
;   -- (make-bullet Number Number State String String  ListOfBullet Number)

(define width 500)
(define height 400)
(define radius 50)
(define mode "outline")
(define col "black")
(define speed 5)
(define radiusOfShot 20)
(define shot-img (circle radiusOfShot "solid" "white"))

(define background  (place-image (circle radius mode col) (/ width 2) height 
          (empty-scene width height)))

(define two (make-world 5 empty empty))

(define (render-shotting shot scn)
  (cond ((empty? shot) (place-image shot-img (/ width 2) height scn))
  (else (place-image  (circle radiusOfShot "solid" (bullet-col shot)) (bullet-x shot) (bullet-y shot) scn))))  

(define (render-player p scn)
  (place-image (line (* radius (cos p)) (* radius (sin p)) "black")  (+ (/ width 2) (/ (* radius (cos p)) 2) )   (+ height (/ (* radius (sin p)) 2))
  (place-image (circle 5 "solid" "black") ( + (* radius (cos p)) (/  width 2) ) (+ (* radius (sin p)) height ) scn)))

(define (render-shottedlist sl scn)
  (cond ((empty? sl) scn)
(else (place-image  (circle radiusOfShot "solid" (bullet-col (first sl)))
(bullet-x (first sl))
(bullet-y (first sl))
(render-shottedlist (rest sl) scn)))))
  

(define (render world) 
  (render-shottedlist (world-shottedlist world)
  (render-shotting (world-shot world)
                   (render-player (world-player world) background))))

; add-world : World -> World
; moves the current bullet until it lands then adds a new bullet to the world 

(define (add-shot world)  
  (cond ((and (not (empty? (world-shot world))) ; it is not possible for (world-shot world) to ever be empty? 
              (string=? (bullet-state (world-shot world)) "new"))
         (make-world  (world-player world)
                      (make-bullet (world-player world) ; angle 
                                   (/ width 2) ; x
                                   height ; y 
                                   (bullet-col (world-shot world)) ; color 
                                   "running" )
                      (world-shottedlist world))) ; end of clause 
        (else world)))

 
(define (key world ke)
  (cond
    ((key=? ke "left")(make-world  (- (world-player world) 0.1) (world-shot world) (world-shottedlist world)))
    ((key=? ke "right")(make-world  (+ (world-player world) 0.1) (world-shot world) (world-shottedlist world) ))
    ((key=? ke "up")(add-shot world))
    (else world)))


(define (reflect world)
  (moveBullet (make-world (world-player world) (make-bullet (- pi (bullet-angle (world-shot world)))
                                                            (bullet-x (world-shot world))
                                                            (bullet-y (world-shot world))
                                                            (bullet-col (world-shot world))
                                                            "running") (world-shottedlist world))))

(define (moveBullet world)
 (make-world
          (world-player world)
          (make-bullet (bullet-angle (world-shot world))
                       (+ (bullet-x (world-shot world)) (* speed (cos (bullet-angle (world-shot world)))))
                       (+ (bullet-y (world-shot world)) (* speed (sin (bullet-angle (world-shot world)))))
                       (bullet-col (world-shot world))
                       "running")
          (world-shottedlist world)))

; update : World -> World
; 
(define (update world)
  (make-world (world-player world)
              empty 
              (eliminate (cons (world-shot world) (world-shottedlist world)))))

(define (eliminate lob)
  (let ((b (first lob))
        (circles (rest lob)))
    (let ((result (filter (lambda (circle)
                            (not (and (near? circle b) 
                                      (same-color? circle b)))) 
                          circles)))
      (if (= (length circles) (length result))
          (cons b result)
          result))))

(define (same-color? s1 s2)
  (string=? (bullet-col s1) (bullet-col s2)))
  

(define (near-any? s slist)
  (cond ((empty? slist) false)
        (else (or (near? s (first slist))
                  (near-any? s (rest slist))))))
 
(define (near? s1 s2)
 (< (dist s1 s2) (* 2 radiusOfShot)))

(define (dist s1 s2)
(sqrt (+ (sqr (- (bullet-x s1) (bullet-x s2)))
(sqr (- (bullet-y s1) (bullet-y s2))))))


; addbullet : World -> World
; adds a bullet because the running one has landed

(define (addbullet world)
  (make-world  (world-player world)
               (make-bullet (world-player world)
                            (/ width 2)
                            height
                            (cond [(= (random 4) 0) "yellow"] [(= (random 4) 1) "red"] [(= (random 4) 2) "blue"] [else "green"])
                            "new")
               (world-shottedlist world)))

(define (tick  world)
  (cond ((empty? (world-shot world)) (addbullet world))
        (else (cond ((string=? (bullet-state (world-shot world)) "new") world) 
                    ((< (round (bullet-y (world-shot world))) radiusOfShot) 
                     (make-world (world-player world)
                                 empty
                                 (cons (world-shot world) (world-shottedlist world))))                   
                    ((near-any? (world-shot world) (world-shottedlist world)) (update world))
                    ((< (round (bullet-x (world-shot world))) radiusOfShot) (reflect world))
                    ((> (round (bullet-x (world-shot world))) (- width radiusOfShot)) (reflect world))
                    (else (moveBullet world) )))))


(define (main initial)
  (big-bang initial
            (to-draw render)
            (on-tick tick)
            (on-key key)))

(main two)


