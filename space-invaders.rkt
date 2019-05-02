;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
; #reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-kreiger) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 3)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define BLANK (rectangle WIDTH HEIGHT "outline" "WHITE"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 2))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1.5))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1.2)) ;> landed, moving right
(define I4 (make-invader 150 100 1))           ;not landed, moving right
(define I5 (make-invader 0 100 1))           ;not landed, moving right
(define I6 (make-invader 0 0 1))           ;not landed, moving right
(define I7 (make-invader 100 0 1))           ;not landed, moving right
(define I8 (make-invader 100 20 1))           ;not landed, moving right
(define I9 (make-invader 100 80 -1))           ;not landed, moving left


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;main
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game (list I4 I5) (list M1 M2) T1))
(define G5 (make-game (list I1 I4 I5) empty T1))
(define G6 (make-game (list I6 I7 I8 I9) empty T1))


;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (cons (make-invader 150 100 12) (cons (make-invader 150 HEIGHT -10) empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of invaders

(define LOM1 empty)
(define LOM2 (cons (make-missile 150 300) (cons (make-missile (invader-x I1) (+ (invader-y I1) 10)) empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-invader (first lom))
              (fn-for-lom (rest lom)))]))


;; =================
;; Functions:

;; ListOfGame -> ListOfGame
;; start the world with (main empty)
;; 
(define (main g)
  (big-bang g
    (on-tick next-game)     ; Game -> Game
    (to-draw render-game)  ; Game -> Image
    (on-key  move-tank)    ; Game KeyEvent -> Game
    (stop-when invader-landed?))) ; Game -> Boolean
;; Game -> Game
;; produces next game state

(check-expect (next-game (make-game (list (make-invader 150 100 12))
                                    (list (make-missile 150 300))
                                    (make-tank 50 1)))
              (make-game (list (make-invader (+ 150 (* INVADER-X-SPEED 12)) (+ 100 INVADER-Y-SPEED) 12))
                         (list (make-missile 150 (- 300 MISSILE-SPEED)))
                         (make-tank (+ 50 TANK-SPEED) 1))) 

; (define (next-game g) g) ; stub

(define (next-game g)
  (make-game (next-invaders (game-invaders g) (game-missiles g))
             (next-missiles (game-missiles g) (game-invaders g))
             (next-tank (game-tank g))))

;; Game -> Boolean
;; produces true when an invader is below the MTS
(check-expect (invader-landed? empty) false)
(check-expect (invader-landed? (make-game (list (make-invader 3 4 12)
                                                (make-invader 1 (+ 1 (+ HEIGHT (/ (image-height INVADER) 2))) 12)
                                                ) empty T1)) true)


; (define (invader-landed? g) false) ; stub
(define (invader-landed? g)
  (cond [(empty? g) false]
        [else
         (invaders-offscreen (game-invaders g))]))


;; ListOfInvaders -> Boolean
;; produces true if an invader is offscreen
; (define (invaders-offscreen loi) false)

(define (invaders-offscreen loi)
  (cond [(empty? loi) false]
        [else
         (if (offscreen? (first loi))
             true
             (invaders-offscreen (rest loi)))]))


;; Invader -> Boolean
;; produce true if i has fallen off the bottom of MTS
(check-expect (offscreen? (make-invader 2 (- HEIGHT 1) 12)) false)
(check-expect (offscreen? (make-invader 2    HEIGHT 12))    false)
;; make sure entire invader has cleared the screen
(check-expect (offscreen? (make-invader 2 (+ (+ HEIGHT (/ (image-height INVADER) 2)) 1) 12)) true)



(define (offscreen? i)  
  (> (invader-y i) (+ HEIGHT (/ (image-height INVADER) 2))))

;; ListOfInvader ListOfMissiles -> ListOfInvader
;; produce next list of invaders
(check-expect (next-invaders empty empty) empty)
(check-expect (next-invaders (list (make-invader 150 100 12)) empty) (list (make-invader (+ 150 (* INVADER-X-SPEED 12)) (+ 100 INVADER-Y-SPEED) 12))) 
(check-expect (next-invaders (list (make-invader 150 100 12)) (list (make-missile 150 100))) empty) 
;(define (next-invaders loi) loi); stub

(define (next-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (cond [(invaderHitByMissile? (first loi) lom) (next-invaders (rest loi) lom)]
               [else (cons (next-invader (first loi))
                           (next-invaders (rest loi) lom))])
         ]))


;; Invader ListOfMissles -> Boolean
;; returns true if missle hit an invader

;; produces true if given Invader collides with any of the Missiles in the list
(check-expect (invaderHitByMissile? I1 LOM1) false)
(check-expect (invaderHitByMissile? I1 LOM2) true)
(check-expect (invaderHitByMissile? I2 LOM2) false)

; (define (missleHitInvader? i lom) false) ;stub
(define (invaderHitByMissile? i lom)
  (cond [(empty? lom) false]
        [else
         (if (and (<= (abs (- (missile-x (first lom)) (invader-x i))) HIT-RANGE)  (<= (abs (- (missile-y (first lom)) (invader-y i))) HIT-RANGE) )
             true
             (invaderHitByMissile? i (rest lom)))]))


;; Invader -> Invader
;; produce next invader
(check-expect (next-invader (make-invader 150 100 12)) (make-invader (+ 150 (* INVADER-X-SPEED 12)) (+ 100 INVADER-Y-SPEED) 12))
(check-expect (next-invader (make-invader (- WIDTH (* INVADER-X-SPEED 12)) 100 12)) (make-invader WIDTH (+ 100 INVADER-Y-SPEED) (* -1 12)))
(check-expect (next-invader (make-invader (* INVADER-X-SPEED 12) 100 -12)) (make-invader 0 (+ 100 INVADER-Y-SPEED) (* -1 -12)))
; (define (next-invader i) i);stub
(define (next-invader invader)
  (cond [(>= (+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader))) WIDTH)
         (make-invader (+ (* INVADER-X-SPEED (invader-dx invader)) (invader-x invader)) (+ INVADER-Y-SPEED (invader-y invader)) (- (invader-dx invader)))
         ]
        [(<= (+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader))) 0)
         (make-invader (+ (* INVADER-X-SPEED (invader-dx invader)) (invader-x invader)) (+ INVADER-Y-SPEED (invader-y invader)) (* -1 (invader-dx invader)))
         ]
        [else
         (make-invader (+ (* INVADER-X-SPEED (invader-dx invader)) (invader-x invader)) (+ INVADER-Y-SPEED (invader-y invader)) (invader-dx invader))
         ]))


;; ListOfMissile ListOfInvader -> ListOfMissile
;; produce next list of missiles
(check-expect (next-missiles empty empty) empty)
(check-expect (next-missiles (list (make-missile 150 300)) (list (make-invader 0 100 12))) (list (make-missile 150 290))) 
(check-expect (next-missiles (list (make-missile 150 300)) (list (make-invader 150 300 12))) empty) 
;(define (next-missiles lom loi) lom); stub
(define (next-missiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (cond [(missileHitInvader? (first lom) loi) (next-missiles (rest lom) loi)]
            
               [else
                (cons (next-missile (first lom))
                      (next-missiles (rest lom) loi))])
         ]))

;; Missile ListOfInvaders -> Boolean
;; produces true if a missile hit an invader
(check-expect (missileHitInvader? M1  (list I1)) false)
(check-expect (missileHitInvader? M1 empty) false)
(check-expect (missileHitInvader? M2 (list I1)) true)
;(define (missileHitInvader? m loi) false) ; stub

(define (missileHitInvader? m loi)
  (cond [(empty? loi) false]
        [else
         (if (and (<= (abs (- (invader-x (first loi)) (missile-x m))) HIT-RANGE)  (<= (abs (- (invader-y (first loi)) (missile-y m))) HIT-RANGE) )
             true
             (missileHitInvader? m (rest loi)))]))

;; Missile -> Missle
;; produce next missile
(check-expect (next-missile (make-missile 150 300)) (make-missile 150 (- 300 MISSILE-SPEED)))
; (define (next-missile lom) lom); stub

(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
(check-expect (next-tank (make-tank (/ WIDTH 2) 1)) (make-tank (+ (/ WIDTH 2) (* TANK-SPEED 1)) 1)) ; tank going right
(check-expect (next-tank (make-tank (/ WIDTH 2) -1)) (make-tank (+ (/ WIDTH 2) (* TANK-SPEED -1)) -1)) ; tank going left
(check-expect (next-tank (make-tank (- WIDTH (* TANK-SPEED 1)) 1)) (make-tank WIDTH -1)) ; tank going goes left after hitting right edge
(check-expect (next-tank (make-tank (* TANK-SPEED 1) -1)) (make-tank 0 1)) ; tank going goes right after hitting right edge

; (define (next-tank t) t); stub

(define (next-tank t)
  (cond [(>= (+ (tank-x t) (* TANK-SPEED (tank-dir t))) WIDTH)
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (- (tank-dir t)))]
        [(<= (+ (tank-x t) (* TANK-SPEED (tank-dir t))) 0)
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (- (tank-dir t)))]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))
         ]
        ))



;; Game -> Image
;; renders game onto background
(check-expect (render-game empty) BACKGROUND)
(check-expect (render-game (make-game empty empty T0)) (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2) (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))
(check-expect (render-game (make-game empty empty T1)) (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2) (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))
(check-expect (render-game (make-game empty (list M1) T1))
              (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2) (place-image MISSILE (missile-x M1) (missile-y M1) (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2) (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))))
(check-expect (render-game (make-game (list I1 I2) (list M1 M2) T1))
              (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2)
                           (place-image INVADER (invader-x I1) (invader-y I1)
                                        (place-image INVADER (invader-x I2) (invader-y I2)
                                                     (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2)
                                                                  (place-image MISSILE (missile-x M1) (missile-y M1)
                                                                               (place-image MISSILE (missile-x M2) (missile-y M2)
                                                                                            (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2)
                                                                                                         (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))))))))
(define (render-game g)
  (cond [(empty? g) BACKGROUND]
        [else
         (place-image (render-invaders (game-invaders g)) (/ WIDTH 2) (/ HEIGHT 2)
                      (place-image (render-missiles (game-missiles g)) (/ WIDTH 2) (/ HEIGHT 2)
                                   (place-image TANK (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))]))

;; ListOfInvaders -> Image
;; render invaders
(check-expect (render-invaders empty) BLANK)
(check-expect (render-invaders (list I1)) (place-image INVADER (invader-x I1) (invader-y I1) BLANK))
(check-expect (render-invaders (list I1 I2)) (place-image INVADER (invader-x I1) (invader-y I1)
                                                          (place-image INVADER (invader-x I2) (invader-y I2) BLANK)))

;(define (render-invaders loi) BLANK) ; stub


(define (render-invaders loi )
  (cond [(empty? loi) BLANK]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (render-invaders (rest loi)))]))


;; ListOfMissiles -> Image
(check-expect (render-missiles empty) BLANK)
(check-expect (render-missiles (list M1)) (place-image MISSILE (missile-x M1) (missile-y M1) BLANK))
(check-expect (render-missiles (list M1 M2)) (place-image MISSILE (missile-x M1) (missile-y M1)
                                                          (place-image MISSILE (missile-x M2) (missile-y M2) BLANK)))
;(define (render-missiles g) BLANK) ; stub
(define (render-missiles lom )
  (cond [(empty? lom) BLANK]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (render-missiles (rest lom)))]))



;; Game KeyEvent -> Game
(check-expect (move-tank G1 "left")
              (make-game (game-invaders G1)
                         (game-missiles G1)
                         (make-tank (tank-x (game-tank G1)) -1)))
(check-expect (move-tank G1 "right")
              (make-game (game-invaders G1)
                         (game-missiles G1)
                         (make-tank (tank-x (game-tank G1)) 1)))

(define (move-tank g ke)
  (cond [(key=? ke "left") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [(key=? ke " ") (fire-missile g)]
        [else g]))
;(define (move-tank g key) g) ; stub

(check-expect (fire-missile G2) (make-game (game-invaders G2) (list (make-missile 50 488) (make-missile 150 300)) (make-tank (tank-x (game-tank G2)) (tank-dir (game-tank G2)))))
;; Game -> Game
(define (fire-missile g)
  (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles g)) (make-tank (tank-x (game-tank g)) (tank-dir (game-tank g)))))


