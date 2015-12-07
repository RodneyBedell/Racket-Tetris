(require 2htdp/universe)
(require 2htdp/image)


;; PROBLEM 4 - TETRIS

;; Data Definitions
 
;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))
 
;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))
 
;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.
 
;; A World is a (make-world Tetra BSet)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))
;; a Pixel is constant number
; represents width and height to chop up the board into a grid
; 10x10
(define PIXEL 10)
(check-expect PIXEL 10)
;; a HALF is a constant number which represents half of the boards width
(define HALF
  (* PIXEL 5))
(check-expect HALF 50)
;; a BOARD is a 10 PIXEL by 20 PIXEL empty scene
(define BOARD
  (empty-scene (+ PIXEL (* PIXEL 10)) (+ PIXEL (* PIXEL 20))))
(check-expect BOARD (empty-scene 110 210))

;Rotations are difficult; here’s how you do it. Assuming the above data definitions,
;this code will perform a counterclockwise rotation of a block around a given point:

;; block-rotate-ccw : Posn Block -> Block
;; Rotate the block 90 counterclockwise around the posn.
(define (block-rotate-ccw c b)
  (make-block (+ (posn-x c)
                 (- (posn-y c)
                    (block-y b)))
              (+ (posn-y c)
                 (- (block-x b)
                    (posn-x c)))
              (block-color b)))

;; 7 types of tetras
;; each has 4 blocks
;; these tetras are at their INITIAL location only good
;; when they are first placed on the BOARD
;; a tetra is a make-tetra: posn, BSet

;; O is a tetra shaped like a square
;; green
(define O
  (make-tetra (make-posn HALF 0)
              (list
              (make-block HALF 0 'green)
              (make-block (- HALF PIXEL) 0 'green)
              (make-block HALF PIXEL 'green)
              (make-block (- HALF PIXEL) PIXEL 'green))))
(check-expect O (make-tetra (make-posn 50 0)
                            (list
                             (make-block 50 0 'green)
                             (make-block 40 0 'green)
                             (make-block 50 10 'green)
                             (make-block 40 10 'green))))

;; I is a tetra shaped like a line
;; horizontal to start
;; blue
(define I
  (make-tetra (make-posn HALF 0)
              (list
              (make-block HALF 0 'blue)
              (make-block (- HALF PIXEL) 0 'blue)
              (make-block (- HALF (* PIXEL 2)) 0 'blue)
              (make-block (+ HALF PIXEL) 0 'blue))))
(check-expect I (make-tetra (make-posn 50 0)
                            (list
                             (make-block 50 0 'blue)
                             (make-block 40 0 'blue)
                             (make-block 30 0 'blue)
                             (make-block 60 0 'blue))))
;; L is a tetra shaped like:   X    
;;                           XXX
;; purple
(define L
  (make-tetra (make-posn HALF 0)
              (list
               (make-block (+ HALF PIXEL) 0 'purple)
               (make-block (- HALF PIXEL) PIXEL 'purple)
               (make-block HALF PIXEL 'purple)
               (make-block (+ HALF PIXEL) PIXEL 'purple))))
(check-expect L (make-tetra(make-posn 50 0)
                           (list
                            (make-block 60 0 'purple)
                            (make-block 40 10 'purple)
                            (make-block 50 10 'purple)
                            (make-block 60 10 'purple))))
;; J is a tetra shaped like : X
;;                            XXX
;; cyan
(define J
  (make-tetra (make-posn HALF 0)
              (list
               (make-block (- HALF PIXEL) 0 'cyan)
               (make-block (- HALF PIXEL) PIXEL 'cyan)
               (make-block HALF PIXEL 'cyan)
               (make-block (+ HALF PIXEL) PIXEL 'cyan))))
(check-expect J (make-tetra(make-posn 50 0)
                           (list
                            (make-block 40 0 'cyan)
                            (make-block 40 10 'cyan)
                            (make-block 50 10 'cyan)
                            (make-block 60 10 'cyan))))
;; T is a tetra shaped like  X
;;                          XXX
;; orange
(define T
  (make-tetra (make-posn HALF 0)
              (list
               (make-block HALF 0 'orange)
               (make-block (- HALF PIXEL) PIXEL 'orange)
               (make-block HALF PIXEL 'orange)
               (make-block (+ HALF PIXEL) PIXEL 'orange))))
(check-expect T (make-tetra(make-posn 50 0)
                           (list
                            (make-block 50 0 'orange)
                            (make-block 40 10 'orange)
                            (make-block 50 10 'orange)
                            (make-block 60 10 'orange))))
;; z is a tetra shaped like: XX
;;                            XX
;; pink
(define Z
  (make-tetra (make-posn HALF 0)
              (list
               (make-block (- HALF PIXEL) 0 'pink)
               (make-block HALF 0 'pink)
               (make-block HALF PIXEL 'pink)
               (make-block (+ HALF PIXEL) PIXEL 'pink))))
(check-expect Z (make-tetra(make-posn 50 0)
                           (list
                            (make-block 40 0 'pink)
                            (make-block 50 0 'pink)
                            (make-block 50 10 'pink)
                            (make-block 60 10 'pink))))
;; S is a tetra shaped like:  XX
;;                           XX
;; red
(define S
  (make-tetra (make-posn HALF 0)
              (list
               (make-block HALF 0 'red)
               (make-block (+ HALF PIXEL) 0 'red)
               (make-block (- HALF PIXEL) PIXEL 'red)
               (make-block HALF PIXEL 'red))))
(check-expect S (make-tetra(make-posn 50 0)
                           (list
                            (make-block 50 0 'red)
                            (make-block 60 0 'red)
                            (make-block 40 10 'red)
                            (make-block 50 10 'red))))
;;INITIAL is a world
;; empty pile
;; O tetra to start with
(define INITIAL
  (make-world O empty))
(check-expect INITIAL (make-world
               (make-tetra
               (make-posn 50 0)
               (list
                (make-block 50 0 'green)
                (make-block 40 0 'green)
                (make-block 50 10 'green)
                (make-block 40 10 'green)))
                empty))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; key-handler: world keyevent -> world
;; given either left right a or s, determine
;; either a left or translation
;; or a left or right rotation
(define (key-handler w key)
  (local
    ; checker-left: bset -> boolean
    [(define (checker-left bset)
       (cond
         [(empty? bset) false]
         [(> (/ PIXEL 2) (- (block-x (first bset)) PIXEL)) true] 
         [else (checker-left (rest bset))]))
     ; checker-right: bset -> boolean
     (define (checker-right bset)
       (cond
         [(empty? bset) false]
         [(< (* PIXEL 10) (+ (block-x (first bset)) PIXEL)) true]
         [else (checker-right (rest bset))]))]
  (cond [(key=? "left" key) (cond
                              [(checker-left (tetra-blocks (world-tetra w))) w]
                              [else
                                (make-world (tetra-move-left (world-tetra w))
                                          (world-pile w))])]
        [(key=? "right" key) (cond
                               [(checker-right (tetra-blocks (world-tetra w))) w]
                               [else
                                (make-world (tetra-move-right (world-tetra w))
                                          (world-pile w))])]
        [(key=? "up" key) (make-world (make-tetra 
                                      (make-posn (posn-x (tetra-center (world-tetra w)))
                                                 (posn-y (tetra-center (world-tetra w))))
                                      (rotate-bset (tetra-center(world-tetra w))
                                       (rotate-bset (tetra-center(world-tetra w))
                                        (rotate-bset (tetra-center(world-tetra w)) 
                                                    (tetra-blocks(world-tetra w))))))
                                     (world-pile w))]
        [(key=? "down" key) (make-world (make-tetra
                                      (make-posn (posn-x (tetra-center (world-tetra w)))
                                                 (posn-y (tetra-center (world-tetra w))))
                                      (rotate-bset (tetra-center(world-tetra w)) 
                                                   (tetra-blocks(world-tetra w))))
                                     (world-pile w))]
        [(key=? "\r" key) INITIAL]
        [else w]))) 
(check-expect (key-handler INITIAL "s") (make-world
                                        (make-tetra
                                         (make-posn 50 0)
                                         (list
                                         (make-block 50 0 'green)
                                         (make-block 50 10 'green)
                                         (make-block 60 0 'green)
                                         (make-block 60 10 'green)))
                                        empty))
(check-expect (key-handler INITIAL "a") (make-world
                                        (make-tetra
                                         (make-posn 50 0)
                                         (list
                                         (make-block 50 0 'green)
                                         (make-block 50 -10 'green)
                                         (make-block 40 0 'green)
                                         (make-block 40 -10 'green)))
                                        empty))
(check-expect (key-handler INITIAL "left") (make-world
                                            (make-tetra
                                             (make-posn 40 0) 
                                             (list
                                              (make-block 40 0 'green)
                                              (make-block 30 0 'green)
                                              (make-block 40 10 'green)
                                              (make-block 30 10 'green)))
                                             empty))
(check-expect (key-handler INITIAL "right") (make-world
                                            (make-tetra
                                             (make-posn 60 0)
                                             (list
                                              (make-block 60 0 'green)
                                              (make-block 50 0 'green)
                                              (make-block 60 10 'green)
                                              (make-block 50 10 'green)))
                                             empty))
(check-expect (key-handler INITIAL "\r") INITIAL)
;; tetra-move-left: tetra -> tetra
;; makes the center posn move 1 left on BOARD
;; by subtracting 1 from its posn-x
;; moves each block left one space on the BOARD
;; by subtracting one from each blocks x
(define(tetra-move-left tetra)
  (local
    [(define (move block)
       (make-block
          (- (block-x block) PIXEL)
          (block-y block)
          (block-color block)))]
  (make-tetra
   (make-posn (- (posn-x (tetra-center tetra)) PIXEL)
              (posn-y (tetra-center tetra)))
   (list (move (first (tetra-blocks tetra)))
         (move (second (tetra-blocks tetra)))
         (move (third (tetra-blocks tetra)))
         (move (fourth (tetra-blocks tetra)))))))
;;chekcing with L tetra   
(check-expect (tetra-move-left L)
              (make-tetra(make-posn 40 0)
                           (list
                            (make-block 50 0 'purple)
                            (make-block 30 10 'purple)
                            (make-block 40 10 'purple)
                            (make-block 50 10 'purple))))
;; tetra-move-right: tetra -> tetra
;; makes the center posn move 1 left on BOARD
;; by adding 1 PIXEL from its posn-x
;; moves each block left one space on the BOARD
;; by adding one PIXEL from each blocks x
(define(tetra-move-right tetra)
  (local
    [(define (move block)
       (make-block
          (+ (block-x block) PIXEL)
          (block-y block)
          (block-color block)))]
  (make-tetra
   (make-posn (+ (posn-x (tetra-center tetra)) PIXEL)
              (posn-y (tetra-center tetra)))
   (list (move (first (tetra-blocks tetra)))
         (move (second (tetra-blocks tetra)))
         (move (third (tetra-blocks tetra)))
         (move (fourth (tetra-blocks tetra)))))))
;;chekcing with L tetra   
(check-expect (tetra-move-right L)
              (make-tetra(make-posn 60 0)
                           (list
                            (make-block 70 0 'purple)
                            (make-block 50 10 'purple)
                            (make-block 60 10 'purple)
                            (make-block 70 10 'purple))))


;; rotate-bset: posn bset -> bset
;; given list of blocks and the center from the tetra
;; recursively sends each block in list to the block-rotate to be flipped
;; gives back the new list
(define (rotate-bset c bset)
  (cond
    [(empty? bset) empty]
    [else (list (block-rotate-ccw c (first bset))
                (block-rotate-ccw c (second bset))
                (block-rotate-ccw c (third bset))
                (block-rotate-ccw c (fourth bset)))]))
;; checking with L block
(check-expect (rotate-bset (make-posn HALF 0)
                           (list
                            (make-block (+ HALF PIXEL) 0 'purple)
                            (make-block (- HALF PIXEL) PIXEL 'purple)
                            (make-block HALF PIXEL 'purple)
                            (make-block (+ HALF PIXEL) PIXEL 'purple)))
              (list
                            (make-block 50 10 'purple)
                            (make-block 40 -10 'purple)
                            (make-block 40 0 'purple)
                            (make-block 40 10 'purple)))

;;display-game: world -> image
;;places the blocks in the tetra and in the pile on an empty scene the size of BOARD 
(define (display-game w)
  (local
    [(define (pile->image bset)
       (cond
         [(empty? bset) BOARD]
         [else
           (place-image (block->image (first bset)) 
                                 (block-x (first bset))
                                 (block-y (first bset))
                                 (pile->image (rest bset)))]))]
    
    (cond
      [(empty? (append (tetra-blocks(world-tetra w)) (world-pile w))) BOARD]
      [else (place-image (pile->image (append (tetra-blocks(world-tetra w)) (world-pile w)))
                                      55
                                      105
                                      BOARD)])))

(define test-bset (list
                   (make-block 50 200 'blue)
                   (make-block 40 200 'green)
                   (make-block 30 200 'red)
                   (make-block 60 200 'cyan)))
(check-expect (display-game (make-world O test-bset))
              (place-image (square 10 "solid" 'green) 50 0
                 (place-image (square 10 "solid" 'green) 40 0
                    (place-image (square 10 "solid" 'green) 50 10
                      (place-image (square 10 "solid" 'green) 40 10
                          (place-image (square PIXEL "solid" 'blue) 50 200
                             (place-image (square PIXEL "solid" 'green) 40 200
                                 (place-image (square PIXEL "solid" 'red) 30 200
                                    (place-image (square PIXEL "solid" 'cyan) 60 200
                                        (place-image BOARD 55 105 BOARD)))))))))) 
;; block->image: block -> image
;; takes in a block and makes a squares
;; one at the position of the block
(define (block->image b)
  (square PIXEL "solid" (block-color b)))
(check-expect (block->image (make-block 50 10 'purple))
              (square PIXEL "solid" 'purple))
;; pick-tetra: num -> tetra
;; when called based on number given selects a new tetra
(define (pick-tetra num)
  (cond
    [(= num 0) O]
    [(= num 1) I]
    [(= num 2) L]
    [(= num 3) J]
    [(= num 4) T]
    [(= num 5) Z]
    [(= num 6) S]))
(check-expect (pick-tetra 5) Z)

;; drop-down: bset -> bset
;; drops every block in the list by 1 PIXEL
(define (drop-down bset)
  (map drop-block bset))

;; drop-block: block -> block
;; drops one block down 1 PIXEL
(define (drop-block block)
       (make-block
        (block-x block)
          (+ (block-y block) PIXEL)
          (block-color block)))

;; count-line: number bset -> number
;; given a line number, count how many blocks in the bset are on that line (posn-y)
(define (count-line line bset)
  (length (filter (lambda (block) (= (* PIXEL line) (block-y block))) bset)))
(check-expect (count-line 20 test-bset) 4)

;; remove-line: number bset -> bset
;; removes all blocks at the [number] line from the bset
(define (remove-line line bset)
  (filter (lambda (block) (not (= (* PIXEL line) (block-y block)))) bset))
(check-expect (remove-line 20 test-bset) empty)

;; drop-rest: number bset -> bset
;; drops all blocks above line [number] down 1 PIXEL
(define (drop-rest line bset)
  (map (lambda (block) (if (> (* PIXEL line) (block-y block)) (drop-block block)
                           block)) bset))
(check-expect (drop-rest 5 (list 
                            (make-block 10 10 'red)))
              (list
               (make-block 10 20 'red)))

;; sweep-world: number world -> world
;; removes all full lines above [number] and drops the rest of the blocks
(define (sweep-world line world)
  (if (or (= 0 line) (> 10 (count-line line (world-pile world)))) world
      (sweep-world (sub1 line)
                   (make-world (world-tetra world) 
                               (drop-rest line (remove-line line (world-pile world)))))))
                            

;; next-world: world -> world
;; moves the tetra in play down one PIXEL each .5 second
;; checks to see if the tetra in motion has hit another tetra
;; if so add it to the pile
(define (next-world w)
  (local
     [(define (pile-collision blocks pile)
       (cond
         [(empty? blocks) false]
         [(cond
             [(= (block-y (first blocks)) (* PIXEL 20)) true]
             [(empty? pile) false]
             [(and (= (block-x (first blocks))
                      (block-x (first pile)))
                   (= (+ (block-y (first blocks)) PIXEL)
                      (block-y (first pile)))) true] 
             [else (pile-collision blocks (rest pile))])
             true]
         [else (pile-collision (rest blocks) pile)]))]
     
    (cond
      [(pile-collision (tetra-blocks 
                        (world-tetra w)) 
                       (world-pile w)) 
       (sweep-world 20 (make-world 
                        (pick-tetra (random 7))
                        (append (world-pile w) 
                                (tetra-blocks (world-tetra w)))))]
      [else 
       (sweep-world 20 (make-world (make-tetra
                                    (make-posn
                                     (posn-x (tetra-center (world-tetra w)))
                                     (+ PIXEL (posn-y (tetra-center (world-tetra w)))))
                                    (drop-down (tetra-blocks (world-tetra w))))       
                                   (world-pile w)))])))

(check-expect (next-world (make-world O (list
                                         (make-block 50 200 'blue)
                                         (make-block 40 200 'green)
                                         (make-block 30 200 'red)
                                         (make-block 60 200 'cyan))))
              (make-world (make-tetra (make-posn 50 10)
                                      (list
                                       (make-block 50 10 'green)
                                       (make-block 40 10 'green)
                                       (make-block 50 20 'green)
                                       (make-block 40 20 'green)))
                          (list
                                           (make-block 50 200 'blue)
                                           (make-block 40 200 'green)
                                           (make-block 30 200 'red)
                                           (make-block 60 200 'cyan))))
;;new-game: world -> bool
;; checks to see if tetra has touched the top of the board, if so returns true
;; ending the big bang
(define (new-game w)
  (cond
    [(empty? (world-pile w)) false]
    [(> 5 (block-y (first (world-pile w)))) true] 
    [else (new-game (make-world 
                     (world-tetra w)
                     (rest (world-pile w))))]))
(check-expect (new-game INITIAL) false)
(check-expect (new-game (make-world O (list
                                       (make-block 40 -10 'blue)))) true)
;; score: world -> image
;; score will calculate how many blocks are in the pile and then display the score
;; in text
(define (score w)
  (local
    [(define (adder bset)
       (cond
         [(empty? bset) 0]
         [(block? (first bset)) (add1 (adder (rest bset)))]))]
    (place-image (text "SCORE: " 25 'red) 55 25
    (place-image (text (number->string (adder (world-pile w))) 25 'red) 50 50 BOARD))))
(check-expect (score INITIAL) (place-image (text "SCORE: " 25 'red) 55 25
                                           (place-image (text "0" 25 'red) 50 50 BOARD)))

(big-bang INITIAL
   (on-tick next-world .150)
   (on-key key-handler)
   (to-draw display-game)
   (stop-when new-game score))
   
