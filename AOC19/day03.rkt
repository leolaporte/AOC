#lang racket

;  2019 Advent of Code Solutions
; https://adventofcode.com/2019/day/3
; 
; --- Day 3: Crossed Wires ---
; 
; The gravity assist was successful, and you're well on your way to the Venus refuelling station. During the rush
; back on Earth, the fuel management system wasn't completely installed, so that's next on the priority list.
; 
; Opening the front panel reveals a jumble of wires. Specifically, two wires are connected to a central port and
; extend outward on a grid. You trace the path each wire takes as it leaves the central port, one wire per line
; of text (your puzzle input).
; 
; The wires twist and turn, but the two wires occasionally cross paths. To fix the circuit, you need to find the
; intersection point closest to the central port. Because the wires are on a grid, use the Manhattan distance for
; this measurement. While the wires do technically cross right at the central port where they both start, this
; point does not count, nor does a wire count as crossing with itself.
; 
; For example, if the first wire's path is R8,U5,L5,D3, then starting from the central port (o), it goes right 8,
; up 5, left 5, and finally down 3:
; 
; ...........
; ...........
; ...........
; ....+----+.
; ....|....|.
; ....|....|.
; ....|....|.
; .........|.
; .o-------+.
; ...........
; 
; Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4, and left 4:
; 
; ...........
; .+-----+...
; .|.....|...
; .|..+--X-+.
; .|..|..|.|.
; .|.-X--+.|.
; .|..|....|.
; .|.......|.
; .o-------+.
; ...........
; 
; These wires cross at two locations (marked X), but the lower-left one is closer to the central port: its
; distance is 3 + 3 = 6.
; 
; Here are a few more examples:
; 
;     R75,D30,R83,U83,L12,D49,R71,U7,L72
;     U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
;     R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
;     U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
; 
; What is the Manhattan distance from the central port to the closest intersection?
; 
; Manhattan Distance:
; In a plane with p1 at (x1, y1) and p2 at (x2, y2), it is |x1 - x2| + |y1 - y2|
; 
; 


(require rackunit racket/string racket/file racket/set)

;; --------------------
;; DATA STRUCTURES
;; --------------------

(struct pos (x y) #:transparent)
;; interp. (pos Number Number)
;; position on an x y axis
  
(struct move (x y)  #:transparent)
;; interp. (move Number Number)
;; where the first Natural is the distance and direction of the move
;; for the X axis (neg is left, pos is right)
;; and the second Natural is the same for the y axis, neg is up, pos is down
;; e.g. (-1 1) is left and down by 1


;; --------------------
;; CONSTANTS
;; --------------------

(define DATA "input3.txt")
(define ORIGIN (pos 0 0))

; Examples for tests
(define W1 "R8,U5,L5,D3\nU7,R6,D4,L4")
(define W1S 6)

(define W2 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")
(define W2S 159)

(define W3 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
(define W3S 135)

;; --------------------
;; FUNCTIONS
;; --------------------

;; String -> (list-of (list-of Move))
;; converts the input string into a list of two lists of moves
;; each list of moves is one wire
;; assume lists are equal in size, well-formed, and non-empty
(define (input->moves s)
  (local [(define los (string-split s))  ; input data is a string separated by \n, turn it into 2 strings
          (define (str->move str)        ; convert provided "<dir><dist>" string into move, e.g. "U2" becomes (0 -2)
            (match (substring str 0 1)   ; direction
              ["U" (move 0 (- (string->number (substring str 1))))]
              ["D" (move 0 (+ (string->number (substring str 1))))]
              ["L" (move (- (string->number (substring str 1))) 0)]
              ["R" (move (+ (string->number (substring str 1))) 0)]))]
    ; split provided data into "<dir><dist>" strings, then turn into a list containing two lists of Moves (1 per wire)
    (list (map str->move (string-split (first los) ",")) (map str->move (string-split (second los) ",")))))

;; test input->moves
(check-equal? (input->moves "U2,L23\nD15,R200")
              (list (list (move 0 -2) (move -23 0)) (list (move 0 15) (move 200 0)))) 

;; pos move -> pos
;; given the current pos and move, return the next pos
(define (next-pos p m)
  (pos (+ (pos-x p) (move-x m)) (+ (pos-y p) (move-y m))))

(check-equal? (next-pos (pos 0 0) (move 5 0)) (pos 5 0))
(check-equal? (next-pos (pos 10 20) (move -5 0)) (pos 5 20))
(check-equal? (next-pos (pos 0 0) (move 0 -20)) (pos 0 -20))

;; pos move -> (list-of pos)
;; given the starting pos and the move. creates a list of the next positions up to
;; and including the end point, but not including the starting point
(define (next-position-list psn mv)
  (local [(define dest (next-pos psn mv))                                    ; the end point
          (define offset (if (or (< (move-x mv) 0) (< (move-y mv) 0)) -1 1)) ; are we going forward or back
          (define horizontal-move? (equal? (move-y mv) 0))                   ; movement on the x or y axis?  
          (define start
            (if horizontal-move?
                (+ (pos-x psn) offset)
                (+ (pos-y psn) offset))) ; the starting point on that axis
          (define end (if horizontal-move? (pos-x dest) (pos-y dest)))]      ; the end point on that axis
    (for/list
        ([i (in-range start (+ end offset) offset)])                ; offset based on direction: -1 or +1
      (if horizontal-move?
          (pos i (pos-y psn))
          (pos (pos-x psn) i)))))       ; this routine is the bottleneck - optimize?
 
(check-equal? (next-position-list ORIGIN (move -1 0)) (list (pos -1 0)))
(check-equal? (next-position-list (pos 5 10) (move 0 2)) (list (pos 5 11) (pos 5 12)))
(check-equal? (next-position-list (pos 5 10) (move 0 3))
              (list (pos 5 11) (pos 5 12) (pos 5 13)))

;; (list-of Move) -> (list-of Pos)
;; given the starting point and subsequent moves list of a wire,
;; return the list of all the positions it occupies except the origin
(define (make-wire-move-list pos lom)
  (cond [(empty? lom) empty]
        [else
         (append (next-position-list pos (first lom))
                 (make-wire-move-list (next-pos pos (first lom)) (rest lom)))]))
    
(check-equal? (make-wire-move-list ORIGIN (list (move -1 0) (move -2 0)))
              (list (pos -1 0) (pos -2 0) (pos -3 0)))

(check-equal? (make-wire-move-list ORIGIN (list (move -1 0) (move 4 0)))
              (list (pos -1 0) (pos 0 0) (pos 1 0) (pos 2 0) (pos 3 0)))

;; (List-of pos) -> (list-of Number)
;; Given a list of intersections return a list of Manhattan distances to the ORIGIN
(define (lowest-md lop)
  (local [(define (dist pt)  (+ (abs (pos-x pt)) (abs (pos-y pt))))]  ; the Manhattan dist to ORIGIN
    (apply min (map dist lop))))
  
(check-equal? (lowest-md (list (pos 0 0) (pos 0 1) (pos 0 2)))
              0)
(check-equal? (lowest-md (list (pos 5 20) (pos -3 21) (pos 42 -1)))
              24)

;; String -> Number
;; Given a problem set string, produce the distance of the closest intersection of wires
(define (solve-day3.1 s)
  (local [(define move-lists (input->moves s))]
    (lowest-md
     (set-intersect
      (make-wire-move-list ORIGIN (first move-lists))
      (make-wire-move-list ORIGIN (second move-lists))))))

(check-equal? (solve-day3.1 W1) W1S)
(check-equal? (solve-day3.1 W2) W2S)
(check-equal? (solve-day3.1 W3) W3S)

;; OK put it all together - boy this takes forever - looks like the for loop does 300,000 plus iterations!
;  (time (printf "AOC Problem 3.1 = ~a\n" (solve-day3.1 (file->string DATA))))

;  
; --- Part Two ---
; 
; It turns out that this circuit is very timing-sensitive; you actually need to minimize the signal delay.
; 
; To do this, calculate the number of steps each wire takes to reach each intersection; choose the intersection
; where the sum of both wires' steps is lowest. If a wire visits a position on the grid multiple times, use the
; steps value from the first time it visits that position when calculating the total value of a specific
; intersection.
; 
; The number of steps a wire takes is the total number of grid squares the wire has entered to get to that
; location, including the intersection being considered. Again consider the example from above:
; 
; ...........
; .+-----+...
; .|.....|...
; .|..+--X-+.
; .|..|..|.|.
; .|.-X--+.|.
; .|..|....|.
; .|.......|.
; .o-------+.
; ...........
; 
; In the above example, the intersection closest to the central port is reached after 8+5+5+2 = 20 steps by the
; first wire and 7+6+4+3 = 20 steps by the second wire for a total of 20+20 = 40 steps.
; 
; However, the top-right intersection is better: the first wire takes only 8+5+2 = 15 and the second wire takes
; only 7+6+2 = 15, a total of 15+15 = 30 steps.
; 
; Here are the best steps for the extra examples from above:
; 
;     W1 = 30 steps
;     W2 = 610 steps
;     W3 = 410 steps
; 
; What is the fewest combined steps the wires must take to reach an intersection?
; 


;; string -> list-of Number
;; given the problem set in a string, return a list of distances to intersections
(define (get-shortest-distance s)
  (local [(define wires (input->moves s))
          (define wire1-steps (make-wire-move-list ORIGIN (first wires)))
          (define wire2-steps (make-wire-move-list ORIGIN (second wires)))
          (define list-of-intersections (set-intersect wire1-steps wire2-steps))]
    (apply min (list-of-distances list-of-intersections wire1-steps wire2-steps))))

;; (list-of Pos) (List-of Pos) (List-of Pos) -> (list-of Number)
;; given the intersections and the steps for both wires, return a list of
;; distances for all the intersections

(define (list-of-distances loi w1 w2)
  (local [(define (dist i) (+ (+ (index-of w1 i) 1) (+ (index-of w2 i) 1)))] ; add one to include intersection point
    (cond [(empty? loi) empty]
          [else
           (cons (dist (first loi))
                 (list-of-distances (rest loi) w1 w2))])))

(check-equal? (get-shortest-distance W1) 30)
(check-equal? (get-shortest-distance W2) 610)
(check-equal? (get-shortest-distance W3) 410)

; (time (printf "AOC Problem 3.2 = ~a\n" (get-shortest-distance (file->string DATA))))
