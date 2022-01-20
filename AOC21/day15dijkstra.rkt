#lang racket

;AOC Day 15
;Leo Laporte 17 Jan 2022

#|==============================================================================

--- Day 15: Chiton ---

What is the lowest total risk of any path from the top left to the bottom right?
      
================================================================================|#

(require rackunit
         threading
         racket/trace
         data/heap)  ; for binary heap 

#|==============================================================================|#
#|                                      DATA                                    |#
#|==============================================================================|#

(define raw-input (file->string "input15.txt"))

(define raw-test "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(define up-test "19999\n19111\n11191") ; to make sure I'm not only looking at down and right moves

(struct grid (width height points) #:transparent)
;; Natural Natural (vector-of Natural)
;; where width and height are the dimensions of a rectangular grid
;; and points is a vector containing all the data points in
;; the structure

(struct node (point risk dist visited) #:transparent)
;; Natural Natural Natural Boolean
;; interp. point is the node's position on the grid
;; risk is that point's risk value, dist is the lowest total
;; risk travelling from start (initially 9999)
;; and visited is a boolean

;; String->Grid
;; Given a \n separated string produce a Grid structure
(define (input->grid str)
  (let ([width (string-length (first (string-split str)))]
        [height (length (string-split str))])
    
    (~> str 
        (string-replace _ "\n" "")       ; drop the \n
        (regexp-match* #px"(\\d{1})" _ ) ; create a list of digit strings
        (map string->number _)           ; turn the strings into Natural numbers
        (list->vector _)          ; convert the resulting list into a vector for easy addressing
        (grid width height _))))  ; make it a grid

(define input-grid (input->grid raw-input))
(define test-grid (input->grid raw-test))
(define up-test-grid (input->grid up-test))

#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#
#|

A little examination of the sample data and optimum path shows that I can't
simply select the lowest next point on the grid I will have to try all paths
and save the risk from each. I can short-circuit any route by keeping track
of the lowest number I've found and stop a route if it gets higher. 

A couple of other points, we don't count the risk of the starting point, and
even though the instructions imply backtracking is allowed it seems like that
would never produce a lower risk.

As usual, I'll represent this 2D array as a vector using the Grid structure. Which
means I'll have to calculate the height and width of the grid, too. I'll also bring in
my point->pos and pos->point functions and a modified version of my surrounding points
function.

OK this is really slow. I guess I've come up with the naive solution. But I notice
that it's very fast if I start with a max-risk close to the actual answer. By
short-circuiting guesses that are too big I can really speed things up. I've
been using a simple max-risk generator (just a simple L-shaped path - which
generates 60 in the test-grid) but I think I'll try my own version of Dijkstra:
go all the way down using the lowest risk at each point, creating a good first
guess. 

I'll use binary heap from data/heap. It's an implmentation of a priority queue
I'll use for 

|#
#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

;; natural natural grid -> natural
;; given an x y coordinate and a grid, produce the equivalent point on the grid vector
(define (pos->point x y g)
  (+ x (* y (grid-width g))))

;; Natural Grid -> (cons Natural Natural)
;; Given a point on the grid vector and the accompanying Grid produce
;; the corresponding x,y coordinate as a cons
(define (point->pos p g)
  (let-values ([(y x) (quotient/remainder p (grid-width g))])
    (cons x y)))

;; Natural Grid -> Natural
;; returns the value of a point in a grid
(define (get-value p g)
  (vector-ref (grid-points g) p))

;; Natural Grid -> (list-of Natural)
;; given a point on a grid structure, return a list of the 
;; surrounding points - with lowest risk points first
(define (surrounds p g)
  (let* ([pos (point->pos p g)]
         [x (car pos)]
         [y (cdr pos)]
         [width (grid-width g)]
         [height (grid-height g)]
         [l (cons (sub1 x) y)]
         [r (cons (add1 x) y)]
         [u (cons x (sub1 y))]
         [d (cons x (add1 y))])
    
    (define (in-grid? xy) (and (< -1 (car xy) width) (< -1 (cdr xy) height)))
   
    (~>  (list d r l u)               ; the list of points as (x . y)
         (filter in-grid? _)          ; remove points that are off the grid
         (map (λ (x) (pos->point (car x) (cdr x) g)) _)))) ; convert (x . y) back to vector-ref

(define (make-unvisited-queue g)
  "use info from grid g to build a heap filled with unvisited node structures"
  (define (node<=? x y)  ; sort by total distance from start (distance = risk)
    (< (node-dist x) (node-dist y)))
  
  (let ([h (make-heap node<=?)])
    (for ([p (in-range (vector-length (grid-points g)))])
      (heap-add! (node p (vector-ref (grid-points g) p) 9999 #f))))) ; infinite dist and unvisited
   
(define (day15.1 g)
  (define unvisited (make-unvisited-queue g))
  (define destination (sub1 (vector-length (grid-points g)))) ; destination: last point on grid

  (for )

; update estimates
  let c = current point
  for-each surrounding 0
       p.lowest = c.current + p.risk
         if (> (node-lowest p) 


; visit next points
               unexplored point with lowest dist
  
  (define (next-step p g risk max-risk seen)          ; check current point on grid
    ;  (printf "Point: ~a - Risk: ~a - Max: ~a - Seen: ~a\n\n" p risk max-risk seen)
    (cond [(equal? p end) (set! max-risk risk) risk]  ; end, make risk new max, return it
          [(>= risk max-risk) risk]                   ; this path is worse - abandon
          [(set-member? seen p) max-risk]             ; been here already - don't repeat
          [else
           (next-steps                            ; go on with next points on grid
            (look-around p g)                     ; get next points sorted by risk
            g                                     ; where are we again? oh yeah, grid
            (+ risk (get-value p g))              ; keeping track of path's total risk
            max-risk                              ; keep track of best path so far
            (set-add seen p))]))                  ; adding this point to seen
  
  (define (next-steps lop g risk max-risk seen)
    (cond [(empty? lop) max-risk]
          [else (min (next-step (first lop) g risk max-risk seen)
                     (next-steps (rest lop) g risk max-risk seen))]))
  
  (next-step 0 g 0 9999 (set)))

(module+ test
  (check-equal? (day15.1 up-test-grid) 8)
  (check-equal? (day15.1 test-grid) 40)
  )

; (time (printf "2021 AOC Problem 15.1 = ~a\n" (day15.1 input-grid)))

#|=================================================================================
                                        PART 2
                               

==================================================================================|#

; (time (printf "2021 AOC Problem 15.2 = ~a\n" (day15.1 input)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
