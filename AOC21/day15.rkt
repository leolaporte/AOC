#lang racket

;AOC Day 15
;Leo Laporte 17-22 Jan 2022

#|==============================================================================

--- Day 15: Chiton ---

What is the lowest total risk of any path from the top left to the bottom right?
      
================================================================================|#

(require rackunit
         threading
         profile
         racket/trace
         data/heap)  ; for priority queue 

#|==============================================================================|#
#|                                      DATA                                    |#
#|==============================================================================|#

(define raw-input (file->string "input15.txt"))

(define raw-test "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581")

(struct grid (width height points) #:transparent)
;; Natural Natural (vector-of Natural)
;; where width and height are the dimensions of a rectangular grid
;; and points is a vector containing all the data points in
;; the structure

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

#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#
#|

This is a classic path-finding problem. If you read "risk" as "distance" it's
pretty clear what I'll need to do . A little examination of the sample data
and optimum path shows that I can't simply select the lowest next point on the
grid I will have to try all paths and save the risk from each. I can brute force
the sample data but it gets pretty big fast (as usual).

So optimizations: I can short-circuit any route by keeping track
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
generates 60 in the test-grid) but I think I'll try to grok and implement Dijkstra.

As I understand it, this algo spreads by starting at start, set distance to 0,
examine the (up to) four surrounding nodes, if none of them are the end point,
calculate the distance from start, and if it's lower than the node-dist replace
 node-dist. Once you've completed that for all the surrounding points, remove
start from the unvisited queue and pop the next point (the queue auto-sorts
so that the next point is always the one with the lowest node-dist). Repeat.

If Dijkstra isn't fast enough you can further optimize by adding a heuristic for
choosing the next item in the queue - that's called A*. The heuristic for
a grid like this is just "prefer down and to the right" - aka Manhattan. 

I'll use Racket's binary heap from data/heap for my priority queue.

Further optimizations took the first part from 160 seconds to 70 milliseconds.
Turns out that most of the time spent was in searching the queue. So I minimized
queue interactions. I only push and pop now. I created a hash for total risk
and visited for quick searches. Also I don't bother ever deleting higher queue
entries, just push on the new, lower entries. I can ignore the dupes. Further-
more, unlike in a traditional Dijkstra I don't need to fill the queue with
infinite distance (risk) nodes because I'm not trying to find all paths, I just
need the lowest risk number. Saved a LOT of time. Now it's fast enough for the
giant grid in part two. I think. 

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
;; surrounding points
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

;; Doin' the Dijkstra

;; Grid -> Natural
;; Given a Grid, return the length of the shortest path from the
;; first point to the last
(define (day15.1 grid)

  (define start 0)
  (define end (sub1 (vector-length (grid-points grid))))

  ; create a priority queue for the analyzed nodes 
  (define (risk<=? x y) (<= (cdr x) (cdr y)))
  (define q (make-heap risk<=?))  ; auto-sort by risk
 
  (define (pop q)   ; remove a vertex, returns the values 
    (let ([n (heap-min q)])
      (heap-remove-min! q)
      n))
  
  ; to speed this up I'm using two hashes to track total risk from start
  ; to a node, and whether that node has been visited
  (define risks (make-hash))
  (define visited (make-hash))
  (for ([p (in-range (vector-length (grid-points grid)))]) 
    (hash-set! risks p 9999)  ; risk starts infinite until actually calculated
    (hash-set! visited p #f)) ; all nodes start unvisited
  
  (define (walk-path vertex risk)
    
    (cond [(equal? vertex end) risk]   ; all done, return total risk
          
          [else
          
           (for ([n (in-list (filter
                              (λ (x) (not (hash-ref visited x)))   ; if visited is false
                              (surrounds vertex grid)))])          ; process neighbors
             
             (let* ([neighbor-risk (vector-ref (grid-points grid) n)]  ; base risk of neighbor vertex
                    [total-risk (+ risk neighbor-risk)]) ; add base risk to source vertex risk
               (when (< total-risk (hash-ref risks n))   ; found a better route?
                 (hash-set! risks n total-risk)          ; replace old total with new better total
                 (heap-add! q (cons n total-risk)))))         ; update the node in the queue

           ; we've done all the neighbors
           (let ([new-vertex (pop q)])          ; so pop next vertex off the queue 
             (hash-set! visited new-vertex #t)  ; we no longer need to consider it
             (walk-path (car new-vertex) (cdr new-vertex)))]))   ; and repeat

  (walk-path start 0))

(module+ test
  (check-equal? (day15.1 test-grid) 40))

; (profile-thunk (thunk (day15.1 input-grid)))

(time (printf "2021 AOC Problem 15.1 = ~a\n" (day15.1 input-grid)))

#|=================================================================================
                                        PART 2

The entire cave is actually five times larger in both dimensions than you thought;
the area you originally scanned is just one tile in a 5x5 tile area that forms the
full map. Your original map tile repeats to the right and downward; each time the
tile repeats to the right or downward, all of its risk levels are 1 higher than the
tile immediately up or left of it. However, risk levels above 9 wrap back around to 1.

What is the lowest total risk of any path from the top left to the bottom right?

==================================================================================|#

;; The only challenge here is to assemble the input data. I'll do this going across
;; and down, copying the source info from the original grid and bumping it by a
;; calculated amount. Oh but there's a challenge. Risk can only be 1-9 no zeros!

(define (make-giant-grid g)
  (define new-dim 5)
  
  (define width (* (grid-width g) new-dim))
  (define height (* (grid-height g) new-dim))
  (define big-vec (make-vector (* height width)))
  (define big-grid (grid width height big-vec))
  (define source (grid-points g))

  ; numbers are in range of 1-9 and 9 rolls over to 1
  ; this is surprisingly tricky
  (define (bump origin factor)
    (let ([res (+ origin factor)])
      (if (> res 9)
          (add1 (modulo res 10))
          res)))

  ;; we'll do this by row
  (for ([row (in-range 0 new-dim)])
    (for ([y (in-range (* row (grid-height g))  (* (add1 row) (grid-height g)))])
      (for ([x (in-range width)])
        (let ([origin (vector-ref source (pos->point (remainder x (grid-width g))
                                                     (remainder y (grid-height g)) g))]
              [factor (+ row (quotient x (grid-width g)))])
          (vector-set! big-vec (pos->point x y big-grid) (bump origin factor))))))
  
  big-grid)

(define (day15.2 g) (day15.1 (make-giant-grid g)))

(module+ test
  (check-equal? (day15.2 test-grid) 315))

(time (printf "2021 AOC Problem 15.2 = ~a\n" (day15.2 input-grid)))

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

2021 AOC Problem 15.1 = 423
cpu time: 65 real time: 77 gc time: 10
2021 AOC Problem 15.2 = 2778
cpu time: 2146 real time: 2333 gc time: 234

2022 Mac Studio Max with 32GB RAM

2021 AOC Problem 15.1 = 423
cpu time: 57 real time: 59 gc time: 9
2021 AOC Problem 15.2 = 2778
cpu time: 2063 real time: 2088 gc time: 450

|#