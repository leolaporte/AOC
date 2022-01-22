#lang racket

;AOC Day 15
;Leo Laporte 17 Jan 2022

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

;; Set up a priority queue using Racket's data/heap
;; adapted from ﻿Stelly, James. W.. Racket Programming the Fun Way (p. 193). No Starch Press.

(define (peek q) (heap-min q))  ; what's the next lowest-risk vertex?

﻿(define (push q n) (heap-add! q n)) ; add a vertex to the queue

(define (pop q)   ; remove a vertex, returns the vertex
  (let ([n (peek q)])
    (heap-remove-min! q)
    n))

﻿(define (queue->list q) (for/list ([n (in-heap q)]) n))

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
;  (for ([i (in-range 1 (vector-length (grid-points grid)))]) ; leave out start
;    (push q (cons i 9999)))
 
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
                 (push q (cons n total-risk)))))         ; update the node in the queue

           ; we've done all the neighbors
           (let ([new-vertex (pop q)])       ; so pop next vertex off the queue (i.e. it's visited)
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



;(module+ test
;  (check-equal? (day15.2 test2-grid) 315)
;  )
; (time (printf "2021 AOC Problem 15.2 = ~a\n" (day15.1 input)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
