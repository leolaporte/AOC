#lang racket

;AOC Day 15
;Leo Laporte 17 Jan 2022

#|==============================================================================

--- Day 15: Chiton ---

What is the lowest total risk of any path from the top left to the bottom right?
      
================================================================================|#

(require rackunit
         threading
         racket/trace)

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

(struct grid (width height points) #:transparent)
;; a grid is a structure
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

;; Natural Grid -> (list-of Natural)
;; given a point on a grid structure, return a list of the 
;; surrounding points 
(define (look-around p g)
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

    (~>  (list u l r d)               ; the list of points as (x . y)
         (filter in-grid? _)          ; remove points that are off the grid
         (map (λ (x) (pos->point (car x) (cdr x) g)) _)))) ; convert (x . y) back to vector-ref

(define (day15.1 g)
  (let ([end (sub1 (vector-length (grid-points g)))])  
    
  (define (next-step p g risk max-risk visited)
    (cond [(equal? p end) (set! max-risk risk) max-risk]
          [(>= risk max-risk) risk]
          [(not (false? (member p visited))) max-risk]
          [else (set! risk (+ risk (vector-ref (grid-points g) p)))
                (set! visited (cons p visited))
                (next-steps (look-around p g) g risk max-risk visited)]))

    (define (next-steps lop g risk max-risk visited)
      (cond [(empty? lop) max-risk]
            [else (min (next-step (first lop) g risk max-risk visited)
                       (next-steps (rest lop) g risk max-risk visited))]))
  
  (next-step 0 g 0 (apply + (vector->list (grid-points g))) empty)))

(module+ test
  (check-equal? (day15.1 test-grid) 40))

; (time (printf "2021 AOC Problem 15.1 = ~a\n" (day15.1 input)))

#|=================================================================================
                                        PART 2
                               

==================================================================================|#

; (time (printf "2021 AOC Problem 15.2 = ~a\n" (day15.1 input)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
