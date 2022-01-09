#lang racket
; AOC 2021
; Leo Laporte 14-Dec 2021
; 
; 
; --- Day 9: Smoke Basin ---
; 
; These caves seem to be lava tubes. Parts are even still volcanically active; small
; hydrothermal vents release smoke into the caves that slowly settles like rain.
; 
; If you can model how the smoke flows through the caves, you might be able to avoid it
; and be that much safer. The submarine generates a heightmap of the floor of the nearby
; caves for you (your puzzle input).
; 
; Smoke flows to the lowest point of the area it's in. For example, consider the following
; heightmap:
; 
; 2199943210
; 3987894921
; 9856789892
; 8767896789
; 9899965678
; 
; Each number corresponds to the height of a particular location, where 9 is the highest
; and 0 is the lowest a location can be.
; 
; Your first goal is to find the low points - the locations that are lower than any of its
; adjacent locations. Most locations have four adjacent locations (up, down, left, and right);
; locations on the edge or corner of the map have three or two adjacent locations,
; respectively. (Diagonal locations do not count as adjacent.)
; 
; In the above example, there are four low points, all highlighted: two are in the first
; row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5).
; All other locations on the heightmap have some lower adjacent location, and so are not low points.
; 
; The risk level of a low point is 1 plus its height. In the above example, the risk levels
; of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the
; heightmap is therefore 15.
; 
; Find all of the low points on your heightmap. What is the sum of the risk levels of all low
; points on your heightmap?
; 


(require threading        ; some flow macros including ~>
         rackunit         ; for testing
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        DATA                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I like to represent two-dimensional arrays as a one-dimensional vector

(struct grid (width height points) #:transparent)
;; a grid is a structure
;; where width and height are the dimensions of a rectangular grid
;; and points is a vector containing all the data points in
;; the structure (in this case the heightmap of the sea floor).

;; Parse provided AOC Problem data into a vector of all points
(define (make-heightmap str)
  "given an \n separated string of single digits return a vector of the digits as Natural"
  (~> str 
      (string-replace _ "\n" "")       ; drop the \n
      (regexp-match* #px"(\\d{1})" _ ) ; create a list of digit strings
      (map string->number _)           ; turn the strings into Natural numbers
      (list->vector _)))      ; convert the resulting list into a vector for easy addressing

;; create a grid struct for the provided data
(define problem-grid
  (let* ([day9data (file->string "input9.txt")]
         [width (string-length (first (string-split day9data "\n")))]    ; width of grid 
         [height (length (string-split day9data "\n"))])                 ; height of grid 
    (grid width height (make-heightmap day9data))))                      ; make-struct grid

;; A grid struct for the test data
(define test-grid
  (let* ([sample-data "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"]
         [width (string-length (first (string-split sample-data "\n")))]
         [height (length (string-split sample-data "\n"))] )
    (grid width height (make-heightmap sample-data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     LIL UTILITIES                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; natural natural grid -> natural
;; given an x y coordinate and a grid, produce the equivalent point on the grid vector
(define (pos->point x y g)
  (+ x (* y (grid-width g))))

(module+ test
  (check-equal? (pos->point 0 0 test-grid) 0)
  (check-equal? (pos->point 0 1 test-grid) 10)
  (check-equal? (pos->point 5 3 test-grid) 35)
  (check-equal? (pos->point 9 4 test-grid) 49))

;; Natural Grid -> (cons Natural Natural)
;; Given a point on the grid vector and the accompanying Grid produce
;; the corresponding x,y coordinate as a cons
(define (point->pos p g)
  (let-values ([(y x) (quotient/remainder p (grid-width g))])
    (cons x y)))

(module+ test
  (check-equal? (point->pos (pos->point 4 5 test-grid) test-grid) '(4 . 5))
  (check-equal? (point->pos (pos->point 0 0 test-grid) test-grid) '(0 . 0))
  (check-equal? (point->pos (pos->point 9 4 test-grid) test-grid) '(9 . 4)))

;; Natural Natural Grid -> Natural
;; given a point and a grid, read the value at that point
(define (get-value p g)
  (vector-ref (grid-points g) p))

(module+ test
  (check-equal? (get-value 0 test-grid) 2)
  (check-equal? (get-value 10 test-grid) 3)
  (check-equal? (get-value 35 test-grid) 9)
  (check-equal? (get-value 49 test-grid) 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     THE REAL WORK                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
             
(module+ test
  (check-equal? (look-around 13 test-grid) (list 3 12 14 23))
  (check-equal? (look-around 0 test-grid) (list 1 10))
  (check-equal? (look-around (pos->point 9 4 test-grid) test-grid) (list 39 48)))

;; Natural Natural Grid -> Boolean
;; given an x.y coordinate and the corresponding grid, return #true if it's a low point:
;; "a location lower than any of its adjacent locations"
(define (low-point? p g)
  (let* ([pts (map (λ (x) (get-value x g)) (look-around p g))]    ; values of the surrounding points
         [val (get-value p g)]) ; value of the point we're checking
    (< val (apply min pts))))   ; is it lower than all the rest?

(module+ test
  (check-equal? (low-point? 1 test-grid) #t)
  (check-equal? (low-point? 15 test-grid) #f))

;; Grid -> (list-of Natural)
;; Given a grid, return a list of low-points
(define (get-low-points g)
  (for/fold  ([lp empty]   ; accumulator: list of low points in g
              #:result (reverse lp)) 
            
             ([p (in-range (vector-length (grid-points g)))]) ; for every point in Grid

    ; check to see if it's a low-point, and, if it is, cons it to low-points
    (values (if (low-point? p g) 
                (cons p lp)
                lp))))

(module+ test
  (check-equal? (get-low-points test-grid) '(1 9 22 46)))

;; Grid -> Natural
;; given a Grid structure, return the sum of the value+1 of all its
;; low points (the Part 1 solution)
(define (day9.1 g)
  (~> (get-low-points g)              ; get a list of the low-points in the grid
      (map (λ (x) (get-value x g)) _) ; convert it to a list of values
      (map add1 _)                    ; add 1 to each value
      (apply + _)))                   ; then sum them for the result

(module+ test
  (check-equal? (day9.1 test-grid) 15))

(time (printf "2021 AOC Problem 9.1 = ~a\n" (day9.1 problem-grid)))

;  --- Part Two ---
; 
; Next, you need to find the largest basins so you know what areas are most important to avoid.
; 
; A basin is all locations that eventually flow downward to a single low point. Therefore,
; every low point has a basin, although some basins are very small. Locations of height 9 do not
; count as being in any basin, and all other locations will always be part of exactly one basin.
; 
; The size of a basin is the number of locations within the basin, including the low point. The
; example above has four basins.
; 
; The top-left basin, size 3:
; 
; 2199943210
; 3987894921
; 9856789892
; 8767896789
; 9899965678
; The top-right basin, size 9:
; 
; 2199943210
; 3987894921
; 9856789892
; 8767896789
; 9899965678
; The middle basin, size 14:
; 
; 2199943210
; 3987894921
; 9856789892
; 8767896789
; 9899965678
; The bottom-right basin, size 9:
; 
; 2199943210
; 3987894921
; 9856789892
; 8767896789
; 9899965678
; Find the three largest basins and multiply their sizes together. In the above example,
; this is 9 * 14 * 9 = 1134.
; 
; What do you get if you multiply together the sizes of the three largest basins?
; 


;; NOTES:
;; Hmmm. How to solve this. I can start with the low points found in part one, then expand
;; out from every point until I reach a 9 or the border of the grid. This will produce the
;; basins. All I need to save, though, is the number of points in the basin. The solution
;; is the product of the three largest basins' sizes.

;; A basin is a tree with the node values = 9. I can use CAR/CDR recursion to walk the
;; tree collecting the points as I go. Counting the visited points will give me the number
;; of points in the basin. 

;; Natural Grid -> Natural
;; Given a low-point on a grid produce the number of points in the associated basin
(define (map-basin p g)
  (define seen (list p))   ; the list of visited points starting with the low-point

  (define (next-points p)
    (let ([surrounding-points (filter-not
                      (λ (x) (or (equal? (get-value x g) 9) (member x seen)))
                      (look-around p g))])
      (set! seen (append surrounding-points seen))   ; add surrounding points to seen
      surrounding-points))                           ; return list of surrounding points

  (define (expand-to surrounding-points)
    (cond [(empty? surrounding-points) empty]
          [else
           (append (expand-to (next-points (car surrounding-points)))
                   (expand-to (cdr surrounding-points)))]))

  (expand-to (next-points p))
  (length seen))
  
(module+ test
  (check-equal? (map-basin 0 test-grid) 3)
  (check-equal? (map-basin 9 test-grid) 9)
  (check-equal? (map-basin 22 test-grid) 14)
  (check-equal? (map-basin 46 test-grid) 9))

(define (day9.2 a-grid)
  (~> (get-low-points a-grid)               ; get list of low points
      (map (λ (x) (map-basin x a-grid)) _)  ; get number of points in each low-point's basin
      (sort _ >)                            ; sort into descending order
      (take _ 3)                            ; take the three highest
      (apply * _)))                         ; multiply them together  

(module+ test
  (check-equal? (day9.2 test-grid) 1134))

(time (printf "2021 AOC Problem 9.2 = ~a\n" (day9.2 problem-grid)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
; 2021 AOC Problem 9.1 = 603
; cpu time: 16 real time: 16 gc time: 0
; 2021 AOC Problem 9.2 = 786780
; cpu time: 35 real time: 35 gc time: 0