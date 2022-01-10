#lang racket

;;; AOC 2021
;;; Leo Laporte 8-Jan-2022
;;; 
;;; --- Day 13: Transparent Origami ---
;;; 
;;; The first section is a list of dots on the transparent paper.
;;;
;;; Then, there is a list of fold instructions. Each instruction indicates a line
;;; on the transparent paper and wants you to fold the paper up (for horizontal y=... lines)
;;; or left (for vertical x=... lines). In this example, the first fold instruction
;;; is fold along y=7, which designates the line formed by all of the positions 
;;; where y is 7 (marked here with -):
;;; 
;;; Because this is a horizontal line, fold the bottom half up. Some of the dots
;;; might end up overlapping after the fold is complete, but dots will never appear
;;; exactly on a fold line.
;;; 
;;; Now, only 17 dots are visible.
;;; 
;;; How many dots are visible after completing just the first fold instruction
;;; on your transparent paper?

(require threading
         rackunit
         racket/trace)

;; NOTES: Another Monday another fairly simple problem. I'll use the Grid structure
;; from Day 9. Looks like I'll have to calculate the height and width, and recalculate
;; it after every fold. I'll represent a dot with 1 and 0 for no dot. The only trick
;; here is to realign the vector after a fold, removing the folded part and if the fold
;; isn't in the middle (i'm guessing it will never be) extending the grid on the opposite
;; side by the overlap. It's just arithmetic. First, the data.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                DATA                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(struct grid (width height points) #:transparent)
;; a grid is a structure
;; where width and height are the dimensions of a rectangular grid
;; and points is a vector containing all the data points in
;; the structure

(struct fold-line (v h) #:transparent)
;; a fold-line is a structure
;; where v is a point on a grid where a vertical fold occurs
;; and h is the point where a horizontal fold begins.

(define DOT 1)
(define EMPTY 0)

;; Problem input from adventofcode.com
(define day13data (file->string "input13.txt"))
(define test-data
  "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\n\nfold along y=7\nfold along x=5\n")

;; Split the input into two values, the points and the instructions
(define input-str (string-split day13data "\n\n"))
(define-values (input-points input-instructions) (values (first input-str) (second input-str)))

(define test-str (string-split test-data "\n\n"))
(define-values (test-points test-instructions) (values (first test-str) (second test-str)))

;; this is going to take some massaging. I need to find the height and width of the grid
;; then create a vector of the proper length and use the provided points to set the dots on
;; the vector.

(define (str->pos str)
  "Converts input string to a list of x y coordinates for the dots"
  (~> str
      (string-split _ "\n")
      (map (λ (x) (regexp-match #px"(\\d+),(\\d+)" x)) _)
      (map (λ (x) (rest x)) _)
      (map (λ (x) (map string->number x)) _)))

(module+ test
  (check-equal? (str->pos "6,1003\n0,14\n902,10\n0,3\n10,4\n4,11\n6,0\n6,12\n")
                (list '(6 1003) '(0 14) '(902 10) '(0 3) '(10 4) '(4 11) '(6 0) '(6 12))))

(define (make-grid str)
  "takes a string of points representing dots and turns it into a grid structure"
  (let* ([dot-list (str->pos str)]                                        ; x y coords for each dot
         [width (add1 (apply max (map (λ (x) (first x)) dot-list)))]      ; find max x + 1 for the width
         [height (add1 (apply max (map (λ (x) (second x)) dot-list)))]    ; max y + 1 for the height
         [dots (map (λ (x) (+ (first x) (* (second x) width))) dot-list)] ; covert x y to vector pos
         [vec (make-vector (* height width) EMPTY)])                      ; make 0 filled vector
    (for ([pos (in-list dots)])      
      (vector-set! vec pos DOT))                                          ; set the dots
    (grid width height vec)))                                             ; build the grid

(module+ test
  (check-equal? (make-grid "1,2\n3,4\n")
                (grid 4 5 '#(0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1))))

;; Now to parse the instructions. The text is something like:
;; "fold along y=7" - all I really care about is x or y for
;; vertical or horizontal fold and the number for the row or col
;; How about I represent the instruction as (0 7) for a fold
;; across the middle at row 7 and (7 0) for a fold down the middle
;; at column 7? Let's try that with a fold-line structure. 

(define (inst->points str)
  (let* ([s (string-split str "\n")]
         [inst-list (map (λ (x) (rest (regexp-match #px"([a-z])=(\\d+)" x))) s)])
    
    (define (pair->fold-line lst)
      (if (equal? (first lst) "x") 
          (fold-line (string->number (second lst)) 0)     ; vertical fold
          (fold-line 0 (string->number (second lst)))))   ; horiz fold
 
    (map (λ (p) (pair->fold-line p)) inst-list)))

;; OK let's parse some data!

;; Test Data
(define test-grid (make-grid test-points))
(define test-inst (inst->points test-instructions))

;; Input Data
(define input-grid (make-grid input-points))
(define input-inst (inst->points input-instructions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               UTILS                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Natural Natural Grid -> Natural
;; given a point and a grid, read the value at that point
(define (get-value p g)
  (vector-ref (grid-points g) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               MAIN                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now to explore what "folding" means to a grid. According to the text
;; the fold line "disappears." So a fold at the line from (0,7) to (10,7)
;; on a paper of the dimensions 11x15 (the provided test data) results
;; in a paper of 11x7 (sub1 (/ 15 2)). We don't have to worry about
;; the fold extending beyond the edges, at least in the test case, it's
;; always a fold in half on a odd number of lines, but that may be some-
;; thing to watch in part 2.
;;
;; Next what is the transformation of a folded point on the grid?
;; For a point (x y) and (fold-line v h) the transformation is
;;
;; If (zero? (fold-line-v inst)) then it's a Horizontal Fold so
;; (5 10) with a (0 7) fold becomes (5 4)
;; In other words (x y) becomes (x (- (fold-line-h inst) (- y (fold-line-h inst))
;;
;; So for a vector v folded horizontally at (fold-line v h) the point (x y) moves:
;; (vector-set! v (pos->point x (- h (- y h)))
;;             (or (vector-ref v (pos->point x y)) (pos->point x (- h (- y h)))))
;; 
;; After all adjustments are made we drop the bottom
;; half of the vector and adjust the new height to get
;; a new grid with the same width, height b, (vector-take vec b)).
;;
;; Vertical Fold: I don't need it in Part 1 but I might as well
;; make the folding function universal. For a point (x y) with
;; (fold-line a b), if (zero? fold-line-h inst) then
;; (x y) -> ((- (fold-line-v inst) (- x (fold-line-v inst))) y)
;; 
;; It's not as easy to drop the right half of a fold, however. Instead of
;; vector-take I'll have to do a vector-map with a lambda that chops off the
;; right hand side. Save it for part 2. 
;;
;; In both cases resulting dot is the OR of the original value and new
;; value (or (5 4) (5 10)).

;; Natural Fold-Line Grid -> Natural
;; Given a point and a fold-line on a grid
;; produce the mirror point on the grid
(define (mirror-point p f grid)
  (let ([x (car (point->pos p grid))]
        [y (cdr (point->pos p grid))])
    
    (if (zero? (fold-line-v f))      
        (pos->point x (- (fold-line-h f) (- y (fold-line-h f))) grid)    ; horizontal fold
        (pos->point (- (fold-line-v f) (- x (fold-line-v f))) y grid)))) ; vertical fold

(module+ test
  (check-equal? (mirror-point (pos->point 5 12 test-grid) (fold-line 0 7) test-grid)
                (pos->point 5 2 test-grid))
  (check-equal? (mirror-point (pos->point 10 5 test-grid) (fold-line 7 0) test-grid)
                (pos->point 4 5 test-grid)))

;; Grid Fold-Line -> Grid
;; Given a Grid and a fold-line, mirrors the
;; dots on the right or bottom side of the grid's
;; point vector to the left or top side. 
;; (depending on a vertical or horizontal fold-line)
;; Assumes the fold-line is in the middle of the vector
(define (mirror-points grid f)
  (if (zero? (fold-line-v f))
      "Make sure fold is in the middle"
      (cond [(not (equal? (fold-line-h f)
                          (quotient (grid-height grid) 2))) "Error: fold not in the middle"])
      (cond [(not (equal? (fold-line-v f)
                          (quotient (grid-width grid) 2))) "Error: fold not in the center"]))

  
 
  

;(define (day13.1 data) 0) ; stub
;
;(module+ test
; (check-equal? (day13.1 sample-data) 17))
;
;(time (printf "2021 AOC Problem 13.1 = ~a\n" (day13.1 day13data)))

;  
; 


;(define (day13.2 data) 0) ; stub
;
;(module+ test
;  (check-equal? (day13.2 sample-data) 0))
;
;(time (printf "2021 AOC Problem 13.2 = ~a\n" (day13.2 day13data)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

