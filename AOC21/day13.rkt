#lang racket
;  AOC 2021
; Leo Laporte 8-Jan-2022
; 
; --- Day 13: Transparent Origami ---
; 
; The first section is a list of dots on the transparent paper. 0,0 represents
; the top-left coordinate. The first value, x, increases to the right. The
; second value, y, increases downward. So, the coordinate 3,0 is to the right
; of 0,0, and the coordinate 0,7 is below 0,0. The coordinates in this example
; form the following pattern, where # is a dot on the paper and . is an empty,
; unmarked position:
; 
; Then, there is a list of fold instructions. Each instruction indicates a line
; on the transparent paper and wants you to fold the paper up (for horizontal y=... lines)
; or left (for vertical x=... lines). In this example, the first fold instruction
; is fold along y=7, which designates the line formed by all of the positions w
; here y is 7 (marked here with -):
; 
; Because this is a horizontal line, fold the bottom half up. Some of the dots
; might end up overlapping after the fold is complete, but dots will never appear
; exactly on a fold line.
; 
; Now, only 17 dots are visible.
; 
; Notice, for example, the two dots in the bottom left corner before the
; transparent paper is folded; after the fold is complete, those dots appear
; in the top left corner (at 0,0 and 0,1). Because the paper is transparent,
; the dot just below them in the result (at 0,3) remains visible, as it can
; be seen through the transparent paper.
; 
; Also notice that some dots can end up overlapping; in this case, the dots
; merge together and become a single dot.
; 
; The transparent paper is pretty big, so for now, focus on just completing
; the first fold. After the first fold in the example above, 17 dots are
; visible - dots that end up overlapping after the fold is completed count
; as a single dot.
; 
; How many dots are visible after completing just the first fold instruction
; on your transparent paper?
; 


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

;; Problem input from adventofcode.com
(define day13data (file->string "input13.txt"))
(define test-data
"6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\n\nfold along y=7\nfold along x=5\n")

;; Split the input into two values, the points and the instructions
(define input-str (string-split day13data "\n\n"))
(define-values (input-points input-instructions) (values (first input-str) (second input-str)))

(define test-str (string-split test-data "\n\n"))
(define-values (test-points test-instructions) (values (first test-str) (second test-str)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               MAIN                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (day13.1 data) 0) ; stub
;
;(module+ test
; (check-equal? (day13.1 sample-data) 0))
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

