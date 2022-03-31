#lang racket

#|
 AOC 2021
 Leo Laporte 8-Jan-2022
 
 --- Day 13: Transparent Origami ---
 
 How many dots are visible after completing just the first fold instruction
 on your transparent paper?
|#

(require threading
         rackunit
         racket/trace)

#|
 NOTES: This seems a conceptually simple problem. I'll use the Grid structure
 from Day 9. Looks like I'll have to calculate the height and width, and recalculate
 it after every fold. I'll represent a dot with 1 and 0 for no dot. I notice
 that the fold is always along the middle, I'm hoping Eric doesn't get too devilish
 in part 2, but I'll assume it's always the middle.  First, the data.
|#

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
         [vec (make-vector (* height width) #f)])                         ; make #f filled vector
    (for ([pos (in-list dots)])      
      (vector-set! vec pos #t))                                           ; set the dots
    (grid width height vec)))                                             ; build the grid

;; Now to parse the instructions. The text is something like:
;; "fold along y=7" - all I really care about is x or y for
;; vertical or horizontal fold and the number for the row or col.

(define (inst->points str)
  "given a text instruction parse it to a direction and fold-ppint"
  (let* ([s (string-split str "\n")]
         [inst-list (map (λ (x) (rest (regexp-match #px"([a-z]{1})=(\\d+)" x))) s)])
    
    (map (λ (p) (if (equal? (first p) "x")
                    (list '↓ (string->number (second p)))
                    (list '→ (string->number (second p))))) inst-list)))

(module+ test
  (check-equal? (inst->points "fold along y=7\nfold along x=5\n") (list (list '→ 7) (list '↓ 5))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               MAIN                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now to explore what "folding" means to a grid. According to the text
;; the fold line "disappears." So a fold at the line from (0,7) to (10,7)
;; on a paper of the dimensions 11x15 (the provided test data) results
;; in a paper of 11x7 (- 15 7 1)). We don't have to worry about
;; the fold extending beyond the edges, at least in the test case, it's
;; always a fold in half on a odd number of lines, but that may be some-
;; thing to watch in part 2. Folding vertically is a bit more
;; complicated but it just requires truncating all the lines in
;; the grid at the fold point.
;;
;; Next what is the transformation of a folded point on the grid?
;; For a point (x y) the transformation is
;;
;; If '→ then it's a Horizontal Fold so the mirror point of (x y)
;; is (x, (- (quotient grid-height 2) (- y (quotient grid-height 2))))
;; In other words (5,10) with a mid-line of 7 becomes (5, (- 7 (- 10 7))
;; or (5,4)
;; 
;; After all adjustments are made we drop the bottom
;; half of the vector and adjust the new height to get
;; a new grid with the same width and a height of (quotient grid-height 2)
;;
;; Vertical Fold: I don't need it in Part 1 but I might as well
;; make the folding function universal.
;; If '↓ then it's a Vertical Fold so the mirror point of (x y)
;; is ((- (quotient grid-height 2) (- x (quotient grid-height 2))) y)
;; In other words (8,6) with a center-line of 5 becomes ((- 5 (- 8 5))
;; or (2,6)
;; 
;; It's not as easy to drop the right half of a fold, however. Instead of
;; vector-take I'll have to do a loop that chops off the
;; right hand side. 
;;
;; In both cases the mirror dot is the OR of the original value and the
;; mirror dot. (If either is set the mirror is set, if both are empty,
;; then the mirror dot is empty.)

;;;;;;; ERROR!! That's not true. I was getting a low count.
;;;;;;; Turns out in Racket (or 0 1) is 0 not 1! I guess I'm mired
;;;;;;; in languages that define all non false values as true.
;;;;;;; I can fix this by
;;;;;;; using #t for a dot and #f for empty and counting #t vals
;;;;;;; in the vector but then ALL my tests break. Guess I should
;;;;;;; have used the constants in my tests. Lesson learned.
;;;;;;; SO this was loaded with tests, all of which passed, except
;;;;;;; for in the mirror functions that assumed (or 0 1) is 1.
;;;;;;; I've removed most of the tests rather than editing them for
;;;;;;; the new #t and #f constants.

;; So three functions, one to mirror a point on a grid
;; another to mirror all the points on a grid
;; a third to "fold" the grid by chopping it in half
;; and then we have to count the dots in the resulting
;; vector.
;;
;; The only complication is that the arithmetic changes
;; depending on whether it's a vertical or horizontal fold
;; so each function will have to offer both methods. I'll
;; break out the various methods to facilitate testing. 

;; Natural symbol Grid -> Natural
;; Given a point and a fold-line on a grid
;; produce the mirror point on the grid
(define (mirror-point pt inst grid)
  (let ([x (car (point->pos pt grid))]
        [y (cdr (point->pos pt grid))]
        [fp (second inst)])
    
    (if (equal? (first inst) '→)      
        (pos->point x (- fp (- y fp)) grid)    ; horizontal fold
        (pos->point (- fp (- x fp)) y grid)))) ; vertical fold

(module+ test
  (check-equal? (mirror-point (pos->point 5 12 test-grid) (list '→ 7) test-grid)
                (pos->point 5 2 test-grid))
  (check-equal? (mirror-point (pos->point 10 5 test-grid) (list '↓ 7) test-grid)
                (pos->point 4 5 test-grid)))

;; Grid Natural -> (vector-of Natural)
;; Given a grid and a horizontal fold line, mirror
;; all the points below the fold to above
;; the fold and return the resulting vector
(define (mirror-points-h g inst)
  (let* ([vec (grid-points g)]
         [fp (second inst)]
         [start (pos->point 0 (add1 fp) g)]                ; first point of bottom half
         [end (vector-length vec)])                        ; last point of bottom half
    
    (for ([bhp (in-range start end)])                      ; bottom half of grid
      (let ([mp (mirror-point bhp inst g)])                ; the mirror point of bp
        (vector-set! vec mp (or (vector-ref vec bhp)       ; set the mirror point
                                (vector-ref vec mp)))))
    vec))

;; Grid Natural -> (vector-of Natural)
;; Given a grid and a vertical fold line, mirror
;; all the points to the right of the fold to
;; the left side of the fold and rerturn the resulting vector
(define (mirror-points-v g inst)
  (let ([fp (second inst)]
        [w (grid-width g)]
        [h (grid-height g)]
        [vec (grid-points g)])
  
    (for ([y (in-range h)])                                  ; from top to bottom
      (for ([x (in-range fp w)])                             ; from center to right edge
        (let* ([rsp (pos->point x y g)]                      ; right side point
               [mp (mirror-point rsp inst g)])               ; its mirror point                 
          (vector-set! vec mp (or (vector-ref vec rsp)       ; set the mirror point
                                  (vector-ref vec mp))))))
    vec))

;; Grid Instruction -> Grid
;; given a grid and a fold instruction
;; mirror points on one half of the fold
;; to the other half
(define (mirror-points g inst)
  (grid (grid-width g) (grid-height g)
        (cond [(equal? (first inst) '→)                                 ; it's a horizontal fold
               (mirror-points-h g inst)]
              [else                                                     ; it's a vertical fold
               (mirror-points-v g inst)])))

;; Grid Inst -> Grid
;; given a grid and an instruction chops off the
;; area below or to the right of the fold line
(define (fold-grid g inst)
  (let* ([vec (grid-points g)]     ; the vector to fold
         [w (grid-width g)]        ; pre-fold width
         [h (grid-height g)]       ; pre-fold height
         [fl (second inst)])       ; fold line
    
    (cond [(equal? (first inst) '→)                    ; it's a horizontal fold
           (grid w fl (vector-take vec (* w fl)))]     ; chop off bottom half

          [else                                        ; it's a vertical fold
           (grid fl h (list->vector                    ; is there a more direct way to do this?? TK
                       (apply append (for/list ([y (in-range 0 h)])
                                       ; go from top to bottom of grid and copy the left half 
                                       (vector->list (vector-copy vec (* y w) (+ (* y w) fl)))))))])))

(module+ test
  (check-equal? (fold-grid (grid 3 3 (list->vector '(1 2 3 4 5 6 7 8 9))) (list '↓ 1))
                (grid 1 3 (list->vector '(1 4 7))))
  
  (check-equal? (fold-grid (grid 3 3 (list->vector '(1 2 3 4 5 6 7 8 9))) (list '→ 1))
                (grid 3 1 (list->vector '(1 2 3)))))

(define (day13.1 a-grid inst)
  (~> a-grid                                           ; given a grid and a fold-line
      (mirror-points _ inst)                           ; mirror all the points
      (fold-grid _ inst)                               ; make the fold
      (grid-points _)                                  ; take the resulting vector
      (vector-count (λ (val) (not (false? val))) _)))  ; and count the dots

(module+ test
  (check-equal? (day13.1 test-grid (first test-inst)) 17))

(time (printf "2021 AOC Problem 13.1 = ~a\n" (day13.1 input-grid (first input-inst))))

;;; --- Part Two ---
;;;
;;; Finish folding the transparent paper according to the instructions.
;;; The manual says the code is always eight capital letters.
;;;
;;; What code do you use to activate the infrared thermal imaging camera system?

;;; NOTES: I've already written the code to finish the folds, but do I REALLY
;;; have to write an OCR to recognize the letters? Can I do that part by hand?
;;; I guess I'll have to write code to display the result?

(define (fold-paper a-grid inst-list)
  (for/fold ([g a-grid]
             #:result g)
            ([inst (in-list inst-list)])
    (values
     (fold-grid (mirror-points g inst) inst))))

;; Grid -> ASCII Text
;; given a grid, display it
(define (display-grid g)
  (for ([y (in-range (grid-height g))])
    (for ([x (in-range (grid-width g))])
      (if (false? (vector-ref (grid-points g) (pos->point x y g)))
          (display #\ )
          (display #\█)))
    (display "\n")))

(define (day13.2 g i)
  (display-grid (fold-paper g i)))

(day13.2 test-grid test-inst)

(time (printf "2021 AOC Problem 13.2 = ~a\n" (day13.2 input-grid input-inst)))

#|
 Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
2021 AOC Problem 13.1 = 755
cpu time: 443 real time: 451 gc time: 97

█████
█   █
█   █
█   █
█████
     
     
███  █    █  █   ██ ███  ███   ██   ██  
█  █ █    █ █     █ █  █ █  █ █  █ █  █ 
███  █    ██      █ █  █ ███  █  █ █    
█  █ █    █ █     █ ███  █  █ ████ █ ██ 
█  █ █    █ █  █  █ █ █  █  █ █  █ █  █ 
███  ████ █  █  ██  █  █ ███  █  █  ███

2021 AOC Problem 13.2 = #<void>
cpu time: 784 real time: 793 gc time: 175


2022 Mac Studio Max with 32GB RAM

2021 AOC Problem 13.1 = 755
cpu time: 260 real time: 268 gc time: 84
2021 AOC Problem 13.2 = #<void>
cpu time: 380 real time: 394 gc time: 77

|#
