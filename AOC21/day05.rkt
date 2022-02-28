#lang racket
 #|

  AOC 2021
  Leo Laporte
  
  --- Day 5: Hydrothermal Venture ---
    
  Consider only horizontal and vertical lines. At how many points do at least two lines overlap?

|#

(require threading
         rackunit)

;; Problem input from adventofcode.com
(define day5data (file->string "input5.txt"))

(define input-points (~> day5data ; turn the given data into a list of lines (x1 y1 x2 y2)
                         (string-split _ "\n")
                         (map
                          (λ (x) (regexp-match #px"(\\d+),(\\d+).+ (\\d+),(\\d+)" x)) _)
                         (map (λ (x) (rest x)) _)
                         (map (λ (x) (map string->number x)) _)))

;; Provided test data (in same format as above)
(define sample-points (list '(0 9 5 9) '(8 0 0 8) '(9 4 3 4) '(2 2 2 1) '(7 0 7 4)
                            '(6 4 2 0) '(0 9 2 9) '(3 4 1 4) '(0 0 8 8) '(5 5 8 2)))

#|
 NOTES
 This looks pretty easy. Too easy...
 Convert the input to a list of lines, each line stored as four points (x1 y1 x2 y2)
 Convert each line into the set of points it touches: ((x1,y1)..(x2,y2))
 for each point in the set increment that point's entry in a hash table, if a point
 is not in the hash add it, count total number of points with more than one entry

 Oh and it WAS too easy... there are two additional tricky points
 1) the provided end points can be right->left as well as left->right, and bottom->top
 as well as top->bottom (make sure to account for those)
 2) some of the provided lines might be diagonals(!) and for part 1 we needn't consider
 them.

 Just in case we need to pay attention to diagonals in part two, I need to write the
 point generator to handle both these cases. For now, I can eliminate the diagonals.

 Update: re-wrote the point generator, but it's ugly. I'm sure there's a better way
 to do it with Racket iterators. I'll investigate some day.

|#

(define (count-multipoint-entries hsh)
  (length (filter (λ (x) (> x 1)) (hash-values hsh))))

(define (make-point-hash hsh all-points)
  (cond [(empty? all-points) hsh]
        [else
         (increment-points hsh (make-point-list (first all-points)))
         (make-point-hash hsh (rest all-points))]))

(define (make-point-list end-points)
  (let* ([x1 (first end-points)]
        [y1 (second end-points)]
        [x2 (third end-points)]
        [y2 (fourth end-points)]
        [step-x (if (> x1 x2) -1 1)]  ; handle right->left 
        [step-y (if (> y1 y2) -1 1)]) ; and bottom->top lines
    
    (if (straight-line? end-points)
        (for*/list ([x (in-inclusive-range x1 x2 step-x)]
                    [y (in-inclusive-range y1 y2 step-y)])
          (cons x y))
        (for/list ([x (in-inclusive-range x1 x2 step-x)]
                   [y (in-inclusive-range y1 y2 step-y)])
          (cons x y)))))
      
(module+ test
  (check-equal? (make-point-list '(0 9 5 9))  ; horizontal
                (list '(0 . 9) '(1 . 9) '(2 . 9) '(3 . 9) '(4 . 9) '(5 . 9)))
  (check-equal? (make-point-list '(7 0 7 4))  ; vertical 
                (list '(7 . 0) '(7 . 1) '(7 . 2) '(7 . 3) '(7 . 4)))
  (check-equal? (make-point-list '(9 7 7 7))  ; right -> left
                '((9 . 7) (8 . 7) (7 . 7)))
  (check-equal? (make-point-list '(0 5 0 0))  ; bottom -> top
                 '((0 . 5) (0 . 4) (0 . 3) (0 . 2) (0 . 1) (0 . 0)))
  (check-equal? (make-point-list '(4 4 8 8))  ; diagonal left->right
                '((4 . 4) (5 . 5) (6 . 6) (7 . 7) (8 . 8)))
  (check-equal? (make-point-list '(8 0 0 8))  ; diagonal right->left
                 '((8 . 0) (7 . 1) (6 . 2) (5 . 3) (4 . 4) (3 . 5) (2 . 6) (1 . 7) (0 . 8))))

(define (increment-points hsh point-list)
  (cond [(equal? empty point-list) hsh]
        [else (increment-point hsh (first point-list))
              (increment-points hsh (rest point-list))]))

(define (increment-point hsh pt)
  (if (hash-has-key? hsh pt)
      (hash-update! hsh pt add1)      
      (hash-update! hsh pt add1 0)))

;; (list-of Natural) -> boolean
;; given four points representing the beginning and end of a line
;; return true if that line is horizontal or vertical (i.e. not diagonal)
(define (straight-line? points)
  (let ([x1 (first points)]
        [y1 (second points)]
        [x2 (third points)]
        [y2 (fourth points)])
    (or (= x1 x2) (= y1 y2))))

(module+ test
  (check-equal? (straight-line? '(8 0 0 8)) #f)
  (check-equal? (straight-line? '(0 9 5 9)) #t))

(define (day5.1 input)
  (~> input
      (filter straight-line? _) ; only count straight lines
      (make-point-hash (make-hash) _)
      (count-multipoint-entries _)))

(module+ test
  (check-equal? (day5.1 sample-points) 5))

(time (printf "2021 AOC Problem 5.1 = ~a\n" (day5.1 input-points)))

#|

   --- Part Two ---
  
  Unfortunately, considering only horizontal and vertical lines doesn't give you the full
  picture; you need to also consider diagonal lines.
    
  Consider all of the lines. At how many points do at least two lines overlap?
  
|#

;; NOTES: I KNEW it!! Rewriting make-point-list to include 45 degree diagonal lines.

(define (day5.2 input)
   (~> input
     ; (filter straight-line? _) ; we need to count diagonals this time
      (make-point-hash (make-hash) _)
      (count-multipoint-entries _)))

(module+ test
  (check-equal? (day5.2 sample-points) 12))

(time (printf "2021 AOC Problem 5.2 = ~a\n" (day5.2 input-points)))

#|

 Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

2021 AOC Problem 5.1 = 7468
cpu time: 112 real time: 114 gc time: 29
2021 AOC Problem 5.2 = 22364
cpu time: 207 real time: 212 gc time: 38

 Real world timing

      --------Part 1---------   --------Part 2--------
Day       Time    Rank  Score       Time   Rank  Score
  5       >24h   71260      0       >24h  68517      0
  4       >24h   77972      0       >24h  75701      0
  3       >24h  102352      0       >24h  88448      0
  2   00:38:41   13051      0   01:06:26  14647      0
  1   00:24:22    7349      0   12:27:40  59726      0

|#
