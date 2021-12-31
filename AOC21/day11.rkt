#lang racket
;  AOC 2021
; Leo Laporte 29 Dec 2021
; 
; --- Day 11: Dumbo Octopus ---
; 
; Instructions are very lengthy - I'll just summarize
; 
; ...
; 
; Given the starting energy levels of the dumbo octopuses in your cavern,
; simulate 100 steps. How many total flashes are there after 100 steps?
; 


(require threading
         rackunit)

;; NOTES: Good spec challenge here: 90% of the work was interpreting the problem
;; description. There was enough ambiguity in the prose description that
;; I had to test my hypotheses against the sample data. So to start, let's
;; get that data massaged into a vector. Each point on the vector is
;; the state of an octopus. (I've been using 1D vectors to represent
;; 2D grids for efficiency.)

;; Given dimension constants
(define HEIGHT 10)
(define WIDTH 10)
(define LENGTH 100)

;; Parse provided AOC Problem data into a vector of all points
(define (octopus str)
  "given an \n separated string of single digits return a vector of the digits as Natural"
  (~> str 
      (string-replace _ "\n" "")       ; drop the \n
      (regexp-match* #px"(\\d{1})" _ ) ; create a list of digit strings
      (map string->number _)           ; turn the strings into Natural numbers
      (list->vector _)))      ; convert the resulting list into a vector for easy addressing

;; Create octopus vectors
(define test-data
  (octopus "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"))

(define problem-data
  (octopus (file->string "input11.txt")))

;; First thing to do, modify Day 9's surrounding routine to add
;; diagonals.

;; Natural -> (list-of Natural)
;; given a point on a grid vector, return a list of the 
;; eight surrounding vector refs including diagonals
(define (surrounding p)
  ; I prefer to store 2D grids as a vector of points
  ; so we'll need some utilities to convert back and forth
  ; from point to x,y position:
  (define (pos->point x y)
    "convert x y coords to vector point"
    (+ x (* y WIDTH)))
  
  (define (point->pos p)
    "convert vector point to x.y coords"
    (let-values ([(y x) (quotient/remainder p WIDTH)])
      (cons x y)))

  (define (in-grid? xy)
    "using x.y coords are we still inside the grid?"
    (and (< -1 (car xy) WIDTH) (< -1 (cdr xy) HEIGHT)))

  ; calculate all the surrounding points 
  (let* ([pos (point->pos p)]
         [x (car pos)]                    ; convert vector-ref...
         [y (cdr pos)]                    ; to x,y coordinates (simplifies in-grid test)
         [w (cons (sub1 x) y)]            ; west
         [e (cons (add1 x) y)]            ; east
         [n (cons x (sub1 y))]            ; north
         [s (cons x (add1 y))]            ; south 
         [nw (cons (sub1 x) (sub1 y))]    ; northwest
         [ne (cons (add1 x) (sub1 y))]    ; northeast
         [sw (cons (sub1 x) (add1 y))]    ; southwest
         [se (cons (add1 x) (add1 y))])   ; southeast

    (~>  (list nw n ne w e sw s se)   ; the list of points as (x . y)
         (filter in-grid? _)          ; remove points that are off the grid
         (map (λ (x) (pos->point (car x) (cdr x))) _)))) ; convert (x . y) back to vector-ref

(module+ test
  (check-equal? (surrounding 0) (list 1 10 11))
  (check-equal? (surrounding 9) (list 8 18 19))
  (check-equal? (surrounding 34) (list 23 24 25 33 35 43 44 45))
  (check-equal? (surrounding 90) (list 80 81 91))
  (check-equal? (surrounding 99) (list 88 89 98)))

;; The rules seem ambiguous - but after close examination of the examples
;; and some testing I've come up with this:
;;
;; Repeat 100 times:
;; 1. Add1 to all points 
;; 2. Start at Vec[0].
;;    IF point value is >9 set to 0 and ADD1 to all SURROUNDS > 0. Go to 2.
;;    ELSE go to next point in vector and scan again
;;    When you reach end of vector you're done.
;; 3. Count 0s in Vector, add count to flash-count. Go to 1
;; 

;; (Vector-of Natural) -> (Vector-of Natural)
;; Given a grid of octopi return grid after all flashers are set-off
(define (scan p v)
  (cond [(= p LENGTH) v]            ; at end of vector, all done
        
        [(> (vector-ref v p) 9)     ; flasher!
         (vector-set! v p 0)        ; set it to 0 and ... 
         (for ([pt (in-list (surrounding p))]) ; ... increment surrounding points but ...
           (let ([val (vector-ref v pt)]) 
             (cond [(> val 0)  (vector-set! v pt (add1 val))]))) ; .. only if it hasn't yet flashed, then ...
         (scan 0 v)] ; ... restart scan
        
        [else (scan (add1 p) v)]))  ; not flasher so go to next point

(module+ test 
  (check-equal? (scan 0 (vector-map add1 test-data))
                (octopus "6594254334\n3856965822\n6375667284\n7252447257\n7468496589\n5278635756\n3287952832\n7993992245\n5957959665\n6394862637")))
 
; do 10 steps (with data and result from problem description) - I really want to test this method!
(check-equal? (~> (octopus "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526\n")
                  (vector-map add1 _)
                  (scan 0 _)           ; step 1
                  (vector-map add1 _)
                  (scan 0 _)           ; step 2
                  (vector-map add1 _)
                  (scan 0 _)           ; step 3
                  (vector-map add1 _)
                  (scan 0 _)           ; step 4
                  (vector-map add1 _)
                  (scan 0 _)           ; step 5
                  (vector-map add1 _)
                  (scan 0 _)           ; step 6
                  (vector-map add1 _)
                  (scan 0 _)           ; step 7
                  (vector-map add1 _)
                  (scan 0 _)           ; step 8
                  (vector-map add1 _)
                  (scan 0 _)           ; step 9
                  (vector-map add1 _)
                  (scan 0 _))           ; step 10
              (octopus "0481112976\n0031112009\n0041112504\n0081111406\n0099111306\n0093511233\n0442361130\n5532252350\n0532250600\n0032240000\n"))
  
;; (vector-of Natural) -> Natural
;; given a vector of octopi states, advance iters steps, returns
;; number of flashes that were triggered
(define (day11.1 octopi iters)
  (for/fold ([vec octopi]
             [flashes 0]
             #:result flashes)
            
            ([i (in-range (add1 iters))])
    
    (values  (scan 0 (vector-map add1 vec))
             (+ flashes (vector-count zero? vec)))))

(module+ test
  (check-equal? (day11.1 test-data 10) 204)
  (check-equal? (day11.1 test-data 100) 1656))

(time (printf "2021 AOC Problem 11.1 = ~a\n" (day11.1 problem-data 100)))

;  --- Part Two ---
; 
; It seems like the individual flashes aren't bright enough to navigate. However, you might
; have a better option: the flashes seem to be synchronizing!
; 
; If you can calculate the exact moments when the octopuses will all flash simultaneously,
; you should be able to navigate through the cavern. What is the first step during which
; all octopuses flash? 
; 


;; NOTES: Simple enough. Just break when flashers=100. I can reuse the for/fold above
;; but instead of returning flashes I'll return the number of iterations

;; (vector-of Natural) -> Natural
;; given a vector of octopi states, advance until all octopi flash at once
(define (day11.2 octopi)
  (for/fold ([vec octopi]
             [flashes 0]
             [iterations 0]
             #:result iterations)
            
            ([i (in-naturals)]) ; iterate forever
    #:break (= flashes 100)     ; until we have 100 flashers
    (values  (scan 0 (vector-map add1 vec))
             (vector-count zero? vec)
             i)))

(module+ test
  (check-equal? (day11.2 test-data) 195))

(time (printf "2021 AOC Problem 11.2 = ~a\n" (day11.2 problem-data)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
; 2021 AOC Problem 11.1 = 1719
; cpu time: 11 real time: 10 gc time: 3
; 2021 AOC Problem 11.2 = 232
; cpu time: 16 real time: 16 gc time: 1

