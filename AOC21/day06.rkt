#lang racket

#|
   AOC 2021
  Leo Laporte 9-Dec-21
  
  --- Day 6: Lanternfish ---
  
Each lanternfish creates a new lanternfish once every 7 days.
  
Furthermore a new lanternfish waits two more days for its first cycle.

Each day, a 0 becomes a 6 and adds a new 8 to the end of the list, while
each other number decreases by 1 if it was present at the start of the day.

Find a way to simulate lanternfish. How many lanternfish would there be after 80 days?

|#

;; NOTES:
;; the number of fish grows so large I can't store them in memory
;; so clearly I'm going to have to compress the results. Ah! That's what run-length
;; encoding is for. And by replacing that massive and exponentially larger (list-of fish)
;; with a vector of length 9 representing the number of fish at each day of maturity
;; (0-8) I can massively reduce the compute time and memory requirements.

(require threading
         rackunit)

;; (list-of Natural) -> Vector
;; given a list of single digit numbers return a vector with the counts for each
;; (use Run Length Encoding to compress a vast number of fish into just counts of
;; fish at each stage 0-9)
(define (rle list)
  (for/vector ([i (in-range 0 10)])
    (count (λ (x) (equal? i x)) list)))

;; Problem input from adventofcode.com
(define input (~> (file->string "input6.txt")  ; read in raw data
                  (string-split _ "\n")     ; split off pesky little \n at end
                  (first _)                 ; drop \n
                  (string-split _ ",")      ; chop it up into 300 strings
                  (map string->number _)    ; turn each string into a number
                  (rle _)))                 ; run-length encoding turns it into a vector 
                   
;; Provided test data
(define sample-data (rle '(3 4 3 1 2))) 

;; Vector Natural -> Natural
;; Given a vector representing the current fish population and the number of days
;; to breed, return the new number of fish
(define (breed-lanternfish fish days)
  (for ([d (in-range 0 days)])
    (set! fish (next-generation fish)))
  (apply + (rest (reverse (vector->list fish))))) ; drop the 9 position before adding

;; Vector -> Vector
;; given a vector of fish counts, return a vector aged by one day
(define (next-generation fish)
  (let* ([f (vector-rotate-left fish)]   ; subtract a day - vector-ref 9 is unused
         [new-fishies (vector-ref f 9)]) ; (vector 9) is unused, stores # of new fish
    (vector-set! f 6 (+ new-fishies (vector-ref f 6))) ; mother fish reset
    (vector-set! f 8 new-fishies)                      ; add newborns
    f))            ; return updated vector

;; Vector -> Vector
;; rotates a vector left, moving 0 position to 9
(define (vector-rotate-left v)
  (local [(define (rotate-left l)
            (append (rest l) (cons (first l) empty)))]
    (list->vector (rotate-left (vector->list v)))))

(module+ test
  (check-equal? (vector-rotate-left (list->vector '(0 1 2 3 4 5 6 7 8 9)))
                (list->vector '(1 2 3 4 5 6 7 8 9 0))))

(module+ test
  (check-equal? (breed-lanternfish sample-data 18) 26)
  (check-equal? (breed-lanternfish sample-data 80) 5934))

(time (printf "2021 AOC Problem 6.1 = ~a\n" (breed-lanternfish input 80)))

#|
   --- Part Two ---

  How many lanternfish would there be after 256 days? 

|# 

(module+ test
  (check-equal? (breed-lanternfish sample-data 256) 26984457539))

(time (printf "2021 AOC Problem 6.2 = ~a\n" (breed-lanternfish input 256)))

#|
 Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

2021 AOC Problem 6.1 = 350917
cpu time: 1 real time: 1 gc time: 0 (wow! a LOT faster using the RLE encoded population)
2021 AOC Problem 6.2 = 1592918715629
cpu time: 0 real time: 0 gc time: 0

 Real world timing
      --------Part 1---------   --------Part 2--------
Day       Time    Rank  Score       Time   Rank  Score
  6       >24h   71604      0       >24h  67093      0
  5       >24h   71260      0       >24h  68517      0
  4       >24h   77972      0       >24h  75701      0
  3       >24h  102352      0       >24h  88448      0
  2   00:38:41   13051      0   01:06:26  14647      0
  1   00:24:22    7349      0   12:27:40  59726      0

|#