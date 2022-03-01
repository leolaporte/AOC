#lang racket

#|
  AOC 2021
 Leo Laporte 10-Dec-2021
 
 --- Day 7: The Treachery of Whales ---
 
 You quickly make a list of the horizontal position of each crab (your puzzle input).
 Crab submarines have limited fuel, so you need to find a way to make all of their
 horizontal positions match while requiring them to spend as little fuel as possible.
 
 Determine the horizontal position that the crabs can align to using the least fuel
 possible. How much fuel must they spend to align to that position?
 
|#

(require racket/file threading rackunit)

;; Convert problem input from adventofcode.com into (list-of Natural)
(define input (~> (file->string "input7.txt")  ; read in raw data
                  (string-split _ "\n")        ; split off pesky little \n at end
                  (first _)                    ; drop \n
                  (string-split _ ",")         ; chop it up into 300 strings
                  (map string->number _)))     ; turn each string into a number

;; NOTES
;; This feels like one of those problems that has a simple solution based on some
;; mathematical theorem. But, not knowing that theorem, here's the obvious/naive
;; brute force solution...
;; 0. Establish range of possible solutions (min input)->(max input)
;; 1. For each crab in the list, calculate the distance to every point from min to max
;; (each crab will have a vector[min..max] with the distances)
;; 2. total the numbers in each position of the crab vectors, lowest number is winner
;; So we'll have a two-dimensional array of vectors with a y-vec for each crab, x-vec
;; for each horizontal position.
;;
;; INSIGHT!
;; Come to think of it,  we don't actually have to create a vector for each crab
;; just total the movement to each point and save it as we go. (Kind of like day 6)
;; So only one vector - containing the total movements for each point on the line. 
;; Wow. Worked first time. Amazing.

;; Provided test data
(define sample-data '(16 1 2 0 4 2 7 1 2 14)) 

(define (day7.1 crabs)
  (for/fold ([fuel-required (make-vector (apply max crabs) 0)] ; accumulator vector
             #:result (apply min (vector->list fuel-required))) ; final result..
            ([crab (in-list crabs)])      ;  go through all the provided positions
    (values (add-distances crab fuel-required)))) ; and add total fuel consumption to vect

;; Natural (vector-of Natural) -> (vector-of Natural)
;; add the relative distance from pos to each point on the vector to the existing
;; points on the vector
(define (add-distances crab positions)
  (for/vector #:length (vector-length positions) 
    ([point (in-naturals)]) ; keep going until we run out of vector
    ; add the fuel consumption needed to reach each position
    (+ (vector-ref positions point) (abs (- crab point))))) 
    
(module+ test
  (check-equal? (day7.1 sample-data) 37))

(time (printf "2021 AOC Problem 7.1 = ~a\n" (day7.1 input)))

;  --- Part Two ---
; 
; The crabs don't seem interested in your proposed solution. Perhaps you misunderstand
; crab engineering?
; 
; As it turns out, crab submarine engines don't burn fuel at a constant rate. Instead,
; each change of 1 step in horizontal position costs 1 more unit of fuel than the last:
; the first step costs 1, the second step costs 2, the third step costs 3, and so on.
; 
; As each crab moves, moving further becomes more expensive. This changes the best
; horizontal position to align them all on; in the example above, this becomes 5:
; 
; Move from 16 to 5: 66 fuel
; Move from 1 to 5: 10 fuel
; Move from 2 to 5: 6 fuel
; Move from 0 to 5: 15 fuel
; Move from 4 to 5: 1 fuel
; Move from 2 to 5: 6 fuel
; Move from 7 to 5: 3 fuel
; Move from 1 to 5: 10 fuel
; Move from 2 to 5: 6 fuel
; Move from 14 to 5: 45 fuel
; This costs a total of 168 fuel. This is the new cheapest possible outcome;
; the old alignment position (2) now costs 206 fuel instead.
; 
; Determine the horizontal position that the crabs can align to using the least
; fuel possible so they can make you an escape route! How much fuel must they spend to align to that position? 
; 


;; Only change required here is the calculation of the fuel requirement
;; here's the rewrite:

(define (day7.2 crabs) ; same as above
  (for/fold ([fuel-required (make-vector (apply max crabs) 0)] ; accumulator vector
             #:result (apply min (vector->list fuel-required))) ; final result..
            ([crab (in-list crabs)])      ;  go through all the provided positions
    (values (new-add-distances crab fuel-required)))) ; and add total fuel consumption 

;; Natural (vector-of Natural) -> (vector-of Natural)
;; add the relative distance from pos to each point on the vector to the existing
;; points on the vector
(define (new-add-distances crab positions)
  (for/vector #:length (vector-length positions) 
    ([point (in-naturals)]) ; keep going until we run out of vector
    ; add the fuel consumption needed to reach each position
    (+ (vector-ref positions point) (fuel-use crab point))))

(define (fuel-use a b)
  "fuel use from point a to point b"
  (let ([dist (abs (- a b))])
    (for/sum ([i (in-range 0 dist)])
      (add1 i))))

(module+ test
  (check-equal? (day7.2 sample-data) 168))

(time (printf "2021 AOC Problem 7.2 = ~a\n" (day7.2 input)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
;2021 AOC Problem 7.1 = 347011
;cpu time: 59 real time: 66 gc time: 7
;2021 AOC Problem 7.2 = 98363777
;cpu time: 7474 real time: 7965 gc time: 521

; Real world timing
;      --------Part 1---------   --------Part 2--------
;Day       Time    Rank  Score       Time   Rank  Score
;  7       >24h   68317      0       >24h  66595      0
;  6       >24h   71604      0       >24h  67093      0
;  5       >24h   71260      0       >24h  68517      0
;  4       >24h   77972      0       >24h  75701      0
;  3       >24h  102352      0       >24h  88448      0
;  2   00:38:41   13051      0   01:06:26  14647      0
;  1   00:24:22    7349      0   12:27:40  59726      0