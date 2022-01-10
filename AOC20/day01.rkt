#lang racket

;  --- Day 1: Report Repair ---
; 
; Before you leave, the Elves in accounting just need you to fix your expense
; report (your puzzle input); apparently, something isn't quite adding up.
; 
; Specifically, they need you to find the two entries that sum to 2020 and then
; multiply those two numbers together.
; 
; For example, suppose your expense report contained the following:
; 
; 1721
; 979
; 366
; 299
; 675
; 1456
; 
; In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
; them together produces 1721 * 299 = 514579, so the correct answer is 514579.
; 
; Of course, your expense report is much larger. Find the two entries that sum to
; 2020; what do you get if you multiply them together?
; 
; 


;; provided by AOC:
(define list-of-entries (file->list "input1.txt"))

(define LOE1 (list 1721 979 366 299 675 1456)) ; example given
(define LOE2 (list 1 2 3 2010 5 5 10)) 

;; Integer (list-of Integer) -> (list-of Integer)
;; produces the first pair of Integers in a list of Integers that add up to the base value
;; if no such pair exists, return #false

(define (find-pair base loe)
  (cond [(empty? loe) #false]
        [else
         (local [(define testnum (- base (first loe)))
                 (define testres (member testnum (rest loe)))]
           (if testres
               (list (first loe) (first testres))
               (find-pair base (rest loe))))]))
        
(module+ test
  (require rackunit)
  (check-equal? (find-pair 2020 LOE1) (list 1721 299))
  (check-equal? (find-pair 2020 LOE2) (list 2010 10)))

(define pair-of-entries (find-pair 2020 list-of-entries))
(printf "AOC Problem 1.1 = ~a\n"
        (* (first pair-of-entries) (second pair-of-entries))) 
        

; The Elves in accounting are thankful for your help; one of them even offers you
; a starfish coin they had left over from a past vacation. They offer you a second
; one if you can find three numbers in your expense report that meet the same criteria.
; 
; Using the above example again, the three entries that sum to 2020 are 979, 366,
; and 675. Multiplying them together produces the answer, 241861950.
; 
; In your expense report, what is the product of the three entries that sum to 2020? 


;; Integer (list-of Integer) -> (list-of Integer)
;; Given a list of Integers, produces the first triplet that adds up to the base

(define (find-triplet base loe)
  (cond [(empty? loe) #false]
        [else
         (local [(define newbase (- base (first loe)))
                 (define testres (find-pair newbase (rest loe)))]
           (if testres
               (list (first loe) (first testres) (second testres))
               (find-triplet base (rest loe))))]))

(module+ test
  (check-equal? (find-triplet 2020 LOE1) (list 979 366 675))
  (check-equal? (find-triplet 2020 LOE2) (list 2010 5 5)))

(define triplets (find-triplet 2020 list-of-entries))
(printf "AOC Problem 1.2 = ~a\n"
        (* (first triplets) (second triplets) (third triplets))) 