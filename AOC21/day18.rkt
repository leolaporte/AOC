#lang racket

;AOC Day 18
;Leo Laporte 12-Fen-2022

#|==============================================================================

                              --- Day 18: Snailfish ---

Add up all of the snailfish numbers from the homework assignment in the order
 they appear. What is the magnitude of the final sum?
            
================================================================================|#

(require rackunit
         threading
         racket/trace)

#|==============================================================================|#
#|                                      DATA                                    |#
#|==============================================================================|#

; since we're working with strings a simple string-split on \n will do

#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#

#|
Initially it looks so much like nested lists my inclination is to make a tree
out of the data. But I think I can do everything by manipulating strings. Why
overcomplicate?
|#

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

;; String String -> String
;; combines two strings using snailfish math
(define (sn-add str1 str2)
  (string-append "[" str1 "," str2 "]"))

(module+ test
  (check-equal? (sn-add "[1,2]" "[[3,4],5]") "[[1,2],[[3,4],5]]"))

#|
To explode a pair, the pair's left value is added to the first regular
number to the left of the exploding pair (if any), and the pair's right
value is added to the first regular number to the right of the exploding
 pair (if any). Exploding pairs will always consist of two regular numbers.
Then, the entire exploding pair is replaced with the regular number 0.
|#

; String -> String
; explodes a string according to the rules
(define (sn-explode str) "") ;stub

(module+ test
  (check-equal? (sn-explode "[[[[[9,8],1],2],3],4]") "[[[[0,9],2],3],4]")
  (check-equal? (sn-explode "[7,[6,[5,[4,[3,2]]]]]") "[7,[6,[5,[7,0]]]]")
  (check-equal? (sn-explode "[[6,[5,[4,[3,2]]]],1]") "[[6,[5,[7,0]]],3]")
  (check-equal? (sn-explode "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
  (check-equal? (sn-explode "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]") "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"))

#|
To split a regular number, replace it with a pair; the left element of the pair
should be the regular number divided by two and rounded down, while the right
element of the pair should be the regular number divided by two and rounded up.
For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [6,6], and so on.
|#

; String -> String
; splits a string according to the rules
(define (sn-split str) "") ;stub

(module+ test
  (check-equal? (sn-split "[[[[0,7],4],[15,[0,13]]],[1,1]]") "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
  (check-equal? (sn-split "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]") "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")
  (check-equal? (sn-split "") "")
  (check-equal? (sn-split "") ""))

#|
The magnitude of a pair is 3 times the magnitude of its left element
plus 2 times the magnitude of its right element. The magnitude of a
regular number is just that number
|#

; String -> Natural
; calculates the magnitude of a string according to the rules
(define (sn-magnitude str) 0) ;stub

(module+ test
  (check-equal? (sn-magnitude "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]") 4140)
  (check-equal? (sn-magnitude "") ""))


; (time (printf "2021 AOC Problem 18.1 = ~a\n" (day18.1 input)))

#|=================================================================================
                                        PART 2
                               

==================================================================================|#

;(module+ test
;  (check-equal? (day18.2 test-data) 0))

; (time (printf "2021 AOC Problem 18.2 = ~a\n" (day18.2 input)))

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM


|#