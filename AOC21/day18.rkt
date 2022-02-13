#lang racket

;AOC Day 18
;Leo Laporte 12-Fen-2022

#|==============================================================================

                              --- Day 18: Snailfish ---

To reduce a snailfish number, you must repeatedly do the first action in this
list that applies to the snailfish number:

    If any pair is nested inside four pairs, the leftmost such pair explodes.
    If any regular number is 10 or greater, the leftmost such regular number splits.

To explode a pair, the pair's left value is added to the first regular number to
the left of the exploding pair (if any), and the pair's right value is added to
the first regular number to the right of the exploding pair (if any).

Exploding pairs will always consist of two regular numbers. Then, the entire
exploding pair is replaced with the regular number 0.

Once no action in the above list applies, the snailfish number is reduced.
Add up all of the snailfish numbers from the homework assignment in the order
 they appear. What is the magnitude of the final sum?
            
================================================================================|#

(require rackunit
         threading
         racket/trace)

#|==============================================================================|#
#|                                      DATA                                    |#
#|==============================================================================|#

#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#

#|


|#

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

;(module+ test
;  (check-equal? (day18.1 test-data) 0))

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