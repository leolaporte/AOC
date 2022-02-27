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

(define test "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")

;; String -> (list-of (list-of Symbol))
;; Turns the input into a list of list of Symbols
(define (make-stack str)
  (for/list ([c (in-list (string->list str))])
    (match c
      [#\[ 'open]
      [#\] 'close]
      [#\, 'sep]
      [c (string->number (string c))])))

(define (snailify str)
  (~> str
      (string-split _ "\n")
      (map make-stack _)))

#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#

#|
This is one of those problems where choosing the right data representation will
make all the difference. I could use strings and regex. Although I note that
these are nested lists - native to Racket. I've looked at a number of ways of
doing this but I like JHonaker's idea of implementing it as a pseudo stack.

"MisterMentat — 12/18/2021
It works like The gist is that you can explode after a close brace if it's
 deep enough. So make braces increase and decrease a depth counter and do
the required manipuation of the stack (much easier then finding the leftmost
element of the right tree on the call stack and the rightmost element of the
left tree) and restart. For splitting, just replace it with [a, b] when the
number is greater than 9 and restart. If it makes it all the way through,
you're done reducing. For magnitude, just push a 3 onto the stack any time
you hit an open bracket. Push numbers literally. Pop two numbers off the
stack and multiply, push the result back on, then push a 2 on when you hit
a comma. Finally, pop two and multiply, push the result, then pop two, add,
and push back when you hit a close bracket."

|#

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

;(module+ test
;  (check-equal? (day18.1 sample-data) 4140))

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