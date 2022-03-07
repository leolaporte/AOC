#lang racket

;AOC Day 18
;Leo Laporte 12-Fen-2022

#|==============================================================================

                              --- Day 18: Snailfish ---

"Add up all of the snailfish numbers from the homework assignment in the order
 they appear. What is the magnitude of the final sum?"
            
================================================================================|#

(require rackunit
         threading
         racket/trace)

#|==============================================================================|#
#|                                      DATA                                    |#
#|==============================================================================|#

; since we're working with strings a simple string-split on \n will do

;; String -> (list-of String)
;; converts a string into a list of strings
(define (snailify str) (string-split str "\n"))

(define test-data #<<here
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
here
  )



#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#

#|
Initially it looks so much like nested lists my inclination is to make a tree
out of the data. But I think I can do everything by manipulating strings. Why
overcomplicate?

There are a LOT of tests in this code. Mostly because it took significant
trial and error to debug the regexes. I suspect Eric Wastl knew this
because today's problem has an ususual number of examples suitable for
testing.

|#

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

#|
"If any pair is nested inside four pairs, the leftmost such pair explodes.

To explode a pair, the pair's left value is added to the first regular
number to the left of the exploding pair (if any), and the pair's right
value is added to the first regular number to the right of the exploding
pair (if any). Exploding pairs will always consist of two regular numbers.
Then, the entire exploding pair is replaced with the regular number 0."

 NOTES:
 Unfortunately, while I can do everything else with regex I can't figure
 out how to find the exploding pairs when there's a mix of left and right
 brackets in the left hand expression, the latter cancelling the former.
 There's probably a regular expression that can do this, but it's beyond
 my meager capabilities, so...

 I'll have to write a function for sn-explode that will find the
 leftmost pair to explode by counting the brackets (increasing with [ and
 decreasing with every ]) until I get to five (or reach the end of the str).
 Turns out this is pretty easy using for/fold.

 also, what happens if you can't add to the left OR right???? Undefined.
|#

;; String -> String
;; given a string find a pair nested inside four pairs, returns
;; left side of string up to the pair, if no pair exists returns entire
;; string
(define (find-pair str)
  (for/fold ([left-side ""]      ; build the left hand string here
             [depth 0]           ; keep track of [ and ]
             #:result (list->string (reverse (string->list left-side))))

            ([i (in-range (string-length str))])  ; iterate through string from L to R
    
    #:break (equal? depth 5)                      ; stop when nested 4 deep
    
    (let ([c (substring str i (add1 i))])
      (values
       (string-append c left-side)
       (match c
         ("[" (add1 depth))
         ("]" (sub1 depth))
         (_ depth))))))

(module+ test
  (check-equal? (find-pair "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]") "[[[[0,7],4],[[7,8],[0,[")
  (check-equal? (find-pair "[[1,2][1,2][1,2]]") "[[1,2][1,2][1,2]]")
  (check-equal? (find-pair "[[[[[9,8],1],2],3],4]") "[[[[[")
  (check-equal? (find-pair "[7,[6,[5,[4,[3,2]]]]]") "[7,[6,[5,[4,["))

; get exploding pair 
(define right-side (pregexp "(\\d+),(\\d+)\\](.*)$"))

; find last number in left part (if any)
; four parts: whole string, left fragment, digit(s), remaining chars, or #f if no digit
(define split-left (pregexp "^(.*[\\[|,])(\\d+)(\\D*)$"))

; find first number in right part (if any)
; four parts: whole string, left fragment of right, digit(s), right fragment, or #f if no digit
(define split-right (pregexp "^(.*?)(\\d+)(.*)$"))

; String -> String
; explodes a string according to the rules, continues exploding until fully reduced
(define (sn-explode str)
  ; look for pair to explode
  (let ([left-side (find-pair str)]) 
    (cond [(equal? left-side str) str]                            ; no pair to explode so return str
          [else                                                   ; work with the parts of the string
           
           ; first get the parts to reassemble
           (let* ([llen (string-length left-side)]                
                  [left (substring left-side 0 (sub1 llen))]      ; chop off last [
                  [part (regexp-match right-side (substring str llen))]
                  [d1 (string->number (second part))]             ; left value of pair
                  [d2 (string->number (third part))]              ; right value of pair
                  [right (fourth part)]                           ; right side of string
                  
                  ; now we have to figure out where to add, left or right?
                  [left-last (regexp-match split-left left)]      ; is there a number to left?
                  [right-first (regexp-match split-right right)]) ; is there a number to the right?

             (sn-explode       ; keep exploding until it's fully reduced (recurse)
              (string-append   ; build the exploded string
              
               (cond [left-last   ; can we add to the left?
                      (string-append
                       (second left-last)  
                       (number->string (+ d1 (string->number (third left-last))))
                       (fourth left-last)
                       "0"

                       (cond [right-first ; left added, right, too?
                              (string-append 
                               (second right-first) ;yes
                               (number->string (+ d2 (string->number (third right-first))))
                               (fourth right-first))]
                             [else right]))] ; no adding to the right, just finish it off

                     
                     [right-first ; couldn't add to the left, can we add to the right?
                      (string-append
                       left
                       "0"
                       (second right-first)
                       (number->string (+ d2 (string->number (third right-first))))
                       (fourth right-first))]

                     [else "Error: can't add to the left OR right!"]))))])))
  
(module+ test
  (check-equal? (sn-explode "[[[[[9,8],1],2],3],4]") "[[[[0,9],2],3],4]")
  (check-equal? (sn-explode "[7,[6,[5,[4,[3,2]]]]]") "[7,[6,[5,[7,0]]]]")
  (check-equal? (sn-explode "[[6,[5,[4,[3,2]]]],1]") "[[6,[5,[7,0]]],3]")
  (check-equal? (sn-explode "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]") "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
  (check-equal? (sn-explode "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
  (check-equal? (sn-explode "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]") "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
  (check-equal? (sn-explode "[[[[12,12],[6,14]],[[15,0],[17,[8,1]]]],[2,9]]") "[[[[12,12],[6,14]],[[15,0],[25,0]]],[3,9]]")

  ;  and some two-steppers:
  (check-equal? (sn-explode "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]") "[[[[0,7],4],[15,[0,13]]],[1,1]]")
  (check-equal? (sn-explode "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
  )

#|
"If any regular number is 10 or greater, the leftmost such regular number splits.

To split a regular number, replace it with a pair; the left element of the pair
should be the regular number divided by two and rounded down, while the right
element of the pair should be the regular number divided by two and rounded up."
|#

;; a regular expression to find the _first_ multiple digit number in a string
;; creates a list of four parts: the string, the left half, the multi-digit number, the right half
(define double-d (pregexp "^(.*?)(\\d{2,})(.*)$"))
;
;; String -> String
;; splits a string according to the rules, splits once then returns
(define (sn-split str)
  (let ([part (regexp-match double-d str)])  ; is there a doubled-number?
    (cond [(false? part) str]                ; nope, return string unmodified
          [else                              ; yep, build new string
           (let* ([num (string->number (third part))])
             
             (string-append
              (second part)
              "["
              (number->string (floor (/ num 2)))
              ","
              (number->string (ceiling (/ num 2)))
              "]"
              (fourth part)))])))

(module+ test
  (check-equal? (sn-split "[[[[0,7],4],[15,[0,2]]],[17,1]]") "[[[[0,7],4],[[7,8],[0,2]]],[17,1]]")
  (check-equal? (sn-split "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]") "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"))

#|
"To reduce a snailfish number, you must repeatedly do the first action in this list that
applies to the snailfish number:

    If any pair is nested inside four pairs, the leftmost such pair explodes.
    If any regular number is 10 or greater, the leftmost such regular number splits.

Once no action in the above list applies, the snailfish number is reduced.

During reduction, at most one action applies, after which the process returns to the
top of the list of actions. For example, if split produces a pair that meets the explode
criteria, that pair explodes before other splits occur."
|#

;;; String -> String
;;; reduces a string by repeated applications of explode and split
(define (sn-reduce str)
  (let ([new-string (sn-split (sn-explode str))]) ; explode until can't, then split
    (cond [(equal? new-string str) str]           ; once exploding and splitting do nothing, we're done
          [else (sn-reduce new-string)])))        ; reduce some more

(module+ test
  (check-equal? (sn-reduce "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
                "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))

;;; String String -> String
;;; adds two strings using snailfish math, reducing after addition
(define (sn-add str1 str2)
  (sn-reduce (string-append "[" str1 "," str2 "]")))

(module+ test
  (check-equal? (sn-add "[1,2]" "[[3,4],5]") "[[1,2],[[3,4],5]]")
  (check-equal? (sn-add "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]") "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

  ; now step-by-step the sn-adds of the test-data - with answers provided by AOC
  (check-equal? (sn-add "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]" "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]")
                "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]")
  
  (check-equal? (sn-add "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
                        "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]")
                "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]")

  (check-equal? (sn-add "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
                        "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]")
                "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]")

  (check-equal? (sn-add "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"
                        "[7,[5,[[3,8],[1,4]]]]")
                "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]")

  (check-equal? (sn-add "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"
                        "[[2,[2,2]],[8,[8,1]]]")
                "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]")

  (check-equal? (sn-add "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"
                        "[2,9]")
                "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]")

  (check-equal? (sn-add "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"
                        "[1,[[[9,3],9],[[9,0],[0,7]]]]")
                "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]")
  
  (check-equal? (sn-add "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"
                        "[[[5,[7,4]],7],1]")
                "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]")

  (check-equal? (sn-add "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"
                        "[[[[4,2],2],6],[8,7]]")
                "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))
 
;; (list-of String) -> String
;; given a list of snailfish numbers return the sum of all the numbers
(define (sn-add-list lst)
  (define (add-up total l)                                    
    (cond [(empty? l) total]
          [else (add-up (sn-add total (first l)) (rest l))]))
  
  (add-up (first lst) (rest lst)))

(module+ test
  (check-equal? (sn-add-list (snailify "[1,1]\n[2,2]\n[3,3]\n[4,4]"))
                "[[[[1,1],[2,2]],[3,3]],[4,4]]")
  (check-equal? (sn-add-list (snailify "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]"))
                "[[[[3,0],[5,3]],[4,4]],[5,5]]")
  (check-equal? (sn-add-list (snailify "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]"))
                "[[[[5,0],[7,4]],[5,5]],[6,6]]")
  (check-equal? (sn-add-list (snailify test-data))
                "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"))

#|
"The magnitude of a pair is 3 times the magnitude of its left element
plus 2 times the magnitude of its right element. The magnitude of a
regular number is just that number."
|#

; find first digit pair in a string
; four parts: whole string, left fragment of right, digit, right fragment, or #f if no digit
(define dp (pregexp "(.*?)\\[(\\d+),(\\d+)\\](.*)$"))

; String -> Natural
; calculates the magnitude of a string according to the rules
(define (sn-magnitude str)
  (let ([part (regexp-match dp str)])
    (cond [(false? part) (string->number str)] ; fully reduced, return the result
          [else
           (sn-magnitude
            (string-append
             (second part)
             (number->string
              (+ (* 3 (string->number (third part)))
                 (* 2 (string->number (fourth part)))))
             (fifth part)))])))

(module+ test
  (check-equal? (sn-magnitude "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]") 4140)
  (check-equal? (sn-magnitude "[[9,1],[1,9]]") 129)
  (check-equal? (sn-magnitude "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") 1384)
  (check-equal? (sn-magnitude "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") 3488)
  (check-equal? (sn-magnitude "[[1,2],[[3,4],5]]") 143)
  (check-equal? (sn-magnitude "[[[[1,1],[2,2]],[3,3]],[4,4]]") 445)
  (check-equal? (sn-magnitude "[[[[3,0],[5,3]],[4,4]],[5,5]]") 791)
  (check-equal? (sn-magnitude "[[[[5,0],[7,4]],[5,5]],[6,6]]") 1137)
  )

(define (day18.1 str)
  (~> str
      snailify
      sn-add-list
      sn-magnitude))

(module+ test
  (check-equal? (day18.1 test-data) 4140))

(time (printf "2021 AOC Problem 18.1 = ~a\n" (day18.1 (file->string "input18.txt"))))

#|=================================================================================
                                        PART 2
                               
  "What is the largest magnitude of any sum of two different snailfish numbers from
  the homework assignment?"
==================================================================================|#

;; NOTE:  snailfish math is NOT commutative so we'll have  to add 1 + 2 AND 2 + 1)
;; otherwise this is pretty easy.



;;(module+ test
;;  (check-equal? (day18.2 test-data) 0))
;
;; (time (printf "2021 AOC Problem 18.2 = ~a\n" (day18.2 input)))
;
;#|
;Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
;
;|#