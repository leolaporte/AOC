#lang racket

;AOC Day 14
;Leo Laporte 13 Jan 2021

#|==============================================================================|#
#|                                     PART 1                                   |#
#|==============================================================================|#


#|================================================================================
                      --- Day 14: Extended Polymerization ---
"...You just need to work out what polymer would result after repeating the pair
insertion process a few times.
The first line is the polymer template - this is the starting point of the process.
The following section defines the pair insertion rules. A rule like AB -> C means
that when elements A and B are immediately adjacent, element C should be inserted
 between them. These insertions all happen simultaneously.
Apply 10 steps of pair insertion to the polymer template and find the most and
least common elements in the result. What do you get if you take the quantity
of the most common element and subtract the quantity of the least common element?"
================================================================================|#

(require rackunit
         threading
         racket/trace)

#|==============================================================================|#
#|                                      DATA                                    |#
#|==============================================================================|#

(define raw-input (string-split (file->string "input14.txt") "\n\n"))
(define raw-test
  (string-split
   "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C\n"
   "\n\n"))

(define (make-rule-hash rules)
  (let ([rule-list (string-split rules "\n")])
    (for/hash ([r (in-list rule-list)])
      (let ([parts (regexp-match #px"([A-Z]{2}) -> ([A-Z]{1})" r)])
        (values (second parts) (third parts))))))

(define-values (input-polymer input-rules) (values (first raw-input)
                                                   (make-rule-hash (second raw-input))))

(define-values (test-polymer test-rules) (values (first raw-test)
                                                 (make-rule-hash (second raw-test))))

#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#

#|
According to the problem text, the size of the polymer string almost doubles every
iteration. 10 is tolerable but it grows very fast. So I'll need some way
to represent the data I want to track. Shades of Lanternfish!
I only really need to count the elements added. Can I make a hash table of
all the char pairs, scrub through that for each iteration, adding any new pairs
and keeping a count of each element inserted. That should do it, right?
Let's see...
|#

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

;; First some helper functions

(define (string->pairs str)
  "return a list of all the side-by-side character pairs in the string"
  (for/list ([i (in-range (sub1 (string-length str)))])
    (substring str i (+ i 2))))

(module+ test
  (check-equal? (string->pairs "ABCD")
                (list "AB" "BC" "CD"))
  (check-equal? (string->pairs "ABCDEFG")
                (list "AB" "BC" "CD" "DE" "EF" "FG")))

(define (hash-key+ hash key num)
  "If a key exists add num to existing value, otherwise insert key into hash with value of num"
  (if (hash-has-key? hash key)
      (hash-set! hash key (+ num (hash-ref hash key)))
      (hash-set! hash key num))
  hash)

(module+ test
  (check-equal? (hash-key+ (make-hash '(("A" . 1) ("B" . 2) ("C" . 3))) "C" 10)
                (make-hash '(("A" . 1) ("B" . 2) ("C" . 13))))
  (check-equal? (hash-key+ (make-hash '(("A" . 1) ("B" . 2) ("C" . 3))) "D" 10)
                (make-hash '(("A" . 1) ("B" . 2) ("C" . 3) ("D" . 10)))))

(define (make-element-hash str)
  "given a polymer string make a hash with keys of elements and values of element count"
  (let ([h (make-hash)])
    (for ([i (in-range (string-length str))])
      (hash-key+ h (substring str i (add1 i)) 1))
    h))

(module+ test
  (check-equal? (make-element-hash "ABC")
                (make-hash '(("A" . 1) ("B" . 1) ("C" . 1))))
  (check-equal? (make-element-hash "ABCC")
                (make-hash '(("A" . 1) ("B" . 1) ("C" . 2)))))

(define (make-pair-hash str)
  "make a hash of all two char pairs in a polymer"
  (let ([h (make-hash)]
        [pairs (string->pairs str)])
    (for ([p (in-list pairs)])
      (hash-key+ h p 1))
    h))

(module+ test
  (check-equal? (make-pair-hash "ABC")
                (make-hash '(("AB" . 1) ("BC" . 1))))
  (check-equal? (make-pair-hash "ABCDCD")
                (make-hash '(("AB" . 1) ("BC" . 1) ("CD" . 2) ("DC" . 1)))))

(define (new-pairs pair element)
  "inserts a character into the middle of a character pair, returns resulting string pairs"
  (string->pairs (string-append (substring pair 0 1) element (substring pair 1 2))))

(module+ test
  (check-equal? (new-pairs "AB" "C")
                (list "AC" "CB")))

;; Now the meat
    
;; String Hash Natural -> Hash
;; Given a polymer string, a hash of insertion rules in the form of "AB" -> "C"
;; update the hash for each element inserted with a count of the number of total
;; insertions: "A" -> 123
(define (insert-element polymer rules pairs elements)

  (for/fold ([ph pairs]        ; accumulate the pairs in the polymer with their count
             [els elements]    ; accumulate the number of elements inserted
             #:result (cons ph els))
             
            ([pair (in-list (hash-keys pairs))])  ; go through all the pairs in the polymer to date

    (cond [(hash-has-key? rules pair)                                   ; only if there is rule for this pair
           (hash-key+ els (hash-ref rules pair) (hash-ref pairs pair))  ; add (number of pairs like this) to elements
           (for ([p (in-list (new-pairs pair (hash-ref rules pair)))])  ; get new pairs created by element insertion
             (hash-key+ ph p 1))])                                      ; increment these pairs in the pair hash
    (values ph els)))                                                   ; store resulting hashes in accumulators

;; String Hash Natural -> Natural
;; Given a polymer string and a hash of insertion rules produce the
;; difference between the most used and least used element after
;; the given number of insertion iterations
(define (day14.1 polymer rules iterations)
  (define pairs (make-pair-hash polymer))       ; make a hash of all the pairs in the starting polymer
  (define elements (make-element-hash polymer)) ; ditto elements

  (define (insert-elements ps els iter)         ; insert elements into polymer iter times
    (cond [(zero? iter) els]
          [else
           (let ([res (insert-element polymer rules ps els)])
             (insert-elements (car res) (cdr res) (sub1 iter)))]))

  (let* ([element-counts (insert-elements pairs elements iterations)]
        [max (apply max (hash-values element-counts))]
        [min (apply min (hash-values element-counts))])
    (- max min)))

(module+ test
 (check-equal? (day14.1 test-polymer test-rules 10) 1588))

(time (printf "2021 AOC Problem 14.1 = ~a\n" (day14.1 input-polymer input-rules 10)))


#|
Part Two
|#

(time (printf "2021 AOC Problem 14.2 = ~a\n" (day14.1 input-polymer input-rules 40)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

