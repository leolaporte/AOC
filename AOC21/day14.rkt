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


(define (string->pairs str)
  "return a list of all the side-by-side character pairs in the string"
  (for/list ([i (in-range (sub1 (string-length str)))])
    (substring str i (+ i 2))))

(define (hash-key+ hash key num)
  "If the key exists add num to existing value, otherwise insert key into hash with value of num"
  (if (hash-has-key? hash key)
      (hash-set! hash key (+ num (hash-ref hash key)))
      (hash-set! hash key num)))

(define (make-element-hash str)
  "given a polymer string make a hash of each element -> count"
  (let ([h (make-hash)])
    (for ([i (in-range (string-length str))])
      (hash-key+ h (substring str i (add1 i)) 1))
    h))

(define (make-pair-hash str)
  "make a hash of all two char pairs in str"
  (let ([h (make-hash)]
        [pairs (string->pairs str)])
    (for ([p (in-list pairs)])
      (hash-key+ h p 1))
    h))

(define (new-pairs pair element)
  "inserts a character into the middle of a character pair, returns resulting string pairs"
  (string->pairs (string-append (substring pair 0 1) element (substring pair 1 2))))
    
(define test-pair-hash (make-pair-hash test-polymer))  
(define input-pair-hash (make-pair-hash input-polymer))

(define test-element-hash (make-element-hash test-polymer))
(define input-element-hash (make-element-hash input-polymer))

#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#

#|
I did a little math (below) and the size of the string almost doubles every
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
 
;; String Hash Natural -> Hash
;; Given a polymer string, a hash of insertion rules in the form of "AB" -> "C"
;; update the hash for each element inserted with
;; a count of the number of total inertions: "A" -> 123
(define (insert-element polymer rules pairs elements)

  (for/fold ([ph pairs]        ; accumulate the pairs in the polymer with their count
             [els elements]    ; accumulate the number of elements inserted
             #:result (values ph els))
             
            ([pair (in-list (hash-keys pairs))])  ; go through all the pairs in the polymer to date

    (cond [(hash-has-key? rules pair)                                   ; only if there is rule for this pair
           (hash-key+ els (hash-ref rules pair) (hash-ref ph pair))     ; add (number of pairs like this) to elements
           (for ([p (in-list (new-pairs pair (hash-ref rules pair)))])  ; get new pairs created by element insertion
             (hash-key+ ph p 1))])                                      ; increment these pairs in the pair hash
    (values ph els)))                                                   ; store resulting hashes in accumulators

(module+ test
  (check-equal? (insert-element test-polymer test-rules test-pair-hash test-element-hash)
                (values (make-pair-hash "NCNBCHB") (make-element-hash "NCNBCHB"))))

; (define (day14.1 d) 0) ; stub

; (module+ test
;   (day14.1 test-data) 1588)

; (time (printf "2021 AOC Problem 14.1 = ~a\n" (day14.1 input)))


#|

Part Two

|#

; (define (day14.2 d) 0) ; stub

; (module+ test
;   (day14.2 test-data) 1588)

; (time (printf "2021 AOC Problem 14.1 = ~a\n" (day14.1 input)))
; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
