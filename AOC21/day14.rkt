#lang racket

;AOC Day 14
;Leo Laporte 13-17 Jan 2021

#|==============================================================================

                       --- Day 14: Extended Polymerization ---

                                         PART 1 

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

(require rackunit) ; for tests

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

Except, wait. I don't really need to keep track of elements inserted. I have
that information in the pair counts. After I build the entire string I can
use the pair counts to derive the elements. Each element appears in two positions:
front and back of the pairs. So, if I count the number of times an element appears in
all pairs and divide it by two that should be the total count for that element.
But there's one more thing: the elements at each end of the string occur don't pair
at one end so they'll be short by one. So, add one to those before dividing by two.

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
  "If a key exists add num to existing value, otherwise insert key into hash with value of num
   this is for incrementing key counts in elements and pairs hashes - num is previous count for
   pairs and pair count for elements"
  (if (hash-has-key? hash key)
      (hash-set! hash key (+ num (hash-ref hash key)))   ; add num to existing value or
      (hash-set! hash key num))                          ; add key with num for value
  hash)

(module+ test
  (check-equal? (hash-key+ (make-hash '(("A" . 1) ("B" . 2) ("C" . 3))) "C" 10)
                (make-hash '(("A" . 1) ("B" . 2) ("C" . 13))))
  (check-equal? (hash-key+ (make-hash '(("A" . 1) ("B" . 2) ("C" . 3))) "D" 10)
                (make-hash '(("A" . 1) ("B" . 2) ("C" . 3) ("D" . 10))))
  (check-equal? (hash-key+ (make-hash '(("A" . 1) ("B" . 2) ("C" . 3) ("D" . 2))) "D" 10)
                (make-hash '(("A" . 1) ("B" . 2) ("C" . 3) ("D" . 12)))))

(define (make-pair-hash str)
  "make a hash of all two char pairs in a polymer"
  (let ([h (make-hash)])
    (for ([p (in-list (string->pairs str))])
      (hash-key+ h p 1))
    h))

(module+ test
  (check-equal? (make-pair-hash "ABC")
                (make-hash '(("AB" . 1) ("BC" . 1))))
  
  (check-equal? (make-pair-hash "ABCDCD")
                (make-hash '(("AB" . 1) ("BC" . 1) ("CD" . 2) ("DC" . 1))))

  (check-equal? (make-pair-hash "NBBBCNCCNBBNBNBBCHBHHBCHB")
                (make-hash '(("BB" . 4)
                             ("BC" . 3)
                             ("BH" . 1)
                             ("BN" . 2)
                             ("CC" . 1)
                             ("CH" . 2)
                             ("CN" . 2)
                             ("HB" . 3)
                             ("HH" . 1)
                             ("NB" . 4)
                             ("NC" . 1)))))

(define (new-pairs pair element)
  "inserts a character into the middle of a character pair, returns resulting string pairs"
  (string->pairs (string-append (substring pair 0 1) element (substring pair 1 2))))

(module+ test
  (check-equal? (new-pairs "AB" "C")
                (list "AC" "CB")))

;; Hash Hash -> Hash
;; Given a hash of insertion rules and a starting hash of the current pairs
;; produce a pairs hash with the new pairs created
(define (insert-element rules pairs)
  "Once thru old polymer using rules to produce a new polymer hash of the pairs and their count"
  (for/fold ([ps (make-hash)])                                  ; the new pair hash
            ([pair (in-list (hash-keys pairs))])                ; for all pairs in starting polymer
    (for ([p (in-list (new-pairs pair (hash-ref rules pair)))]) ; get new pairs created by element insertion
      (hash-key+ ps p (hash-ref pairs pair)))                   ; update pair hash
    (values ps)))                                               ; store resulting hashes in accumulator

(module+ test
  (check-equal? (insert-element test-rules (make-pair-hash "NNCB"))
                (make-pair-hash "NCNBCHB"))
  
  (check-equal? (insert-element test-rules (make-pair-hash "NCNBCHB"))
                (make-pair-hash "NBCCNBBBCBHCB"))
                
  (check-equal? (insert-element test-rules (make-pair-hash "NBCCNBBBCBHCB"))
                (make-pair-hash "NBBBCNCCNBBNBNBBCHBHHBCHB"))

  (check-equal? (insert-element test-rules (make-pair-hash "NBBBCNCCNBBNBNBBCHBHHBCHB"))
                (make-pair-hash "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")))

;; Hash -> (list-of Natural)
;; Given a pair-hash make a new list of the count for each element in the pair-hash
;; this will give a list of doubled results so don't forget to divide the answer by 2
;; every letter appears in two pairs except the end points - so add one to them
(define (element-count pair-hash first last)
  (define raw-char-hash
    (for/fold ([ch (make-hash)]) 
              ([pair (in-list (hash-keys pair-hash))])
      (for ([c (in-list (list (substring pair 0 1) (substring pair 1 2)))])
        (hash-key+ ch c (hash-ref pair-hash pair)))
      (values ch)))

  (hash-key+ raw-char-hash first 1)  ; fix the outer elements
  (hash-key+ raw-char-hash last 1)   
  (hash-values raw-char-hash))       ; list of DOUBLED values (divide by two later)
             
(module+ test
  (check-equal? (element-count (make-pair-hash "NNCB") "N" "B")
                '(2 2 4))
  (check-equal? (element-count (make-pair-hash "NCNBCHB") "N" "B")
                '(4 2 4 4))
  (check-equal? (element-count (make-pair-hash "NBCCNBBBCBHCB") "N" "B")
                '(8 2 12 4)))
  
;; String Hash Natural -> Natural
;; Given a polymer string and a hash of insertion rules produce the
;; difference between the most used and least used element after
;; the given number of insertion iterations
(define (day14.1 polymer rules iterations)
  (let* ([start-char (substring polymer 0 1)]  ; keep track of first and last char in polymer string
         [end-char (substring polymer (sub1 (string-length polymer)) (string-length polymer))]

         [final-pairs (for/fold ([pairs (make-pair-hash polymer)]) ; run insert-element...
                                ([i (in-range iterations)])        ; ...iteration times...
                        (values (insert-element rules pairs)))]    ; ...to produce final polymer as hash of pairs

         [counts (element-count final-pairs start-char end-char)]) ; produce list of element counts
         
    (/ (- (apply max counts) (apply min counts)) 2)))  ; divide the final answer by 2 (cf. element-count)
        
(module+ test
  (check-equal? (day14.1 test-polymer test-rules 1) 1)
  (check-equal? (day14.1 test-polymer test-rules 2) 5)
  (check-equal? (day14.1 test-polymer test-rules 3) 7)
  (check-equal? (day14.1 test-polymer test-rules 4) 18)
  (check-equal? (day14.1 test-polymer test-rules 10) 1588))

 (time (printf "2021 AOC Problem 14.1 = ~a\n" (day14.1 input-polymer input-rules 10)))

#|=================================================================================
                                        PART 2
                               
Apply 40 steps of pair insertion to the polymer template and find the most and
least common elements in the result. What do you get if you take the quantity
of the most common element and subtract the quantity of the least common element?

==================================================================================|#

(time (printf "2021 AOC Problem 14.2 = ~a\n" (day14.1 input-polymer input-rules 40)))

#|
################################################################################
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
2021 AOC Problem 14.1 = 3118
cpu time: 0 real time: 0 gc time: 0
2021 AOC Problem 14.2 = 4332887448171
cpu time: 2 real time: 2 gc time: 0

2022 Mac Studio Max with 32GB RAM

2021 AOC Problem 14.1 = 3118
cpu time: 0 real time: 0 gc time: 0
2021 AOC Problem 14.2 = 4332887448171
cpu time: 6 real time: 6 gc time: 2

################################################################################
|#