#lang racket
;  AOC 2021
; Leo Laporte 3 Dec 2021
; 
; --- Day 3: Binary Diagnostic ---
; 
; The submarine has been making some odd creaking noises, so you ask it to produce a
; diagnostic report just in case.
; 
; The diagnostic report (your puzzle input) consists of a list of binary numbers
; which, when decoded properly, can tell you many useful things about the conditions
; of the submarine. The first parameter to check is the power consumption.
; 
; You need to use the binary numbers in the diagnostic report to generate two new
; binary numbers (called the gamma rate and the epsilon rate). The power consumption
; can then be found by multiplying the gamma rate by the epsilon rate.
; 
; Each bit in the gamma rate can be determined by finding the most common bit in
; the corresponding position of all numbers in the diagnostic report. For example,
; given the following diagnostic report:
; 
; 00100
; 11110
; 10110
; 10111
; 10101
; 01111
; 00111
; 11100
; 10000
; 11001
; 00010
; 01010
; 
; Considering only the first bit of each number, there are five 0 bits and seven 1 bits.
; Since the most common bit is 1, the first bit of the gamma rate is 1.
; 
; The most common second bit of the numbers in the diagnostic report is 0, so the second
; bit of the gamma rate is 0.
; 
; The most common value of the third, fourth, and fifth bits are 1, 1, and 0, respectively,
; and so the final three bits of the gamma rate are 110.
; 
; So, the gamma rate is the binary number 10110, or 22 in decimal.
; 
; The epsilon rate is calculated in a similar way; rather than use the most common bit,
; the least common bit from each position is used. So, the epsilon rate is 01001, or 9 in
; decimal. Multiplying the gamma rate (22) by the epsilon rate (9) produces the power
; consumption, 198.
; 
; Use the binary numbers in your diagnostic report to calculate the gamma rate and
; epsilon rate, then multiply them together. What is the power consumption of the submarine?
; (Be sure to represent your answer in decimal, not binary.)
; 
; 


(require racket/file threading rackunit)

;; Problem input from adventofcode.com
(define day3data (string-split (file->string "input3.txt") "\n"))

(define sample-data
  (list "00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"))

;; naive method
;; 1. add up each place of each number in the list of binary numbers 
;; 2. per place divide place total by 2, if greater than length of list, that place is 1, else 0
;; 3. multiply that number represented by all places (gamma) by its ones-complement (epsilon), result is solution
;; this is so heinous there must be a better way but I can't think of what it is

;; list-of string -> integer
;; given a list of strings of binary numbers produce the product of the gamma and epsilon
(define (day3.1 num-list)
  (let* ((num-entries (length num-list))  ; number of entries in the list
         (column-totals (add-columns (map string->list num-list))) ; add up the numbers in each column
         (sums (sum-row num-entries column-totals)) ; calc the gamma and epsilon '(gamma . epsilon)
         (gamma (car sums))
         (epsilon (cdr sums)))
    (* gamma epsilon)))
           
(module+ test
  (check-equal? (day3.1 sample-data) 198))

;; natural (list-of (list-of bytes)) -> (list-of natural)
;; add up the numbers in each column of a list of byte-strings representing binary numbers
;; produces a list with a total for each column in the given numbers
(define (add-columns byte-list)
  (local [(define (recursive-add-columns entry-accum lst)
            (cond ((empty? lst) entry-accum)
                  (else  (recursive-add-columns
                          (add-entry (map char->number (first lst)) entry-accum) (rest lst)))))]
    (recursive-add-columns (make-list (length (first byte-list)) 0) byte-list)))

(module+ test
  (check-equal? (add-columns (map string->list sample-data)) (list 7 5 8 7 5)))

(define (add-entry entry entries)
  (for/list ([i (in-list entry)]
             [j (in-list entries)])
    (+ i j)))

(module+ test
  (check-equal? (add-entry '(0 0 0 0) (map char->number (string->list "1111"))) '(1 1 1 1))
  (check-equal? (add-entry '(1 1 1 1) (map char->number (string->list "1111"))) '(2 2 2 2))
  (check-equal? (add-entry '(9 9 9 9) (map char->number (string->list "1111"))) '(10 10 10 10)))
   
;; natural (list-of natural) -> (gamma . epsilon)
;; given a list of numbers, return the gamma and epsilon values
(define (sum-row len total-list)
  (let ([threshold (quotient len 2)]) ; if sum of digits > len/2 then there are more 1s than 0s
    (for/fold ([gamma null]       ; accumulators...
               [epsilon null]     ; ... fold builds lists of 1s and 0s
               #:result (cons (bin-list->decimal (reverse gamma)) (bin-list->decimal (reverse epsilon)))) 
              ([x (in-list total-list)])
      (if (> x threshold)
          (values (cons 1 gamma) (cons 0 epsilon))      
          (values (cons 0 gamma) (cons 1 epsilon))))))

(module+ test
  (check-equal? (sum-row 14 '(12 5 14 1)) '(10 . 5))
  (check-equal? (sum-row 12 '(7 5 8 7 5)) '(22 . 9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some simple utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; char -> natural
;; given a number char (0-9) return the number it represents (#\0 -> 0)
(define (char->number char)
  (- (char->integer char) (char->integer #\0) ))

;; (list-of 1 or 0) -> natural
;; turn list of binary digits into its decimal equivalent, eg. '(1 0 1 0) into 10
(define (bin-list->decimal lst)
  (local [(define (list->string l)
            (cond ((empty? l) "")
                  (else (string-append (number->string (first l)) (list->string (rest l))))))]
    (string->number (list->string lst) 2)))

(module+ test
  (check-equal? (bin-list->decimal '(1 0 1 0)) 10)
  (check-equal? (bin-list->decimal '(1 1 1 1)) (+ 1 2 4 8))
  (check-equal? (bin-list->decimal '(1 0 0 0)) 8))

(time (printf "2021 AOC Problem 3.1 = ~a\n" (day3.1 day3data)))

;  --- Part Two ---
; 
; Next, you should verify the life support rating, which can be determined by multiplying
; the oxygen generator rating by the CO2 scrubber rating.
; 
; Both the oxygen generator rating and the CO2 scrubber rating are values that can be found
; in your diagnostic report - finding them is the tricky part. Both values are located using
; a similar process that involves filtering out values until only one remains. Before searching
; for either rating value, start with the full list of binary numbers from your diagnostic
; report and consider just the first bit of those numbers. Then:
; 
; Keep only numbers selected by the bit criteria for the type of rating value for which you
; are searching. Discard numbers which do not match the bit criteria.
; If you only have one number left, stop; this is the rating value for which you are searching.
; Otherwise, repeat the process, considering the next bit to the right.
; The bit criteria depends on which type of rating value you want to find:
; 
; To find oxygen generator rating, determine the most common value (0 or 1) in the current bit
; position, and keep only numbers with that bit in that position. If 0 and 1 are equally common,
; keep values with a 1 in the position being considered.
; 
; To find CO2 scrubber rating, determine the least common value (0 or 1) in the current bit
; position, and keep only numbers with that bit in that position. If 0 and 1 are equally common,
; keep values with a 0 in the position being considered.
; 
; For example, to determine the oxygen generator rating value using the same example diagnostic
; report from above:
; 
; Start with all 12 numbers and consider only the first bit of each number.
; There are more 1 bits (7) than 0 bits (5), so keep only the 7 numbers with a 1 in the
; first position: 11110, 10110, 10111, 10101, 11100, 10000, and 11001.
; 
; Then, consider the second bit of the 7 remaining numbers: there are more 0 bits (4)
; than 1 bits (3), so keep only the 4 numbers with a 0 in the
; second position: 10110, 10111, 10101, and 10000.
; 
; In the third position, three of the four numbers have a 1, so keep those three:
; 10110, 10111, and 10101.
; 
; In the fourth position, two of the three numbers have a 1, so keep those two: 10110 and 10111.
; 
; In the fifth position, there are an equal number of 0 bits and 1 bits (one each). So, to find
; the oxygen generator rating, keep the number with a 1 in that position: 10111.
; 
; As there is only one number left, stop; the oxygen generator rating is 10111, or 23 in decimal.
; Then, to determine the CO2 scrubber rating value from the same example above:
; 
; Start again with all 12 numbers and consider only the first bit of each number.
; There are fewer 0 bits (5) than 1 bits (7), so keep only the 5 numbers with a 0 in the
; first position: 00100, 01111, 00111, 00010, and 01010.
; Then, consider the second bit of the 5 remaining numbers: there are fewer 1 bits (2) than
; 0 bits (3), so keep only the 2 numbers with a 1 in the second position: 01111 and 01010.
; In the third position, there are an equal number of 0 bits and 1 bits (one each). So, to
; find the CO2 scrubber rating, keep the number with a 0 in that position: 01010.
; As there is only one number left, stop; the CO2 scrubber rating is 01010, or 10 in decimal.
; Finally, to find the life support rating, multiply the oxygen generator rating (23) by the
; CO2 scrubber rating (10) to get 230.
; 
; Use the binary numbers in your diagnostic report to calculate the oxygen generator rating and
; CO2 scrubber rating, then multiply them together. What is the life support rating of the
; submarine? (Be sure to represent your answer in decimal, not binary.)
; 


(define O2 1)
(define CO2 0)

(define (day3.2 data)
  (* (solve-for-type data 0 O2) (solve-for-type data 0 CO2)))

;; (list-of string) type -> natural
;; given a list of binary numbers represented as strings and the type of rating we're solving ;; for return the decimal solution
(define (solve-for-type data col type)
  (cond [(equal? (length data) 1) (string->number (car data) 2)]
        [else (solve-for-type (reduce-list data col type) (add1 col) type)]))
                                 
(module+ test
  (check-equal? (solve-for-type sample-data 0 O2) 23)
  (check-equal? (solve-for-type sample-data 0 CO2) 10))

;; (list-of string) natural natural -> (list-of string)
;; given a list of binary numbers represented as strings, the column to total, and the type of
;; solution (O2 or CO2) return the list of strings that match the type
(define (reduce-list data col type)
  (let* ([len (length data)]
         [col-total (total-column data col)]
         [key (if (equal? type O2)
                  (get-mcv col-total len)
                  (get-lcv col-total len))])
    (filter (lambda (x) (equal? key (string-ref x col))) data)))    

(module+ test
  (check-equal? (reduce-list sample-data 0 O2)
                (list "11110" "10110" "10111" "10101" "11100" "10000" "11001"))
  (check-equal? (reduce-list sample-data 0 CO2)
                (list "00100" "01111" "00111" "00010" "01010"))
  (check-equal? (reduce-list '("11110" "10110" "10111" "10101" "11100" "10000" "11001") 1 O2)
                (list "10110" "10111" "10101" "10000")))

(define (get-mcv col-total len)
  (if (>= col-total (/ len 2)) #\1 #\0))

(define (get-lcv col-total len)
  (if (>= col-total (/ len 2)) #\0 #\1))

;; natural (list-of string) -> natural
;; given a list of binary numbers represented as strings return the sum of digits in the given bit position
(define (total-column data col)
  (local [(define (process-list vl)
            (cond [(empty? vl) 0]
                  [else (+ (binary-string-ref (first vl) col) (process-list (rest vl)))]))]
    (process-list data)))

(module+ test
  (check-equal? (total-column '("100" "101" "010" "111") 0) 3)
  (check-equal? (total-column '("100" "101" "010") 1) 1)
  (check-equal? (total-column '("100" "101" "010" "111") 2) 2))

;; string natural -> 1 or 0
;; given a string representing a binary number return the value (1 or 0) in the given position
;; pos starts at 0 for left-most digit
(define (binary-string-ref str pos)
  (~> str string->list list->vector (vector-ref _ pos) char->number))

(module+ test
  (check-equal? (binary-string-ref "100000" 0) 1)
  (check-equal? (binary-string-ref "010000" 1) 1)
  (check-equal? (binary-string-ref "001000" 2) 1)
  (check-equal? (binary-string-ref "000100" 3) 1))

(module+ test
  (check-equal? (day3.2 sample-data) 230))

 (time (printf "2021 AOC Problem 3.2 = ~a\n" (day3.2 day3data)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
; 2021 AOC Problem 3.1 = 3885894
; cpu time: 3 real time: 3 gc time: 0
; 2021 AOC Problem 3.2 = 4375225
; cpu time: 2 real time: 2 gc time: 0

; Real world timing
;      --------Part 1---------   --------Part 2--------
;Day       Time    Rank  Score       Time   Rank  Score
;  3       >24h  102352      0       >24h  88448      0
;  2   00:38:41   13051      0   01:06:26  14647      0
;  1   00:24:22    7349      0   12:27:40  59726      0
