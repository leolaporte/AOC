#lang racket

#|

AOC 2021
Leo Laporte 10-Dec-2021
 
 --- Day 8: Seven Segment Search ---
 
 In the output values, how many times do digits 1, 4, 7, or 8 appear?

|#


(require threading
         rackunit)

;; Well. That's a lot of narrative for a problem asking us to count string lengths.
;; Part two must be a doozy. Meanwhile...
(define input
  (~> (file->string "input8.txt")                  ; read in provided data
      (string-split _ "\n")                        ; split into entries
      (map (λ (x) (rest (string-split x "| "))) _) ; split entries by |
      (map (λ (x) (first x)) _)))                  ; only keep part after |

;; Provided test data
(define sample-data '("cdfeb fcadb cdfeb cdbaf"))
(define sample-data2 '("fdgacbe cefdb cefbgd gcbe" "fcgedb cgb dgebacf gc" "cg cg fdcagb cbg" "efabcd cedba gadfec cb" "gecf egdcabf bgf bfgea" "gebdcfa ecba ca fadegcb" "cefg dcbef fcge gbcadfe" "ed bcgafe cdgba cbgef" "gbdfcae bgc cg cgb" "fgae cfgab fg bagce")) ; only the words after the pipe

;; The digits that are easy to find all have a unique number of segments lit - the so-called
;; easy digits: 
;; (1 = 2 segs lit)
;; (4 = 4 segs lit)
;; (7 = 3 segs lit)
;; (8 = 7 segs lit)
;; so we're searching for 2 4 3 and 7 (missed that bit at first - that's what the tests are for!)
(define easy-digits '(2 4 3 7))

;; (list-of String) -> Natural
;; given a list of strings, return the number of words that match the "easy digits"
(define (day8.1 data)
  (for/sum ([words (in-list data)])
    (count-easy-digits words)))

(module+ test
  (check-equal? (day8.1 sample-data) 0)
  (check-equal? (day8.1 sample-data2) 26))

;; String -> Natural
;; Given a string return the number of easy digits
(define (count-easy-digits str)
  (~> str
      (string-split _)                            ; split it into words
      (map string-length _)                       ; create a list of word lengths
      (filter (λ (x) (member x easy-digits)) _)   ; keep only the easy digits
      (length _)))                                ; return number of digits 

(module+ test
  (check-equal? (count-easy-digits "fgae cfgab fg bagce") 2)
  (check-equal? (count-easy-digits "fgae gg fgggggh fgg") 4)
  (check-equal? (count-easy-digits "") 0))
  
(time (printf "2021 AOC Problem 8.1 = ~a\n" (day8.1 input)))

#|
  --- Part Two ---
 
 Through a little deduction, you should now be able to determine the remaining digits. 
 For each entry, determine all of the wire/segment connections and decode the four-digit
 output values. What do you get if you add up all of the output values?

 NOTES
 The real problem today is understanding the specification. Wow.
 Let me see if I can re-state the problem more clearly

 We have 200 displays consisting of four 7-segment numbers (the problem input)
 Each display is mis-wired differently.
 For each display we have observations for 10 unique wire combos representing
 the digits 0 through 9. And, after the |, we have the four that are actually turned on

 The challenge is to figure out which letters correspond to which segments on each display.

 The big leap in part two comes after the words "careful analysis." So the first thing I have
 to figure out is the deductive process to match each string to a digit. And again this is
 different for each display. It's kind of like Mastermind. The first four digits we know...

  wires  |  len  | digit |   segments    |  rule  
 -------------------------------------------------------------
 ab      |   2   |   1   |     3     6   |  length = 2      
 ab d    |   3   |   7   | 1   3     6   |  length = 3  
 ab  ef      4   |   4   |   2 3 4   6   |  length = 4      
 abcdefg |   7   |   8   | 1 2 3 4 5 6 7 |  length = 7
 abcd f  |   5   |   3   | 1   3 4   6 7 |  len (union d3 d1) = 5
 abcdef  |   6   |   9   | 1 2 3 4   6 7 |  (union d4 d3)
  bcdef  |   5   |   5   | 1 2   4   6 7 |  len (union d4 d5) = 6
 a cd fg |   5   |   2   | 1   3 4 5   7 |  len = 5
  bcdefg |   6   |   6   | 1 2   4 5 6 7 |  len (union d1 x) = 7
 abcde g |   6   |   0   | 1 2 3   5 6 7 |  len = 6

 OK now here we go. Honestly I bet this isn't much faster than just brute forcing the
 solution. There are only 7! tests x 200 measurements. It took me two days to figure
 out how to deduce the digits. This is likely a case of being too clever. Spend less
 time designing and let the computer do the work. Oh well.
|# 

;; Problem input from adventofcode.com
;; convert the string into a list of lists
;; (list '(the digits 0-9) '(the 4 display digits)) - one line per display
(define readings (~> (file->string "input8.txt")
                     (string-split _ "\n")
                     (map (λ (x) (string-split x " | ")) _)
                     (map (λ (x) (list (string-split (first x))
                                       (string-split (second x)))) _)))

(define sample-data3 (list '("acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab") '("cdfeb" "fcadb" "cdfeb" "cdbaf")))

(define another-sample '("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe" "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc" "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg" "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb" "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea" "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb" "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe" "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef" "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb" "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))

(define sample-data4 (~> another-sample
                         (map (λ (x) (string-split x " | ")) _)
                         (map (λ (x) (list (string-split (first x))
                                           (string-split (second x)))) _)))

(define (day8.2 measurements)
  (for/fold ([total 0]                   ; keep track of sum of each readout
             #:result total)             ; return the total sum
            ([m (in-list measurements)]) ; work through the provided measurements
    ; each call to get-value returns the value of the 4-digit readout
    (values (+ total (get-value m))))) 

(module+ test
  (check-equal? (day8.2 sample-data4) 61229))

;; (list-of (list-of String)) -> Natural
;; given one entry in the list of measurements (list '(0-9 wiring) '(4 digits for display)
;; produce the value the four digits represent
(define (get-value measurement)
  (local [(define digit-vector (get-digits (first measurement))) ; str representing digits from 0-9
          (define digits (for/list ([digit (in-list (second measurement))]) ; the four digits
                           (vector-member (sort-string digit) digit-vector)))
          (define (digits->value d1 d2 d3 d4)
            (+ (* d1 1000) (* d2 100) (* d3 10) d4))]
    (digits->value (first digits) (second digits) (third digits) (fourth digits))))

(module+ test
  (check-equal? (get-value sample-data3) 5353)
  (check-equal? (get-value (first sample-data4)) 8394)
  (check-equal? (get-value (second sample-data4)) 9781)
  (check-equal? (get-value (third sample-data4)) 1197)
  (check-equal? (get-value (fourth sample-data4)) 9361))

;; String -> String
;; sort the characters in a string
(define (sort-string str)
  (local [(define (string->chars s)
            (cond [(zero? (string-length s)) empty]
                  [else (cons (substring s 0 1) (string->chars (substring s 1)))]))]
    (apply string-append (sort (string->chars str) string<?))))

;; (list-of String) -> (list-of String)
;; sort each string in a list of strings alphabetically 
(define (sort-string-list los)
  (map sort-string los))

(define (union-strings s1 s2)
  (list->string (set-union (string->list s1) (string->list s2))))

;; (list-of String) -> (vector-of String)
;; Given the list of strings from the input, determine which strings
;; represent which digits, return a 10-string vector in containing
;; the strings representing the digits from 0 to 9
;; to get the digit that goes with a given string use (vector-member vec str)
(define (get-digits wire-strings)
  (let* ([ws (sort-string-list wire-strings)]   ; keep everything alphabetic for matching 
         [five-strings (filter (λ (x) (= 5 (string-length x))) ws)] ; set of strings with 6 char
         [six-strings (filter (λ (x) (= 6 (string-length x))) ws)]  ; set of strings with 5 char
         [d1 (first (filter (λ (x) (= 2 (string-length x))) ws))]   ; do the easy ones first
         [d7 (first (filter (λ (x) (= 3 (string-length x))) ws))]
         [d4 (first (filter (λ (x) (= 4 (string-length x))) ws))]
         [d8 (first (filter (λ (x) (= 7 (string-length x))) ws))]
         ; now we get to the deductions
         ; e.g. d3 is the only five char string that's still lenghth five after union w/ d1
         [d3 (first (filter (λ (x) (= 5 (string-length (union-strings x d1)))) five-strings))]
         [five-strings (remove d3 five-strings)]  ; remove strings that have been used
         [d9 (sort-string (union-strings d4 d3))] ; the union of d3 and d4
         [six-strings (remove d9 six-strings)]
         [d5 (first (filter (λ (x) (= 6 (string-length (union-strings x d4)))) five-strings))]
         [five-strings (remove d5 five-strings)]
         [d2 (first five-strings)]
         [d6 (first (filter (λ (x) (= 7 (string-length (union-strings x d1)))) six-strings))]
         [six-strings (remove d6 six-strings)]
         [d0 (first six-strings)])
    (list->vector (list d0 d1 d2 d3 d4 d5 d6 d7 d8 d9))))

(module+ test
  (check-equal? (get-digits (first sample-data3))
                (list->vector
                 (sort-string-list
                  '("cagedb" "ab" "gcdfa" "fbcad" "eafb" "cdfbe" "cdfgeb" "dab" "acedgfb" "cefabd")))))

(time (printf "2021 AOC Problem 8.2 = ~a\n" (day8.2 readings)))

#|

Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

2021 AOC Problem 8.1 = 301
cpu time: 0 real time: 0 gc time: 0
2021 AOC Problem 8.2 = 908067
cpu time: 6 real time: 6 gc time: 0 (it IS pretty fast though!)

 Real world timing

      --------Part 1---------   --------Part 2--------
Day       Time    Rank  Score       Time   Rank  Score
  8       >24h   61217      0       >24h  55574      0
  7       >24h   68317      0       >24h  66595      0
  6       >24h   71604      0       >24h  67093      0
  5       >24h   71260      0       >24h  68517      0
  4       >24h   77972      0       >24h  75701      0
  3       >24h  102352      0       >24h  88448      0
  2   00:38:41   13051      0   01:06:26  14647      0
  1   00:24:22    7349      0   12:27:40  59726      0
|#