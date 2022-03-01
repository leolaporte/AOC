#lang racket
;  AOC 2021
; Leo Laporte 28-29 Dec 2021

#|

 --- Day 10: Syntax Scoring ---
  
 A corrupted line is one where a chunk closes with the wrong character - that
 is, where the characters it opens and closes with do not form one of the four
 legal pairs () [] {} and <>

 To calculate
 the syntax error score for a line, take the first illegal character on
 the line and look it up in the following table:
 
 ): 3 points.
 ]: 57 points.
 }: 1197 points.
 >: 25137 points.
 
 Find the first illegal character in each corrupted line of the navigation
 subsystem. What is the total syntax error score for those errors?
|#

(require threading
         rackunit)

;; Problem input from adventofcode.com
(define problem-data (string-split (file->string "input10.txt") "\n"))

(define test-data
  (string-split "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"
                "\n"))

#|
 NOTES:
 to simplify an expression look for matching pairs () {} [] <> and eliminate them.
 Once an expression cannot be simplified further you've found a bug or an incomlete
 statement.
 The incomplete statement will have no closing delimiters.
 If statement is not incomplete it's bugged.
 The first closing delimiter is the bug.
|#

;; Regexp patterns
(define PAIRS (regexp "<>|\\[\\]|\\{\\}|\\(\\)")) ; matching opens and closes
(define CLOSES (regexp ">|\\]|\\}|\\)"))          ; the closing delimiters

;; String -> String
;; Removes matching pairs from string (repeat until no more pairs)
(define (strip-pairs str)
  (let ([shrunk (regexp-replace* PAIRS str "")])       
    
    (cond [(false? (regexp-match PAIRS shrunk)) shrunk]  
          [else (strip-pairs shrunk)])))                 

;; String -> Natural
;; Given a string return the score of the first bug or 0 if no bugs
(define (bug-bounty str)
  (match (regexp-match CLOSES str)
    ['(")") 3]
    ['("]") 57]
    ['("}") 1197]
    ['(">") 25137]
    [#f 0]))

(module+ test
  (check-equal? (bug-bounty (strip-pairs "{([(<{}[<>[]}>{[]{[(<()>")) 1197)
  (check-equal? (bug-bounty (strip-pairs "[[<[([]))<([[{}[[()]]]")) 3)
  (check-equal? (bug-bounty (strip-pairs "[{[{({}]{}}([{[{{{}}([]")) 57)
  (check-equal? (bug-bounty (strip-pairs "[<(<(<(<{}))><([]([]()")) 3)
  (check-equal? (bug-bounty (strip-pairs "<{([([[(<>()){}]>(<<{{")) 25137) 
  (check-equal? (bug-bounty (strip-pairs "[({(<(())[]>[[{[]{<()<>>")) 0))

;; (list-of String) -> Natural
;; Given a list of navigation strings return the total bug-bounty for
;; strings with errors
(define (day10.1 input)
  (for/sum ([str (in-list input)])
    (bug-bounty (strip-pairs str))))

(module+ test
  (check-equal? (day10.1 test-data) 26397))

(time (printf "2021 AOC Problem 10.1 = ~a\n" (day10.1 problem-data)))

#|
  --- Part Two ---
 
 Now, discard the corrupted lines. The remaining lines are incomplete.
 
 Incomplete lines don't have any incorrect characters - instead, they're missing
 some closing characters at the end of the line. To repair the navigation subsystem,
 you just need to figure out the sequence of closing characters that complete all
 open chunks in the line.
 
 Find the completion string for each incomplete line, score the completion
 strings, and sort the scores. What is the middle score?
 
|#

;; Notes: Easy peasy. We already have the routines we need to find the missing
;; closing characters, except we don't need to. Strip-pairs reduces us to the
;; opening chars in order. Score each string, find the middle and bob's your uncle. 

(define (day10.2 input)
  (~> (map strip-pairs input)  ; reduce all strings to unmatched char pairs
      (filter complete? _)     ; eliminate buggy strings
      (map string->list _)     ; convert strings to lists of chars
      (map reverse _)          ; flip each string
      (map score _)            ; calculate the score for each string
      (middle _)))             ; find the middle score

;; String -> Boolean
;; returns true if str is complete (not buggy)
(define (complete? str)
  (false? (regexp-match CLOSES str)))

(module+ test
  (check-equal? (complete? "{{[[({([") #t)
  (check-equal? (complete? "{{[[({([>") #f))

;; (list-of Char) -> Natural
;; Calculates the score of a list of chars (note we're matching the left
;; side of the pair because that's what we've got).
(define (score loc)
  (foldl (λ (c total)
           (+ (* total 5)
              (match c
                [#\( 1]
                [#\[ 2]
                [#\{ 3]
                [#\< 4])))
         0 loc))
  
(module+ test
  (check-equal? (score (string->list "{{[[({([")) 288957)
  (check-equal? (score (string->list "({<[{(")) 5566)
  (check-equal? (score (string->list "{{<{<((((")) 1480781))

;; (list-of Natural) -> Natural
;; given an odd length list of numbers, return the value in the center of the list
(define (middle lst)
  (let ([l (length lst)])
    (if (odd? l)
        (~> (sort lst <)
            (list-tail _ (quotient l 2))
            (first _))
        ("Error: list is not odd"))))

(module+ test
  (check-equal? (middle '(1 2 3 4 5)) 3)
  (check-equal? (middle '(1 2 3 4 5 6 7)) 4))
     
(module+ test
  (check-equal? (day10.2 test-data) 288957))

(time (printf "2021 AOC Problem 10.2 = ~a\n" (day10.2 problem-data)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

; 2021 AOC Problem 10.1 = 392043
; cpu time: 7 real time: 7 gc time: 0
; 2021 AOC Problem 10.2 = 1605968119
; cpu time: 6 real time: 6 gc time: 0
; 
