;; 2020 AOC Solutions 
;; Leo Laporte
;; Day 6

#lang racket

;  --- Day 6: Custom Customs ---
; 
; As your flight approaches the regional airport where you'll switch to a much
; larger plane, customs declaration forms are distributed to the passengers.
; 
; The form asks a series of 26 yes-or-no questions marked a through z. All you
; need to do is identify the questions for which anyone in your group answers
; "yes". Since your group is just you, this doesn't take very long.
; 
; However, the person sitting next to you seems to be experiencing a language
; barrier and asks if you can help. For each of the people in their group, you
; write down the questions for which they answer "yes", one per line. For example:
; 
; abcx
; abcy
; abcz
; 
; In this group, there are 6 questions to which anyone answered "yes": a, b, c,
; x, y, and z. (Duplicate answers to the same question don't count extra; each
;                         question counts at most once.)
; 
; Another group asks for your help, then another, and eventually you've collected
; answers from every group on the plane (your puzzle input). Each group's answers
; are separated by a blank line, and within each group, each person's answers are
; on a single line. For example:
; 
; abc
; 
; a
; b
; c
; 
; ab
; ac
; 
; a
; a
; a
; a
; 
; b
; 
; This list represents answers from five groups:
; 
; The first group contains one person who answered "yes" to 3 questions: a, b,
; and c.
; 
; The second group contains three people; combined, they answered "yes" to 3
; questions: a, b, and c.
; 
; The third group contains two people; combined, they answered "yes" to 3
; questions: a, b, and c.
; 
; The fourth group contains four people; combined, they answered "yes" to only
; 1 question, a.
; 
; The last group contains one person who answered "yes" to only 1 question, b.
; In this example, the sum of these counts is 3 + 3 + 3 + 1 + 1 = 11.
; 
; For each group, count the number of questions to which anyone answered "yes".
; 
; What is the sum of those counts?


(require racket/set)

;; provided by AOC:
(define INPUT (file->string "input6.txt"))
(define EXAMPLE "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n\n")

;; String -> Integer
;; Given the yes answers from a group of people, produce the total number of
;; questions that were answered yes in each group
(define (yes-count str)
  (foldr + 0 (map count-unique (make-group-list str))))

(module+ test
  (require rackunit)
  (check-equal? (yes-count EXAMPLE) 11))

;; String -> Integer
;; given a string, return the number of unique characters in that string
(define (count-unique str)
  (length (remove-duplicates (string->list str))))

(module+ test
  (check-equal? (count-unique "abc") 3)
  (check-equal? (count-unique "abac") 3)
  (check-equal? (count-unique "aaaa") 1))

;; String -> (list-of String)
;; utility function
;; given the AOC input as one long String, produce a list of strings, one per group
(define (make-group-list str)
  (local [(define (replace-newlines str)
            (string-replace str "\n" ""))]
    (map replace-newlines (string-split str "\n\n"))))

(module+ test
  (check-equal? (make-group-list EXAMPLE)
                (list "abc" "abc" "abac" "aaaa" "b")))

(time (printf "AOC Problem 6.1 = ~a\n" (yes-count INPUT)))
        
;  --- Part Two ---
; 
; As you finish the last group's customs declaration, you notice that you
; misread one word in the instructions:
; 
; You don't need to identify the questions to which anyone answered "yes"; you
; need to identify the questions to which everyone answered "yes"!
; 
; Using the same example as above:
; 
; abc
; 
; a
; b
; c
; 
; ab
; ac
; 
; a
; a
; a
; a
; 
; b
; 
; This list represents answers from five groups:
; 
; In the first group, everyone (all 1 person) answered "yes" to 3 questions: a,
; b, and c.
; 
; In the second group, there is no question to which everyone answered "yes".
; 
; In the third group, everyone answered yes to only 1 question, a. Since some
; people did not answer "yes" to b or c, they don't count.
; 
; In the fourth group, everyone answered yes to only 1 question, a.
; 
; In the fifth group, everyone (all 1 person) answered "yes" to 1 question, b.
; 
; In this example, the sum of these counts is 3 + 0 + 1 + 1 + 1 = 6.
; 
; For each group, count the number of questions to which everyone answered "yes".
; What is the sum of those count


;; String -> (list-of (list-of String))
;; given a string of survey responses, create a list of groups,
;; each group containing a list of each person's survey responses in a strng
(define (survey-list str)
  (local [(define (split s) (string-split s "\n"))]
    (map split (string-split str "\n\n"))))

;; (list-of String) -> (list-of Char)
;; Given a list of Strings (one group) produce a (list-of Char) that
;; contains any characters that appear in all the strings
(define (common-chars str)
  (set->list (apply set-intersect (map (compose list->set string->list) str))))
  
(module+ test
  (check-equal? (common-chars '("rce" "w")) empty)
  (check-equal? (common-chars
                 '("nwjie" "xulkaegsy" "vipehm" "oevcrbf" "jme"))
                '(#\e))
  (check-equal? (common-chars ' ("qepdrhamt" "ifnd" "nxfdy")) '(#\d)))

(time (printf "AOC Problem 6.2 = ~a\n"
              (foldr + 0
                     (map length
                          (map common-chars (survey-list INPUT))))))