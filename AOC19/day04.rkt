#lang racket

;  --- Day 4: Secure Container ---
; 
; You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the
; password on a sticky note, but someone threw it out.
; 
; However, they do remember a few key facts about the password:
; 
;     It is a six-digit number.
;     The value is within the range given in your puzzle input.
;     Two adjacent digits are the same (like 22 in 122345).
;     Going from left to right, the digits never decrease;
;     they only ever increase or stay the same (like 111123 or 135679).
; 
; Other than the range rule, the following are true:
; 
;     111111 meets these criteria (double 11, never decreases).
;     223450 does not meet these criteria (decreasing pair of digits 50).
;     123789 does listnot meet these criteria (no double).
;  
; How many different passwords within the range given in your puzzle input meet these criteria?
; 
; Your puzzle input is 197487-673251.


(require rackunit)

(define START 197487)
(define END 673251)
(define DOUBLED-DIGITS-REGEX (pregexp "(\\d)\\1+")) ; precompile for speed

;; Number -> Boolean
;; given a number only returns true if any two adjacent digits are the same
(define (has-double-digits? n)
  (regexp-match? DOUBLED-DIGITS-REGEX (number->string n)))

(check-equal? (has-double-digits? 11) #t)
(check-equal? (has-double-digits? 10) #f)
(check-equal? (has-double-digits? 123454) #f)
(check-equal? (has-double-digits? 123445) #t)
(check-equal? (has-double-digits? 1234445) #t) ;; this will fail Part 2

;; Number -> Boolean
;; Given a Number, return true if the digits,
;; from left to right, never decrease
(define (never-decreasing? n)
  (local [(define s (number->string n))
          (define len (string-length s))]
    (for/and ([i (in-range (- len 1))])
      (<= (string->number (substring s i (+ i 1)))
          (string->number (substring s (+ i 1) (+ i 2)))))))
          
(check-equal? (never-decreasing? 12345) #t)
(check-equal? (never-decreasing? 23450) #f)

;; Number Number -> List
;; given a range of numbers, produce a list that satisfies two conditions:
;; 1) Two adjacent digits are the same (like 22 in 122345) and
;; 2) Going from left to right, the digits never decrease
 
(define (test-pwd start finish)
  (for/list ([pwd (in-range start (+ finish 1))]
             #:when (and (has-double-digits? pwd)
                         (never-decreasing? pwd)))
    pwd))

(check-equal? (test-pwd 1123 1125) (list 1123 1124 1125))

(printf "The solution to 4.1 is ~a\n" (length (test-pwd START END)))

;  --- Part Two ---
; 
; An Elf just remembered one more important detail: the two adjacent matching digits are
; not part of a larger group of matching digits.
; 
; Given this additional criterion, but still ignoring the range rule, the following are
; now true:
; 
;     112233 meets these criteria because the digits never decrease and all repeated
;     digits are exactly two digits long.
; 
;     123444 no longer meets the criteria (the repeated 44 is part of a larger group of
;     444).
; 
;     111122 meets the criteria (even though 1 is repeated more than twice, it still
;     contains a double 22).
; 
; How many different passwords within the range given in your puzzle input meet all of the
; criteria?
; 
; Your puzzle input is still 197487-673251.


(define (just-double-digits? n)
  (local [(define lom (regexp-match* DOUBLED-DIGITS-REGEX (number->string n))) ; list of all matches
          (define (length2? s) (= (string-length s) 2))]                 ; are any exactly 2 digits?
    (ormap length2? lom)))

(check-equal? (just-double-digits? 12345) #f)
(check-equal? (just-double-digits? 112345) #t)
(check-equal? (just-double-digits? 1112345) #f)
(check-equal? (just-double-digits? 11122233344455) #t)

;; Number Number -> (list-of Number)
;; a more picky version...
;; given a range of numbers, produce a list that satisfies two conditions:
;; 1) Two adjacent digits are the same (like 22 in 122345)
;; _and_
;; the two adjacent matching digits are not part of a larger group of matching digits
;; 2) Going from left to right, the digits never decrease

(define (test-pwd.2 start finish)
  (for/list ([pwd (in-range start (+ finish 1))]
             #:when (and (just-double-digits? pwd)
                         (never-decreasing? pwd)))
    pwd))

(check-equal? (test-pwd.2 12340 12345) (list 12344))
(check-equal? (test-pwd.2 123440 123445) (list 123445))

(printf "The solution to 4.2 is ~a\n" (length (test-pwd.2 START END)))
