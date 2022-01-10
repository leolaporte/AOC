#lang racket

;  --- Day 2: Password Philosophy ---
; 
; Your flight departs in a few days from the coastal airport; the easiest way
; down to the coast from here is via toboggan.
; 
; The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day.
; "Something's wrong with our computers; we can't log in!" You ask if you can
; take a look.
; 
; Their password database seems to be a little corrupted: some of the passwords
; wouldn't have been allowed by the Official Toboggan Corporate Policy that was
; in effect when they were chosen.
; 
; To try to debug the problem, they have created a list (your puzzle input) of
; passwords (according to the corrupted database) and the corporate policy when
; that password was set.
; 
; For example, suppose you have the following list:
; 
; 1-3 a: abcde
; 1-3 b: cdefg
; 2-9 c: ccccccccc
; 
; Each line gives the password policy and then the password. The password policy
; indicates the lowest and highest number of times a given letter must appear for
; the password to be valid. For example, 1-3 a means that the password must
; contain a at least 1 time and at most 3 times.
; 
; In the above example, 2 passwords are valid. The middle password, cdefg, is not;
; it contains no instances of b, but needs at least 1. The first and third
; passwords are valid: they contain one a or nine c, both within the limits of
; their respective policies.
; 
; How many passwords are valid according to their policies?
;  
; 


;; provided by AOC:
(define password-list (file->lines "input2.txt")) ; import problem set data
          
(define LOP1 '("1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc")) ; example for tests
(define pass-string (regexp "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)")) ; pre-compile regex

;; String -> Integer
;; given a line from the password list, parse it and test to
;; see if the password conforms to the given rule
;; returns 1 if the password satisfies the rule, otherwise 0
(define (password-check password-line)
  (local [(define parsed-line (rest (regexp-match pass-string password-line)))
          (define mn (string->number (first parsed-line)))
          (define mx (string->number (second parsed-line)))
          (define chr (third parsed-line))
          (define pwd (string->list (fourth parsed-line)))
          (define char-count (count (λ (x) (equal? chr (string x))) pwd))]
    (if (and (<= mn char-count) (>= mx char-count))
        1
        0)))
           
(module+ test
  (require rackunit)
  (check-equal? (password-check (first LOP1)) 1)
  (check-equal? (password-check (second LOP1)) 0)
  (check-equal? (password-check (third LOP1)) 1)
  (check-equal? (foldl + 0 (map password-check LOP1)) 2))

(printf "AOC Problem 2.1 = ~a\n" (foldl + 0 (map password-check password-list)))
        
;  --- Part Two ---
; 
; While it appears you validated the passwords correctly, they don't seem to be
; what the Official Toboggan Corporate Authentication System is expecting.
; 
; The shopkeeper suddenly realizes that he just accidentally explained the password
; policy rules from his old job at the sled rental place down the street! The
; Official Toboggan Corporate Policy actually works a little differently.
; 
; Each policy actually describes two positions in the password, where 1 means
; the first character, 2 means the second character, and so on.
; (Be careful; Toboggan Corporate Policies have no concept of "index zero"!)
;     Exactly one of these positions must contain the given letter. Other
;     occurrences of the letter are irrelevant for the purposes of policy
;     enforcement.
;     
; Given the same example list from above:
; 
;     1-3 a: abcde is valid: position 1 contains a and position 3 does not.
;     1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
;     2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
; 
; How many passwords are valid according to the new interpretation of the policies?
; 
; 


;; same signature as above but the rules are slightly different
(define (new-password-check password-line)
  (local [(define parsed-line (rest (regexp-match pass-string password-line)))
          (define p1 (sub1 (string->number (first parsed-line)))) ;  correct for 0 offset
          (define p2 (sub1 (string->number (second parsed-line))))
          (define chr (third parsed-line))
          (define pwd (fourth parsed-line))
          (define pos1? (equal? chr (substring pwd p1 (add1 p1))))
          (define pos2? (equal? chr (substring pwd p2 (add1 p2))))]
    (if (xor pos1? pos2?) ; exactly one matches
        1
        0)))
           
(module+ test
  (require rackunit)
  (check-equal? (new-password-check (first LOP1)) 1)
  (check-equal? (new-password-check (second LOP1)) 0)
  (check-equal? (new-password-check (third LOP1)) 0)
  (check-equal? (foldl + 0 (map new-password-check LOP1)) 1))

(printf "AOC Problem 2.2 = ~a\n" (foldl + 0 (map new-password-check password-list)))
