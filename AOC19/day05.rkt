#lang racket

;; AoC 2019 Day 5
;; https://adventofcode.com/2019/day/5
;;
;; 12/24/19 Part 1 solved
;; 12/25/19 Part 2 solved
;; Leo Laporte

(define DATA "input5.txt")
(require "intcode.rkt")

; read in the data
(define TEST (vector->immutable-vector                                  ; make it immutable for safety
              (list->vector                                             ; make a vector
               (map string->number                                      ; turn data into list of numbers
                    (string-split (string-trim (file->string DATA)) ",")))))

;; Part 1 (input code 1)
(run-intcode (vector-copy TEST) 0 1)
; Final diagnostic code: 15426686

;; Part 2 (input code 5)
(run-intcode (vector-copy TEST) 0 5)
; 11430197
