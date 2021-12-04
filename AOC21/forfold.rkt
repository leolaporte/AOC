#lang racket

(require rackunit)

(define day2data (string-split (file->string "input2.txt") "\n"))

(define sample-data (list "forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2")) ; for test

(define (process lst)
  (for*/fold ([forward 0]
             [depth 0]
             #:result (* forward depth))
             ([move (in-list lst)])
    (match (cons) (first (car (regexp-match #px"(.+)\\s(\\d)" move))
      

      )

(module+ test
  (check-equal? (process sample-data) 150))