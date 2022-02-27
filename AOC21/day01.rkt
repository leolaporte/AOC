#lang racket
#|
 AOC 2021
 --- Day 1: Sonar Sweep ---
 Leo Laporte 1-Dec-2021
  
 How many measurements are larger than the previous measurement?
|#

(require racket/file
         rackunit)

(define sample-data (list 199 200 208 210 200 207 240 269 260 263))

;; Problem input from adventofcode.com
(define day1data (map string->number (string-split (file->string "input1.txt"))))

;; (list-of integer) -> integer
;; given a list of numbers produce the number of measurements that are larger than the previous 
(define (day1.1 list)
  
  (define (count-increases count l)
    (cond ((empty? (rest l)) count)
          (else (count-increases
                 (+ count (if (> (second l) (first l)) 1 0))
                 (rest l)))))
  
  (count-increases 0 list))

(module+ test
  (check-equal? (day1.1 sample-data) 7))

(time (printf "2021 AOC Problem 1.1 = ~a\n" (day1.1 day1data)))

#|  --- Part Two ---

 Consider sums of a three-measurement sliding window. How many sums are larger than
 the previous sum?
|# 

(define (day1.2 readings)
  
  (define (rolling-average-increases count prev lst)
    (cond [(empty? (cddr lst)) count] ; too few readings left, done
          [else (let* ([sum (+ (first lst) (second lst) (third lst))]
                       [count (if (> sum prev) (add1 count) count)])
                  (rolling-average-increases count sum (rest lst)))])) 
  
  (sub1 (rolling-average-increases 0 0 readings))) ; discount first reading - it's always > 0

(module+ test
  (check-equal? (day1.2 sample-data) 5))

(time (printf "2021 AOC Problem 1.2 = ~a\n" (day1.2 day1data)))

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

2021 AOC Problem 1.1 = 1342
cpu time: 0 real time: 0 gc time: 0
2021 AOC Problem 1.2 = 1378
cpu time: 0 real time: 0 gc time: 0

 Real world timing
 Day       Time  Rank  Score       Time   Rank  Score
  1   00:24:22  7349      0   12:27:40  59726      0
|#