#lang racket
;  AOC 2021
; --- Day 1: Sonar Sweep ---
; Leo Laporte 1-Dec-2021
; 
; You're minding your own business on a ship at sea when the overboard alarm goes off!
; You rush to see if you can help. Apparently, one of the Elves tripped and accidentally
; sent the sleigh keys flying into the ocean!
; 
; Before you know it, you're inside a submarine the Elves keep ready for situations like
; this. It's covered in Christmas lights (because of course it is), and it even has an
; experimental antenna that should be able to track the keys if you can boost its signal
; strength high enough; there's a little meter that indicates the antenna's signal strength
; by displaying 0-50 stars.
; 
; Your instincts tell you that in order to save Christmas, you'll need to get all fifty
; stars by December 25th.
; 
; Collect stars by solving puzzles. Two puzzles will be made available on each day in
; the Advent calendar; the second puzzle is unlocked when you complete the first. Each
; puzzle grants one star. Good luck!
; 
; As the submarine drops below the surface of the ocean, it automatically performs a
; sonar sweep of the nearby sea floor. On a small screen, the sonar sweep report
; (your puzzle input) appears: each line is a measurement of the sea floor depth as
; the sweep looks further and further away from the submarine.
; 
; For example, suppose you had the following report:
; 
; 199
; 200
; 208
; 210
; 200
; 207
; 240
; 269
; 260
; 263
; 
; This report indicates that, scanning outward from the submarine, the sonar sweep
; found depths of 199, 200, 208, 210, and so on.
; 
; The first order of business is to figure out how quickly the depth increases, just
; so you know what you're dealing with - you never know if the keys will get carried
; into deeper water by an ocean current or a fish or something.
; 
; To do this, count the number of times a depth measurement increases from the previous
; measurement. (There is no measurement before the first measurement.) In the example
; above, the changes are as follows:
; 
; 199 (N/A - no previous measurement)
; 200 (increased)
; 208 (increased)
; 210 (increased)
; 200 (decreased)
; 207 (increased)
; 240 (increased)
; 269 (increased)
; 260 (decreased)
; 263 (increased)
; 
; In this example, there are 7 measurements that are larger than the previous measurement.
; 
; How many measurements are larger than the previous measurement?


(require racket/file rackunit)

(define sample-data (list 199 200 208 210 200 207 240 269 260 263))

;; Problem input from adventofcode.com
(define day1data (map string->number (string-split (file->string "input1.txt"))))

;; (list-of integer) -> integer
;; given a list of numbers produce the number of measurements that are larger than the previous 
(define (day1.1 list)
  (local [(define (count-increases count l)
            (cond ((empty? (rest l)) count)
                  (else (count-increases
                         (+ count (if (> (second l) (first l)) 1 0))
                         (rest l)))))] ; not tail recursive. I promise to do better next time.
    (count-increases 0 list)))

(module+ test
  (check-equal? (day1.1 sample-data) 7))

(time (printf "2021 AOC Problem 1.1 = ~a\n" (day1.1 day1data)))

;  --- Part Two ---
; 
; Considering every single measurement isn't as useful as you expected: there's just
; too much noise in the data.
; 
; Instead, consider sums of a three-measurement sliding window. Again considering the
; above example:
; 
; 199  A      
; 200  A B    
; 208  A B C  
; 210    B C D
; 200  E   C D
; 207  E F   D
; 240  E F G  
; 269    F G H
; 260      G H
; 263        H
; 
; Start by comparing the first and second three-measurement windows. The measurements
; in the first window are marked A (199, 200, 208); their sum is 199 + 200 + 208 = 607.
; The second window is marked B (200, 208, 210); its sum is 618. The sum of measurements
; in the second window is larger than the sum of the first, so this first comparison increased.
; 
; Your goal now is to count the number of times the sum of measurements in this sliding
; window increases from the previous sum. So, compare A with B, then compare B with C,
; then C with D, and so on. Stop when there aren't enough measurements left to create a
; new three-measurement sum.
; 
; In the above example, the sum of each three-measurement window is as follows:
; 
; A: 607 (N/A - no previous sum)
; B: 618 (increased)
; C: 618 (no change)
; D: 617 (decreased)
; E: 647 (increased)
; F: 716 (increased)
; G: 769 (increased)
; H: 792 (increased)
; 
; In this example, there are 5 sums that are larger than the previous sum.
; 
; Consider sums of a three-measurement sliding window. How many sums are larger than
; the previous sum?
; 


(define sample-data2 (list 199 200 208 210 200 207 240 269 260 263))

(define (day1.2 readings)
  (local [(define (rolling-average-increases count prev lst)
            (cond [(empty? (cddr lst)) count] ; too few readings left, done
                  [else (let* ([sum (+ (first lst) (second lst) (third lst))]
                              [count (if (> sum prev) (add1 count) count)])
                          (rolling-average-increases count sum (rest lst)))]))] ; tail recursive this time!
    (sub1 (rolling-average-increases 0 0 readings)))) ; discount first reading - it's always > 0

(module+ test
  (check-equal? (day1.2 sample-data2) 5))

 (time (printf "2021 AOC Problem 1.2 = ~a\n" (day1.2 day1data)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM
; 2021 AOC Problem 1.1 = 1342
; cpu time: 1 real time: 1 gc time: 0
; 2021 AOC Problem 1.2 = 1378
 ;cpu time: 1 real time: 1 gc time: 0

; Real world timing
; Day       Time  Rank  Score       Time   Rank  Score
;  1   00:24:22  7349      0   12:27:40  59726      0