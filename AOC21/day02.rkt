#lang racket

#|
  AOC 2021
 Leo Laporte, 1 December 2021

 Calculate the horizontal position and depth you would have after following
 the planned course. What do you get if you multiply your final horizontal
 position by your final depth?
|#

(require racket/file
         rackunit)

;; Problem input from adventofcode.com
(define day2data (string-split (file->string "input2.txt") "\n")) ; turn it into a list-of moves

(define sample-data (list "forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2")) ; for test

(define cmd-str (pregexp "(down|up|forward)\\s(\\d)"))  ; regex for the individual moves

;; (list-of string) -> Integer
;; given a list of moves, return the resulting position as the product of the distance and depth
(define (day2.1 lst)
  
  (define (make-moves horiz depth l)
    (cond [(empty? l) (* horiz depth)]  ; done, return product of horiz and depth
          [else (let ([pos (make-one-move horiz depth (first l))]) ; process move
                  (make-moves (car pos) (cdr pos) (rest l)))]))  ; get next move
  
  (make-moves 0 0 lst))

;; integer integer string -> (integer . integer)
;; given a move and the current position, return the new position (horiz . depth)
(define (make-one-move horiz depth move)
  (let* ([m (regexp-match cmd-str move)]     ; convert the string to a list of dir and dist
         [dir (second m)]                    ; direction of move
         [dist (string->number (third m))])  ; distance of move
    (match dir
      ("forward" (cons (+ horiz dist) depth)) ; forward by dist
      ("up" (cons horiz (- depth dist)))      ; surface by dist
      ("down" (cons horiz (+ depth dist)))))) ; dive by dist

(module+ test
  (check-equal? (day2.1 sample-data) 150))

(time (printf "2021 AOC Problem 2.1 = ~a\n" (day2.1 day2data)))

#|
  --- Part Two ---
 Using this new interpretation of the commands, calculate the horizontal position and
 depth you would have after following the planned course. What do you get if you multiply
 your final horizontal position by your final depth?
|# 

;; (list-of string) -> Integer
;; given a list of dir and aim, return the resulting position as the product of the distance and depth
(define (day2.2 lst)
  (local [(define (make-moves dist aim depth l)
            (cond [(empty? l) (* dist depth)]  ; done, return product of horiz and depth
                  [else (let ([pos (aim-fire dist aim depth (first l))]) ; process move
                          (make-moves (first pos) (second pos) (third pos) (rest l)))]))]   ; get next move
    (make-moves 0 0 0 lst)))

;; integer integer string -> (list-of integer integer integer)
;; given a move and the current aim and position, return the new aim and position (list dist aim depth)
(define (aim-fire dist aim depth move)
  (let* ([m (regexp-match cmd-str move)]   ; convert the string to a list of dir and dist
         [aim-or-move (second m)]          ; either aim or fire
         [d (string->number (third m))])   ; distance to move or aim
    (match aim-or-move
      ("forward" (list (+ dist d) aim (+ depth (* aim d)))) ; make a move based on dist and aim
      ("up" (list dist (- aim d) depth))                    ; adjust aim upward
      ("down" (list dist (+ aim d) depth)))))               ; adjust aim downward
  
(module+ test
  (check-equal? (day2.2 sample-data) 900))

(time (printf "2021 AOC Problem 2.2 = ~a\n" (day2.2 day2data)))


#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

2021 AOC Problem 2.1 = 1524750
cpu time: 0 real time: 0 gc time: 0
2021 AOC Problem 2.2 = 1592426537
cpu time: 0 real time: 0 gc time: 0

Real world timing

      --------Part 1--------   --------Part 2--------
Day       Time   Rank  Score       Time   Rank  Score
  2   00:38:41  13051      0   01:06:26  14647      0
  1   00:24:22   7349      0   12:27:40  59726      0

|#