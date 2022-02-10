#lang racket

;AOC Day 17
;Leo Laporte 31-Jan-2022

#|==============================================================================

--- Day 17: Trick Shot ---

The probe's x,y position starts at 0,0. Then, it will follow some trajectory by
moving in steps. On each step, these changes occur in the following order:

   * The probe's x position increases by its x velocity.
   * The probe's y position increases by its y velocity.
   * Due to drag, the probe's x velocity changes by 1 toward the value 0;
     that is, it decreases by 1 if it is greater than 0, increases by 1 if it is
     less than 0, or does not change if it is already 0.
   * Due to gravity, the probe's y velocity decreases by 1.

Find the initial velocity that causes the probe to reach the highest y position
and still eventually be within the target area after any step. What is the
highest y position it reaches on this trajectory?
            
================================================================================|#

(require rackunit
         threading
         racket/trace)

#|==============================================================================|#
#|                                      DATA                                    |#
#|==============================================================================|#

; Data is pretty simple...
; "target area: x=248..285, y=-85..-56"

;; String -> (list-of Number)
;; given the problem provided coordinate string, return a list of values
;; defining a target rectangle containing four points:
;; (top-left, top-right, bottom-left, bottom-right)
(define (target str) (map string->number (regexp-match* #px"(\\d+)" str)))

#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#

#|
First instinct is to brute force it. Let's see how fast that is.

- I'll need an object, probe, to record the current state of the probe.
- a function, step, that advances the probe one step.
- a function, flight, that repeats until probe hits the target or passes out of reach.
- a boolean, hit-target?, that tells us whether the probe has hit the target
- a boolean, in-range? that tells us whether the probe is out of range (stop IOW)
- a hash, hits, with starting velocity as the key and max-y as the value.

Only one more question: what range of velocities should I try? I don't want to
search an infinite set. Ah this is what makes it not entirely brute force. Can I
reason out what the ranges of x-v and y-v can be?

x-v has to be > 1 or we go straight up or down. Or both. And since it slows
down by one every step it has to have enough velocity to make it to the left
edge of the target so:

x-v has to be high enough so as not to peter out (related to y) 
y-v has to be high enough to overcome gravity (related to x)

It's possible to come up with values that pass right through the target -
the rules are very specific that a hit must occur at the end of a step.

Ok but here's a weird question, if y-velocity hits zero does it then
go NEGATIVE?? in which case it bounces back?? That's pathological but
not prevented by the problem statement.

|#

(struct probe (x y xv yv) #:transparent) ; contains the current state of the probe
(define hits (make-hash))  ; contains the max-y of all successful launches

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

;; Probe -> Probe
;; given a probe position return the next position according to the rules provided
(define (step p)
  (probe
   (let ([next-x (+ (probe-x p) (probe-xv p))])
     (cond [(<= next-x 0) 0]
           [(< next-x 0) (add1 next-x)]
           [(> next-x 0) (sub1 next-x)]))
   (sub1 (+ (probe-y p) (probe-yv p)))
   (probe-xv p) (probe-yv p)))

(module+ test
  (check-equal? (step (probe 0 0 0 0)) (probe 0 -1 0 0))
  (check-equal? (step (probe -1 2 7 8)) (probe 5 9 7 8)))
    
;; Probe -> Natural or #f
;; Given a probe return the maximum y position 
;; if the probe hits the target or #f if it misses
;; !!!
(define (flight p) 0 )

;; Probe Target -> Boolean
;; returns true if the probe has hit the target
(define (hit-target? p t)
  (let ([x (probe-x p)]
        [y (probe-y p)])
    (and (>= x (first t))
         (<= x (second t))
         (>= y (third t))
         (<= y (fourth t))))) 

;; Probe Target -> Boolean
;; returns true if it's still possible to hit target
(define (in-range? p t)
  (let ([x (probe-x p)]
        [y (probe-y p)])
    (or (and (positive? (probe-xv p)) (> x (second t)))     ; x is past
        (and (positive? (probe-yv p)) (< y (fourth t))))))  ; y is past


;(module+ test
;  (check-equal? (day17.1 '(20 30 -10 -5) 45))

; (time (printf "2021 AOC Problem 16.1 = ~a\n" (day17.1 (target (file->string "input17.txt")))

#|=================================================================================
                                        PART 2
                               

==================================================================================|#

;(module+ test
;  (check-equal? (day17.2 test-data) 0))

; (time (printf "2021 AOC Problem 17.2 = ~a\n" (day16.2 input)))

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM


|#