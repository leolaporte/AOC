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

(struct target (x-min x-max y-min y-max) #:transparent)

;; String -> Target
;; given the problem provided coordinate string, return a
;; Target structure
(define (make-target str)
  (let ([t-list (map string->number (regexp-match* #px"(-?\\d+)" str))])
    (target (first t-list)
            (second t-list)
            (third t-list)
            (fourth t-list))))

(module+ test
  (check-equal? (make-target "target area: x=248..285, y=-85..-56")
                (target 248 285 -85 -56)))

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

OH FOR CRYING OUT LOUD!

Turns out I could do Part 1 in my head. Well at least all that code isn't wasted.
Part 2 needs to know more. 

|#

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

;; Target -> Natural
;; given a target calculate the maximum height a probe
;; can reach and still hit the target
(define (max-y-height t)
  "the sum of a number series ending in n = (n * (n + 1)) / 2"
  (define (sum-of-numbers n)
    (/ (* n (add1 n)) 2))    ; triangle numbers

  ; how low can we go? that defines how high we can fly
  (sum-of-numbers (- (- (target-y-min t)) 1)))

(define (day17.1 str)
  (max-y-height (make-target str)))

(module+ test
  (check-equal? (day17.1 "target area: x=20..30, y=-10..-5") 45))

(time (printf "2021 AOC Problem 16.1 = ~a\n" (day17.1 (file->string "input17.txt"))))

#|=================================================================================
                                        PART 2
                               
How many distinct initial velocity values cause the probe to be within the target
area after any step?

==================================================================================|#

(struct probe (x y xv yv) #:transparent) ; contains the current state of the probe

;; Probe -> Probe
;; given a probe position return the next position according to the rules provided
(define (step p)
  (probe
   ; new x-pos
   (if (<= (probe-xv p) 0)           ; velocity has bottomed out
       (probe-x p)                   ; so we ain't movin'
       (+ (probe-x p) (probe-xv p))) ; otherwise move by x velocity

   ; new y-pos 
   (+ (probe-y p) (probe-yv p))
       
   ; new x-velocity
   (if (> (probe-xv p) 0)
       (sub1 (probe-xv p))
       0)
     
   ; new y-velocity
   (sub1 (probe-yv p))))

(module+ test
  (check-equal? (step (probe 0 0 0 0)) (probe 0 0 0 -1))
  (check-equal? (step (probe 1 2 7 8)) (probe 8 10 6 7))
  (check-equal? (step (probe 0 -2 7 8)) (probe 7 6 6 7))
  (check-equal? (step (probe 1 -2 7 -8)) (probe 8 -10 6 -9)))
    
;; Probe Target -> Boolean
;; returns true if the probe has hit the target
(define (hit-target? p t)
  (and (>= (target-x-max t) (probe-x p) (target-x-min t))   
       (>= (target-y-max t) (probe-y p) (target-y-min t))))

(module+ test
  (check-equal? (hit-target? (probe 10 20 0 0) (target 10 15 20 25)) #t)
  (check-equal? (hit-target? (probe 10 20 0 0) (target 20 25 20 25)) #f))

;; Probe Target -> Boolean
;; returns true if it's still possible to hit target
(define (in-range? p t)
  (and (<= (probe-x p) (target-x-max t))   
       (>= (probe-y p) (target-y-max t)))) 

(module+ test
  (check-equal? (in-range? (probe 5 -9 0 0) (target 20 30 -5 -10)) #t)
  (check-equal? (in-range? (probe 26 -11 0 0) (target 20 30 -5 -10)) #f)
  (check-equal? (in-range? (probe 26 -9 0 0) (target 20 30 -5 -10)) #t))

;; Probe -> Natural or #f
;; Given a probe return #t if it hits the target 
;; #f if it misses
(define (flight p t)
  (cond [(hit-target? p t) #t]
        [(not (in-range? p t)) #f]
        [else (flight (step p) t)]))

#|
 (trace flight)
 (flight (probe 0 0 7 8) (make-target "target area: x=20..30, y=-10..-5"))

(regexp-match* #px"(-?\\d+),(-?\\d+)" str)

"23,-10  25,-9   27,-5   29,-6   22,-6   21,-7   9,0     27,-7   24,-5
25,-7   26,-6   25,-5   6,8     11,-2   20,-5   29,-10  6,3     28,-7
8,0     30,-6   29,-8   20,-10  6,7     6,4     6,1     14,-4   21,-6
26,-10  7,-1    7,7     8,-1    21,-9   6,2     20,-7   30,-10  14,-3
20,-8   13,-2   7,3     28,-8   29,-9   15,-3   22,-5   26,-8   25,-8
25,-6   15,-4   9,-2    15,-2   12,-2   28,-9   12,-3   24,-6   23,-7
25,-10  7,8     11,-3   26,-7   7,1     23,-9   6,0     22,-10  27,-6
8,1     22,-8   13,-4   7,6     28,-6   11,-4   12,-4   26,-9   7,4
24,-10  23,-8   30,-8   7,0     9,-1    10,-1   26,-5   22,-9   6,5
7,5     23,-6   28,-10  10,-2   11,-1   20,-9   14,-2   29,-7   13,-3
23,-5   24,-8   27,-9   30,-7   28,-5   21,-10  7,9     6,6     21,-5
27,-10  7,2     30,-9   21,-8   22,-7   24,-9   20,-6   6,9     29,-5
8,-2    27,-8   30,-5   24,-7"
|#
#|
So yeah, everything I wrote in part 1 is still good here. The question remains,
what should the range of values for xv and yv be? In the sample problem the range
is xv: 6..30, yv: 10...-7 - I could just guess.
|#

;; Natural Natural Natural Natural Target -> Natural
;; given the minimum and maximum velocities for x and y
;; return the number of successful flights
(define (test-flights xv-min xv-max yv-min yv-max t)
  (for*/fold ([hits 0])
            ([xv (in-range xv-min (add1 xv-max))]
             [yv (in-range yv-min (add1 yv-max))])
    (values (+ hits
               (if (flight (probe 0 0 xv yv) t)
                   1
                   0)))))

(define (day17.2 str)
  (test-flights 0 100 -100 100 (make-target str)))

(module+ test
  (check-equal? (day17.2 "target area: x=20..30, y=-10..-5") 112))

; (time (printf "2021 AOC Problem 17.2 = ~a\n" (day16.2 input)))

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM


|#