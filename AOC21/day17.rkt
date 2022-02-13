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
- a boolean, out-of-range? that tells us whether the probe is out of range

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

(define (sum-of-numbers n)
  "the sum of a number series ending in n = (n * (n + 1)) / 2"
  (/ (* n (add1 n)) 2))    ; triangle numbers

;; Target -> Natural
;; given a target calculate the maximum height a probe
;; can reach and still hit the target
(define (max-y-height t)
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

#|
Two ways to go about this. One is my original plan to try all the possibilities
and count up the hits. Given reasonable boundaries on the x and y velocities
this is pretty quick.

The other way - inspired by the backwards way I solved part one - is to calculate
the set of triangle numbers for x and y and find the intersection with the set
of points in the target. That should work too.

I've already written the code for method one so let's see if we can get it fast
enuf. (Turns out it's plenty fast so I'll leave the second method to another
time.)

|#
(struct probe (x y xv yv) #:transparent) ; contains the current state of the probe

;; Probe -> Probe
;; given a probe position return the next position according to the rules provided
(define (step p)
  (probe
   ; new x-pos
   (if (<= (probe-xv p) 0)           ; velocity has bottomed out
       (probe-x p)                   ; so we ain't movin'
       (+ (probe-x p) (probe-xv p))) ; otherwise move by x velocity
   (+ (probe-y p) (probe-yv p))      ; new y-pos 
   (sub1 (probe-xv p))               ; new x-velocity
   (sub1 (probe-yv p))))             ; new y-velocity
  
(module+ test
  (check-equal? (step (probe 0 0 0 0)) (probe 0 0 -1 -1))
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
;; returns true if target can no longer be reached
(define (out-of-range? p t)
  (or (> (probe-x p) (target-x-max t))
      (< (probe-y p) -100)))  ; this number is a wild guess

(module+ test
  (check-equal? (out-of-range? (probe 31 -9 0 0) (target 20 30 -5 -10)) #t)
  (check-equal? (out-of-range? (probe 26 -11 0 0) (target 20 30 -5 -10)) #f))

;; Probe -> Natural or #f
;; Given a probe return #t if it hits the target 
;; #f if it misses
(define (flight p t)
  (cond [(hit-target? p t) #t]
        [(out-of-range? p t) #f]
        [else (flight (step p) t)]))

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

#|
This will be way too slow unless I can calculate reasonable
ranges for xv and yv.

xv isn't too hard: vx-min = the lowest velocity that gets to the target,
and vx-max = right edge of the target (any higher and the first shot
will go right past the target).

I'll have to think about what vy-max should be. I'm sure there's
a way. Meanwhile I'll just plug in -100 .. 100
|#

;; Natural Natural -> Natural
;; Given a target point and a velocity calculate
;; the lowest starting velocity at point 0 that
;; will get to the target - this starts at the left
;; edge of the target and works backward from a
;; velocity of zero by adding 1 each step until
;; it goes past the 0 point.
(define (minimum-x-velocity target velocity)
  (cond [(<= target 0) (sub1 velocity)]
        [else (minimum-x-velocity (- target velocity) (add1 velocity))]))

#|
The yv range is another matter. It can be negative or positive.
yv-min is the number that would hit the target
in one shot, (target-y-min t). Not sure how to figure the highest
so I'll choose 100 and see if it works. It does!
|#

(define (day17.2 str)
  (let ([t (make-target str)])
    (test-flights (minimum-x-velocity (target-x-min t) 0)
                  (add1 (target-x-max t)) 
                  (target-y-min t)        ; velocity that gets there in 1 step
                  100 ; completely arbitrary fudge factor
                  t)))

(module+ test
  (check-equal? (day17.2 "target area: x=20..30, y=-10..-5") 112))

(time (printf "2021 AOC Problem 17.2 = ~a\n" (day17.2 (file->string "input17.txt"))))

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

2021 AOC Problem 16.1 = 3570
cpu time: 0 real time: 0 gc time: 0
2021 AOC Problem 17.2 = 1919
cpu time: 65 real time: 68 gc time: 7

|#