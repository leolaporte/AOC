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
; target area: x=248..285, y=-85..-56
; You'll forgive me if I don't write a parser for this.

(define TARGET '(248 285 85 -56))

#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#

#|
First instinct is to brute force it. Let's see how fast that is.

I'll need an object, probe, to record the current state of the probe.

I'll need a function, step, that advances the probe one step.

I'll need a function, flight, that repeats until probe enters target or passes out of reach.

I'll need a boolean, hit-target?, that tells us whether the probe has hit the target

I'll need a boolean, in-range? that tells us whether the probe is out of range (stop IOW)

I'll need a hash, hits, with starting velocity as the key and max-y as the value.

So then. Starting at 0,0 try velocities and record those which hit the target. When
we've tried all the velocities find the hash key with the biggest y-value.

Only one more question: what range of velocities should I try? I don't want to
search an infinite set. Ah this is what makes it not entirely brute force. Can I
reason out what the ranges of x-v and y-v can be?

x-v has to be > 1 or we go straight up or down. Or both.
x-y has to be high enough 
y-v has to be high enough to overcome gravity 

|#

(struct probe (x y xv yv)) ; contains the current state of the probe
(define hits (make-hash))  ; contains the max-y of all successful launches

(define (step probe) (probe 0 0 0 0)) ; advances the probe one step according to the rules

(define (flight probe velocity) #f) ; tests all the velocites, returns #t if we hit

(define (hit-target? probe) #f) ; have we hit target?

(define (in-range? probe) #f) ; are we still in range? (is it still possible to hit?)

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

;(module+ test
;  (check-equal? (day17.1 '(20 30 -10 -5) 45))

; (time (printf "2021 AOC Problem 16.1 = ~a\n" (day17.1 input)))

#|=================================================================================
                                        PART 2
                               

==================================================================================|#

;(module+ test
;  (check-equal? (day17.2 test-data) 0))

; (time (printf "2021 AOC Problem 17.2 = ~a\n" (day16.2 input)))

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM


|#