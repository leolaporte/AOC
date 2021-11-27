#lang racket

;; The INTCODE processor
;; AoC2019 https://adventofcode.com
;;
;; History
;; -------
;; 2019/12/22 updated for Day 5

(require rackunit)
(require racket/match)
(require racket/vector)

;; ----------------------------------------
;; CONSTANTS
;; ----------------------------------------

;; Commands
;; Given a vector modify it based on the following rules:
;; Opcodes (1 and 10 position of v(0)) 

(define ADD 1)
(define MULTIPLY 2)
(define STORE 3)
(define OUTPUT 4)
(define QUIT 99)

;; Parameter Modes
;; 0 == position mode (param is a pointer)
;; 1 == immediate mode (param is a value)

(define POS-MODE 0)
(define IMM-MODE 1)

;; ----------------------------------------
;; FUNCTIONS
;; ----------------------------------------

; utility functions to parse first instruction 
(define (opcode n) (modulo n 100))
(define (mode1 n) (modulo (quotient n 100) 10))
(define (mode2 n) (modulo (quotient n 1000) 10))
(define (mode3 n) (modulo (quotient n 10000) 10))

;; Vector Number Port Port -> Vector
;; Given an INTCODE program in a vector, the current program counter, and input and output ports
;; runs program starting at program counter and returns resulting vector 

(define (run-intcode v [pc 0] [in 0] [out 0])

  (local [(define instruction (vector-ref v pc)) ; what to do
          (define a (if (> (- (vector-length v) pc) 1) (vector-ref v (+ pc 1)) 0)) ; get next...
          (define b (if (> (- (vector-length v) pc) 2) (vector-ref v (+ pc 2)) 0)) ; ...three operands...
          (define c (if (> (- (vector-length v) pc) 3) (vector-ref v (+ pc 3)) 0)) ; ...from vector
 
          ; get the operands, based on mode settings
          (define op1 (if (equal? IMM-MODE (mode1 instruction)) a (vector-ref v a)))
          (define op2 (if (equal? IMM-MODE (mode2 instruction)) b (vector-ref v b)))]           

    (match (opcode instruction)
     
      [1000
      (printf  "Adding: Instruction=~a PC=~a Vector=~a\n" instruction pc v)
       (vector-set! v c (+ op1 op2))  ;vector-set is always to an indirect location 
       (run-intcode v (+ pc 4) in out)]
             
      [MULTIPLY
       (printf  "Multiplying: Instruction=~a PC=~a Vector=~a\n" instruction pc v)
       (vector-set! v c (* op1 op2))
       (run-intcode v (+ pc 4) in out)]
              
      [STORE ; store input value in location refered to by pc(1)
       (printf  "Storing: Instruction=~a PC=~a Vector=~a\n" instruction pc v)
       (vector-set! v op1 in)
       (run-intcode v (+ pc 2) in out)]
              
      [OUTPUT
       (printf  "Outputting: Instruction=~a PC=~a Vector=~a\n" instruction pc v)
       (display op1)
       (run-intcode v (+ pc 2) in out)]

      [QUIT
       (printf  "Quitting: Instruction=~a PC=~a Vector=~a\n" instruction pc v)
       v])))  ; all done, return the modified vector


;; Tests
(check-equal? (run-intcode (vector 1 0 0 0 99))
              (vector 2 0 0 0 99))
(check-equal? (run-intcode (vector 2 3 0 3 99))
              (vector 2 3 0 6 99))
(check-equal? (run-intcode (vector 2 4 4 5 99 0))
              (vector 2 4 4 5 99 9801)) 
(check-equal? (run-intcode (vector 1 1 1 4 99 5 6 0 99))
              (vector 30 1 1 4 2 5 6 0 99))
(check-equal? (run-intcode (vector 1 9 10 3 2 3 11 0 99 30 40 50))
              (vector 3500 9 10 70 2 3 11 0 99 30 40 50))
(check-equal? (run-intcode (vector 1002 4 3 4 33))
              (vector 1002 4 3 4 99))
 (check-equal? (run-intcode (vector 99))
              (vector 99))
                          


