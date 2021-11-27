#lang racket

;; The INTCODE processor
;; AoC2019 https://adventofcode.com
;;
;; History
;; -------
;; 2019/12/24 updated for Day 5

; runs the INTCODE program provided in a vector, optional args: program counter, in, and out
(provide run-intcode) ; (run-intcode vector pc=0 in=0 out=0)

(require rackunit)

;; ----------------------------------------
;; CONSTANTS
;; ----------------------------------------

;; Commands
;; Given a vector modify it based on the following rules:
;; Opcodes (1 and 10 position of v(0)) 

(define ADD 1)            ; Day 2
(define MULTIPLY 2)       ; Day 2
(define STORE 3)          ; Day 2
(define OUTPUT 4)         ; Day 2

(define JUMP-IF-TRUE 5)   ; Day 5
(define JUMP-IF-FALSE 6)  ; Day 5
(define LESS-THAN 7)      ; Day 5
(define EQUALS 8)         ; Day 5

(define QUIT 99)          ; Day 2

;; Parameter Modes
;; 0 == position mode (param is a pointer)
;; 1 == immediate mode (param is a value)
(define POSITION-MODE 0)
(define IMMEDIATE-MODE 1)

;; ----------------------------------------
;; FUNCTIONS
;; ----------------------------------------

;; Vector Number Number Number -> Vector
;; Given an INTCODE program in a vector, the current program counter, input value, and output value
;; runs program starting at program counter and returns resulting vector (printing intermediate diagnostic
;; information via OUTPUT)

(define (run-intcode v [pc 0] [in 0] [out 0])
  ; (printf "In run, pc=~a v=~a\n" pc v)
  (local [(define instruction (vector-ref v pc))
          (define param1 (+ pc 1))
          (define param2 (+ pc 2))
          (define param3 (+ pc 3))
          (define param4 (+ pc 4))

          ;; utility functions that convert the instruction into opcode and mode settings
          (define (opcode n) (modulo n 100))
          (define (mode param inst)  ; given instruction, return the mode 
            (modulo (quotient inst (expt 10 (+ param 1))) 10))

          ; get the parameter specified by param (1, 2, or 3), handles immediate and position modes
          (define (getp param)  
            (if (equal? IMMEDIATE-MODE (mode param instruction))
                (vector-ref v (+ pc param))                     ; immediate mode, the value is the param
                (vector-ref v (vector-ref v (+ pc param)))))]   ; position mode, the param is a pointer

    (match (opcode instruction)
      
      [(== ADD)
       ; (display "ADD\n")
       ; adds together numbers read from next two positions
       ; (using immediate or position addressing) and stores the
       ; result in the location specified by third position
       (vector-set! v (vector-ref v param3) ;vector-set! is always in position mode  
                    (+ (getp 1) (getp 2))) 
       (run-intcode v (+ pc 4) in out)]
             
      [(== MULTIPLY)
       ; (display "MULTIPLY\n")
       ;multiplies the numbers read from the next two positions (immed or pos) and stores the
       ;result in the location specified by the third position
       (vector-set! v (vector-ref v param3) ;vector-set! is always in position mode  
                    (* (getp 1) (getp 2))) 
       (run-intcode v (+ pc 4) in out)]
              
      [(== STORE)
       ; (display "STORE\n")
       ; takes a single integer as input and saves it to the position given by its only parameter.
       ; (define val (read))  ; get from user
       (vector-set! v (vector-ref v param1) in)
       (run-intcode v (+ pc 2) in out)]
              
      [(== OUTPUT)
       ; (display "OUTPUT\n")
       ; outputs the value of its only parameter
       (printf "Diagnostic code = ~a\n" (getp 1))
       (run-intcode v (+ pc 2) in out)]

      [(== JUMP-IF-TRUE)
       ; (display "JIT\n")
       ; if the first parameter is non-zero, it sets the instruction
       ; pointer to the value from the second parameter. Otherwise, it does nothing.
       (run-intcode v
                    (if (zero? (getp 1))
                        (+ pc 3)
                        (getp 2))
                    in out)]

      [(== JUMP-IF-FALSE)
       ; (display "JIF\n")
       ; if the first parameter is zero, it sets the instruction pointer to
       ;the value from the second parameter. Otherwise, it does nothing.
       (run-intcode v
                    (if (zero? (getp 1))
                        (getp 2)
                        (+ pc 3))
                    in out)]

      [(== LESS-THAN)
       ; (display "LESS-THAN\n")
       ; if the first parameter is less than the second parameter, it stores 1 in the
       ;position given by the third parameter. Otherwise, it stores 0.
       (vector-set! v (vector-ref v param3)
                    (if (< (getp 1) (getp 2))
                        1
                        0))
       (run-intcode v (+ pc 4) in out)]


      [(== EQUALS)
       ; (display "EQUALS\n")
       ; if the first parameter is equal to the second parameter, it stores 1 in the position
       ;given by the third parameter. Otherwise, it stores 0.
       (vector-set! v (vector-ref v param3)
                    (if (equal? (getp 1) (getp 2))
                        1
                        0))
       (run-intcode v (+ pc 4) in out)]
      
      [(== QUIT)
       (display "QUIT\n")
      ; v  ; uncomment for tests
       ]

      [_
       (printf "Error: unmatched opcode: ~a\n" (opcode instruction))])))



;; Tests


; from Day 2 description
(define-test-suite day2
  (check-equal? (run-intcode (vector 1 9 10 3 2 3 11 0 99 30 40 50))
                (vector 3500 9 10 70 2 3 11 0 99 30 40 50))

  (check-equal? (run-intcode (vector 1 0 0 0 99))
                (vector 2 0 0 0 99))

  (check-equal? (run-intcode (vector 2 3 0 3 99))
                (vector 2 3 0 6 99))

  (check-equal? (run-intcode (vector 2 4 4 5 99 0))
                (vector 2 4 4 5 99 9801))

  (check-equal? (run-intcode (vector 1 1 1 4 99 5 6 0 99))
                (vector 30 1 1 4 2 5 6 0 99))

  (check-equal? (run-intcode (vector 1002 4 3 4 33))
                (vector 1002 4 3 4 99)))

; from Day 5 description
(define-test-suite day5
  (check-equal? (run-intcode (vector 1002 4 3 4 33))
                (vector 1002 4 3 4 99))

  (check-equal? (run-intcode (vector 3 9 8 9 10 9 4 9 99 -1 8) 0 8)
                '#(3 9 8 9 10 9 4 9 99 1 8)) ; diagnostic 1 and halt
  
  (check-equal? (run-intcode (vector 1 0 3 3 1005 2 10 5 1 0 4 1 99))
                '#(1 0 3 4 1005 2 10 5 1 0 4 1 99)) ; output 0 and halt

  (check-equal? (run-intcode (vector 101 -1 7 7 4 7 1105 11 0 99))
                '#(101 -1 7 7 4 7 1105 0 0 99))) ; countdown from 10 to zero

;; Reddit examples....

; Test position mode
; (run-intcode (vector 3 9 8 9 10 9 4 9 99 -1 8)) ; if input = 8 1 0
; (run-intcode (vector 3 9 7 9 10 9 4 9 99 -1 8)) ; if input < 8 1 0

; Test immediate mode
; (run-intcode (vector 3 3 1108 -1 8 3 4 3 99)) ; if input = 8 1 0
; (run-intcode (vector 3 3 1107 -1 8 3 4 3 99)) ; if input < 8 1 0

; Test jumps
; (run-intcode (vector 3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9)) ; if input=0 0 1 (position)
; (run-intcode (vector 3 3 1105 -1 9 1101 0 0 12 4 12 99 1)) ; if input=0 0 1 (immediate)

; Big test
; output < 8 = 999
; output = 8 = 1000
; or output > 8 = 1001 
;(run-intcode (vector 3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99))
