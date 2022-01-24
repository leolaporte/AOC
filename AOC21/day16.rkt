#lang racket

;AOC Day 16
;Leo Laporte 22-Jan-2022

#|==============================================================================

                                   Part One

Decode the structure of your hexadecimal-encoded BITS transmission; what do you
get if you add up the version numbers in all packets?

================================================================================|#

(require rackunit
         threading
         racket/trace)

#|==============================================================================|#
#|                                      DATA                                    |#
#|==============================================================================|#

(define (parse-input str)
  
  (define (hex->bits hex)
    (match hex
      [#\0 '(0 0 0 0)] [#\1 '(0 0 0 1)] [#\2 '(0 0 1 0)] [#\3 '(0 0 1 1)]
      [#\4 '(0 1 0 0)] [#\5 '(0 1 0 1)] [#\6 '(0 1 1 0)] [#\7 '(0 1 1 1)]
      [#\8 '(1 0 0 0)] [#\9 '(1 0 0 1)] [#\A '(1 0 1 0)] [#\B '(1 0 1 1)]
      [#\C '(1 1 0 0)] [#\D '(1 1 0 1)] [#\E '(1 1 1 0)] [#\F '(1 1 1 1)]
      [#\newline '()]))

  (~> str
      string->list
      (map hex->bits _)
      flatten))

(define input (parse-input (file->string "input16.txt")))
(define test-1 (parse-input "8A004A801A8002F478"))
(define test-2 (parse-input "620080001611562C8802118E34"))
(define test-3 (parse-input "C0015000016115A2E0802F182340"))
(define test-4 (parse-input "A0016C880162017C3686B18A3D4780"))

#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#

#|
Let me start by trying to boil down the rules:
The input is a string of hex digits. Each representing four binary digits.
But we have to split the input into groups of three binary digits.

Bits
0-2 Packet version (throw away)
3-5 Type ID (sum these for part 1 and throw away the rest)
         Type 4 means a literal is to follow*
         All other types are operators
               For operators, bit 6 is the length type (0 or 1)
                     0 means the next 15 bits are the length of the data
                     1 means the next 11 bits are the number of sub-packets to follow
                                           restart with each sub-packet

* Literal packets begin with a 1 until the last packet which begins with a 0 - so each
literal packet is 5 bits

So using these rules (and considering what I might want to do in part 2) I think
the best way to solve part 1 is to create a function that takes the input as
a stream of bits and turns it into a list of expressions. For part one
I can just sum the version numbers of each item in the list.

(flatten input) gives us a stream of bits. racket/stream gives us (first stream)
and (rest stream). So I can make an assembly line which assembles the streams
into expression structures I'll call xp.

|#

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

(require racket/stream)

(struct xp (version op data))
;; xp is (xp Natural Natural Natural)
;; where version is a number from 0-7
;; op is a number from 0-7
;; and data is a Natural

(define (process-stream st)
  (for/list ))

;; Stream -> Xp
;; Given a stream of bits render it an expression Xp
(define (stream->xp stream)
  (define version (bin->decimal (take 3 stream)))
  (define op (bin->decimal (take 3 stream)))

  (cond [(equal? op 4) (define data (get-literal stream))]
        [else (if (equal? 0 (take 1 stream))
                 (define data (get-data stream))
                 (define data (xp-data (stream->xp stream))))])

  ; dump padding to get to start of next expression

  (xp version op data))

(module+ test
  (check-equal? (stream->xp '(110100101111111000101000)) (xp 6 4 2021))
  (check-equal? (stream->xp '(00111000000000000110111101000101001010010001001000000000))
                (xp 1 6 '(10 20)))
  (check-equal? (stream->xp '(11101110000000001101010000001100100000100011000001100000))
                (xp 7 3 '(1 2 3))))

;; Natural Stream -> Natural
;; Take count bits off stream and convert them into a number
;; !!!
(define (take cnt st) 0) ; stub

;; Stream -> Natural
;; given a stream use the literal rules to produce a number
;; !!!
(define (get-literal st) 0 ) ; stub

;; Stream -> (list-of Natural)
;; given a stream use the literal rules to produce a list
;; of numbers
;; !!!
(define (get-data st) 0 ) ; stub

;; Stream -> (list-of Natural)
;; given a stream use the literal rules to produce a list
;; of numbers
;; !!!
(define (get-sub-packets st) 0 ) ; stub

;; (list-of 1 or 0) -> natural
;; turn list of binary digits into its decimal equivalent, eg. '(1 0 1 0) into 10
(define (bin->decimal lst)
  (local [(define (list->string l)
            (cond ((empty? l) "")
                  (else (string-append (number->string (first l)) (list->string (rest l))))))]
    (string->number (list->string lst) 2)))



(define (day16.1 l) 0) ; stub

(module+ test
  (check-equal? (day16.1 test-1) 16)
  (check-equal? (day16.1 test-2) 12)
  (check-equal? (day16.1 test-3) 23)
  (check-equal? (day16.1 test-4) 21))

; (time (printf "2021 AOC Problem 16.1 = ~a\n" (day16.1 input)))

#|=================================================================================
                                        PART 2
                               

==================================================================================|#

;(module+ test
;  (check-equal? (day16.2 test-data) 0))

; (time (printf "2021 AOC Problem 16.2 = ~a\n" (day16.2 input)))

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM


|#