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

(require data/queue) ; for queue structure

;; String -> Queue
(define (input->queue str)
  "takes input as a string of hex digits and turns it into a queue of bits"
  (let ([q (make-queue)])
    
    (define (hex->bits hex)
      (match hex
        [#\0 '(0 0 0 0)] [#\1 '(0 0 0 1)] [#\2 '(0 0 1 0)] [#\3 '(0 0 1 1)]
        [#\4 '(0 1 0 0)] [#\5 '(0 1 0 1)] [#\6 '(0 1 1 0)] [#\7 '(0 1 1 1)]
        [#\8 '(1 0 0 0)] [#\9 '(1 0 0 1)] [#\A '(1 0 1 0)] [#\B '(1 0 1 1)]
        [#\C '(1 1 0 0)] [#\D '(1 1 0 1)] [#\E '(1 1 1 0)] [#\F '(1 1 1 1)]
        [#\newline '()]))

    (define bit-list
      (~> str
          string->list
          (map hex->bits _)
          flatten))

    (for ([bit (in-list bit-list)])
      (enqueue! q bit))

    q))

(module+ test
  (check-equal? (queue->list (input->queue "F")) '(1 1 1 1))
  (check-equal? (queue->list (input->queue "DF")) '(1 1 0 1 1 1 1 1))
  (check-equal? (queue->list (input->queue "13")) '(0 0 0 1 0 0 1 1))
  )


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

"The hexadecimal representation of this packet might encode a few extra 0 bits at the end;
these are not part of the transmission and should be ignored." So after processing
discard extra 0 bits until you reach a border. TKTKTK

So using these rules (and considering what I might want to do in part 2) I think
the best way to solve part 1 is to create a function that takes the input as
a stream of bits and turns it into a list of expressions. For part one
I can just sum the version numbers of each item in the list.

For efficiency I'll store the bits in a vector, and use a pointer to show
my current position in the stream. So I can make an assembly line which assembles
the bits into a list of expression structures.

|#

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

(struct xp (version op data) #:transparent)
;; xp is (xp Natural Natural Natural)
;; where version is a number from 0-7
;; op is a number from 0-7
;; and data is a Natural or (list-of xp)

;; (queue-of 1 or 0) -> Xp
;;; Given a stream of bits render it an expression Xp
(define (bits->xp q)
  (let* ([version (take 3 q)]
         [op (take 3 q)]
         [data
          (cond [(equal? op 4) (get-literal q)]
                [else (if (equal? 0 (take 1 q)) ; length type ID
                          (get-data q)
                          (bits->xp q))])])
 
    ; dump padding to get to start of next expression

    (xp version op data)))

(module+ test
  (check-equal? (bits->xp (input->queue "D2FE28")) (xp 6 4 2021))
  (check-equal? (bits->xp (input->queue "38006F45291200")) (xp 1 6 '((xp 6 4 10) (xp 2 4 20))))
  (check-equal? (bits->xp (input->queue "EE00D40C823060")) (xp 7 3 '((xp 2 4 1) (xp 4 4 2) (xp 1 4 3)))))
  )

;; (list-of 1 or 0) -> natural
;; turn list of binary digits into its decimal equivalent, eg. '(1 0 1 0) into 10
;; reused from Day 3
(define (bin->decimal lst)
  (local [(define (list->string l)
            (cond ((empty? l) "")
                  (else (string-append (number->string (first l)) (list->string (rest l))))))]
    (string->number (list->string lst) 2)))

(module+ test
  (check-equal? (bin-list->decimal '(1 0 1 0)) 10)
  (check-equal? (bin-list->decimal '(1 1 1 1)) (+ 1 2 4 8))
  (check-equal? (bin-list->decimal '(1 0 0 0)) 8))

;; Natural Stream -> Natural
;; Take count bits off stream and convert them into a number
(define (take cnt q)
  (bin->decimal
   (for/list ([i (in-range cnt)])
     (dequeue! q))))

(module+ test
  (check-equal? (take 3 (input->queue "D2FE28")) 6)
  (check-equal? (take 3 (input->queue "38006F45291200")) 1)
  (check-equal? (take 3 (input->queue "EE00D40C823060")) 7)
  )

;; Stream -> Natural
;; given a stream use the literal rules to produce a number
;; !!!
(define (get-literal q) 0 ) ; stub

;; Stream -> (list-of Natural)
;; given a stream use the literal rules to produce a list
;; of numbers
;; !!!
(define (get-data q) 0 ) ; stub


#|
(module+ test
  (check-equal? (day16.1 (input->queue "8A004A801A8002F478")) 16)
  (check-equal? (day16.1 (input->queue "620080001611562C8802118E34")) 12)
  (check-equal? (day16.1 (input->queue "C0015000016115A2E0802F182340")) 23)
  (check-equal? (day16.1 (input->queue "A0016C880162017C3686B18A3D4780")) 21))

(time (printf "2021 AOC Problem 16.1 = ~a\n" (day16.1 (file->string "input16.txt"))))

|#

#|=================================================================================
                                        PART 2
                               

==================================================================================|#

#|
(module+ test
  (check-equal? (day16.2 test-data) 0))

 (time (printf "2021 AOC Problem 16.2 = ~a\n" (day16.2 input)))
|#

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM


|#