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

#|

Notes:

As usual the most important step is to figure out how to represent the given
input. In this case I'm going to convert the hex string into a queue of bits.
My reasoning: we only need to look at each bit once, and we examine the bits
in order, so popping them off a queue as we go seems the right way to go.

The only big problem I see is the issue of padding. At some point there are
some number of 0 bits used to get us to the new hex digit. This could prove
to be a showstopper but any solution to that problem is complicated.
Let's see what happens if I ignore the problem.

So, first parse the given input.

|#

(require data/queue) ; for queue structure

;; String -> Queue
(define (input->queue str)
  "takes input as a string of hex digits and turns it into a queue of bits"

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
      flatten
      list->queue))

(module+ test
  (check-equal? (queue->list (input->queue "F")) '(1 1 1 1))
  (check-equal? (queue->list (input->queue "DF")) '(1 1 0 1 1 1 1 1))
  (check-equal? (queue->list (input->queue "13")) '(0 0 0 1 0 0 1 1)))

#|
Notes (cont.)

Now what do we do with this queue?

Let me start by trying to boil down the rules, going bit by bit:

Bits
0-2 Packet version (add these for the answer to part 1)
3-5 Type ID (tells us how to interpret the following bits)
         Type ID 4 means a literal is to follow*
         All other types are operators
               For operators, bit 6 is the length type (0 or 1)
                     0 means the next 15 bits are the length of the operands
                     1 means the next 11 bits are the number of operands to follow
                               process operand bits as a list of literals

* Literal packets begin with a 1 until the last packet which begins with a 0 - so each
literal packet is 5 bits

"The hexadecimal representation of this packet might encode a few extra 0 bits at the end;
these are not part of the transmission and should be ignored." So after processing
discard extra 0 bits until you reach a border. TKTKTK

So using these rules (and considering what I might want to do in part 2) I think
the best way to solve part 1 is to create a function that takes the input as
a stream of bits and turns it into a Packet struct. For part one
I can just sum the version numbers of each item in the list.

|#

(struct packet (version operator operand) #:transparent)
;; packet is (packet Natural Natural (Natural or (list-of Natural))
;; where version is a number from 0-7
;; operator is a number from 0-7
;; and operand is a Natural or (list-of Natural)

#|==============================================================================|#
#|                                  UTILITIES                                   |#
#|==============================================================================|#

#|
A few little utilities for coverting lists to queues, queue to bits, bits to decimal
|#

;; (list-of 1 or 0) -> natural
;; turn list of binary digits into its decimal equivalent, eg. '(1 0 1 0) into 10
;; reused from Day 3
(define (bin->decimal lst)
  (local [(define (list->string l)
            (cond ((empty? l) "")
                  (else (string-append (number->string (first l)) (list->string (rest l))))))]
    (string->number (list->string lst) 2)))

;; Natural Stream -> (list-of 1s and 0s)
;; Take count bits off queue and convert them into a list
(define (pop cnt q)
  (for/list ([i (in-range cnt)])
    (dequeue! q)))

(module+ test
  (check-equal? (pop 3 (input->queue "D")) '(1 1 0))
  (check-equal? (pop 4 (input->queue "3")) '(0 0 1 1))
  (check-equal? (pop 5 (input->queue "EE")) '(1 1 1 0 1))
  (check-equal? (pop 6 (input->queue "EE")) '(1 1 1 0 1 1)))

;; Natural Queue -> Natural
;; given a count and a queue, dequeue count bits and return as a
;; decimal number
(define (pop->decimal cnt q)
  (bin->decimal (pop cnt q)))

(module+ test
  (check-equal? (pop->decimal 3 (input->queue "D")) 6)
  (check-equal? (pop->decimal 4 (input->queue "3")) 3)
  (check-equal? (pop->decimal 5 (input->queue "EE")) 29)
  (check-equal? (pop->decimal 6 (input->queue "EE")) 59))

;; (list-of 1|0) -> Queue
;; Given a list of 1 and 0 create a queue
(define (list->queue lst)
  (let ([q (make-queue)])
    (for ([bit (in-list lst)])
      (enqueue! q bit))
    q))

(module+ test
  (check-equal? (queue->list (list->queue '(1 0 1 0))) '(1 0 1 0)))

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

;; (queue-of 1 or 0) -> Packet
;;; Given a queue of bits build a Packet
(define (bits->packet q)
  (let* ([version (pop->decimal 3 q)]
         [operator (pop->decimal 3 q)]
         [operand (cond [(equal? operator 4) (get-literal q)]
                        [else (get-operands q)])])

    (packet version operator operand)))

(module+ test
  (check-equal? (bits->packet (input->queue "D2FE28")) (packet 6 4 2021))
  
  (check-equal? (bits->packet (input->queue "38006F45291200"))
                (packet 1 6 '(10 20)))
  
  (check-equal? (bits->packet (input->queue "EE00D40C823060"))
                (packet 7 3 '(1 2 3))))

;; Queue -> Natural
;; given a queue use the literal rules to produce a number
(define (get-literal q)
  (define (get-bits q)
    (let ([b (pop 5 q)])                             ; 5 bits at a time
      (cond [(= (first b) 0) (rest b)]               ; if first bit is 0 this is the last group
            [else (append (rest b) (get-bits q))]))) ; otherwise keep fetching bits
  (bin->decimal (get-bits q)))                       ; convert resulting list of bits to decimal
  
(module+ test
  (check-equal? (get-literal (list->queue '(1 0 1 1 1 1 1 1 1 0 0 0 1 0 1))) 2021))

;; Queue -> (list-of Natural)
;; Given a queue produce the operands as a list of Naturals
;; (follow-up to the initial packet processing)
;; !!!
(define (get-operands q)
  (define (get-len-subs len q)
    ...)

  (define (get-num-subs num q)
    ...)
             
  (if (= (pop->decimal 1 q) 0)             ; get length type ID
    (get-len-subs (pop->decimal 15 q) q)   ; get sub-packets of this total length 
    (get-num-subs (pop->decimal 11 q) q))) ; get this total number of sub-packets

;; Packet -> Natural
;; Given a packet, return the sum of all the types in the Packet
;; !!!
(define (day16.1 pckt) 0) ; stub

#|
(module+ test
  (check-equal? (day16.1 (bits->packet (input->queue "8A004A801A8002F478"))) 16)
  (check-equal? (day16.1 (bits->packet (input->queue "620080001611562C8802118E34"))) 12)
  (check-equal? (day16.1 (bits->packet (input->queue "C0015000016115A2E0802F182340"))) 23)
  (check-equal? (day16.1 (bits->packet (input->queue "A0016C880162017C3686B18A3D4780"))) 31))

(time (printf "2021 AOC Problem 16.1 = ~a\n"
 (day16.1 (bits->packet (input->queue "input16.txt")))))

|#

#|=================================================================================
                                        PART 2
                               

==================================================================================|#

#|
(module+ test
  (check-equal? (day16.2 test-data) 0))

 (time (printf "2021 AOC Problem 16.2 = ~a\n" (day16.2 (input->queue "input16.txt"))))
|#

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM


|#