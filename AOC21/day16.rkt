#lang racket

;AOC Day 16
;Leo Laporte 22-Jan-2022

#|==============================================================================

                                   Part One

Decode the structure of your hexadecimal-encoded BITS transmission; what do you
get if you add up the version numbers in all packets?

================================================================================|#

(require rackunit       ; for unit tests
         threading      ; func composition tool: "~>" instead of nested functions
         racket/trace)  ; for debugging

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
Let's see what happens if I ignore the issue for now. (Update: it works,
padding is ignored. Phew!)

So, first parse the given input.

|#

(require data/queue) ; for queue structure

;; String -> Queue
;; takes input as a string of hex digits and turns it into a queue of bits
(define (input->queue str)
 
  (define (hex->bits hex)
    (match hex
      [#\0 '(0 0 0 0)] [#\1 '(0 0 0 1)] [#\2 '(0 0 1 0)] [#\3 '(0 0 1 1)]
      [#\4 '(0 1 0 0)] [#\5 '(0 1 0 1)] [#\6 '(0 1 1 0)] [#\7 '(0 1 1 1)]
      [#\8 '(1 0 0 0)] [#\9 '(1 0 0 1)] [#\A '(1 0 1 0)] [#\B '(1 0 1 1)]
      [#\C '(1 1 0 0)] [#\D '(1 1 0 1)] [#\E '(1 1 1 0)] [#\F '(1 1 1 1)]
      [#\newline '()]))

  (~> str                 ; our input string
      string->list        ; convert string into a list of hex digits
      (map hex->bits _)   ; turn hex into binary
      flatten             ; flatten the lists into one big list
      list->queue))       ; turn that list into a queue

(module+ test
  (check-equal? (queue->list (input->queue "F")) '(1 1 1 1))
  (check-equal? (queue->list (input->queue "DF")) '(1 1 0 1 1 1 1 1))
  (check-equal? (queue->list (input->queue "13")) '(0 0 0 1 0 0 1 1)))

#|
Notes (cont.)

Now what do we do with this queue?

Let me start by trying to boil down the rules, going bit by bit:

Bits  Interp.
0-2   Packet version (add these for the answer to part 1)
3-5   Type ID (tells us how to interpret the following bits)
         = 4 means a literal is to follow*
         = some operator (to be named later?)
               For operators, bit 6 is the length type (0 or 1)
                     0 means the next 15 bits are the length of the operands
                     1 means the next 11 bits are the number of operands to follow
                               process operand bits as a list of packets (recurse)

* Literal packets begin with a 1 until the last packet which begins with a 0 - so each
literal packet is 5 bits

"The hexadecimal representation of this packet might encode a few extra 0 bits at the end;
these are not part of the transmission and should be ignored." So after processing
discard extra 0 bits until you reach a border. TKTKTK

So using these rules (and considering what I might want to do in part 2) I think
the best way to solve part 1 is to create a function that takes the input as
a stream of bits and turns it into a tree of Packets. For part one
I can just sum the version numbers of each Packet in the tree.

|#

(struct packet (version operator operand) #:transparent)
;; packet is (packet Natural Natural (one-of Natural or (list-of Packet))
;; where version is a number from 0-7
;; operator is a number from 0-7
;; and operand is one of Natural or (list-of Packet)

#|==============================================================================|#
#|                                  UTILITIES                                   |#
#|==============================================================================|#

;; A few little utilities for coverting lists to queues, queue to bits, bits to decimal

;; (list-of 1 or 0) -> natural
;; turn list of binary digits into its decimal equivalent, eg. '(1 0 1 0) into 10
;; reused from Day 3
(define (bin->decimal lst)
  (local [(define (list->string l)
            (cond ((empty? l) "")
                  (else (string-append (number->string (first l)) (list->string (rest l))))))]
    (string->number (list->string lst) 2)))

;; Natural Queue -> (list-of 1s and 0s)
;; Take count bits off queue and convert them into a list of 1s and 0s
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
#|                                     MAIN                                     |#
#|==============================================================================|#

;; Queue -> Packet
;; Given a queue of bits build a Packet (this is the main function)
(define (bits->packet q)
  (let* ([version (pop->decimal 3 q)]                         ; the version number
         [operator (pop->decimal 3 q)]                        ; the operator
         [operand (cond [(equal? operator 4) (get-literal q)] ; get a literal
                        [else (get-operands q)])])            ; get a list of the remaining packets

    (packet version operator operand)))

(module+ test
  (check-equal? (bits->packet (input->queue "D2FE28")) (packet 6 4 2021))
  
  (check-equal? (bits->packet (input->queue "38006F45291200"))
                (packet 1 6 (list (packet 6 4 10) (packet 2 4 20))))
  
  (check-equal? (bits->packet (input->queue "EE00D40C823060"))
                (packet 7 3 (list (packet 2 4 1) (packet 4 4 2) (packet 1 4 3)))))

;; Queue -> (list-of Packet)
;; Given a queue produce the operands as a list of Packet
(define (get-operands q)
  
  (define (get-len-subs len q)
    "make new queue of len, then process it"
    (let ([new-queue (list->queue (pop len q))]) ; here's where the padding gets ignored

      (define (subs q)
        (cond [(queue-empty? q) empty]
              [else (cons (bits->packet q) (subs q))]))
      
      (subs new-queue)))

  (define (get-num-subs num q)
    "process num many packets"
    (for/list ([i (in-range num)])                
      (bits->packet q)))
             
  (if (= (pop->decimal 1 q) 0)               ; get length type 
      (get-len-subs (pop->decimal 15 q) q)   ; get sub-packets by length
      (get-num-subs (pop->decimal 11 q) q))) ; get sub-packets by number

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

;; Packet -> Natural
;; Given a tree of Packet, return the sum of all the version fields in the tree
(define (sum-versions p)

  ; a couple of utility functions
  (define (atom? x)
    (and (not (null? x))
         (not (pair? x))))

  (define (leaf? x)
    (and (packet? x) (atom? (packet-operand x))))

  ;; now begin the CAR/CDR recusion through the tree
  (define (sum p)
    (cond  [(leaf? p) (packet-version p)]
           [else (sums (packet-operand p))]))

  (define (sums lop)
    (cond [(empty? lop) 0]
          [else (+ (sum (first lop))
                   (sums (rest lop)))]))
  (trace sum sums)
  (sum p))

(define (day16.1 input)
  (~> input              ; given a string of hex digits
      input->queue       ; turn into a queue of bits
      bits->packet       ; turn bits into packets
      sum-versions))     ; return the sum of the packet version numbers

(module+ test
  (check-equal? (day16.1 "8A004A801A8002F478") 16)
  ;(check-equal? (day16.1 "620080001611562C8802118E34") 12)
  ;(check-equal? (day16.1 "C0015000016115A2E0802F182340") 23)
  ;(check-equal? (day16.1  "A0016C880162017C3686B18A3D4780") 31)
  )

;(time (printf "2021 AOC Problem 16.1 = ~a\n"
; (day16.1 (file->string "input16.txt"))))



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