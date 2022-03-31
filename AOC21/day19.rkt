#lang racket

;AOC Day 19
;Leo Laporte 7-Mar-2022

#|==============================================================================

                      --- Day 19: Beacon Scanner ---

"Unfortunately, while each scanner can report the positions of all detected
beacons relative to itself, the scanners do not know their own position.
You'll need to determine the positions of the beacons and scanners yourself.
...
How many beacons are there?"
            
================================================================================|#

(require rackunit
         threading
         racket/trace)

#|==============================================================================|#
#|                                      DATA                                    |#
#|==============================================================================|#

(struct beacon (id x y z distances scanners) #:mutable #:transparent)
;; (beacon Byte Integer Integer Integer Hash (listof Scanner))
;; each beacon has a unique id, its (relative to its scanner) x y z position,
;; calculated distances to all other beacons that scanner sees, and a list
;; of all the scanners that can see it

(struct scanner (id x y z roll pitch yaw beacons) #:mutable #:transparent)
;; (scanner Byte Integer Integer Integer Integer Integer Integer (listof Beacon))
;; each scanner has a unique id, an x y z position relative to 0,0,0
;; in the space, its orientation in the space (as described by roll
;; pitch and yaw) and a list of beacons it can see

(define (import-beacons str)
  (~> str
      (string-replace _ "---" "")      ; strip out useless jonque
      (string-replace _ "scanner" "")  ; ditto
      (string-split _ "\n\n")          ; split string into scanner entries
      (map build-scanner _)))          ; turn each scanner string in list into Scanner struct

;; String -> Scanner
;; given a string create a new scanner object with it
(define (build-scanner str)
  (let* ([l (string-split str "\n")]                    ; split the entry into id and beacon list
         [id (string->number (string-trim (first l)))]) ; trim the spaces around the scanner id
    (scanner
     id
     0 0 0  ; we don't yet know its absolute coordinates
     0 0 0  ; nor roll pitch or yaw
     (build-beacon-list (rest l) id))))                 ; add the beacon list

;; the regexp for separating the beacon coordinates
(define xyz (pregexp "(-?\\d{1,3})"))  ; a 1-3 digit signed integer

;; (listof String) Integer -> Beacon
;; given a list of coordinates and the scanner id the coords
;; came from, build a list of beacons
(define (build-beacon-list l id)
  (for/list ([i (in-range (length l))])               ; for every entry in the list
    (let ([part (regexp-match* xyz (list-ref l i))])  ; separate out the coordinates
      (beacon
       i                                ; assign a unique id to this beacon
       (string->number (first part))    ; x 
       (string->number (second part))   ; y 
       (string->number (third part))    ; z
       (make-hash)                      ; distances hash: key is other beacon, val is dist
       (list id)))))                    ; list of scanners starting with the source


#|==============================================================================|#
#|                                     NOTES                                    |#
#|==============================================================================|#

#|

Tempting to oversolve this, and I might need to for part two, but all I really
need for part one is a unique list of beacons so I can count them.

While I have a list of 30 scanners and as many as 780 beacons I know there's
some overlap. Many beacons are seen by multiple scanners. So I need a way of
fingerprinting beacons. The problem suggests a much more complicated system
of finding the exact position of each beacon - is that really necessary?

If I calculate the distances between all the beacons seen by a given scanner,
I think that should be a way to compare beacons belonging to multiple scanners
and eliminate scanners that are counted more than once. So...

1. Import all the data making a list of scanners and their beacons
2. Create a set of distances for every scanner/beacon to all the other scanner/beacons 
3. When there's overlap of >5? points (we'll have to look at the data to get
an idea of this number) that's a duplicate beacon. Assign it to all the scanners
that can see it.

At the end of this process, count the beacons, eliminating duplicates.

One extra thing I'll do, assuming that part two is going to want me to figure
the absolute position of each beacon, is keep track of which scanners can see
any given beacon. (That will make it easier to figure out the 12 overlapping
beacons the problem set refers to.)

|#

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

;; Integer Integer Integer Integer Integer Integer -> Integer
;; given two points in a three-dimensional space, return
;; the distance between the points (well actually not,
;; the actual distance would be the square root of the sum
;; but there's no need for that. I'll keep it an integer.)
(define (dist x1 y1 z1 x2 y2 z2)
   (+ (sqr (- x2 x1)) (sqr (- y2 y1)) (sqr (- z2 z1)))) 

;; (listof Scanner) -> (listof Scanner)
;; Given a list of scanner entries (formatted as above) calculate
;; the distances for each associated beacon to every other beacon
;; and add them to the hash-set beacon-distances.
(define (calculate-distances scanners)
  (for/list ([i (in-range (length scanners))])    ; for all the scanners in the list
    (let ([s (list-ref scanners i)])
      (scanner                                    ; create a new scanner
       (scanner-id s)                             ; with stuff we know
       (scanner-x s) (scanner-y s) (scanner-z s)  ; ditto
       (scanner-roll s) (scanner-pitch s) (scanner-yaw s) ; ditto
       (beacon-dists (scanner-beacons s))))))     ; and a list of beacons with distances

;; (listof Beacon) -> (listof Beacon)
;; given a list of beacons return the list with a populated distance hash 
;; containing the distance between each beacon and all the other beacons. 
(define (beacon-dists bs)
  (for/list ([source (in-range (length bs))])    ; for all the beacons in the list
    (let ([s (list-ref bs source)])
      (beacon                                    ; build a beacon structure
       (beacon-id s)                             ; with stuff we already know
       (beacon-x s) (beacon-y s) (beacon-z s)    ; ditto
       
       (for/hash ([dest (in-range (length bs))]) ; plus the calculated distance hash
         (let ([d (list-ref bs dest)])
           (values (beacon-id d)
                   (dist (beacon-x s) (beacon-y s) (beacon-z s)
                         (beacon-x d) (beacon-y d) (beacon-z d)))))
      
       (beacon-scanners s)))))                    ; more stuff we already know

;; NOTES:
;; Now to populate beacon-scanners with the ID of every scanner that can see a given beacon
;; I will assume that the beacon is shared by a scanner if more than MIN-MATCHES of distances
;; match between two beacons.
;;
;; Once I KNOW a beacon is seen by two different scanners I can adjust the second scanner's position
;; and orientation relative to scanner 0 using the two sets of coordinates for that beacon.


;(define (beacon-matches scanners)
;  (for/list (scanner (in-range (length scanners)))
;    (let ([s (list-ref scanners scanner)])
;      (for/hash (beacon (in-range (length (scanner-beacons s))))
;        (let ([b (list-ref (scanner-beacons s))])
;          (values (cons (scanner-id s) (beacon-id b))
;                  (shared-dists (hash-values (beacon-distances b)))))))))

;; (listof Natural) -> (listof cons)
;; Given a list of a beacon's distances to other points in its space
;; return a list of scanner.beacon pairs that have MIN-MATCHES or more points
;; in common
(define MIN-MATCHES 6)


 


  (define (day19.1 str)
    (~> str
        import-beacons            ; convert the string into a list of Scanner
        calculate-distances       ; populate list with inter-beacon distances
        ))

(require "test-data-19.rkt")

(define s (day19.1 test-data))
(define bs (scanner-beacons (first s)))


  ;(module+ test
  ;  (check-equal? (day19.1 test-data) 79))
  ;
  ;(time (printf "2021 AOC Problem 19.1 = ~a\n" (day19.1 (file->string "input19.txt"))))

  #|=================================================================================
                                        PART 2
                               

==================================================================================|#

  ;(module+ test
  ;  (check-equal? (day19.2 test-data) 0))

  ; (time (printf "2021 AOC Problem 19.2 = ~a\n" (day19.2 input)))

  #|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM



|#
