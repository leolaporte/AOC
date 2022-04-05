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

(struct beacon (id x y z distances) #:mutable #:transparent)
;; (beacon Pair Integer Integer Integer (list-of Integer))
;; each beacon has a unique id, its (relative to its scanner) x y z position,
;; calculated distances to all other beacons that scanner sees

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
     (build-beacon-list (rest l) id))))  ; add the beacon list

;; the regexp for separating the beacon coordinates
(define xyz (pregexp "(-?\\d{1,3})"))  ; a 1-3 digit signed integer

;; (listof String) Integer -> Beacon
;; given a list of coordinates and the scanner id the coords
;; came from, build a list of beacons
(define (build-beacon-list l id)
  (for/list ([i (in-range (length l))])               ; for every entry in the list
    (let ([part (regexp-match* xyz (list-ref l i))])  ; separate out the coordinates
      (beacon                           ; now build a beacon structure:
       (cons id i)                      ; assign a unique id to this beacon
       (string->number (first part))    ; x 
       (string->number (second part))   ; y 
       (string->number (third part))    ; z
       empty))))                        ; list of distances to other beacons


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
an idea of this number) that's a duplicate beacon. Make a list of all the
beacons and all the scanners that can see it. 

At the end of this process, count the beacons, eliminating duplicates.

|#

#|==============================================================================|#
#|                                     CODE                                     |#
#|==============================================================================|#

;; Beacon Beacon -> Integer
;; given two Beacons return the distance between the Beacons
;; (well no, the actual distance would be the square
;; root of the sum but there's no need for that. I'll keep it
;; an integer.)
(define (fingerprint b1 b2)
  (define-values (x1 y1 z1) (values (beacon-x b1) (beacon-y b1) (beacon-z b1)))
  (define-values (x2 y2 z2) (values (beacon-x b2) (beacon-y b2) (beacon-z b2)))
  (+ (sqr (- x2 x1)) (sqr (- y2 y1)) (sqr (- z2 z1))))

;; (listof Scanner) -> (listof Scanner)
;; Given a list of scanner entries calculate
;; the distances for each associated beacon to every other beacon
;; and add them to the list of beacon-distances.
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
       
       (remove 0                                 ; we don't need a beacon's dist to itself
               (for/list ([dest (in-range (length bs))]) ; a list of calculated fingerprints
                 (let ([d (list-ref bs dest)])
                   (fingerprint s d))))))))        

;; NOTES: Now I can walk the scanner list and make a list of unique beacon IDS.
;; Beacons with the same fingerprint will be concatenated into a single list
;; inside the list of beacons. e.g. if 3 4 and 5 are the same beacon return
;; the list '(1 2 '(3 4 5) 6 7)

;; First create a list of all beacons

;; (list-of Scanners) -> (list-of Beacon)
;; given a list of scanners and their beacons return a list
;; of individual beacons
(define (flatten-beacons scanners)
  (flatten
   (for/list ([index (in-range (length scanners))])
     (scanner-beacons (list-ref scanners index)))))

;; Now find duplicates in that list

;; (list-of Beacon) -> (list-of (list-of Beacons))
;; Given a list of beacons consolidate it so each item in the list
;; is a list of beacon ids that match a single beacon
(define (combine-dupes beacons deduped)
  (cond [(empty? beacons) deduped]
        [else
         (combine-dupes                       ; recurse with...
          
          (filter-not                         ; a list of the remaining, unmatched, beacons
           (λ (b) (beacons-match? (first beacons) b)) beacons)
          
          (cons                               ; and accumulate the matching beacons
           (map (λ (b) (beacon-id b))         ; well, just the ids 
                (filter (λ (b) (beacons-match? (first beacons) b)) beacons)) ; matches
           deduped))]))                       ; onto the list of deduped beacons
       
;; Beacon Beacon -> Boolean
;; returns true if two beacons have more than MIN-MATCHES shared
;; distance fingerprints
(define MIN-MATCHES 4)

(define (beacons-match? b1 b2)
  (> (length (set-intersect (beacon-distances b1) (beacon-distances b2))) MIN-MATCHES)) 

(define (day19.1 str)
  (~> str
      import-beacons            ; convert the string into a list of Scanner
      calculate-distances       ; populate list with inter-beacon distances
      flatten-beacons           ; create a list of all beacons
      (combine-dupes _ empty)   ; combine it into a unique list
      length))                  ; so how many is that?

(require "test-data-19.rkt")

(module+ test
  (check-equal? (day19.1 test-data) 79))

(time (printf "2021 AOC Problem 19.1 = ~a\n" (day19.1 (file->string "input19.txt"))))

#|=================================================================================
                                        PART 2
                               

==================================================================================|#

;(module+ test
;  (check-equal? (day19.2 test-data) 0))

; (time (printf "2021 AOC Problem 19.2 = ~a\n" (day19.2 input)))

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM



|#
