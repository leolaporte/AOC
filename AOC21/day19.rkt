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

(struct scanner (id x y z beacons) #:mutable #:transparent)
;; (scanner Byte Integer Integer Integer (listof Beacon))
;; each scanner has a unique id, an x y z position relative to 0,0,0
;; in the space, and a list of beacons it can see

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
;; the distance between the points
(define (dist x1 y1 z1 x2 y2 z2)
  (integer-sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)) (sqr (- z2 z1))))) ; round down

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

;; (listof Scanner) -> (setof Integer)
;; Given a list of scanners return the set of unique
;; beacon distances
(define (make-unique-distance-set los)
  (cond [(empty? los) (set)]
        [else (set-union (walk-beacons (scanner-beacons (first los)))
                         (make-unique-distance-set (rest los)))]))

;; (listof Beacon) -> (setof Integer)
;; given a list of beacons return a set of unique
;; beacon distances
(define (walk-beacons lob)
  (cond [(empty? lob) (set)]
        [else 
         (set-union
          (list->set (hash-values (beacon-distances (first lob))))
          (walk-beacons (rest lob)))]))

(define (day19.1 str)
  (~> str
      import-beacons            ; convert the string into a list of Scanner
      calculate-distances       ; populate list with inter-beacon distances
      make-unique-distance-set  ; create set of unique distances between beacons
      set->list                 ; convert to list so we can...
      length                    ; ...count it
  ))


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

(define test-data #<<here
--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14
here
  )