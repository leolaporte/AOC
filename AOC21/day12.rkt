#lang racket
;  AOC 2021
; Leo Laporte 8-Jan-2022
; 
; --- Day 12: Passage Pathing ---
; 
; With your submarine's subterranean subsystems subsisting suboptimally, the only way you're
; getting out of this cave anytime soon is by finding a path yourself. Not just a path - the
; only way to know if you've found the best path is to find all of them.
; 
; How many paths through this cave system are there that visit small caves at most once?

(require threading
         rackunit
         racket/trace)

;; The first challenge is how to represent the input data. There's not that much of it.
;; I think we want to convert it into a hash of paths in the form (vertex . (list-of child vertices).
;; I could use a Racket Graph library but I'll learn more doing it by hand.

;; Gotcha 1: my first attempt made a directed graph, but the problem requires
;; an undirected graph - A->c means c->A so it's really A<->c. This was giving
;; numbers that were too low - I wasn't capturing legal paths like start-A-b-A-c-A-end

;; Gotcha 2: it's looping endlessly. I need to look at how it should terminate. Probably
;; need to track seen paths. Yep. Never clear visited list. Each path search takes
;; its own list with it, of course. Lesson learned.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                DATA                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Precompile regex strings for speed
(define graph-regex (pregexp "start|end|[a-z]{1,2}|[A-Z]{1,2}")) ; regex for conversion to graph
(define lower-case (regexp "[a-z]+"))  ; lower-case regex  

(define (list->hash list-of-edges)
  "makes an immutable hash from a list of edges such that
   for every vertex there's an entry (vertex . '(children))"
  (local [(define (list-vertices loe)
            "makes a list of all vertices in list-of-edges"
            (remove-duplicates (flatten loe) string=?))

          (define (children vertex loe)
            "returns the children of a vertex in a list-of-edges"
            (map second (filter (λ (x) (equal? vertex (first x))) loe)))

          (define (flip-pair lst)
            "swaps first and second items in a two-item list"
            (cons (second lst) (cons (first lst) empty)))]
    
    (for/hash ([vertex (in-list (list-vertices list-of-edges))])
      (values vertex
              (children vertex (append list-of-edges
                                       (map (λ (s) (flip-pair s)) list-of-edges)))))))

(define (edge-list->graph str) ; turn a list of edges into hash of paths
  (~> str
      (string-split _ "\n")
      (map (λ (x) (regexp-match* graph-regex x)) _)
      list->hash))

;; Provided test data
(define small (edge-list->graph "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")) 
(define medium (edge-list->graph "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"))
(define large (edge-list->graph "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"))

;; Problem input from adventofcode.com
(define full (edge-list->graph (file->string "input12.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               MAIN                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OK so now we need to count all the paths from start to end
;; allowing NO MORE than one visit to any small (lower-case) cave. That
;; means we'll only add small caves to the seen queue. This is a
;; depth-first search with backtracking using CAR/CDR recursion.
;; The visited list goes with the search - each path has its own
;; visited list. (In other words, no need to clear it.)

;; Vertex Vertex Graph -> Natural
;; Uses a depth-first search of graph to produce
;; a count of the number of paths from start to end that visit no lower
;; case vertex more than once
(define (count-paths start end graph)

  (local [(define (small-cave? v)
            "is the provided vertex lower-case? (i.e. a small cave, start, or end)"
            (regexp-match? lower-case v))]

    (define (next-path v visited)
      (cond [(equal? v end) 1]           ; success, add 1 to path count
            [(member v visited) 0]       ; been there, done that, not a path
            [else (cond [(small-cave? v) (set! visited (cons v visited))]) ; don't revisit small caves
                  (next-paths (hash-ref graph v) visited)])) ; continue search

    (define (next-paths lov visited)
      (cond [(empty? lov) 0]
            [else (+ (next-path (first lov) visited)
                     (next-paths (rest lov) visited))]))

    ; (trace next-paths) 
    (next-path start empty)))

(define (day12.1 graph)
  (count-paths "start" "end" graph)) 

(module+ test
  (check-equal? (day12.1 small) 10)
  (check-equal? (day12.1 medium) 19)
  (check-equal? (day12.1 large) 226))

(time (printf "2021 AOC Problem 12.1 = ~a\n" (day12.1 full)))

;  --- Part Two ---
; 
; After reviewing the available paths, you realize you might have time to visit
; a single small cave twice. Specifically, big caves can be visited any number of
; times, a single small cave can be visited at most twice, and the remaining small
; caves can be visited at most once. However, the caves named start and end can
; only be visited exactly once each: once you leave the start cave, you may not
; return to it, and once you reach the end cave, the path must end immediately.
; 
; Now, there are 36 possible paths through the first example
; 
; The slightly larger example above now has 103 paths through it, and the even larger
; example now has 3509 paths through it.
; 
; Given these new rules, how many paths through this cave system are there? 
; 

;; Notes: I can reuse my path search. But the logic for terminating a path
;; has changed: I can visit any one small cave twice, but the subsequent small
;; caves can only be visited once. And start and end can only be visited once.
;; I'll need to set a boolean once I've visited any small cave twice. Then
;; the previous rules apply. I'll use hurry? as my boolean: it starts false
;; but becomes true once we've visited a little cave twice.

;; This turned out kind of slow (.5 sec!) - possible optimizations: use set for
;; visited instead of list?

(define (leisurely-list-paths start end graph)

  (define little-cave-regex (pregexp "^[a-z]{1,2}$"))  ; lower-case with one or two letters

          (define (not-big-cave? v)
            "is the provided vertex lower-case? (i.e. a small cave start or end)"
            (regexp-match? lower-case v))
  
          (define (little-cave? v)
            "is the provided vertex lower-case and not 'start' or 'end'? (i.e. a one- or two-letter cave)"
            (regexp-match? little-cave-regex v))

    (define (next-path v visited hurry?)
      (cond [(equal? v end) 1]                                        ; success, add 1 to path count
            [(and (member v visited) hurry?) 0]                       ; in a hurry so no extra visits
            [(and (equal? v start) (member start visited)) 0]         ; can't visit start twice

            [else (cond [(not-big-cave? v)                            ; little cave or start?
                         (cond [(member v visited) (set! hurry? #t)]) ; now we're in hurry!
                         (set! visited (cons v visited))])            ; add to visited list
                  (next-paths (hash-ref graph v) visited hurry?)]))   ; continue search

    (define (next-paths lov visited hurry?)
      (cond [(empty? lov) 0]
            [else (+ (next-path (first lov) visited hurry?)
                     (next-paths (rest lov) visited hurry?))]))

    ; (trace next-path) 
    (next-path start empty #f)) ; start the path search

(define (day12.2 graph)
  (leisurely-list-paths "start" "end" graph)) 

(module+ test
  (check-equal? (day12.2 small) 36)
  (check-equal? (day12.2 medium) 103)
  (check-equal? (day12.2 large) 3509))

(time (printf "2021 AOC Problem 12.2 = ~a\n" (day12.2 full)))

#|
Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

;2021 AOC Problem 12.1 = 5756
;cpu time: 11 real time: 12 gc time: 0
;2021 AOC Problem 12.2 = 144603
;cpu time: 530 real time: 579 gc time: 19

2022 Mac Studio Max with 32GB RAM

2021 AOC Problem 12.1 = 5756
cpu time: 75 real time: 70 gc time: 62
2021 AOC Problem 12.2 = 144603
cpu time: 352 real time: 370 gc time: 7

|#

