;; 2020 AOC Solutions 
;; Leo Laporte, 17 Dec 2020
;; Day 7

#lang racket

;  -- Day 7: Handy Haversacks ---
; 
; You land at the regional airport in time for your next flight. In fact, it
; looks like you'll even have time to grab some food: all flights are currently
; delayed due to issues in luggage processing.
; 
; Due to recent aviation regulations, many rules (your puzzle input) are being
; enforced about bags and their contents; bags must be color-coded and must
; contain specific quantities of other color-coded bags. Apparently, nobody
; responsible for these regulations considered how long they would take to enforce!
; 
; For example, consider the following rules:
; 
; light red bags contain 1 bright white bag, 2 muted yellow bags.
; dark orange bags contain 3 bright white bags, 4 muted yellow bags.
; bright white bags contain 1 shiny gold bag.
; muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
; shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
; dark olive bags contain 3 faded blue bags, 4 dotted black bags.
; vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
; faded blue bags contain no other bags.
; dotted black bags contain no other bags.
; 
; These rules specify the required contents for 9 bag types. In this example,
; every faded blue bag is empty, every vibrant plum bag contains 11 bags
; (5 faded blue and 6 dotted black), and so on.
; 
; You have a shiny gold bag. If you wanted to carry it in at least one other bag,
; how many different bag colors would be valid for the outermost bag?
; (In other words: how many colors can, eventually, contain at least one shiny gold bag?)
; 
; In the above rules, the following options would be available to you:
; 
;     A bright white bag, which can hold your shiny gold bag directly.
;     A muted yellow bag, which can hold your shiny gold bag directly, plus some
; other bags.
;     A dark orange bag, which can hold bright white and muted yellow bags, either
; of which could then hold your shiny gold bag.
;     A light red bag, which can hold bright white and muted yellow bags, either
; of which could then hold your shiny gold bag.
; 
; So, in this example, the number of bag colors that can eventually contain at
; least one shiny gold bag is 4.
; 
; How many bag colors can eventually contain at least one shiny gold bag?
; (The list of rules is quite long; make sure you get all of it.)


(require rackunit threading racket/dict memoize graph)

;; given problem data
(define INPUT  (file->string "input7.txt"))
(define EXAMPLE
  #<<DATA
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
DATA
  )

;; create a regular expression to parse each line as follows:
;;
;; a two-word color followed by " bags contain " and
;; EITHER
;;     a number, a color and "bag(s)" followed by
;;     EITHER
;;         "." in which break
;;         "," in wich case loop
;;     "no other bags" in which case break

(define bag-pattern (regexp "^([a-z]+\s[a-z]+) bags contain (((\d) ([a-z]+\s[a-z]+) bag[s]?,)*|\.|no other bags\.)$")) 


;; String -> Bag
;; Given a single rule as provided in the input, return a Bag
(define (make-bag str) empty) ; stub

(module+ test
  (check-equal? (make-bag "light red bags contain 1 bright white bag, 2 muted yellow bags.")
  (bag "light red" 0 (list (bag 1 "bright white" empty) (bag 2 "muted yellow" empty)))))

;; Bag -> Boolean
;; Returns true if a bag (or the bags it contains) has at least one shiny gold
;; bag within


(time (printf "AOC Problem 7.1 = ~a\n" 0))
        
;  


(module+ test)

(time (printf "AOC Problem 7.2 = ~a\n" 0))