#lang racket
(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either)

(define full "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.")
(define empty "faded blue bags contain no other bags.")

(define word/p  ; parses a single word and returns it as a string
  (do (word <- (many/p letter/p))
    (pure (list->string word))))

(define color/p ; parses a two word string
  (do (modifier <- word/p)
    space/p
    (color <- word/p)
    (pure (string-append modifier " " color))))

(define abag/p  ; parses a single bag
  (do (cnt <- integer/p)
    space/p
    (color <- color/p)
    


(parse-string color/p full)
