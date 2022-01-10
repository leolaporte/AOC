#lang racket

;  --- Day 3: Toboggan Trajectory ---
; 
; With the toboggan login problems resolved, you set off toward the airport. While travel by toboggan might be easy, it's certainly not safe: there's very minimal steering and the area is covered in trees. You'll need to see which angles will take you near the fewest trees.
; 
; Due to the local geology, trees in this area only grow on exact integer coordinates in a grid. You make a map (your puzzle input) of the open squares (.) and trees (#) you can see. For example:
; 
; ..##.......
; #...#...#..
; .#....#..#.
; ..#.#...#.#
; .#...##..#.
; ..#.##.....
; .#.#.#....#
; .#........#
; #.##...#...
; #...##....#
; .#..#...#.#
; 
; These aren't the only trees, though; due to something you read about once involving arboreal genetics and biome stability, the same pattern repeats to the right many times:
; 
; ..##.........##.........##.........##.........##.........##.......  --->
; #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
; .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
; ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
; .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
; ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
; .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
; .#........#.#........#.#........#.#........#.#........#.#........#
; #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
; #...##....##...##....##...##....##...##....##...##....##...##....#
; .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
; 
; You start on the open square (.) in the top-left corner and need to reach the bottom (below the bottom-most row on your map).
; 
; The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers); start by counting all the trees you would encounter for the slope right 3, down 1:
; 
; From your starting position at the top-left, check the position that is right 3 and down 1. Then, check the position that is right 3 and down 1 from there, and so on until you go past the bottom of the map.
; 
; The locations you'd check in the above example are marked here with O where there was an open square and X where there was a tree:
; 
; ..##.........##.........##.........##.........##.........##.......  --->
; #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
; .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
; ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
; .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
; ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
; .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
; .#........#.#........X.#........#.#........#.#........#.#........#
; #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
; #...##....##...##....##...#X....##...##....##...##....##...##....#
; .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
; 
; In this example, traversing the map using this slope would cause you to encounter 7 trees.
; 
; Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?
; 


;; provided by AOC:
(define INPUT (file->lines "input3.txt"))

(define X-SLOPE 3)
(define Y-SLOPE 1)

(define RUN0 (list
              "..##......."
              "#...#...#.."
              ".#....#..#."
              "..#.#...#.#"
              ".#...##..#."
              "..#.##....."
              ".#.#.#....#"
              ".#........#"
              "#.##...#..."
              "#...##....#"
              ".#..#...#.#")) ; example given

;; List Integer Integer -> Integer
;; Given a run and slope returns the number of tree hits
(define (go-sledding run x y)
  (local [(define slope-width (string-length (list-ref run 1)))
          (define slope-height (length run))
          (define max-width (* slope-width slope-height))]
    (for/sum ([row (in-range y slope-height y)]
              [col (in-range x max-width x)]) 
      (if (tree? (list-ref run row) (modulo col slope-width))
          1
          0))))
 
(module+ test
  (require rackunit)
  (check-equal? (go-sledding RUN0 X-SLOPE Y-SLOPE) 7))

;; String -> Boolean
;; Produces true if the string at position p contains a tree ("#")
(define (tree? str p)
  (equal? "#" (substring str p (add1 p)))) 
 
(module+ test
  (check-equal? (tree? ".###...#.#.##..###..#...#...#.." 4) #false)
  (check-equal? (tree? ".###...#.#.##..###..#...#...#.." 3) #true))

(time (printf "AOC Problem 3.1 = ~a\n" (go-sledding INPUT X-SLOPE Y-SLOPE)))
        
;  --- Part Two ---
; 
; Time to check the rest of the slopes - you need to minimize the probability of
; a sudden arboreal stop, after all.
; 
; Determine the number of trees you would encounter if, for each of the following
; slopes, you start at the top-left corner and traverse the map all the way to
; the bottom:
; 
;     Right 1, down 1.
;     Right 3, down 1. (This is the slope you already checked.)
;     Right 5, down 1.
;     Right 7, down 1.
;     Right 1, down 2.
; 
; In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s)
; respectively; multiplied together, these produce the answer 336.
; 
; What do you get if you multiply together the number of trees encountered on each of the listed slopes?
; 


(time (printf "AOC Problem 3.2 = ~a\n" (* (go-sledding INPUT 1 1)
                                          (go-sledding INPUT 3 1)
                                          (go-sledding INPUT 5 1)
                                          (go-sledding INPUT 7 1)
                                          (go-sledding INPUT 1 2))))
