#lang racket
;  AOC 2021
; Leo Laporte 28-29 Dec 2021
; 
; --- Day 10: Syntax Scoring ---
; 
; You ask the submarine to determine the best route out of the deep-sea cave,
; but it only replies:
; 
; Syntax error in navigation subsystem on line: all of them
; All of them?! The damage is worse than you thought. You bring up a copy of
; the navigation subsystem (your puzzle input).
; 
; The navigation subsystem syntax is made of several lines containing chunks.
; There are one or more chunks on each line, and chunks contain zero or more
; other chunks. Adjacent chunks are not separated by any delimiter; if one
; chunk stops, the next chunk (if any) can immediately start. Every chunk
; must open and close with one of four legal pairs of matching characters:
; 
; If a chunk opens with (, it must close with ).
; If a chunk opens with [, it must close with ].
; If a chunk opens with {, it must close with }.
; If a chunk opens with <, it must close with >.
; 
; So, () is a legal chunk that contains no other chunks, as is []. More complex
; but valid chunks include ([]), {()()()}, <([{}])>, [<>({}){}[([])<>]], and
; even (((((((((()))))))))).
; 
; Some lines are incomplete, but others are corrupted. Find and discard the
; corrupted lines first.
; 
; A corrupted line is one where a chunk closes with the wrong character - that
; is, where the characters it opens and closes with do not form one of the four
; legal pairs listed above.
; 
; Examples of corrupted chunks include (], {()()()>, (((()))}, and <([]){()}[{}]).
; Such a chunk can appear anywhere within a line, and its presence causes the
; whole line to be considered corrupted.
; 
; For example, consider the following navigation subsystem:
; 
; [({(<(())[]>[[{[]{<()<>>
; [(()[<>])]({[<{<<[]>>(
; {([(<{}[<>[]}>{[]{[(<()>
; (((({<>}<{<{<>}{[]{[]{}
; [[<[([]))<([[{}[[()]]]
; [{[{({}]{}}([{[{{{}}([]
; {<[[]]>}<{[{[{[]{()[[[]
; [<(<(<(<{}))><([]([]()
; <{([([[(<>()){}]>(<<{{
; <{([{{}}[<[[[<>{}]]]>[]]
;         
; Some of the lines aren't corrupted, just incomplete; you can ignore these
; lines for now. The remaining five lines are corrupted:
; 
; {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
; [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
; [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
; [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
; <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.
;        
; Stop at the first incorrect closing character on each corrupted line.
; 
; Did you know that syntax checkers actually have contests to see who can
; get the high score for syntax errors in a file? It's true! To calculate
; the syntax error score for a line, take the first illegal character on
; the line and look it up in the following table:
; 
; ): 3 points.
; ]: 57 points.
; }: 1197 points.
; >: 25137 points.
; 
; In the above example, an illegal ) was found twice (2*3 = 6 points),
; an illegal ] was found once (57 points), an illegal } was found once
; (1197 points), and an illegal > was found once (25137 points). So,
; the total syntax error score for this file is 6+57+1197+25137 = 26397 points!
; 
; Find the first illegal character in each corrupted line of the navigation
; subsystem. What is the total syntax error score for those errors?


(require threading
         rackunit)

;; Problem input from adventofcode.com
(define problem-data (string-split (file->string "input10.txt") "\n"))

(define test-data
  (string-split "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"
                "\n"))

;; NOTES:
;; to simplify an expression look for matching pairs () {} [] <> and eliminate them.
;; Once an expression cannot be simplified further you've found a bug or an incomlete
;; statement.
;; The incomplete statement will have no closing delimiters.
;; If statement is not incomplete it's bugged.
;; The first closing delimiter is the bug.

;; Regexp patterns
(define PAIRS (regexp "<>|\\[\\]|\\{\\}|\\(\\)")) ; matching opens and closes
(define CLOSES (regexp ">|\\]|\\}|\\)"))          ; the closing delimiters

;; String -> String
;; Removes matching pairs from string (repeat until no more pairs)
(define (strip-pairs str)
  (let ([shrunk (regexp-replace* PAIRS str "")])       
    
    (cond [(false? (regexp-match PAIRS shrunk)) shrunk]  
          [else (strip-pairs shrunk)])))                 

;; String -> Natural
;; Given a string return the score of the first bug or 0 if no bugs
(define (bug-bounty str)
  (match (regexp-match CLOSES str)
    ['(")") 3]
    ['("]") 57]
    ['("}") 1197]
    ['(">") 25137]
    [#f 0]))

(module+ test
  (check-equal? (bug-bounty (strip-pairs "{([(<{}[<>[]}>{[]{[(<()>")) 1197)
  (check-equal? (bug-bounty (strip-pairs "[[<[([]))<([[{}[[()]]]")) 3)
  (check-equal? (bug-bounty (strip-pairs "[{[{({}]{}}([{[{{{}}([]")) 57)
  (check-equal? (bug-bounty (strip-pairs "[<(<(<(<{}))><([]([]()")) 3)
  (check-equal? (bug-bounty (strip-pairs "<{([([[(<>()){}]>(<<{{")) 25137) 
  (check-equal? (bug-bounty (strip-pairs "[({(<(())[]>[[{[]{<()<>>")) 0))

;; (list-of String) -> Natural
;; Given a list of navigation strings return the total bug-bounty for
;; strings with errors
(define (day10.1 input)
  (for/sum ([str (in-list input)])
    (bug-bounty (strip-pairs str))))

(module+ test
  (check-equal? (day10.1 test-data) 26397))

(time (printf "2021 AOC Problem 10.1 = ~a\n" (day10.1 problem-data)))

;  --- Part Two ---
; 
; Now, discard the corrupted lines. The remaining lines are incomplete.
; 
; Incomplete lines don't have any incorrect characters - instead, they're missing
; some closing characters at the end of the line. To repair the navigation subsystem,
; you just need to figure out the sequence of closing characters that complete all
; open chunks in the line.
; 
; You can only use closing characters (), ], }, or >), and you must add them in the
; correct order so that only legal pairs are formed and all chunks end up closed.
; 
; In the example above, there are five incomplete lines:
; 
; [({(<(())[]>[[{[]{<()<>> - Complete by adding }}]])})].
; [(()[<>])]({[<{<<[]>>( - Complete by adding )}>]}).
; (((({<>}<{<{<>}{[]{[]{} - Complete by adding }}>}>)))).
; {<[[]]>}<{[{[{[]{()[[[] - Complete by adding ]]}}]}]}>.
; <{([{{}}[<[[[<>{}]]]>[]] - Complete by adding ])}>.
; 
; Did you know that autocomplete tools also have contests? It's true! The score is
; determined by considering the completion string character-by-character. Start with
; a total score of 0. Then, for each character, multiply the total score by 5 and
; then increase the total score by the point value given for the character in the
; following table:
; 
; ): 1 point.
; ]: 2 points.
; }: 3 points.
; >: 4 points.
; 
; So, the last completion string above - ])}> - would be scored as follows:
; 
; Start with a total score of 0.
; Multiply the total score by 5 to get 0,
; then add the value of ] (2) to get a new total score of 2.
; 
; Multiply the total score by 5 to get 10,
; then add the value of ) (1) to get a new total score of 11.
; 
; Multiply the total score by 5 to get 55,
; then add the value of } (3) to get a new total score of 58.
; 
; Multiply the total score by 5 to get 290,
; then add the value of > (4) to get a new total score of 294.
; 
; The five lines' completion strings have total scores as follows:
; 
; }}]])})] - 288957 total points.
; )}>]}) - 5566 total points.
; }}>}>)))) - 1480781 total points.
; ]]}}]}]}> - 995444 total points.
; ])}> - 294 total points.
; 
; Autocomplete tools are an odd bunch: the winner is found by sorting all
; of the scores and then taking the middle score. (There will always be
; an odd number of scores to consider.) In this example, the middle score
; is 288957 because there are the same number of scores smaller and larger than it.
; 
; Find the completion string for each incomplete line, score the completion
; strings, and sort the scores. What is the middle score?
; 


;; Notes: Easy peasy. We already have the routines we need to find the missing
;; closing characters, except we don't need to. Strip-pairs reduces us to the
;; opening chars in order. Score each string, find the middle and bob's your uncle. 

(define (day10.2 input)
  (~> (map strip-pairs input)  ; reduce all strings to unmatched char pairs
      (filter complete? _)     ; eliminate buggy strings
      (map string->list _)     ; convert strings to lists of chars
      (map reverse _)          ; flip each string
      (map score _)            ; calculate the score for each string
      (middle _)))             ; find the middle score

;; String -> Boolean
;; returns true if str is complete (not buggy)
(define (complete? str)
  (false? (regexp-match CLOSES str)))

(module+ test
  (check-equal? (complete? "{{[[({([") #t)
  (check-equal? (complete? "{{[[({([>") #f))

;; (list-of Char) -> Natural
;; Calculates the score of a list of chars (note we're matching the left
;; side of the pair because that's what we've got).
(define (score loc)
  (foldl (λ (c total)
           (+ (* total 5)
              (match c
                [#\( 1]
                [#\[ 2]
                [#\{ 3]
                [#\< 4])))
         0 loc))
  
(module+ test
  (check-equal? (score (string->list "{{[[({([")) 288957)
  (check-equal? (score (string->list "({<[{(")) 5566)
  (check-equal? (score (string->list "{{<{<((((")) 1480781))

;; (list-of Natural) -> Natural
;; given an odd length list of numbers, return the value in the center of the list
(define (middle lst)
  (let ([l (length lst)])
    (if (odd? l)
        (~> (sort lst <)
            (list-tail _ (quotient l 2))
            (first _))
        ("Error: list is not odd"))))

(module+ test
  (check-equal? (middle '(1 2 3 4 5)) 3)
  (check-equal? (middle '(1 2 3 4 5 6 7)) 4))
     
(module+ test
  (check-equal? (day10.2 test-data) 288957))

(time (printf "2021 AOC Problem 10.2 = ~a\n" (day10.2 problem-data)))

; Time to solve, in milliseconds, on a 2021 M1 Pro MacBook Pro 14" with 16GB RAM

; 2021 AOC Problem 10.1 = 392043
; cpu time: 7 real time: 7 gc time: 0
; 2021 AOC Problem 10.2 = 1605968119
; cpu time: 6 real time: 6 gc time: 0
; 
