#lang racket
(require test-engine/racket-tests)
(struct position (row column) #:transparent)

;; (listof lists) -> hash
;; Build a hash out of a list of lists. Each inner list is a key-value
;; pair. The returned hash maps from the keys to the values in the
;; pairs.
(define (list->hash list-of-key-value-pairs)
  (foldl
    (lambda (lst hsh)
            (hash-set hsh (first lst) (second lst)))
    (hash)
    list-of-key-value-pairs))

(check-expect
  (list->hash '(("apple" red) ("banana" yellow)))
  (hash "apple" 'red "banana" 'yellow))
(check-expect
  (list->hash (list (list (position 0 0) 'A)
                    (list (position 0 1) 'B)))
  (hash (position 0 0) 'A (position 0 1) 'B))

;; (list-of-lists any-func) -> (any-func height width big-list)
;; This is a generic function that processes a boggle board. It calls the "func"
;; with the height of the board, the width of the board, and a list of all the
;; cells of the board.
(define (process-board board func)
  (local [(define height (length board))
          (define width (length (first board)))
          (define big-list
            (foldr append empty board))]
         (func height width big-list)))

;; (listof lists) -> (hashof positions to letters)
;; Build a hash mapping from positions to letters
(define (build-position-to-letter board)
  (local [(define (build-hash height width big-list)
            (list->hash
              (for/list ([index (in-naturals)]
                         [letter big-list])
                        (let ([row (quotient index width)]
                              [column (remainder index width)])
                          (list (position row column) letter)))))]
         (process-board board build-hash)))

(check-expect (build-position-to-letter
                '((A B)
                    (C D)))
              (hash (position 0 0) 'A
                    (position 0 1) 'B
                    (position 1 0) 'C
                    (position 1 1) 'D))

;; (listof lists) -> (hashof position to neighbors)
;; Build a hash mapping from positions to their neighbors which are a list of
;; positions
(define (build-position-to-neighbor board)
  (local [(define (build-neighbors height width big-list)
            (list->hash
              (for/list ([index (in-naturals)]
                         [letter big-list])
                        (let* ([row (quotient index width)]
                               [column (remainder index width)]
                               [neighbors (filter
                                            (lambda (position)
                                                    (and
                                                      (<= 0 (position-row position) (sub1 height))
                                                      (<= 0 (position-column position) (sub1 width))))
                                            (list
                                              (position (sub1 row) (sub1 column))
                                              (position (sub1 row) column)
                                              (position (sub1 row) (add1 column))
                                              (position row (sub1 column))
                                              (position row (add1 column))
                                              (position (add1 row) (sub1 column))
                                              (position (add1 row) column)
                                              (position (add1 row) (add1 column))))])
                          (list (position row column) neighbors)))))]
         (process-board board build-neighbors)))

(check-expect (build-position-to-neighbor
                '((A B)
                    (C D)))
              (hash (position 0 0) (list (position 0 1) (position 1 0) (position 1 1))
                    (position 0 1) (list (position 0 0) (position 1 0) (position 1 1))
                    (position 1 0) (list (position 0 0) (position 0 1) (position 1 1))
                    (position 1 1) (list (position 0 0) (position 0 1) (position 1 0))))

;; (listof lists) -> (hashof positions to booleans)
;; Return a hash mapping from positions to false The booleans indicate that none
;; of the cells has been visited.
(define (build-visited board)
  (process-board board
              (lambda (height width big-list)
                      (list->hash
                        (for/list ([index (in-naturals)]
                                   [letter big-list])
                                  (let ([row (quotient index width)]
                                        [column (remainder index width)])
                                    (list (position row column) false)))))))

(check-expect (build-visited
                '((A B)
                    (C D)))
              (hash (position 0 0) false
                    (position 0 1) false
                    (position 1 0) false
                    (position 1 1) false))


;; build-positions : (listof lists) -> (listof positions)
;; Given a boggle board, return a list of positions (struct) for all the cells
;; on the board.
(define (build-positions board)
  (process-board board
              (lambda (height width big-list)
                      (for/list ([index (in-naturals)]
                                 [letter big-list])
                                (let ([row (quotient index width)]
                                      [column (remainder index width)])
                                  (position row column))))))

(check-expect (build-positions
                '((A B)
                  (C D)))
              (list
                (position 0 0)
                (position 0 1)
                (position 1 0)
                (position 1 1)))

;; (listof symbols) -> string
;; Return a word from a list of symbols
(define (list-of-symbols-to-word symbols)
  (foldl
    (lambda (sym word)
            (string-append word (symbol->string sym)))
    ""
    symbols))

(check-expect (list-of-symbols-to-word '(h e l l o)) "hello")

;; string hash -> boolean
;; Determine if a given string is a word or not using a dictionary
(define (is-a-word? str dictionary)
  (set-member? dictionary str))

(check-expect (is-a-word? "hello" (set "hello" "world")) true)
(check-expect (is-a-word? "print" (set "hello" "world")) false)

;; string -> set
;; Build a dictionary set from a dictionary file
(define (build-dictionary filename)
  (foldl
    (lambda (sym a-set)
            (set-add a-set (string-upcase (symbol->string sym))))
    (set)
    (file->list filename)))

;; How can I test build-dictionary?

;; (listof lists) position hash hash -> set
;; Given:
;; * a boggle board
;; * a starting position
;; * a hash mapping from positions to their neighbors
;; * a hash mapping from positions to letters
;;
;; Return all the possible strings starting from the given position.
(define (find-strings-from-position board position position-to-neighbors position-to-letter)
  (local [(define (breadth-first-search partial-string
                                        visited
                                        position-to-neighbors
                                        position-to-letter)
            (foldl
              (lambda (neighbor string-set)
                      (if (hash-ref visited neighbor)
                        string-set
                        (let* ([neighbor-letter (hash-ref position-to-letter neighbor)]
                               [new-partial-string (append partial-string (list neighbor-letter))]
                               [word (list-of-symbols-to-word new-partial-string)]
                               [new-visited (hash-set visited neighbor true)]
                               [rest-words (breadth-first-search new-partial-string
                                                                 new-visited
                                                                 position-to-neighbors
                                                                 position-to-letter)])
                          (set-union (set-add string-set word) rest-words))))
              (set)
              (hash-ref position-to-neighbors position)))]
         (breadth-first-search
           (list (hash-ref position-to-letter position))
           (hash-set (build-visited board) position true)
           position-to-neighbors
           position-to-letter)))

(check-expect (find-strings-from-position
                '(('A 'B) ('C 'D))
                (position 0 0)
                (hash (position 0 0) (list (position 0 1) (position 1 0) (position 1 1))
                      (position 0 1) (list (position 0 0) (position 1 0) (position 1 1))
                      (position 1 0) (list (position 0 0) (position 0 1) (position 1 1))
                      (position 1 1) (list (position 0 0) (position 0 1) (position 1 0)))
                (hash (position 0 0) 'A
                      (position 0 1) 'B
                      (position 1 0) 'C
                      (position 1 1) 'D))
              (set "AB" "AC" "AD" "ABC" "ABD" "ACD" "ACB" "ADB" "ADC" "ABCD" "ABDC" "ACBD"
                   "ACDB" "ADBC" "ADCB"))


;; (listof lists) position hash hash -> set
;; Given:
;; * a boggle board
;; * a starting position
;; * a hash mapping from positions to their neighbors
;; * a hash mapping from positions to letters
;; * a dictionary
;;
;; Return all the possible words starting from the given position.
(define (find-words-from-position board position position-to-neighbors position-to-letter dictionary)
  (let [(strings-from-position (find-strings-from-position board position
                                                           position-to-neighbors
                                                           position-to-letter))]
    (list->set (filter (lambda (str) (is-a-word? str dictionary))
                       (set->list strings-from-position)))))

(check-expect (find-words-from-position
                '((A B) (C D))
                (position 0 0)
                (hash (position 0 0) (list (position 0 1) (position 1 0) (position 1 1))
                      (position 0 1) (list (position 0 0) (position 1 0) (position 1 1))
                      (position 1 0) (list (position 0 0) (position 0 1) (position 1 1))
                      (position 1 1) (list (position 0 0) (position 0 1) (position 1 0)))
                (hash (position 0 0) 'A
                      (position 0 1) 'B
                      (position 1 0) 'C
                      (position 1 1) 'D)
                (set "ABC" "ABCD" "hello" "world"))
              (set "ABC" "ABCD"))

;; find-all-words : (listof lists) -> (setof strings)
;; Find all possible words of a boggle board. The basic idea is:
;; Find all the words starting from every single cell of the board.
;; Return all the words found above.
(define (find-all-words board)
  (let ([position-to-letter (build-position-to-letter board)]
        [position-to-neighbors (build-position-to-neighbor board)]
        [visited (build-visited board)]
        [positions (build-positions board)]
        [dictionary (build-dictionary  "/usr/share/dict/web2")])
    (list-of-sets->set
      (map
        (lambda (position)
                (find-words-from-position board
                                          position
                                          position-to-neighbors
                                          position-to-letter
                                          dictionary))
        positions))))

(check-expect (find-all-words
                '((A B) (C D)))
              (set "BA" "CA" "DA" "AB" "AD" "BAC" "CAB" "DAB" "BAD" "CAD"))

;; (listof sets) -> set
;; Given a list of sets, return a set that is the union of all the given sets
(define (list-of-sets->set list-of-sets)
  (foldl
   (lambda (a-set final-set)
           (set-union final-set a-set))
    (set)
    list-of-sets))

(check-expect (list-of-sets->set
                (list (set 1 2)
                      (set 3 4)
                      (set 1 2 3 4)))
              (set 1 2 3 4))

(find-all-words
  (list
    '(C O)
    '(A R))
  ;(list
  ;  '(C O D A)
  ;  '(A R N P)
  ;  '(O S E L)
  ;  '(I R U M))
  ;(list
  ;  '(O A A N)
  ;  '(E T R I)
  ;  '(I H K R)
  ;  '(I F L V))
  )
;
(test)
