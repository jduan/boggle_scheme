#lang racket
(struct position (row column) #:transparent)

;; input: a list of pairs
;; output: a hash mapping from the first of the paris to the second of the pairs
(define (list-to-hash list-of-pairs)
  (foldl
    (lambda (lst hsh)
            (hash-set hsh (first lst) (second lst)))
    (hash)
    list-of-pairs))

;; input:
;; board: a boggle board
;; func: a function that get's called with the height, the width, and a single
;; list of all the rows appended.
;;
;; output:
;; whatever is returned by func
;;
(define (preprocess board func)
  (local [(define height (length board))
          (define width (length (first board)))
          (define big-list
            (foldr append empty board))]
         (func height width big-list)))

;; input:
;; height: the height of the board
;; width: the width of the board
;; big-list: the list of all the rows of the board
;;
;; output:
;; a hash mapping from positions to letters
(define (build-hash height width big-list)
  (list-to-hash
    (for/list ([index (in-naturals)]
               [letter big-list])
              (let ([row (quotient index width)]
                    [column (remainder index width)])
                (list (position row column) letter)))))

;; input:
;; board; a boggle board
;;
;; output:
;; a hash mapping from positions to letters
(define (build-position-to-letter board)
  (preprocess board build-hash))

;; input:
;; height: the height of the board
;; width: the width of the board
;; big-list: the list of all the rows of the board
;;
;; output:
;; a hash mapping from positions to their neighbors (a list of positions)
(define (build-neighbors height width big-list)
  (list-to-hash
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
                (list (position row column) neighbors)))))

(define (build-position-to-neighbor board)
  (preprocess board build-neighbors))

;; Return a hash mapping from position to a boolean (if its' been visited)
(define (build-visited board)
  (preprocess board
              (lambda (height width big-list)
                      (list-to-hash
                        (for/list ([index (in-naturals)]
                                   [letter big-list])
                                  (let ([row (quotient index width)]
                                        [column (remainder index width)])
                                    (list (position row column) false)))))))

;; Return a list of positions
(define (build-positions board)
  (preprocess board
              (lambda (height width big-list)
                      (for/list ([index (in-naturals)]
                                 [letter big-list])
                                (let ([row (quotient index width)]
                                      [column (remainder index width)])
                                  (position row column))))))

;; Return a word from a list of symbols
(define (list-of-symbols-to-word symbols)
  (foldl
    (lambda (sym word)
            (string-append word (symbol->string sym)))
    ""
    symbols))

;; Determine if a given string is a word or not using a dictionary
(define (is-a-word? str dictionary)
  (set-member? dictionary str))

;; Find words from a single position
(define (find-words position lst visited position-to-neighbors position-to-letter dictionary)
  (foldl
    (lambda (neighbor all-words)
            (if (hash-ref visited neighbor)
              all-words
              (let* ([neighbor-letter (hash-ref position-to-letter neighbor)]
                     [new-lst (append lst (list neighbor-letter))]
                     [word (list-of-symbols-to-word new-lst)]
                     [new-visited (hash-set visited neighbor true)]
                     [rest-words (find-words neighbor new-lst new-visited position-to-neighbors position-to-letter dictionary)])
                (if (is-a-word? word dictionary)
                  (set-union (set-add all-words word) rest-words)
                  (set-union all-words rest-words)))))
    (set)
    (hash-ref position-to-neighbors position)))

;; Build a dictionary set from a dictionary file
(define (build-dictionary filename)
  (foldl
    (lambda (sym a-set)
            (set-add a-set (string-upcase (symbol->string sym))))
    (set)
    (file->list filename)))

;; Find all possible words of a boggle board
(define (find-all-words board)
  (let ([position-to-letter (build-position-to-letter board)]
        [position-to-neighbors (build-position-to-neighbor board)]
        [visited (build-visited board)]
        [positions (build-positions board)]
        [dictionary (build-dictionary  "/usr/share/dict/web2")])
    (map
      (lambda (position)
              (find-words position
                          (list (hash-ref position-to-letter position))
                          (hash-set visited position true)
                          position-to-neighbors
                          position-to-letter
                          dictionary))
      positions)
    ))

(define board
  (list
    '(C O)
    '(A R)))

;(define board
;  (list
;    '(C O D A)
;    '(A R N P)
;    '(O S E L)
;    '(I R U M)))


;(define board
;  (list
;    '(O A A N)
;    '(E T R I)
;    '(I H K R)
;    '(I F L V)))

(find-all-words board)
