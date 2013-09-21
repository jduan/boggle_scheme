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

(define (preprocess board func)
  (local [(define height (length board))
          (define width (length (first board)))
          (define big-list
            (foldr append empty board))]
         (func height width big-list)))

(define (build-hash height width big-list)
  (list-to-hash
    (for/list ([index (in-naturals)]
               [letter big-list])
              (let ([row (quotient index width)]
                    [column (remainder index width)])
                (list (position row column) letter)))))

(define (build-big-hash board)
  (preprocess board build-hash))

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

(define (build-all-neighbors board)
  (preprocess board build-neighbors))

(define (build-visited board)
  (preprocess board
              (lambda (height width big-list)
                      (list-to-hash
                        (for/list ([index (in-naturals)]
                                   [letter big-list])
                                  (let ([row (quotient index width)]
                                        [column (remainder index width)])
                                    (list (position row column) false)))))))

(define (build-positions board)
  (preprocess board
              (lambda (height width big-list)
                      (for/list ([index (in-naturals)]
                                 [letter big-list])
                                (let ([row (quotient index width)]
                                      [column (remainder index width)])
                                  (position row column))))))

(define (list-of-symbols-to-word symbols)
  (foldl
    (lambda (sym word)
            (string-append word (symbol->string sym)))
    ""
    symbols))

(define (is-a-word? str dictionary)
  (set-member? dictionary str))

(define (find-words position visited position-to-neighbors position-to-letter lst dictionary)
  (foldl
    (lambda (neighbor all-words)
            (if (hash-ref visited neighbor)
              all-words
              (let* ([neighbor-letter (hash-ref position-to-letter neighbor)]
                     [new-lst (append lst (list neighbor-letter))]
                     [word (list-of-symbols-to-word new-lst)]
                     [new-visited (hash-set visited neighbor true)]
                     [rest-words (find-words neighbor new-visited position-to-neighbors position-to-letter new-lst dictionary)])
                (if (is-a-word? word dictionary)
                  (set-union (set-add all-words word) rest-words)
                  (set-union all-words rest-words)))))
    (set)
    (hash-ref position-to-neighbors position)))

(define (build-dictionary filename)
  (foldl
   (lambda (sym a-set)
     (set-add a-set (string-upcase (symbol->string sym))))
   (set)
   (file->list filename)))

(define (find-all-words board)
  (let ([position-to-letter (build-big-hash board)]
        [position-to-neighbors (build-all-neighbors board)]
        [visited (build-visited board)]
        [positions (build-positions board)]
        [dictionary (build-dictionary  "/usr/share/dict/web2")])
    (map
      (lambda (position)
              (find-words position
                          (hash-set visited position true)
                          position-to-neighbors
                          position-to-letter
                          (list (hash-ref position-to-letter position))
                          dictionary))
      positions)
    ))

(define board
  (list
    '(C O)
    '( A R)))

;(define board
;  (list
;    '(C O D A)
;    '( A R N P )
;    '( O S E L )
;    '(I R U M)))


;(define board
;  (list
;    '(O A A N)
;    '(E T R I)
;    '(I H K R)
;    '(I F L V)))

(find-all-words board)
