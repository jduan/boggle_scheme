#lang racket
(struct position (row column) #:transparent)
(struct cell (row column value ) #:transparent)
;
;(define board
;  (list
;    '(C O D A)
;    '( A R N P )
;    '( O S E L )
;    '(I R U M)))


(define board
  (list
    '(O A A N)
    '(E T R I)
    '(I H K R)
    '(I F L V)))

(define (list-to-hash list-of-lists)
  (foldl
    (lambda (lst hsh)
            (hash-set hsh (first lst) (second lst)))
    (hash)
    list-of-lists))

(define (preprocess list-of-lists func)
  (local [(define height (length list-of-lists))
          (define width (length (first list-of-lists)))
          (define big-list
            (foldr append empty list-of-lists))]
         (func height width big-list)))

(define (build-hash height width big-list)
  (list-to-hash
    (for/list ([index (in-naturals)]
               [letter big-list])
              (let ([row (quotient index width)]
                    [column (remainder index width)])
                (list (position row column) letter)))))

(define (build-big-hash list-of-lists)
  (preprocess list-of-lists build-hash))

;; a hash mapping from position to letter
(define position-to-letter
  (build-big-hash
    board))

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

(define (build-all-neighbors list-of-lists)
  (preprocess list-of-lists build-neighbors))

(define position-to-neighbors
  (build-all-neighbors
    board))

(define (build-visited list-of-lists)
  (preprocess list-of-lists
              (lambda (height width big-list)
                      (list-to-hash
                        (for/list ([index (in-naturals)]
                                   [letter big-list])
                                  (let ([row (quotient index width)]
                                        [column (remainder index width)])
                                    (list (position row column) false)))))))

(define visited
  (build-visited
    board))

(define (build-positions list-of-lists)
  (preprocess list-of-lists
              (lambda (height width big-list)
                      (for/list ([index (in-naturals)]
                                 [letter big-list])
                                (let ([row (quotient index width)]
                                      [column (remainder index width)])
                                  (position row column))))))

(define positions
  (build-positions
    board))

(define (list-of-symbols-to-word symbols)
  (foldl
    (lambda (sym word)
            (string-append word (symbol->string sym)))
    ""
    symbols))
(define dictionary
  (foldl
   (lambda (sym a-set)
     (set-add a-set (string-upcase (symbol->string sym))))
   (set)
   (file->list "/usr/share/dict/web2")))

(define (is-a-word? str dictionary)
  (set-member? dictionary str))

(define (find-words position visited position-to-neighbors position-to-letter lst dictionary)
  (for ([neighbor (hash-ref position-to-neighbors position)])
    (when (not (hash-ref visited neighbor))
      (let* ([neighbor-letter (hash-ref position-to-letter neighbor)]
             [new-lst (append lst (list neighbor-letter))]
             [word (list-of-symbols-to-word new-lst)]
             [new-visited (hash-set visited neighbor true)])
        (when (is-a-word? word dictionary)
          (print word)          )
        (find-words neighbor new-visited position-to-neighbors position-to-letter new-lst dictionary))
   )))

(find-words (position 0 0)
            (hash-set visited (position 0 0) true)
            position-to-neighbors
            position-to-letter
            '(C)
            dictionary)

