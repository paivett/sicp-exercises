(define nil (list))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (neq a b) (not (= a b)))

; positions is a list if pairs, each pair is a position in the board (row, col)
(define (safe? k positions)
  (if (> (length positions) 1)
      (let ((safe-pos (car (filter (lambda (pos) (= k (cdr pos))) positions)))
            (other-positions (filter (lambda (pos) (neq k (cdr pos))) positions)))
           (and (safe-row? safe-pos other-positions) (safe-diag? safe-pos other-positions)))
      #t))

(define (safe-row? safe-pos positions)
  (= (length (filter (lambda (pos) (= (car pos) (car safe-pos))) positions)) 0)
)

(define (safe-diag? safe-pos positions)
  (= (length (filter (lambda (pos) (= (abs (- (car safe-pos) (car pos))) (abs (- (cdr safe-pos) (cdr pos))))) positions)) 0)
)

(define (adjoin-position new-row k rest-of-queens) (cons (cons new-row k) rest-of-queens))

(define empty-board (list))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))