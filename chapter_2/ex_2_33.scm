(define nil (list))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


(define l1 (list 1 2 3 4))
(define l2 (list 5 6 7 8))