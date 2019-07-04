; (square-list (list 1 2 3 4))
; (1 4 9 16)

(define (square-list items)
  (if (null? items)
      (list)
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))