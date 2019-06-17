; This solution might have numerical issues with large numbers
; Another solution would be to count how many times the number
; is divisible by 2 or 3

(define logB 
    (lambda (x B) 
      (/ (log x) (log B))))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car p)
  (if (> (remainder p 3) 0) (round (logB p 2)) (car (/ p 3))))

(define (cdr p)
  (if (> (remainder p 2) 0) (round (logB p 3)) (cdr (/ p 2))))
