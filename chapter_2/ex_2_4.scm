(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; When applying (car (cons 1 2)) with the above definition we obtain the following expansion

; (car (cons 1 2))
; (car (lambda (m) (m 1 2)))
; ((lambda (m) (m 1 2)) (lambda (p q) p))
; (((lambda (p q) p) 1 2))
; 2

; A possible definition for cdr would be
(define (cdr z)
  (z (lambda (p q) q)))
