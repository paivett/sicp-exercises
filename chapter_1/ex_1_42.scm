(define (compose f g) (lambda (x) (f (g x))))

(define (inc x) (+ 1 x))

((compose square inc) 6)
; returns 49