(define (double n) (* n 2))

(define (halve n) (/ n 2))

(define (iter-mult a b c)
    (cond ((= b 0) c)
          ((even? b) (iter-mult (double a) (halve b) c))
          (else (iter-mult a (- b 1) (+ a c)))
    )
)

(define (mult a b) (iter-mult a b 0))