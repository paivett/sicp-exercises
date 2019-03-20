(define (sqr x) (* x x))

(define (foo a b c) (cond ((and (< a b) (< a c)) (+ (sqr b) (sqr c)))
                          ((and (< b a) (< b c)) (+ (sqr a) (sqr c)))
                          (else (+ (sqr a) (sqr b)))
))