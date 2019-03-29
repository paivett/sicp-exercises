(define (cube-root-iter guess prev_guess x)
    (if (good-enough? guess prev_guess)
        guess
        (cube-root-iter (improve guess x) guess x)
    )
)

(define (good-enough? guess prev_guess)
    (< (/ (abs (- guess prev_guess)) prev_guess) 0.00001)
)

(define (improve guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)
)

(define (average x y)
    (/ (+ x y) 2)
)

(define (cube-root x)
    (cube-root-iter 1.0 0.0 x)
)
