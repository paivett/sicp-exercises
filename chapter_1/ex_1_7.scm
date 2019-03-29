(define (sqrt-iter guess prev_guess x)
    (if (good-enough? guess prev_guess)
        guess
        (sqrt-iter (improve guess x) guess x)
    )
)

(define (square x)
    (* x x)
)

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001)
)
; The problem is that representing small differences of big numbers works wrong

; (define (good-enough? guess prev_guess)
;     (< (/ (abs (- guess prev_guess)) prev_guess) 0.001)
; )

(define (improve guess x)
    (average guess (/ x guess))
)

(define (average x y)
    (/ (+ x y) 2)
)

(define (new-sqrt x)
    (sqrt-iter 1.0 0.0 x)
)
