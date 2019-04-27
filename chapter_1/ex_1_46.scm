(define (iterative-improve guess-good-enough? improve-guess)
    (define (iter guess)
        (if (guess-good-enough? guess)
            guess
            (iter (improve-guess guess))))
    (lambda (x) (iter x)))

(define (average x y)
    (/ (+ x y) 2.0))

(define (sqrt x)
    ((iterative-improve (lambda (guess) 
                             (< (abs (- (square guess) x)) 0.00001))
                        (lambda (guess) (average guess (/ x guess))))
                        1.0))

(define (fixed-point f initial_guess)
    ((iterative-improve (lambda (guess) 
                             (< (abs (- (f guess) guess)) 0.00001))
                        f) initial_guess))