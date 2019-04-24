(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
    (define (iter-repeated i result)
        (if (= i 0)
            result
            (iter-repeated (- i 1) (compose f f))))
    (lambda (x) ((iter-repeated n f) x)))


(define dx 0.00001)
(define (smooth f)
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (n-fold f n) ((repeated smooth n) f))
