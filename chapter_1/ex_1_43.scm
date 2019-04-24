(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
    (define (iter-repeated i result)
        (if (= i 0)
            result
            (iter-repeated (- i 1) (compose f f))))
    (lambda (x) ((iter-repeated n f) x)))

((repeated square 2) 5)
; 625