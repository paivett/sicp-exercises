(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
    (define (iter-repeated i result)
        (if (= i 1)
            result
            (iter-repeated (- i 1) (compose result f))))
    (lambda (x) ((iter-repeated n f) x)))

((repeated square 2) 5)
; 625

((repeated square 3) 5)
; 390625