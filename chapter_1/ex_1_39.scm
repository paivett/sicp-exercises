(define (iter-cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (/ (n i) (+ (d i) result)))))
    (iter k 0))


(define (tan-cf x k) 
    (iter-cont-frac (lambda (i) (if (= i 1)
                                    x
                                    (- (square x))))
                    (lambda (i) (- (* 2.0 i) 1.0))
                    k))
