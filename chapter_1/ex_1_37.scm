(define (cont-frac n d k)
    (define (cont-frac-rec i)
        (if (> i k)
            0
            (/ (n i) (+ (d i) (cont-frac-rec (+ i 1))))))
    (cont-frac-rec 1))

(define (iter-cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (/ (n i) (+ (d i) result)))))
    (iter k 0))

(iter-cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
