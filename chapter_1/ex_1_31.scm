(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b))))

(define (iter-product term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial n)
    (product identity 1 inc n))

(define (pi-approx n)
    (define (floor-to-even k) 
        (if (even? k)
            k
            (- k 1)))
    (define (ceil-to-even k) 
        (if (even? k)
            k
            (+ 1 k)))
    (define (term k)
        (/ (+ 2 (floor-to-even k)) (+ 1 (ceil-to-even k))))

    (* 4.0 (product term 1 inc n))
)