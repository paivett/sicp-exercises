(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))

(define (iter-accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a)))))
    (iter a null-value))

(define (sum term a next b)
    (accumulate + 0 term a next b))

(define (product term a next b)
    (accumulate * 1 term a next b))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial n)
    (product identity 1 inc n))

(define (sum-ints n)
    (sum identity 0 inc n))
