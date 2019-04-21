(define (prime? n)
    (define (next d)
        (if (= d 2)
            3
            (+ d 2)))
    
    (define (smallest-divisor n)
        (find-divisor n 2))
    
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
                ((divides? test-divisor n) test-divisor)
                (else (find-divisor n (next test-divisor)))))
    
    (define (divides? a b)
        (= (remainder b a) 0))

    (= n (smallest-divisor n)))


(define (filtered-accumulate combiner filter null-value term a next b)
    (define (filter-term k)
        (if (filter k)
            (term k)
            null-value))

    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (filter-term a)))))

    (iter a null-value))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (sum-squared-primes n)
    (filtered-accumulate + prime? 0 square 1 inc n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prod n)
    (define (filter x)
        (= (gcd x n) 1))
    (filtered-accumulate * filter 1 identity 1 inc (- n 1)))
