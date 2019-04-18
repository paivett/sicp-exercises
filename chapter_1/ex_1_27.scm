(define (prime? n)
    (define (expmod base exp m)
        (cond ((= exp 0) 1)
                ((even? exp)
                (remainder (square (expmod base (/ exp 2) m))
                            m))
                (else
                (remainder (* base (expmod base (- exp 1) m))
                            m))))    

    (define (fermat-test n a)
        (= (expmod a n n) a))

    (define (fermat-test-all n a)
        (cond ((= a 0) true)
            ((fermat-test n a) (fermat-test-all n (- a 1)))
            (else false)))

    (fermat-test-all n (- n 1))
)

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
