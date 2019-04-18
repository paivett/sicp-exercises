(define (search-for-primes n m)
    (define (even? n)
        (= (remainder n 2) 0))

    (define (smallest-divisor n)
        (find-divisor n 2))
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
                ((divides? test-divisor n) test-divisor)
                (else (find-divisor n (+ test-divisor 1)))))
    (define (divides? a b)
        (= (remainder b a) 0))

    (define (prime? n)
        (= n (smallest-divisor n)))

    (define (timed-prime-test n)
        (newline)
        (display n)
        (start-prime-test n (runtime)))
    (define (start-prime-test n start-time)
        (if (prime? n)
            (report-prime (- (runtime) start-time))))
    (define (report-prime elapsed-time)
        (display " *** ")
        (display elapsed-time))

    (define (search-n-continue n m)
        (timed-prime-test n)
        (search-for-primes (+ n 2) m)
    )

    (if (even? n)
        (search-for-primes (+ n 1) m)
        (if (<= n m)
            (search-n-continue n m)
        )
    )
)

(search-for-primes 1000 10000)
;1009, 1013 and 1019

(search-for-primes 10000 100000)
;10007, 10009 and 10037

(search-for-primes 100000 100050)
; 100003, 100019, 100043

(search-for-primes 1000000 1000037)
; 1000003, 1000033 and 1000037