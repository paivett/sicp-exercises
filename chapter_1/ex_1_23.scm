(define (search-for-primes n m)
    (define (even? n)
        (= (remainder n 2) 0))

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

(search-for-primes 100000000000000 100000000000031)

; On my machine, testing for 100000000000031 goes from roughly 13 secs to 8.5 secs.
;This is not 2x boost. The fact that we need to make a func call and an additional if may account for that.