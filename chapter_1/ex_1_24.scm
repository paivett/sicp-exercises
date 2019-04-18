(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))    

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))


(define (search-for-primes n m)
    (define (even? n)
        (= (remainder n 2) 0))

    (define (timed-prime-test n)
        (newline)
        (display n)
        (start-prime-test n (runtime)))
    (define (start-prime-test n start-time)
        (if (fast-prime? n 100)
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

(search-for-primes 10000000000000000000000000 10000000000000000000010000)
