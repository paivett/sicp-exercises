(define (prime? n)
    (define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp) (check-root (expmod base (/ exp 2) m) m))
              (else (remainder (* base (expmod base (- exp 1) m)) m))
        )
    )

    (define (check-root r m)
        (if (and (not (or (= r 1) (= r (- m 1)))) (= (remainder (square r) m) 1))
            0
            (remainder (square r) m)
        )
    )

    (define (fermat-test n)
        (define (try-it a)
            (= (expmod a (- n 1) n) 1))
        (try-it (+ 1 (random (- n 1)))))

    (define (fast-prime? n times)
        (cond ((= times 0) true)
              ((fermat-test n) (fast-prime? n (- times 1)))
              (else false)))

    (fast-prime? n 100)
)

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
