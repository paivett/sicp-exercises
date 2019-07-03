(define (filter l func)
    (if (null? l)
        l
        (if (func (car l))
            (cons (car l) (filter (cdr l) func))
            (filter (cdr l) func))))

(define (same-parity . nums)
    (filter nums (if (even? (car nums)) even? odd?)))

; (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)

; (same-parity 2 3 4 5 6 7)
; (2 4 6)