(define (even? n)
  (= (remainder n 2) 0))

(define (iter-expt b n a)
    (cond ((= n 1) (* a b))
          ((even? n) (iter-expt (* b b) (/ n 2) a))
          (else (iter-expt b (- n 1) b))
    )
)

(define (expt b n) (iter-expt b n 1))