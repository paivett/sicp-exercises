; f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) 

; (define (fr n)
;     (cond ((< n 3) n)
;           (else (+ (fr (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))
;     )
; )

(define (f n)
    (define (f-iter a b c n)
        (cond ((= n 0) c)
              (else (f-iter b c (+ c (* 2 b) (* 3 a)) (- n 1)))
        )
    )
    (cond ((< n 3) n)
          (else (f-iter 0 1 2 (- n 2)))
    )
)