(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-integral f a b n)
    (define h (/ (- b a) n))
    
    (define (yk-coef k)
        (cond ((or (= k 0) (= k n)) 1)
              ((even? k) 2)
              (else 4))
    )

    (define (yk k)
        (+ a (* k h)))
    
    (define (aux k)
        (* (yk-coef k) (f (yk k))))

    (define (inc x) (+ x 1))

    (* (/ h 3) (sum aux 0.0 inc n))
)

(define (cube x) (* x x x))

; (simpson-integral cube 0 1 100) = .24999999999999992
; (simpson-integral cube 0 1 1000) = .2500000000000003

; Simpsons rule seems to be more accurate that the original method