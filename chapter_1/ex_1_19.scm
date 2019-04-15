(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; a_1 = (b_0 * q) + (a_0 * q) + (a_0 * p)
; b_1 = (b_0 * p) + (a_0 * q)
;
; so, let's compute b_2
;
; b_2 = (b_1 * p) + (a_1 * q)
;
; Replacing the definitions for a_1 and b_1 we get
;
; b_2 = ((b_0 * p) + (a_0 * q)) * p + (b_0 * q + a_0 * q + a_0 * p) * q
; 
;Doing some algebra gets us to
; 
; b_2 = b_0 * (p^2 + q^q) + a_0 * (2*p*q + q^2)
;
; And there we have the definition of b_2 in terms of b_0 and a_0 with p' = (p^2 + q^q) and q' = (2*p*q + q^2)