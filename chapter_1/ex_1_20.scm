(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Normal-order

(gcd 206 40)
; 
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))
; 
(gcd 40 (remainder 206 40))
; 
(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
;
;  (remainder 206 40) is computed
;
(if (= 6) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;
(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;
; (remainder 40 (remainder 206 40)) is computed, reminder is called 2 times
;
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
;
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) is computed, remainder is called 4 times 
;
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    ...)
;
; (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) is computed, remainder is called 7 times
;
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;
; remainder is now called 4 times
;
; remainder is called, overall, 18 times
;
; In the applicative order, remainder is called only 4 times
;