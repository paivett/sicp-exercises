(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (< (* n d) 0) -1 1)))
    (cons (* sign (abs (/ n g))) (abs (/ d g)))
  )
)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


(print-rat (make-rat 1 2))
(print-rat (make-rat -1 -2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
