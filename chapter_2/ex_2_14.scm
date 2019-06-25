;It is not clear if a and b are ordered, or could be specified in any order, thats 
;why we need the min and max for the lower-bound and upper-bound definitions
(define (make-interval a b) (cons a b))

(define (lower-bound i) (min (car i) (cdr i)))

(define (upper-bound i) (max (car i) (cdr i)))

;-------------------------------------------------------

(define (make-center-percent c p)
  (let ((delta (* p (/ c 100.0))))
    (make-interval (- c delta) (+ c delta))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (/ (* 100.0 (width i)) (center i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (spans-zero? i) (and (<= (lower-bound i) 0) 
                             (>= (upper-bound i) 0)))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "Interval y spans zero")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (print-interval i)
  (newline)
  (display "(")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display ")"))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define A (make-center-percent 5 0.1))
(define B (make-center-percent 10 0.2))

(print-interval (par1 A B))
(print-interval (par2 A B))

(print-interval (div-interval A A))

; Lem is right, the algebra for real numbers does not apply when dealing with these intervals.
;For each operation the uncertainty changes, so the more operations are done, the more the uncertainty changes