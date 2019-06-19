;It is not clear if a and b are ordered, or could be specified in any order, thats 
;why we need the min and max for the lower-bound and upper-bound definitions
(define (make-interval a b) (cons a b))

(define (lower-bound i) (min (car i) (cdr i)))

(define (upper-bound i) (max (car i) (cdr i)))

;-------------------------------------------------------

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
