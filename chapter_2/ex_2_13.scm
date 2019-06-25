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


; If we assume possitive intervals, then the multiplication of two intervals can be expressed as

; (define (new-mul-interval x y) 
;   (let ((lx (lower-bound x))
;         (ux (upper-bound x))
;         (ly (lower-bound y))
;         (uy (upper-bound y)))
;     ((make-interval (* lx ly) (* ux uy)))))

; Given two intervals i,j with percentages a,b respectively, we can write both intervals as

; i = (ci - delta_a, ci + delta_a)
; j = (cj - delta_b, cj + delta_b)

; Where ci, cj are the centers of i, j and delta_a = (a * (ci / 100)), delta_b = (b * (cj /100))

; Now, if we write i * j

; i * j = ((ci - delta_a) * (cj - delta_b), (ci + delta_a) * (cj + delta_b))
;       = (ci * cj - ci * delta_b - cj * delta_a + delta_a * delta_b, ci * cj + ci * delta_b + cj * delta_a + delta_a * delta_b)

; We can consider delta_a * delta_b being super small so we can omit it, then (i * j) is approximated by

; i * j = (ci*cj - (ci * delta_b + cj * delta_a), ci*cj + (ci * delta_b + cj * delta_a))

; Replacing by the definition of delta_a and delta_b we get

; i * j = (ci*cj - (ci * cj * (b/100) + cj * ci * (a/100)), ci*cj + (ci * cj * (b/100) + cj * ci * (a/100)))

; Now, if we look at the definition of the constructor of the percentage interval, we can assume that this is the
; interval i*j where delta is (ci * cj * (b/100) + cj * ci * (a/100)) so we can write

; (ci * cj * (b/100) + cj * ci * (a/100) = p * c / 100

; p is the percentage of i*j, and c is the center, which we know is ci*cj, then

; (ci * cj * (b/100) + cj * ci * (a/100)) = p * ci * cj / 100
; (ci * cj / 100) * (b + a) = p * (ci * cj / 100)

; b + a = p

; We conclude that we can estimate the percentage p as a + b