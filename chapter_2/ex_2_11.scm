;It is not clear if a and b are ordered, or could be specified in any order, thats 
;why we need the min and max for the lower-bound and upper-bound definitions
(define (make-interval a b) (cons a b))

(define (lower-bound i) (min (car i) (cdr i)))

(define (upper-bound i) (max (car i) (cdr i)))

;-------------------------------------------------------

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;-------------------------------------------------------

(define (new-mul-interval x y) 
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
        (let ((xneg? (and (< lx 0) (< ux 0)))
              (xpos? (and (> lx 0) (> ux 0)))
              (yneg? (and (< ly 0) (< uy 0)))
              (ypos? (and (> ly 0) (> uy 0)))
              (xspans? (and (<= lx 0) (>= ux 0)))
              (yspans? (and (<= ly 0) (>= uy 0))))

              (cond ((and xneg? yneg?) (make-interval (* ux uy) (* lx ly)))
                    ((and xneg? ypos?) (make-interval (* lx uy) (* ux ly)))
                    ((and xneg? yspans?) (make-interval (* lx uy) (* lx ly)))

                    ((and xpos? ypos?) (make-interval (* lx ly) (* ux uy)))
                    ((and xpos? yneg?) (make-interval (* ux ly) (* lx uy)))
                    ((and xpos? yspans?) (make-interval (* ux ly) (* ux uy)))
                    
                    ((and xspans? ypos?) (make-interval (* lx uy) (* ux uy)))
                    ((and xspans? yneg?) (make-interval (* ux ly) (* lx ly)))
                    ((and xspans? yspans?) (make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy))))))))

;--------------------------------------------------------


(define (print-interval i)
  (newline)
  (display "(")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display ")"))

(define x1 (make-interval -5 -2))
(define x2 (make-interval -5  2))
(define x3 (make-interval  5  2))

(define y1 (make-interval -3 -1))
(define y2 (make-interval -3  1))
(define y3 (make-interval  3  1))

(print-interval (mul-interval x1 y1))
(print-interval (new-mul-interval x1 y1))
(newline)
(print-interval (mul-interval x1 y2))
(print-interval (new-mul-interval x1 y2))
(newline)
(print-interval (mul-interval x1 y3))
(print-interval (new-mul-interval x1 y3))
(newline)
(print-interval (mul-interval x2 y1))
(print-interval (new-mul-interval x2 y1))
(newline)
(print-interval (mul-interval x2 y2))
(print-interval (new-mul-interval x2 y2))
(newline)
(print-interval (mul-interval x2 y3))
(print-interval (new-mul-interval x2 y3))
(newline)
(print-interval (mul-interval x3 y1))
(print-interval (new-mul-interval x3 y1))
(newline)
(print-interval (mul-interval x3 y2))
(print-interval (new-mul-interval x3 y2))
(newline)
(print-interval (mul-interval x3 y3))
(print-interval (new-mul-interval x3 y3))
(newline)