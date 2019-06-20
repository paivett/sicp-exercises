;It is not clear if a and b are ordered, or could be specified in any order, thats 
;why we need the min and max for the lower-bound and upper-bound definitions
(define (make-interval a b) (cons a b))

(define (lower-bound i) (min (car i) (cdr i)))

(define (upper-bound i) (max (car i) (cdr i)))

;-------------------------------------------------------

(define (width i) (/ (- (upper-bound i) (lower-bound i)) 2.0))

;-------------------------------------------------------

; Let there be two intervals a = (la, ua) and b = (lb, ub), and be s
; the sum of a and b then, by the sum definition we have

; s = (ls, us), where ls = la + lb, and us = (ua + ub)

; Now, let's compute the width of s 

; w = (us - ls) / 2
; w = (ua + ub - la - lb) / 2
; w = (ua - la + ub - lb) / 2
; w = (ua - la) / 2 + (ub - lb) / 2
; w = wa + wb

; So, the width of the sum is the sum of the widths

; For the substraction we can do the same reasoning. Let s be a - b

; w = (us - ls) / 2
; w = ((ua - lb) - (la - ub)) / 2
; w = (ua - lb - la + ub) / 2
; w = (ua - la) / 2 + (ub - lb) / 2
; w = wa + wb

; So for both sum and sub the width is the sum of widths

;----------------------------------------------------------------------

; Now, for multiplication, let a = (0, 1) and b = (1, 2), both intervals with a width of 0.5

; Let m be the multiplication. Applying the multiplication definition, m = (0, 2), with a width of 1.

; Now let's change b = (2, 3). It still has a width of 0.5, but now m = a * b = (0, 3), with a width of 1.5.

; We can conclude that the width of multiplication is not a function only of the widths because widths never changed.
