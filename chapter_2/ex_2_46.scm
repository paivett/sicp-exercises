(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

;---------------------------------------

(define (add-vect u v)
  (make-vect (+ (xcor-vect u) (xcor-vect v))
             (+ (ycor-vect u) (ycor-vect v))))

(define (sub-vect u v)
  (make-vect (- (xcor-vect u) (xcor-vect v))
             (- (ycor-vect u) (ycor-vect v))))

(define (scale-vect a v)
  (make-vect (* a (xcor-vect v))
             (* a (ycor-vect v))))

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))