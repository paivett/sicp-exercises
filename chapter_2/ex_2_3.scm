(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

;---------------------------------------------------------------------------

(define (make-rect bottom-left top-right) (cons bottom-left top-right))

(define (rect-bottom-left r) (car r))

(define (rect-top-right r) (cdr r))

; Another possible implementation of this could be

; (define (make-rect bottom-left width length) (cons bottom-left (cons width length)))

; (define (rect-bottom-left r) (car r))

; (define (rect-top-right r) 
;   (let ((x1 (x-point (rect-bottom-left r)))
;         (y1 (y-point (rect-bottom-left r)))
;         (w (car (cdr r)))
;         (l (cdr (cdr r))))
;    (make-point (+ x1 w) (+ y1 l))))

;---------------------------------------------------------------------------

(define (rect-length r) 
   (let ((x1 (x-point (rect-bottom-left r)))
         (x2 (x-point (rect-top-right r))))
    (abs (- x2 x1))))

(define (rect-width r)
   (let ((y1 (y-point (rect-bottom-left r)))
         (y2 (y-point (rect-top-right r))))
    (abs (- y2 y1))))

;---------------------------------------------------------------------------

(define (rect-perimeter r) (* 2 (+ (rect-width r) (rect-length r))))

(define (rect-area r) (* (rect-width r) (rect-length r)))

(newline)
(display (rect-perimeter (make-rect (make-point 0 0) (make-point 2 3))))
; (display (rect-perimeter (make-rect (make-point 0 0) 2 3)))

(newline)
(display (rect-area (make-rect (make-point 0 0) (make-point 2 3))))
; (display (rect-area (make-rect (make-point 0 0) 2 3)))
