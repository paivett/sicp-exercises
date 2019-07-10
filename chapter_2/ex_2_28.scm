(define (fringe l)
    (if (null? l)
        l
        (let ((head (car l))
              (tail (cdr l)))
             (if (pair? head)
                 (append (fringe head) (fringe tail))
                 (append (list head) (fringe tail))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(fringe (list x x))
