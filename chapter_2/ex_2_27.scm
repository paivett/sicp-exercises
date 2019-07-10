(define (deep-reverse l)
    (if (null? l)
        l
        (let ((head (car l))
              (tail (cdr l)))
             (if (pair? head)
                 (append (deep-reverse tail) (list (deep-reverse head)))
                 (append (deep-reverse tail) (list head))))))

(define a (list 1 2 3 4))
(define b (list (list 1 2) (list 3 4)))

(deep-reverse a)
(deep-reverse b)
