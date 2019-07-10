(define nil (list))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; (1 (4 (9 16) 25) (36 49))

(square-tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; (1 (4 (9 16) 25) (36 49))