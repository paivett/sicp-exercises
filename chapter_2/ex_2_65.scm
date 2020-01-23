(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;------------------------------------------------------------

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;------------------------------------------------------------

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;------------------------------------------------------------


(define (intersection-set tree1 tree2)
  (define (intersection set1 set2)
    (if (or (null? set1) (null? set2))
        '()    
        (let ((x1 (car set1)) (x2 (car set2)))
             (cond ((= x1 x2) (cons x1 (intersection (cdr set1) (cdr set2))))
                   ((< x1 x2)
                    (intersection (cdr set1) set2))
                   ((< x2 x1)
                    (intersection set1 (cdr set2)))))))

  (list->tree (intersection (tree->list tree1) (tree->list tree2))))


(define (union-set tree1 tree2)
  (define (merge s1 s2 output)
    (if (or (null? s1) (null? s2))
        (append output s1 s2)
        (let ((x1 (car s1)) (x2 (car s2)))
          (cond ((= x1 x2) (merge (cdr s1) (cdr s2) (append output (list x1))))
                ((< x1 x2) (merge (cdr s1) s2 (append output (list x1))))
                ((< x2 x1) (merge s1 (cdr s2) (append output (list x2))))))))

  (list->tree (merge (tree->list tree1) (tree->list tree2) '())))

;------------------------------------------------------------

(define t0 '())

(define t1 (make-tree 7 
                      (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                      (make-tree 9 '() (make-tree 11 '() '()))))

(define t2 (make-tree 5 
                      (make-tree 2 (make-tree 1 '() '()) '())
                      (make-tree 10 (make-tree 6 '() '()) (make-tree 11 '() '()))))
