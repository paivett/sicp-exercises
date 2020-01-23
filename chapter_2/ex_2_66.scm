(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;----------------------------------------------------

(define (make-record key value) (list key value))
(define (key record) (car record))
(define (value record) (cdr record))

;----------------------------------------------------

(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) false)
        ((equal? given-key (key (entry tree-of-records)))
         (value (entry tree-of-records)))
        ((< given-key (key (entry tree-of-records)))
         (lookup given-key (left-branch tree-of-records)))
        (else (lookup given-key (right-branch tree-of-records)))))

;----------------------------------------------------


; Some records

(define r1 (make-record 1 "uno"))
(define r3 (make-record 3 "tres"))
(define r5 (make-record 5 "cinco"))
(define r7 (make-record 7 "siete"))
(define r9 (make-record 9 "nueve"))
(define r11 (make-record 11 "once"))

; Some databases

(define t0 '())

(define t1 (make-tree r7 
                      (make-tree r3 (make-tree r1 '() '()) (make-tree r5 '() '()))
                      (make-tree r9 '() (make-tree r11 '() '()))))
