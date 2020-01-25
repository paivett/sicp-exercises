(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (choose-bit symbol branch)
  (cond ((memq symbol (symbols (left-branch branch))) '0)
        ((memq symbol (symbols (right-branch branch))) '1)
        (else (error "bad symbol -- CHOOSE-BIT" symbol))))

(define (encode-symbol symbol tree)
  (define (emit-bits symbol current-branch bits)
    (if (leaf? current-branch)
        bits
        (let ((next-bit (choose-bit symbol current-branch)))
          (emit-bits symbol 
                     (if (eq? next-bit '0) (left-branch current-branch) (right-branch current-branch))
                     (append bits (list next-bit))))))
  
  (emit-bits symbol tree '()))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge branches)
  (let ((b (car branches))
        (bs (cdr branches)))
    (if (null? (cdr branches))
        b
        (successive-merge (adjoin-set (make-code-tree b (car bs)) (cdr bs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define freq-sample '((A 4) (B 2) (C 1) (D 1)))

(define sample-tree (generate-huffman-tree freq-sample))
