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


(define songs-freq '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define songs-tree (generate-huffman-tree songs-freq))

(encode '(Get a job) songs-tree)
; (1 1 1 1 1 1 1 0 0 1 1 1 1 0)

(encode '(Sha na na na na na na na na) songs-tree)
; (1 1 1 0 0 0 0 0 0 0 0 0)

(encode '(Get a job) songs-tree)
; (1 1 1 1 1 1 1 0 0 1 1 1 1 0)

(encode '(Sha na na na na na na na na) songs-tree)
; (1 1 1 0 0 0 0 0 0 0 0 0)

(encode '(Wah yip yip yip yip yip yip yip yip yip) songs-tree)
; (1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)

(encode '(Sha boom) songs-tree)
; (1 1 1 0 1 1 0 1 1)

; The total number of bits needed is 84
; Using a fixed-length encoding would take 3 bits per symbol that would be a total length of 108 bits
