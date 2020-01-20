(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

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

(define l (list 1 3 5 7 9 11))

; The auxiliary function partial-tree takes a list of elements, and an integer N expressing
; that the tree should be built using the N first elements of the list.
; The output is a pair where the left element is the produced tree, and the right element is
; the remainder elements of the original tree. The tree is built such as the inorder traversal
; is the original list.
; The tree is built recursively as follows:
; 1. Build a 'left-tree' with the first (N-1)/2 of the original list
; 2. Of the remaining elements of the original list, take the first, and call it 'this-entry'
; 3. Build a 'right-tree' with the remaining elements (after step 1 and 2).
; 4. Return a pair where the first element is the make-tree with root 'this-entry' and both 'left-tree' and 'right-tree' as subtrees
;    and the second element are the possible remaining elements of the original list.


;The output is this tree
;
;     5
;    / \
;   /   \
;  /     \
; 1       9
;  \     / \
;   3   7  11

; The number of steps is as follows
; T(n) = 2T(n/2) + O(1)
; This is O(n)
