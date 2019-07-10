(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;-------------------------------------------------


(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (car (cdr branch)))

;-------------------------------------------------

(define (branch-weight branch)
    (let ((struct (branch-structure branch)))
         (if (pair? struct)
             (total-weight struct)
             struct)))

(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

;-------------------------------------------------

(define (balanced mobile) 
    (define (branch-torque branch) (* (branch-length branch) (branch-weight branch)))
    
    (define (balanced-branch branch) 
        (let ((struct (branch-structure branch)))
         (if (pair? struct)
             (balanced struct)
             #t)))

    (let ((lbranch (left-branch mobile))
          (rbranch (right-branch mobile)))
         (and (= (branch-torque lbranch) (branch-torque rbranch))
              (balanced-branch lbranch)
              (balanced-branch rbranch))))

;-------------------------------------------------

; Not balanced mobile
(define m1 (make-mobile 
                (make-branch 1 
                    (make-mobile
                        (make-branch 3 4)
                        (make-branch 3 1)
                    )
                )
                (make-branch 2 3)
))

; Balanced mobile
(define m2 (make-mobile 
                (make-branch 1 
                    (make-mobile (make-branch 3 4) (make-branch 4 3))
                )
                (make-branch 7 1)
))

; If the internal representation of mobile and branches were to change, then the only thing
; that we should change are the selectors left-branch, right-branch, branch-length and branch-structure
; The rest of the functions would not be affected, but there are some uses of pair? function and that
; still works because how Scheme represents internally the lists, but if that changed, then we would
; also have to make some changes there