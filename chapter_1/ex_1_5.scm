(define (p) (p))

(define (test x y)
    (if (= x 0)
        0
        y))

; (test 0 (p))
; With the normal order the expression will be expanded to
; (if (= 0 0) 0 (p))
; (if #t 0 (p))
; 0
; So, it finishes
; But with applicative order, the arguments are expanded before appliying so
; (test 0 (p)) will try to expand (p), so we get
; (test 0 (p)), and the process repeats since p is defined as itself