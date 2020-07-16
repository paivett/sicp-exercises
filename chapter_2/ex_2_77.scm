; First, we assume that magnitud was defined as follows at the top level
(define (magnitude z) apply-generic 'magnitude z)
; And that z is defined as
(define z (list 'complex 'rectangular 3 4))
; Then, when (magnitude z) is invoked, a first call to apply generic is done
; This first call finds that there is a registerd function for the 'complex tag and calls that function
; Now there is a call as follows

(magnitude (list 'rectangular 3 4))

; Again this call invokes apply-generic, which finds that there is a function for 'rectangular type
; Finally the following call is done
(magnitude (cons 3 4))
; But that magnitude is the one defined inside the rectangular-complex package