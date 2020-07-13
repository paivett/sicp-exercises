(define (install-division-A)
  ;; internal procedures
  (define records-file (
      ; some definition of how records are stored
  ))
  (define (get-record records employee-name) ( 
    ; some implementation    
  ))
  (define (get-salary record) ( 
    ; some implementation    
  ))
  
  ;; interface to the rest of the system
  (define (tag-file x) (attach-tag 'divA-file x))
  (define (tag-record x) (attach-tag 'divA-record x))

  (put 'get-record 'divA-file (lambda (records ename) (tag-record (get-record records ename))))
  (put 'records 'divA-file (tag-file records-file))
  (put 'get-salary 'divA-record get-salary)
  'done)

; This apply-generic looks for the type of the first argument and dispatches accordingly
(define (apply-generic op obj . args)
  (let ((obj-type (type-tag obj)))
    (let ((proc (get op obj-type)))
      (if proc
          (apply proc (cons obj args))
          (error
            "No method for this object type -- APPLY-GENERIC"
            (list op obj-type))))))

; This is the general interface
(define (get-record file employee-name) (apply-generic 'get-record file employee-name))
(define (get-salary record) (apply-generic 'get-salary record))


(define divA-file (get 'records 'divA-file ))