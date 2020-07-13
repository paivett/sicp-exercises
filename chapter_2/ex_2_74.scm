(define (install-division-A)
  ;; internal procedures
  (define records-file (
      ; some definition of how records are stored
  ))
  (define (get-record records employee-name) ( 
    ; some implementation that returns false if the record is not found
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

(define (find-employee-record files name) 
    (if (null? files)
        false
        ((let ((employee-record (get-record (car files) name)))
              (if (not employee-record)
                  (find-employee-record (cdr files) name)
                   employee-record)))))

; When the company acquires a another company, all new divisions will have to implement their own
; install-division-?? implementing the get-record, records and get-salary.
; The only thing that must be coordinated is that they all use a different type-tag.