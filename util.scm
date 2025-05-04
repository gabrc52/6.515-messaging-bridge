;;; Predicates

(define (of-exact-arity? f x)
  (let ((arity (procedure-arity f)))
    (and (eqv? (procedure-arity-min arity) x)
	 (eqv? (procedure-arity-max arity) x))))

(define (message-accepting-procedure? f)
  (and (procedure? f)
       (of-exact-arity? f 1)))

(define platform-id? symbol?)
(define platform-ids-equal? eqv?)

;; Reimplementing platform-is? like this to see if it fixes the bug
(define (platform-predicate target-platform-id)
  (let ((procedure
	 (most-specific-generic-procedure
	  (symbol target-platform-id '-predicate)
	  1
	  #f)))
    
    (define-generic-procedure-handler procedure
      (match-args platform-id?)
      (lambda (id) (platform-ids-equal? id target-platform-id)))
    
    (define-generic-procedure-handler procedure
      (match-args message-accepting-procedure?)
      (lambda (f) (platform-ids-equal? (f 'get-platform-id) target-platform-id)))

    (define-generic-procedure-handler procedure
      (match-args identifier?)
      (lambda (identifier) (platform-ids-equal? (identifier-platform identifier) target-platform-id)))

    procedure))

(define (platform-id-predicate target-platform)
  (lambda (x) (and (platform-id? x) (eqv? target-platform x))))

(define ((list-beginning-with? symbol) config)
  (and (list? config)
       (not (null? config))
       (eqv? (car config) symbol)))

;;; Iteration

;; Maybe these functions already exist, but I don't know what they would be called.

(define (for-all-pairs list f)
  (for-each (lambda (item1)
	      (for-each (lambda (item2)
			  (f item1 item2)) list)) list))
(define (for-all-different-pairs list f)
  (for-all-pairs list
		 (lambda (item1 item2)
		   (unless (eqv? item1 item2)
		     (f item1 item2)))))


