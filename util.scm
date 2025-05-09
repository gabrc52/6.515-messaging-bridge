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

;; Makes a predicate to check if any object belongs to the given platform
(define (platform-predicate target-platform-id)
  ;; And generic procedure was more code/didn't make sense for something so simple that doesn't need flexiblity.
  (lambda (obj)
    (platform-ids-equal? target-platform-id
			 ;; I guess this reveals that getting the platform *could* be a generic procedure
			 (cond ((platform-id? obj) obj)
			       ((message-accepting-procedure? obj) (obj 'get-platform-id))
			       ((identifier? obj) (identifier-platform obj))
			       ((event? obj) (event-platform obj))))))

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

(define (get-time-ms)
    (let ((ticks (real-time-clock)))
        (internal-time/ticks->seconds ticks)))

