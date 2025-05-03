;;; Predicates

(define (of-exact-arity? f x)
  (let ((arity (procedure-arity f)))
    (and (eqv? (procedure-arity-min arity) x)
	 (eqv? (procedure-arity-max arity) x))))

(define (message-accepting-procedure? f)
  (and (procedure? f)
       (of-exact-arity? f 1)))

(define platform-id? symbol?)

(define platform-is?
  (most-specific-generic-procedure
   'platform-is?
   2 ;; object platform
   #f))

(define-generic-procedure-handler platform-is?
  (match-args platform-id? platform-id?)
  (lambda (object platform) (eqv? platform object)))

(define-generic-procedure-handler platform-is?
  (match-args message-accepting-procedure? platform-id?)
  (lambda (procedure platform)
    (platform-is? (procedure 'get-platform-id) platform)))

(define-generic-procedure-handler platform-is?
  (match-args identifier? platform-id?)
  (lambda (identifier platform)
    (platform-is? (identifier-platform identifier) platform)))

(define (platform-predicate target-platform)
  (lambda (obj) (platform-is? obj target-platform)))

(define ((platform-is? target-platform) symbol)
  (and (symbol? symbol)
       (eqv? symbol target-platform)))

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


