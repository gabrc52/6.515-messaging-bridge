;;; Predicates

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


