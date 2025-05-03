;;; FROM ps08 codebase

;;; We use the queue design similar to SICP Section 3.3.2
;;; Although the operations that are done here must be atomic, 
;;; the atomicity is imposed at the calls to these routines.

(define-record-type queue
    (queue:make-record front-ptr rear-ptr)
    queue?
  (front-ptr queue:front-ptr queue:set-front-ptr!)
  (rear-ptr  queue:rear-ptr  queue:set-rear-ptr!))

(define (queue:make)
  (queue:make-record '() '()))

(define (queue:empty? queue)
  (null? (queue:front-ptr queue)))

(define (queue:get-first! queue)
  (if (null? (queue:front-ptr queue))
      (error "get-first! called with an empty queue" queue)
      (let ((first (car (queue:front-ptr queue)))
	    (rest (cdr (queue:front-ptr queue))))
	(queue:set-front-ptr! queue rest)
	(if (null? rest)
	    (queue:set-rear-ptr! queue '()))
	first)))

(define (queue:add-to-end! queue item)
  (let ((new-pair (cons item '())))
    (cond ((null? (queue:front-ptr queue))
	   (queue:set-front-ptr! queue new-pair)
	   (queue:set-rear-ptr! queue new-pair))
	  (else
	   (set-cdr! (queue:rear-ptr queue) new-pair)
	   (queue:set-rear-ptr! queue new-pair))))
  'done)
