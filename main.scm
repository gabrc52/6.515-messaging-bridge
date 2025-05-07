;; Gabriel's implementation of main
;; TODO: Consolidate interfaces, make this work with queues
;;   We could also make clients tell us whether we need to start a thread for them or not.

(define (create-client-thread! platform client)
  (create-thread
   #f
   (lambda ()
        (let loop ()
            (when-available
                ((client 'raw-event-receiver)) ;; This may be blocking
                (lambda (raw-event)
                    ;; TODO: put in queue instead and have a thread that handles all events in that queue
                    ;; (display (string ";; Got " platform " event: ")) (write raw-event) (newline)
                    (handle-event! (make-event platform raw-event))))
            (loop)))
   (symbol platform '-thread)))
	 
(for-each (lambda (pair)
	    (let ((platform (car pair))
		  (client (cdr pair)))
	      (create-client-thread! platform client)))
	  *all-clients*)
