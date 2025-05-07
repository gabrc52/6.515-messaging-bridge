;; A JSON-RPC client.
;; https://www.jsonrpc.org/specification

;; A simple counter for request IDs
(define (make-counter)
  (let ((current-count 0))
    (lambda ()
      (set! current-count (+ current-count 1))
      current-count)))

;; A callback store is just a hash table for storing callbacks to run whenever the JSON-RPC call completes.
;;   (Not a queue because they may not return in the same order, otherwise there would be no point in the IDs).
;;   (Assuming the server and clients are reasonable. We could add extra checks if they aren't).
(define (make-callback-store)
  ;; TODO: I am not confident on my choice of strong and not weak.
  ;;   Presumably I don't want the callbacks to disappear until I am done with them?
  (make-strong-eqv-hash-table))
(define (push-callback! store id callback)
  (hash-table-set! store id callback))
;; Get the callback and remove it from the hash table, as we only need it once.
(define (pop-callback! store id)
  (let ((callback (hash-table-ref store id)))
    (hash-table-delete! store id)
    callback))

(define (make-json-rpc-based-client delegate)
  (let ((counter (make-counter))
	(success-callbacks (make-callback-store))
	(error-callbacks (make-callback-store))
	(sender (delegate 'raw-event-sender))
	(receiver (delegate 'raw-event-receiver)))
	
    (define (json-rpc-call-expression method params id)
      `(dict ("jsonrpc" . "2.0")
	     ("method" . ,method)
	     ("params" . ,params)
	     ("id" . ,id)))

    ;; In JavaScript, you can turn callback-based interfaces into awaitable interfaces.
    ;;   I don't know how much we can replicate that in Scheme LOL.
    (define (json-rpc-call method params success-callback error-callback)
      (let ((id (counter)))
	(sender (json-rpc-call-expression method params id))
	(push-callback! success-callbacks id success-callback)
	(push-callback! error-callbacks id error-callback)))

    (define (%receive-raw-event!)
      (when-available
       (receiver)
       (lambda (event)
	 (if (json-key-exists? event "id")
	     ;; This is the response to one of our calls.
	     (let ((id (json-key event "id")))
	       (let ((success-callback (pop-callback! success-callbacks id))
		     (error-callback (pop-callback! error-callbacks id)))
		 (cond ((json-key-exists? event "result") (success-callback (json-key event "result")))
		       ((json-key-exists? event "error") (error-callback (json-key event "error")))
		       (else (display (string "Strange JSON-RPC event received: " event))))
		 #f))
	     ;; Otherwise, the server sent us a call ("method" is "receive" on Signal)
	     event))))
    
    (lambda (message)
      (case message
	((raw-event-receiver) %receive-raw-event!)
	((remote-function-caller) json-rpc-call)
	(else (delegate message))))))
