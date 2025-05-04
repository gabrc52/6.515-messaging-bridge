;; TODO: implement and make it even compile lol

;;; Trying to get a clean implementation while also brainstorming the common interface

(define *signal-socket* "/tmp/signal-socket")
(define *signal-binary* (os/find-program "signal-cli" ""))

;; TODO: we need a way to associate the symbol 'signal with this maker (in the other direction, probably a simple key, value store)

;; TODO: move these to a different file

;; There is no `self` because no OOP but we can name a lambda (message passing procedure) and just use its name and return it

;; We don't actually need ready? if we use threads,
;; but it allows us to revert to the busy-waiting implementation if threads

;; I guess they are more like mixins/delegates/idk

(define (make-stream-based-messaging-client stream ready? getter putter)
  )

(define (make-port-based-messaging-client port)
  (make-stream-based-messaging-client port
				      port-ready? ;; alias for char-ready?
))				      

;; TODO: move common, non-signal stuff to something more general
(define (make-signal)
  (define signal-process
    (start-pipe-subprocess *signal-binary*
			   (vector *signal-binary* "daemon" "--socket" "/tmp/signal-socket")
			   #()))
  (define socket (open-unix-stream-socket "/tmp/signal-socket"))
  (lambda (op)
    (case op
      ((get-platform) 'signal)
      ;; TODO: some notion of a general sender, kind of like dispatch stores?
      ;; TODO: do we need a lock? not if we have a queue, I guess? not sure
      ;; TODO: but then we need the RPC stuff
      ;; I think I have some cyclical dependencies in the design and need to think carefully about where to start...
      ((get-sender) (lambda (jsexpr) (port-send-json socket 
    ))))))
