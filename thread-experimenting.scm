;; Demonstration that with simple threads we can listen to various messaging apps at the same time

(define mattermost
  (websocket-connect! "wss://mattermost.mit.edu/api/v4/websocket"
		      '(("Authorization" . "Bearer otfjuew96pfh8rrfxga3nf7mby"))))

;; Now we actually do want the blocking version of each, instead of checking if it is ready

(define (port-next-json-blocking port)
  (string->jsexpr (read-line port)))

(define (websocket-next-json-blocking websocket)
  (port-next-json-blocking (websocket-port websocket)))

;; (websocket-close! mattermost)

(define *signal-binary* (os/find-program "signal-cli" ""))
(define signal-process
  (start-pipe-subprocess *signal-binary*
			 (vector *signal-binary* "daemon" "--socket" "/tmp/signal-socket")
			 #()))
(define signal-socket (open-unix-stream-socket "/tmp/signal-socket"))

;; Now create both threads

;; We don't need a continuation, set it to #f. TODO: when do we need continuations?

(define mattermost-thread
  (create-thread
   #f
   (lambda ()
     ;; This would be the actual logic... TODO: turn into a more general function
     (let loop ()
       (let ((event (websocket-next-json-blocking mattermost)))
	 (pp event)
	 (loop))))))

(define signal-thread
  (create-thread
   #f
   (lambda ()
     (let loop ()
       (let ((event (port-next-json-blocking signal-socket)))
	 (pp event)
	 (loop))))))


;; This test is successful!! We are listening on two ports at the same time without turning on the fan.
;; Messages from either Signal or Mattermost get printed out successfully.

