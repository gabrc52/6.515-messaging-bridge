;; Depends on http.scm for the header builder
;; Depends on a JSON implementation to get a JSON

(define *websocat-binary* (os/find-program "websocat" ""))

;; The port is almost enough for everything, but not for e.g. knowing if the connection was closed
;; because the port acts like it is still open. So let's have a record type with both.
(define-record-type <websocket>
  (%make-websocket url subprocess port)
  websocket?
  (url websocket-url)
  (subprocess websocket-subprocess)
  (port websocket-port))

;; Connect to a websocket, returns a port
;; Other than stdout/stdin, there are 2 alternatives for websocat:
;;   * Open a socket at a TCP port
;;   * Open a unix socket at a specific filename
;; I am choosing to go with reading from stdout/stdin, but we can reconsider the other options
;; if there are issues.
(define (websocket-connect! websocket-url #!optional headers more-args)
  (let ((headers (if (default-object? headers) (list) headers))
	(more-args (if (default-object? more-args) (list) more-args)))
    (let ((subprocess (start-pipe-subprocess
		       *websocat-binary*
		       (list->vector
			(append (list *websocat-binary*)
			        more-args
				(list "--text" "--exit-on-eof" websocket-url)
				(prepare-headers headers)))
		       #())))
      (assert (eqv? (subprocess-status subprocess) 'running))
      ;; There is just one port which is a pipe for both input and output :)
      (assert (eqv? (subprocess-output-port subprocess) (subprocess-input-port subprocess)))
      (%make-websocket
       websocket-url
       subprocess
       (subprocess-output-port subprocess)))))

(define (subprocess-running? subprocess)
  ;; If the connection is closed, it will probably be 'exited
  ;; But if you `pkill websocat` or (subprocess-quit subprocess), it will say 'signalleld
  ;; From the source code, 'stopped also exists
  (eqv? (subprocess-status subprocess) 'running))

(define (websocket-connected? websocket)
  (subprocess-running? (websocket-subprocess websocket)))

;; Closing the port should stop the process too.
(define (websocket-close! websocket)
  (close-port (websocket-port websocket))
  ;; The assertion does not actually pass, it might take some milliseconds to be deemed as not running
  #|(assert (not (websocket-connected? websocket)))|#)

;; These are just operations on ports, but good for websockets too

(define port-ready? char-ready?)
(define (websocket-ready? websocket)
  (port-ready? (websocket-port websocket)))

;; Defensive: These functions provide extra guardrails by not hanging when there is no new message

;; When not #f, do something
;; Question: would continuations help here? Probably not
(define (when-available obj-or-false callback)
  (if obj-or-false
      (callback obj-or-false)
      #f))

(define (port-send-line port string)
  (display string port)
  (newline port)
  (flush-output port))
(define (websocket-send-line websocket string)
  (port-send-line (websocket-port websocket) string))
(define (port-send-json port jsexpr)
  (port-send-line port (jsexpr->string jsexpr)))
(define (websocket-send-json websocket jsexpr)
  (port-send-json (websocket-port websocket) jsexpr))

