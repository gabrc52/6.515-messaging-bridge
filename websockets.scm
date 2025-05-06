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

;; Not sure if this is a good name.
;; TODO: For abstraction we should have something called message-available?
;;   (and it can work with ports, but not necessarily)
;; Yeah, but that is a higher level wrapper for messages, we should just wrap the current functions
;;   into the domain-specific functions we want
(define port-ready? char-ready?)
(define (websocket-ready? websocket)
  (port-ready? (websocket-port websocket)))

;; Would this help?
#||
(define (websocket-ready? websocket)
  (let ((subprocess (websocket-subprocess websocket))
	(port (websocket-port websocket)))
    (and (subprocess-running? subprocess) (port-ready? port))))
||#

;; Defensive: These functions provide extra guardrails by not hanging when there is no new message

(define (port-next-line port)
  (and (port-ready? port)
       (read-line port)))
(define (websocket-next-line websocket)
  (port-next-line (websocket-port websocket)))

;; When not #f, do something
;; Question: would continuations help here? Probably not
(define (when-available obj-or-false callback)
  (if obj-or-false
      (callback obj-or-false)
      #f))

;; TODO: handle when you can't parse JSON or when the port reaches #!eof
(define (port-next-json port)
  (when-available (port-next-line port) string->jsexpr))
(define (websocket-next-json websocket)
  (port-next-json (websocket-port websocket)))

;; Blocking json from port
(define (port-next-json-blocking port)
  (string->jsexpr (read-line port)))

(define (websocket-next-json-blocking websocket)
  (port-next-json-blocking (websocket-port websocket)))


;; TODO: should the get/send be curried? it's very easy to get a getter/sender function instead
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

;;;; Demonstration

(define mattermost
  (websocket-connect! "wss://mattermost.mit.edu/api/v4/websocket"
		     '(("Authorization" . "Bearer otfjuew96pfh8rrfxga3nf7mby"))))

;; This works now! Returns #f or the next message.
(pp (websocket-next-json mattermost))

(websocket-connected? mattermost) ;; #t

(websocket-close! mattermost)

(websocket-connected? mattermost) ;; #f

;;;; Done

;; TODO: move TODOs below to a separate messaging-client.scm thing or something

;; TODO: something like make-websocket-message-receiver or something
;; or something like message client which abstracts both plain HTTPS and websockets

;; TODO: we need a thing which takes multiple ports and checks them all, and calls callbacks
;; depending. This is why we 
