;; Depends on http.scm for the header builder
;; Depends on a JSON implementation to get a JSON

(define *websocat-binary*
  (or (get-environment-variable "WEBSOCAT_BINARY")
      "/home/rgabriel/.cargo/bin/websocat"))

;; For debugging
(define *latest-subprocess*)

;; Connect to a websocket, returns a port
;; Other than stdout/stdin, there are 2 alternatives for websocat:
;;   * Open a socket at a TCP port
;;   * Open a unix socket at a specific filename
;; I am choosing to go with reading from stdout/stdin, but we can reconsider the other options
;; if there are issues.
(define (websocket-connect! websocket-url headers)
  (let ((subprocess (start-pipe-subprocess
		     *websocat-binary*
		     (list->vector
		      (append (list *websocat-binary* "--text" "--exit-on-eof" websocket-url)
			      (prepare-headers headers)))
		     #())))
    (assert (eqv? (subprocess-status subprocess) 'running))
    ;; There is just one port which is a pipe for both input and output :)
    (assert (eqv? (subprocess-output-port subprocess) (subprocess-input-port subprocess)))
    (set! *latest-subprocess* subprocess)
    (subprocess-output-port subprocess)))

;; Closing the port should stop the process too.
(define websocket-close close-port)

;; These are just operations on ports, but good for websockets too

;; Not sure if this is a good name.
;; TODO: For abstraction we should have something called message-available?
;;   (and it can work with ports, but not necessarily)
(define port-ready? char-ready?)

;; Defensive: These functions provide extra guardrails by not hanging when there is no new message

(define (port-next-line port)
  (and (port-ready? port)
       (read-line port)))
(define websocket-next-line port-next-line)

;; When not #f, do something
;; Question: would continuations help here? Probably not
(define (when-available obj-or-false callback)
  (if obj-or-false
      (callback obj-or-false)
      #f))

;; TODO: handle when you can't parse JSON
(define (port-next-json port)
  (when-available (port-next-line port) string->jsexpr))
(define websocket-next-json port-next-json)

;; TODO: should the get/send be curried? it's very easy to get a getter/sender function instead
(define (port-send-line port string)
  (display string port)
  (newline port)
  (flush-output port))
(define (port-send-json port jsexpr)
  (port-send-line port (jsexpr->string jsexpr)))
(define websocket-send-line port-send-line)
(define websocket-send-json port-send-json)

;;;; Demonstration

(define mattermost
  (websocket-connect! "wss://mattermost.mit.edu/api/v4/websocket"
		     '(("Authorization" . "Bearer otfjuew96pfh8rrfxga3nf7mby"))))

;; This works now! Returns #f or the next message.
(port-next-json mattermost)

;;;; Done

;; TODO: move TODOs below to a separate messaging-client.scm thing or something

;; TODO: something like make-websocket-message-receiver or something
;; or something like message client which abstracts both plain HTTPS and websockets

;; TODO: we need a thing which takes multiple ports and checks them all, and calls callbacks
;; depending. This is why we 
