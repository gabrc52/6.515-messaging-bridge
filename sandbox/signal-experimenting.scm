;;; Experimenting etc
;; Signal is trivial compared to Discord
;; TODO start the process
;; signal-cli daemon --socket /tmp/socket

(define *signal-socket* "/tmp/signal-socket")
(define *signal-binary* (os/find-program "signal-cli" ""))
(define signal-process
  (start-pipe-subprocess *signal-binary*
			 (vector *signal-binary* "daemon" "--socket" "/tmp/signal-socket")
			 #()))
(define socket (open-unix-stream-socket "/tmp/signal-socket"))

;; Group ID of test chat I created
(define signal-group-id "YVs2/HSkS8viSYndY2hw3dHIFfWAR2Wl88/VNU/PreY=")

(define (make-counter)
  (define current-count 0)
  (lambda ()
    (set! current-count (+ current-count 1))
    current-count))

(define signal-counter (make-counter))

;; https://www.jsonrpc.org/specification
(define (%json-rpc-call-expr method params)
  `(dict ("jsonrpc" . "2.0")
	 ("method" . ,method)
	 ("params" . ,params)
	 ;; TODO: the point of this is that the response comes with the same ID
	 ("id" . ,(signal-counter))))

(define (json-rpc-call method params)
  ;; TODO: this should construct a call with the above function, and check for a respose (if needed?)
  ;; e.g. some stuff like sending a message doesn't need a response, except a success confirmation maybe
  (begin))

;; TODO: it would be nice to be able eto do something like
(json-rpc-call "send"
	       'group signal-group-id
	       'message "Hello world!") ;; (if it is case insensitive)

;; Receiving
(pp (port-next-json socket))

;; TODO(for json serializer): for example, I forgot to write ,signal-group-id instead of signal-group-id
;; to-json.rkt prints an error message when finding a symbol in an unexpected place

;; Demo: Send a direct message
(port-send-json
 socket
 (%json-rpc-call-expr "send"
		      `(dict ("message" . "Hello!!! So true")
			     ("recipient" . "+18572680473"))))
;; You get the success message
;; Note that you get a delivery notification for all devices of the recipient (in my case ipad, phone and laptop)
(pp (port-next-json socket))
(pp (port-next-json socket))
(pp (port-next-json socket))

;; List groups
(port-send-json socket (%json-rpc-call-expr "listGroups" `(dict)))
(pp (port-next-json socket))

;; Demo: Send a message to a group
(port-send-json
 socket
 ;;(current-output-port) ;; For debugging
 (%json-rpc-call-expr "send"
		      `(dict ("message" . "Hello!!! So true bestie")
			     ("group-id" . ,signal-group-id))))
(pp (port-next-json socket))

;; Errors

;; Specifying the group ID as "recipient" gives an "UNREGISTERED_FAILURE".
;; (dict ("id" . 9) ("error" dict ("code" . -1) ("data" dict ("response" dict ("timestamp" . 1745703796909) ("results" list (dict ("recipientAddress" dict ("uuid" . null) ("number" . "+12823288")) ("type" . "UNREGISTERED_FAILURE"))))) ("message" . "Failed to send message")) ("jsonrpc" . "2.0"))

;; Specifying the group ID as "group" rather than "group-id" gives a "No recipients given" error
;; (dict ("id" . 10) ("error" dict ("code" . -1) ("data" . null) ("message" . "No recipients given")) ("jsonrpc" . "2.0"))

;; Closing the socket
(close-port socket)
;; TODO: also end process, once these are all wrapped
