(define signal? (platform-predicate 'signal))
(register-predicate! signal? 'signal)
(define *signal-socket* "/tmp/signal-socket")
(define *signal-binary* (os/find-program "signal-cli" ""))

(define signal-config:phone-number
  (make-property 'phone-number
		 'predicate string?))
(define signal-config?
  (make-type 'signal-config (list signal-config:phone-number)))
(set-predicate<=! signal-config? platform-config?)
(register-config-constructor! 'signal signal-config?)
(define signal-config-phone-number (property-getter signal-config:phone-number signal-config?))

(define (make-signal! config)
  (define phone-number (signal-config-phone-number config))
  (define signal-process (start-pipe-subprocess *signal-binary*
			   (vector *signal-binary* "-a" phone-number "jsonRpc")
			   #()))
  (define port (subprocess-output-port signal-process))
  (assert (eqv? port (subprocess-input-port signal-process))) ;; It is a single port for both input & output
  (define delegate (make-json-rpc-based-client (make-port-based-client port)))
  (define json-rpc-call (delegate 'remote-function-caller))
  (define receiver (delegate 'raw-event-receiver))
  (define (%receive-raw-event!)
    (when-available
     (receiver)
     (lambda (event)
       (let ((method (json-key event "method")))
	 (if (equal? method "receive")
	     (json-key event "params") ;; We just care about the parameters
	     (display (string "Signal: Unknown incoming method " method)))))))
  (define (%send-message chat text)
    ;; DMs/users are identifed by a 32-character UUID or a shorter phone number. Group IDs seem to be ~44 chars.
    (let ((is-dm (<= (string-length chat) 36)))
      (json-rpc-call "send"
		     `(dict ("message" . ,text)
			    ,(if is-dm `("recipient" . ,chat) `("group-id" . ,chat)))
		     pp error)))
  (write-line "Signal client created.")
  (lambda (op)
    (case op
      ((get-platform-id) 'signal)
      ((%get-process) signal-process)
      ((%get-port) port)
      ((raw-event-receiver) %receive-raw-event!)
      ((message-sender) %send-message)
      (else (delegate op)))))
(define-generic-procedure-handler make-client! (match-args signal-config?) make-signal!)

;; All messages are inside an "envelope"
;; It appears to have "source", "sourceUuid", "sourceName", "sourceDevice", "timestamp",
;;   "serverReceivedTimestamp", "serverDeliveredTimestamp", and either
;;   "typingMessage" or "receiptMessage" or "dataMessage" (there may be more)
;; Normal messages are "dataMessage". If it is in a group it will contain "groupInfo" inside the "dataMessage"
;;   Otherwise that will be blank. Either way, the source (sender) will be set, whether a DM or not.
;; Probably the easiest way to implement non-data messages would be to pre-process the messages
;;   and replace "dataMessage" with "payload" and add a new "type" field

(define-generic-procedure-handler chat-event?
  (match-args signal?) ;; TODO: if this does not work, make a predicate for events (and same for below)
  (lambda (event)
    (assert (signal? event) "should be signal event")
    ;; (write-line "We are at Signal chat-event?")
    (and (event-key? event "envelope")
	 (let ((envelope (event-key event "envelope")))
	   ;; TODO(stretch): missing typingMessage (and potentially others? but even reactions have "dataMessage")
	   (json-key-exists? envelope "dataMessage")))))

(define-generic-procedure-handler message-content
  (match-args signal?)
  (lambda (event)
    (guarantee chat-event? event)
    (event-key event "envelope" "dataMessage" "message")))

(define-generic-procedure-handler message-event?
  (match-args signal?)
  (lambda (event)
    (and (chat-event? event) ;; TODO(stretch): ditto (and below too)
	 (not (eqv? (message-content event) null)))))

(define-generic-procedure-handler bridged-event?
  (match-args signal?)
  ;; In my testing, Signal does not send us messages sent by ourselves.
  ;; TODO: double check / implement it defensively (maybe when multiple devices, it might)
  (lambda (_) #f))

(define-generic-procedure-handler event-chat
  ;; TODO(stretch): again would not handle typingMessage (because signal-cli returns a horrible JSON format
  ;;   where the key used depends on the type of thing)
  (match-args signal?)
  (lambda (event)
    (guarantee chat-event? event)
    (let ((dataMessage (event-key event "envelope" "dataMessage")))
      (make-identifier 'signal 
		       (if (json-key-exists? dataMessage "groupInfo")
			   (json-key dataMessage "groupInfo" "groupId")
			   ;; For now, the chat in direct messages shall be the sender (if it is in a DM)
			   (event-key event "envelope" "sourceUuid"))))))

(define-generic-procedure-handler event-sender
  (match-args signal?)
  (lambda (event)
    (guarantee chat-event? event)
    (event-key event "envelope" "sourceName")))
