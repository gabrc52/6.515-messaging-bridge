;; Discord reference documentation for receiving: https://discord.com/developers/docs/events/gateway
;; As for sending messages and performing actions, it is a HTTPS API

;;;; Define constants and functions

(begin ;; Wrapped in begin to allow pressing Alt+Enter below and defining them all in the REPL at once

  ;; TODO: don't hardcode this, use a config file. Invalidate this token before releasing repo publicly
  (define *discord-token* "OTAxMjUxMDU1MjczMjcxMzE3.GEjI7x.hScW3U9Y7qF7QeOekRwpUdEPwzzDt8smW8YjS4")

  ;; From https://github.com/simmsb/racket-cord/blob/master/private/gateway.rkt
  ;; I did not read the Racket code but it may in come handy to port some of it to MIT/GNU Scheme
  ;;   or at least it can serve as a reference implementation
  (define op-dispatch 0)
  (define op-heartbeat 1)
  (define op-identify 2)
  (define op-presence-update 3)
  (define op-voice-state-update 4)
  (define op-resume 6)
  (define op-reconnect 7)
  (define op-request-guild-members 8)
  (define op-invalid-session 9)
  (define op-hello 10)
  (define op-heartbeat-ack 11)

  ;; "The inner d key is the last sequence number—s—received by the client.
  ;; If you have not yet received one, send null."
  (define (make-heartbeat number)
    `(dict ("op" . ,op-heartbeat)
	   ("d" . ,number)))

  (define (discord-op event)
    (json-key event "op"))

  ;; TODO: is there a better way to define these predicates for all the types?
  ;;   Would it help or would it just be unnecessary?
  (define (event-heartbeat? event)
    (= (discord-op event) op-heartbeat))
  (define (event-ready? event)
    (= (discord-op event) op-ready))
  (define (event-heartbeat-ack? event)
    (= (discord-op event) op-heartbeat-ack))
  (define (event-dispatch? event)
    (= (discord-op event) op-dispatch))
  (define (event-hello? event)
    (= (discord-op event) op-hello))

  ;; https://discord.com/developers/docs/events/gateway#gateway-intents
  (define << shift-left)
  (define (get-intent-number intent-list)
    (reduce bitwise-ior 0 intent-list))
  ;; https://discord.com/developers/docs/events/gateway#list-of-intents
  (define *intents*
    (get-intent-number
     (list
      ;; GUILDS
      (<< 1 0)

      ;; GUILD_MEMBERS
      (<< 1 1)

      ;; GUILD_MESSAGES
      ;; This is the one we definitely do need
      (<< 1 9)
      ;; MESSAGE_CONTENT
      ;; Also very important! We want the actual message content...
      (<< 1 15)

      ;; GUILD_MESSAGE_REACTIONS
      (<< 1 10)

      ;; GUILD_MESSAGE_TYPING
      (<< 1 11)

      ;; DIRECT_MESSAGES
      ;; Gonna be good for debugging or some basic bot functionality
      (<< 1 12)

      ;; DIRECT_MESSAGE_REACTIONS
      (<< 1 13)

      ;; DIRECT_MESSAGE_TYPING
      (<< 1 14))))
  
  ;; This will be set by the handler for the first "ready" event
  ;; See step 7. Resume is "to replay missed events when a disconnected client resumes"
  ;;   It does not seem essential, though.
  (define *resume-gateway-url*)
  (define *session-id*)

  (define *heartbeat-interval*) ;; Will be set by "hello" event
  
  (define *heartbeat-number* null)
  (define (discord-send-heartbeat! discord)
    (websocket-send-json discord (make-heartbeat *heartbeat-number*)))

  ;; NOTE: "Clients are limited to 1000 IDENTIFY calls to the websocket in a 24-hour period"
  (define (discord-identify! discord)
    ;; Identify (authenticate etc) ourselves to Discord
    (websocket-send-json
     discord
     `(dict
       ("op" . ,op-identify) ;; Important
       ("d" . (dict
	       ("token" . ,*discord-token*)
	       ("properties" . (dict
				("os" . ,microcode-id/operating-system-variant)
				("browser" . "mit-scheme")
				("device" . "mit-scheme")))
	       ("intents" . ,*intents*)
	       ;; This is optional, just for aesthetic purposes
	       ("presence" . (dict
			      ("afk" . #f)
			      ("status" . "online")
			      ("activities" . (list (dict
						     ("name" . "MIT/GNU Scheme")
						     ;; 0 is "playing" (as in a game)
						     ;; 4 is "custom"
						     ("type" . 0)))))))))))

  ;;;; Handling for specific events

  ;; No idea why the Discord Gateway API is so unnecessarily complicated with its
  ;; heartbeats and everything

  ;; Basic event handler (first prototype)
  ;; TODO: Consider using a generic procedure here instead
  ;; * We have logic for all base events (keep track of sequence number)
  ;; * And we have some logic for specific type events
  ;; * And we have predicates that are subsets of other predicates (e.g. all the "dispatch" events)
  (define (handle-event event)
    ;; Print (for debugging rn)
    (pp event)
    ;; Get sequence number and set global variable
    (let ((sequence-number (json-key event "s")))
      (set! *heartbeat-number* sequence-number))
    ;; Handle per type of event
    (cond ;; "Discord may send the app a Heartbeat (opcode 1) event,
     ;; in which case the app should send a Heartbeat event IMMEDIATELY [emphasis added]".
     ((event-heartbeat? event) (discord-send-heartbeat! discord))
     ;; TODO: "If an app doesn't receive a Heartbeat ACK [in response to a heartbeat],
     ;;        it should close the connection and reconnect."
     ((event-heartbeat-ack? event) (begin))
     ((event-dispatch? event) (handle-dispatch-event event))
     ((event-hello? event)
      (begin
	(set! *heartbeat-interval* (json-key event "d" "heartbeat_interval"))
	(display "Discord says hello")))
     (else (display "Unknown event type"))))

  ;; "Dispatch" (word from Discord docs) events are messages and most user-facing features, rather
  ;; than events only for implementation details.
  (define (handle-dispatch-event event)
    (assert (event-dispatch? event))
    (let ((type (string->symbol (string-downcase (json-key event "t"))))
	  (content (json-key event "d")))
      (case type
	((ready)
	 (begin
	   (display (string "Logged in as " (json-key content "user" "username")))
	   (set! *session-id* (json-key content "session_id"))
	   (set! *resume-gateway-url* (json-key content "resume_gateway_url"))))
	((guild_create) (display (string "We are in the server " (json-key content "name"))))
	((message_create)
	 (let ((name (json-key content "author" "global_name"))
	       (message (json-key content "content")))
	   (display (format #false "~A says \"~A\"!" name message))))
	(else (display (string "Unimplemented dispatch event type " type))))))

  (define (discord-handle-event discord)
    (let ((json (websocket-next-json discord)))
      (if json
	  (handle-event json)
	  (unless (websocket-connected? discord)
	    ;; TODO: reconnect with *resume-gateway-url* and *session-id*. See Gateway docs
	    ;; Alternatively, just starting the whole connection again probably works too (but it might
	    ;;   lead to some missed messages).
	    ;; My thought is we should either
	    ;;    (1) really make sure to reply to heartbeats immediately
	    ;;       (deal with time!) (because otherwise we might be handling some other event from some other
	    ;;                          messaging service and by the time we handle this one, it will be too late)
	    ;;    (2) make sure to reconnect using the Discord-provided instructions
	    ;; (1) sounds like more effort, so I would go with (2) is just implementing what Discord asks you to,
	    ;;  and it is more robust because the docs imply that Discord may drop the connection through
	    ;;  no fault of our own
	    (display "The connection got dropped! We should reconnect!"))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Discord demo ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define discord
  (websocket-connect! "wss://gateway.discord.gg/?v=10&encoding=json"
		      '() ;; No headers necessary.
		      ;; "When you close the connection to the gateway with close code 1000 or 1001,
		      ;;  your session will be invalidated and your bot will appear offline."
		      '("--close-status-code" "1000")))

;; Previous implementation:
;;(when-available (websocket-next-json discord) handle-event)

(discord-handle-event discord) ;; "Discord says hello"
(discord-handle-event discord) ;; #f

;; TODO: Need to send a heartbeat every 40 seconds following the instructions in the documentation.
;; "If your app fails to send a heartbeat event in time, your connection will be closed and you will be forced to Resume."

;; We need to send the first heartbeat at the beginning
(discord-send-heartbeat! discord)
(discord-handle-event discord)
;; (dict ("d" . null) ("t" . null) ("op" . 11) ("s" . null))

(discord-identify! discord)
(discord-handle-event discord) ;; Logged in as Messaging Bridge

(with-timings
 (lambda () (discord-handle-event discord))
 (lambda (run-time gc-time real-time)
   (write (internal-time/ticks->seconds run-time))
   (write-char #\space)
   (write (internal-time/ticks->seconds gc-time))
   (write-char #\space)
   (write (internal-time/ticks->seconds real-time))
   (newline)))
;; We are in the server gabrielr's server
;; * json-handler: .09 0. .084
;; * json-racket: .09 0. .255

;; Send another heartbeat so they don't kill us
(discord-send-heartbeat! discord)
(discord-handle-event discord)
;; (dict ("d" . null) ("t" . null) ("op" . 11) ("s" . null))

;; After sending a message to the server, we can read it now!
(discord-handle-event discord) ;; Unimplemented dispatch event type typing_start
(discord-handle-event discord) ;; Gabriel R. says "Hello, this is a test message!"!

;; NOTE: This is what happens if you don't reply to heartbeats on time:
;; `The primitive channel-write, while executing the write system call, received the error: Broken pipe.`

;; To test disconnections, it is surprisingly easy to make it disconnect (by sending it an invalid JSON)
(websocket-send-json discord '(dict))
(discord-handle-event discord) ;; The connection got dropped! We should reconnect!

;; Simple stub loop. It does not handle multiple websockets (for multiple services).
;; nor does it send the heartbeats every *heartbeat-interval* yet
(let loop ()
  (unless (websocket-connected? discord) (websocket-close! discord))
  (discord-handle-event discord)
  (loop))
;; This is blocking and turns on my laptop fans. Maybe there is a better way

;; There is a way of closing the connection explicitly that is not just closing the port
;; See https://discord.com/developers/docs/events/gateway#initiating-a-disconnect
;; "When you close the connection to the gateway with close code 1000 or 1001, your session will be invalidated and your bot will appear offline." (TODO: implement this?)
;; It also closes the connection if you send it invalid data
(websocket-close! discord)

;; TODO: also handle what if Discord disconnects
;; "Due to Discord's architecture, disconnects are a semi-regular event and should be expected and handled. When your app encounters a disconnect, it will typically be sent a close code which can be used to determine whether you can reconnect and Resume the session, or whether you have to start over and re-Identify."

