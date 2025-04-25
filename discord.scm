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
    (port-send-json discord (make-heartbeat *heartbeat-number*)))

  (define (discord-identify! discord)
    ;; Identify (authenticate etc) ourselves to Discord
    (port-send-json
     discord
     `(dict
       ("op" . ,op-identify) ;; Important
       ("d" . (dict
	       ("token" . ,*discord-token*)
	       ("properties" . (dict
				("os" . ,microcode-id/operating-system-variant)
				("browser" . "mit-scheme")
				("device" . "mit-scheme")))
	       ;; could also pass a presence, see docs
	       ("intents" . ,*intents*)))))))

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
	   (write-line "Discord says hello")))
	(else (write-line "Unknown event type"))))

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

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Discord demo ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define discord
  (websocket-connect! "wss://gateway.discord.gg/?v=10&encoding=json" '()))

(when-available (port-next-json discord) handle-event) ;; "Discord says hello"

;; TODO: Need to send a heartbeat every 40 seconds following the instructions in the documentation.
;; "If your app fails to send a heartbeat event in time, your connection will be closed and you will be forced to Resume."

;; We need to send the first heartbeat at the beginning
(discord-send-heartbeat! discord)
(when-available (port-next-json discord) handle-event)
;; (dict ("d" . null) ("t" . null) ("op" . 11) ("s" . null))

(discord-identify! discord)
(when-available (port-next-json discord) handle-event) ;; Logged in as Messaging Bridge
(when-available (port-next-json discord) handle-event) ;; We are in the server gabrielr's server

;; Send another heartbeat so they don't kill us
(discord-send-heartbeat! discord)
(when-available (port-next-json discord) handle-event)
;; (dict ("d" . null) ("t" . null) ("op" . 11) ("s" . null))

;; After sending a message to the server, we can read it now!
(when-available (port-next-json discord) handle-event) ;; Unimplemented dispatch event type typing_start
(when-available (port-next-json discord) handle-event) ;; Gabriel R. says "Hello, this is a test message!"!

;; NOTE: This is what happens if you don't reply to heartbeats on time:
;; `The primitive channel-write, while executing the write system call, received the error: Broken pipe.`

;; There is a way of closing the connection explicitly that is not just closing the port
;; See https://discord.com/developers/docs/events/gateway#initiating-a-disconnect
;; "When you close the connection to the gateway with close code 1000 or 1001, your session will be invalidated and your bot will appear offline."
(close-port discord)

;; TODO: also handle what if Discord disconnects
;; "Due to Discord's architecture, disconnects are a semi-regular event and should be expected and handled. When your app encounters a disconnect, it will typically be sent a close code which can be used to determine whether you can reconnect and Resume the session, or whether you have to start over and re-Identify."
