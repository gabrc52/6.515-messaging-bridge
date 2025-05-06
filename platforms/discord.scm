;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Discord Internals ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Discord reference documentation for receiving: https://discord.com/developers/docs/events/gateway
;; As for sending messages and performing actions, it is a HTTPS API

;;;; Define constants and functions
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

(define (make-event-predicate op)
(lambda (event)
    (= (discord-op event) op)))

;   (define event-heartbeat? (make-event-predicate op-heartbeat))
;   (define event-ready? (make-event-predicate op-ready))
;   (define event-heartbeat-ack? (make-event-predicate op-heartbeat-ack))
;   (define event-dispatch? (make-event-predicate op-dispatch))
;   (define event-hello? (make-event-predicate op-hello))

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
;; NEEDS to be configured on the discord dev portal
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

; (define (discord-send-and-wait fn-with-args discord) ;; TODO: generalize w/ combinator
;     ,fn-with-args
;     (discord-handle-event discord))

(define (discord-send-heartbeat! discord)
    (websocket-send-json discord (make-heartbeat *heartbeat-number*)))

(define (discord-keepalive-fetch discord last-ts) ;; TODO: generalize
    (if (> (- (get-time-ms) last-ts) *heartbeat-interval*)
        (cons (#t get-time-ms))
        (cons (#f last-ts))))

(define (discord-keepalive-loop discord ts)
    (when (websocket-connected? discord)
        (let ((fetched (discord-keepalive-fetch discord ts)))
            (let ((ready (car fetched))
                (next-ts (cdr fetched)))
                (when ready
                    (begin
                        (pp "[discord-keepalive-loop] Sending heartbeat!")
                        (discord-send-heartbeat! discord)))
                (discord-keepalive-loop discord next-ts)))))

(define (discord-start-keepalive-loop discord)
    (create-thread
        #f
        (lambda ()
            (discord-keepalive-loop discord 0))
        "Discord Keepalive Loop"))

;; NOTE: "Clients are limited to 1000 IDENTIFY calls to the websocket in a 24-hour period"
(define (discord-identify! discord token)
    ;; Identify (authenticate etc) ourselves to Discord
    (websocket-send-json
        discord
        `(dict
        ("op" . ,op-identify) ;; Important
        ("d" . (dict
            ("token" . ,token)
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
(define (discord-handle-json-event discord event)
    ;; Print (for debugging rn)
    ; (pp event)
    ;; Get sequence number and set global variable
    (let ((sequence-number (json-key event "s")))
        (set! *heartbeat-number* sequence-number))
        ;; Handle per type of event
        (cond ;; "Discord may send the app a Heartbeat (opcode 1) event,
            ;; in which case the app should send a Heartbeat event IMMEDIATELY [emphasis added]".
            ((event-heartbeat? event) 
                (begin
                    (pp "[discord-handle-json-event] Sending heartbeat!")
                    (discord-send-heartbeat! discord)
                    '())) ;; Return '()
            ;; TODO: "If an app doesn't receive a Heartbeat ACK [in response to a heartbeat],
            ;;        it should close the connection and reconnect."
            ((event-heartbeat-ack? event) 
                (begin
                    '())) ;; Return '()
            ((event-dispatch? event) (discord-handle-dispatch-event discord event))
            ((event-hello? event)
                (begin
                    (set! *heartbeat-interval* (json-key event "d" "heartbeat_interval"))
                    (display "Discord says hello"))
                    '()) ;; Return '()
            (else 
                (begin
                    (display "Unknown event type")
                    '())))) ;; Return '()

;; "Dispatch" (word from Discord docs) events are messages and most user-facing features, rather
;; than events only for implementation details. (opcode 0)
(define (discord-handle-dispatch-event discord event)
    (assert (event-dispatch? event))
    (let ((type (string->symbol (string-downcase (json-key event "t"))))
          (content (json-key event "d")))
        (case type
            ((ready)
                (begin
                    (display (string "Logged in as " (json-key content "user" "username")))
                    (set! *session-id* (json-key content "session_id"))
                    (set! *resume-gateway-url* (json-key content "resume_gateway_url"))
                    (display (string "Session Id " (json-key content "session_id")))
                    '())) ;; Return ()
            ((guild_create) 
                (begin
                    (display (string "We are in the server " (json-key content "name")))
                    '())) ;; Return ()
            ((message_create)
                (let ((user-name (json-key content "author" "global_name"))
                      (user-id (json-key content "author" "id"))
                      (content (json-key content "content"))
                      (chat-id (json-key content "channel_id")))
                    (display (format #false "~A says \"~A\"!" user-name content))
                    (make-discord-chat-event (%user:make user-id user-name) chat-id content)))
            (else 
                (begin
                    (display (string "Unimplemented dispatch event type " type))
                    '()))))) ;; Return ()

;; Returns: (result, socket)
(define (discord-handle-event discord-socket)
    (if (websocket-connected? discord-socket)
        (let ((json (websocket-next-json-blocking discord-socket)))
            (if json
                (cons (discord-handle-json-event discord-socket json) discord-socket)
                (cons '() discord-socket)))
        (begin
            (display "The connection to discord got dropped! Reconnecting...!") ;; TODO: make sure to stay under 1000 quota
            (cons '() (discord-setup (get-from-env "discord-token")))))) ;; TODO: use configured token



(define (discord-setup token)
    (let ((discord-socket (websocket-connect! "wss://gateway.discord.gg/?v=10&encoding=json"
                                           '()
                                           '("--close-status-code" "1000"))))
         (discord-handle-event discord-socket)    ;; 'Hello' message
         (discord-send-heartbeat! discord-socket) ;; Heartbeat (handle first one manually)
         (discord-handle-event discord-socket)    ;; Receive heartbeat
         (discord-identify! discord-socket token) ;; Identify the bot
         (discord-handle-event discord-socket)
         (pp "[discord-setup] Starting heartbeat loop")
         (discord-start-keepalive-loop discord-socket)   ;; Automatically heartbeat
         (pp "[discord-setup] Heartbeat loop started")
         discord-socket))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Message Handling ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type <discord-chat-event>
  (%discord-chat-event:make sender chat platform content timestamp)
  discord-chat-event?
  (sender discord-chat-event:sender)
  (chat discord-chat-event:chat)
  (platform discord-chat-event:platform)
  (content discord-chat-event:content)
  (timestamp discord-chat-event:timestamp))

(define-record-type <user>
    (%user:make id name)
    user?
    (id user:id)
    (name user:name))

(define (make-discord-chat-event sender chat-id content)
    (let ((event (make-event 'discord '())))
        (%discord-chat-event:make 
            sender ;; Should be a user
            (make-identifier 'discord chat-id) 
            (event-platform event) 
            content 
            (event-timestamp event))))

;; Generic getters:
(define-generic-procedure-handler generic-event-platform
    (match-args discord-chat-event?)
    (lambda (event)
        (discord-chat-event:platform event)))

(define-generic-procedure-handler event-chat
    (match-args discord-chat-event?)
    (lambda (event)
        (discord-chat-event:chat event)))

(define-generic-procedure-handler event-sender
    (match-args discord-chat-event?)
    (lambda (event)
        (discord-chat-event:sender event)))

(define-generic-procedure-handler message-content
    (match-args discord-chat-event?)
    (lambda (event)
        (discord-chat-event:content event)))

;;; Generic predicates:

;;; Chat event: any event coming from a chat
(define-generic-procedure-handler chat-event?
    (match-args discord-chat-event?) 
    (lambda (event) #t))

;;; Bridged event: chat event coming from the bridge 
(define (discord-bridged-event? event)
    (discord-chat-event:sender event))

(define-generic-procedure-handler bridged-event? ;; Need to store bot user somewhere
    (match-args discord-chat-event?)
    (lambda (event) #f)) ; TODO: Fix when writing is enabled to avoid rate limiting

;;; Message event: chat event with a message payload
(define-generic-procedure-handler message-event?
    (match-args discord-chat-event?)
    (lambda (event) #t))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Queue Interface  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type discord-i
    (discord-i:make-record tx-queue rx-queue token)
    discord-i?
  (tx-queue discord-i:tx-queue)
  (rx-queue discord-i:rx-queue)
  (token discord-i:token)) ;; TODO: somehow link the queue stuff to messaging/inner logic or make it a bridge like manta

(define (discord-i:make token)
    (discord-i:make-record (queue:make) (queue:make) token))

(define (discord-i:loop discord-i)
    (let ((discord-socket (discord-setup (discord-i:token discord-i)))
          (tx-queue (discord-i:tx-queue discord-i)))
        (define (discord-i:loop discord-inner-socket)
            (let ((next (discord-handle-event discord-inner-socket)))
                (pp (list "[discord-i:loop] Discord Received:" next))
                (let ((next-json (car next))
                      (next-socket (cdr next)))
                      (unless (equal? next-json '())
                        (queue:add-to-end! tx-queue next-json))
                      (discord-i:loop next-socket)))) ;; Loop
        (pp (list "[discord-i:loop] Starting discord loop"))
        (discord-i:loop discord-socket))) ;; Start loop

(define (discord-i:start! discord-i)
    (pp "Starting discord")
    (create-thread
        #f
        (lambda ()
            (discord-i:loop discord-i))
        "Discord Loop"))

(define (discord-i:read! discord-i)
    ;  (pp "Reading discord")
    (if (queue:empty? (discord-i:tx-queue discord-i))
        '()
        (queue:get-first! (discord-i:tx-queue discord-i))))

(define (discord-i:write! discord-i event)
    (pp (list "Discord writing" event)))
;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bridge interface ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generic predicate
(define discord? (platform-predicate 'discord))
; (define (discord? lst)
;     (equal? (car lst) 'discord))

;;; Configuration
(define discord-config:token
  (make-property 'bot-token 'predicate string?)) ;; In config should use "bot-token"

(define discord-config?
    (make-type 'discord-config
        (list discord-config:token)))

(register-config-constructor! 'discord discord-config?)

(define get-discord-token
    (property-getter discord-config:token discord-config?))

;;; Bridge constructor
(define (make-discord! config)
    (write-line (list "A discord client has been created with config:" config))
    (let ((discord-interface (discord-i:make (get-discord-token config))))
        (lambda (message)
        (case message
            ((get-platform-id) 'discord)
            ((get-config) config)
            ((get-interface) discord-interface)
            (else (error "not implemented"))))))
    
(define-generic-procedure-handler make-client!
    (match-args discord-config?)
    make-discord!)


;;; Interface Operations

;;; Hack to coerce first argument into the interface
(define (wrap-interface fn)
    (lambda args
        (apply fn (cons ((car args) 'get-interface) (cdr args)))))

(define-generic-procedure-handler start-client!
    (match-args discord?)
    (wrap-interface discord-i:start!))

(define-generic-procedure-handler read-client! ;; TODO: filter such that only events from bridged chats are sent out (not all that bot is linked to)
    (match-args discord?)
    (wrap-interface discord-i:read!))

(define-generic-procedure-handler write-client!
    (match-args discord? discord-chat-event?) ; TODO: rethink this
    (wrap-interface discord-i:write!))

; (load-relative "platforms/discord.scm")
; (define sck (discord-setup (get-from-env "discord-token")))