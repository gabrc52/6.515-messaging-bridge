;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Discord Internals ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Opcodes ;;;
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

(define (discord-op discord-json)
    (json-key discord-json "op"))

;; Op predicates
(define (make-event-predicate op)
(lambda (event)
    (= (discord-op event) op)))
(define event-heartbeat? (make-event-predicate op-heartbeat))
(define event-heartbeat-ack? (make-event-predicate op-heartbeat-ack))
(define event-dispatch? (make-event-predicate op-dispatch))
(define event-hello? (make-event-predicate op-hello))

;;; Intents ;;;
(define << shift-left)
(define (get-intent-number intent-list)
    (reduce bitwise-ior 0 intent-list))

(define *intents*
    (get-intent-number
        (list
            (<< 1 0)     ;; GUILDS
            (<< 1 1)     ;; GUILD_MEMBERS
            (<< 1 9)     ;; GUILD_MESSAGES
            (<< 1 15)    ;; MESSAGE_CONTENT
            (<< 1 10)    ;; GUILD_MESSAGE_REACTIONS
            (<< 1 11)    ;; GUILD_MESSAGE_TYPING
            (<< 1 12)    ;; DIRECT_MESSAGES
            (<< 1 13)    ;; DIRECT_MESSAGE_REACTIONS
            (<< 1 14)))) ;; DIRECT_MESSAGE_TYPING

;;; Global Constants ;;;
;; (Set up with the connection)
(define *resume-gateway-url*)
(define *session-id*)           
(define *heartbeat-interval*)    
(define *heartbeat-number* null)
(define *bot-id* (get-from-env "bot-id"))

;;; Heartbeats ;;;
(define (make-heartbeat number)
    `(dict ("op" . ,op-heartbeat)
        ("d" . ,number)))

(define (discord-send-heartbeat! discord-socket)
    (websocket-send-json discord-socket (make-heartbeat *heartbeat-number*)))

;; Heartbeat Loop
(define (discord-keepalive-fetch last-ts) ;; TODO: generalize
    (if (> (- (get-time-ms) last-ts) *heartbeat-interval*)
        (cons #t get-time-ms)
        (cons #f last-ts)))
;; Returns (interval-passed? last-interval-started-time)

(define (discord-keepalive-loop discord-socket ts)
    (when (websocket-connected? discord-socket)
        (let ((fetched (discord-keepalive-fetch ts)))
            (let ((ready (car fetched))
                (next-ts (cdr fetched)))
                (when ready
                    (begin
                        (pp "[discord-keepalive-loop] Sending heartbeat!")
                        (discord-send-heartbeat! discord-socket)))
                (discord-keepalive-loop discord-socket next-ts)))))

(define (start-discord-keepalive-loop discord-socket)
    (create-thread
        #f
        (lambda ()
            (discord-keepalive-loop discord-socket 0))
        "Discord Keepalive Loop"))

;;; Identify ;;;
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
                    (display (string "Logged in as " (json-key content "user" "username") "\n"))
                    (set! *session-id* (json-key content "session_id"))
                    (set! *resume-gateway-url* (json-key content "resume_gateway_url"))
                    ; (set! *bot-id* (json-key content "user" "id"))
                    (display (string "Session Id " (json-key content "session_id") "\n"))
                    '())) ;; Return ()
            ((guild_create) 
                (begin
                    (display (string "We are in the server " (json-key content "name") "\n"))
                    '())) ;; Return ()
            ((message_create)
                (let ((user-name (json-key content "author" "global_name"))
                      (user-id (json-key content "author" "id"))
                      (content (json-key content "content"))
                      (chat-id (json-key content "channel_id")))
                    (display (format #f "~A says \"~A\"!" user-name content))
                    event))
            (else 
                (begin
                    (display (string "Unimplemented dispatch event type " type))
                    '()))))) ;; Return ()

;;;;;;;;;;;;;;;;;;
;;; Interfaces ;;;
;;;;;;;;;;;;;;;;;;

(define discord? (platform-predicate 'discord))
(register-predicate! discord? 'discord)
(define discord-config:token
  (make-property 'token
		 'predicate string?))
(define discord-config?
  (make-type 'discord-config (list discord-config:token)))
(set-predicate<=! discord-config? platform-config?)
(register-config-constructor! 'discord discord-config?)
(define get-discord-token (property-getter discord-config:token discord-config?))

(define (make-discord! config)
    (pp (list "Creating a discord client with config:" config))
    (define discord:tx-queue (queue:make))
    (define discord:rx-queue (queue:make))
    (define discord:token (get-discord-token config))


    ;; Setup websocket connection ;;
    (define discord:socket (websocket-connect! "wss://gateway.discord.gg/?v=10&encoding=json"
                                           '()
                                           '("--close-status-code" "1000")))

    (define delegate (make-port-based-client (websocket-port discord:socket))) ;; TODO: also have(make-port-http-client) for handling sending http messages
    (define receiver (delegate 'raw-event-receiver))
    (define sender (delegate 'raw-event-sender))

    (define (%receive-raw-event!)
        (when-available
            (receiver)
            (lambda (json)
            ;    (pp (list "handling event" json))
                (if (json-dict? json)
                    (let ((result (discord-handle-json-event discord:socket json)))
                        (if (equal? result '())
                            #f
                            result))
                #f))))

    (%receive-raw-event!)                            ;; 'Hello' message
    (discord-send-heartbeat! discord:socket)         ;; Heartbeat (handle first one manually)
    (%receive-raw-event!)                            ;; Receive heartbeat
    (discord-identify! discord:socket discord:token) ;; Identify the bot
    (%receive-raw-event!) 

    (start-discord-keepalive-loop discord:socket)    ;; Start heartbeating
    ;;-------- End setup -------- ;;

    (define (%send-message chat-id text)
        (curl-http-request
            "POST"
            (string-append "https://discord.com/api/v10/channels/" chat-id "/messages")
            ""
            (list (cons "Authorization" (string-append "Bot " discord:token)) (cons "Content-Type" "application/json"))
            (jsexpr->string `(dict ("content" . ,text)))))
    
    (lambda (op)
        (case op
            ((get-platform-id) 'discord)
            ((raw-event-receiver) %receive-raw-event!)
            ((message-sender) %send-message)
            (else (delegate op)))))

(define-generic-procedure-handler make-client! (match-args discord-config?) make-discord!)

(define-generic-procedure-handler chat-event?
  (match-args discord?) 
  (lambda (event)
    (write-line "We are at Discord chat-event?")
    ; (pp ("[chat-event?]" event))
    (and (json-dict? (event-body event)) (event-key? event "op") (equal? (event-key event "op") op-dispatch))))

(define-generic-procedure-handler message-content
  (match-args discord?)
  (lambda (event)
    ; (pp ("[message-content]" event))
    (guarantee message-event? event)
    (event-key event "d" "content")))

(define-generic-procedure-handler message-event?
  (match-args discord?)
  (lambda (event)
    ; (pp ("[message-event?]" event))
    (and (chat-event? event) 
	 (equal? (event-key event "t") "MESSAGE_CREATE"))))

(define-generic-procedure-handler bridged-event?
  (match-args discord?)
  (lambda (event)
    ; (pp ("[bridged-event?]" event))
    (and (message-event? event)
        (equal? (event-key event "d" "author" "id") *bot-id*))))

(define-generic-procedure-handler event-chat
  (match-args discord?)
  (lambda (event)
    ; (pp ("[event-chat]" event))
    (guarantee message-event? event) 
    (make-identifier 'discord 
		    (event-key event "d" "channel_id"))))

(define-generic-procedure-handler event-sender
  (match-args discord?)
  (lambda (event)
    ; (pp ("[event-sender]" event))
    (guarantee message-event? event)
    (event-key event "d" "author" "global_name")))
