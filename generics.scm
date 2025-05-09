;; Create the actual platform object based on the config
(define make-client!
  (simple-generic-procedure 'make-client! 1 #f))

;; They are all message-accepting procedures which would accept this message
(define (client-platform client) (client 'get-platform-id))

;;; Predicates for event types
;;; They are platform-specific generic procedures.

;; A chat event belongs to a chat, rather than something more general or low-level
(define chat-event?
  (simple-generic-procedure 'chat-event 1 #f))

(register-predicate! chat-event? 'chat-event)

(define typing-event?
  (simple-generic-procedure 'typing-event 1 #f))
(register-predicate! typing-event? 'typing-event)

(define message-event?
  (simple-generic-procedure 'message-event 1 #f))
(register-predicate! message-event? 'message-event)

(define reaction-event?
  (simple-generic-procedure 'reaction-event 1 #f))
(register-predicate! reaction-event? 'reaction-event)

(set-predicate<=! chat-event? event?)
(set-predicate<=! message-event? chat-event?)
(set-predicate<=! reaction-event? chat-event?)
(set-predicate<=! typing-event? chat-event?)

;; Not a distinct disjoint sub-type, but necessary to determine whether to bridge or not, and avoid loops.

(define bridged-event?
  (simple-generic-procedure 'bridged-event? 1 #f))
(register-predicate! bridged-event? 'bridged-event)

;; Maybe we want to define DM-only commands, who knows. I don't anticipate needing this.
(define in-direct-message?
  (simple-generic-procedure 'in-direct-message? 1 #f))
(register-predicate! in-direct-message? 'in-direct-message)

;;; Getters for chat events

(define event-chat ;; mapping from <event> to a chat identifier
  (simple-generic-procedure 'event-chat 1 #f))

;; For now, this is a human-readable representation (display name for showing in bridged messages)
;; Later on, we might want event-sender-id and event-sender-username too
(define event-sender
  (simple-generic-procedure 'event-sender 1 #f))

(define message-content
  (simple-generic-procedure 'message-content 1 #f))

;; TODO: define others as needed

(define start-client!
    (simple-generic-procedure 'start-client! 1 #f))

(define read-client!
    (simple-generic-procedure 'read-client! 1 #f))

(define write-client!
    (simple-generic-procedure 'write-client 2 #f))
