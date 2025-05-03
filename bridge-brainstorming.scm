;;;; Brainstorming
;; TODO: I am trying to determine if make-property and make-type from the adventure game would be useful

;;;; Identifiers

identifier?
;; Probably looks something like this 
('identifier 'discord "820354450073714688")

;; And we can resolve an identifier into the right type of user, message, chat, etc
(resolve-identifier identifier)
;; Get the identifier from an object
(user-identifier user)

;; We should also be able to resolve the platform
(get-platform user) ;; 'discord
(get-platform user-id) ;; 'discord

;; We can consider making identifiers and the things that they point to interchangeable
;; (if we have a function that takes a user but we give it an identifier, it could transparently get the user)

;;;; Store/Configuration
(define config (parse-config "config.txt"))
;; This could populate some hash tables or something

;; Either is fine, we can get all chats that are bridged
(config-get-equivalent-chats chat)
(config 'get-equivalent-chats chat)

;;;; Receiver and sender

(define discord (make-discord))

;; TODO the receiver and the sender are tied to each other on Signal (use same socket), so we may not be able to keep as separate abstractions

;; These should probably be global
(define discord-sender (discord 'make-sender))

;; TODO: think of the way the dispatch store was implemented. JSON should be just one possible type

;; Makes a websocket client that starts receiving messages
;; TODO: but maybe we instead want a make thread
(define discord-receiver (discord 'make-receiver!))

;; The sender is probably just a function (yea, looks good)
(discord-sender message chat user)

;; The receiver may or may not have an event
(has-event? discord-receiver)
(get-event! discord-receiver)

;; TODO: for the threaded one, make platforms able to do low level things (some events are low level events that need something to happen)

;; Stops receiving messages
(receiver-close! discord-receiver)

;; The returned thing should satisfy:
event?
;; which may be a message received, or a non-message thing (someone is online, a channel was created, or something more low-level

;; There should be sub-types of event:
message-event? typing-event? reaction-event?
;; others we don't need to implement for a messaging bridge

;; We need a predicate for every event that belongs to a specific chat (as opposed to a DM, channel creation, etc)
chat-event?

;; the other ones are subtypes of chat event, I guess
;; These predicates are generic procedures themselves! (implementation depends on platform)
(set-predicate<=! message-event? chat-event?)
(set-predicate<=! typing-event? chat-event?)
(set-predicate<=! reaction-event? chat-event?)

;;;; Messaging logic

(define (send-message! chat message #!optional sender)
  (make-generic-procedure ...))

;; Handle an event, may be a message, etc
;; Platform agnostic
(define (handle-event! event)
  ;; Instead of an if statement, it can be a generic procedure, dispatched on chat-event?
  (if (and (chat-event? event)
	   (is-bridged-event? event)) ;; Crucial
      (let ((platform (event-platform event)) ;; Not sure if we need this
	    (chat (event-chat event)))
	(let ((equivalent-chats (config 'get-equivalent-chats chat)))
	  (for-each (lambda (other-chat)
		      ;; TODO: we can later handle other things such as typing, reactions, etc
		      (send-message! other-chat event sender))
		    equivalent-chats)))))

