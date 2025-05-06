;; Create the actual platform object based on the config
(define make-client!
  (most-specific-generic-procedure 'make-client! 1 #f))

;; They are all message-accepting procedures which would accept this message
(define (client-platform client) (client 'get-platform-id))

;;; Hash table of linked chats
(define *linked-chats* (make-hash-table identifier-comparator))
(define (linked-chats-get identifier)
  (hash-table-ref *linked-chats* identifier (lambda () (list))))
(define identifier-member (member-procedure identifier=?))
;; "Although they are often used as predicates, memq, memv, and member do not have question marks in their names because they return useful values rather than just #t or #f."
(define identifier-member?
  (lambda (obj l) (not (not (identifier-member obj l)))))
;; Whether 2 chats are linked
(define (linked-chats? identifier1 identifier2)
  (let ((linked (linked-chats-get identifier1)))
    (identifier-member? identifier2 linked)))
;; Whether chat is linked to any chats at all (yet)
(define (chat-registered? identifier)
  (not (null? (linked-chats-get identifier))))
(define (linked-chats-add! identifier1 identifier2)
  (define (%append! key value)
    (hash-table-set! *linked-chats* key (cons value (linked-chats-get key))))
  (%append! identifier1 identifier2))
;; May help debugging: (hash-table->alist *linked-chats*)
;;; End hash table logic

(define (link-chats! chat-list)
  ;; Pre-emptively avoiding infinite loops by validating user input
  (for-each (lambda (identifier)
	      (when (chat-registered? identifier)
		(error "A chat may only show up on one list, please consolidate the lists. Culprit:"
		       identifier)))
	    chat-list)
  ;; Actually link the chats
  ;; Bridging is bidirectional, so pairs in both sides are included
  (for-all-different-pairs chat-list linked-chats-add!))

;; ASSUMPTION: I am assuming that the functions to link chats are only called when doing initialization
;;   while reading the config file, which is when calling the below function.
;;   But since the linked chats are a global variable, they can probably be called later too.
;;   (But this does mean no guarantee they would work fine with concurrency or threads).

(define (load-linked-chats! list-of-bridges)
  (hash-table-clear! *linked-chats*)
  (for-each link-chats! list-of-bridges))

;;; Predicates for event types
;;; They are platform-specific generic procedures.

;; A chat event belongs to a chat, rather than something more general or low-level
;; TODO: first thing if something does not work try omitting the ? in the symbols below
(define chat-event?
  (most-specific-generic-procedure 'chat-event? 1 #f))
(register-predicate! chat-event? 'chat-event)

(define message-event?
  (most-specific-generic-procedure 'message-event? 1 #f))
(register-predicate! message-event? 'message-event)

(define reaction-event?
  (most-specific-generic-procedure 'reaction-event? 1 #f))
(register-predicate! reaction-event? 'reaction-event)

(set-predicate<=! chat-event? event?)
(set-predicate<=! message-event? chat-event?)
(set-predicate<=! reaction-event? chat-event?)

;; Not a distinct disjoint sub-type, but necessary to determine whether to bridge or not, and avoid loops.

(define bridged-event?
  (most-specific-generic-procedure 'bridged-event? 1 #f))
(register-predicate! bridged-event? 'bridged-event)

;; Maybe we want to define DM-only commands, who knows. I don't anticipate needing this.
(define in-direct-message?
  (most-specific-generic-procedure 'in-direct-message? 1 #f))
(register-predicate! in-direct-message? 'in-direct-message)

;;; Getters for chat events
(define generic-event-platform
  (most-specific-generic-procedure 'event-chat 1 #f))

(define event-chat ;; mapping from <event> to a chat identifier
  (most-specific-generic-procedure 'event-chat 1 #f))

(define event-sender
  (most-specific-generic-procedure 'event-sender 1 #f))

(define message-content
  (most-specific-generic-procedure 'message-content 1 #f))

;; TODO: define others as needed

;;; Message receiving and sending

;; Sending messages
(define (send-message! message chat #!optional sender)
  ;; TODO: implement, most likely implementation is to find the client and then pass a message
  ;;   Alternatively, it could be a generic procedure.
  (let ((client (cdr (assoc (generic-event-platform message) *all-clients*))))
    (write-client! client message)))
  

;; NOTE: because some of the event handlers are stateful, e.g. Discord has:
;;  (let ((sequence-number (json-key event "s")))
;;    (set! *heartbeat-number* sequence-number))
;; At least some sort of event handler needs to be put in the client / message-accepting procedure
;; since it does have access to local state.

;;; Low-level event handler
(define handle-raw-event! (chaining-generic-procedure 'handle-raw-event! 2 #f)) ;; Takes platform ID and event

;;; High-level event handler
; (define handle-event! (chaining-generic-procedure 'handle-event! 1 #f))
(define handle-event! (most-specific-generic-procedure 'handle-event! 1 #f))

;; TODO: it is likely that we need to implement a very similar type of chaining generic procedure
;;   BUT in the other direction (from supertype to subtype rather than the opposite)
;;   Don't do this until we must, though. (Copy the existing implementation but reverse the order.)

(define-generic-procedure-handler handle-event!
  (match-args chat-event?)
;   (lambda (super event)
;     (super event)
  (lambda (event)
    (unless (bridged-event? event) ;; Crucial to avoid infinite loops
      ;; TODO: we can avoid this condition by doing the above, calling the subtype's handler here,
      ;;   and defining a handler for message-event?
      (when (message-event? event)
        (let ((chat (event-chat event))
            (sender (event-sender event)))
        (let ((equivalent-chats (linked-chats-get chat)))
            (for-each (lambda (other-chat)
                ;; It is a different platform, but generics should take care of it :D
                (send-message! event other-chat sender))
                equivalent-chats)))))))

;; TODO: is one platform-agnostic and one platform-specific?
;; I think it has to be either one or the other so that `super` is defined and makes sense
;; handle-raw-event HAS to be platform-specific
;; but what if we want platform-specific high-level logic?
;; Maybe have a third function then?

(define start-client!
    (most-specific-generic-procedure 'start-client! 1 #f))

(define read-client!
    (most-specific-generic-procedure 'read-client! 1 #f))

(define write-client!
    (most-specific-generic-procedure 'write-client 2 #f))
