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

(define (load-linked-chats! list-of-bridges)
  (hash-table-clear! *linked-chats*)
  (for-each link-chats! list-of-bridges))

;;; Message receiving and sending

(define *message-format* "[~A] ~A") ;; TODO(stretch): this can go in the config file
(define (format-bridged-message message)
  ;; HERE is where generics do shine. Reading the properties does not require constructing an instance
  ;;   of a record type or similar, and is a platform-specific implementation.
  (format #f *message-format* (event-sender message) (message-content message)))

;; Sending messages (UNUSED - remove?)
(define (send-message! message chat)
  ;; This client may be wrong. We want the platform of the platform to bridge *to*, not *from*.
  (let ((client (get-client (generic-event-platform message))))
    ;; TODO: it's supposed to use the chat
    (write-client! client message)))

(define (bridge-message! message chat)
  (let ((recipient-client (get-client (identifier-platform chat))))
    ;; TODO: not all platforms would want format-bridged-message. This is for proof of concept.
    ;;   Discord (via webhooks), Zephyr, Mattermost, etc let you override the sender username.
    ;;   So each platform should be allowed to implement their own version of this! (generic)
    ((recipient-client 'message-sender) (identifier-id chat) (format-bridged-message message))))

(define (%default-event-handler event)
  (display (string "Unimplemented " (event-platform event) " event: " event)) (newline))

(define (reply! message reply-text)
  (let* ((client (get-client (generic-event-platform message)))
	 (sender (client 'message-sender)))
    ;; TODO: for succintness it would be nice if the sender took either an identifier or a chat
    (sender (identifier-id (event-chat message)) reply-text)))

(define (handle-commands! message)
  (let ((content (message-content message)))
    (if (equal? content "!which-chat")
	(begin
	  (reply! message (string (identifier-id (event-chat message))))
	  #t)
	#f)))

(define (inspect-event event)
    (pp (list "Classifying event:" event))
    (pp (list "Event?" (event? event)))
    ;; (pp (list "Event Body:" (event-body event)))
    (pp (list "Chat event?" (chat-event? event)))
    (pp (list "Bridge event?" (bridged-event? event)))
    (pp (list "Message event?" (message-event? event))))
;;; High-level event handler
;; (define handle-event! (chaining-generic-procedure 'handle-event! 1 #f))
;; (define handle-event! (most-specific-generic-procedure 'handle-event! 1 %default-event-handler))
;; TODO: the generic procedure did not seem to be working right?
;;   It calls the default handler even if the predicate is true. Very strange.
;;   See 75fc6e08e17c2c8c827a33c7e0ef9eed60a49989 if you wish to restore the generic procedure.
(define (handle-event! event)
  ;; (inspect-event event) ;; For debugging
  (if (chat-event? event)
      (unless (bridged-event? event) ;; Crucial to avoid infinite loops
	(when (message-event? event)
	  (unless (handle-commands! event)
            (let* ((chat (event-chat event))
		   (equivalent-chats (linked-chats-get chat)))
              (for-each (lambda (other-chat)
			  (bridge-message! event other-chat))
			equivalent-chats)))))
      (%default-event-handler event)))

