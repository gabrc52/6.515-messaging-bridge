;; Create the actual platform object based on the config
(define make-client!
  (most-specific-generic-procedure 'make-client!
				   1
				   #f))

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
