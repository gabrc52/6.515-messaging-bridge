;;; Config parsing

(define (read-sexp-from-file file)
  (call-with-input-file file
    (lambda (port)
      (read port))))

;; Parse an identifier like `(discord "787146644264189975")`
(define (parse-identifier identifier)
  (define (%parse-identifier platform . args)
    (let ((arg-or-args
	   (if (= (length args) 1)
	       (first args)
	       args)))
      (make-identifier platform arg-or-args)))
  (guarantee list? identifier)
  (assert (> (length identifier) 1))
  (apply %parse-identifier identifier))

;; Parse a set of linked chats like
;; (linked (discord "787146644264189975")
;;	   (discord "820354450073714688")
;;	   (slack "C08PR8QQU93"))
;; Output: A list of <identifier>
(define (parse-linked-chats linked-chats)
  (guarantee (list-beginning-with? 'linked) linked-chats)
  (map parse-identifier (cdr linked-chats)))

;; Parses a list of sets of linked chats like (bridge (linked ...) (linked ...) ...)
(define (parse-bridge linked-chats-list)
  (guarantee (list-beginning-with? 'bridge) linked-chats-list)
  (map parse-linked-chats (cdr linked-chats-list)))

;; Parse platform-specific options like
;;(discord
;; token "my token"
;; custom-status "I am a bot!")
(define (parse-platform platform-options)
  (define (%parse-platform platform . plist)
    (let ((constructor (get-platform-config-constructor platform)))
      (apply constructor plist)))
  (apply %parse-platform platform-options))
  
;; Parse the options specific to a platform 
(define get-platform-config-constructor
  (most-specific-generic-procedure 'parse-platform-config
				   1
				   #f))

;; TODO: maybe move these to a different file

;;; Hash table of linked chats
(define *linked-chats* (make-hash-table identifier-comparator))
(define (linked-chats-get identifier)
  (hash-table-ref *linked-chats* identifier (lambda () (list))))
(define %identifier-member (member-procedure identifier=?))
;; "Although they are often used as predicates, memq, memv, and member do not have question marks in their names because they return useful values rather than just #t or #f."
(define identifier-member?
  (lambda (obj l) (not (not (%identifier-member obj l)))))
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

(define (load-platforms! config-platforms)
  
  ...)

(define (load-config! config)
  (guarantee (list-beginning-with? 'config) config)
  ;; The first element of config must be the linked chats, then all the platform-specific options
  (let ((config-bridge (cadr config))
	(config-platforms (cddr config)))
    (load-linked-chats! (parse-bridge config-bridge))
    ;;(for-each (lambda 
    
    ;;(for-each load-platform! config-platforms)))
    
    ;;(for-each get-platform-config-constructor
    
    ;; TODO: initialize platforms
    ))

(define (load-config-file! file)
  (load-config! (read-sexp-from-file file)))
  
