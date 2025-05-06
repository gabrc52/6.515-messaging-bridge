;;; Generic predicate
(define mattermost? (platform-predicate 'mattermost))
; (define (mattermost? lst)
;     (equal? (car lst) 'mattermost))

;;; Config format
(define mattermost-config?
  (make-type 'mattermost-config
	     (list http-platform-config:base-url
		   ;; Let's use tokens for now (TODO: handle username/password)
		   ;; Would it be possible to implement type unions? (either access token or password is set)
		   platform-config:access-token)))
(set-predicate<=! mattermost-config? http-platform-config?)

(define make-mattermost-config
  (type-instantiator-with-defaults mattermost-config?
				   '(platform-id mattermost)))

(define-generic-procedure-handler get-platform-config-constructor
  (match-args mattermost?)
  (lambda (platform) make-mattermost-config))

;;; Bridge constructor
(define (make-mattermost! config)
  (write-line "A mattermost client has been created")
  (lambda (message)
    (case message
      ((get-platform-id) 'mattermost)
      (else (error "not implemented")))))
(define-generic-procedure-handler make-client!
  (match-args mattermost-config?)
  make-mattermost!)
