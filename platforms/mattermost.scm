;;; Generic predicate
(define mattermost? (platform-predicate 'mattermost))
(register-predicate! mattermost? 'mattermost)

;;; Config format
(define mattermost-config?
  (make-type 'mattermost-config
	     (list http-platform-config:base-url platform-config:access-token)))
(set-predicate<=! mattermost-config? http-platform-config?)
(register-config-constructor! 'mattermost mattermost-config?)

;;; Bridge constructor
(define (make-mattermost! config)
  (write-line "A mattermost client has been created")
  (lambda (message)
    (case message
      ((get-platform-id) 'mattermost)
      (else (error (list "Not implemented:" message))))))
(define-generic-procedure-handler make-client!
  (match-args mattermost-config?)
  make-mattermost!)
