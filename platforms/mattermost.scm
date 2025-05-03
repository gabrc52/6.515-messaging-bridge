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
  (match-args (platform-is? 'mattermost))
  (lambda (platform) make-mattermost-config))

;;; Bridge constructor
;;(define (make-mattermost config)
;;  ...)
