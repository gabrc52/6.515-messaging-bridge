;;; Common bridge types

;;; Identifiers

(define-record-type <identifier>
  (make-identifier platform id)
  identifier?
  (platform identifier-platform)
  (id identifier-id))

(define-print-method identifier?
  (lambda (identifier port)
    (display "[" port)
    (display (identifier-platform identifier) port)
    (display " " port)
    (display (identifier-id identifier) port)
    (display "]" port)))

;; NOTE: we could easily implement an equality operator for SDF custom type system, but this is a record type
(define (identifier=? identifier1 identifier2)
  (and (eqv? (identifier-platform identifier1) (identifier-platform identifier2))
       (equal? (identifier-id identifier1) (identifier-id identifier2))))

;; TODO: cache this?
(define (identifier-hash identifier)
  (string-hash
   (string (identifier-platform identifier) ":" (identifier-id identifier))))

;; For hash tables
(define identifier-comparator
  (make-comparator identifier? identifier=? #f identifier-hash #f))

;; MAY NEED resolve-identifier
;; MAY NEED user-identifier etc
;; MAY NEED generic get-platform?

;;; Platform-based config

;; STRETCH GOAL: Identifiers should be associated with a name, and platform-specific configs would
;;   also need an arbitrary name (default to platform name). This way, we can also have more than one
;;   instance of a specific platform (for siloed platforms such as Slack and Mattermost where 2 different
;;   hosted instances or workspaces are completely separate including usernames, tokens, etc.)
;;   Since it is such an uncommon use case, we are fine foregoing some flexibility here. If I ever decide
;;   to implement it, I am hoping the refactoring won't be too bad since I kept it in mind when implementing.
;; ACTUALLY: We would be able to wrap everything without needing to do modifications. We can programmatically
;;   create more platforms such as slack1, slack2, etc. along with constructors/generic procedures/etc.

;;; Platforms may want to use these (they are reusable, even if not in the base platform-config)

;; (Usernames are useful whether there is a password or not, API endpoints may need it explicitly)
(define platform-config:username
  (make-property 'username
		 'predicate string?))

(define platform-config:password
  (make-property 'password
		 'predicate string?))

;; TODO: we could allow setting the token to some "undefined" option so that it is obtained by a password
(define platform-config:access-token
  (make-property 'access-token
		 'predicate string?))

;; This might be redundant. Redundancy is good, no?
(define platform-config:platform-id
  (make-property 'platform-id
		 'predicate symbol?))

(define platform-config?
  (make-type 'platform-config (list platform-config:platform-id)))

;; Defining getters (for convenience)
(define config-username (property-getter platform-config:username platform-config?))
(define config-password (property-getter platform-config:password platform-config?))
(define config-access-token (property-getter platform-config:access-token platform-config?))
(define config-platform-id (property-getter platform-config:platform-id platform-config?))

;;; For platforms that perform actions using HTTP(S) requests.

;; For self-hosted open-source messaging platforms, maybe even for Slack?
;; e.g. https://matrix-synapse.mit.edu, https://mattermost.mit.edu
;; And we can hardcode it for other apps
;; (Discord uses a different websocket baseurl but there is an endpoint to fetch it, so that is fine.)
(define http-platform-config:base-url
  (make-property 'base-url
		 'predicate string?))
;; TODO: maybe we want a better URL-specialized predicate
;; NOTE: The SDF implementation of user-defined types doesn't actually seem to use the predicate to perform
;;   validation when constructing. Or hmm. It should. I think it did during the adventure game with bias?
;; TODO: check the user-defined-types code / ask gjs

(define http-platform-config?
  (make-type 'http-platform-config (list http-platform-config:base-url)))

;; Getter
(define config-base-url (property-getter http-platform-config:base-url http-platform-config?))

(set-predicate<=! http-platform-config? platform-config?)

;;; We need a way to set default values for subtype constructors, like on OOP.

;; Defaults is a plist
(define (type-instantiator-with-defaults type defaults)
  ;; This does not check for duplicates, but I'd blame the "plist" parser.
  ;;   It is probably fine because we append the defaults to the front, so if a property is repeated
  ;;   in the plist, it is effectively ignored as expected.
  (lambda plist
    (apply (type-instantiator type) (append defaults plist))))

;;; Events

;; If record types are too constraining, switch to user-defined types from SDF.
;; But I anticipate they are all a platform and a body, maybe a timestamp

(define-record-type <event>
  (%make-event platform body timestamp)
  event?
  (platform event-platform)
  (body event-body)
  ;; Time this was instantiated, not received
  (timestamp event-timestamp))

(register-predicate! event? 'event)

(define (make-event platform body)
  (%make-event platform body (get-universal-time)))

;; TODO: Remove if unused
;; discord? is too broad. We want to have discord-event? <= event? and so on
;; Actually we want subtypes in a different way so hmmm think about it
;; No, I think we can just use event-platform, we don't want per-platform subtypes
(define (make-event-predicate platform)
  (lambda (event)
    (and (event? event)
	 (platform-ids-equal? (event-platform event) platform))))

