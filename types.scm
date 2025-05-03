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

;;; Config

;;; Authentication / Authorization

;; I don't like this

(define base-auth?
  (make-type 'base-auth (list)))
(define no-auth
  (type-instantiator base-auth?))

;; Token-based configuration
(define token-auth:token
  (make-property 'token
		 'predicate string?))
(define token-auth?
  (make-type 'token-auth (list token-auth:token)))
(set-predicate<=! token-auth? base-auth?)

;; We could add a username/password based configuration if we need it.

;;; Overall configuration

;; STRETCH GOAL: This could be overriden to have more than one instance per app.
(define base-config:name
  (make-property 'name
		 'predicate symbol?))
(define base-config:auth
  (make-property 'auth
		 'predicate base-auth?
		 'default-supplier no-auth))
(define base-config?
  (make-type 'base-config (list base-config:name)))

;;; Platform-specific configs (TODO: move to their own file in platforms/ to make them self-contained)

(define mattermost-config?
  (make-type 'mattermost-config (list)))
;; TODO: what if I want to mandate that auth must be token-auth and not base-auth??
;;   easy with OOP
;; Or what if I want to set 'name to 'mattermost every time? it becomes a wrapper yeah
;; I think we can actually make mattermost a subtype of token and so on...

;;; TODO think of a better way of getting the constructor


(set-predicate<=! mattermost-config? base-config?)
