;;; Common bridge stuff (maybe move to a file called common.scm)

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



;;; Bridge implementation
