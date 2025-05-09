;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dummy Internals  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; tx/rx is from the perspective of the dummy to the bridge

(define-record-type dummy-i
    (dummy-i:make-record tx-queue rx-queue interval)
    dummy-i?
  (tx-queue dummy-i:tx-queue)
  (rx-queue dummy-i:rx-queue)
  (interval dummy-i:interval))

(define (dummy-i:make interval) ;; common
    (dummy-i:make-record (queue:make) (queue:make) interval))

(define (dummy-i:read! dummy-i) ;; common
    (if (queue:empty? (dummy-i:tx-queue dummy-i))
        '()
        (queue:get-first! (dummy-i:tx-queue dummy-i))))

(define (dummy-i:write! dummy-i obj)
    (pp (cons "dummy-i received" obj)))

(define *dummy-count* 0)
(define (get-dummy-count)
    (set! *dummy-count* (+ *dummy-count* 1))
    *dummy-count*)

(define (dummy-i:tx-fetch-value dummy-i last-ts)
    (if (> (- (get-universal-time) last-ts) (dummy-i:interval dummy-i))
        (cons (make-dummy-chat-event "dummy-i" (make-identifier 'dummy "test") (list "dummy test message #" (get-dummy-count))) (get-universal-time))
        (cons '() last-ts)))

(define (dummy-i:tx-loop dummy-i ts)
    (let ((tx-value (dummy-i:tx-fetch-value dummy-i ts))
          (tx-queue (dummy-i:tx-queue dummy-i)))
        (unless (equal? (car tx-value) '())
            (queue:add-to-end! tx-queue (car tx-value)))

        (dummy-i:tx-loop dummy-i (cdr tx-value))))

(define (dummy-i:start! dummy-i) ;; common
  (create-thread
   #f
   (lambda ()
     (dummy-i:tx-loop dummy-i 0))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Message Handling ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <dummy-chat-event>
  (%dummy-chat-event:make sender chat platform body timestamp)
  dummy-chat-event?
  (sender dummy-chat-event:sender)
  (chat dummy-chat-event:chat)
  (platform dummy-chat-event:platform)
  (body dummy-chat-event:body)
  (timestamp dummy-chat-event:timestamp))

(define (make-dummy-chat-event sender chat content)
    (let ((event (make-event 'dummy '())))
        (%dummy-chat-event:make sender chat (event-platform event) content (event-timestamp event))))
        ;; TODO: actually subtype event

(define-generic-procedure-handler event-chat
    (match-args dummy-chat-event?)
    (lambda (event)
        (dummy-chat-event:chat event)))

(define-generic-procedure-handler event-sender
    (match-args dummy-chat-event?)
    (lambda (event)
        (dummy-chat-event:sender event)))

(define-generic-procedure-handler message-content
    (match-args dummy-chat-event?)
    (lambda (event)
        (dummy-chat-event:body event)))

;;; Generic predicates:

;;; Chat event: any event coming from a chat
(define-generic-procedure-handler chat-event?
    (match-args dummy-chat-event?) ;; Needs to be dummy-specific
    (lambda (event) #t))

;;; Bridged event: chat event coming from the bridge 
(define (dummy-bridged-event? event)
    (dummy-chat-event:sender event))

(define-generic-procedure-handler bridged-event?
    (match-args dummy-chat-event?)
    (lambda (event) #f)) ; TODO: link up with chat stuff

;;; Message event: chat event with a message payload
(define-generic-procedure-handler message-event?
    (match-args dummy-chat-event?)
    (lambda (event) #t))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bridge interface ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generic predicate
(define dummy? (platform-predicate 'dummy))

;;; Configuration
(define dummy-config:interval
  (make-property 'interval
		 'predicate number?))

(define dummy-config?
    (make-type 'dummy-config
        (list dummy-config:interval)))

(register-config-constructor! 'dummy dummy-config?)

(define get-dummy-interval
    (property-getter dummy-config:interval dummy-config?))

;;; Bridge constructor
(define (make-dummy! config)
    (write-line (list "A dummy client has been created with config:" config))
    (let ((dummy-interface (dummy-i:make (get-dummy-interval config))))
        (lambda (message)
        (case message
            ((get-platform-id) 'dummy)
            ((get-config) config)
            ((get-interface) dummy-interface)
            (else (error "not implemented"))))))
    
(define-generic-procedure-handler make-client!
    (match-args dummy-config?)
    make-dummy!)


;;; Interface Operations

;;; Hack to coerce first argument into the interface
(define (wrap-interface fn)
    (lambda args
        (apply fn (cons ((car args) 'get-interface) (cdr args)))))

(define-generic-procedure-handler start-client!
    (match-args dummy?)
    (wrap-interface dummy-i:start!))

(define-generic-procedure-handler read-client!
    (match-args dummy?)
    (wrap-interface dummy-i:read!))

(define-generic-procedure-handler write-client!
    (match-args dummy? dummy-chat-event?) ; TODO: rethink this
    (wrap-interface dummy-i:write!))

; (define clt (cdr (assoc 'dummy *all-clients*)))
; (define evt (make-dummy-chat-event "dummy-i" (make-identifier 'dummy "test") (list "dummy test message #" (get-dummy-count))))
; (write-client! clt evt)
