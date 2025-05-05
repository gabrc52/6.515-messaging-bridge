; (load "queue.scm")

; tx/rx is from the perspective of the dummy to the bridge

(define-record-type dummy-i
    (dummy-i:make-record tx-queue rx-queue interval)
    dummy-i?
  (tx-queue dummy-i:tx-queue)
  (rx-queue dummy-i:rx-queue)
  (interval dummy-i:interval))

(define (dummy-i:make interval)
    (dummy-i:make-record (queue:make) (queue:make) interval))

(define (dummy-i:read! dummy-i)
    (if (queue:empty? (dummy-i:tx-queue dummy-i))
        '()
        (pp (list "Received from Queue:" (queue:get-first! (dummy-i:tx-queue dummy-i))))))

(define (dummy-i:write! dummy-i obj)
    (pp (cons "dummy-i received" obj)))


(define (dummy-i:tx-fetch-value dummy-i last-ts)
    (if (> (- (get-universal-time) last-ts) (dummy-i:interval dummy-i))
        (cons "dummy-i Sent Msg" (get-universal-time))
        (cons '() last-ts)))

(define (dummy-i:tx-loop dummy-i ts)
    (let ((tx-value (dummy-i:tx-fetch-value dummy-i ts))
          (tx-queue (dummy-i:tx-queue dummy-i)))
        (if (equal? (car tx-value) '())
            '()
            (queue:add-to-end! tx-queue tx-value))

        (dummy-i:tx-loop dummy-i (cdr tx-value))))

(define (dummy-i:start! dummy-i)
  (create-thread
   #f
   (lambda ()
     (dummy-i:tx-loop dummy-i 0))))

; Testing
; (define the-dummy (dummy:make))

; (dummy:start the-dummy)

; Main loop
; (define (loop)
;     (let ((out (dummy:read the-dummy)))
;     (loop)))

; (loop)

;;; Generic predicate
(define dummy? (platform-predicate 'dummy))

;;; Config format
(define dummy-config:interval
  (make-property 'interval
		 'predicate number?))

(define dummy-config?
    (make-type 'dummy-config
        (list dummy-config:interval)))

(define get-dummy-interval
    (property-getter dummy-config:interval dummy-config?))

(define make-dummy-config
    (type-instantiator-with-defaults dummy-config?
        '(platform-id dummy)))

(define-generic-procedure-handler get-platform-config-constructor
    (match-args dummy?)
    (lambda (platform) make-dummy-config))

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
    (match-args dummy? event?)
    (wrap-interface dummy-i:write!))