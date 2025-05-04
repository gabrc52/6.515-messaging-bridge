(define (make-port-based-client port)
  (define (%receive-raw-event-blocking!)
    (string->jsexpr (read-line port)))
  (define (%send-raw-event! jsexpr)
    (port-send-json port jsexpr))
  (lambda (message)
    (case message
      ((%get-port) port) ;; Intended to be private/encapsulated. May help debugging.
      ((raw-event-receiver) %receive-raw-event-blocking!)
      ((message-sender) %send-raw-event!) ;; Design pending
      (else (error "port-based-client: Unknown command" message)))))
