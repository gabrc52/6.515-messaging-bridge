(define (start-clients!)
    (for-each 
        (lambda (client)
            (pp "Starting client")
            (start-client! (cdr client)))
        *all-clients*))

(start-clients!)

(define (read-clients!)
    (for-each
        (lambda (client)
            ; (pp (list "Reading from client" client))
            (let ((obj (read-client! (cdr client))))
                ; (pp (list "Got obj" obj))
                (unless (equal? obj '()) (handle-event! obj))))
        *all-clients*))

(define (loop)
    (read-clients!)
    (loop))

(loop)