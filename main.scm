(define (start-clients!)
    (for-each 
        (lambda (client)
            (start-client! (cdr client)))
        *all-clients*))

(start-clients!)

(define (read-clients!)
    (for-each
        (lambda (client)
            (let ((in (read-client! (cdr client))))
                (unless (equal? in '()) (handle-event! in))))
        *all-clients*))

(define (loop)
    (read-clients!)
    (loop))

(loop)