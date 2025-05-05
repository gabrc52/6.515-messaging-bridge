(define (start-clients!)
    (for-each 
        (lambda (client)
            (start-client! (cdr client)))
        *all-clients*))

(start-clients!)

(define (read-clients!)
    (for-each
        (lambda (client)
            (read-client! (cdr client)))
        *all-clients*))

(define (loop)
    (read-clients!)
    (loop))

(loop)