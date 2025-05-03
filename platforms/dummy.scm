(load "../queue.scm")

; tx/rx is from the perspective of the dummy to the bridge

(define-record-type dummy
    (dummy:make-record tx-queue rx-queue interval)
    dummy?
  (tx-queue dummy:tx-queue)
  (rx-queue dummy:rx-queue)
  (interval dummy:interval))

(define (dummy:make)
    (dummy:make-record (queue:make) (queue:make) 5)) ; 5 seconds default interval

(define (dummy:read dummy)
    (if (queue:empty? (dummy:tx-queue dummy))
        '()
        (pp (list "Received from Queue:" (queue:get-first! (dummy:tx-queue dummy))))))

(define (dummy:write dummy obj)
    (pp (cons "Dummy received" obj)))


(define (dummy:tx-fetch-value dummy last-ts)
    (if (> (- (get-universal-time) last-ts) (dummy:interval dummy))
        (cons "Dummy Sent Msg" (get-universal-time))
        (cons '() last-ts)))

(define (dummy:tx-loop dummy ts)
    (let ((tx-value (dummy:tx-fetch-value dummy ts))
          (tx-queue (dummy:tx-queue dummy)))
        (if (equal? (car tx-value) '())
            '()
            (queue:add-to-end! tx-queue tx-value))

        (dummy:tx-loop dummy (cdr tx-value))))

(define (dummy:start dummy)
  (create-thread
   #f
   (lambda ()
     (dummy:tx-loop dummy 0))))

; Testing
(define the-dummy (dummy:make))

(dummy:start the-dummy)

; Main loop
(define (loop)
    (let ((out (dummy:read the-dummy)))
    (loop)))

(loop)