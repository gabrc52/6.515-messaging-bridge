;; Depends on: JSON implementation

(load-option 'format)

;; Credit https://www.reddit.com/r/scheme/comments/x1xd09/mit_scheme_httprequest/
(define (prepare-headers alist)
  (append-map (lambda (h)
                (list "--header"
                      (format #false "~A: ~A" (car h) (cdr h))))
              alist))

(define (curl-http-request method baseurl path #!optional headers content)
  (let ((headers (if (default-object? headers) (list) headers))
	(url (string-append baseurl path))
    (content (if (default-object? content) '() (list "-d" content))))
    (call-with-output-string
     (lambda (port)
       (assert (zero?
                (run-synchronous-subprocess
                 "/usr/bin/curl"
                 (append
                    (cons* "--request" method
                            "--location"
                            "--silent"
                            "--url" url
                            (prepare-headers headers))
                    content)
                 'output port))
               "Error in HTTP request."
               url)))))

;; TODO: We need function combinators to fix the baseurl and only specify the path (already in SDF)
;; Or a function combinator that lets you specify headers (authentication)
;; We can also get some function combinators for converting to JSON
;;   (then the new functions just need the more specific request)
;; And maybe a way of declaratively expressing which combinators need to be applied?

