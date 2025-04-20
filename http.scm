(load-option 'format)

;; TODO: depends on JSON stuff. import it somehow?

;; Credit https://www.reddit.com/r/scheme/comments/x1xd09/mit_scheme_httprequest/
(define (curl-prepare-headers alist)
  (append-map (lambda (h)
                (list "--header"
                      (format #false "~A: ~A" (car h) (cdr h))))
              alist))

(define (curl-http-request method baseurl path #!optional headers)
  (let ((headers (if (default-object? headers) (list) headers))
	(url (string-append baseurl path)))
    (call-with-output-string
     (lambda (port)
       (assert (zero?
                (run-synchronous-subprocess
                 "/usr/bin/curl"
                 (cons* "--request" method
			"--location"
                        "--silent"
                        "--url" url
                        (curl-prepare-headers headers))
                 'output port))
               "Error in HTTP request."
               url)))))

;; TODO: We need function combinators to fix the baseurl and only specify the path (already in SDF)
;; Or a function combinator that lets you specify headers (authentication)
;; We can also get some function combinators for converting to JSON
;;   (then the new functions just need the more specific request)
;; And maybe a way of declaratively expressing which combinators need to be applied?

(define (get-advice)
  (json-key
   (string->jsexpr (curl-http-request "GET" "https://api.adviceslip.com" "/advice"))
   "slip" "advice"))

(let loop ((i 0))
  (if (< i 15)
      (begin
	(write i) (display " ")
	(display (get-advice))
	(newline)
	(loop (+ i 1)))))

(string->jsexpr (curl-http-request "GET" "https://api.github.com" "/users/vi"))

;; Back to using the builtin HTTP implementation
;; HTTP suffices thanks to stunnel

(define (http-get-string domain path #!optional headers)
  (let ((headers (if (default-object? headers) (list) headers))
	(url (string-concatenate (list "http://" domain path))))
    (utf8->string (http-response-body (http-get url headers)))))

(define (http-get-json domain path #!optional headers)
  (string->jsexpr (http-get-string domain path headers)))

;; If we wanted to, we can easily implement http-put copying the implementation of http-post but
;; changing it to PUT
(define (http-put uri headers body)
  (http-client-exchange "PUT" (->uri uri) headers body))

(http-get-json "127.0.0.1:20001" "/advice")

(http-get "http://127.0.0.1:20001/advice" '())

(define response (http-get "http://hydrant.mit.edu/latestTerm.json" '()))
(string->jsexpr (utf8->string (http-response-body response)))

;; Example Matrix unauthenticated endpoint (gets the types of logins that a server supports)
"/_matrix/client/v3/login"

(http-get "http://127.0.0.1:10002/_matrix/client/v3/login" '())
(string->jsexpr (curl-http-request "GET" "http://127.0.0.1:10002" "/_matrix/client/v3/login"))
;The object #[binary-output-port 13], passed as an argument to write, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n):

;; This works
(string->jsexpr (curl-http-request "GET" "https://mattermost.mit.edu/" "api/v4/config/client?format=old"))

;; This also works
(string->jsexpr (curl-http-request "GET" "http://127.0.0.1:10001/" "api/v4/config/client?format=old"))


(http-get "http://127.0.0.1:10001/api/v4/config/client?format=old" '())

;; This connects to the ChatGPT-generated proxy which does not actually work with query strings?
;; But it does not work either
(http-get "http://127.0.0.1:8080/api/v4/config/client?format=old" '())

;; Does it work with ANYTHING localhost then???

(http-get "http://localhost:10001/api/v4/config/client?format=old" '())

;; None of the http-get work
