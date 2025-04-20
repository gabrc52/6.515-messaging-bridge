;; (depends on http code)
;;; Trying to use websocket. Maybe the lower level things work

(define thing
  "GET /api/v4/websocket HTTP/1.1
Host: mattermost.mit.edu
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Key: MQeGyXMgAmrJztKtiVugMw==
Sec-WebSocket-Version: 13
Authorization: Bearer otfjuew96pfh8rrfxga3nf7mby
Origin: https://mattermost.mit.edu

")
;; IMPORTANT: the newlines are important otherwise the server won't return anything

(define socket (open-tcp-stream-socket "127.0.0.1" 10001))
(write-string thing socket)
(flush-output socket)
(read-line socket) ;; we could run this multiple times in a loop in its own thread
(close-port socket)

;; THe webhook works!!!!!
;; But we need to parse the things in bytes...

;; IGNORE BELOW, failed attempt

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now we try using the other thing then
;; which already implemented handling the special stuff
;; $ websocat -H="Authorization: Bearer otfjuew96pfh8rrfxga3nf7mby" wss://mattermost.mit.edu/api/v4/websocket

;; But if we do this then we should also consider
;;subprocess - Support to run other programs as subprocesses of the Scheme process. Undocumented; see the source file runtime/process.scm. Used extensively by Edwin.

;; openssl is lower level, still requires doing http, which is not too different from the socket code above
(define shell-command "openssl")
(define shell-args '("s_client" "-connect" "mattermost.mit.edu:443" "-crlf" "-quiet"))
(define output-port (open-output-string))
(define input-port (open-input-string thing))
(define proc
  (run-synchronous-subprocess shell-command shell-args
			      'input input-port
			      'output output-port
			      ;; run periodically when output from the subprocess is available
			      'redisplay-hook (lambda ()
						(display "hook ran!") (newline))
			      ))
;; For some really strange reason the connection instantly closes
;; TODO: I think it is because the input-port has an eof!! Is there a way to not?
(read-line output-port) ;; did not work
(get-output-string output-port)

;; Yeah to fix the eof issue, websocat (what I was trying to get to work, since openssl there is no point since
;; the code above already works) doesn't need an input because it already has the header supplied

;; websocat is higher level
(define shell-command "/home/rgabriel/.cargo/bin/websocat")
(define shell-args '("wss://mattermost.mit.edu/api/v4/websocket"
		     "-H" "Authorization: Bearer otfjuew96pfh8rrfxga3nf7mby"))
;; no input port needed
(define output-port (open-output-string))
;; TODO is there an output port we get for free? without needing to declare like this. i don't like this hm
(define proc
  (run-synchronous-subprocess shell-command shell-args
			      'output output-port
			      ;; run periodically when output from the subprocess is available
			      'redisplay-hook (lambda ()
						(display "hook ran!") (newline))
			      ))
;; proc is just 0
;; Yeah this also terminated instantly

;; Presumably if we really wanted to go with this route we would use the undocumented one, but sockets are fine
