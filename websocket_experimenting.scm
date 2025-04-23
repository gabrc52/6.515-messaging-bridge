(load-option 'subprocess)

;; (depends on http code)
;;; Trying to use websocket. Maybe the lower level things work

;;; First we need to start websocat

(define s (run-subprocess-in-foreground "/bin/echo" #("/bin/echo" "hello") #()))
;; prints: hello
(subprocess-exit-reason s) ;; 0

;; TODO: find a way to not hardcode this, or just to install somewhere else
;;   try using ~ notation
;;   otherwise try adding more helpers with path
;;   or put this in a configuration file
(define *websocat-binary* "/home/rgabriel/.cargo/bin/websocat")
(define mattermost-port 10001) ;; here it would allocate an available port, keeping track internally
(define signal-port 7583) ;; default signal-cli port
(define websocket-url "wss://mattermost.mit.edu/api/v4/websocket")
;; we'd need to authenticate first and/or get the token from config file
(define header "Authorization: Bearer otfjuew96pfh8rrfxga3nf7mby")

;; Mattermost, listen at TCP socket
(define websocat
  (start-subprocess-in-background *websocat-binary*
				  (vector *websocat-binary* "--text" "--exit-on-eof"
					  (string "tcp-listen:127.0.0.1:" port)
					  websocket-url "-H" header)
				  #()))

(start-subprocess-in-background "/bin/echo" #("/bin/echo" "Hello world") #())

;; Unix socket
;; NOTE: --unlink unlinks it before starting instead of throwing an error (if there is one still running)
;;   but it would be better to terminate it
(define websocat
  (start-subprocess-in-background *websocat-binary*
				  (vector *websocat-binary* "--unlink" "--text" "--exit-on-eof"
					  (string "unix-listen:/tmp/mattermost-socket")
					  websocket-url "-H" header)
				  #()))
;; We are now ready to use TCP sockets directly, and bypass implementing the websocket spec

;; Mattermost...
(define socket (open-tcp-stream-socket "127.0.0.1" mattermost-port))
;; ...or Signal
(define socket (open-tcp-stream-socket "127.0.0.1" signal-port))
;; Or Signal via sockets
;; Better -- no port numbers!
(define socket (open-unix-stream-socket "/run/user/1000/signal-cli/socket"))
;; I guess if we wanted to, for Signal CLI we can also use --socket /tmp/signal-socket (instead of the default)

;; Mattermost test socket
(define socket (open-unix-stream-socket "/tmp/mattermost-socket"))

;; This function is blocking and may wait forever
(read-line socket) ;; we could run this multiple times in a loop in its own thread
;; However, we have a way to check if we can call read-line in the first place
(char-ready? socket)

;; Demonstration
(and (char-ready? socket) (read-line socket)) ;; returns #f or gets next message

;; Demonstration and even with parsing!
(and (char-ready? socket)
     (pp (string->jsexpr (read-line socket))))
;; Try executing this multiple times! it will show the next event or otherwise #f
;; We can have a running loop to read a line from each open socket (probably with another abstraction
;; that is not necessarily socket-based in case there are other ways of polling, e.g. zephyr is
;; definitely not websockets and JSON)
;; And we need an abstraction to start the connection (which in this case would start websocat)

;; Yeah after a while the connection gets dropped

(close-port socket)

;; Do this if we ever need to exit (but there is no exit condition, the bridge would run forever?)
(subprocess-quit websocat)
;; TODO: not sure what the difference between subprocess-quit and subprocess-stop is

;;;;;;; Irrelevant now:

;; It may become stuck at #!eof and #t. This is where reading the spec comes in, maybe websocket has
;; a mechanism to tell it "hey, I'm still here listening". (yeah ping/pong, there is)

;; This is no longer needed now that we use websocat
#||(define thing
  "GET /api/v4/websocket HTTP/1.1
Host: mattermost.mit.edu
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Key: MQeGyXMgAmrJztKtiVugMw==
Sec-WebSocket-Version: 13
Authorization: Bearer otfjuew96pfh8rrfxga3nf7mby
Origin: https://mattermost.mit.edu

")||#
;; IMPORTANT: the newlines are important otherwise the server won't return anything

;; This used to be needed to pass the HTTP payload to the socket. Now websocat takes care of it
;; (write-string thing socket)
;; (flush-output socket) ;; important, to actually make it go to the server
