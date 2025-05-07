;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Discord demo ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define discord
  (websocket-connect! "wss://gateway.discord.gg/?v=10&encoding=json"
		      '() ;; No headers necessary.
		      ;; "When you close the connection to the gateway with close code 1000 or 1001,
		      ;;  your session will be invalidated and your bot will appear offline."
		      '("--close-status-code" "1000")))



;; Previous implementation:
;;(when-available (websocket-next-json discord) handle-event)

(discord-handle-event discord) ;; "Discord says hello"
(discord-handle-event discord) ;; #f

;; TODO: Need to send a heartbeat every 40 seconds following the instructions in the documentation.
;; "If your app fails to send a heartbeat event in time, your connection will be closed and you will be forced to Resume."

;; We need to send the first heartbeat at the beginning
(discord-send-heartbeat! discord)
(discord-handle-event discord)
;; (dict ("d" . null) ("t" . null) ("op" . 11) ("s" . null))

(discord-identify! discord)
(discord-handle-event discord) ;; Logged in as Messaging Bridge

(with-timings
 (lambda () (discord-handle-event discord))
 (lambda (run-time gc-time real-time)
   (write (internal-time/ticks->seconds run-time))
   (write-char #\space)
   (write (internal-time/ticks->seconds gc-time))
   (write-char #\space)
   (write (internal-time/ticks->seconds real-time))
   (newline)))
;; We are in the server gabrielr's server
;; * json-handler: .09 0. .084
;; * json-racket: .09 0. .255

;; Send another heartbeat so they don't kill us
(discord-send-heartbeat! discord)
(discord-handle-event discord)
;; (dict ("d" . null) ("t" . null) ("op" . 11) ("s" . null))

;; After sending a message to the server, we can read it now!
(discord-handle-event discord) ;; Unimplemented dispatch event type typing_start
(discord-handle-event discord) ;; Gabriel R. says "Hello, this is a test message!"!

;; NOTE: This is what happens if you don't reply to heartbeats on time:
;; `The primitive channel-write, while executing the write system call, received the error: Broken pipe.`

;; To test disconnections, it is surprisingly easy to make it disconnect (by sending it an invalid JSON)
(websocket-send-json discord '(dict))
(discord-handle-event discord) ;; The connection got dropped! We should reconnect!

;; Simple stub loop. It does not handle multiple websockets (for multiple services).
;; nor does it send the heartbeats every *heartbeat-interval* yet
(let loop ()
  (unless (websocket-connected? discord) (websocket-close! discord))
  (discord-handle-event discord)
  (loop))
;; This is blocking and turns on my laptop fans. Maybe there is a better way

;; There is a way of closing the connection explicitly that is not just closing the port
;; See https://discord.com/developers/docs/events/gateway#initiating-a-disconnect
;; "When you close the connection to the gateway with close code 1000 or 1001, your session will be invalidated and your bot will appear offline." (TODO: implement this?)
;; It also closes the connection if you send it invalid data
(websocket-close! discord)

;; TODO: also handle what if Discord disconnects
;; "Due to Discord's architecture, disconnects are a semi-regular event and should be expected and handled. When your app encounters a disconnect, it will typically be sent a close code which can be used to determine whether you can reconnect and Resume the session, or whether you have to start over and re-Identify."