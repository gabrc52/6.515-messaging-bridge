;;;; Demonstration

(define mattermost
  (websocket-connect! "wss://mattermost.mit.edu/api/v4/websocket"
		     '(("Authorization" . "Bearer otfjuew96pfh8rrfxga3nf7mby"))))

;; This works now! Returns #f or the next message.
(pp (websocket-next-json mattermost))

(websocket-connected? mattermost) ;; #t

(websocket-close! mattermost)

(websocket-connected? mattermost) ;; #f

;;;; Done
