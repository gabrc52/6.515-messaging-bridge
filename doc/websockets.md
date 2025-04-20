# Websockets

**Decision**: Use [websocat](https://github.com/vi/websocat) to expose the websocket
to a localhost port via TCP, so it handles all the ping/pong/keepalive and parsing
the Websocket-specific format, and then we can simply open a TCP stream socket from
MIT/GNU Scheme and talk higher-level JSON instead of having to parse a binary format.

MIT/GNU Scheme does not support websockets.

We were able to get websockets to work by manually talking HTTP to a port opened by stunnel
(see `https.md`).

```lisp
(define socket (open-tcp-stream-socket "127.0.0.1" 10001))
(write-string raw-http-request socket)
(flush-output socket)
(and (char-ready? socket) (read-line socket)) ;; returns #f or gets next message
```

This is very promising, but it includes low-level gibberish. To parse it, we would
have needed to port [code from Racket](https://github.com/tonyg/racket-rfc6455) to MIT/GNU Scheme,
or we would have needed to reimplement [RFC 6455 - The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455).