# Websockets

**Decision**: Use [websocat](https://github.com/vi/websocat) to expose the websocket
to a localhost port via TCP, so it handles all the ping/pong/keepalive and parsing
the Websocket-specific format, and then we can simply open a TCP stream socket from
MIT/GNU Scheme and talk higher-level JSON instead of having to parse a binary format.

---------

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
have needed to reimplement [RFC 6455 - The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455)
or port [the Racket implementation](https://github.com/tonyg/racket-rfc6455) to MIT/GNU Scheme.

Instead, websocat can deal with all the implementation details! The following command ([doc](https://github.com/vi/websocat/blob/master/doc.md#tcp-listen)) can listen on an unencrypted TCP port and relay everything to/from the secure websocket.

```sh
websocat --text --exit-on-eof tcp-listen:127.0.0.1:10001 wss://mattermost.mit.edu/api/v4/websocket -H "Authorization: Bearer blabla"
```
