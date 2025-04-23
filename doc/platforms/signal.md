# Signal

Signal is an open-source, but centralized, messaging app, that provides secure end-to-end encryption for direct messages and group chats.

<https://github.com/AsamK/signal-cli> is a client for Signal. To install it, for example, can put the `signal-cli` binary from the GitHub releases somewhere in your path. Here is an example set of commands to register: 

```shell
$ signal-cli -a +1857xxxxxxx register
Captcha required for verification, use --captcha CAPTCHA
To get the token, go to https://signalcaptchas.org/registration/generate.html
After solving the captcha, right-click on the "Open Signal" link and copy the link.
$ signal-cli -a +1857xxxxxxx register --captcha "signalcaptcha://..."
$ signal-cli -a +1857xxxxxxx verify 510795
$ signal-cli updateProfile --given-name Relay --family-name Bot --about "Relaying messages!" --avatar ~/Downloads/carrier-pigeon.jpeg 
INFO  AccountHelper - The Signal protocol expects that incoming messages are regularly received.
$ signal-cli send -m "Hello world!" +1857yyyyyyy
```

## Receiving messages

The `signal-cli receive` command receives the latest batch of messages. However, the `daemon` mode creates a long-running connection to Signal and listens for new messages. `signal-cli receive --tcp` opens a port for this, which we can confirm that it works with netcat, and even displays typing indicators:

```js
$ nc 127.0.0.1 7583
{"jsonrpc":"2.0","method":"receive","params":{"envelope":{"source":"+1857yyyyyyy","sourceNumber":"+1857yyyyyyy","sourceUuid":"7ccae375-cd51-4c82-8b1e-7be3280aa485","sourceName":"Gabriel","sourceDevice":1,"timestamp":1745372297238,"serverReceivedTimestamp":1745372294830,"serverDeliveredTimestamp":1745372294831,"typingMessage":{"action":"STARTED","timestamp":1745372297238}},"account":"+1857xxxxxxx"}}
{"jsonrpc":"2.0","method":"receive","params":{"envelope":{"source":"+1857yyyyyyy","sourceNumber":"+1857yyyyyyy","sourceUuid":"7ccae375-cd51-4c82-8b1e-7be3280aa485","sourceName":"Gabriel","sourceDevice":1,"timestamp":1745372301011,"serverReceivedTimestamp":1745372298698,"serverDeliveredTimestamp":1745372298699,"dataMessage":{"timestamp":1745372301011,"message":"This is a test","expiresInSeconds":0,"viewOnce":false}},"account":"+1857xxxxxxx"}}
{"jsonrpc":"2.0","method":"receive","params":{"envelope":{"source":"+1857yyyyyyy","sourceNumber":"+1857yyyyyyy","sourceUuid":"7ccae375-cd51-4c82-8b1e-7be3280aa485","sourceName":"Gabriel","sourceDevice":1,"timestamp":1745372303298,"serverReceivedTimestamp":1745372300942,"serverDeliveredTimestamp":1745372300943,"typingMessage":{"action":"STOPPED","timestamp":1745372303298}},"account":"+1857xxxxxxx"}}
```

The format of the JSON is [JSON-RPC](https://www.jsonrpc.org/specification) which is a "stateless, light-weight remote procedure call (RPC) protocol".

It is pretty easy to use this socket from MIT Scheme:

```lisp
;; This works if using `signal-cli daemon --tcp`
(define socket (open-tcp-stream-socket "127.0.0.1" 7583))

;; Or this works if using `signal-cli daemon --socket`
(define socket (open-unix-stream-socket "/run/user/1000/signal-cli/socket"))

;; check on this multiple times
(and (char-ready? socket)
     (pp (string->jsexpr (read-line socket))))
```

This function can be called in a loop and will return `#f` if there's no new message available, or will pretty-print the message if there is one, e.g.:

```lisp
(dict
 ("method" . "receive")
 ("jsonrpc" . "2.0")
 ("params"
  dict
  ("account" . "+1857xxxxxxx")
  ("envelope"
   dict
   ("serverReceivedTimestamp" . 1745372661357)
   ("serverDeliveredTimestamp" . 1745372661358)
   ("sourceNumber" . "+1857yyyyyyy")
   ("sourceName" . "Gabriel")
   ("sourceDevice" . 1)
   ("source" . "+1857yyyyyyy")
   ("timestamp" . 1745372663674)
   ("dataMessage"
    dict
    ("timestamp" . 1745372663674)
    ("textStyles" list
                  (dict ("start" . 7) ("length" . 30) ("style" . "BOLD"))
                  (dict ("start" . 24) ("length" . 12) ("style" . "ITALIC")))
    ("expiresInSeconds" . 0)
    ("message" . "Hello! This is bold and also italics.")
    ("viewOnce" . #f))
   ("sourceUuid" . "7ccae375-cd51-4c82-8b1e-7be3280aa485"))))
```

## Sending messages or other requests

`signal-cli send` etc would work. However, once it is running on daemon mode, it complains:

```
INFO  SignalAccount - Config file is in use by another instance, waiting…
```

Attachments seem to have specific IDs and `getAttachment` works to retrieve them (the TCP connection does not include the attachments themselves).

## Warning

Note that the README of signal-cli gives this warning, also shown when running many commands.

> Hint: The Signal protocol expects that incoming messages are regularly received (using daemon or receive command). This is required for the encryption to work efficiently and for getting updates to groups, expiration timer and other features.

Not sure how often "regularly" means, but once this is running on production, it is clearly a good reason to keep it always receiving messages so that messages are not dropped. The warning makes it sound like it might outright break if it does not receive messages often enough.