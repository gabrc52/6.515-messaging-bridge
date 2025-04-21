# HTTPS

**Decision**: We are invoking `curl` via `run-synchronous-subprocess`.

---------

Unfortunately, MIT/GNU Scheme does not support HTTPS, and only supports HTTP.

An option that we considered very strongly is to use [stunnel](https://www.stunnel.org/), since it is a daemon
that can run in the background and expose a local HTTP port and relay those requests
to the remote server via HTTPS, while still doing all the necessary security validation.

Even with this daemon, the built-in function `http-get` was throwing errors when
trying to connect to URLs on localhost. As it's not even documented, it is likely a bad idea
to use it due to these bugs.

The MIT/GNU Scheme developers recommend calling `curl` instead, and gave us sample code for that.
`curl` should be very robust and properly implement any edge cases in the HTTP and HTTPS
protocol, and better than rolling our own HTTP.

Another option that we considered would be to call a shell `openssl s_client hostname.mit.edu:443 -crlf -quiet`.
It would require reimplementing HTTP or wrapping the existing MIT/GNU Scheme to use this port instead of the TCP socket port.
However, working with input and output of subprocesses in real time has proven cumbersome.
