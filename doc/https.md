# HTTPS

**Decision**: We are invoking `curl` via `run-synchronous-subprocess`.

Unfortunately, MIT/GNU Scheme does not support HTTPS, and only supports HTTP.

A nice way of using HTTPS via the existing functions like `http-get` is to use `stunnel`.
However, there still seem to be bugs in `http-get` so it does not work.

The MIT/GNU Scheme developers recommend calling `curl` instead, and gave us sample code for that.

An option that we considered very strongly is to use `stunnel`, since it is a daemon
that can run in the background and expose a local HTTP port and relay those requests
to the remote server via HTTPS, while still doing all the necessary security validation.

Another option that we considered would be to call a shell `openssl s_client hostname.mit.edu:443 -crlf -quiet`.
It would require reimplementing HTTP or wrapping the existing MIT/GNU Scheme to use this port instead of the TCP socket port.
However, working with input and output of subprocesses in real time has proven cumbersome.
