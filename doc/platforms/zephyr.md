# Zephyr

Zephyr is definitely trickier to set up. For now, we have a Python shim that acts as a proxy, to avoid having to reimplement the Zephyr receiving logic or use the C FFI, since it is the simplest option that works. See `util/zephyr_shim/main.py`. The dependency it relies on is <https://github.com/ebroder/python-zephyr>.

More information about Zephyr can be found at <https://sipb.mit.edu/doc/zephyr/> and <https://en.wikipedia.org/wiki/Zephyr_(protocol)>. It is messaging software from the Project Athena days, and still used by some at SIPB (computer club).

For non-Athena computers, it can be compiled from source at <https://github.com/zephyr-im/zephyr>, and requires `zhm` to be running in the background, and you have to be on the MIT net to receive messages or otherwise not on a NAT.

In other platforms, you would have to invite a stateful bot user to the chats you want it to be in. However, on Zephyr, there is no concept of membership. Instead, one can "subscribe" to specific "classes" (which are collections of "instances"). The `zephyr.subs` file contains these subscriptions.

"Classes" work by having everyone wishing to talk mutually agree on a class name. "Instances" can be conversation topics, and they don't have to be created in advance: as long as everyone who wants to be on the conversation is subscribed to a class, they will see the message. In a way, every class and every instance exists. However, you can choose to subscribe to an entire class, or only some instances of it.

For sending messages, the `zwrite` command should work to avoid having to reimplement sending functionality. If we are calling `curl` every time we do a HTTPS web request, calling `zwrite` every time we send a message is totally fine. See `zwrite --help` or `man zwrite` for usage information.
