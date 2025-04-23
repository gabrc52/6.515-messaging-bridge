# Discord

Discord is a proprietary platform which aims to provide a casual atmosphere for a wide variety of use cases.

Discord has "servers" (collections of chats called "channels"). One has to invite the bot user to the server so it has access to the messages, and it has to be granted access to the message intents from the bot itself and from the admin dashboard website to create bots.

Discord has seemingly comprehensive [documentation](https://discord.com/developers/docs/reference) for its API. There is an HTTP API for making requests such as sending messages, and a Websocket-based ["Gateway API" for receiving messages or other events]((https://discord.com/developers/docs/events/gateway)). The Gateway API also allows performing actions, but the HTTP API seems more standard for that.

There is also a warning provided in the documentation:

> Interacting with the Gateway can be tricky ... If you're planning on writing a custom implementation, be sure to read the following documentation in its entirety so you understand the sacred secrets of the Gateway (or at least those that matter for apps).

It appears that there are some extra steps needed to initialize the connection once you are connected ([identify command](https://discord.com/developers/docs/events/gateway#identifying)), to declare the right to receive messages and tell Discord who you are.

A URL for the Gateway is `wss://gateway.discord.gg/?v=10&encoding=json`. JSON (JavaScript plain-text notation) and ETF (Erlang binary format) are the two supported encodings. JSON is definitely the way to go, since it is more standard, and we got a JSON parser now.