# Config file

The config file `config.txt` must be a Scheme S-expression, with the following format.

```lisp
(config 
  (bridge
    (linked [identifier1]
            [identifier2]
            ...)
    (linked ...)
    ...
  )
  ([platform1] [options1])
  ([platform2] [options2])
  ...
)
```

It **must** have only one S-expression consisting of `(config ...)`

The first item inside `config` **must** be `(bridge (linked ...) ...)`. Items in `bridge` **must** be lists starting with `linked`.

Identifiers are in the format `([platform] [id])`, for example `(discord "787146644264189975")`.

Identifiers may have multiple arguments depending on the platform, for example `(zephyr "sipb" "lisp")`.

You are allowed to link chats of the same platform together, but a chat identifier may only show up once on the list of `linked` to avoid infinite loops.

Platform options are property lists in the form `[property name 1] [property value 1] [property name 2] [property value 2] ...`

For secrets such as tokens you can use `(env "DISCORD_TOKEN")` instead of the literal secret to get the relevant env variable. This makes config files shareable without having to share secrets!

*env variables come from `.env`, a newline-delimited list of `key=value`. 

Here is an example config bridging two pairs of chats between two platforms.

```lisp
(config
 (bridge
  (linked (discord "1369642663191777391") (signal "YVs2/HSkS8viSYndY2hw3dHIFfWAR2Wl88/VNU/PreY="))
  (linked (signal "uWj8kwYxwYADr3L48dI3yi/eRQY/pytzVZ4et4XsjeE=") (discord "1369642682086981703"))
  )
 (discord
  token (env "discord-token"))
 (signal
  phone-number (env "phone-number"))
 )
```