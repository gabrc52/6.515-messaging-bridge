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

TODO define platform options

TODO give an example