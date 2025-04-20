# JSON

**Decision**: Delegate JSON parsing and serializing to Racket, a dialect of Scheme
which does have JSON support.

Unfortunately, MIT/GNU Scheme does not come with a built-in JSON library, nor do
there seem to be any readily available.

However, Racket, a dialect of Scheme, does have built-in JSON support.
So `json_racket.scm` simply runs `util/from-json.rkt` and `util/to-json.rkt` via
a subprocess.

`json-handler.scm` is an LLM-generated JSON parser. It is not fully tested yet, so
delegating to Racket seems like the most sensible option, and we can consider
switching to it in the future, and we can even consider having built-in redundancy
between both implementations.