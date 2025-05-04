;;; Run this from the project root
(load "env.scm")

(split-string "asdf" "9")
(split-string "asdf--jsdf" "--")
(split-string "key=value" "=")
(split-string "key1=value1\nkey2=value2\nkey3=value3" "\n")
(split-string (read-file ".env") "\n")

(parse-env (read-file ".env")) ; Can parse the project root
(parse-env (read-file "tests/envs/.env-empty")) ; Can parse an empty .env
(parse-env (read-file "tests/envs/.env-does-not-exist")) ; Can safely handle non-existing file

(get-from-env "key" "tests/envs/.env") ; Can obtain a key
(get-from-env "should-not-exist" "tests/envs/.env") ; Safely returns #f is key doesn't exist