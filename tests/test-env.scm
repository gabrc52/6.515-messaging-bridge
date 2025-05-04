;;; Run this from the project root

(load "env.scm")

(split-string "asdf" "9")
(split-string "asdf--jsdf" "--")
(split-string "key=value" "=")
(split-string "key1=value1\nkey2=value2\nkey3=value3" "\n")
(split-string (read-file ".env") "\n")

(parse-env (read-file ".env"))

(get-from-env "discord-token")