;; TODO: don't hardcode. Get from .env (after reading it first?)
;;   Alternatively, once we are finished, bring in the subset of SDF's code that we actually use.
(load (if
       (equal? (current-user-name) "rgabriel")
       "~/6.5151/sdf/manager/load"
       "~/local/6.5150/sdf/manager/load"))
(manage 'new 'user-defined-types)

;; gjs said he isn't actually sure if running it all at once may work. You may need to run the manager first,
;; and THEN the rest of the stuff so it uses the environment of the manager.

;; Configuration:
(define *project-home* (if
			(equal? (current-user-name) "rgabriel")
		        "~/6.5151/project/messaging-bridge"
		        "~/git/messaging-bridge"))

;; cd to the right place if running on emacs
(when
  (pathname=? (user-home-directory (current-user-name)) (pwd))
  (cd *project-home*))

;; Only works when executing, not when on emacs
;; Stolen from https://github.com/ProjectMAC/propagators/blob/master/load.scm
(define (self-relatively thunk)
  (let ((place (ignore-errors current-load-pathname)))
    (if (pathname? place)
	(with-working-directory-pathname
	 (directory-namestring place)
	 thunk)
	(thunk))))
(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(load-option 'synchronous-subprocess)
(load-option 'subprocess)
(load-option 'format)

;; Load from .env file
(load-relative "env")

;;; JSON parser and serializer
;; Choose either of them
(load-relative "json-racket") ;; Racket + subprocess implementation
;;(load-relative "json-handler") ;; Modified Beomjun's ChatGPT implementation

;; Example timings to parse GUILD_CREATE Discord JSON event:
;; * json-handler: .09 0. .084
;; * json-racket: .09 0. .255

;; TODO: we could add some redundancy.
;;   If we can make the native Scheme JSON parser fail when it has a JSON it cannot parse
;;   (instead of giving wrong answers, which I am not sure if it does or not),
;;   then we can catch the exception and call the Racket handler instead.
;;   e.g. I don't think it handles unicode escape codes? But I don't know all of JSON so I don't know
;;        what it returns a wrong answer to or fails at.

;; Uses curl
(load-relative "http")

;; Uses websockets
(load-relative "websockets")

(load-relative "util") ;; misc utilities
(load-relative "types")
(load-relative "queue")
(load-relative "generics")
(load-relative "config")

;; Load platform pieces
(load-relative "platforms/common/port")
(load-relative "platforms/common/json-rpc")

;; Load each platform
(load-relative "platforms/mattermost")
(load-relative "platforms/dummy")
(load-relative "platforms/discord")
(load-relative "platforms/signal")

;; Apparently it has to be below, otherwise generic procedures will silently be wrong
(load-relative "bridge")

;; Load the config file
(load-config-file! "config.txt")
(load-relative "main") ;; Entrypoint
