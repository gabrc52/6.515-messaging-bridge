;; Configuration:
; (define *project-home* "~/git/messaging-bridge")
(define *project-home* "~/6.5151/project/messaging-bridge")

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

;; Load from .env file
(load-relative "env")