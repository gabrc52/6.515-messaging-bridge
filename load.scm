;; cd to the right place if running on emacs
(when
  (pathname=? (user-home-directory (current-user-name)) (pwd))
  (cd "~/6.5151/project/messaging-bridge"))

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

;; Choose either of them
(load-relative "json-racket") ;; Racket + subprocess implementation
;; (load-relative "json-handler") ;; Modified Beomjun's ChatGPT implementation

;; Uses curl
(load-relative "http")

