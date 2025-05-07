;; Tried to do this unsuccessfully, but Python was better.
;; (load-option 'FFI)
;; (c-generate "zephyr" "#include <netinet/in.h>\n#include <arpa/inet.h>\n#include <sys/time.h>\n#include <zephyr/zephyr.h>\n#include <com_err.h>\n#include <stdlib.h>\n#include <string.h>")

(define zephyr? (platform-predicate 'zephyr))
(define *zwrite-binary* (os/find-program "zwrite" ""))
(define *python-binary* (os/find-program "python" ""))

(define zephyr-config:subscriptions
  (make-property 'subscriptions
		 'predicate list?))

;; Generate a .zephyr.subs format from subscriptions
(define (generate-zephyr-subs subscriptions)
  (define (%generate-sub subscription)
    (string (first subscription) "," (second subscription) ",*\n"))
  (string* (map %generate-sub subscriptions))) ;; or apply string-append

(define zephyr-config?
  (make-type 'zephyr-config (list zephyr-config:subscriptions)))

;; TODO WIP
(define (make-zephyr! config) (begin))
