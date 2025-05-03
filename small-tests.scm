#||
(for-all-different-pairs '(a b c) (lambda (x y) (write-line (cons x y))))
(a . b)
(a . c)
(b . a)
(b . c)
(c . a)
(c . b)
||#

(identifier=? (make-identifier 'discord "5") (make-identifier 'discord "5"))
;Value: #t

(identifier=? (make-identifier 'zephyr '("sipb" "office")) (make-identifier 'zephyr '("sipb" "office")))
;Value: #t

(identifier=? (make-identifier 'zephyr '("sipb" "office")) (make-identifier 'zephyr '("sipb" "help")))
;Value: #f

;; Declines invalid config as expected
(load-config!
 '(config
   (bridge
    (linked (discord "787146644264189975")
            (discord "820354450073714688")
            (slack "C08PR8QQU93"))
    (linked (discord "820354450073714688") (zephyr "hello" "world")))))
;A chat may only show up on one list, please consolidate the lists. Culprit: [discord 820354450073714688]

;; It seems to load the bridge options just fine
(load-config!
 '(config
   (bridge
    (linked (discord "787146644264189975")
            (discord "820354450073714688")
            (slack "C08PR8QQU93"))
    (linked (discord "1105970284307165194") (zephyr "hello" "world")))))
(pp (hash-table->alist *linked-chats*))
#||
(([discord 1105970284307165194] [zephyr (hello world)])
 ([discord 787146644264189975] [slack C08PR8QQU93]
                               [discord 820354450073714688])
 ([zephyr (hello world)] [discord 1105970284307165194])
 ([discord 820354450073714688] [slack C08PR8QQU93]
                               [discord 787146644264189975])
 ([slack C08PR8QQU93] [discord 820354450073714688]
                      [discord 787146644264189975]))
||#

;;; Generic platform predicates

((platform-predicate 'discord) 'a)
;Value: #f

((platform-predicate 'discord) 'discord)
;Value: #t

((platform-predicate 'discord) (lambda (x) 'discord))
;Value: #t

((platform-predicate 'discord) (lambda (x) 'a))
;Value: #f

((platform-predicate 'discord) #t)
;Inapplicable generic procedure: platform-is? (#t discord)
