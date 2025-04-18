#lang racket/base

(require racket/port)
(require json)

;; Read JSON from standard input
(define jsexpr (read-json))

;; The #hasheq expression cannot be used in Scheme, so let's just use an alist
;; To distinguish between [] and {} we are consing it with 'list and 'dict
(define (with-no-hash-tables jsexpr)
  (cond ((list? jsexpr)
         (cons 'list (map with-no-hash-tables jsexpr)))
        ((hash? jsexpr)
         (let ((alist (hash->list jsexpr)))
           (cons 'dict
                 (map
                  (lambda (element)
                    (cons
                     ;; MIT Scheme symbols are case-insensitive, but JSON is case-sensitive
                     ;; Let's just use strings then
                     (symbol->string (car element))
                     (with-no-hash-tables (cdr element))))
                  alist))))
        (else jsexpr)))

(write (with-no-hash-tables jsexpr))
