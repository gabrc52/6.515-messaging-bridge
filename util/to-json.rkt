#lang racket/base

(require racket/port)
(require json)

;; Read Scheme expression from stdin
(define expr (read))

(define (make-jsexpr expr)
  (if (list? expr)
      (let ((type (car expr))
            (datum (cdr expr)))
        (case type
          ((list) (map make-jsexpr datum))
          ((dict) (make-hash
                   (map
                    (lambda (element)
                      ;; The Scheme representation we chose uses strings, but Racket's jsexpr use symbols
                      (cons (string->symbol (car element))
                            (make-jsexpr (cdr element))))
                    datum)))
          (else (error "Unknown type annotation"))))
      expr))

(define jsexpr (make-jsexpr expr))
(write-json jsexpr)
