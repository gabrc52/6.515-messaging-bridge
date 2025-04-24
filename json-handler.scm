;; File from Beomjun Kim
;;; Mainly written by GPT 4o under appropriate prompting
;;; json-handler.scm — Pure MIT Scheme JSON handler

;; Gabriel made some modifications to use the same format as his json-racket.scm

;;; === Type and Tag System ===

;; Changing to be a specific symbol. null is not the same thing as false. IDK what Beomjun's ChatGPT was on
(define null-object 'null)
(define (json-null? v) (eqv? v null-object))

(define (json-dict? expr)
  (and
   (pair? expr)
   (eqv? (car expr) 'dict)
   (alist? (cdr expr))))

(define (json-list list)
  (cons 'list list))

(define (json-list? expr)
  (and
   (pair? expr)
   (eqv? (car expr) 'list)
   (list? (cdr expr))))

(define (make-json alist)
  (cons 'dict alist))

(define (json-unwrap j)
  (if (json-dict? j)
      (cdr json)
      (error "Not a JSON object")))

(define (json-dict . pairs)
  (make-json pairs))

(define (pair key val)
  (cons key val))

;; Equivalent of dict[key]
(define (%json-key dict key)
  (assert (json-dict? dict))
  (let ((pair (assoc key (cdr dict))))
    (if (false? pair)
	(error (format #f "Key ~A not found on ~A" key dict))
	(cdr (assoc key (cdr dict))))))

;; I don't know if this will have a practical use but
;; (json-key j key1 key2 ... keyn) should be equivalent to j[key1][key2][...][keyn]
(define (json-key expr . keys)
  (define (%json-key-path expr keys)
    (if (eqv? keys (list))
	expr
	(%json-key-path (%json-key expr (car keys)) (cdr keys))))
  (%json-key-path expr keys))

;;; === JSON Parser ===

(define (string->jsexpr str)
  (define len (string-length str))
  (define pos 0)

  (define (peek)
    (if (< pos len) (string-ref str pos) #\nul))

  (define (advance) (set! pos (+ pos 1)))

  (define (skip-whitespace)
    (let loop ()
      (if (and (< pos len) (char-whitespace? (peek)))
          (begin (advance) (loop)))))

  (define (match str2)
    (let ((n (string-length str2)))
      (if (and (<= (+ pos n) len)
               (string=? (substring str pos (+ pos n)) str2))
          (begin (set! pos (+ pos n)) #t)
          #f)))

  (define (parse-null) (if (match "null") null-object (error "Invalid: expected null")))
  (define (parse-true) (if (match "true") #t (error "Invalid: expected true")))
  (define (parse-false) (if (match "false") #f (error "Invalid: expected false")))

  (define (parse-number)
    (let ((start pos))
      (let loop ()
        (if (and (< pos len)
                 (or (char-numeric? (peek))
                     (char=? (peek) #\.)
                     (char=? (peek) #\-)
                     (char=? (peek) #\+)
                     (char=? (peek) #\e)
                     (char=? (peek) #\E)))
            (begin (advance) (loop))
            (string->number (substring str start pos))))))

  (define (parse-string)
    (if (not (char=? (peek) #\"))
        (error "Expected string"))
    (advance)
    (let loop ((acc '()))
      (cond ((>= pos len) (error "Unterminated string"))
            ((char=? (peek) #\") (advance) (list->string (reverse acc)))
            ((char=? (peek) #\\)
             (begin
               (advance)
               (let ((c (peek)))
                 (advance)
                 (cond
                   ((char=? c #\" ) (loop (cons #\"  acc)))
                   ((char=? c #\\) (loop (cons #\\ acc)))
                   ((char=? c #\/) (loop (cons #\/ acc)))
                   ((char=? c #\b) (loop (cons #\backspace acc)))
                   ((char=? c #\f) (loop (cons #\page acc)))
                   ((char=? c #\n) (loop (cons #\newline acc)))
                   ((char=? c #\r) (loop (cons #\return acc)))
                   ((char=? c #\t) (loop (cons #\tab acc)))
                   (else (error "Invalid escape char"))))))
            (else
             (let ((c (peek)))
               (advance)
               (loop (cons c acc)))))))

  (define (parse-array)
    (if (not (char=? (peek) #\[)) (error "Expected ["))
    (advance)
    (skip-whitespace)
    (let loop ((acc '()))
      (skip-whitespace)
      (if (char=? (peek) #\])
          (begin (advance) (json-list (reverse acc)))
          (let ((v (parse-value)))
            (skip-whitespace)
            (if (char=? (peek) #\,)
                (begin (advance) (loop (cons v acc)))
                (begin
                  (skip-whitespace)
                  (if (char=? (peek) #\])
                      (begin (advance) (json-list (reverse (cons v acc))))
                      (error "Expected , or ]"))))))))

  (define (parse-object)
    (if (not (char=? (peek) #\{)) (error "Expected {"))
    (advance)
    (skip-whitespace)
    (let loop ((acc '()))
      (skip-whitespace)
      (if (char=? (peek) #\})
          (begin (advance) (make-json (reverse acc)))
          (let ((key (parse-string)))
            (skip-whitespace)
            (if (not (char=? (peek) #\:)) (error "Expected :"))
            (advance)
            (skip-whitespace)
            (let ((val (parse-value)))
              (skip-whitespace)
              (if (char=? (peek) #\,)
                  (begin (advance) (loop (cons (cons key val) acc)))
                  (if (char=? (peek) #\})
                      (begin (advance) (make-json (reverse (cons (cons key val) acc))))
                      (error "Expected , or }"))))))))

  (define (parse-value)
    (skip-whitespace)
    (cond ((char=? (peek) #\{) (parse-object))
          ((char=? (peek) #\[) (parse-array))
          ((char=? (peek) #\") (parse-string))
          ((char-numeric? (peek)) (parse-number))
          ((char=? (peek) #\- ) (parse-number))
          ((match "true") #t)
          ((match "false") #f)
          ((match "null") null-object)
          (else (error "Unexpected value"))))

  (parse-value))

;;; === JSON Serializer ===

(define (escape-string s)
  (string-append "\"" (list->string
    (apply append (map (lambda (c)
      (cond ((char=? c #\") (string->list "\\\""))
            ((char=? c #\\) (string->list "\\\\"))
            ((char=? c #\newline) (string->list "\\n"))
            ((char=? c #\tab) (string->list "\\t"))
            (else (list c))))
      (string->list s)))) "\""))

(define (string-join strs sep)
  (if (null? strs)
      ""
      (let loop ((acc (car strs)) (rest (cdr strs)))
        (if (null? rest)
            acc
            (loop (string-append acc sep (car rest)) (cdr rest))))))

(define (jsexpr->string j)
  (define (emit v)
    (cond ((json-dict? v)
           (let ((fields (cdr v)))
             (string-append
              "{"
              (string-join
               (map (lambda (p)
                      (string-append (escape-string (car p)) ": " (emit (cdr p))))
                    fields)
               ", ")
              "}")))
          ((string? v) (escape-string v))
          ((boolean? v) (if v "true" "false"))
          ((number? v) (number->string v))
          ((json-list? v)
           (string-append "[" (string-join (map emit (cdr v)) ", ") "]"))
          ((json-null? v) "null")
          (else (error "Unsupported value" v))))
  (emit j))

;;=== Usage Examples ===

(define j (string->jsexpr
  "{ \"event\": \"message-received\", \"message\": { \"sender\": \"John\", \"content\": \"Hello!\", \"timestamp\": 1744573905, \"bot\": false } }"))

(json-dict? j) ;; #t
(json-key j "event") ;; "message-received"
(define m (json-key j "message"))
(json-key m "sender") ;; "John"
(json-key m "bot") ;; #f

;; Now this also works
(json-key j "message" "sender")

(define k (json-dict
  (pair "event" "message-sent")
  (pair "message" (json-dict
    (pair "sender" "Ben Bitdiddle")
    (pair "content" "Hello John!")
    (pair "timestamp" 1744573905)
    (pair "bot" #f)))))

(jsexpr->string k)

(define j (json-dict (pair "event" "test") (pair "value" 42)))
(json-write-file "test.json" j)

(define j2 (json-read-file "test.json"))
(json-key j2 "event")  ;; => "test"

;; My test cases should work, let's see
(define s1 "[1, 2, {\"a\": \"b\"}, {\"b\": {}, \"c\": false}, true, null]")
(string->jsexpr s1)
(define s2 "{\n  \"preSemester\": {\n    \"urlName\": \"i25\",\n    \"startDate\": \"2025-01-06\",\n    \"endDate\": \"2025-01-31\",\n    \"holidayDates\": [\n      \"2025-01-20\"\n    ]\n  },\n  \"semester\": {\n    \"urlName\": \"s25\",\n    \"startDate\": \"2025-02-03\",\n    \"h1EndDate\": \"2025-03-21\",\n    \"h2StartDate\": \"2025-03-31\",\n    \"endDate\": \"2025-05-13\",\n    \"mondayScheduleDate\": \"2025-02-18\",\n    \"holidayDates\": [\n      \"2025-02-17\",\n      \"2025-03-24\",\n      \"2025-03-25\",\n      \"2025-03-26\",\n      \"2025-03-27\",\n      \"2025-03-28\",\n      \"2025-04-21\"\n    ]\n  }\n}\n")
(string->jsexpr s2)

(string->jsexpr "{\"so true\": null}")
(string->jsexpr "[1, 2, 3, 4, 5]")
