;;; Mainly written by GPT 4o under appropriate prompting
;;; json-handler.scm — Pure MIT Scheme JSON handler

;;; === Type and Tag System ===

(define json-tag 'json-object)

(define (json? obj)
  (and (vector? obj)
       (eq? (vector-ref obj 0) json-tag)))

(define (make-json obj)
  (vector json-tag obj))

(define (json-unwrap j)
  (if (json? j)
      (vector-ref j 1)
      (error "Not a JSON object")))

(define (json-dict . pairs)
  (make-json (list->vector pairs)))

(define (pair key val)
  (cons key val))

(define (json-key json key)
  (let ((dict (json-unwrap json)))
    (let loop ((i 0))
      (if (= i (vector-length dict))
          #f
          (let ((entry (vector-ref dict i)))
            (if (equal? (car entry) key)
                (cdr entry)
                (loop (+ i 1))))))))

;;; === JSON Parser ===

(define (string->json str)
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

  (define (parse-null) (if (match "null") #f (error "Invalid: expected null")))
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
          (begin (advance) (list->vector (reverse acc)))
          (let ((v (parse-value)))
            (skip-whitespace)
            (if (char=? (peek) #\,)
                (begin (advance) (loop (cons v acc)))
                (begin
                  (skip-whitespace)
                  (if (char=? (peek) #\])
                      (begin (advance) (list->vector (reverse (cons v acc))))
                      (error "Expected , or ]"))))))))

  (define (parse-object)
    (if (not (char=? (peek) #\{)) (error "Expected {"))
    (advance)
    (skip-whitespace)
    (let loop ((acc '()))
      (skip-whitespace)
      (if (char=? (peek) #\})
          (begin (advance) (make-json (list->vector (reverse acc))))
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
                      (begin (advance) (make-json (list->vector (reverse (cons (cons key val) acc)))))
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
          ((match "null") #f)
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

(define (json->string j)
  (define (emit v)
    (cond ((json? v)
           (let ((fields (vector->list (json-unwrap v))))
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
          ((vector? v)
           (string-append "[" (string-join (map emit (vector->list v)) ", ") "]"))
          ((not v) "null")
          (else (error "Unsupported value"))))
  (emit j))

;;; === JSON File I/O Helpers (MIT Scheme Compatible) ===

;; Read entire file as string
(define (read-file-as-string path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (string-join (reverse lines) "\n")
              (loop (cons line lines))))))))

;; Write string to file (truncates if exists)
(define (write-string-to-file path str)
  (let ((port (open-output-file path)))
    (display str port)
    (close-output-port port)))

;; Read JSON from file
(define (json-read-file path)
  (string->json (read-file-as-string path)))

;; Write JSON to file
(define (json-write-file path j)
  (write-string-to-file path (json->string j)))

;;=== Usage Examples ===

(define j (string->json
  "{ \"event\": \"message-received\", \"message\": { \"sender\": \"John\", \"content\": \"Hello!\", \"timestamp\": 1744573905, \"bot\": false } }"))

(json? j) ;; #t
(json-key j "event") ;; "message-received"
(define m (json-key j "message"))
(json-key m "sender") ;; "John"
(json-key m "bot") ;; #f

(define k (json-dict
  (pair "event" "message-sent")
  (pair "message" (json-dict
    (pair "sender" "Ben Bitdiddle")
    (pair "content" "Hello John!")
    (pair "timestamp" 1744573905)
    (pair "bot" #f)))))

(json->string k)


(define j (json-dict (pair "event" "test") (pair "value" 42)))
(json-write-file "test.json" j)

(define j2 (json-read-file "test.json"))
(json-key j2 "event")  ;; => "test"

;; My test cases should work, let's see
(define s1 "[1, 2, {\"a\": \"b\"}, {\"b\": {}, \"c\": false}, true, null]")
(string->json s1)
(define s2 "{\n  \"preSemester\": {\n    \"urlName\": \"i25\",\n    \"startDate\": \"2025-01-06\",\n    \"endDate\": \"2025-01-31\",\n    \"holidayDates\": [\n      \"2025-01-20\"\n    ]\n  },\n  \"semester\": {\n    \"urlName\": \"s25\",\n    \"startDate\": \"2025-02-03\",\n    \"h1EndDate\": \"2025-03-21\",\n    \"h2StartDate\": \"2025-03-31\",\n    \"endDate\": \"2025-05-13\",\n    \"mondayScheduleDate\": \"2025-02-18\",\n    \"holidayDates\": [\n      \"2025-02-17\",\n      \"2025-03-24\",\n      \"2025-03-25\",\n      \"2025-03-26\",\n      \"2025-03-27\",\n      \"2025-03-28\",\n      \"2025-04-21\"\n    ]\n  }\n}\n")
(string->json s2)
