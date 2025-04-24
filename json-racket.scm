;; An implementation of JSON parsing that delegates all the parsing to Racket

(load-option 'synchronous-subprocess)

;; For here, a jsexpr is either something like a boolean, string, etc, or
;; (cons 'dict alist) or (cons 'list list)

(define (string->jsexpr str)
  (read (open-input-string ;; or string->input-port
	 (call-with-output-string
	  (lambda (output-port)
	    (run-synchronous-subprocess "racket" '("util/from-json.rkt")
					'input (open-input-string str)
					'output output-port))))))

(define (jsexpr->string jsexpr)
  (call-with-output-string
   (lambda (racket-output)
     (run-synchronous-subprocess "racket" '("util/to-json.rkt")
				 'input (open-input-string
					 (call-with-output-string
					  (lambda (output-port) (write jsexpr output-port))))
				 'output racket-output))))

(define (json-dict? expr)
  (and
   (pair? expr)
   (eqv? (car expr) 'dict)
   (alist? (cdr expr))))

(define (json-list? expr)
  (and
   (pair? expr)
   (eqv? (car expr) 'list)
   (list? (cdr expr))))

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

;; Some test cases

#|
(define s1 "[1, 2, {\"a\": \"b\"}, {\"b\": {}, \"c\": false}, true, null]")
(define j1 (string->jsexpr s1))
(write j1) (newline)
(define c1 (jsexpr->string j1))
(display c1) (newline)

(define s2 "{\n  \"preSemester\": {\n    \"urlName\": \"i25\",\n    \"startDate\": \"2025-01-06\",\n    \"endDate\": \"2025-01-31\",\n    \"holidayDates\": [\n      \"2025-01-20\"\n    ]\n  },\n  \"semester\": {\n    \"urlName\": \"s25\",\n    \"startDate\": \"2025-02-03\",\n    \"h1EndDate\": \"2025-03-21\",\n    \"h2StartDate\": \"2025-03-31\",\n    \"endDate\": \"2025-05-13\",\n    \"mondayScheduleDate\": \"2025-02-18\",\n    \"holidayDates\": [\n      \"2025-02-17\",\n      \"2025-03-24\",\n      \"2025-03-25\",\n      \"2025-03-26\",\n      \"2025-03-27\",\n      \"2025-03-28\",\n      \"2025-04-21\"\n    ]\n  }\n}\n")
(define j2 (string->jsexpr s2))
(write j2) (newline)
(define c2 (jsexpr->string j2))
(display c2) (newline)
|#
