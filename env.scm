(define do-run-env-tests #f)
(define (run-env-test result)
    (if do-run-env-tests
        result
        #t))
(define (run-env-tests . results)
    (map run-env-test results)
)

(define (read-file filename)
    (let ((file-port (open-input-file filename)))
        (read-string (char-set) file-port)))

;; Given a string, return a list of strings separated in the original string by `delimiter`
;; string-to-split and delimiter must not be the empty string
(define (split-string string-to-split delimiter)
    (define (loop string string-list start-index)
        (let ((next-index (string-search-forward delimiter string)))
            (if (not next-index)
                (append string-list (list string))
                (loop
                    (substring string (+ next-index (string-length delimiter)))
                    (append string-list (list (substring string start-index next-index))) 
                    (+ next-index (string-length delimiter))
                ))))
    (loop string-to-split (list) 0))

;; Tests
(run-env-tests
    (split-string "asdf" "9")
    (split-string "asdf--jsdf" "--")
    (split-string "key=value" "=")
    (split-string "key1=value1\nkey2=value2" "\n")
    (split-string (read-file ".env") "\n"))


;; Parses a .env file to a list of pairs
(define (parse-env env-string)
    (let ((keyvals (split-string env-string "\n")))
            (map (lambda (string) (split-string string "=")) keyvals))
)

;; Tests
(run-env-test
    (parse-env (read-file ".env")))

;; Fetches a variable from the .env file
(define (get-from-env var #!optional env-filename)
    (let ((env-filename (if (default-object? env-filename) ".env" env-filename)))
        (let ((pairs (parse-env (read-file env-filename))))
            (cadr (assoc var pairs)))))
;; Tests
(run-env-test
    (get-from-env "discord-token"))

;; TODO: maybe plop these into the environment with a singular command to avoid re-parsing & ease development
