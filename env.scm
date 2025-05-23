(define (read-file filename)
    (if (or (not (file-exists? filename)) (eq? (file-length filename) 0))
        ""
        (let ((file-port (open-input-file filename)))
            (read-string (char-set) file-port))))
        

;; Given a string, return a list of strings separated in the original string by `delimiter`
;; string-to-split and delimiter must not be the empty string
(define (split-string string-to-split delimiter)
    (define (loop string string-list)
        (let ((next-index (string-search-forward delimiter string)))
            (if (not next-index)
                (append string-list (list string))
                (loop
                    (substring string (+ next-index (string-length delimiter)))
                    (append string-list (list (substring string 0 next-index))) 
                ))))
    (loop string-to-split (list)))

;; Parses a .env file to a list of pairs
(define (parse-env env-string)
    (let ((keyvals (split-string env-string "\n")))
            (map (lambda (string) (split-string string "=")) keyvals))
)

;; Fetches a variable from the .env file
(define (get-from-env key #!optional env-filename)
    (let ((env-filename (if (default-object? env-filename) ".env" env-filename)))
        (let ((pairs (parse-env (read-file env-filename))))
            (let ((val (assoc key pairs)))
                (if (not val)
                    val
                    (cadr val))))))

;; TODO: maybe plop these into real environment variables with a singular command to avoid re-parsing
