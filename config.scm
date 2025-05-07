;;; Config parsing

(define (read-sexp-from-file file)
  (call-with-input-file file
    (lambda (port)
      (read port))))

;; Parse an identifier like `(discord "787146644264189975")`
(define (parse-identifier identifier)
  (define (%parse-identifier platform . args)
    (let ((arg-or-args
	   (if (= (length args) 1)
	       (first args)
	       args)))
      (make-identifier platform arg-or-args)))
  (guarantee list? identifier)
  (assert (> (length identifier) 1))
  (apply %parse-identifier identifier))

;; Parse a set of linked chats like
;; (linked (discord "787146644264189975")
;;	   (discord "820354450073714688")
;;	   (slack "C08PR8QQU93"))
;; Output: A list of <identifier>
(define (parse-linked-chats linked-chats)
  (guarantee (list-beginning-with? 'linked) linked-chats)
  (map parse-identifier (cdr linked-chats)))

;; Parses a list of sets of linked chats like (bridge (linked ...) (linked ...) ...)
(define (parse-bridge linked-chats-list)
  (guarantee (list-beginning-with? 'bridge) linked-chats-list)
  (map parse-linked-chats (cdr linked-chats-list)))

(define *config-constructors* (make-weak-eqv-hash-table))
(define (register-config-constructor! platform config-predicate #!optional defaults)
  (let ((defaults (if (default-object? defaults) '() defaults)))
    (let ((constructor
	   (type-instantiator-with-defaults config-predicate
					    `(platform-id ,platform ,@defaults))))
      (hash-table-set! *config-constructors* platform constructor))))
;; Parse the options specific to a platform
(define (get-platform-config-constructor platform)
  (hash-table-ref *config-constructors* platform))

(define (env-fetch option)
    (if (and (list? option) (eq? (car option) 'env))
        (get-from-env (cadr option))
        option))

;; Parse platform-specific options like
;;(discord
;; token "my token"
;; custom-status "I am a bot!")
(define (parse-platform platform-options)
  (define (%parse-platform platform . plist)
    (let ((constructor (get-platform-config-constructor platform))
          (env-fetched (map env-fetch plist))) ; Parse (env "env-var")
      (apply constructor env-fetched)))
  (apply %parse-platform platform-options))

;; Associative list of all clients (hash table would be overkill)
(define *all-clients* '())
(define (get-client platform)
  (cdr (assoc platform *all-clients*)))

;; TODO: it does not actually validate that the platforms you give under `bridge` actually exist below
(define (load-config! config)
  (guarantee (list-beginning-with? 'config) config)
  ;; The first element of config must be the linked chats, then all the platform-specific options
  (let* ((config-bridge (cadr config))
	 (config-platforms (cddr config))
	 (platform-options-list (map parse-platform config-platforms))
	 (clients (map make-client! platform-options-list)))
    (load-linked-chats! (parse-bridge config-bridge))
    (set! *all-clients*
	  (map (lambda (client) (cons (client-platform client) client)) clients))))

(define (load-config-file! file)
  (load-config! (read-sexp-from-file file)))
