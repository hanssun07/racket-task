#lang racket/base

(require
    "model.rkt"
    "domain.rkt"

    racket/file
    racket/match
    racket/block
    racket/string
    racket/list
)
(provide startup)

(define configfile "task.config")

(define (startup)
    (if (file-exists? configfile)
        (startup-from-config)
        (startup-from-default)))

(define (startup-from-default)
    (define base-domain (make-domain "data.dat"))
    (domain/load base-domain)
    (register-domain '() base-domain))

(define login-list '())
(define (startup-from-config) (block
    (define input (file->list configfile))

    (define domain-specs (filter (lambda (x) (equal? 'domain (car x))) input))
    (for-each load-domain domain-specs)

    (define options (make-immutable-hash input))
    (when (hash-has-key? options 'home-domain)
        (select-domain (car (hash-ref options 'home-domain))))

    (unless (empty? login-list)
        (set! login-list (reverse login-list))
        (define offset (+ (string-length (caar login-list)) 13))
        (printf "Logged in as ~a~a" (caar login-list) (cadar login-list))
        (for ((login (cdr login-list)))
            (printf ",\n~a~a~a" (make-string (max 0 (- offset (string-length (car login)))) #\space) (car login) (cadr login)))
        (printf "\n"))))

(define (load-domain spec)
    (match-define `(domain ,rawpath ,args ...) spec)
    (define dmpath (itempath-dmpath (string->itempath (symbol->string rawpath))))
    (define options (make-immutable-hash args))
    (define domain (make-domain))
    (when (hash-has-key? options 'datafile)
        (set-domain-datafile! domain (car (hash-ref options 'datafile)))
        (domain/load domain))
    (when (hash-has-key? options 'login)
        (define userid (car (hash-ref options 'login)))
        (domain/login domain userid)
        (set! login-list (cons (list (symbol->string rawpath) userid) login-list)))
    (register-domain dmpath domain))
    
    

