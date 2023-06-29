#lang racket/base

(require
    racket/list)

(provide assert!! expect!!
    error-failthrough
    list-uniq
    list-truncate)

(define-syntax-rule
    (assert!! expr)
    (when (not expr)
        (error (format "assertion failed: ~a" (quote expr)))))
(define-syntax-rule
    (expect!! expr fmt xs ...)
    (when (not expr)
        (raise (format fmt xs ...))))

(define ((error-failthrough fmt . vs))
    (error (apply fmt vs)))

(define (list-uniq lst)
    (define (helper x xs)
        (cond
            [(empty? xs) (list x)]
            [(equal? x (car xs)) (helper x (cdr xs))]
            [#t (cons x (helper (car xs) (cdr xs)))]))
    (if (empty? lst) lst (helper (car lst) (cdr lst))))

(define (list-truncate lst [n (in-naturals 0)])
    (for/list ([x lst] [_ n]) x))

