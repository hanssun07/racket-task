#lang racket/base

(require
    "utils/ann.rkt"
    racket/list)

(provide assert!! expect!!
    error-failthrough
    list-uniq
    list-truncate)

(: assert!! (Bool -> (U Void (^ Exn:Fail))))
(define-syntax-rule
    (assert!! expr)
    (when (not expr)
        (error (format "assertion failed: ~a" (quote expr)))))

(: expect!! (Bool -> (U Void (^ String))))
(define-syntax-rule
    (expect!! expr fmt xs ...)
    (when (not expr)
        (raise (format fmt xs ...))))

(: error-failthrough
    (String              -> (^ Exn:Fail))
    (String Any          -> (^ Exn:Fail))
    (Symbol String Any * -> (^ Exn:Fail)))
(define ((error-failthrough . args))
    (error args))

(: list-uniq ((Listof a) -> (Listof a)))
(define (list-uniq lst)
    (define (helper x xs)
        (cond
            [(empty? xs) (list x)]
            [(equal? x (car xs)) (helper x (cdr xs))]
            [#t (cons x (helper (car xs) (cdr xs)))]))
    (if (empty? lst) lst (helper (car lst) (cdr lst))))

(:typedef Cardinal (U ExactNonnegativeInteger Sequence))
(: list-truncate ((Listof a) Cardinal -> (Listof a)))
(define (list-truncate lst [n (in-naturals 0)])
    (for/list ([x lst] [_ n]) x))
