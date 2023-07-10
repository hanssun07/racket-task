#lang typed/racket/base

(require
    "types.rkt"
    racket/list
    racket/block)

(provide
    with-casts
    seconds-since-epoch
    list-uniq
    list-truncate Cardinality cardinal-infty)

(define-syntax with-casts (syntax-rules (:)
    [(with-casts ([x : T] ...) body0 body ...)
     (let ([x : T (cast x T)] ...) body0 body ...)]))

(define (seconds-since-epoch) : Timestamp
    (cast (current-seconds) Timestamp))

(: list-uniq (All (T) ((Listof T) -> (Listof T))))
(define (list-uniq lst)
    (: helper (All (T) (T (Listof T) -> (Listof T))))
    (define (helper x xs)
        (cond
            [(empty? xs) (list x)]
            [(equal? x (car xs)) (helper x (cdr xs))]
            [#t (cons x (helper (car xs) (cdr xs)))]))
    (if (empty? lst) lst (helper (car lst) (cdr lst))))

(define-type Cardinality
    (U Exact-Nonnegative-Integer
       (Sequenceof Any)))
       
(: list-truncate (All (T) (Listof T) Cardinality -> (Listof T)))
(define cardinal-infty (in-naturals 0))
(define (list-truncate lst n)
    (for/list ([x lst] [_ n]) x))

