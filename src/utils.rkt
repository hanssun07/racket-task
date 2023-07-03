#lang typed/racket/base

(require
    "types.rkt"
    racket/list)

(provide
    error-failthrough
    seconds-since-epoch
    list-uniq
    list-truncate)

(: error-failthrough (String Any * -> (-> Nothing)))
(define ((error-failthrough fmt . vs))
    (error (apply format fmt vs)))

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

(: list-truncate (All (T) ((Listof T) (Sequenceof Any) -> (Listof T))))
(define (list-truncate lst [n (in-naturals 0)])
    (for/list ([x lst] [_ n]) x))

