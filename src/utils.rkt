#lang racket/base

(require
    "utils/ann.rkt"
    racket/list racket/function
    racket/block)

(provide assert!! expect!!
    error-failthrough
    unwrap-const
    if* if**
    list-uniq
    list-truncate
    list-inner-merge
    datum-rec-transform)

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
    (String              -> (-> (^ Exn:Fail)))
    (String Any          -> (-> (^ Exn:Fail)))
    (Symbol String Any * -> (-> (^ Exn:Fail))))
(define ((error-failthrough . args))
    (apply error args))

(: unwrap-const ((-> v) -> v)
                (v      -> v))
(define (unwrap-const ->v)
    (if (procedure? ->v) (->v) ->v))

(define-syntax-rule
    (if* test texp fbody0 fbody ...)
    (if test texp (block fbody0 fbody ...)))

(define-syntax-rule
    (if** test (tbody0 tbody ...) fbody0 fbody ...)
    (if test (block tbody0 tbody ...) (block fbody0 fbody ...)))

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

(: datum-rec-transform (Any (Any -> Any) -> Any))
(define (datum-rec-transform datum fn)
    (if (list? datum)
        (map (curryr datum-rec-transform fn) datum)
        (fn datum)))

(: list-inner-merge ((a a -> (Listof a)) (Listof a) -> (Listof a)))
(define (list-inner-merge merger lst)
    (let loop ([lst-rev (reverse lst)] [res '()])
        (if* (empty? lst-rev) res
        (define a (car lst-rev))
        (define a-rest (cdr lst-rev))
        (if* (empty? res) (loop a-rest (cons a res))
        (define b (car res))
        (define b-rest (cdr res))
        (define merged (merger a b))
        (loop a-rest (append merged b-rest))))))
