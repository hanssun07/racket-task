#lang racket/base

(provide
    : :typedef :structdef)

(define-syntax-rule
    (: name types ...)
    (begin))
(define-syntax-rule
    (:typedef cons type)
    (begin))
(define-syntax :structdef
    (syntax-rules (:)
        [(_ name : tname ([fname : ftype] ...))
         (begin)]))
