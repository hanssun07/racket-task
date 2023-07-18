#lang racket/base

(provide
    : :typedef :structdef
)

(define-syntax-rule
    (: args ...)
    (begin))
(define-syntax-rule
    (:typedef args ...)
    (begin))
(define-syntax-rule
    (:structdef args ...)
    (begin))
