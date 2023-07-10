#lang racket/base

(require
    racket/block
    racket/function
)
(provide
    assert!! expect!!
    if* if**
    errorthunk
)

(define-syntax-rule
    (assert!! expr)
    (when (not expr)
        (error (format "assertion failed: ~a" (quote expr)))))
(define-syntax-rule
    (expect!! expr fmt xs ...)
    (when (not expr)
        (raise (format fmt xs ...))))

(define-syntax-rule
    (if* test te fbody0 fbody ...)
    (if test te (block fbody0 fbody ...)))
(define-syntax-rule
    (if** test (tb ...) fb ...)
    (if* test (block tb ...) fb ...))

(define-syntax-rule
    (errorthunk . args)
    (thunk (error . args)))

