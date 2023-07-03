#lang racket/base

(provide
    assert!! expect!!)

(define-syntax-rule
    (assert!! expr)
    (when (not expr)
        (error (format "assertion failed: ~a" (quote expr)))))
(define-syntax-rule
    (expect!! expr fmt xs ...)
    (when (not expr)
        (raise (format fmt xs ...))))


