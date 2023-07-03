#lang typed/racket/base

(provide
    Timestamp
    Task-ID
    User-ID
    Eval-Rating
)

(define-type Timestamp Exact-Nonnegative-Integer)
(define-type Task-ID Exact-Nonnegative-Integer)
(define-type User-ID Exact-Nonnegative-Integer)
(define-type Eval-Rating (Option Exact-Nonnegative-Integer))
