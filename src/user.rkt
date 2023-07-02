#lang racket/base

(require 
    "utils.rkt"
    racket/match racket/list)
(provide
    make-user user?
    user-id user-name
    user-set-interest!  user-interest
    user-set-priority!  user-priority
    user-set-needs-refinement!
        user-task-needs-refinement?
    user->datum datum->user)

(struct user
    (id
     name
     task-id->interest
     task-id->priority
     task-id->needs-refinement))

(define (make-user id name)
    (assert!! (exact-nonnegative-integer? id))
    (assert!! (string? name))
    (user id name (make-hash) (make-hash) (make-hash)))

(define (user-set-interest! u id interest)
    (assert!! (or (not interest) (<= 0 interest 4)))
    (assert!! (exact-nonnegative-integer? id))
    (hash-set! (user-task-id->interest u)
               id interest))
(define (user-interest u id)
    (assert!! (exact-nonnegative-integer? id))
    (hash-ref (user-task-id->interest u) id #f))

(define (user-set-priority! u id priority)
    (assert!! (exact-nonnegative-integer? id))
    (assert!! (or (not priority) (<= 0 priority 4)))
    (hash-set! (user-task-id->priority u)
               id priority))
(define (user-priority u id)
    (assert!! (exact-nonnegative-integer? id))
    (hash-ref (user-task-id->priority u) id #f))

(define (user-set-needs-refinement! u id nr?)
    (assert!! (exact-nonnegative-integer? id))
    (assert!! (or (not nr?) (= 0 nr?) (= 1 nr?)))
    (hash-set! (user-task-id->needs-refinement u)
               id nr?))
(define (user-task-needs-refinement? u id)
    (assert!! (exact-nonnegative-integer? id))
    (hash-ref (user-task-id->needs-refinement u) id #f))

(define (user->datum u)
    (match-define (user id name >i >p >nr) u)
    (define task-ids (list-uniq (sort (append (hash-keys >i) (hash-keys >p) (hash-keys >nr)) <)))
    (define (make k) (list k (hash-ref >i k #f)
                             (hash-ref >p k #f)
                             (hash-ref >nr k #f)))
    (define (useful-record? r) (or (second r) (third r) (fourth r)))
    (append `(,id ,name) (filter useful-record? (map make task-ids))))

(define (datum->user d)
    (match-define `(,id ,name . ,evals) d)
    (define u (make-user id name))
    (for ((eval evals))
        (match-define (list id i p nr) eval)
        (user-set-interest! u id i)
        (user-set-priority! u id p)
        (user-set-needs-refinement! u id nr))
    u)
