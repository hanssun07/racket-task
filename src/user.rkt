#lang typed/racket/base

(require 
    "utils.rkt"
    "utils-untyped.rkt"
    "types.rkt"
    racket/match racket/list)
(provide
    User    make-user user?
    user-id user-name
    user-set-interest!  user-interest
    user-set-priority!  user-priority
    user-set-needs-refinement!
        user-task-needs-refinement?
    user->datum datum->user)

(struct user
    ([id                        : User-ID]
     [name                      : String]
     [task-id->interest         : (Mutable-HashTable Task-ID Eval-Rating)]
     [task-id->priority         : (Mutable-HashTable Task-ID Eval-Rating)]
     [task-id->needs-refinement : (Mutable-HashTable Task-ID Eval-Rating)])
    #:type-name User)

(: make-user (User-ID String -> User))
(define (make-user id name)
    (user id name (make-hash) (make-hash) (make-hash)))

(: user-set-interest! (User Task-ID Eval-Rating -> Void))
(define (user-set-interest! u id interest)
    (assert!! (or (not interest) (<= 0 interest 4)))
    (hash-set! (user-task-id->interest u)
               id interest))
(: user-interest (User Task-ID -> Eval-Rating))
(define (user-interest u id)
    (assert!! (exact-nonnegative-integer? id))
    (hash-ref (user-task-id->interest u) id #f))

(: user-set-priority! (User Task-ID Eval-Rating -> Void))
(define (user-set-priority! u id priority)
    (assert!! (exact-nonnegative-integer? id))
    (assert!! (or (not priority) (<= 0 priority 4)))
    (hash-set! (user-task-id->priority u)
               id priority))
(: user-priority (User Task-ID -> Eval-Rating))
(define (user-priority u id)
    (assert!! (exact-nonnegative-integer? id))
    (hash-ref (user-task-id->priority u) id #f))

(: user-set-needs-refinement! (User Task-ID Eval-Rating -> Void))
(define (user-set-needs-refinement! u id nr?)
    (assert!! (exact-nonnegative-integer? id))
    (assert!! (or (not nr?) (= 0 nr?) (= 1 nr?)))
    (hash-set! (user-task-id->needs-refinement u)
               id nr?))
(: user-task-needs-refinement? (User Task-ID -> Eval-Rating))
(define (user-task-needs-refinement? u id)
    (assert!! (exact-nonnegative-integer? id))
    (hash-ref (user-task-id->needs-refinement u) id #f))

(define-type Eval-Record
    (List Task-ID Eval-Rating Eval-Rating Eval-Rating))
(define-type User-Datum
    (List* User-ID String (Listof Eval-Record)))

(: user->datum (User -> User-Datum))
(define (user->datum u)
    (match-define (user id name >i >p >nr) u)
    (: task-ids (Listof Task-ID))
    (define task-ids (list-uniq (sort (append (hash-keys >i) (hash-keys >p) (hash-keys >nr)) <)))
    (: make (Task-ID -> Eval-Record))
    (define (make k) (list k (hash-ref >i k #f)
                             (hash-ref >p k #f)
                             (hash-ref >nr k #f)))
    (: useful-record? (Eval-Record -> Any))
    (define (useful-record? r) (or (second r) (third r) (fourth r)))
    `(,id ,name . ,(filter useful-record? (map make task-ids))))

(: datum->user (Any -> User))
(define (datum->user d)
    (match-define `(,id ,name . ,evals) (cast d User-Datum))
    (define u (make-user id name))
    (for ((eval : Eval-Record evals))
        (match-define (list id i p nr) eval)
        (user-set-interest! u id i)
        (user-set-priority! u id p)
        (user-set-needs-refinement! u id nr))
    u)
