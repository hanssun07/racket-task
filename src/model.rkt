#lang typed/racket/base/shallow

(require
    "task.rkt"
    "user.rkt"
    "utils.rkt"
    "utils-untyped.rkt"
    "domain.rkt"
    "types.rkt"
    racket/list racket/function
    racket/string
    racket/block racket/match
)
(provide
    register-task   get-task    next-task-id    task-count
    register-user
        get-user-by-id
        get-user-by-name
        next-user-id
        get-user-me
    (struct-out uteval)
        get-user-task-evals
        get-task-evals
    get-task-priority
    get-user-task-assignment-index
    user-needs-eval-task?
    query-tasks
        filter-by   sort-by)

(: register-task (->* (Task) (DomainPath) Void))
(define (register-task t [dmpath empty])
    (define ns (resolve-domain dmpath))
    (domain/register-task ns t))
(: get-task (->* (Task-ID) (DomainPath) Task))
(define (get-task tid [dmpath empty])
    (domain/get-task (resolve-domain dmpath) tid))
(: next-task-id (->* () (DomainPath) Task-ID))
(define (next-task-id [dmpath empty])
    (domain/next-task-id (resolve-domain dmpath)))
(: task-count (->* () (DomainPath) Exact-Nonnegative-Integer))
(define (task-count [dmpath empty])
    (domain/task-count (resolve-domain dmpath)))

(: register-user (->* (User) (DomainPath) Void))
(define (register-user u [dmpath empty])
    (define ns (resolve-domain dmpath))
    (domain/register-user ns u))
(: get-user-by-id (->* (User-ID) (DomainPath) User))
(define (get-user-by-id uid [dmpath empty])
    (domain/get-user-by-id (resolve-domain dmpath) uid))
(: get-user-by-name (All (T) (->* (String) (DomainPath (-> T)) (U T User))))
(define (get-user-by-name name
         [dmpath empty]
         [failure-result (error-failthrough "no user by name ~a" name)])
    (domain/get-user-by-name (resolve-domain dmpath) name failure-result))
(: next-user-id (->* () (DomainPath) User-ID))
(define (next-user-id [dmpath empty])
    (domain/next-user-id (resolve-domain dmpath)))
(: get-user-me (->* () (DomainPath) (Option User)))
(define (get-user-me [dmpath empty])
    (domain-cur-user (resolve-domain dmpath)))

(struct uteval
    ([interest          : Eval-Rating]
     [priority          : Eval-Rating]
     [needs-refinement  : Eval-Rating])
    #:type-name UserTaskEval)
(: get-user-task-evals : User Task -> UserTaskEval)
(define (get-user-task-evals u t)
    (assert!! (eq? (domain-of u) (domain-of t)))
    (uteval (user-interest u (task-id t))
            (user-priority u (task-id t))
            (user-task-needs-refinement? u (task-id t))))
    
(: get-task-evals : Task -> (Immutable-HashTable User-ID UserTaskEval))
(define (get-task-evals t)
    (define users (domain/users (domain-of t)))
    (for/hash ((u users)) : (Immutable-HashTable User-ID UserTaskEval)
        (values (user-id u)
                (get-user-task-evals u t))))

(: get-task-priority : Task -> (Option Nonnegative-Real))
(define (get-task-priority t)
    (define evals (get-task-evals t))
    (define vals (cast (filter values (map uteval-priority (hash-values evals)))
                       (Listof Nonnegative-Real)))
    (if (empty? vals) #f
        (cast (/ (apply + vals) (length vals)) Nonnegative-Real)))

(: get-user-task-assignment-index : User Task -> Nonnegative-Real)
(define (get-user-task-assignment-index u t)
    (assert!! (eq? (domain-of u) (domain-of t)))
    (define base-priority (get-task-priority t))
    (define interest (user-interest u (task-id t)))
    (define priority (user-priority u (task-id t)))
    (define assigned-value (and (task-assigned? t) 48))
    (define mine-value (and (equal? (task-assigned-to t) (user-id u)) 48))
    (define base-value (and base-priority interest priority
        (* interest (+ priority (* 2 base-priority)))))
    (+  (or assigned-value 0)
        (or mine-value 0)
        (or base-value 0)))

(: user-needs-eval-task? : User Task -> Any)
(define (user-needs-eval-task? u t)
    (assert!! (eq? (domain-of u) (domain-of t)))
    (define id (task-id t))
    (and
        (task-ready? t)
        (not (task-assigned? t))
        (not (and (user-interest u id) (user-priority u id) (user-task-needs-refinement? u id)))))

(module query racket/base
    (require
        "domain.rkt"
        racket/list)
    (provide
        filter-by sort-by
        query-tasks
        _filter-by? _sort-by?)

    (struct _filter-by (pred) #:transparent)
    (define (filter-by . fns)
        (_filter-by (apply compose fns)))
    (struct _sort-by (< key) #:transparent)
    (define (sort-by < . fns)
        (_sort-by < (apply compose fns)))

    (define (tasks-under-domain dmf)
        (define dm (domain-frame-in-domain dmf))
        (apply append (if dm (domain/tasks dm) empty)
            (map tasks-under-domain (hash-values (domain-frame-subdomains dmf)))))
    (define (query-tasks #:recursive? [recursive? #f] #:domain-frame [dmf (current-domain-frame)]
                 . conditions)
        (define tasks (if recursive?
            (tasks-under-domain dmf)
            (let ([dm (domain-frame-in-domain dmf)])
                (if dm (domain/tasks dm) empty))))
        (define filterers (filter _filter-by? conditions))
        (define after-filter
            (foldl filter tasks (map _filter-by-pred filterers)))
        (define sorters (filter _sort-by? conditions))
        (define after-sort
            (foldl (lambda (< key xs) (sort xs < #:key key #:cache-keys? #t))
                   after-filter
                   (map _sort-by-< sorters)
                   (map _sort-by-key sorters)))
        after-sort)
)
(require/typed/provide 'query
    [#:opaque Filter-By _filter-by?]
    [#:opaque Sort-By _sort-by?]
    ;[filter-by ((Any -> Any) (Any -> Any) * -> Filter-By)]
    ;[sort-by (All (T) (T T -> Any) (Any -> T) (Any -> Any) * -> Sort-By)]
    [filter-by (Procedure Procedure * -> Filter-By)]
    [sort-by (Procedure Procedure Procedure * -> Sort-By)]
    [query-tasks (->* () (#:recursive? Boolean #:domain-frame DomainFrame)
                      #:rest (U Filter-By Sort-By)
                      (Listof Task))])
