#lang racket/base

(require
    "task.rkt"
    "user.rkt"
    "utils.rkt"
    "domain.rkt"
    racket/list racket/function racket/contract
    racket/string
    racket/block racket/match
)
(provide
    register-task   get-task    next-task-id    task-count
    register-user   get-user    get-user-me
    (struct-out uteval)
        get-user-task-evals
        get-task-evals
    get-task-priority
    get-user-task-assignment-index
    user-needs-eval-task?
    query-tasks
        filter-by   sort-by)

(define (register-task t [dmpath empty])
    (define ns (resolve-domain dmpath))
    (domain/register-task ns t))
(define (get-task tid [dmpath empty])
    (domain/get-task (resolve-domain dmpath) tid))
(define (next-task-id [dmpath empty])
    (domain/next-task-id (resolve-domain dmpath)))
(define (task-count [dmpath empty])
    (domain/task-count (resolve-domain dmpath)))

(define (register-user u [dmpath empty])
    (define ns (resolve-domain dmpath))
    (domain/register-user ns u))
(define (get-user uid [dmpath empty])
    (domain/get-user (resolve-domain dmpath) uid))
(define (get-user-me [dmpath empty])
    (domain-cur-user (resolve-domain dmpath)))

(struct uteval (interest priority needs-refinement))
(define (get-user-task-evals u t)
    (assert!! (task? t))
    (assert!! (user? u))
    (assert!! (eq? (domain-of u) (domain-of t)))
    (uteval (user-interest u (task-id t))
            (user-priority u (task-id t))
            (user-task-needs-refinement? u (task-id t))))
    
(define (get-task-evals t)
    (assert!! (task? t))
    (define users (domain/users (domain-of t)))
    (for/hash ((u users))
        (values (user-id u)
                (get-user-task-evals u t))))

(define (get-task-priority t)
    (assert!! (task? t))
    (define evals (get-task-evals t))
    (define vals (filter values (map uteval-priority (hash-values evals))))
    (if (empty? vals) #f
        (/ (apply + vals) (length vals))))

(define (get-user-task-assignment-index u t)
    (assert!! (task? t))
    (assert!! (user? u))
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

(struct _filter-by (pred))
(define (filter-by . fns)
    (_filter-by (apply compose fns)))
(struct _sort-by (< key))
(define (sort-by < . fns)
    (_sort-by < (apply compose fns)))
(define (tasks-under-domain dmf)
    (apply append (domain/tasks (domain-frame-in-domain dmf))
        (map tasks-under-domain (hash-values (domain-frame-subdomains)))))
(define (query-tasks #:recursive? [recursive? #f] #:domain-frame [dmf (current-domain-frame)]
             . conditions)
    (define tasks (if recursive?
        (tasks-under-domain dmf)
        (domain/tasks (domain-frame-in-domain dmf))))
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

(define (user-needs-eval-task? u t)
    (assert!! (task? t))
    (assert!! (user? u))
    (assert!! (eq? (domain-of u) (domain-of t)))
    (define id (task-id t))
    (and
        (task-ready? t)
        (not (task-assigned? t))
        (not (and (user-interest u id) (user-priority u id) (user-task-needs-refinement? u id)))))
