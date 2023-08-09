#lang racket/base

(require
    "utils/ann.rkt"
    "task.rkt"
    "user.rkt"
    "utils.rkt"
    "domain.rkt"
    racket/list racket/function racket/contract
    racket/string
    racket/block racket/match
)
(provide
    register-task
        get-task    get-task/ui
        next-task-ui-id    task-count
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

(: register-task (Task -> Void))
(: get-task (TaskId -> (U Task (^ Exn:Fail))))
(: next-task-ui-id (-> UiId))
(: task-count (-> ExactNonnegativeInteger))
(define (register-task t [dmpath empty])
    (define ns (resolve-domain dmpath))
    (domain/register-task ns t))
(define (get-task tid [dmpath empty])
    (domain/get-task (resolve-domain dmpath) tid))
(define (get-task/ui tuiid [dmpath empty])
    (domain/get-task/ui (resolve-domain dmpath) tuiid))
(define (next-task-ui-id [dmpath empty])
    (domain/next-task-ui-id (resolve-domain dmpath)))
(define (task-count [dmpath empty])
    (domain/task-count (resolve-domain dmpath)))

(: register-user (User -> Void))
(: get-user-by-id (UserId -> (U User (^ Exn:Fail))))
(: get-user-by-name
    (String DomainPath (-> f) -> (U User f))
    (String DomainPath f      -> (U User f))
    (String                   -> (U User (^ Exn:Fail))))
(: next-user-id (-> UserId))
(: get-user-me (-> (? User)))
(define (register-user u [dmpath empty])
    (define ns (resolve-domain dmpath))
    (domain/register-user ns u))
(define (get-user-by-id uid [dmpath empty])
    (domain/get-user-by-id (resolve-domain dmpath) uid))
(define (get-user-by-name name
         [dmpath empty]
         [failure-result (error-failthrough "no user by name ~a" name)])
    (domain/get-user-by-name (resolve-domain dmpath) name failure-result))
(define (next-user-id [dmpath empty])
    (domain/next-user-id (resolve-domain dmpath)))
(define (get-user-me [dmpath empty])
    (domain-cur-user (resolve-domain dmpath)))

(:structdef uteval : EvalRecord
    ([interest      : (? UserEval)]
     [priority      : (? UserEval)]
     [needs-refinement : (? UserEval)]))
(: get-user-task-evals (User Task -> EvalRecord))
(struct uteval (interest priority needs-refinement))
(define (get-user-task-evals u t)
    (assert!! (task? t))
    (assert!! (user? u))
    (assert!! (eq? (domain-of u) (domain-of t)))
    (uteval (user-interest u (task-id t))
            (user-priority u (task-id t))
            (user-task-needs-refinement? u (task-id t))))

(: get-task-evals (Task -> (Hash User EvalRecord)))
(define (get-task-evals t)
    (assert!! (task? t))
    (define users (domain/users (domain-of t)))
    (for/hash ((u users))
        (values (user-id u)
                (get-user-task-evals u t))))

(: get-task-priority (Task -> NonnegativeNumber))
(define (get-task-priority t)
    (assert!! (task? t))
    (define evals (get-task-evals t))
    (define vals (filter values (map uteval-priority (hash-values evals))))
    (if (empty? vals) #f
        (/ (apply + vals) (length vals))))

(: get-user-task-assignment-index (User Task -> NonnegativeNumber))
(define (get-user-task-assignment-index u t)
    (assert!! (task? t))
    (assert!! (user? u))
    (assert!! (eq? (domain-of u) (domain-of t)))
    (define base-priority (get-task-priority t))
    (define interest (user-interest u (task-id t)))
    (define priority (user-priority u (task-id t)))
    (define status-value (cond
        [(task-closed? t)     0]
        [(task-done? t)     200]
        [(task-started? t)  800]
        [(task-ready? t)    600]
        [#t                 400]))
    (define mine-value (and (task-assigned-to-user? t (user-id u)) 100))
    (define eval-value (and base-priority interest priority
        (* interest (+ priority (* 2 base-priority)))))
    (+ (or status-value 0)
       (or mine-value 0)
       (or eval-value 0)))

(: filter-by ((Any -> Any) * -> Filterer))
(: sort-by ((Any Any -> Bool) (Any -> Any) * -> Sorter))
(: query-tasks ((U Filterer Sorter) -> (Listof Task)))
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

(: user-needs-eval-task? (User Task -> Bool))
(define (user-needs-eval-task? u t)
    (assert!! (task? t))
    (assert!! (user? u))
    (assert!! (eq? (domain-of u) (domain-of t)))
    (define id (task-id t))
    (and
        (task-ready? t)
        (not (task-resolved? t))
        (not (and (user-interest u id) (user-priority u id) (user-task-needs-refinement? u id)))))

