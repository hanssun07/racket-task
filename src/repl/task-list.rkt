#lang racket/base

(require
    racket/block    racket/match
    racket/function racket/list
    racket/format
    "../model.rkt"
    "../task.rkt"
    "../user.rkt"
    "../utils.rkt"
    "../domain.rkt"
    "utils.rkt")
(provide
    repl-list-tasks
    repl-summary
    list-tasks)

(define repl-list-matchers '("l" "ls" "list"))
(define repl-list-help '(
    ("ls" "list [opts ...]"         "list tasks...")
    (""   " -a all"                 "    repl is all pending and in-progress tasks")
    (""   "    ready blocked"       "    require or exclude ready tasks")
    (""   "    assigned unassigned" "    require or exclude assigned tasks")
    (""   "    done not-done"       "    require or exclude done tasks")
    (""   "    mine"                "    require assigned to me")
    (""   "    <number>"            "    maximum to show")
    (""   " -t by-id"               "    order by ascending id")
    (""   " -p by-priority"         "    order by descending priority score")
    (""   ""                        "  options trigger left-to-right")))
(define (repl-list-handler argc argv) (block
    (define filters (list
        (filter-by task-ready?)
        (filter-by not task-done?)))
    (define (add-filter . args) (set! filters (cons (apply filter-by args) filters)))
    (define by-priority (sort-by > (curry get-user-task-assignment-index (me))))
    (define by-id (sort-by < task-id))
    (define sorter (list by-id by-priority))
    (define n (in-naturals 0))
    (for ((arg (cdr argv)))
        (match arg
            ["blocked" (add-filter not task-ready?)]
            ["pending" (add-filter task-ready?) (add-filter not task-assigned?)]
            [(or "all" "-a") (set! filters empty)]
            ["ready" (add-filter task-ready?)]
            ["assigned" (add-filter task-assigned?)]
            ["unassigned" (add-filter not task-assigned?)]
            ["done" (add-filter task-done?)]
            ["not-done" (add-filter not task-done?)]
            ["mine" (add-filter (curry equal? (user-id (me))) task-assigned-to)]
            [(? number?) (set! n arg)]
            [(or "by-id" "-t") (set! sorter (list by-id))]
            [(or "by-priority" "-p") (set! sorter (list by-id by-priority))]))
    (define tasks (list-truncate (apply query-tasks (append sorter filters)) n))
    (list-tasks tasks)
    (printf "~a/~a tasks shown.\n" (length tasks) (task-count))))

(define repl-summary-matchers '(#f "sum" "summary"))
(define repl-summary-help '(
    (":" "summary" "get a summary of assigned, pending tasks")))
(define (repl-summary-handler argc argv) 
    (define tasks-assigned (apply append
        (for/list ([dmf (in-domain)]) (parameterize ([current-domain-frame dmf])
            (define tasks
                (if (me)
                    (query-tasks (sort-by < task-id)
                                 (filter-by (curry equal? (user-id (me))) task-assigned-to)
                                 (filter-by task-assigned?)
                                 (filter-by not task-done?))
                    empty))
            (define dmstr (dmpath->string (domain-frame-path dmf)))
            (map (curry cons dmstr)
                 (map task->summaryrow tasks))))))
    (define tasks-awaiting-eval (apply append
        (for/list ([dmf (in-domain)])
            (parameterize ([current-domain-frame dmf])
                (if (me)
                    (query-tasks (filter-by (curry user-needs-eval-task? (me))))
                    empty)))))
    (define num-tasks-pending 0)
    (define tasks-pending (apply append
        (for/list ([dmf (in-domain)]) (parameterize ([current-domain-frame dmf])
            (define tasks
                (if (me)
                    (query-tasks (sort-by < task-id)
                                 (sort-by > (curry get-user-task-assignment-index (me)))
                                 (filter-by task-ready?)
                                 (filter-by not task-assigned?))
                    empty))
            (define dmstr (dmpath->string (domain-frame-path dmf)))
            (set! num-tasks-pending (+ num-tasks-pending (length tasks)))
            (map (curry cons dmstr)
                 (map task->summaryrow (list-truncate tasks 5)))))))
    (define tab (append
        (if (empty? tasks-assigned)
            (list (list "" "" "" "" "\rNo tasks in progress."))
            (list (list "" "" "" "" (format "\r~a tasks in progress." (length tasks-assigned)))))
        tasks-assigned
        (if (empty? tasks-awaiting-eval)
            empty
            (list (list "" "" "" "" (format "\r~a tasks awaiting evaluation." (length tasks-awaiting-eval)))))
        (if (zero? num-tasks-pending)
            (list (list "" "" "" "" "\rNo tasks pending."))
            (list (list "" "" "" "" (format "\r~a tasks pending." num-tasks-pending))))
        tasks-pending))
    (print-table tab
        '(1 1 20 4 0)
        '(20 5 60 4 1000)
        '(2 0 1 1 2)
        '(right right left left left)))

(define (task->summaryrow t)
    (list
        (~a (task-id t))
        (~a (task-title t))
        (cond
            [(not (task-ready? t))    "b   "]
            [(not (task-assigned? t)) " r  "]
            [(not (task-done? t))     "  a "]
            [#t                       "   d"])
        (cond
            [(and (task-ready? t) (not (task-assigned? t))) 
             (define p (get-task-priority t))
             (if p (~r (get-user-task-assignment-index (me) t) #:precision 0) "-")]
            [(and (task-assigned? t) (not (task-done? t)))
             (task-assigned-to t)]
            [#t ""])))

(define (list-tasks tasks)
    (define tab (map task->summaryrow tasks))
    (print-table tab
        '(1 20 4 0)
        '(20 60 4 1000)
        '(2 1 1 2)
        '(right left left left)))
 
(define repl-list-tasks (cmdentry repl-list-matchers repl-list-help repl-list-handler))
(define repl-summary (cmdentry repl-summary-matchers repl-summary-help repl-summary-handler))
