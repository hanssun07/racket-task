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
        (filter-by not task-resolved?)))
    (define (add-filter . args) (set! filters (cons (apply filter-by args) filters)))
    (define by-priority (sort-by > (curry get-user-task-assignment-index (me))))
    (define by-id (sort-by < task-id))
    (define sorter (list by-id by-priority))
    (define n (in-naturals 0))
    (for ((arg (cdr argv)))
        (match arg
            [(or "all" "-a") (set! filters empty)]
            ["blocked" (add-filter not task-ready?)]
            ["ready" (add-filter task-ready?)]
            ["pending" (add-filter not task-started?)]
            ["started" (add-filter task-started?)]
            ["done" (add-filter task-done?)]
            ["not-done" (add-filter not task-done?)]
            ["closed" (add-filter task-closed?)]
            ["not-closed" (add-filter not task-closed?)]
            ["resolved" (add-filter task-resolved?)]
            ["unresolved" (add-filter not task-resolved?)]
            ["mine" (add-filter (curryr task-assigned-to-user? (user-id me)))]
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
    (define rel-dmpath-to (let ([here (current-domain-frame)])
        (lambda (dmf) (dmpath-relative-from dmf here))))
    (define tasks-in-progress (apply append
        (for/list ([dmf (in-domain)]) (parameterize ([current-domain-frame dmf])
            (define tasks
                (if (me)
                    (query-tasks (sort-by < task-id)
                                 (filter-by task-started?)
                                 (filter-by not task-done?)
                                 (filter-by (curryr task-assigned-to-user? (user-id (me)))))
                    empty))
            (define dmstr (dmpath->string (rel-dmpath-to dmf)))
            (map (curry cons dmstr)
                 (map task->summaryrow tasks))))))
    (define tasks-assigned (apply append
        (for/list ([dmf (in-domain)]) (parameterize ([current-domain-frame dmf])
            (define tasks
                (if (me)
                    (query-tasks (sort-by < task-id)
                                 (sort-by > (curry get-user-task-assignment-index (me)))
                                 (filter-by task-ready?)
                                 (filter-by not task-started?)
                                 (filter-by (curryr task-assigned-to-user? (user-id (me)))))
                    empty))
            (define dmstr (dmpath->string (rel-dmpath-to dmf)))
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
                                 (filter-by not task-started?)
                                 (filter-by not task-done?)
                                 (filter-by not (curryr task-assigned-to-user? (user-id (me)))))
                    empty))
            (define dmstr (dmpath->string (rel-dmpath-to dmf)))
            (set! num-tasks-pending (+ num-tasks-pending (length tasks)))
            (map (curry cons dmstr)
                 (map task->summaryrow (list-truncate tasks 5)))))))
    (define tab (append
        (if (empty? tasks-in-progress)
            (list (list "" "" "" "" "\rNo tasks in progress."))
            (list (list "" "" "" "" (format "\r~a tasks in progress." (length tasks-in-progress)))))
        tasks-in-progress
        (if (empty? tasks-assigned)
            (list (list "" "" "" "" "\rNo other tasks assigned."))
            (list (list "" "" "" "" (format "\r~a other tasks assigned." (length tasks-assigned)))))
        tasks-assigned
        (if (empty? tasks-awaiting-eval)
            empty
            (list (list "" "" "" "" (format "\r~a tasks awaiting evaluation." (length tasks-awaiting-eval)))))
        (if (zero? num-tasks-pending)
            (list (list "" "" "" "" "\rNo tasks pending."))
            (list (list "" "" "" "" (format "\r~a tasks pending." num-tasks-pending))))
        tasks-pending))
    (print-table tab
        '(1 1 20 6 0)
        '(20 5 60 6 1000)
        '(2 0 1 1 1)
        '(right right left left left)
        #:elide-repeated? '(#t #f #f #f #f)))

(define (task->summaryrow t)
    (list
        (~a (task-id t))
        (~a (task-title t))
        (format "~a~a~a ~a"
            (if (task-ready? t)     "r" "-")
            (if (task-started? t)   "s" "-")
            (cond [(task-done? t)   "d"]
                  [(task-closed? t) "c"]
                  [#t               "-"])
            (cond
                [(not (me)) ""]
                [(user-needs-eval-task? (me) t) "--"]
                [#t (~r (do ([v (get-user-task-assignment-index (me) t) (- v 100)])
                            ((< v 100) v))
                        #:precision 0 #:min-width 2)]))
        (block
            (define assns (task-assigned-to t))
            (define me? (and (me) (task-assigned-to-user? t (user-id (me)))))
            (define +k (and (< 1 (length assns)) (sub1 (length assns))))
            (if (empty? assns) ""
                (format "~a~a"
                    (if me? (user-display-name (me))
                            (user-display-name (get-user-by-id (car assns))))
                    (if +k (format " +~a" +k) ""))))))

(define (list-tasks tasks)
    (define tab (map task->summaryrow tasks))
    (print-table tab
        '(1 20 6 0)
        '(20 60 6 1000)
        '(2 1 1 1)
        '(right left left left)))
 
(define repl-list-tasks (cmdentry repl-list-matchers repl-list-help repl-list-handler))
(define repl-summary (cmdentry repl-summary-matchers repl-summary-help repl-summary-handler))
