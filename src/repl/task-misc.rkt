#lang racket

(require
    racket/block
    "../task.rkt"
    "../model.rkt"
    "../user.rkt"
    "../domain.rkt"
    "utils.rkt")
(provide
    new-task
    show-task)


(define (new-task name)
    (define id (next-task-id))
    (register-task (make-task id name))
    (printf "Registered task ~a: ~a.\n" id name))

(define (show-task id)
    (define t (get-task id))
    (newline)
    (printf "Task ~a: ~a\n" (task-id t) (task-title t))
    (if (task-desc t)
        (printf "~a\n\n" (task-desc t))
        (printf "[no description]\n\n"))
    (when (task-ready? t)
        (printf "  Ready ~a.\n" (format-date (task-ready-by t))))
    (when (task-assigned? t)
        (define u (user-display-name (get-user-by-id (task-assigned-to t))))
        (printf "  Assigned ~a to ~a.\n" (format-date (task-assigned-by t)) u))
    (when (task-done? t) 
        (printf "  Completed ~a.\n" (format-date (task-done-by t))))
    (unless (task-ready? t)
        (printf "  Pending..."))
    (unless (task-assigned? t)
        (define rates (get-user-task-evals (me) t))
        (define priority (get-task-priority t))
        (printf "    priority #~a (~a)\n"
            (add1 (length (filter (lambda (t) (if priority (> (get-task-priority t) priority) #t))
                                  (query-tasks (filter-by task-ready?)
                                               (filter-by not task-assigned?)
                                               (filter-by get-task-priority)))))
            priority)
        (printf "    your ratings:\n")
        (printf "      ~a/4 interest\n" (or (uteval-interest rates) '-))
        (printf "      ~a/4 priority\n" (or (uteval-priority rates) '-))
        (printf "      ~a   needs refinement?\n" (or (uteval-needs-refinement rates) '-)))
    (newline))


