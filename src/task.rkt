#lang racket/base

(require racket/match)
(provide
    make-task   task?
    task-id task-title  task-desc
    task-ready-by   task-started-by task-assigned-to    task-done-by
    task-set-title!
    task-set-desc!
    task-block!
    task-ready! task-assign!    task-done!
    task-ready? task-assigned?  task-done?
    task->datum datum->task)

(struct task
    (id
     title
     desc
     ready-by
     started-by
     assigned-to
     done-by) #:mutable)

(define (make-task id title)
    (task id title #f (current-seconds) #f #f #f))
(define task-set-title! set-task-title!)
(define task-set-desc! set-task-desc!)
(define (task-block! t) (set-task-ready-by! t #f))
(define (task-ready! t) (set-task-ready-by! t (current-seconds)))
(define (task-assign! t user)
    (set-task-assigned-to! t user)
    (set-task-started-by! t (current-seconds)))
(define (task-done! t)
    (set-task-done-by! t (current-seconds)))
(define task-ready? task-ready-by)
(define task-assigned? task-assigned-to)
(define task-done? task-done-by)

(define (task->datum t)
    (match-define (task id title desc rb sb at db) t)
    (list id title desc rb sb at db))
(define (datum->task d)
    (match-define (list id title desc rb sb at db) d)
    (task id title desc rb sb at db))
