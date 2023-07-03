#lang typed/racket/base

(require
    "types.rkt"
    "utils.rkt"
    racket/match)
(provide
    Task    make-task   task?
    task-id task-title  task-desc
    task-ready-by   task-started-by task-assigned-to    task-done-by
    task-set-title!
    task-set-desc!
    task-block!
    task-ready! task-assign!    task-done!
    task-ready? task-assigned?  task-done?
    task->datum datum->task)

(struct task
    ([id            : Task-ID]
     [title         : String]
     [desc          : (Option String)]
     [ready-by      : (Option Timestamp)]
     [started-by    : (Option Timestamp)]
     [assigned-to   : (Option User-ID)]
     [done-by       : (Option Timestamp)])
    #:mutable
    #:type-name Task)

(: make-task (Task-ID String -> Task))
(: task-block! (Task -> Void))
(: task-ready! (Task -> Void))
(: task-assign! (Task User-ID -> Void))
(: task-done! (Task -> Void))

(define (make-task id title)
    (task id title #f (seconds-since-epoch) #f #f #f))
(define task-set-title! set-task-title!)
(define task-set-desc! set-task-desc!)
(define (task-block! t) (set-task-ready-by! t #f))
(define (task-ready! t) (set-task-ready-by! t (seconds-since-epoch)))
(define (task-assign! t user)
    (set-task-assigned-to! t user)
    (set-task-started-by! t (seconds-since-epoch)))
(define (task-done! t)
    (set-task-done-by! t (seconds-since-epoch)))
(define task-ready? task-ready-by)
(define task-assigned? task-assigned-to)
(define task-done? task-done-by)

(define-type TaskDatum
    (List Task-ID String (Option String)
          (Option Timestamp) (Option Timestamp) (Option User-ID) (Option Timestamp)))
(: task->datum (Task -> TaskDatum))
(: datum->task (Any -> Task))

(define (task->datum t)
    (match-define (task id title desc rb sb at db) t)
    (list id title desc rb sb at db))
(define (datum->task d)
    (match-define (list id title desc rb sb at db) (cast d TaskDatum))
    (task id title desc rb sb at db))
