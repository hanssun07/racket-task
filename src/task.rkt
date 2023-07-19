#lang racket/base

(require
    "utils/ann.rkt"
    racket/match)
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

(:typedef TaskId ExactNonnegativeInteger)
(:structdef task : Task
    ([id : TaskId]
     [title : String]
     [desc : (? String)]
     [ready-by : (? TimeStamp)]
     [started-by : (? TimeStamp)]
     [assigned-to : (? UserId)]
     [done-by : (? TimeStamp)]))
(struct task
    (id
     title
     desc
     ready-by
     started-by
     assigned-to
     done-by) #:mutable)

(: make-task (TaskId String -> Task))
(define (make-task id title)
    (task id title #f (current-seconds) #f #f #f))

(: task-set-title! (Task String -> Void))
(define task-set-title! set-task-title!)

(: task-set-desc! (Task (? String) -> Void))
(define task-set-desc! set-task-desc!)


(: task-block! (Task -> Void))
(define (task-block! t) (set-task-ready-by! t #f))

(: task-ready! (Task -> Void))
(define (task-ready! t) (set-task-ready-by! t (current-seconds)))

(: task-assign! (Task UserId -> Void))
(define (task-assign! t user)
    (set-task-assigned-to! t user)
    (set-task-started-by! t (current-seconds)))

(: task-done! (Task -> Void))
(define (task-done! t)
    (set-task-done-by! t (current-seconds)))


(: task-ready? (Task -> Bool))
(define task-ready? task-ready-by)

(: task-assigned? (Task -> Bool))
(define task-assigned? task-assigned-to)

(: task-done? (Task -> Bool))
(define task-done? task-done-by)


(: task->datum (Task -> Any))
(define (task->datum t)
    (match-define (task id title desc rb sb at db) t)
    (list id title desc rb sb at db))

(: datum->task (Any -> (U Task (^ Exn:Fail:Contract))))
(define (datum->task d)
    (match-define (list id title desc rb sb at db) d)
    (task id title desc rb sb at db))
