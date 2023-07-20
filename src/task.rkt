#lang racket/base

(require
    "utils/ann.rkt"
    "utils/gregor.rkt"
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
     [ready-by : (? Moment)]
     [started-by : (? Moment)]
     [assigned-to : (? UserId)]
     [done-by : (? Moment)]))
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
    (task id title #f (now/moment) #f #f #f))

(: task-set-title! (Task String -> Void))
(define task-set-title! set-task-title!)

(: task-set-desc! (Task (? String) -> Void))
(define task-set-desc! set-task-desc!)


(: task-block! (Task -> Void))
(define (task-block! t) (set-task-ready-by! t #f))

(: task-ready! (Task -> Void))
(define (task-ready! t) (set-task-ready-by! t (now/moment)))

(: task-assign! (Task UserId -> Void))
(define (task-assign! t user)
    (set-task-assigned-to! t user)
    (set-task-started-by! t (now/moment)))

(: task-done! (Task -> Void))
(define (task-done! t)
    (set-task-done-by! t (now/moment)))


(: task-ready? (Task -> Bool))
(define task-ready? task-ready-by)

(: task-assigned? (Task -> Bool))
(define task-assigned? task-assigned-to)

(: task-done? (Task -> Bool))
(define task-done? task-done-by)


(: task->datum (Task -> Any))
(define (task->datum t)
    (match-define (task id title desc rb sb at db) t)
    (list id title desc 
        (and rb (moment->datum rb))
        (and sb (moment->datum sb))
        at
        (and db (moment->datum db))))

(: datum->task (Any -> (U Task (^ Exn:Fail:Contract))))
(define (datum->task d)
    (match-define (list id title desc rb sb at db) d)
    (task id title desc 
        (and rb (datum->moment rb))
        (and sb (datum->moment sb))
        at
        (and db (datum->moment db))))
