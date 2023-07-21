#lang racket/base

(require
    "utils/ann.rkt"
    "utils/gregor.rkt"
    "utils.rkt"
    racket/match
    racket/list)
(provide
    make-task   task?
    task-id task-title  task-desc
    task-ready-by   task-assigned-by task-assigned-to    task-done-by
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
     [attrs : (MHash Symbol (Listof Any))]))
(struct task
    (id
     title
     desc
     attrs)
    #:mutable)

(: make-task (TaskId String -> Task))
(define (make-task id title)
    (task id title #f (make-hash `((ready ,(now/moment))))))

(: task-set-title! (Task String -> Void))
(define task-set-title! set-task-title!)

(: task-set-desc! (Task (? String) -> Void))
(define task-set-desc! set-task-desc!)

(: task-ref-attr (Task Symbol (-> f) -> (U f (Listof a)))
                 (Task Symbol f      -> (U f (Listof a)))
                 (Task Symbol        -> (? (Listof a))))
(define (task-ref-attr t attr [on-fail #f])
    (hash-ref (task-attrs t) attr on-fail))

(: task-set-attr! (Task Symbol Any -> Void))
(define (task-set-attr! t attr v)
    (hash-set! (task-attrs t) attr v))

(: task-update-attr! (Task Symbol (v -> v) (-> f) -> (U f Void))
                     (Task Symbol (v -> v) f      -> Void)
                     (Task Symbol (v -> v)        -> (U Void (^ Exn:Fail))))
(define (task-update-attr! t attr updater
        [on-fail (error-failthrough 'task-update-attr! "no attribute ~a to update on task ~a"
                                    attr (task-id t))])
    (hash-update! (task-attrs t) attr updater on-fail))

(: task-remove-attr! (Task Symbol -> Void))
(define (task-remove-attr! t attr)
    (hash-remove! (task-attrs t) attr))

(: task-block! (Task -> Void))
(define (task-block! t) (task-remove-attr! t 'ready))

(: task-ready! (Task -> Void))
(: task-ready? (Task -> Bool))
(: task-ready-by (Task (-> f) -> (U Moment f))
                 (Task f      -> (U Moment f))
                 (Task        -> (U Moment (^ Exn:Fail))))
(define (task-ready! t) (task-set-attr! t 'ready (list (now/moment))))
(define (task-ready? t) (task-ref-attr t 'ready))
(define (task-ready-by t
        [on-fail (error-failthrough 'task-ready-by "task ~a is not ready" (task-id t))])
    (define res (task-ref-attr t 'ready))
    (if res (car res) (if (procedure? on-fail) (on-fail) on-fail)))

(: task-assign! (Task UserId -> Void))
(: task-assigned? (Task -> Bool))
(: task-assigned-by (Task (-> f) -> (U Moment f))
                    (Task f      -> (U Moment f))
                    (Task        -> (U Moment (^ Exn:Fail))))
(: task-assigned-to (Task (-> f) -> (U UserId f))
                    (Task f      -> (U UserId f))
                    (Task        -> (U Userid (^ Exn:Fail))))
(define (task-assign! t uid) (task-set-attr! t 'assigned (list uid (now/moment))))
(define (task-assigned? t) (task-ref-attr t 'assigned))
(define (task-assigned-by t
        [on-fail (error-failthrough 'task-assigned-by "task ~a is not assigned" (task-id t))])
    (define res (task-ref-attr t 'assigned))
    (if res (second res) (if (procedure? on-fail) (on-fail) on-fail)))
(define (task-assigned-to t
        [on-fail (error-failthrough 'task-assigned-to "task ~a is not assigned" (task-id t))])
    (define res (task-ref-attr t 'assigned))
    (if res (first res) (if (procedure? on-fail) (on-fail) on-fail)))

(: task-done! (Task -> Void))
(: task-done? (Task -> Bool))
(: task-assigned-by (Task (-> f) -> (U Moment f))
                    (Task f      -> (U Moment f))
                    (Task        -> (U Moment (^ Exn:Fail))))
(define (task-done! t) (task-set-attr! t 'done (list (now/moment))))
(define (task-done? t) (task-ref-attr t 'done))
(define (task-done-by t
        [on-fail (error-failthrough 'task-done-by "task ~a is not done" (task-id t))])
    (define res (task-ref-attr t 'done))
    (if res (car res) (if (procedure? on-fail) (on-fail) on-fail)))


(: task->datum (Task -> Any))
(define (task->datum t)
    (match-define (task id title desc attrs) t)
    (list id title desc 
        (sort (hash->list attrs)
              symbol<? #:key car)))

(: datum->task (Any -> (U Task (^ Exn:Fail:Contract))))
(define (datum->task d)
    (match-define (list id title desc attrs) d)
    (task id title desc (make-hash (datum-rec-transform attrs
                                    (lambda (x) (or (datum->moment x) x))))))
