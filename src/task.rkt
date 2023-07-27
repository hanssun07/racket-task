#lang racket/base

(require
    "utils/ann.rkt"
    "utils/gregor.rkt"
    "utils.rkt"
    threading
    racket/match
    racket/list racket/function)
(provide
    make-task   task?
    task-id task-created
    task-title  task-desc
    task-set-title!  task-set-desc!
    task-block!
    task-ready!  task-ready?    task-ready-by
    task-assign!
        task-assigned-to task-assigned-to-user?
        task-unassign! 
    task-start!  task-started?  task-started-by
    task-done!   task-done?     task-done-by
    task-close!  task-closed?   task-closed-by
                 task-resolved? task-resolved-by
    task->datum datum->task)

(:typedef TaskId ExactNonnegativeInteger)
(:structdef task : Task
    ([id : TaskId]
     [created : Moment]
     [title : String]
     [desc : (? String)]
     [attrs : (MHash Symbol (Listof Any))]))
(struct task
    (id
     created
     title
     desc
     attrs)
    #:mutable)

(: make-task (TaskId String -> Task))
(define (make-task id title)
    (define _now (now))
    (task id _now title #f (make-hash `((ready ,_now)))))

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
(: task-update-attr! (Task Symbol (lv -> lv) (-> f) -> (U f Void))
                     (Task Symbol (lv -> lv) lv     -> Void)
                     (Task Symbol (lv -> lv)        -> (U Void (^ Exn:Fail)))
                     (: lv (Listof v)))
(: task-remove-attr! (Task Symbol -> Void))
(: task-block! (Task -> Void))
(define (task-set-attr! t attr v)
    (hash-set! (task-attrs t) attr v))
(define (task-update-attr! t attr updater
        [on-fail (error-failthrough 'task-update-attr! "no attribute ~a to update on task ~a"
                                    attr (task-id t))])
    (hash-update! (task-attrs t) attr updater on-fail))
(define (task-remove-attr! t attr)
    (hash-remove! (task-attrs t) attr))
(define (task-block! t) (task-remove-attr! t 'ready))

(: task-ready! (Task -> Void))
(: task-ready? (Task -> Bool))
(: task-ready-by (Task (-> f) -> (U Moment f))
                 (Task f      -> (U Moment f))
                 (Task        -> (U Moment (^ Exn:Fail))))
(define (task-ready! t) (task-set-attr! t 'ready (list (now))))
(define (task-ready? t) (task-ref-attr t 'ready))
(define (task-ready-by t
        [on-fail (error-failthrough 'task-ready-by "task ~a is not ready" (task-id t))])
    (define res (task-ref-attr t 'ready))
    (if res (~> res car unwrap-moment) (unwrap-const on-fail)))

(: task-assign! (Task UserId (-> f) -> (U Void f))
                (Task UserId f      -> (U Void f))
                (Task UserId        -> (U Void (^ Exn:Fail))))
(: task-unassign! (Task UserId -> Void))
(: task-assigned-to (Task -> (Listof UserId)))
(: task-assigned-to-user? (Task UserId -> Bool))
(define (task-assign! t uid
        [on-fail (error-failthrough 'task-assign! "task ~a is already assigned to user ~a"
                                    (task-id t) uid)])
    (if (member uid (task-ref-attr t 'assigned empty))
        (unwrap-const on-fail)
        (task-update-attr! t 'assigned (curry cons uid) empty)))
(define (task-unassign! t uid) (task-update-attr! t 'assigned (curry remove uid) empty))
(define (task-assigned-to t) (task-ref-attr t 'assigned empty))
(define (task-assigned-to-user? t uid)
    (member uid (task-ref-attr t 'assigned empty)))

(: task-start! (Task -> Void))
(: task-started? (Task -> Bool))
(: task-started-by (Task (-> f) -> (U Moment f))
                   (Task f      -> (U Moment f))
                   (Task        -> (U Moment (^Exn:Fail))))
(define (task-start! t) (task-set-attr! t 'started (list (now))))
(define (task-started? t) (task-ref-attr t 'started #f))
(define (task-started-by t
        [on-fail (error-failthrough 'task-started-by "task ~a has not been started" (task-id t))])
    (define res (task-ref-attr t 'started #f))
    (if res (~> res car unwrap-moment) (unwrap-const on-fail)))

(: task-done! (Task -> Void))
(: task-done? (Task -> Bool))
(: task-done-by (Task (-> f) -> (U Moment f))
                (Task f      -> (U Moment f))
                (Task        -> (U Moment (^ Exn:Fail))))
(define (task-done! t) (task-set-attr! t 'done (list (now))))
(define (task-done? t) (task-ref-attr t 'done))
(define (task-done-by t
        [on-fail (error-failthrough 'task-done-by "task ~a is not done" (task-id t))])
    (define res (task-ref-attr t 'done))
    (if res (~> res car unwrap-moment) (unwrap-const on-fail)))

(: task-close! (Task -> Void))
(: task-closed? (Task -> Bool))
(: task-closed-by (Task (-> f) -> (U Moment f))
                  (Task f      -> (U Moment f))
                  (Task        -> (U Moment (^ Exn:Fail))))
(define (task-close! t) (task-set-attr! t 'closed (list (now))))
(define (task-closed? t) (task-ref-attr t 'closed))
(define (task-closed-by t
        [on-fail (error-failthrough 'task-closed-by "task ~a is not closed" (task-id t))])
    (define res (task-ref-attr t 'done))
    (if res (~> res car unwrap-moment) (unwrap-const on-fail)))

(: task-resolved? (Task -> Bool))
(: task-resolved-by (Task (-> f) -> (U Moment f))
                    (Task f      -> (U Moment f))
                    (Task        -> (U Moment (^ Exn:Fail))))
(define (task-resolved? t) (or (task-done? t) (task-closed? t)))
(define (task-resolved-by t
        [on-fail (error-failthrough 'task-resolved-by "task ~a is not resolved" (task-id t))])
    (define res (or (task-done-by t #f) (task-closed-by t #f)))
    (if res res (unwrap-const on-fail)))

(: task->datum (Task -> Any))
(define (task->datum t)
    (match-define (task id created title desc attrs) t)
    (list id (moment->datum created) title desc 
        (datum-rec-transform
            (sort (hash->list attrs)
                  symbol<? #:key car)
            (lambda (x) (if (moment/datum? x) (moment->datum x) x)))))

(: datum->task (Any -> (U Task (^ Exn:Fail:Contract))))
(define (datum->task d)
    (match-define (list id created title desc attrs) d)
    (task id (datum->moment created) title desc
        (make-hash (map (lambda (x) (case (car x)
                            [(ready started done closed)
                             `(,(car x) ,(datum->moment (cadr x)))]
                            [else x]))
                        attrs))))
