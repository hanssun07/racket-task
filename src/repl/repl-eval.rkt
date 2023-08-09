#lang racket/base

(require
    racket/match    racket/block
    racket/list
    "../utils.rkt"
    "../model.rkt"
    "../task.rkt"
    "../user.rkt"
    "../domain.rkt"
    "utils.rkt"
    "task-misc.rkt"
    "repl-task-edit.rkt")
(provide
    repl-eval)

(define (eval-task-loop t)
    (define ui-id (task-ui-id t))
    (define res
        (retry-until-success 
            (prompt (format "eval ~a@~a~a" (user-display-name (me)) (dmpath->string) ui-id))
            (eof-barrier)
            (define argv (read-line-tokens))
            (match (car argv)
                [(or "q" "quit") #f]
                [(or "s" "skip") #t]
                [(or "?" "help")
                 (define helpmsg (list
                    "    <i> <p> <nr>       evaluate the task and continue"
                    "s   skip               skip this task"
                    "q   quit               exit evaluation"
                    "?   help               display this message"))
                 (for-each displayln helpmsg)
                 'loop]
                [_ (handle-edit-task t (cons "e" argv)) #t])))
    (if (eq? 'loop res) (eval-task-loop t) res))
(define (repl-eval remaining)
    (unless (empty? remaining) (block
        (define t (car remaining))
        (me)
        (printf "~a left." (length remaining))
        (show-task t)
        (when (eval-task-loop t) (repl-eval (cdr remaining))))))
        


