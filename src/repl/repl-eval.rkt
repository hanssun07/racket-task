#lang typed/racket/base

(require
    racket/match    racket/block
    racket/list
    "../utils.rkt"
    "../utils-untyped.rkt"
    "../model.rkt"
    "../task.rkt"
    "../user.rkt"
    "../domain.rkt"
    "utils.rkt"
    "task-misc.rkt"
    "repl-task-edit.rkt")
(provide
    repl-eval)

(: eval-task-loop : Task -> Boolean)
(define (eval-task-loop t)
    (define id (task-id t))
    (define user-me (assert (me)))
    (define res
        (retry-until-success : (U Boolean 'loop)
            (prompt (format "eval ~a@~a~a" (user-display-name user-me) (dmpath->string) id))
            (eof-barrier)
            (define argv (read-line-tokens))
            (assert argv pair?)
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
                [_ (handle-edit-task id (cons "e" argv)) #t])))
    (if (eq? 'loop res) (eval-task-loop t) res))

(: repl-eval : (Listof Task) -> Void)
(define (repl-eval remaining)
    (unless (empty? remaining) (block
        (define t (car remaining))
        (define id (task-id t))
        (me)
        (printf "~a left." (length remaining))
        (show-task id)
        (when (eval-task-loop t) (repl-eval (cdr remaining))))))
        


