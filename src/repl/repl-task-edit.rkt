#lang typed/racket/base

(require
    racket/match    racket/block
    racket/string   racket/list
    "../utils.rkt"
    "../utils-untyped.rkt"
    "../model.rkt"
    "../task.rkt"
    "../user.rkt"
    "../domain.rkt"
    "../types.rkt"
    "utils.rkt"
    "task-misc.rkt")
(provide
    handle-edit-task
    repl-edit)

(: handle-edit-task : Task-ID (Listof Any) -> Boolean)
(define (handle-edit-task id argv)
    (define t (get-task id))
    (define continue? : Boolean #t)
    (define argc (length argv))
    (assert argv pair?)
    (match (car argv)
        [(or "cat" "show")
         (show-task id)]
        [(or "desc" "set-desc" "sd") (block
         (assert!! (= 1 argc))
         (define desc (prompt-editor (or (task-desc t) "")))
         (task-set-desc! t (if (zero? (string-length desc)) #f desc)))]
        [(or "name" "rename" "title" "retitle" "n" "t")
         (assert!! (< 1 argc))
         (with-casts ([argv : (List* Any String (Listof String))])
         (define name (string-join (cdr argv)))
         (task-set-title! t name))]
        [(or "block" "b")
         (assert!! (= 1 argc))
         (assert!! (not (task-done? t)))
         (task-block! t)]
        [(or "ready" "r")
         (assert!! (= 1 argc))
         (assert!! (not (task-ready? t)))
         (task-ready! t)]
        [(or "assign" "ass" "a")
         (assert!! (<= 1 argc 2))
         (assert!! (task-ready? t))
         (assert!! (not (task-done? t)))
         (with-casts ([argv : (List Any String)])
         (define u (if (= 1 argc) (user-id (assert (me))) (user-id (get-user-by-name (second argv)))))
         (task-assign! t u))]
        [(or "done" "d" "finish" "fin" "f")
         (assert!! (= 1 argc))
         (assert!! (task-assigned? t))
         (assert!! (not (task-done? t)))
         (task-done! t)]
        [(or "eval" "e")
         (define cdrargv (cdr argv))
         (assert!! (<= 2 argc 4))
         (with-casts ([cdrargv : (Listof Eval-Rating)])
         (define cur-user (assert (me)))
         (user-set-interest! cur-user id (first cdrargv))
         (when (<= 3 argc) (user-set-priority! cur-user id (second cdrargv)))
         (when (<= 4 argc) (user-set-needs-refinement! cur-user id (third cdrargv))))]
        [(or "help" "?")
         (define helpmsg (list
            "cat show               show the task"
            "sd  set-desc           rewrite the description"
            "n   rename \"<name>\""
            "b   block              set the task as blocked"
            "r   ready"
            "a   assign [<user-id>] assign to user, or to self by default"
            "d   done"
            "e   eval <i> <p> <nr>  interest, priority, needs refinement?"
            "q   quit"
            "?   help               display this message"))
         (for-each displayln helpmsg)]
        [(or "quit" "q")
         (set! continue? #f)])
    continue?)

(: repl-edit : Task-ID -> Void)
(define (repl-edit id)
    (define t (get-task id))
    (define continue? #t)
    (retry-until-success (block
        (prompt (format "ed ~a@~a~a" (user-display-name (assert (me))) (dmpath->string) id))
        (eof-barrier)
        (define argv (read-line-tokens))
        (set! continue? (and (handle-edit-task id argv) continue?))))
    (when continue? (repl-edit id)))


