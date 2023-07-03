#lang typed/racket/base

(require
    racket/function racket/list
    racket/match    racket/block
    "../utils.rkt"
    "../utils-untyped.rkt"
    "../model.rkt"
    "../task.rkt"
    "../domain.rkt"
    "../user.rkt"
    "../types.rkt"
    "utils.rkt"
    "task-list.rkt"
    "task-misc.rkt"
    "repl-task-edit.rkt"
    "repl-eval.rkt"
)
(provide repl)

(: handle-repl CommandHandler)
(define (handle-repl argc argv)
    (with-casts ([argv : (Pair (Option String) (Listof Any))])
    (repl-menu-switch argc argv (car argv) (list
        repl-summary
        (cmdentry '("e" "eval")
                  '(("e" "eval" "enter evaluation mode"))
                  (lambda ([argc : Index] [argv : (Listof Any)])
                     (assert!! (= 1 argc))
                     (define tasks-to-eval
                        (query-tasks (sort-by < task-id)
                                     (filter-by (curry user-needs-eval-task? (assert (me))))))
                     (repl-eval tasks-to-eval)))
        (cmdentry '("n" "new" "new-task")
                  '(("n" "new-task \"<name>\"" "create a new task"))
                  (lambda ([argc : Index] [argv : (Listof Any)])
                     (assert!! (= 2 argc))
                     (with-casts ([argv : (List Any String)])
                     (new-task (second argv)))))
        (cmdentry '("ed" "edit" "edit-task")
                  '(("ed" "edit-task <id>" "enter editing mode for task <id>")
                    (""   "          <id> <cmd>" "    run <cmd> in editing mode for the task"))
                  (lambda ([argc : Index] [argv : (Listof Any)])
                     (assert!! (<= 2 argc))
                     (with-casts ([argv : (List* Any Task-ID (Listof Any))])
                     (if (= argc 2) 
                         (repl-edit (second argv))
                         (handle-edit-task (second argv) (cddr argv))))))
        (cmdentry '("cat" "show" "show-task" "view" "view-task")
                  '(("cat" "view-task <id>" "view details of a task"))
                  (lambda ([argc : Index] [argv : (Listof Any)])
                     (assert!! (= 2 argc))
                     (with-casts ([argv : (List Any Task-ID)])
                     (show-task (second argv)))))
        repl-list-tasks
        cmdentry-spacer
        (cmdentry '("reload")
                  '(("" "reload" "recover state in this domain from last commit to file"))
                  (lambda ([argc : Index] [argv : (Listof Any)]) (load)))
        (cmdentry '("commit")
                  '(("" "commit" "commit changes in this domain to file"))
                  (lambda ([argc : Index] [argv : (Listof Any)]) (commit)))
        (cmdentry '("q" "quit")
                  '(("q" "quit" "quit, committing changes in all domains"))
                  (lambda ([argc : Index] [argv : (Listof Any)]) (save-and-exit)))
        (cmdentry '("q!" "exit")
                  '(("q!" "exit" "quit without committing any changes"))
                  (lambda ([argc : Index] [argv : (Listof Any)]) (exit)))
        cmdentry-spacer
        (cmdentry '("cd" "change-domain")
                  '(("cd" "change-domain <path>" "change to another domain")
                    (""   "    /..."             "    from the root domain")
                    (""   "    ~ or <empty>"     "    to the home domain"))
                  (lambda ([argc : Index] [argv : (Listof Any)])
                     (assert!! (<= argc 2))
                     (with-casts ([argv : (U (List Any) (List Any String))])
                     (define path
                        (match argv
                            [`(,_ ,str) (string-append str ":")]
                            [_ "~:"]))
                     (select-domain (string->dmpath path)))))
        (cmdentry '("ld" "list-domains")
                  '(("ld" "list-domains" "see all domains"))
                  (lambda ([argc : Index] [argv : (Listof Any)])   
                     (for ([dmf (in-domain domain-tree-root)])
                        (displayln (dmpath->string (domain-frame-path dmf))))))
        (cmdentry-desc '(("" "<path>:<cmd>" "run <cmd> in domain at <path>")))
        cmdentry-spacer
        (cmdentry '("u" "user" "new-user")
                  '(("u" "new-user <name>" "create a new user"))
                  (lambda ([argc : Index] [argv : (Listof Any)])
                     (assert!! (= 2 argc))
                     (with-casts ([argv : (List Any String)])
                     (define name (second argv))
                     (assert!! (not (get-user-by-name name '() (const #f))))
                     (register-user (make-user (next-user-id) name))
                     (printf "User ~a registered.\n" name))))
        (cmdentry '("swap-user")
                  '(("" "swap-user" "login to a different user"))
                  (lambda ([argc : Index] [argv : (Listof Any)]) (login)))))))

(: repl : -> Any)
(define (repl)
    (retry-until-success : Any (block
        (if (get-user-me)
            (prompt (format "~a@~a" (user-display-name (assert (me))) (dmpath->string)))
            (prompt (format "~a" (dmpath->string))))
        (eof-barrier)
        (define argv (read-line-tokens))
        (define argc (length argv))
        (assert!! (< 0 argc))
        (with-casts ([argv : (Pair String (Listof Any))])
        (match-define (itempath cmd-dom cmd) (string->itempath (car argv)))
        (if (empty? cmd-dom)
            (handle-repl argc (cons cmd (cdr argv)))
            (parameterize ([current-domain-frame (assert (resolve-domain-frame cmd-dom))])
                (handle-repl argc (cons cmd (cdr argv))))))))
    (repl))


